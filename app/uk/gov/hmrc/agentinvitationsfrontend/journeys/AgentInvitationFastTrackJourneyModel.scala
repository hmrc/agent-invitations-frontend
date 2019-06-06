/*
 * Copyright 2019 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.agentinvitationsfrontend.journeys

import org.joda.time.LocalDate
import play.api.mvc.{Request, Result}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.Transitions.CheckDOBMatches
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.personal
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.fsm.JourneyModel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object AgentInvitationFastTrackJourneyModel extends JourneyModel {

  sealed trait State

  val root: State = State.Prologue(None)

  /* State should contain only minimal set of data required to proceed */
  object State {
    case class Prologue(failureUrl: Option[String]) extends State

    case class CheckDetailsNoPostcode(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
        extends State
    case class CheckDetailsNoDob(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State
    case class CheckDetailsNoVatRegDate(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
        extends State
    case class CheckDetailsNoClientTypeVat(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
        extends State
    case class CheckDetailsCompleteItsa(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
        extends State
    case class CheckDetailsCompleteIrv(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
        extends State
    case class CheckDetailsCompletePersonalVat(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
        extends State
    case class CheckDetailsCompleteBusinessVat(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
        extends State

    case class NoPostcode(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State
    case class NoDob(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State
    case class NoVatRegDate(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State

    case class SelectClientTypeVat(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State

    case class IdentifyPersonalClient(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
        extends State
    case class IdentifyBusinessClient(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
        extends State

    case class InvitationSentPersonal(invitationLink: String, continueUrl: Option[String], agencyEmail: String)
        extends State
    case class InvitationSentBusiness(invitationLink: String, continueUrl: Option[String], agencyEmail: String)
        extends State

    case class PendingInvitationExists(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
        extends State
    case class ActiveAuthorisationExists(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
        extends State
    case class KnownFactNotMatched(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State
    case class ClientNotSignedUp(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State
  }

  object Transitions {
    import State._

    type HasPendingInvitations = (Arn, String, String) => Future[Boolean]
    type HasActiveRelationship = (Arn, String, String) => Future[Boolean]
    type GetClientName = (String, String) => Future[Option[String]]
    type CheckPostcodeMatches = (Nino, String) => Future[Option[Boolean]]
    type CheckRegDateMatches = (Vrn, LocalDate) => Future[Option[Int]]
    type GetAgentLink = (Arn, Option[ClientType]) => Future[String]
    type CreateInvitation =
      (Arn, Invitation) => Future[InvitationId]
    type GetAgencyEmail = () => Future[String]

    def prologue(failureUrl: Option[String]) = Transition {
      case _ => goto(Prologue(failureUrl))
    }

    def start(features: FeatureFlags)(continueUrl: Option[String])(agent: AuthorisedAgent)(
      fastTrackRequest: AgentFastTrackRequest)(implicit request: Request[Any], hc: HeaderCarrier) = Transition {
      case _ => {

        val isKfcEnabled = features.isKfcFlagOnForService(fastTrackRequest.service)

        def gotoNoKnownFactOrComplete(
          noKnownFactState: State,
          completeState: State,
          fastTrackRequest: AgentFastTrackRequest) = {
          val nextStateIfKnownFactRequired = if (isKfcEnabled) noKnownFactState else completeState

          def nextState(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) =
            fastTrackRequest.knownFact.fold(nextStateIfKnownFactRequired)(_ => completeState)

          goto(nextState(fastTrackRequest, continueUrl))
        }

        fastTrackRequest match {
          case AgentFastTrackRequest(_, HMRCMTDIT, _, _, _) => {
            val updatedPersonalRequest = fastTrackRequest.copy(clientType = Some(personal))
            gotoNoKnownFactOrComplete(
              CheckDetailsNoPostcode(updatedPersonalRequest, continueUrl),
              CheckDetailsCompleteItsa(updatedPersonalRequest, continueUrl),
              updatedPersonalRequest)
          }

          case AgentFastTrackRequest(_, HMRCPIR, _, _, _) => {
            val updatedPersonalRequest = fastTrackRequest.copy(clientType = Some(personal))
            gotoNoKnownFactOrComplete(
              CheckDetailsNoDob(updatedPersonalRequest, continueUrl),
              CheckDetailsCompleteIrv(updatedPersonalRequest, continueUrl),
              updatedPersonalRequest)
          }

          case AgentFastTrackRequest(Some(ClientType.personal), HMRCMTDVAT, _, _, _) =>
            gotoNoKnownFactOrComplete(
              CheckDetailsNoVatRegDate(fastTrackRequest, continueUrl),
              CheckDetailsCompletePersonalVat(fastTrackRequest, continueUrl),
              fastTrackRequest)

          case AgentFastTrackRequest(Some(ClientType.business), HMRCMTDVAT, _, _, _) =>
            gotoNoKnownFactOrComplete(
              CheckDetailsNoVatRegDate(fastTrackRequest, continueUrl),
              CheckDetailsCompleteBusinessVat(fastTrackRequest, continueUrl),
              fastTrackRequest)

          case AgentFastTrackRequest(None, HMRCMTDVAT, _, _, _) =>
            goto(CheckDetailsNoClientTypeVat(fastTrackRequest, continueUrl))
        }
      }
    }

    def checkIfPendingOrActiveAndGoto(
      fastTrackRequest: AgentFastTrackRequest,
      arn: Arn,
      invitation: Invitation,
      continueUrl: Option[String])(
      hasPendingInvitationsFor: HasPendingInvitations,
      hasActiveRelationshipFor: HasActiveRelationship,
      createInvitation: CreateInvitation,
      getAgentLink: GetAgentLink,
      getAgencyEmail: GetAgencyEmail): Future[State] =
      for {
        hasPendingInvitations <- hasPendingInvitationsFor(
                                  arn,
                                  fastTrackRequest.clientIdentifier,
                                  fastTrackRequest.service)
        result <- if (hasPendingInvitations) {
                   goto(PendingInvitationExists(fastTrackRequest, continueUrl))
                 } else {
                   hasActiveRelationshipFor(arn, fastTrackRequest.clientIdentifier, fastTrackRequest.service)
                     .flatMap {
                       case true => goto(ActiveAuthorisationExists(fastTrackRequest, continueUrl))
                       case false =>
                         for {
                           agencyEmail    <- getAgencyEmail()
                           _              <- createInvitation(arn, invitation)
                           invitationLink <- getAgentLink(arn, fastTrackRequest.clientType)
                           result <- fastTrackRequest.clientType match {
                                      case Some(ClientType.personal) =>
                                        goto(InvitationSentPersonal(invitationLink, continueUrl, agencyEmail))
                                      case Some(ClientType.business) =>
                                        goto(InvitationSentBusiness(invitationLink, continueUrl, agencyEmail))
                                    }
                         } yield result
                     }
                 }
      } yield result

    def checkedDetailsNoClientType(agent: AuthorisedAgent) = Transition {
      case CheckDetailsNoClientTypeVat(fastTrackRequest, continueUrl) =>
        goto(SelectClientTypeVat(fastTrackRequest, continueUrl))

      case NoVatRegDate(fastTrackRequest, continueUrl) =>
        goto(SelectClientTypeVat(fastTrackRequest, continueUrl))
    }

    def checkedDetailsNoKnownFact(agent: AuthorisedAgent) =
      Transition {
        case CheckDetailsNoPostcode(fastTrackRequest, continueUrl) =>
          goto(NoPostcode(fastTrackRequest, continueUrl))

        case CheckDetailsNoDob(fastTrackRequest, continueUrl) =>
          goto(NoDob(fastTrackRequest, continueUrl))

        case CheckDetailsNoVatRegDate(fastTrackRequest, continueUrl) =>
          goto(NoVatRegDate(fastTrackRequest, continueUrl))
      }

    def checkedDetailsChangeInformation(agent: AuthorisedAgent) = {
      def gotoIdentifyClient(ftRequest: AgentFastTrackRequest, continueUrl: Option[String]) =
        if (ftRequest.clientType.contains(personal)) {
          goto(IdentifyPersonalClient(ftRequest, continueUrl))
        } else {
          goto(IdentifyBusinessClient(ftRequest, continueUrl))
        }

      Transition {
        case CheckDetailsCompleteItsa(ftr, continueUrl) =>
          goto(IdentifyPersonalClient(ftr, continueUrl))

        case CheckDetailsCompleteIrv(ftr, continueUrl) =>
          goto(IdentifyPersonalClient(ftr, continueUrl))

        case CheckDetailsCompletePersonalVat(ftr, continueUrl) =>
          goto(IdentifyPersonalClient(ftr, continueUrl))

        case CheckDetailsCompleteBusinessVat(ftr, continueUrl) =>
          goto(IdentifyBusinessClient(ftr, continueUrl))

        case CheckDetailsNoPostcode(ftr, continueUrl) =>
          goto(IdentifyPersonalClient(ftr, continueUrl))

        case CheckDetailsNoDob(ftr, continueUrl) =>
          goto(IdentifyPersonalClient(ftr, continueUrl))

        case CheckDetailsNoVatRegDate(ftr, continueUrl) =>
          gotoIdentifyClient(ftr, continueUrl)

        case CheckDetailsNoClientTypeVat(ftr, continueUrl) =>
          gotoIdentifyClient(ftr, continueUrl)
      }
    }

    def checkedDetailsAllInformation(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(
      hasActiveRelationship: HasActiveRelationship)(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(
      confirmation: Confirmation) = Transition {
      case CheckDetailsCompleteItsa(fastTrackRequest, continueUrl) => {
        if (confirmation.choice) {
          val checkAndGotoInvitationSent = checkIfPendingOrActiveAndGoto(
            fastTrackRequest,
            agent.arn,
            ItsaInvitation(Nino(fastTrackRequest.clientIdentifier), fastTrackRequest.knownFact.map(Postcode.apply)),
            continueUrl
          )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink, getAgencyEmail)
          if (featureFlags.showKfcMtdIt) {
            checkPostcodeMatches(Nino(fastTrackRequest.clientIdentifier), fastTrackRequest.knownFact.getOrElse(""))
              .flatMap {
                case Some(true)  => checkAndGotoInvitationSent
                case Some(false) => goto(KnownFactNotMatched(fastTrackRequest, continueUrl))
                case None        => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
              }
          } else checkAndGotoInvitationSent
        } else goto(IdentifyPersonalClient(fastTrackRequest, continueUrl))
      }

      case CheckDetailsCompleteIrv(fastTrackRequest, continueUrl) =>
        if (confirmation.choice) {
          val checkAndGotoInvitationSent = checkIfPendingOrActiveAndGoto(
            fastTrackRequest,
            agent.arn,
            PirInvitation(Nino(fastTrackRequest.clientIdentifier), fastTrackRequest.knownFact.map(DOB.apply)),
            continueUrl
          )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink, getAgencyEmail)
          if (featureFlags.showKfcPersonalIncome) {
            checkDobMatches(
              Nino(fastTrackRequest.clientIdentifier),
              LocalDate.parse(fastTrackRequest.knownFact.getOrElse("")))
              .flatMap {
                case Some(true)  => checkAndGotoInvitationSent
                case Some(false) => goto(KnownFactNotMatched(fastTrackRequest, continueUrl))
                case None        => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
              }
          } else checkAndGotoInvitationSent
        } else goto(IdentifyPersonalClient(fastTrackRequest, continueUrl))

      case CheckDetailsCompletePersonalVat(fastTrackRequest, continueUrl) =>
        if (confirmation.choice) {
          val checkAndGotoInvitationSent = checkIfPendingOrActiveAndGoto(
            fastTrackRequest,
            agent.arn,
            VatInvitation(
              Some(personal),
              Vrn(fastTrackRequest.clientIdentifier),
              fastTrackRequest.knownFact.map(VatRegDate.apply)),
            continueUrl
          )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink, getAgencyEmail)
          if (featureFlags.showKfcMtdVat) {
            checkRegDateMatches(
              Vrn(fastTrackRequest.clientIdentifier),
              LocalDate.parse(fastTrackRequest.knownFact.getOrElse("")))
              .flatMap {
                case Some(204) => checkAndGotoInvitationSent
                case Some(_)   => goto(KnownFactNotMatched(fastTrackRequest, continueUrl))
                case None      => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
              }
          } else checkAndGotoInvitationSent
        } else goto(IdentifyPersonalClient(fastTrackRequest, continueUrl))

      case CheckDetailsCompleteBusinessVat(fastTrackRequest, continueUrl) =>
        if (confirmation.choice) {
          val checkAndGotoInvitationSent = checkIfPendingOrActiveAndGoto(
            fastTrackRequest,
            agent.arn,
            VatInvitation(
              Some(ClientType.business),
              Vrn(fastTrackRequest.clientIdentifier),
              fastTrackRequest.knownFact.map(VatRegDate.apply)),
            continueUrl
          )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink, getAgencyEmail)
          if (featureFlags.showKfcMtdVat) {
            checkRegDateMatches(
              Vrn(fastTrackRequest.clientIdentifier),
              LocalDate.parse(fastTrackRequest.knownFact.getOrElse("")))
              .flatMap {
                case Some(204) => checkAndGotoInvitationSent
                case Some(_)   => goto(KnownFactNotMatched(fastTrackRequest, continueUrl))
                case None      => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
              }
          } else checkAndGotoInvitationSent
        } else goto(IdentifyBusinessClient(fastTrackRequest, continueUrl))
    }

    def identifiedClientItsa(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(
      hasActiveRelationship: HasActiveRelationship)(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(
      itsaClient: ItsaClient) = Transition {
      case IdentifyPersonalClient(ftr, continueUrl) =>
        val newState = CheckDetailsCompleteItsa(
          ftr.copy(clientIdentifier = itsaClient.clientIdentifier, knownFact = itsaClient.postcode),
          continueUrl)
        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(
          Confirmation(true))
          .apply(newState)
    }

    def identifiedClientIrv(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(
      hasActiveRelationship: HasActiveRelationship)(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(
      irvClient: IrvClient) = Transition {
      case IdentifyPersonalClient(ftr, continueUrl) =>
        val newState =
          CheckDetailsCompleteIrv(
            ftr.copy(clientIdentifier = irvClient.clientIdentifier, knownFact = irvClient.dob),
            continueUrl)
        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(
          Confirmation(true))
          .apply(newState)
    }

    def identifiedClientVat(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(
      hasActiveRelationship: HasActiveRelationship)(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(
      vatClient: VatClient) = Transition {
      case IdentifyPersonalClient(ftr, continueUrl) =>
        val newState = CheckDetailsCompletePersonalVat(
          ftr.copy(clientIdentifier = vatClient.clientIdentifier, knownFact = vatClient.registrationDate),
          continueUrl)
        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(
          Confirmation(true))
          .apply(newState)
      case IdentifyBusinessClient(ftr, continueUrl) =>
        val newState = CheckDetailsCompleteBusinessVat(
          ftr.copy(clientIdentifier = vatClient.clientIdentifier, knownFact = vatClient.registrationDate),
          continueUrl)
        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(
          Confirmation(true))
          .apply(newState)
    }

    def moreDetailsItsa(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(
      hasActiveRelationship: HasActiveRelationship)(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(
      suppliedKnownFact: Option[String]) = Transition {
      case NoPostcode(ftr, continueUrl) =>
        val newState = CheckDetailsCompleteItsa(ftr.copy(knownFact = suppliedKnownFact), continueUrl)
        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(
          Confirmation(true))
          .apply(newState)
    }

    def moreDetailsIrv(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(
      hasActiveRelationship: HasActiveRelationship)(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(
      suppliedKnownFact: Option[String]) = Transition {
      case NoDob(ftr, continueUrl) =>
        val newState = CheckDetailsCompleteIrv(ftr.copy(knownFact = suppliedKnownFact), continueUrl)
        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(
          Confirmation(true))
          .apply(newState)
    }

    def moreDetailsVat(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(
      hasActiveRelationship: HasActiveRelationship)(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(
      suppliedKnownFact: Option[String]) = Transition {
      case NoVatRegDate(ftRequest, continueUrl) => {
        val checkDetailsState =
          if (ftRequest.clientType.contains(personal))
            CheckDetailsCompletePersonalVat
          else
            CheckDetailsCompleteBusinessVat

        val newState = checkDetailsState(ftRequest.copy(knownFact = suppliedKnownFact), continueUrl)

        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(
          Confirmation(true))
          .apply(newState)
      }
    }

    def selectedClientType(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(
      hasActiveRelationship: HasActiveRelationship)(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(
      suppliedClientType: ClientType) = Transition {

      case SelectClientTypeVat(ftr, continueUrl) =>
        val isKnownFactRequired = ftr.knownFact.isDefined || !featureFlags.isKfcFlagOnForService(ftr.service)
        if (isKnownFactRequired) {
          val completeState =
            if (ftr.clientType.contains(personal)) CheckDetailsCompletePersonalVat
            else CheckDetailsCompleteBusinessVat
          val newState =
            completeState(ftr.copy(clientType = Some(suppliedClientType)), continueUrl)

          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(
            Confirmation(true))
            .apply(newState)

        } else {
          val newState = CheckDetailsNoVatRegDate(ftr.copy(clientType = Some(suppliedClientType)), continueUrl)

          checkedDetailsNoKnownFact(agent)
            .apply(newState)
        }
    }

  }
}
