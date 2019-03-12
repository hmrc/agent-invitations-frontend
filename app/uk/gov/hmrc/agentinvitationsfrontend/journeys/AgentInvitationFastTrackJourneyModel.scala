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
import play.api.mvc.Request
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.Transitions.CheckDOBMatches
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object AgentInvitationFastTrackJourneyModel extends JourneyModel {

  sealed trait State

  val root: State = States.Prologue(None)

  /* State should contain only minimal set of data required to proceed */
  object States {
    case class Prologue(failureUrl: Option[String]) extends State

    case class CheckDetailsNoKnownFact(fastTrackRequest: AgentFastTrackRequest, continueURL: Option[String])
        extends State
    case class CheckDetailsNoClientType(fastTrackRequest: AgentFastTrackRequest, continueURL: Option[String])
        extends State

    case class CheckDetailsCompleteItsa(fastTrackRequest: AgentFastTrackRequest, continueURL: Option[String])
        extends State
    case class CheckDetailsCompletePir(fastTrackRequest: AgentFastTrackRequest, continueURL: Option[String])
        extends State
    case class CheckDetailsCompletePersonalVat(fastTrackRequest: AgentFastTrackRequest, continueURL: Option[String])
        extends State
    case class CheckDetailsCompleteBusinessVat(fastTrackRequest: AgentFastTrackRequest, continueURL: Option[String])
        extends State

    case class MoreDetails(fastTrackRequest: AgentFastTrackRequest, continueURL: Option[String]) extends State
    case class SelectClientType(fastTrackRequest: AgentFastTrackRequest, continueURL: Option[String]) extends State
    case class IdentifyPersonalClient(fastTrackRequest: AgentFastTrackRequest, continueURL: Option[String])
        extends State
    case class IdentifyBusinessClient(fastTrackRequest: AgentFastTrackRequest, continueURL: Option[String])
        extends State
    case class PendingInvitationExists(fastTrackRequest: AgentFastTrackRequest, continueURL: Option[String])
        extends State
    case class ActiveAuthorisationExists(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
        extends State
    case class KnownFactNotMatched(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State
    case class InvitationSentPersonal(invitationLink: String, continueUrl: Option[String]) extends State
    case class InvitationSentBusiness(invitationLink: String, continueUrl: Option[String]) extends State
    case class ClientNotSignedUp(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State
  }

  object Transitions {
    import States._

    type HasPendingInvitations = (Arn, String, String) => Future[Boolean]
    type HasActiveRelationship = (Arn, String, String) => Future[Boolean]
    type GetClientName = (String, String) => Future[Option[String]]
    type CheckPostcodeMatches = (Nino, String) => Future[Option[Boolean]]
    type CheckRegDateMatches = (Vrn, LocalDate) => Future[Option[Int]]
    type GetAgentLink = (Arn, Option[ClientType]) => Future[String]
    type CreateInvitation =
      (Arn, Invitation) => Future[InvitationId]

    def prologue(failureUrl: Option[String]) = Transition {
      case _ => goto(Prologue(failureUrl))
    }

    def start(features: FeatureFlags)(continueUrl: Option[String])(agent: AuthorisedAgent)(
      fastTrackRequest: AgentFastTrackRequest)(implicit request: Request[Any], hc: HeaderCarrier) = Transition {
      case _ => {

        val isKfcEnabled = features.isKfcFlagOnForService(fastTrackRequest.service)

        def gotoMoreDetailsOrComplete(completeState: State, fastTrackRequest: AgentFastTrackRequest) = {
          val nextStateIfKnownFactRequired =
            if (isKfcEnabled) CheckDetailsNoKnownFact(fastTrackRequest, continueUrl) else completeState
          def nextState(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) =
            fastTrackRequest.knownFact.fold(nextStateIfKnownFactRequired)(_ => completeState)
          goto(nextState(fastTrackRequest, continueUrl))
        }

        fastTrackRequest match {
          case AgentFastTrackRequest(_, HMRCMTDIT, _, _, _) => {
            val updatedPersonalRequest = fastTrackRequest.copy(clientType = Some(personal))
            gotoMoreDetailsOrComplete(
              CheckDetailsCompleteItsa(updatedPersonalRequest, continueUrl),
              updatedPersonalRequest)
          }

          case AgentFastTrackRequest(_, HMRCPIR, _, _, _) => {
            val updatedPersonalRequest = fastTrackRequest.copy(clientType = Some(personal))
            gotoMoreDetailsOrComplete(
              CheckDetailsCompletePir(updatedPersonalRequest, continueUrl),
              updatedPersonalRequest)
          }

          case AgentFastTrackRequest(Some(ClientType.personal), HMRCMTDVAT, _, _, _) =>
            gotoMoreDetailsOrComplete(CheckDetailsCompletePersonalVat(fastTrackRequest, continueUrl), fastTrackRequest)

          case AgentFastTrackRequest(Some(ClientType.business), HMRCMTDVAT, _, _, knownFact) =>
            gotoMoreDetailsOrComplete(CheckDetailsCompleteBusinessVat(fastTrackRequest, continueUrl), fastTrackRequest)

          case AgentFastTrackRequest(None, _, _, _, _) =>
            goto(CheckDetailsNoClientType(fastTrackRequest, continueUrl))
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
      getAgentLink: GetAgentLink): Future[State] =
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
                           _              <- createInvitation(arn, invitation)
                           invitationLink <- getAgentLink(arn, fastTrackRequest.clientType)
                           result <- fastTrackRequest.clientType match {
                                      case Some(ClientType.personal) =>
                                        goto(InvitationSentPersonal(invitationLink, continueUrl))
                                      case Some(ClientType.business) =>
                                        goto(InvitationSentBusiness(invitationLink, continueUrl))
                                    }
                         } yield result
                     }
                 }
      } yield result

    def checkedDetailsNoClientType(agent: AuthorisedAgent) = Transition {
      case CheckDetailsNoClientType(fastTrackRequest, continueUrl) =>
        goto(SelectClientType(fastTrackRequest, continueUrl))
    }

    def checkedDetailsNoKnownFact(agent: AuthorisedAgent) = Transition {
      case CheckDetailsNoKnownFact(fastTrackRequest, continueUrl) =>
        goto(MoreDetails(fastTrackRequest, continueUrl))
    }

    //FIXME:
//      case PendingInvitationExists(ftr, continueUrl) => goto(MoreDetails(ftr, continueUrl))
//
//      case ActiveAuthorisationExists(ftr, continueUrl) => goto(MoreDetails(ftr, continueUrl))
//
//      case KnownFactNotMatched(ftr, continueUrl) => goto(MoreDetails(ftr, continueUrl))
//
//      case ClientNotSignedUp(ftr, continueUrl) => goto(MoreDetails(ftr, continueUrl))

    def checkedDetailsChangeInformation(agent: AuthorisedAgent) = Transition {
      case CheckDetailsCompleteItsa(ftRequest, continueUrl) =>
        goto(IdentifyPersonalClient(ftRequest, continueUrl))

      case CheckDetailsCompletePir(ftRequest, continueUrl) =>
        goto(IdentifyPersonalClient(ftRequest, continueUrl))

      case CheckDetailsCompletePersonalVat(ftRequest, continueUrl) =>
        goto(IdentifyPersonalClient(ftRequest, continueUrl))

      case CheckDetailsCompleteBusinessVat(ftRequest, continueUrl) =>
        goto(IdentifyBusinessClient(ftRequest, continueUrl))

      case PendingInvitationExists(ftRequest, continueUrl) =>
        if (ftRequest.clientType.contains(personal)) {
          goto(IdentifyPersonalClient(ftRequest, continueUrl))
        } else goto(IdentifyBusinessClient(ftRequest, continueUrl))

      case ActiveAuthorisationExists(ftRequest, continueUrl) =>
        if (ftRequest.clientType.contains(personal)) {
          goto(IdentifyPersonalClient(ftRequest, continueUrl))
        } else goto(IdentifyBusinessClient(ftRequest, continueUrl))

      case KnownFactNotMatched(ftr, continueUrl) =>
        if (ftr.clientType.contains(personal)) {
          goto(IdentifyPersonalClient(ftr, continueUrl))
        } else goto(IdentifyBusinessClient(ftr, continueUrl))

      case ClientNotSignedUp(ftr, continueUrl) =>
        if (ftr.clientType.contains(personal)) {
          goto(IdentifyPersonalClient(ftr, continueUrl))
        } else goto(IdentifyBusinessClient(ftr, continueUrl))
    }

    def checkedDetailsAllInformation(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      featureFlags: FeatureFlags)(agent: AuthorisedAgent)(confirmation: Confirmation) = Transition {
      case CheckDetailsCompleteItsa(fastTrackRequest, continueUrl) => {
        val checkAndGotoInvitationSent = checkIfPendingOrActiveAndGoto(
          fastTrackRequest,
          agent.arn,
          ItsaInvitation(Nino(fastTrackRequest.clientIdentifier), fastTrackRequest.knownFact.map(Postcode.apply)),
          continueUrl
        )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink)
        if (confirmation.choice) {
          if (featureFlags.showKfcMtdIt) {
            checkPostcodeMatches(Nino(fastTrackRequest.clientIdentifier), fastTrackRequest.knownFact.getOrElse(""))
              .flatMap {
                case Some(true)  => checkAndGotoInvitationSent
                case Some(false) => goto(KnownFactNotMatched(fastTrackRequest, continueUrl))
                case None        => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
              }
          } else {
            checkAndGotoInvitationSent
          }
        } else {
          goto(IdentifyPersonalClient(fastTrackRequest, continueUrl))
        }
      }

      case CheckDetailsCompletePir(fastTrackRequest, continueUrl) if featureFlags.showKfcPersonalIncome =>
        if (confirmation.choice) {
          checkDobMatches(
            Nino(fastTrackRequest.clientIdentifier),
            LocalDate.parse(fastTrackRequest.knownFact.getOrElse("")))
            .flatMap {
              case Some(true) =>
                checkIfPendingOrActiveAndGoto(
                  fastTrackRequest,
                  agent.arn,
                  PirInvitation(Nino(fastTrackRequest.clientIdentifier), fastTrackRequest.knownFact.map(DOB.apply)),
                  continueUrl
                )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink)
              case Some(false) => goto(KnownFactNotMatched(fastTrackRequest, continueUrl))
              case None        => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
            }
        } else goto(IdentifyPersonalClient(fastTrackRequest, continueUrl))

      case CheckDetailsCompletePir(fastTrackRequest, continueUrl) if !featureFlags.showKfcPersonalIncome =>
        if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(
            fastTrackRequest,
            agent.arn,
            PirInvitation(Nino(fastTrackRequest.clientIdentifier), fastTrackRequest.knownFact.map(DOB.apply)),
            continueUrl
          )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink)
        } else goto(IdentifyPersonalClient(fastTrackRequest, continueUrl))

      case CheckDetailsCompletePersonalVat(fastTrackRequest, continueUrl) if featureFlags.showKfcMtdVat =>
        if (confirmation.choice) {
          checkRegDateMatches(
            Vrn(fastTrackRequest.clientIdentifier),
            LocalDate.parse(fastTrackRequest.knownFact.getOrElse("")))
            .flatMap {
              case Some(204) =>
                checkIfPendingOrActiveAndGoto(
                  fastTrackRequest,
                  agent.arn,
                  VatInvitation(
                    Some(personal),
                    Vrn(fastTrackRequest.clientIdentifier),
                    fastTrackRequest.knownFact.map(VatRegDate.apply)),
                  continueUrl
                )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink)
              case Some(_) => goto(KnownFactNotMatched(fastTrackRequest, continueUrl))
              case None    => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
            }
        } else goto(IdentifyPersonalClient(fastTrackRequest, continueUrl))

      case CheckDetailsCompletePersonalVat(fastTrackRequest, continueUrl) if !featureFlags.showKfcMtdVat =>
        if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(
            fastTrackRequest,
            agent.arn,
            VatInvitation(
              Some(personal),
              Vrn(fastTrackRequest.clientIdentifier),
              fastTrackRequest.knownFact.map(VatRegDate.apply)),
            continueUrl
          )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink)
        } else goto(IdentifyPersonalClient(fastTrackRequest, continueUrl))

      case CheckDetailsCompleteBusinessVat(fastTrackRequest, continueUrl) if featureFlags.showKfcMtdVat =>
        if (confirmation.choice) {
          checkRegDateMatches(
            Vrn(fastTrackRequest.clientIdentifier),
            LocalDate.parse(fastTrackRequest.knownFact.getOrElse("")))
            .flatMap {
              case Some(204) =>
                checkIfPendingOrActiveAndGoto(
                  fastTrackRequest,
                  agent.arn,
                  VatInvitation(
                    Some(ClientType.business),
                    Vrn(fastTrackRequest.clientIdentifier),
                    fastTrackRequest.knownFact.map(VatRegDate.apply)),
                  continueUrl
                )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink)
              case Some(_) => goto(KnownFactNotMatched(fastTrackRequest, continueUrl))
              case None    => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
            }
        } else goto(IdentifyBusinessClient(fastTrackRequest, continueUrl))

      case CheckDetailsCompleteBusinessVat(fastTrackRequest, continueUrl) if !featureFlags.showKfcMtdVat =>
        if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(
            fastTrackRequest,
            agent.arn,
            VatInvitation(
              Some(ClientType.business),
              Vrn(fastTrackRequest.clientIdentifier),
              fastTrackRequest.knownFact.map(VatRegDate.apply)),
            continueUrl
          )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink)
        } else goto(IdentifyBusinessClient(fastTrackRequest, continueUrl))
    }

    def identifiedClientItsa(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      featureFlags: FeatureFlags)(agent: AuthorisedAgent)(itsaClient: ItsaClient) = Transition {
      case IdentifyPersonalClient(ftr, continueURL) =>
        val newState = CheckDetailsCompleteItsa(
          ftr.copy(clientIdentifier = itsaClient.clientIdentifier, knownFact = itsaClient.postcode),
          continueURL)
        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(Confirmation(true))
          .apply(newState)
    }

    def identifiedClientIrv(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      featureFlags: FeatureFlags)(agent: AuthorisedAgent)(irvClient: IrvClient) = Transition {
      case IdentifyPersonalClient(ftr, continueURL) =>
        val newState =
          CheckDetailsCompletePir(
            ftr.copy(clientIdentifier = irvClient.clientIdentifier, knownFact = irvClient.dob),
            continueURL)
        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(Confirmation(true))
          .apply(newState)
    }

    def identifiedClientVat(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      featureFlags: FeatureFlags)(agent: AuthorisedAgent)(vatClient: VatClient) = Transition {
      case IdentifyPersonalClient(ftr, continueURL) =>
        val newState = CheckDetailsCompletePersonalVat(
          ftr.copy(clientIdentifier = vatClient.clientIdentifier, knownFact = vatClient.registrationDate),
          continueURL)
        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(Confirmation(true))
          .apply(newState)
      case IdentifyBusinessClient(ftr, continueURL) =>
        val newState = CheckDetailsCompleteBusinessVat(
          ftr.copy(clientIdentifier = vatClient.clientIdentifier, knownFact = vatClient.registrationDate),
          continueURL)
        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(Confirmation(true))
          .apply(newState)
    }

    def moreDetailsItsa(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      featureFlags: FeatureFlags)(agent: AuthorisedAgent)(suppliedKnownFact: Option[String]) = Transition {
      case MoreDetails(ftr, continueUrl) =>
        val newState = CheckDetailsCompleteItsa(ftr.copy(knownFact = suppliedKnownFact), continueUrl)
        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(Confirmation(true))
          .apply(newState)
    }

    def moreDetailsIrv(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      featureFlags: FeatureFlags)(agent: AuthorisedAgent)(suppliedKnownFact: Option[String]) = Transition {
      case MoreDetails(ftr, continueUrl) =>
        val newState = CheckDetailsCompletePir(ftr.copy(knownFact = suppliedKnownFact), continueUrl)
        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(Confirmation(true))
          .apply(newState)
    }

    def moreDetailsVat(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      featureFlags: FeatureFlags)(agent: AuthorisedAgent)(suppliedKnownFact: Option[String]) = Transition {
      case MoreDetails(ftRequest, continueUrl) => {
        val checkDetailsState =
          if (ftRequest.clientType.contains(personal))
            CheckDetailsCompletePersonalVat
          else
            CheckDetailsCompleteBusinessVat

        val newState = checkDetailsState(ftRequest.copy(knownFact = suppliedKnownFact), continueUrl)

        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(Confirmation(true))
          .apply(newState)
      }
    }

    def selectedClientType(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      featureFlags: FeatureFlags)(agent: AuthorisedAgent)(suppliedClientType: ClientType) = Transition {

      case SelectClientType(ftr, continueUrl) =>
        val isKnownFactRequired = ftr.knownFact.isDefined || !featureFlags.isKfcFlagOnForService(ftr.service)
        if (isKnownFactRequired) {
          val completeState =
            if (ftr.clientType.contains(personal)) CheckDetailsCompletePersonalVat
            else CheckDetailsCompleteBusinessVat
          val newState =
            completeState(ftr.copy(clientType = Some(suppliedClientType)), continueUrl)

          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(Confirmation(true))
            .apply(newState)

        } else {
          val newState = CheckDetailsNoKnownFact(ftr.copy(clientType = Some(suppliedClientType)), continueUrl)

          checkedDetailsNoKnownFact(agent)
            .apply(newState)
        }
    }

  }
}
