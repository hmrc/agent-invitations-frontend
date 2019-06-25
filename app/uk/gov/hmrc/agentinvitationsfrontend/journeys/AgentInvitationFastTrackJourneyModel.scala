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

  val root: State = State.Prologue(None, None)

  /* State should contain only minimal set of data required to proceed */
  object State {
    case class Prologue(failureUrl: Option[String], refererUrl: Option[String]) extends State

    case class CheckDetailsNoPostcode(
      originalFastTrackRequest: AgentFastTrackRequest,
      fastTrackRequest: AgentFastTrackRequest,
      continueUrl: Option[String])
        extends State

    case class CheckDetailsNoDob(
      originalFastTrackRequest: AgentFastTrackRequest,
      fastTrackRequest: AgentFastTrackRequest,
      continueUrl: Option[String])
        extends State

    case class CheckDetailsNoVatRegDate(
      originalFastTrackRequest: AgentFastTrackRequest,
      fastTrackRequest: AgentFastTrackRequest,
      continueUrl: Option[String])
        extends State
    case class CheckDetailsNoClientTypeVat(
      originalFastTrackRequest: AgentFastTrackRequest,
      fastTrackRequest: AgentFastTrackRequest,
      continueUrl: Option[String])
        extends State

    case class CheckDetailsCompleteItsa(
      originalFastTrackRequest: AgentFastTrackRequest,
      fastTrackRequest: AgentFastTrackRequest,
      continueUrl: Option[String])
        extends State

    case class CheckDetailsCompleteIrv(
      originalFastTrackRequest: AgentFastTrackRequest,
      fastTrackRequest: AgentFastTrackRequest,
      continueUrl: Option[String])
        extends State

    case class CheckDetailsCompletePersonalVat(
      originalFastTrackRequest: AgentFastTrackRequest,
      fastTrackRequest: AgentFastTrackRequest,
      continueUrl: Option[String])
        extends State

    case class CheckDetailsCompleteBusinessVat(
      originalFastTrackRequest: AgentFastTrackRequest,
      fastTrackRequest: AgentFastTrackRequest,
      continueUrl: Option[String])
        extends State

    case class NoPostcode(
      originalFastTrackRequest: AgentFastTrackRequest,
      fastTrackRequest: AgentFastTrackRequest,
      continueUrl: Option[String])
        extends State
    case class NoDob(
      originalFastTrackRequest: AgentFastTrackRequest,
      fastTrackRequest: AgentFastTrackRequest,
      continueUrl: Option[String])
        extends State
    case class NoVatRegDate(
      originalFastTrackRequest: AgentFastTrackRequest,
      fastTrackRequest: AgentFastTrackRequest,
      continueUrl: Option[String])
        extends State

    case class SelectClientTypeVat(
      originalFastTrackRequest: AgentFastTrackRequest,
      fastTrackRequest: AgentFastTrackRequest,
      continueUrl: Option[String])
        extends State

    case class IdentifyPersonalClient(
      originalFastTrackRequest: AgentFastTrackRequest,
      fastTrackRequest: AgentFastTrackRequest,
      continueUrl: Option[String])
        extends State
    case class IdentifyBusinessClient(
      originalFastTrackRequest: AgentFastTrackRequest,
      fastTrackRequest: AgentFastTrackRequest,
      continueUrl: Option[String])
        extends State

    case class InvitationSentPersonal(invitationLink: String, continueUrl: Option[String], agencyEmail: String)
        extends State
    case class InvitationSentBusiness(invitationLink: String, continueUrl: Option[String], agencyEmail: String)
        extends State

    case class PendingInvitationExists(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
        extends State
    case class ActiveAuthorisationExists(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
        extends State
    case class KnownFactNotMatched(
      originalFastTrackRequest: AgentFastTrackRequest,
      fastTrackRequest: AgentFastTrackRequest,
      continueUrl: Option[String])
        extends State
    case class ClientNotSignedUp(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State
    case object TryAgainWithoutFastTrack extends State
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

    def prologue(failureUrl: Option[String], refererUrl: Option[String]) = Transition {
      case _ => goto(Prologue(failureUrl, refererUrl))
    }

    def start(features: FeatureFlags)(continueUrl: Option[String])(agent: AuthorisedAgent)(
      fastTrackRequest: AgentFastTrackRequest)(implicit request: Request[Any], hc: HeaderCarrier) = Transition {
      case _ =>
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
              CheckDetailsNoPostcode(updatedPersonalRequest, updatedPersonalRequest, continueUrl),
              CheckDetailsCompleteItsa(updatedPersonalRequest, updatedPersonalRequest, continueUrl),
              updatedPersonalRequest
            )
          }

          case AgentFastTrackRequest(_, HMRCPIR, _, _, _) => {
            val updatedPersonalRequest = fastTrackRequest.copy(clientType = Some(personal))
            gotoNoKnownFactOrComplete(
              CheckDetailsNoDob(updatedPersonalRequest, updatedPersonalRequest, continueUrl),
              CheckDetailsCompleteIrv(updatedPersonalRequest, updatedPersonalRequest, continueUrl),
              updatedPersonalRequest
            )
          }

          case AgentFastTrackRequest(Some(ClientType.personal), HMRCMTDVAT, _, _, _) =>
            gotoNoKnownFactOrComplete(
              CheckDetailsNoVatRegDate(fastTrackRequest, fastTrackRequest, continueUrl),
              CheckDetailsCompletePersonalVat(fastTrackRequest, fastTrackRequest, continueUrl),
              fastTrackRequest
            )

          case AgentFastTrackRequest(Some(ClientType.business), HMRCMTDVAT, _, _, _) =>
            gotoNoKnownFactOrComplete(
              CheckDetailsNoVatRegDate(fastTrackRequest, fastTrackRequest, continueUrl),
              CheckDetailsCompleteBusinessVat(fastTrackRequest, fastTrackRequest, continueUrl),
              fastTrackRequest
            )

          case AgentFastTrackRequest(None, HMRCMTDVAT, _, _, _) =>
            goto(CheckDetailsNoClientTypeVat(fastTrackRequest, fastTrackRequest, continueUrl))
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
      case CheckDetailsNoClientTypeVat(originalFastTrackRequest, fastTrackRequest, continueUrl) =>
        goto(SelectClientTypeVat(originalFastTrackRequest, fastTrackRequest, continueUrl))

      case NoVatRegDate(originalFastTrackRequest, fastTrackRequest, continueUrl) =>
        goto(SelectClientTypeVat(originalFastTrackRequest, fastTrackRequest, continueUrl))
    }

    def checkedDetailsNoKnownFact(agent: AuthorisedAgent) =
      Transition {
        case CheckDetailsNoPostcode(originalFastTrackRequest, fastTrackRequest, continueUrl) =>
          goto(NoPostcode(originalFastTrackRequest, fastTrackRequest, continueUrl))

        case CheckDetailsNoDob(originalFastTrackRequest, fastTrackRequest, continueUrl) =>
          goto(NoDob(originalFastTrackRequest, fastTrackRequest, continueUrl))

        case CheckDetailsNoVatRegDate(originalFastTrackRequest, fastTrackRequest, continueUrl) =>
          goto(NoVatRegDate(originalFastTrackRequest, fastTrackRequest, continueUrl))
      }

    def checkedDetailsChangeInformation(agent: AuthorisedAgent) = {
      def gotoIdentifyClient(
        originalFtr: AgentFastTrackRequest,
        ftRequest: AgentFastTrackRequest,
        continueUrl: Option[String]) =
        if (ftRequest.clientType.contains(personal)) {
          goto(IdentifyPersonalClient(originalFtr, ftRequest, continueUrl))
        } else {
          goto(IdentifyBusinessClient(originalFtr, ftRequest, continueUrl))
        }

      Transition {
        case CheckDetailsCompleteItsa(originalFtr, ftr, continueUrl) =>
          goto(IdentifyPersonalClient(originalFtr, ftr, continueUrl))

        case CheckDetailsCompleteIrv(originalFtr, ftr, continueUrl) =>
          goto(IdentifyPersonalClient(originalFtr, ftr, continueUrl))

        case CheckDetailsCompletePersonalVat(originalFtr, ftr, continueUrl) =>
          goto(IdentifyPersonalClient(originalFtr, ftr, continueUrl))

        case CheckDetailsCompleteBusinessVat(originalFtr, ftr, continueUrl) =>
          goto(IdentifyBusinessClient(originalFtr, ftr, continueUrl))

        case CheckDetailsNoPostcode(originalFtr, ftr, continueUrl) =>
          goto(IdentifyPersonalClient(originalFtr, ftr, continueUrl))

        case CheckDetailsNoDob(originalFtr, ftr, continueUrl) =>
          goto(IdentifyPersonalClient(originalFtr, ftr, continueUrl))

        case CheckDetailsNoVatRegDate(originalFtr, ftr, continueUrl) =>
          gotoIdentifyClient(originalFtr, ftr, continueUrl)

        case CheckDetailsNoClientTypeVat(originalFtr, ftr, continueUrl) =>
          gotoIdentifyClient(originalFtr, ftr, continueUrl)
      }
    }

    def checkedDetailsAllInformation(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(
      hasActiveRelationship: HasActiveRelationship)(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(
      confirmation: Confirmation) = Transition {
      case CheckDetailsCompleteItsa(originalFtr, fastTrackRequest, continueUrl) => {
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
                case Some(false) => goto(KnownFactNotMatched(originalFtr, fastTrackRequest, continueUrl))
                case None        => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
              }
          } else checkAndGotoInvitationSent
        } else goto(IdentifyPersonalClient(originalFtr, fastTrackRequest, continueUrl))
      }

      case CheckDetailsCompleteIrv(originalFtr, fastTrackRequest, continueUrl) =>
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
                case Some(false) => goto(KnownFactNotMatched(originalFtr, fastTrackRequest, continueUrl))
                case None        => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
              }
          } else checkAndGotoInvitationSent
        } else goto(IdentifyPersonalClient(originalFtr, fastTrackRequest, continueUrl))

      case CheckDetailsCompletePersonalVat(originalFtr, fastTrackRequest, continueUrl) =>
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
                case Some(_)   => goto(KnownFactNotMatched(originalFtr, fastTrackRequest, continueUrl))
                case None      => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
              }
          } else checkAndGotoInvitationSent
        } else goto(IdentifyPersonalClient(originalFtr, fastTrackRequest, continueUrl))

      case CheckDetailsCompleteBusinessVat(originalFtr, fastTrackRequest, continueUrl) =>
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
                case Some(_)   => goto(KnownFactNotMatched(originalFtr, fastTrackRequest, continueUrl))
                case None      => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
              }
          } else checkAndGotoInvitationSent
        } else goto(IdentifyBusinessClient(originalFtr, fastTrackRequest, continueUrl))
    }

    def identifiedClientItsa(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(
      hasActiveRelationship: HasActiveRelationship)(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(
      itsaClient: ItsaClient) = Transition {
      case IdentifyPersonalClient(originalFtr, ftr, continueUrl) =>
        val newState = CheckDetailsCompleteItsa(
          originalFtr,
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
      case IdentifyPersonalClient(originalFtr, ftr, continueUrl) =>
        val newState =
          CheckDetailsCompleteIrv(
            originalFtr,
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
      case IdentifyPersonalClient(originalFtr, ftr, continueUrl) =>
        val newState = CheckDetailsCompletePersonalVat(
          originalFtr,
          ftr.copy(clientIdentifier = vatClient.clientIdentifier, knownFact = vatClient.registrationDate),
          continueUrl)
        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(
          Confirmation(true))
          .apply(newState)
      case IdentifyBusinessClient(originalFtr, ftr, continueUrl) =>
        val newState = CheckDetailsCompleteBusinessVat(
          originalFtr,
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
      case NoPostcode(originalFtr, ftr, continueUrl) =>
        val newState = CheckDetailsCompleteItsa(originalFtr, ftr.copy(knownFact = suppliedKnownFact), continueUrl)
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
      case NoDob(originalFtr, ftr, continueUrl) =>
        val newState = CheckDetailsCompleteIrv(originalFtr, ftr.copy(knownFact = suppliedKnownFact), continueUrl)
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
      case NoVatRegDate(originalFtr, ftRequest, continueUrl) => {
        val checkDetailsState =
          if (ftRequest.clientType.contains(personal))
            CheckDetailsCompletePersonalVat
          else
            CheckDetailsCompleteBusinessVat

        val newState = checkDetailsState(originalFtr, ftRequest.copy(knownFact = suppliedKnownFact), continueUrl)

        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(
          Confirmation(true))
          .apply(newState)
      }
    }

    def tryAgainNotMatchedKnownFact(agent: AuthorisedAgent) =
      Transition {
        case KnownFactNotMatched(originalFtr, fastTrackRequest, continueUrl) =>
          val ftrWithoutKF = fastTrackRequest.copy(knownFact = None)

          def stateForMissingKnownFact(forService: String) =
            forService match {
              case Services.HMRCMTDVAT => NoVatRegDate(originalFtr, ftrWithoutKF, continueUrl)
              case Services.HMRCMTDIT  => NoPostcode(originalFtr, ftrWithoutKF, continueUrl)
              case Services.HMRCPIR    => NoDob(originalFtr, ftrWithoutKF, continueUrl)
            }

          val tryAgainState = originalFtr match {
            case AgentFastTrackRequest(None, Services.HMRCMTDVAT, _, _, _) =>
              val ftrWithoutKFOrClientType = ftrWithoutKF.copy(clientType = None)
              SelectClientTypeVat(originalFtr, ftrWithoutKFOrClientType, continueUrl)
            case AgentFastTrackRequest(Some(_), Services.HMRCMTDVAT, _, _, None) =>
              stateForMissingKnownFact(Services.HMRCMTDVAT)
            case AgentFastTrackRequest(_, _, _, _, Some(_)) =>
              TryAgainWithoutFastTrack
            case AgentFastTrackRequest(_, service, _, _, None) =>
              stateForMissingKnownFact(service)
          }

          goto(tryAgainState)
      }

    def selectedClientType(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(
      hasActiveRelationship: HasActiveRelationship)(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(
      suppliedClientType: ClientType) = Transition {

      case SelectClientTypeVat(originalFtr, ftr, continueUrl) =>
        val isKnownFactRequired = ftr.knownFact.isDefined || !featureFlags.isKfcFlagOnForService(ftr.service)
        if (isKnownFactRequired) {
          val completeState =
            if (ftr.clientType.contains(personal)) CheckDetailsCompletePersonalVat
            else CheckDetailsCompleteBusinessVat
          val newState =
            completeState(originalFtr, ftr.copy(clientType = Some(suppliedClientType)), continueUrl)

          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(
            Confirmation(true))
            .apply(newState)

        } else {
          val newState = CheckDetailsNoVatRegDate(
            originalFtr,
            ftr.copy(clientType = Some(suppliedClientType)),
            continueUrl
          )

          checkedDetailsNoKnownFact(agent)
            .apply(newState)
        }
    }

  }
}
