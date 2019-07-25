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
import play.api.Logger
import play.api.mvc.Request
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.Transitions.CreateInvitation
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.Transitions.CheckDOBMatches
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State.InvalidTrustState
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.personal
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, Utr, Vrn}
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

    case class CheckDetailsCompleteTrust(
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

    case class IdentifyTrustClient(
      originalFastTrackRequest: AgentFastTrackRequest,
      fastTrackRequest: AgentFastTrackRequest,
      continueUrl: Option[String])
        extends State

    case class ConfirmClientTrust(
      originalFastTrackRequest: AgentFastTrackRequest,
      fastTrackRequest: AgentFastTrackRequest,
      continueUrl: Option[String],
      trustName: String)
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

    case class TrustNotFound(
      originalFastTrackRequest: AgentFastTrackRequest,
      fastTrackRequest: AgentFastTrackRequest,
      continueUrl: Option[String])
        extends State
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

    type GetTrustName = Utr => Future[TrustResponse]

    def prologue(failureUrl: Option[String], refererUrl: Option[String]) = Transition {
      case _ => goto(Prologue(failureUrl, refererUrl))
    }

    def start(continueUrl: Option[String])(agent: AuthorisedAgent)(
      fastTrackRequest: AgentFastTrackRequest)(implicit request: Request[Any], hc: HeaderCarrier) = Transition {
      case _ =>
        fastTrackRequest match {
          case AgentFastTrackRequest(_, HMRCMTDIT, _, _, _) =>
            val updatedPersonalRequest = fastTrackRequest.copy(clientType = Some(personal))
            goto(
              fastTrackRequest.knownFact.fold(
                CheckDetailsNoPostcode(updatedPersonalRequest, updatedPersonalRequest, continueUrl): State)(_ =>
                CheckDetailsCompleteItsa(updatedPersonalRequest, updatedPersonalRequest, continueUrl)))

          case AgentFastTrackRequest(_, HMRCPIR, _, _, _) =>
            val updatedPersonalRequest = fastTrackRequest.copy(clientType = Some(personal))
            goto(
              fastTrackRequest.knownFact.fold(
                CheckDetailsNoDob(updatedPersonalRequest, updatedPersonalRequest, continueUrl): State)(_ =>
                CheckDetailsCompleteIrv(updatedPersonalRequest, updatedPersonalRequest, continueUrl)))

          case AgentFastTrackRequest(Some(ClientType.personal), HMRCMTDVAT, _, _, _) =>
            goto(
              fastTrackRequest.knownFact.fold(
                CheckDetailsNoVatRegDate(fastTrackRequest, fastTrackRequest, continueUrl): State)(_ =>
                CheckDetailsCompletePersonalVat(fastTrackRequest, fastTrackRequest, continueUrl)))

          case AgentFastTrackRequest(Some(ClientType.business), HMRCMTDVAT, _, _, _) =>
            goto(
              fastTrackRequest.knownFact.fold(
                CheckDetailsNoVatRegDate(fastTrackRequest, fastTrackRequest, continueUrl): State)(_ =>
                CheckDetailsCompleteBusinessVat(fastTrackRequest, fastTrackRequest, continueUrl)))

          case AgentFastTrackRequest(None, HMRCMTDVAT, _, _, _) =>
            goto(CheckDetailsNoClientTypeVat(fastTrackRequest, fastTrackRequest, continueUrl))

          case AgentFastTrackRequest(Some(ClientType.business), TRUST, _, _, _) =>
            goto(CheckDetailsCompleteTrust(fastTrackRequest, fastTrackRequest, continueUrl))
        }
    }

    def checkIfPendingOrActiveAndGoto(
      fastTrackRequest: AgentFastTrackRequest,
      arn: Arn,
      invitation: Invitation,
      continueUrl: Option[String])(
      hasPendingInvitationsFor: HasPendingInvitations,
      hasActiveRelationshipFor: HasActiveRelationship
    )(createInvitation: CreateInvitation, getAgentLink: GetAgentLink, getAgencyEmail: GetAgencyEmail): Future[State] =
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

        case CheckDetailsCompleteTrust(originalFtr, ftr, continueUrl) =>
          goto(IdentifyTrustClient(originalFtr, ftr, continueUrl))

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
      hasActiveRelationship: HasActiveRelationship)(agent: AuthorisedAgent)(confirmation: Confirmation) = Transition {
      case CheckDetailsCompleteItsa(originalFtr, fastTrackRequest, continueUrl) => {
        if (confirmation.choice) {
          checkPostcodeMatches(Nino(fastTrackRequest.clientIdentifier), fastTrackRequest.knownFact.getOrElse(""))
            .flatMap {
              case Some(true) =>
                checkIfPendingOrActiveAndGoto(
                  fastTrackRequest,
                  agent.arn,
                  ItsaInvitation(
                    Nino(fastTrackRequest.clientIdentifier),
                    Postcode(fastTrackRequest.knownFact.getOrElse(""))),
                  continueUrl
                )(hasPendingInvitations, hasActiveRelationship)(createInvitation, getAgentLink, getAgencyEmail)
              case Some(false) => goto(KnownFactNotMatched(originalFtr, fastTrackRequest, continueUrl))
              case None        => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
            }
        } else goto(IdentifyPersonalClient(originalFtr, fastTrackRequest, continueUrl))
      }

      case CheckDetailsCompleteIrv(originalFtr, fastTrackRequest, continueUrl) =>
        if (confirmation.choice) {
          val knownFact = fastTrackRequest.knownFact.getOrElse("")
          checkDobMatches(Nino(fastTrackRequest.clientIdentifier), LocalDate.parse(knownFact))
            .flatMap {
              case Some(true) =>
                checkIfPendingOrActiveAndGoto(
                  fastTrackRequest,
                  agent.arn,
                  PirInvitation(Nino(fastTrackRequest.clientIdentifier), DOB(knownFact)),
                  continueUrl
                )(hasPendingInvitations, hasActiveRelationship)(createInvitation, getAgentLink, getAgencyEmail)
              case Some(false) => goto(KnownFactNotMatched(originalFtr, fastTrackRequest, continueUrl))
              case None        => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
            }
        } else goto(IdentifyPersonalClient(originalFtr, fastTrackRequest, continueUrl))

      case CheckDetailsCompletePersonalVat(originalFtr, fastTrackRequest, continueUrl) =>
        if (confirmation.choice) {
          val knownFact = fastTrackRequest.knownFact.getOrElse("")
          checkRegDateMatches(Vrn(fastTrackRequest.clientIdentifier), LocalDate.parse(knownFact))
            .flatMap {
              case Some(204) =>
                checkIfPendingOrActiveAndGoto(
                  fastTrackRequest,
                  agent.arn,
                  VatInvitation(Some(personal), Vrn(fastTrackRequest.clientIdentifier), VatRegDate(knownFact)),
                  continueUrl
                )(hasPendingInvitations, hasActiveRelationship)(createInvitation, getAgentLink, getAgencyEmail)
              case Some(_) => goto(KnownFactNotMatched(originalFtr, fastTrackRequest, continueUrl))
              case None    => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
            }
        } else goto(IdentifyPersonalClient(originalFtr, fastTrackRequest, continueUrl))

      case CheckDetailsCompleteBusinessVat(originalFtr, fastTrackRequest, continueUrl) =>
        if (confirmation.choice) {
          val knownFact = fastTrackRequest.knownFact.getOrElse("")
          checkRegDateMatches(Vrn(fastTrackRequest.clientIdentifier), LocalDate.parse(knownFact))
            .flatMap {
              case Some(204) =>
                checkIfPendingOrActiveAndGoto(
                  fastTrackRequest,
                  agent.arn,
                  VatInvitation(
                    Some(ClientType.business),
                    Vrn(fastTrackRequest.clientIdentifier),
                    VatRegDate(knownFact)),
                  continueUrl
                )(hasPendingInvitations, hasActiveRelationship)(createInvitation, getAgentLink, getAgencyEmail)
              case Some(_) => goto(KnownFactNotMatched(originalFtr, fastTrackRequest, continueUrl))
              case None    => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
            }
        } else goto(IdentifyBusinessClient(originalFtr, fastTrackRequest, continueUrl))

      case CheckDetailsCompleteTrust(originalFtr, fastTrackRequest, continueUrl) =>
        if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(
            fastTrackRequest,
            agent.arn,
            TrustInvitation(Utr(fastTrackRequest.clientIdentifier)),
            continueUrl
          )(hasPendingInvitations, hasActiveRelationship)(createInvitation, getAgentLink, getAgencyEmail)
        } else goto(IdentifyTrustClient(originalFtr, fastTrackRequest, continueUrl))
    }

    def identifiedClientItsa(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(
      hasActiveRelationship: HasActiveRelationship)(agent: AuthorisedAgent)(itsaClient: ItsaClient) = Transition {
      case IdentifyPersonalClient(originalFtr, ftr, continueUrl) =>
        val newState = CheckDetailsCompleteItsa(
          originalFtr,
          ftr.copy(clientIdentifier = itsaClient.clientIdentifier, knownFact = Some(itsaClient.postcode)),
          continueUrl)
        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(agent)(Confirmation(true))
          .apply(newState)
    }

    def identifiedClientIrv(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(
      hasActiveRelationship: HasActiveRelationship)(agent: AuthorisedAgent)(irvClient: IrvClient) = Transition {
      case IdentifyPersonalClient(originalFtr, ftr, continueUrl) =>
        val newState =
          CheckDetailsCompleteIrv(
            originalFtr,
            ftr.copy(clientIdentifier = irvClient.clientIdentifier, knownFact = Some(irvClient.dob)),
            continueUrl)
        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(agent)(Confirmation(true))
          .apply(newState)
    }

    def identifiedClientVat(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(
      hasActiveRelationship: HasActiveRelationship)(agent: AuthorisedAgent)(vatClient: VatClient) = Transition {
      case IdentifyPersonalClient(originalFtr, ftr, continueUrl) =>
        val newState = CheckDetailsCompletePersonalVat(
          originalFtr,
          ftr.copy(clientIdentifier = vatClient.clientIdentifier, knownFact = Some(vatClient.registrationDate)),
          continueUrl)
        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(agent)(Confirmation(true))
          .apply(newState)
      case IdentifyBusinessClient(originalFtr, ftr, continueUrl) =>
        val newState = CheckDetailsCompleteBusinessVat(
          originalFtr,
          ftr.copy(clientIdentifier = vatClient.clientIdentifier, knownFact = Some(vatClient.registrationDate)),
          continueUrl)
        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(agent)(Confirmation(true))
          .apply(newState)
    }

    def showConfirmTrustClient(getTrustName: GetTrustName)(agent: AuthorisedAgent)(trustClient: TrustClient) =
      Transition {
        case IdentifyTrustClient(originalFtr, ftr, continueUrl) =>
          getTrustName(trustClient.utr).flatMap { trustResponse =>
            trustResponse.response match {
              case Right(TrustName(name)) =>
                goto(ConfirmClientTrust(originalFtr, ftr, continueUrl, name))
              case Left(invalidTrust) =>
                Logger.warn(s"Des returned $invalidTrust response for utr: ${trustClient.utr}")
                goto(TrustNotFound(originalFtr, ftr, continueUrl))
            }
          }
      }

    def submitConfirmTrustClient(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(
      hasActiveRelationship: HasActiveRelationship)(agent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {
        case ConfirmClientTrust(originalFtr, ftr, continueUrl, trustName) =>
          if (confirmation.choice) {
            checkIfPendingOrActiveAndGoto(
              ftr,
              agent.arn,
              TrustInvitation(Utr(ftr.clientIdentifier)),
              continueUrl
            )(hasPendingInvitations, hasActiveRelationship)(createInvitation, getAgentLink, getAgencyEmail)
          } else {
            goto(IdentifyTrustClient(originalFtr, ftr, continueUrl))
          }
      }

    def moreDetailsItsa(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(
      hasActiveRelationship: HasActiveRelationship)(agent: AuthorisedAgent)(suppliedKnownFact: String) = Transition {
      case NoPostcode(originalFtr, ftr, continueUrl) =>
        val newState = CheckDetailsCompleteItsa(originalFtr, ftr.copy(knownFact = Some(suppliedKnownFact)), continueUrl)
        checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
          getAgentLink)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(agent)(Confirmation(true))
          .apply(newState)
    }

    def moreDetailsIrv(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(
      hasActiveRelationship: HasActiveRelationship)(agent: AuthorisedAgent)(suppliedKnownFact: String) =
      Transition {
        case NoDob(originalFtr, ftr, continueUrl) =>
          val newState =
            CheckDetailsCompleteIrv(originalFtr, ftr.copy(knownFact = Some(suppliedKnownFact)), continueUrl)
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(agent)(Confirmation(true))
            .apply(newState)
      }

    def moreDetailsVat(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(
      hasActiveRelationship: HasActiveRelationship)(agent: AuthorisedAgent)(suppliedKnownFact: String) =
      Transition {
        case NoVatRegDate(originalFtr, ftRequest, continueUrl) => {
          val checkDetailsState =
            if (ftRequest.clientType.contains(personal))
              CheckDetailsCompletePersonalVat
            else
              CheckDetailsCompleteBusinessVat

          val newState =
            checkDetailsState(originalFtr, ftRequest.copy(knownFact = Some(suppliedKnownFact)), continueUrl)

          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(agent)(Confirmation(true))
            .apply(newState)
        }
      }

    def tryAgainNotMatchedKnownFact(agent: AuthorisedAgent) =
      Transition {
        case KnownFactNotMatched(originalFtr, fastTrackRequest, continueUrl) =>
          val ftrWithoutKF = fastTrackRequest.copy(knownFact = None)

          def stateForMissingKnownFact(forService: String) =
            forService match {
              case HMRCMTDVAT => NoVatRegDate(originalFtr, ftrWithoutKF, continueUrl)
              case HMRCMTDIT  => NoPostcode(originalFtr, ftrWithoutKF, continueUrl)
              case HMRCPIR    => NoDob(originalFtr, ftrWithoutKF, continueUrl)
            }

          val tryAgainState = originalFtr match {
            case AgentFastTrackRequest(None, HMRCMTDVAT, _, _, _) =>
              val ftrWithoutKFOrClientType = ftrWithoutKF.copy(clientType = None)
              SelectClientTypeVat(originalFtr, ftrWithoutKFOrClientType, continueUrl)
            case AgentFastTrackRequest(Some(_), HMRCMTDVAT, _, _, None) =>
              stateForMissingKnownFact(HMRCMTDVAT)
            case AgentFastTrackRequest(_, _, _, _, Some(_)) =>
              TryAgainWithoutFastTrack
            case AgentFastTrackRequest(_, service, _, _, None) =>
              stateForMissingKnownFact(service)
          }

          goto(tryAgainState)

        case TrustNotFound(originalFtr, fastTrackRequest, continueUrl) =>
          goto(IdentifyTrustClient(originalFtr, fastTrackRequest, continueUrl))
      }

    def selectedClientType(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(
      hasActiveRelationship: HasActiveRelationship)(agent: AuthorisedAgent)(suppliedClientType: String) =
      Transition {
        case SelectClientTypeVat(originalFtr, ftr, continueUrl) =>
          val isKnownFactRequired = ftr.knownFact.isDefined
          if (isKnownFactRequired) {
            val completeState =
              if (ftr.clientType.contains(personal)) CheckDetailsCompletePersonalVat
              else CheckDetailsCompleteBusinessVat
            val newState =
              completeState(
                originalFtr,
                ftr.copy(clientType = Some(ClientType.toEnum(suppliedClientType))),
                continueUrl)
            checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
              getAgentLink)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(agent)(Confirmation(true))
              .apply(newState)
          } else {
            val newState =
              CheckDetailsNoVatRegDate(
                originalFtr,
                ftr.copy(clientType = Some(ClientType.toEnum(suppliedClientType))),
                continueUrl)

            checkedDetailsNoKnownFact(agent)
              .apply(newState)
          }
      }

  }
}
