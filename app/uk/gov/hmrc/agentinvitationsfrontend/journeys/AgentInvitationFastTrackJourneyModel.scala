/*
 * Copyright 2022 HM Revenue & Customs
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
import play.api.Logging
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{Business, Personal, Trust}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models.VatKnownFactCheckResult._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators.{urnPattern, utrPattern}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, CgtRef, InvitationId, PptRef, SuspensionDetails, TrustTaxIdentifier, Urn, Utr, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.fsm.JourneyModel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object AgentInvitationFastTrackJourneyModel extends JourneyModel with Logging {

  sealed trait State

  val root: State = Prologue(None, None)

  case class Prologue(failureUrl: Option[String], refererUrl: Option[String]) extends State

  trait CheckDetails extends State

  case class CheckDetailsNoPostcode(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends CheckDetails

  case class CheckDetailsNoDob(originalFastTrackRequest: AgentFastTrackRequest, fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
      extends CheckDetails

  case class CheckDetailsNoVatRegDate(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends CheckDetails

  case class CheckDetailsNoClientTypeVat(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends CheckDetails

  case class CheckDetailsCompleteItsa(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends CheckDetails

  case class CheckDetailsCompleteIrv(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends CheckDetails

  case class CheckDetailsCompletePersonalVat(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends CheckDetails

  case class CheckDetailsCompleteBusinessVat(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends CheckDetails

  case class CheckDetailsCompleteTrust(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends CheckDetails

  case class CheckDetailsCompleteCgt(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends CheckDetails

  case class CheckDetailsCompletePpt(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends CheckDetails

  trait MissingDetail extends State

  case class NoPostcode(originalFastTrackRequest: AgentFastTrackRequest, fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
      extends MissingDetail

  case class NoDob(originalFastTrackRequest: AgentFastTrackRequest, fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
      extends MissingDetail

  case class NoVatRegDate(originalFastTrackRequest: AgentFastTrackRequest, fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
      extends MissingDetail

  case class NoPptRegDate(originalFastTrackRequest: AgentFastTrackRequest, fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
      extends MissingDetail

  trait ClientTypeState extends State

  case class SelectClientTypeVat(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String],
    isChanging: Boolean = false)
      extends ClientTypeState

  case class SelectClientTypeCgt(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String],
    isChanging: Boolean = false
  ) extends ClientTypeState

  case class SelectClientTypePpt(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String],
    isChanging: Boolean = false
  ) extends ClientTypeState

  trait Identify extends State

  case class IdentifyPersonalClient(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends Identify

  case class IdentifyBusinessClient(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends Identify

  case class IdentifyNoClientTypeClient(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends Identify

  case class IdentifyTrustClient(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends Identify

  case class IdentifyCgtClient(originalFastTrackRequest: AgentFastTrackRequest, fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
      extends Identify

  case class IdentifyPptClient(originalFastTrackRequest: AgentFastTrackRequest, fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
      extends Identify

  case class ConfirmClientTrust(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String],
    trustName: String)
      extends State

  trait InvitationSent extends State

  case class InvitationSentPersonal(invitationLink: String, continueUrl: Option[String], agencyEmail: String, service: String, isAltItsa: Boolean)
      extends InvitationSent

  case class InvitationSentBusiness(invitationLink: String, continueUrl: Option[String], agencyEmail: String, service: String = HMRCMTDVAT)
      extends InvitationSent

  case class PendingInvitationExists(fastTrackRequest: AgentFastTrackRequest, agentLink: String, clientName: String, continueUrl: Option[String])
      extends State

  trait AuthExists extends State
  case class ActiveAuthorisationExists(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends AuthExists
  case class PartialAuthorisationExists(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends AuthExists

  trait NotMatched extends State

  case class KnownFactNotMatched(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends NotMatched

  case class ClientNotSignedUp(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State

  case class ClientNotRegistered(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State

  case class TrustNotFound(originalFastTrackRequest: AgentFastTrackRequest, fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
      extends NotMatched

  case object TryAgainWithoutFastTrack extends State

  case class ConfirmPostcodeCgt(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String],
    postcodeFromDes: Option[String],
    clientName: String)
      extends State

  case class ConfirmCountryCodeCgt(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String],
    countryCode: String,
    clientName: String)
      extends State

  case class ConfirmClientCgt(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String],
    clientName: String)
      extends State

  case class ConfirmRegDatePpt(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String],
    registrationDate: LocalDate,
    clientName: String)
      extends State

  case class ConfirmClientPpt(
    originalFastTrackRequest: AgentFastTrackRequest,
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String],
    clientName: String)
      extends State

  case class SuspendedAgent(service: String, continueUrl: Option[String]) extends State

  case class CgtRefNotFound(originalFastTrackRequest: AgentFastTrackRequest,
                            fastTrackRequest: AgentFastTrackRequest,
                            continueUrl: Option[String]) extends NotMatched

  case class PptRefNotFound(originalFastTrackRequest: AgentFastTrackRequest,
                            fastTrackRequest: AgentFastTrackRequest,
                            continueUrl: Option[String]) extends NotMatched

  case object AlreadyCopiedAcrossItsa extends State

  case object ClientInsolventFastTrack extends State
  case object CannotCreateFastTrackRequest extends State

  case class LegacyAuthorisationDetected(fastTrackRequest: AgentFastTrackRequest, arn: Arn, invitation: Invitation, continueUrl: Option[String])
      extends State

  object Transitions {
    type HasPendingInvitations = (Arn, String, String) => Future[Boolean]
    type HasActiveRelationship = (Arn, String, String) => Future[Boolean]
    type HasPartialAuthorisation = (Arn, String) => Future[Boolean]
    type GetClientName = (String, String) => Future[Option[String]]
    type CheckPostcodeMatches = (Nino, String) => Future[Option[Boolean]]
    type CheckRegDateMatches = (Vrn, LocalDate) => Future[VatKnownFactCheckResult]
    type CheckPptKnownFact = (PptRef, LocalDate) => Future[Boolean]
    type GetAgentLink = (Arn, Option[ClientType]) => Future[String]
    type CreateInvitation =
      (Arn, Invitation) => Future[InvitationId]
    type GetAgencyEmail = () => Future[String]

    type GetTrustName = TrustTaxIdentifier => Future[TrustResponse]
    type GetSuspensionDetails = () => Future[SuspensionDetails]
    type IsAltItsa = (Arn, String) => Future[Boolean]
    type LegacySaRelationshipStatusFor = (Arn, String) => Future[LegacySaRelationshipResult]
    type GetCgtSubscription = CgtRef => Future[Option[CgtSubscription]]
    type GetPptSubscription = PptRef => Future[Option[PptSubscription]]
    type CheckDOBMatches = (Nino, LocalDate) => Future[Option[Boolean]]

    def prologue(failureUrl: Option[String], refererUrl: Option[String]) = Transition {
      case _ => goto(Prologue(failureUrl, refererUrl))
    }

    def start(agentSuspensionEnabled: Boolean, getSuspensionDetails: GetSuspensionDetails)(continueUrl: Option[String])(agent: AuthorisedAgent)(
      fastTrackRequest: AgentFastTrackRequest) = Transition {
      case _ =>
        if (agentSuspensionEnabled) getSuspensionDetails().flatMap { suspensionDetails =>
          suspensionDetails.isRegimeSuspended(fastTrackRequest.service) match {
            case true  => goto(SuspendedAgent(fastTrackRequest.service, continueUrl))
            case false => getStateForNonSuspendedAgent(fastTrackRequest, continueUrl)
          }
        } else {
          getStateForNonSuspendedAgent(fastTrackRequest, continueUrl)
        }
    }

    def getStateForNonSuspendedAgent(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]): Future[State] =
      fastTrackRequest match {
        case AgentFastTrackRequest(_, HMRCMTDIT, _, _, knownFact) =>
          val updatedPersonalRequest = fastTrackRequest.copy(clientType = Some(Personal))
          goto(knownFact.fold(CheckDetailsNoPostcode(updatedPersonalRequest, updatedPersonalRequest, continueUrl): State)(_ =>
            CheckDetailsCompleteItsa(updatedPersonalRequest, updatedPersonalRequest, continueUrl)))

        case AgentFastTrackRequest(_, HMRCPIR, _, _, knownFact) =>
          val updatedPersonalRequest = fastTrackRequest.copy(clientType = Some(Personal))
          goto(knownFact.fold(CheckDetailsNoDob(updatedPersonalRequest, updatedPersonalRequest, continueUrl): State)(_ =>
            CheckDetailsCompleteIrv(updatedPersonalRequest, updatedPersonalRequest, continueUrl)))

        case AgentFastTrackRequest(Some(ClientType.Personal), HMRCMTDVAT, _, _, knownFact) =>
          goto(knownFact.fold(CheckDetailsNoVatRegDate(fastTrackRequest, fastTrackRequest, continueUrl): State)(_ =>
            CheckDetailsCompletePersonalVat(fastTrackRequest, fastTrackRequest, continueUrl)))

        case AgentFastTrackRequest(Some(ClientType.Business), HMRCMTDVAT, _, _, knownFact) =>
          goto(knownFact.fold(CheckDetailsNoVatRegDate(fastTrackRequest, fastTrackRequest, continueUrl): State)(_ =>
            CheckDetailsCompleteBusinessVat(fastTrackRequest, fastTrackRequest, continueUrl)))

        case AgentFastTrackRequest(None, HMRCMTDVAT, _, _, _) =>
          goto(CheckDetailsNoClientTypeVat(fastTrackRequest, fastTrackRequest, continueUrl))

        case AgentFastTrackRequest(_, TAXABLETRUST | NONTAXABLETRUST, _, _, _) =>
          goto(CheckDetailsCompleteTrust(fastTrackRequest, fastTrackRequest.copy(clientType = Some(ClientType.Trust)), continueUrl))

        case AgentFastTrackRequest(_, HMRCCGTPD, _, _, _) =>
          goto(CheckDetailsCompleteCgt(fastTrackRequest, fastTrackRequest, continueUrl))

        case AgentFastTrackRequest(_, HMRCPPTORG, _, _, _) =>
          goto(CheckDetailsCompletePpt(fastTrackRequest, fastTrackRequest, continueUrl))

        case fastTrackRequest =>
          throw new RuntimeException(s"No state found for fast track request: $fastTrackRequest")
      }

    private def invitationSentState(fastTrackRequest: AgentFastTrackRequest, arn: Arn, invitation: Invitation, continueUrl: Option[String])(
      isAltItsa: IsAltItsa,
      createInvitation: CreateInvitation,
      getAgentLink: GetAgentLink,
      getAgencyEmail: GetAgencyEmail) =
      for {
        agencyEmail    <- getAgencyEmail()
        _              <- createInvitation(arn, invitation)
        invitationLink <- getAgentLink(arn, fastTrackRequest.clientType)
        result <- fastTrackRequest.clientType match {
                   case Some(ClientType.Personal) =>
                     isAltItsa(arn, fastTrackRequest.clientIdentifier).map { altItsa =>
                       InvitationSentPersonal(invitationLink, continueUrl, agencyEmail, fastTrackRequest.service, altItsa)
                     }
                   case Some(ClientType.Business) =>
                     goto(InvitationSentBusiness(invitationLink, continueUrl, agencyEmail, fastTrackRequest.service))
                   case Some(ClientType.Trust) =>
                     goto(InvitationSentBusiness(invitationLink, continueUrl, agencyEmail, fastTrackRequest.service))
                   case None =>
                     throw new RuntimeException(s"No client type found for fast track request: $fastTrackRequest")
                 }
      } yield result

    def confirmedLegacyAuthorisation(
      isAltItsa: IsAltItsa,
      createInvitation: CreateInvitation,
      getAgentLink: GetAgentLink,
      getAgencyEmail: GetAgencyEmail): Transition =
      Transition {
        case LegacyAuthorisationDetected(fastTrackRequest, arn, invitation, continueUrl) =>
          invitationSentState(fastTrackRequest, arn, invitation, continueUrl)(isAltItsa, createInvitation, getAgentLink, getAgencyEmail)
      }

    def checkIfPendingOrActiveAndGoto(fastTrackRequest: AgentFastTrackRequest, arn: Arn, invitation: Invitation, continueUrl: Option[String])(
      hasPendingInvitationsFor: HasPendingInvitations,
      hasActiveRelationshipFor: HasActiveRelationship,
      hasPartialAuthorisation: HasPartialAuthorisation,
      isAltItsa: IsAltItsa,
      legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor,
      appConfig: AppConfig
    )(createInvitation: CreateInvitation, getAgentLink: GetAgentLink, getAgencyEmail: GetAgencyEmail, getClientName: GetClientName): Future[State] = {

      def invitationSent =
        invitationSentState(fastTrackRequest, arn, invitation, continueUrl)(
          isAltItsa: IsAltItsa,
          createInvitation: CreateInvitation,
          getAgentLink: GetAgentLink,
          getAgencyEmail: GetAgencyEmail)

      for {
        hasPendingInvitations <- hasPendingInvitationsFor(arn, fastTrackRequest.clientIdentifier, fastTrackRequest.service)
        result <- if (hasPendingInvitations) {
                   for {
                     agentLink <- getAgentLink(arn, fastTrackRequest.clientType)
                     clientName <- getClientName(fastTrackRequest.clientIdentifier, fastTrackRequest.service)
                     newState <- goto(PendingInvitationExists(fastTrackRequest, agentLink, clientName.getOrElse(fastTrackRequest.clientIdentifier), continueUrl))
                   } yield newState
                 } else {
                   hasActiveRelationshipFor(arn, fastTrackRequest.clientIdentifier, fastTrackRequest.service)
                     .flatMap {
                       case true => goto(ActiveAuthorisationExists(fastTrackRequest, continueUrl))
                       case false =>
                         hasPartialAuthorisation(arn, fastTrackRequest.clientIdentifier).flatMap {
                           case true => goto(PartialAuthorisationExists(fastTrackRequest, continueUrl))
                           case false =>
                             if (appConfig.featuresAltItsa && invitation.service == HMRCMTDIT)
                               legacySaRelationshipStatusFor(arn, fastTrackRequest.clientIdentifier).flatMap {
                                 case LegacySaRelationshipFoundAndMapped => goto(AlreadyCopiedAcrossItsa)
                                 case LegacySaRelationshipFoundNotMapped =>
                                   goto(LegacyAuthorisationDetected(fastTrackRequest, arn, invitation, continueUrl))
                                 case LegacySaRelationshipNotFound => invitationSent
                               } else invitationSent
                         }
                     }
                 }
      } yield result
    }

    def checkedDetailsNoClientType(agent: AuthorisedAgent) = Transition {
      case CheckDetailsNoClientTypeVat(originalFastTrackRequest, fastTrackRequest, continueUrl) =>
        goto(SelectClientTypeVat(originalFastTrackRequest, fastTrackRequest, continueUrl))
      case NoVatRegDate(originalFastTrackRequest, fastTrackRequest, continueUrl) =>
        goto(SelectClientTypeVat(originalFastTrackRequest, fastTrackRequest, continueUrl))
      case CheckDetailsCompleteCgt(originalFastTrackRequest, fastTrackRequest, continueUrl) =>
        goto(SelectClientTypeCgt(originalFastTrackRequest, fastTrackRequest, continueUrl))
      case CheckDetailsCompletePpt(originalFastTrackRequest, fastTrackRequest, continueUrl) =>
        goto(SelectClientTypePpt(originalFastTrackRequest, fastTrackRequest, continueUrl))
    }

    def checkedDetailsNoKnownFact(getCgtSubscription: GetCgtSubscription, getPptSubscription: GetPptSubscription)(agent: AuthorisedAgent) =
      Transition {
        case CheckDetailsNoPostcode(originalFastTrackRequest, fastTrackRequest, continueUrl) =>
          goto(NoPostcode(originalFastTrackRequest, fastTrackRequest, continueUrl))

        case CheckDetailsNoDob(originalFastTrackRequest, fastTrackRequest, continueUrl) =>
          goto(NoDob(originalFastTrackRequest, fastTrackRequest, continueUrl))

        case CheckDetailsNoVatRegDate(originalFastTrackRequest, fastTrackRequest, continueUrl) =>
          goto(NoVatRegDate(originalFastTrackRequest, fastTrackRequest, continueUrl))

        case CheckDetailsCompleteCgt(originalFastTrackRequest, fastTrackRequest, continueUrl) =>
          val cgtRef = CgtRef(fastTrackRequest.clientIdentifier)

          getCgtSubscription(cgtRef).map {
            case Some(subscription) =>
              if (subscription.isUKBasedClient) {
                ConfirmPostcodeCgt(originalFastTrackRequest, fastTrackRequest, continueUrl, subscription.postCode, subscription.name)
              } else {
                ConfirmCountryCodeCgt(originalFastTrackRequest, fastTrackRequest, continueUrl, subscription.countryCode, subscription.name)
              }
            case None =>
              CgtRefNotFound(originalFastTrackRequest, fastTrackRequest, continueUrl)
          }

        case CheckDetailsCompletePpt(originalFastTrackRequest, fastTrackRequest, continueUrl) =>
          val pptRef = PptRef(fastTrackRequest.clientIdentifier)

          getPptSubscription(pptRef).map {
            case Some(subscription) =>
              ConfirmRegDatePpt(originalFastTrackRequest, fastTrackRequest, continueUrl, subscription.dateOfApplication, subscription.customerName)
            case None =>
              PptRefNotFound(originalFastTrackRequest, fastTrackRequest, continueUrl)
          }
      }

    def confirmPostcodeCgt(agent: AuthorisedAgent)(postcode: Postcode): Transition =
      Transition {
        case ConfirmPostcodeCgt(originalFastTrackRequest, fastTrackRequest, continueUrl, postcodeFromDes, name) =>
          val desPostcode = postcodeFromDes.map(withSpacesRemoved)
          val ftrPostcode = withSpacesRemoved(postcode.value)
          if (desPostcode.getOrElse("no_postcode") == ftrPostcode) {
            goto(ConfirmClientCgt(originalFastTrackRequest, fastTrackRequest.copy(knownFact = Some(postcode.value)), continueUrl, name))
          } else {
            goto(KnownFactNotMatched(originalFastTrackRequest, fastTrackRequest, continueUrl))
          }
      }

    def confirmCountryCodeCgt(agent: AuthorisedAgent)(countryCode: CountryCode): Transition =
      Transition {
        case ConfirmCountryCodeCgt(originalFastTrackRequest, fastTrackRequest, continueUrl, countryCodeFromDes, name) =>
          if (countryCodeFromDes.contains(countryCode.value)) {
            goto(ConfirmClientCgt(originalFastTrackRequest, fastTrackRequest.copy(knownFact = Some(countryCode.value)), continueUrl, name))
          } else {
            goto(KnownFactNotMatched(originalFastTrackRequest, fastTrackRequest, continueUrl))
          }
      }


    private def withSpacesRemoved(postcode: String): String =
      postcode.replace(" ", "")

    def identifyCgtClient(getCgtSubscription: GetCgtSubscription)(agent: AuthorisedAgent)(cgtClient: CgtClient): Transition =
      Transition {
        case IdentifyCgtClient(originalFastTrackRequest, fastTrackRequest, continueUrl) =>
          getCgtSubscription(CgtRef(cgtClient.cgtRef.value)) map {
            case Some(subscription) =>
              val ftrKnownFact = fastTrackRequest.knownFact.map(withSpacesRemoved)
              val newFtr = fastTrackRequest.copy(clientIdentifier = cgtClient.cgtRef.value)
              if (subscription.isUKBasedClient) {
                val desPostcode = subscription.postCode.map(withSpacesRemoved)
                if(ftrKnownFact.contains(desPostcode.getOrElse("no_postcode"))) {
                  ConfirmClientCgt(originalFastTrackRequest, newFtr, continueUrl, subscription.name)
                }
                else ConfirmPostcodeCgt(originalFastTrackRequest, newFtr, continueUrl, subscription.postCode, subscription.name)
              } else {
                val desCountryCode = withSpacesRemoved(subscription.countryCode)
                if(ftrKnownFact.contains(desCountryCode)) {
                  ConfirmClientCgt(originalFastTrackRequest, newFtr, continueUrl, subscription.name)
                }
                else ConfirmCountryCodeCgt(originalFastTrackRequest, newFtr, continueUrl, subscription.countryCode, subscription.name)
              }
            case None => CgtRefNotFound(originalFastTrackRequest, fastTrackRequest, continueUrl)
          }
      }

    def confirmRegDatePpt(agent: AuthorisedAgent)(regDate: String): Transition =
      Transition {
        case ConfirmRegDatePpt(originalFastTrackRequest, fastTrackRequest, continueUrl, actualRegDate, name) =>
          if (actualRegDate.equals(LocalDate.parse(regDate))) {
            goto(ConfirmClientPpt(originalFastTrackRequest, fastTrackRequest, continueUrl, name))
          } else {
            goto(KnownFactNotMatched(originalFastTrackRequest, fastTrackRequest, continueUrl))
          }
      }

    def identifyPptClient(checkKnownFact: PptClient => Future[Boolean], getPptCustomerName: PptRef => Future[Option[String]])(agent: AuthorisedAgent)(
      pptClient: PptClient): Transition =
      Transition {
        case IdentifyPptClient(originalFastTrackRequest, fastTrackRequest, continueUrl) =>
          for {
            isCheckOK     <- checkKnownFact(pptClient)
            mCustomerName <- if (isCheckOK) getPptCustomerName(PptRef(fastTrackRequest.clientIdentifier)) else Future.successful(None)
          } yield {
            mCustomerName match {
              case Some(customerName) => ConfirmClientPpt(originalFastTrackRequest, fastTrackRequest, continueUrl, customerName)
              case None               => PptRefNotFound(originalFastTrackRequest, fastTrackRequest, continueUrl)
            }
          }
      }

    def checkedDetailsChangeInformation(agent: AuthorisedAgent): AgentInvitationFastTrackJourneyModel.Transition = {
      def gotoIdentifyClient(originalFtr: AgentFastTrackRequest, ftRequest: AgentFastTrackRequest, continueUrl: Option[String]) =
        if (ftRequest.clientType.contains(Personal)) {
          goto(IdentifyPersonalClient(originalFtr, ftRequest, continueUrl))
        } else if (ftRequest.clientType.contains(Business)) {
          goto(IdentifyBusinessClient(originalFtr, ftRequest, continueUrl))
        } else {
          goto(IdentifyNoClientTypeClient(originalFtr, ftRequest, continueUrl))
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

        case CheckDetailsCompleteCgt(originalFtr, ftr, continueUrl) =>
          if (ftr.clientType.isEmpty) goto(SelectClientTypeCgt(originalFtr, ftr, continueUrl, isChanging = true))
          else goto(IdentifyCgtClient(originalFtr, ftr, continueUrl))

        case CheckDetailsCompletePpt(originalFtr, ftr, continueUrl) =>
          if (ftr.clientType.isEmpty) goto(SelectClientTypePpt(originalFtr, ftr, continueUrl, isChanging = true))
          else goto(IdentifyPptClient(originalFtr, ftr, continueUrl))

        case CheckDetailsNoPostcode(originalFtr, ftr, continueUrl) =>
          goto(IdentifyPersonalClient(originalFtr, ftr, continueUrl))

        case CheckDetailsNoDob(originalFtr, ftr, continueUrl) =>
          goto(IdentifyPersonalClient(originalFtr, ftr, continueUrl))

        case CheckDetailsNoVatRegDate(originalFtr, ftr, continueUrl) =>
          gotoIdentifyClient(originalFtr, ftr, continueUrl)

        case CheckDetailsNoClientTypeVat(originalFtr, ftr, continueUrl) =>
          goto(SelectClientTypeVat(originalFtr, ftr, continueUrl, isChanging = true))
      }
    }

    def checkedDetailsAllInformation(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(checkPptKnownFact: CheckPptKnownFact)(getCgtSubscription: GetCgtSubscription)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(getClientName: GetClientName)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      hasPartialAuthorisation: HasPartialAuthorisation)(hasPendingAltItsaInvitation: IsAltItsa)(
      legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)(appConfig: AppConfig)(agent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {
        case CheckDetailsCompleteItsa(originalFtr, fastTrackRequest, continueUrl) => {
          if (confirmation.choice) {
            checkPostcodeMatches(Nino(fastTrackRequest.clientIdentifier), fastTrackRequest.knownFact.getOrElse(""))
              .flatMap {
                case Some(true) =>
                  checkIfPendingOrActiveAndGoto(
                    fastTrackRequest,
                    agent.arn,
                    ItsaInvitation(Nino(fastTrackRequest.clientIdentifier)),
                    continueUrl
                  )(
                    hasPendingInvitations,
                    hasActiveRelationship,
                    hasPartialAuthorisation,
                    hasPendingAltItsaInvitation,
                    legacySaRelationshipStatusFor,
                    appConfig)(createInvitation, getAgentLink, getAgencyEmail, getClientName)
                case Some(false) => goto(KnownFactNotMatched(originalFtr, fastTrackRequest, continueUrl))
                case None =>
                  if (appConfig.featuresAltItsa) goto(ClientNotRegistered(fastTrackRequest, continueUrl))
                  else goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
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
                    PirInvitation(Nino(fastTrackRequest.clientIdentifier)),
                    continueUrl
                  )(
                    hasPendingInvitations,
                    hasActiveRelationship,
                    hasPartialAuthorisation,
                    hasPendingAltItsaInvitation,
                    legacySaRelationshipStatusFor,
                    appConfig)(createInvitation, getAgentLink, getAgencyEmail, getClientName)
                case _ => goto(KnownFactNotMatched(originalFtr, fastTrackRequest, continueUrl))
              }
          } else goto(IdentifyPersonalClient(originalFtr, fastTrackRequest, continueUrl))

        case CheckDetailsCompletePersonalVat(originalFtr, fastTrackRequest, continueUrl) =>
          if (confirmation.choice) {
            val knownFact = fastTrackRequest.knownFact.getOrElse("")
            checkRegDateMatches(Vrn(fastTrackRequest.clientIdentifier), LocalDate.parse(knownFact))
              .flatMap {
                case VatKnownFactCheckOk =>
                  checkIfPendingOrActiveAndGoto(
                    fastTrackRequest,
                    agent.arn,
                    VatInvitation(Some(Personal), Vrn(fastTrackRequest.clientIdentifier)),
                    continueUrl
                  )(
                    hasPendingInvitations,
                    hasActiveRelationship,
                    hasPartialAuthorisation,
                    hasPendingAltItsaInvitation,
                    legacySaRelationshipStatusFor,
                    appConfig)(createInvitation, getAgentLink, getAgencyEmail, getClientName)
                case VatKnownFactNotMatched       => goto(KnownFactNotMatched(originalFtr, fastTrackRequest, continueUrl))
                case VatRecordClientInsolvent     => goto(ClientInsolventFastTrack)
                case VatRecordMigrationInProgress => goto(CannotCreateFastTrackRequest)
                case VatDetailsNotFound           => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
              }
          } else goto(IdentifyPersonalClient(originalFtr, fastTrackRequest, continueUrl))

        case CheckDetailsCompleteBusinessVat(originalFtr, fastTrackRequest, continueUrl) =>
          if (confirmation.choice) {
            val knownFact = fastTrackRequest.knownFact.getOrElse("")
            checkRegDateMatches(Vrn(fastTrackRequest.clientIdentifier), LocalDate.parse(knownFact))
              .flatMap {
                case VatKnownFactCheckOk =>
                  checkIfPendingOrActiveAndGoto(
                    fastTrackRequest,
                    agent.arn,
                    VatInvitation(Some(ClientType.Business), Vrn(fastTrackRequest.clientIdentifier)),
                    continueUrl
                  )(
                    hasPendingInvitations,
                    hasActiveRelationship,
                    hasPartialAuthorisation,
                    hasPendingAltItsaInvitation,
                    legacySaRelationshipStatusFor,
                    appConfig)(createInvitation, getAgentLink, getAgencyEmail, getClientName)
                case VatKnownFactNotMatched       => goto(KnownFactNotMatched(originalFtr, fastTrackRequest, continueUrl))
                case VatDetailsNotFound           => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
                case VatRecordClientInsolvent     => goto(ClientInsolventFastTrack)
                case VatRecordMigrationInProgress => goto(CannotCreateFastTrackRequest)
              }
          } else goto(IdentifyBusinessClient(originalFtr, fastTrackRequest, continueUrl))

        case CheckDetailsCompleteTrust(originalFtr, fastTrackRequest, continueUrl) =>
          if (confirmation.choice) {
            getClientName(fastTrackRequest.clientIdentifier, fastTrackRequest.service).flatMap {
              case Some(_) =>
                val trustInvitation = fastTrackRequest.clientIdentifier match {
                  case utr if utr.matches(utrPattern) => TrustInvitation(Utr(utr))
                  case urn if urn.matches(urnPattern) => TrustNTInvitation(Urn(urn))
                }
                checkIfPendingOrActiveAndGoto(
                  fastTrackRequest,
                  agent.arn,
                  trustInvitation,
                  continueUrl
                )(
                  hasPendingInvitations,
                  hasActiveRelationship,
                  hasPartialAuthorisation,
                  hasPendingAltItsaInvitation,
                  legacySaRelationshipStatusFor,
                  appConfig)(createInvitation, getAgentLink, getAgencyEmail, getClientName)
              case None => goto(TrustNotFound(originalFtr, fastTrackRequest, continueUrl))
            }
          } else goto(IdentifyTrustClient(originalFtr, fastTrackRequest, continueUrl))

        case CheckDetailsCompleteCgt(originalFtr, fastTrackRequest, continueUrl) =>
          if (confirmation.choice) {
            getCgtSubscription(CgtRef(fastTrackRequest.clientIdentifier)).flatMap {
              case Some(subscription) =>
                val (knownFactEmptyState, field) = if (subscription.isUKBasedClient) {
                  (ConfirmPostcodeCgt(originalFtr, fastTrackRequest, continueUrl, subscription.postCode, subscription.name),
                  subscription.postCode)
                } else
                  (ConfirmCountryCodeCgt(originalFtr, fastTrackRequest, continueUrl, subscription.countryCode, subscription.name),
                    Some(subscription.countryCode))
                fastTrackRequest
                  .knownFact
                  .fold(goto(knownFactEmptyState): Future[State]) {
                    knownFact =>
                      val ftrKnownFact = withSpacesRemoved(knownFact)
                      val desKNownFact = field.map(withSpacesRemoved)
                      if (ftrKnownFact == desKNownFact.getOrElse("no_postcode"))
                        checkIfPendingOrActiveAndGoto(
                          fastTrackRequest,
                          agent.arn,
                          CgtInvitation(CgtRef(fastTrackRequest.clientIdentifier), fastTrackRequest.clientType),
                          continueUrl
                        )(
                          hasPendingInvitations,
                          hasActiveRelationship,
                          hasPartialAuthorisation,
                          hasPendingAltItsaInvitation,
                          legacySaRelationshipStatusFor,
                          appConfig)(createInvitation, getAgentLink, getAgencyEmail, getClientName)
                      else goto(KnownFactNotMatched(originalFtr, fastTrackRequest, continueUrl))
                  }
              case None => goto(CgtRefNotFound(originalFtr, fastTrackRequest, continueUrl))
          }
          } else goto(IdentifyCgtClient(originalFtr, fastTrackRequest, continueUrl))

        case CheckDetailsCompletePpt(originalFtr, fastTrackRequest, continueUrl) =>
          if (confirmation.choice) {
            val knownFact = fastTrackRequest.knownFact.getOrElse("")
            checkPptKnownFact(PptRef(fastTrackRequest.clientIdentifier), LocalDate.parse(knownFact))
              .flatMap {
                case true =>
                  checkIfPendingOrActiveAndGoto(
                    fastTrackRequest,
                    agent.arn,
                    PptInvitation(PptRef(fastTrackRequest.clientIdentifier), fastTrackRequest.clientType),
                    continueUrl
                  )(
                    hasPendingInvitations,
                    hasActiveRelationship,
                    hasPartialAuthorisation,
                    hasPendingAltItsaInvitation,
                    legacySaRelationshipStatusFor,
                    appConfig)(createInvitation, getAgentLink, getAgencyEmail, getClientName)
                case false => goto(KnownFactNotMatched(originalFtr, fastTrackRequest, continueUrl))
              }
          } else goto(IdentifyPptClient(originalFtr, fastTrackRequest, continueUrl))
      }

    def identifiedClientItsa(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(checkRegDateMatches: CheckRegDateMatches)(
      checkPptKnownFact: CheckPptKnownFact)(getCgtSubscription: GetCgtSubscription)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(getClientName: GetClientName)(getAgencyEmail: GetAgencyEmail)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(hasPendingAltItsaInvitation: IsAltItsa)(
      hasPartialAuthorisation: HasPartialAuthorisation)(legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)(appConfig: AppConfig)(
      agent: AuthorisedAgent)(itsaClient: ItsaClient) =
      Transition {
        case IdentifyPersonalClient(originalFtr, ftr, continueUrl) =>
          val newState = CheckDetailsCompleteItsa(
            originalFtr,
            ftr.copy(clientIdentifier = itsaClient.clientIdentifier, knownFact = Some(itsaClient.postcode)),
            continueUrl)
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(createInvitation)(getAgentLink)(getClientName)(
            getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(hasPartialAuthorisation)(hasPendingAltItsaInvitation)(
            legacySaRelationshipStatusFor)(appConfig)(agent)(Confirmation(true))
            .apply(newState)
      }

    def identifiedClientIrv(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(checkRegDateMatches: CheckRegDateMatches)(
      checkPptKnownFact: CheckPptKnownFact)(getCgtSubscription: GetCgtSubscription)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(getClientName: GetClientName)(getAgencyEmail: GetAgencyEmail)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(hasPendingAltItsaInvitation: IsAltItsa)(
      hasPartialAuthorisation: HasPartialAuthorisation)(legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)(appConfig: AppConfig)(
      agent: AuthorisedAgent)(irvClient: IrvClient) =
      Transition {
        case IdentifyPersonalClient(originalFtr, ftr, continueUrl) =>
          val newState =
            CheckDetailsCompleteIrv(
              originalFtr,
              ftr.copy(clientIdentifier = irvClient.clientIdentifier, knownFact = Some(irvClient.dob)),
              continueUrl)
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(createInvitation)(getAgentLink)(getClientName)(
            getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(hasPartialAuthorisation)(hasPendingAltItsaInvitation)(
            legacySaRelationshipStatusFor)(appConfig)(agent)(Confirmation(true))
            .apply(newState)
      }

    def identifiedClientVat(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(checkRegDateMatches: CheckRegDateMatches)(
      checkPptKnownFact: CheckPptKnownFact)(getCgtSubscription: GetCgtSubscription)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(getClientName: GetClientName)(getAgencyEmail: GetAgencyEmail)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(hasPartialAuthorisation: HasPartialAuthorisation)(
      hasPendingAltItsaInvitation: IsAltItsa)(legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)(appConfig: AppConfig)(
      agent: AuthorisedAgent)(vatClient: VatClient) =
      Transition {
        case IdentifyPersonalClient(originalFtr, ftr, continueUrl) =>
          val newState = CheckDetailsCompletePersonalVat(
            originalFtr,
            ftr.copy(clientIdentifier = vatClient.clientIdentifier, knownFact = Some(vatClient.registrationDate)),
            continueUrl)
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(createInvitation)(getAgentLink)(getClientName)(
            getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(hasPartialAuthorisation)(hasPendingAltItsaInvitation)(
            legacySaRelationshipStatusFor)(appConfig)(agent)(Confirmation(true))
            .apply(newState)
        case IdentifyBusinessClient(originalFtr, ftr, continueUrl) =>
          val newState = CheckDetailsCompleteBusinessVat(
            originalFtr,
            ftr.copy(clientIdentifier = vatClient.clientIdentifier, knownFact = Some(vatClient.registrationDate)),
            continueUrl)
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(createInvitation)(getAgentLink)(getClientName)(
            getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(hasPartialAuthorisation)(hasPendingAltItsaInvitation)(
            legacySaRelationshipStatusFor)(appConfig)(agent)(Confirmation(true))
            .apply(newState)

        case IdentifyNoClientTypeClient(originalFtr, ftr, continueUrl) =>
          val newState = CheckDetailsNoClientTypeVat(
            originalFtr,
            ftr.copy(clientIdentifier = vatClient.clientIdentifier, knownFact = Some(vatClient.registrationDate)),
            continueUrl)
          checkedDetailsNoClientType(agent).apply(newState)
      }

    def showConfirmTrustClient(getTrustName: GetTrustName)(agent: AuthorisedAgent)(trustClient: TrustClient) =
      Transition {
        case IdentifyTrustClient(originalFtr, ftr, continueUrl) =>
          getTrustName(trustClient.taxId).flatMap { trustResponse =>
            trustResponse.response match {
              case Right(TrustName(name)) =>
                goto(ConfirmClientTrust(originalFtr, ftr.copy(clientIdentifier = trustClient.taxId.value), continueUrl, name))
              case Left(invalidTrust) =>
                logger.warn(s"Des returned $invalidTrust response for utr: ${trustClient.taxId}")
                goto(TrustNotFound(originalFtr, ftr, continueUrl))
            }
          }
      }

    def submitConfirmTrustClient(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(getClientName: GetClientName)(getAgencyEmail: GetAgencyEmail)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(hasPartialAuthorisation: HasPartialAuthorisation)(
      hasPendingAltItsaInvitation: IsAltItsa)(legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)(appConfig: AppConfig)(
      agent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {
        case ConfirmClientTrust(originalFtr, ftr, continueUrl, trustName) =>
          if (confirmation.choice) {
            val trustInvitation = ftr.clientIdentifier match {
              case utr if utr.matches(utrPattern) => TrustInvitation(Utr(utr))
              case urn if urn.matches(urnPattern) => TrustNTInvitation(Urn(urn))
            }
            checkIfPendingOrActiveAndGoto(
              ftr,
              agent.arn,
              trustInvitation,
              continueUrl
            )(
              hasPendingInvitations,
              hasActiveRelationship,
              hasPartialAuthorisation,
              hasPendingAltItsaInvitation,
              legacySaRelationshipStatusFor,
              appConfig)(createInvitation, getAgentLink, getAgencyEmail, getClientName)
          } else {
            goto(IdentifyTrustClient(originalFtr, ftr, continueUrl))
          }
      }

    def submitConfirmClientCgt(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(getClientName: GetClientName)(getAgencyEmail: GetAgencyEmail)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(hasPartialAuthorisation: HasPartialAuthorisation)(
      hasPendingAltItsaInvitation: IsAltItsa)(legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)(appConfig: AppConfig)(
      agent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {
        case ConfirmClientCgt(originalFtr, ftr, continueUrl, cgtName) =>
          if (confirmation.choice) {
            checkIfPendingOrActiveAndGoto(
              ftr,
              agent.arn,
              CgtInvitation(CgtRef(ftr.clientIdentifier), ftr.clientType),
              continueUrl
            )(
              hasPendingInvitations,
              hasActiveRelationship,
              hasPartialAuthorisation,
              hasPendingAltItsaInvitation,
              legacySaRelationshipStatusFor,
              appConfig)(createInvitation, getAgentLink, getAgencyEmail, getClientName)
          } else {
            goto(IdentifyCgtClient(originalFtr, ftr, continueUrl))
          }
      }

    def submitConfirmClientPpt(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(getClientName: GetClientName)(getAgencyEmail: GetAgencyEmail)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(hasPartialAuthorisation: HasPartialAuthorisation)(
      hasPendingAltItsaInvitation: IsAltItsa)(legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)(appConfig: AppConfig)(
      agent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {
        case ConfirmClientPpt(originalFtr, ftr, continueUrl, pptName) =>
          if (confirmation.choice) {
            checkIfPendingOrActiveAndGoto(
              ftr,
              agent.arn,
              PptInvitation(PptRef(ftr.clientIdentifier), ftr.clientType),
              continueUrl
            )(
              hasPendingInvitations,
              hasActiveRelationship,
              hasPartialAuthorisation,
              hasPendingAltItsaInvitation,
              legacySaRelationshipStatusFor,
              appConfig)(createInvitation, getAgentLink, getAgencyEmail, getClientName)
          } else {
            goto(IdentifyPptClient(originalFtr, ftr, continueUrl))
          }
      }

    def moreDetailsItsa(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(checkRegDateMatches: CheckRegDateMatches)(
      checkPptKnownFact: CheckPptKnownFact)(getCgtSubscription: GetCgtSubscription)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(getClientName: GetClientName)(getAgencyEmail: GetAgencyEmail)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(hasPartialAuthorisation: HasPartialAuthorisation)(
      hasPendingAltItsaInvitation: IsAltItsa)(legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)(appConfig: AppConfig)(
      agent: AuthorisedAgent)(suppliedKnownFact: String) =
      Transition {
        case NoPostcode(originalFtr, ftr, continueUrl) =>
          val newState = CheckDetailsCompleteItsa(originalFtr, ftr.copy(knownFact = Some(suppliedKnownFact)), continueUrl)
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(createInvitation)(getAgentLink)(getClientName)(
            getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(hasPartialAuthorisation)(hasPendingAltItsaInvitation)(
            legacySaRelationshipStatusFor)(appConfig)(agent)(Confirmation(true))
            .apply(newState)
      }

    def moreDetailsIrv(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(checkRegDateMatches: CheckRegDateMatches)(
      checkPptKnownFact: CheckPptKnownFact)(getCgtSubscription: GetCgtSubscription)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(getClientName: GetClientName)(getAgencyEmail: GetAgencyEmail)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(hasPartialAuthorisation: HasPartialAuthorisation)(
      hasPendingAltItsaInvitation: IsAltItsa)(legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)(appConfig: AppConfig)(
      agent: AuthorisedAgent)(suppliedKnownFact: String) =
      Transition {
        case NoDob(originalFtr, ftr, continueUrl) =>
          val newState =
            CheckDetailsCompleteIrv(originalFtr, ftr.copy(knownFact = Some(suppliedKnownFact)), continueUrl)
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(createInvitation)(getAgentLink)(getClientName)(
            getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(hasPartialAuthorisation)(hasPendingAltItsaInvitation)(
            legacySaRelationshipStatusFor)(appConfig)(agent)(Confirmation(true))
            .apply(newState)
      }

    def moreDetailsVat(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(checkRegDateMatches: CheckRegDateMatches)(
      checkPptKnownFact: CheckPptKnownFact)(getCgtSubscription: GetCgtSubscription)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(getClientName: GetClientName)(getAgencyEmail: GetAgencyEmail)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(hasPartialAuthorisation: HasPartialAuthorisation)(
      hasPendingAltItsaInvitation: IsAltItsa)(legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)(appConfig: AppConfig)(
      agent: AuthorisedAgent)(suppliedKnownFact: String) =
      Transition {
        case NoVatRegDate(originalFtr, ftRequest, continueUrl) => {
          val checkDetailsState =
            if (ftRequest.clientType.contains(Personal))
              CheckDetailsCompletePersonalVat
            else
              CheckDetailsCompleteBusinessVat

          val newState =
            checkDetailsState(originalFtr, ftRequest.copy(knownFact = Some(suppliedKnownFact)), continueUrl)

          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(createInvitation)(getAgentLink)(getClientName)(
            getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(hasPartialAuthorisation)(hasPendingAltItsaInvitation)(
            legacySaRelationshipStatusFor)(appConfig)(agent)(Confirmation(true))
            .apply(newState)
        }
      }

    def moreDetailsPpt(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(checkRegDateMatches: CheckRegDateMatches)(
      checkPptKnownFact: CheckPptKnownFact)(getCgtSubscription: GetCgtSubscription)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(getClientName: GetClientName)(getAgencyEmail: GetAgencyEmail)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(hasPartialAuthorisation: HasPartialAuthorisation)(
      hasPendingAltItsaInvitation: IsAltItsa)(legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)(appConfig: AppConfig)(
      agent: AuthorisedAgent)(suppliedKnownFact: String) =
      Transition {
        case NoPptRegDate(originalFtr, ftRequest, continueUrl) => {
          val checkDetailsState = CheckDetailsCompletePpt

          val newState =
            checkDetailsState(originalFtr, ftRequest.copy(knownFact = Some(suppliedKnownFact)), continueUrl)

          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(createInvitation)(getAgentLink)(getClientName)(
            getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(hasPartialAuthorisation)(hasPendingAltItsaInvitation)(
            legacySaRelationshipStatusFor)(appConfig)(agent)(Confirmation(true))
            .apply(newState)
        }
      }

    def tryAgainNotMatchedKnownFact(agent: AuthorisedAgent) =
      Transition {
        case KnownFactNotMatched(originalFtr, fastTrackRequest, continueUrl) =>
          val ftrWithoutKF = fastTrackRequest.copy(knownFact = None)

          def stateForMissingKnownFact(forService: String) =
            forService match {
              case HMRCMTDVAT | HMRCMTDIT | HMRCPIR => IdentifyPersonalClient(originalFtr, ftrWithoutKF, continueUrl)
            }

          val tryAgainState = fastTrackRequest match {
            case AgentFastTrackRequest(None, HMRCMTDVAT, _, _, _) =>
              SelectClientTypeVat(originalFtr, fastTrackRequest, continueUrl)

            case AgentFastTrackRequest(_, HMRCCGTPD, _, _, _) =>
              IdentifyCgtClient(originalFtr, fastTrackRequest, continueUrl)

            case AgentFastTrackRequest(_, HMRCPPTORG, _, _, _) =>
              IdentifyPptClient(originalFtr, fastTrackRequest, continueUrl)

            case AgentFastTrackRequest(_, service, _, _, _) =>
              stateForMissingKnownFact(service)
          }

          goto(tryAgainState)

        case TrustNotFound(originalFtr, fastTrackRequest, continueUrl) =>
          goto(IdentifyTrustClient(originalFtr, fastTrackRequest, continueUrl))

        case CgtRefNotFound(originalFtr, fastTrackRequest, continueUrl) =>
          goto(IdentifyCgtClient(originalFtr, fastTrackRequest, continueUrl))

        case PptRefNotFound(originalFtr, fastTrackRequest, continueUrl) =>
          goto(IdentifyPptClient(originalFtr, fastTrackRequest, continueUrl))
      }

    def selectedClientType(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(checkRegDateMatches: CheckRegDateMatches)(
      checkPptKnownFact: CheckPptKnownFact)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(getClientName: GetClientName)(getAgencyEmail: GetAgencyEmail)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(hasPartialAuthorisation: HasPartialAuthorisation)(
      hasPendingAltItsaInvitation: IsAltItsa)(legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)(
      getCgtSubscription: GetCgtSubscription,
      getPptSubscription: GetPptSubscription)(appConfig: AppConfig)(agent: AuthorisedAgent)(suppliedClientType: String) =
      Transition {
        case SelectClientTypeVat(originalFtr, ftr, continueUrl, isChanging) =>
          val isKnownFactRequired = ftr.knownFact.isDefined
          if (isKnownFactRequired) {
            val completeState =
              if (ftr.clientType.contains(Personal)) CheckDetailsCompletePersonalVat
              else CheckDetailsCompleteBusinessVat
            val newState =
              completeState(originalFtr, ftr.copy(clientType = Some(ClientType.toEnum(suppliedClientType))), continueUrl)
            checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(createInvitation)(
              getAgentLink)(getClientName)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(hasPartialAuthorisation)(hasPendingAltItsaInvitation)(
              legacySaRelationshipStatusFor)(appConfig)(agent)(Confirmation(true))
              .apply(newState)
          } else {
            val newState =
              CheckDetailsNoVatRegDate(originalFtr, ftr.copy(clientType = Some(ClientType.toEnum(suppliedClientType))), continueUrl)
            if (isChanging) checkedDetailsChangeInformation(agent).apply(newState)
            else checkedDetailsNoKnownFact(getCgtSubscription, getPptSubscription)(agent).apply(newState)
          }

        case SelectClientTypeCgt(originalFtr, ftr, continueUrl, isChanging) =>
          val newFtr = ftr.copy(clientType = Some(if (suppliedClientType == "trust") Trust else Personal))
          getCgtSubscription(CgtRef(ftr.clientIdentifier)).map {
            case Some(subscription) =>
              if (isChanging) {
                IdentifyCgtClient(originalFtr, newFtr, continueUrl)
              } else if (subscription.isUKBasedClient) {
                ftr.knownFact.fold(ConfirmPostcodeCgt(originalFtr, newFtr, continueUrl, subscription.postCode, subscription.name): State){
                  postcode => val desPostcode = withSpacesRemoved(subscription.postCode.getOrElse(""))
                    if(withSpacesRemoved(postcode) == desPostcode) ConfirmClientCgt(originalFtr, newFtr, continueUrl, subscription.name)
                    else KnownFactNotMatched(originalFtr, newFtr, continueUrl)
                }
              } else {
                ConfirmCountryCodeCgt(originalFtr, newFtr, continueUrl, subscription.countryCode, subscription.name)
              }
            case None =>
              CgtRefNotFound(originalFtr, newFtr, continueUrl)
          }

        case SelectClientTypePpt(originalFtr, ftr, continueUrl, isChanging) =>
          val newFtr = ftr.copy(clientType = Some(ClientType.toEnum(suppliedClientType)))
          getPptSubscription(PptRef(ftr.clientIdentifier)).map {
            case Some(subscription) =>
              if (isChanging)
                IdentifyPptClient(originalFtr, newFtr, continueUrl)
              else newFtr.knownFact
                .fold(ConfirmRegDatePpt(originalFtr, newFtr, continueUrl, subscription.dateOfApplication, subscription.customerName): State){
                  regDate => if(subscription.dateOfApplication == LocalDate.parse(regDate))
                    ConfirmClientPpt(originalFtr, newFtr, continueUrl, subscription.customerName)
                  else KnownFactNotMatched(originalFtr, newFtr, continueUrl)
                }
            case None =>
              PptRefNotFound(originalFtr, newFtr, continueUrl)
          }
      }
  }
}
