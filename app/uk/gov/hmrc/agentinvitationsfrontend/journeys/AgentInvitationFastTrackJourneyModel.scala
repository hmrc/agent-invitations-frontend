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
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType
import uk.gov.hmrc.agentinvitationsfrontend.models.VatKnownFactCheckResult._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators.{urnPattern, utrPattern}
import uk.gov.hmrc.agentmtdidentifiers.model.{Service, Arn, CgtRef, InvitationId, PptRef, SuspensionDetails, TrustTaxIdentifier, Urn, Utr, Vrn}
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
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends CheckDetails

  case class CheckDetailsNoDob(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
      extends CheckDetails

  case class CheckDetailsNoVatRegDate(
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends CheckDetails

  case class CheckDetailsNoClientTypeVat(
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends CheckDetails

  case class CheckDetailsComplete(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends CheckDetails {
    def service: Service = fastTrackRequest.service
    def clientType: Option[ClientType] = fastTrackRequest.clientType
  }

  trait MissingDetail extends State

  case class NoPostcode(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
      extends MissingDetail

  case class NoDob(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
      extends MissingDetail

  case class NoVatRegDate(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
      extends MissingDetail

  case class NoPptRegDate(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
      extends MissingDetail

  trait ClientTypeState extends State

  case class SelectClientTypeVat(
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String],
    isChanging: Boolean = false)
      extends ClientTypeState

  case class SelectClientTypeCgt(
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String],
    isChanging: Boolean = false
  ) extends ClientTypeState

  case class SelectClientTypePpt(
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String],
    isChanging: Boolean = false
  ) extends ClientTypeState

  trait Identify extends State

  case class IdentifyPersonalClient(
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends Identify

  case class IdentifyBusinessClient(
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends Identify

  case class IdentifyNoClientTypeClient(
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends Identify

  case class IdentifyTrustClient(
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends Identify

  case class IdentifyCgtClient(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
      extends Identify

  case class IdentifyPptClient(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
      extends Identify

  case class ConfirmClientTrust(
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String],
    trustName: String)
      extends State

  trait InvitationSent extends State

  case class InvitationSentPersonal(invitationLink: String, continueUrl: Option[String], agencyEmail: String, service: Service, isAltItsa: Boolean)
      extends InvitationSent

  case class InvitationSentBusiness(invitationLink: String, continueUrl: Option[String], agencyEmail: String, service: Service = Service.Vat)
      extends InvitationSent

  case class PendingInvitationExists(fastTrackRequest: AgentFastTrackRequest, agentLink: String, clientName: String, continueUrl: Option[String])
      extends State

  trait AuthExists extends State
  case class ActiveAuthorisationExists(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends AuthExists
  case class PartialAuthorisationExists(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends AuthExists

  trait NotMatched extends State

  case class KnownFactNotMatched(
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String])
      extends NotMatched

  case class ClientNotSignedUp(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State

  case class ClientNotRegistered(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State

  case class TrustNotFound(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String])
      extends NotMatched

  case object TryAgainWithoutFastTrack extends State

  case class ConfirmPostcodeCgt(
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String],
    postcodeFromDes: Option[String],
    clientName: String)
      extends State

  case class ConfirmCountryCodeCgt(
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String],
    countryCode: String,
    clientName: String)
      extends State

  case class ConfirmClientCgt(
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String],
    clientName: String)
      extends State

  case class ConfirmRegDatePpt(
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String],
    registrationDate: LocalDate,
    clientName: String)
      extends State

  case class ConfirmClientPpt(
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String],
    clientName: String)
      extends State

  case class SuspendedAgent(service: Service, continueUrl: Option[String]) extends State

  case class CgtRefNotFound(fastTrackRequest: AgentFastTrackRequest,
                            continueUrl: Option[String]) extends NotMatched

  case class PptRefNotFound(fastTrackRequest: AgentFastTrackRequest,
                            continueUrl: Option[String]) extends NotMatched

  case object AlreadyCopiedAcrossItsa extends State

  case object ClientInsolventFastTrack extends State
  case object CannotCreateFastTrackRequest extends State

  case class LegacyAuthorisationDetected(fastTrackRequest: AgentFastTrackRequest, arn: Arn, invitation: Invitation, continueUrl: Option[String])
      extends State

  object Transitions {
    type HasPendingInvitations = (Arn, String, Service) => Future[Boolean]
    type HasActiveRelationship = (Arn, String, Service) => Future[Boolean]
    type HasPartialAuthorisation = (Arn, String) => Future[Boolean]
    type GetClientName = (String, Service) => Future[Option[String]]
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
        case AgentFastTrackRequest(_, Service.MtdIt, _, _, knownFact) =>
          val updatedPersonalRequest = fastTrackRequest.copy(clientType = Some(ClientType.Personal))
          knownFact match {
            case None => goto(CheckDetailsNoPostcode(updatedPersonalRequest, continueUrl))
            case Some(_) => goto(CheckDetailsComplete(updatedPersonalRequest, continueUrl))
          }

        case AgentFastTrackRequest(_, Service.PersonalIncomeRecord, _, _, knownFact) =>
          val updatedPersonalRequest = fastTrackRequest.copy(clientType = Some(ClientType.Personal))
          knownFact match {
            case None => goto(CheckDetailsNoDob(updatedPersonalRequest, continueUrl))
            case Some(_) => goto(CheckDetailsComplete(updatedPersonalRequest, continueUrl))
          }

        case AgentFastTrackRequest(None, Service.Vat, _, _, _) =>
          goto(CheckDetailsNoClientTypeVat(fastTrackRequest, continueUrl))

        case AgentFastTrackRequest(_, Service.Vat, _, _, knownFact) =>
          knownFact match {
            case None => goto(CheckDetailsNoVatRegDate(fastTrackRequest, continueUrl))
            case Some(_) => goto(CheckDetailsComplete(fastTrackRequest, continueUrl))
          }

        case AgentFastTrackRequest(_, Service.Trust | Service.TrustNT, _, _, _) =>
          goto(CheckDetailsComplete(fastTrackRequest.copy(clientType = Some(ClientType.Trust)), continueUrl))

        case AgentFastTrackRequest(_, _, _, _, _) =>
          goto(CheckDetailsComplete(fastTrackRequest, continueUrl))
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
                             if (appConfig.featuresAltItsa && invitation.service == Service.MtdIt)
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
      case CheckDetailsNoClientTypeVat(fastTrackRequest, continueUrl) =>
        goto(SelectClientTypeVat(fastTrackRequest, continueUrl))
      case NoVatRegDate(fastTrackRequest, continueUrl) =>
        goto(SelectClientTypeVat(fastTrackRequest, continueUrl))
      case cdc@CheckDetailsComplete(fastTrackRequest, continueUrl) if cdc.service == Service.CapitalGains =>
        goto(SelectClientTypeCgt(fastTrackRequest, continueUrl))
      case cdc@CheckDetailsComplete(fastTrackRequest, continueUrl) if cdc.service == Service.Ppt =>
        goto(SelectClientTypePpt(fastTrackRequest, continueUrl))
    }

    def checkedDetailsNoKnownFact(getCgtSubscription: GetCgtSubscription, getPptSubscription: GetPptSubscription)(agent: AuthorisedAgent) =
      Transition {
        case CheckDetailsNoPostcode(fastTrackRequest, continueUrl) =>
          goto(NoPostcode(fastTrackRequest, continueUrl))

        case CheckDetailsNoDob(fastTrackRequest, continueUrl) =>
          goto(NoDob(fastTrackRequest, continueUrl))

        case CheckDetailsNoVatRegDate(fastTrackRequest, continueUrl) =>
          goto(NoVatRegDate(fastTrackRequest, continueUrl))

        case cdc@CheckDetailsComplete(fastTrackRequest, continueUrl) if cdc.service == Service.CapitalGains =>
          val cgtRef = CgtRef(fastTrackRequest.clientIdentifier)

          getCgtSubscription(cgtRef).map {
            case Some(subscription) =>
              if (subscription.isUKBasedClient) {
                ConfirmPostcodeCgt(fastTrackRequest, continueUrl, subscription.postCode, subscription.name)
              } else {
                ConfirmCountryCodeCgt(fastTrackRequest, continueUrl, subscription.countryCode, subscription.name)
              }
            case None =>
              CgtRefNotFound(fastTrackRequest, continueUrl)
          }

        case cdc@CheckDetailsComplete(fastTrackRequest, continueUrl) if cdc.service == Service.Ppt =>
          val pptRef = PptRef(fastTrackRequest.clientIdentifier)

          getPptSubscription(pptRef).map {
            case Some(subscription) =>
              ConfirmRegDatePpt(fastTrackRequest, continueUrl, subscription.dateOfApplication, subscription.customerName)
            case None =>
              PptRefNotFound(fastTrackRequest, continueUrl)
          }
      }

    def confirmPostcodeCgt(agent: AuthorisedAgent)(postcode: Postcode): Transition =
      Transition {
        case ConfirmPostcodeCgt(fastTrackRequest, continueUrl, postcodeFromDes, name) =>
          val desPostcode = postcodeFromDes.map(withSpacesRemoved)
          val ftrPostcode = withSpacesRemoved(postcode.value)
          if (desPostcode.getOrElse("no_postcode") == ftrPostcode) {
            goto(ConfirmClientCgt(fastTrackRequest.copy(knownFact = Some(postcode.value)), continueUrl, name))
          } else {
            goto(KnownFactNotMatched(fastTrackRequest, continueUrl))
          }
      }

    def confirmCountryCodeCgt(agent: AuthorisedAgent)(countryCode: CountryCode): Transition =
      Transition {
        case ConfirmCountryCodeCgt(fastTrackRequest, continueUrl, countryCodeFromDes, name) =>
          if (countryCodeFromDes.contains(countryCode.value)) {
            goto(ConfirmClientCgt(fastTrackRequest.copy(knownFact = Some(countryCode.value)), continueUrl, name))
          } else {
            goto(KnownFactNotMatched(fastTrackRequest, continueUrl))
          }
      }


    private def withSpacesRemoved(postcode: String): String =
      postcode.replace(" ", "")

    def identifyCgtClient(getCgtSubscription: GetCgtSubscription)(agent: AuthorisedAgent)(cgtClient: CgtClient): Transition =
      Transition {
        case IdentifyCgtClient(fastTrackRequest, continueUrl) =>
          getCgtSubscription(CgtRef(cgtClient.cgtRef.value)) map {
            case Some(subscription) =>
              val ftrKnownFact = fastTrackRequest.knownFact.map(withSpacesRemoved)
              val newFtr = fastTrackRequest.copy(clientIdentifier = cgtClient.cgtRef.value)
              if (subscription.isUKBasedClient) {
                val desPostcode = subscription.postCode.map(withSpacesRemoved)
                if(ftrKnownFact.contains(desPostcode.getOrElse("no_postcode"))) {
                  ConfirmClientCgt(newFtr, continueUrl, subscription.name)
                }
                else ConfirmPostcodeCgt(newFtr, continueUrl, subscription.postCode, subscription.name)
              } else {
                val desCountryCode = withSpacesRemoved(subscription.countryCode)
                if(ftrKnownFact.contains(desCountryCode)) {
                  ConfirmClientCgt(newFtr, continueUrl, subscription.name)
                }
                else ConfirmCountryCodeCgt(newFtr, continueUrl, subscription.countryCode, subscription.name)
              }
            case None => CgtRefNotFound(fastTrackRequest, continueUrl)
          }
      }

    def confirmRegDatePpt(agent: AuthorisedAgent)(regDate: String): Transition =
      Transition {
        case ConfirmRegDatePpt(fastTrackRequest, continueUrl, actualRegDate, name) =>
          if (actualRegDate.equals(LocalDate.parse(regDate))) {
            goto(ConfirmClientPpt(fastTrackRequest, continueUrl, name))
          } else {
            goto(KnownFactNotMatched(fastTrackRequest, continueUrl))
          }
      }

    def identifyPptClient(checkKnownFact: PptClient => Future[Boolean], getPptCustomerName: PptRef => Future[Option[String]])(agent: AuthorisedAgent)(
      pptClient: PptClient): Transition =
      Transition {
        case IdentifyPptClient(fastTrackRequest, continueUrl) =>
          for {
            isCheckOK     <- checkKnownFact(pptClient)
            mCustomerName <- if (isCheckOK) getPptCustomerName(PptRef(fastTrackRequest.clientIdentifier)) else Future.successful(None)
          } yield {
            mCustomerName match {
              case Some(customerName) => ConfirmClientPpt(fastTrackRequest, continueUrl, customerName)
              case None               => PptRefNotFound(fastTrackRequest, continueUrl)
            }
          }
      }

    def checkedDetailsChangeInformation(agent: AuthorisedAgent): AgentInvitationFastTrackJourneyModel.Transition = {
      def gotoIdentifyClient(ftRequest: AgentFastTrackRequest, continueUrl: Option[String]) =
        if (ftRequest.clientType.contains(ClientType.Personal)) {
          goto(IdentifyPersonalClient(ftRequest, continueUrl))
        } else if (ftRequest.clientType.contains(ClientType.Business)) {
          goto(IdentifyBusinessClient(ftRequest, continueUrl))
        } else {
          goto(IdentifyNoClientTypeClient(ftRequest, continueUrl))
        }

      Transition {
        case cdc@CheckDetailsComplete(ftr, continueUrl) if cdc.service == Service.CapitalGains =>
          if (ftr.clientType.isEmpty) goto(SelectClientTypeCgt(ftr, continueUrl, isChanging = true))
          else goto(IdentifyCgtClient(ftr, continueUrl))

        case cdc@CheckDetailsComplete(ftr, continueUrl) if cdc.service == Service.Ppt =>
          if (ftr.clientType.isEmpty) goto(SelectClientTypePpt(ftr, continueUrl, isChanging = true))
          else goto(IdentifyPptClient(ftr, continueUrl))

        case CheckDetailsNoPostcode(ftr, continueUrl) =>
          goto(IdentifyPersonalClient(ftr, continueUrl))

        case CheckDetailsNoDob(ftr, continueUrl) =>
          goto(IdentifyPersonalClient(ftr, continueUrl))

        case CheckDetailsNoVatRegDate(ftr, continueUrl) =>
          gotoIdentifyClient(ftr, continueUrl)

        case CheckDetailsNoClientTypeVat(ftr, continueUrl) =>
          goto(SelectClientTypeVat(ftr, continueUrl, isChanging = true))

        case cdc@CheckDetailsComplete(ftr, continueUrl) =>
          cdc.clientType match {
            case Some(ClientType.Personal) => goto(IdentifyPersonalClient(ftr, continueUrl))
            case Some(ClientType.Business) => goto(IdentifyBusinessClient(ftr, continueUrl))
            case Some(ClientType.Trust) => goto(IdentifyTrustClient(ftr, continueUrl))
          }
      }
    }

    def checkedDetailsAllInformation(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(checkPptKnownFact: CheckPptKnownFact)(getCgtSubscription: GetCgtSubscription)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(getClientName: GetClientName)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      hasPartialAuthorisation: HasPartialAuthorisation)(hasPendingAltItsaInvitation: IsAltItsa)(
      legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)(appConfig: AppConfig)(agent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {
        case cdc@CheckDetailsComplete(fastTrackRequest, continueUrl) if cdc.service == Service.MtdIt => {
          if (confirmation.choice) {
            checkPostcodeMatches(Nino(fastTrackRequest.clientIdentifier), fastTrackRequest.knownFact.getOrElse(""))
              .flatMap {
                case Some(true) =>
                  checkIfPendingOrActiveAndGoto(
                    fastTrackRequest,
                    agent.arn,
                    Invitation(fastTrackRequest.clientType, Service.MtdIt, Nino(fastTrackRequest.clientIdentifier)),
                    continueUrl
                  )(
                    hasPendingInvitations,
                    hasActiveRelationship,
                    hasPartialAuthorisation,
                    hasPendingAltItsaInvitation,
                    legacySaRelationshipStatusFor,
                    appConfig)(createInvitation, getAgentLink, getAgencyEmail, getClientName)
                case Some(false) => goto(KnownFactNotMatched(fastTrackRequest, continueUrl))
                case None =>
                  if (appConfig.featuresAltItsa) goto(ClientNotRegistered(fastTrackRequest, continueUrl))
                  else goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
              }
          } else goto(IdentifyPersonalClient(fastTrackRequest, continueUrl))
        }

        case cdc@CheckDetailsComplete(fastTrackRequest, continueUrl) if cdc.service == Service.PersonalIncomeRecord =>
          if (confirmation.choice) {
            val knownFact = fastTrackRequest.knownFact.getOrElse("")
            checkDobMatches(Nino(fastTrackRequest.clientIdentifier), LocalDate.parse(knownFact))
              .flatMap {
                case Some(true) =>
                  checkIfPendingOrActiveAndGoto(
                    fastTrackRequest,
                    agent.arn,
                    Invitation(fastTrackRequest.clientType, Service.PersonalIncomeRecord, Nino(fastTrackRequest.clientIdentifier)),
                    continueUrl
                  )(
                    hasPendingInvitations,
                    hasActiveRelationship,
                    hasPartialAuthorisation,
                    hasPendingAltItsaInvitation,
                    legacySaRelationshipStatusFor,
                    appConfig)(createInvitation, getAgentLink, getAgencyEmail, getClientName)
                case _ => goto(KnownFactNotMatched(fastTrackRequest, continueUrl))
              }
          } else goto(IdentifyPersonalClient(fastTrackRequest, continueUrl))

        case cdc@CheckDetailsComplete(fastTrackRequest, continueUrl) if (cdc.service == Service.Vat && cdc.clientType.contains(ClientType.Personal)) =>
          if (confirmation.choice) {
            val knownFact = fastTrackRequest.knownFact.getOrElse("")
            checkRegDateMatches(Vrn(fastTrackRequest.clientIdentifier), LocalDate.parse(knownFact))
              .flatMap {
                case VatKnownFactCheckOk =>
                  checkIfPendingOrActiveAndGoto(
                    fastTrackRequest,
                    agent.arn,
                    Invitation(Some(ClientType.Personal), Service.Vat, Vrn(fastTrackRequest.clientIdentifier)),
                    continueUrl
                  )(
                    hasPendingInvitations,
                    hasActiveRelationship,
                    hasPartialAuthorisation,
                    hasPendingAltItsaInvitation,
                    legacySaRelationshipStatusFor,
                    appConfig)(createInvitation, getAgentLink, getAgencyEmail, getClientName)
                case VatKnownFactNotMatched       => goto(KnownFactNotMatched(fastTrackRequest, continueUrl))
                case VatRecordClientInsolvent     => goto(ClientInsolventFastTrack)
                case VatRecordMigrationInProgress => goto(CannotCreateFastTrackRequest)
                case VatDetailsNotFound           => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
              }
          } else goto(IdentifyPersonalClient(fastTrackRequest, continueUrl))

        case cdc@CheckDetailsComplete(fastTrackRequest, continueUrl) if (cdc.service == Service.Vat && cdc.clientType.contains(ClientType.Business)) =>
          if (confirmation.choice) {
            val knownFact = fastTrackRequest.knownFact.getOrElse("")
            checkRegDateMatches(Vrn(fastTrackRequest.clientIdentifier), LocalDate.parse(knownFact))
              .flatMap {
                case VatKnownFactCheckOk =>
                  checkIfPendingOrActiveAndGoto(
                    fastTrackRequest,
                    agent.arn,
                    Invitation(Some(ClientType.Business), Service.Vat, Vrn(fastTrackRequest.clientIdentifier)),
                    continueUrl
                  )(
                    hasPendingInvitations,
                    hasActiveRelationship,
                    hasPartialAuthorisation,
                    hasPendingAltItsaInvitation,
                    legacySaRelationshipStatusFor,
                    appConfig)(createInvitation, getAgentLink, getAgencyEmail, getClientName)
                case VatKnownFactNotMatched       => goto(KnownFactNotMatched(fastTrackRequest, continueUrl))
                case VatDetailsNotFound           => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
                case VatRecordClientInsolvent     => goto(ClientInsolventFastTrack)
                case VatRecordMigrationInProgress => goto(CannotCreateFastTrackRequest)
              }
          } else goto(IdentifyBusinessClient(fastTrackRequest, continueUrl))

        case cdc@CheckDetailsComplete(fastTrackRequest, continueUrl) if List(Service.Trust, Service.TrustNT).contains(cdc.service) =>
          if (confirmation.choice) {
            getClientName(fastTrackRequest.clientIdentifier, fastTrackRequest.service).flatMap {
              case Some(_) =>
                val trustInvitation = fastTrackRequest.clientIdentifier match {
                  case utr if utr.matches(utrPattern) => Invitation(Some(ClientType.Trust), Service.Trust, Utr(utr))
                  case urn if urn.matches(urnPattern) => Invitation(Some(ClientType.Trust), Service.TrustNT, Urn(urn))
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
              case None => goto(TrustNotFound(fastTrackRequest, continueUrl))
            }
          } else goto(IdentifyTrustClient(fastTrackRequest, continueUrl))

        case cdc@CheckDetailsComplete(fastTrackRequest, continueUrl) if cdc.service == Service.CapitalGains =>
          if (confirmation.choice) {
            getCgtSubscription(CgtRef(fastTrackRequest.clientIdentifier)).flatMap {
              case Some(subscription) =>
                val (knownFactEmptyState, field) = if (subscription.isUKBasedClient) {
                  (ConfirmPostcodeCgt(fastTrackRequest, continueUrl, subscription.postCode, subscription.name),
                  subscription.postCode)
                } else
                  (ConfirmCountryCodeCgt(fastTrackRequest, continueUrl, subscription.countryCode, subscription.name),
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
                          Invitation(fastTrackRequest.clientType, Service.CapitalGains, CgtRef(fastTrackRequest.clientIdentifier)),
                          continueUrl
                        )(
                          hasPendingInvitations,
                          hasActiveRelationship,
                          hasPartialAuthorisation,
                          hasPendingAltItsaInvitation,
                          legacySaRelationshipStatusFor,
                          appConfig)(createInvitation, getAgentLink, getAgencyEmail, getClientName)
                      else goto(KnownFactNotMatched(fastTrackRequest, continueUrl))
                  }
              case None => goto(CgtRefNotFound(fastTrackRequest, continueUrl))
          }
          } else goto(IdentifyCgtClient(fastTrackRequest, continueUrl))

        case cdc@CheckDetailsComplete(fastTrackRequest, continueUrl) if cdc.service == Service.Ppt =>
          if (confirmation.choice) {
            val knownFact = fastTrackRequest.knownFact.getOrElse("")
            checkPptKnownFact(PptRef(fastTrackRequest.clientIdentifier), LocalDate.parse(knownFact))
              .flatMap {
                case true =>
                  checkIfPendingOrActiveAndGoto(
                    fastTrackRequest,
                    agent.arn,
                    Invitation(fastTrackRequest.clientType, Service.Ppt, PptRef(fastTrackRequest.clientIdentifier)),
                    continueUrl
                  )(
                    hasPendingInvitations,
                    hasActiveRelationship,
                    hasPartialAuthorisation,
                    hasPendingAltItsaInvitation,
                    legacySaRelationshipStatusFor,
                    appConfig)(createInvitation, getAgentLink, getAgencyEmail, getClientName)
                case false => goto(KnownFactNotMatched(fastTrackRequest, continueUrl))
              }
          } else goto(IdentifyPptClient(fastTrackRequest, continueUrl))
      }

    def identifiedClientItsa(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(checkRegDateMatches: CheckRegDateMatches)(
      checkPptKnownFact: CheckPptKnownFact)(getCgtSubscription: GetCgtSubscription)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(getClientName: GetClientName)(getAgencyEmail: GetAgencyEmail)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(hasPendingAltItsaInvitation: IsAltItsa)(
      hasPartialAuthorisation: HasPartialAuthorisation)(legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)(appConfig: AppConfig)(
      agent: AuthorisedAgent)(itsaClient: ItsaClient) =
      Transition {
        case IdentifyPersonalClient(ftr, continueUrl) =>
          val newState = CheckDetailsComplete(
            ftr.copy(service = Service.MtdIt, clientIdentifier = itsaClient.clientIdentifier, knownFact = Some(itsaClient.postcode)),
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
        case IdentifyPersonalClient(ftr, continueUrl) =>
          val newState =
            CheckDetailsComplete(
              ftr.copy(service = Service.PersonalIncomeRecord, clientIdentifier = irvClient.clientIdentifier, knownFact = Some(irvClient.dob)),
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
        case IdentifyPersonalClient(ftr, continueUrl) =>
          val newState = CheckDetailsComplete(
            ftr.copy(service = Service.Vat, clientType = Some(ClientType.Personal), clientIdentifier = vatClient.clientIdentifier, knownFact = Some(vatClient.registrationDate)),
            continueUrl)
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(createInvitation)(getAgentLink)(getClientName)(
            getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(hasPartialAuthorisation)(hasPendingAltItsaInvitation)(
            legacySaRelationshipStatusFor)(appConfig)(agent)(Confirmation(true))
            .apply(newState)
        case IdentifyBusinessClient(ftr, continueUrl) =>
          val newState = CheckDetailsComplete(
            ftr.copy(service = Service.Vat, clientType = Some(ClientType.Business), clientIdentifier = vatClient.clientIdentifier, knownFact = Some(vatClient.registrationDate)),
            continueUrl)
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(createInvitation)(getAgentLink)(getClientName)(
            getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(hasPartialAuthorisation)(hasPendingAltItsaInvitation)(
            legacySaRelationshipStatusFor)(appConfig)(agent)(Confirmation(true))
            .apply(newState)

        case IdentifyNoClientTypeClient(ftr, continueUrl) =>
          val newState = CheckDetailsNoClientTypeVat(
            ftr.copy(clientIdentifier = vatClient.clientIdentifier, knownFact = Some(vatClient.registrationDate)),
            continueUrl)
          checkedDetailsNoClientType(agent).apply(newState)
      }

    def showConfirmTrustClient(getTrustName: GetTrustName)(agent: AuthorisedAgent)(trustClient: TrustClient) =
      Transition {
        case IdentifyTrustClient(ftr, continueUrl) =>
          getTrustName(trustClient.taxId).flatMap { trustResponse =>
            trustResponse.response match {
              case Right(TrustName(name)) =>
                goto(ConfirmClientTrust(ftr.copy(clientIdentifier = trustClient.taxId.value), continueUrl, name))
              case Left(invalidTrust) =>
                logger.warn(s"Des returned $invalidTrust response for utr: ${trustClient.taxId}")
                goto(TrustNotFound(ftr, continueUrl))
            }
          }
      }

    def submitConfirmTrustClient(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(getClientName: GetClientName)(getAgencyEmail: GetAgencyEmail)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(hasPartialAuthorisation: HasPartialAuthorisation)(
      hasPendingAltItsaInvitation: IsAltItsa)(legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)(appConfig: AppConfig)(
      agent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {
        case ConfirmClientTrust(ftr, continueUrl, trustName) =>
          if (confirmation.choice) {
            val trustInvitation = ftr.clientIdentifier match {
              case utr if utr.matches(utrPattern) => Invitation(Some(ClientType.Trust), Service.Trust, Utr(utr))
              case urn if urn.matches(urnPattern) => Invitation(Some(ClientType.Trust), Service.TrustNT, Urn(urn))
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
            goto(IdentifyTrustClient(ftr, continueUrl))
          }
      }

    def submitConfirmClientCgt(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(getClientName: GetClientName)(getAgencyEmail: GetAgencyEmail)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(hasPartialAuthorisation: HasPartialAuthorisation)(
      hasPendingAltItsaInvitation: IsAltItsa)(legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)(appConfig: AppConfig)(
      agent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {
        case ConfirmClientCgt(ftr, continueUrl, cgtName) =>
          if (confirmation.choice) {
            checkIfPendingOrActiveAndGoto(
              ftr,
              agent.arn,
              Invitation(ftr.clientType, Service.CapitalGains, CgtRef(ftr.clientIdentifier)),
              continueUrl
            )(
              hasPendingInvitations,
              hasActiveRelationship,
              hasPartialAuthorisation,
              hasPendingAltItsaInvitation,
              legacySaRelationshipStatusFor,
              appConfig)(createInvitation, getAgentLink, getAgencyEmail, getClientName)
          } else {
            goto(IdentifyCgtClient(ftr, continueUrl))
          }
      }

    def submitConfirmClientPpt(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(getClientName: GetClientName)(getAgencyEmail: GetAgencyEmail)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(hasPartialAuthorisation: HasPartialAuthorisation)(
      hasPendingAltItsaInvitation: IsAltItsa)(legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)(appConfig: AppConfig)(
      agent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {
        case ConfirmClientPpt(ftr, continueUrl, pptName) =>
          if (confirmation.choice) {
            checkIfPendingOrActiveAndGoto(
              ftr,
              agent.arn,
              Invitation(ftr.clientType, Service.Ppt, PptRef(ftr.clientIdentifier)),
              continueUrl
            )(
              hasPendingInvitations,
              hasActiveRelationship,
              hasPartialAuthorisation,
              hasPendingAltItsaInvitation,
              legacySaRelationshipStatusFor,
              appConfig)(createInvitation, getAgentLink, getAgencyEmail, getClientName)
          } else {
            goto(IdentifyPptClient(ftr, continueUrl))
          }
      }

    def moreDetailsItsa(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(checkRegDateMatches: CheckRegDateMatches)(
      checkPptKnownFact: CheckPptKnownFact)(getCgtSubscription: GetCgtSubscription)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(getClientName: GetClientName)(getAgencyEmail: GetAgencyEmail)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(hasPartialAuthorisation: HasPartialAuthorisation)(
      hasPendingAltItsaInvitation: IsAltItsa)(legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)(appConfig: AppConfig)(
      agent: AuthorisedAgent)(suppliedKnownFact: String) =
      Transition {
        case NoPostcode(ftr, continueUrl) =>
          val newState = CheckDetailsComplete(ftr.copy(service = Service.MtdIt, knownFact = Some(suppliedKnownFact)), continueUrl)
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
        case NoDob(ftr, continueUrl) =>
          val newState =
            CheckDetailsComplete(ftr.copy(service = Service.PersonalIncomeRecord, knownFact = Some(suppliedKnownFact)), continueUrl)
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
        case NoVatRegDate(ftRequest, continueUrl) => {
          val clientType =
            if (ftRequest.clientType.contains(ClientType.Personal))
              ClientType.Personal
            else
              ClientType.Business

          val newState =
            CheckDetailsComplete(ftRequest.copy(service = Service.Vat, clientType = Some(clientType), knownFact = Some(suppliedKnownFact)), continueUrl)

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
        case NoPptRegDate(ftRequest, continueUrl) => {
          val newState =
            CheckDetailsComplete(ftRequest.copy(service = Service.Ppt, knownFact = Some(suppliedKnownFact)), continueUrl)

          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(createInvitation)(getAgentLink)(getClientName)(
            getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(hasPartialAuthorisation)(hasPendingAltItsaInvitation)(
            legacySaRelationshipStatusFor)(appConfig)(agent)(Confirmation(true))
            .apply(newState)
        }
      }

    def tryAgainNotMatchedKnownFact(agent: AuthorisedAgent) =
      Transition {
        case KnownFactNotMatched(fastTrackRequest, continueUrl) =>
          val ftrWithoutKF = fastTrackRequest.copy(knownFact = None)

          def stateForMissingKnownFact(forService: Service) =
            forService match {
              case Service.Vat | Service.MtdIt | Service.PersonalIncomeRecord => IdentifyPersonalClient(ftrWithoutKF, continueUrl)
            }

          val tryAgainState = fastTrackRequest match {
            case AgentFastTrackRequest(None, Service.Vat, _, _, _) =>
              SelectClientTypeVat(fastTrackRequest, continueUrl)

            case AgentFastTrackRequest(_, Service.CapitalGains, _, _, _) =>
              IdentifyCgtClient(fastTrackRequest, continueUrl)

            case AgentFastTrackRequest(_, Service.Ppt, _, _, _) =>
              IdentifyPptClient(fastTrackRequest, continueUrl)

            case AgentFastTrackRequest(_, service, _, _, _) =>
              stateForMissingKnownFact(service)
          }

          goto(tryAgainState)

        case TrustNotFound(fastTrackRequest, continueUrl) =>
          goto(IdentifyTrustClient(fastTrackRequest, continueUrl))

        case CgtRefNotFound(fastTrackRequest, continueUrl) =>
          goto(IdentifyCgtClient(fastTrackRequest, continueUrl))

        case PptRefNotFound(fastTrackRequest, continueUrl) =>
          goto(IdentifyPptClient(fastTrackRequest, continueUrl))
      }

    def selectedClientType(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(checkRegDateMatches: CheckRegDateMatches)(
      checkPptKnownFact: CheckPptKnownFact)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(getClientName: GetClientName)(getAgencyEmail: GetAgencyEmail)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(hasPartialAuthorisation: HasPartialAuthorisation)(
      hasPendingAltItsaInvitation: IsAltItsa)(legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)(
      getCgtSubscription: GetCgtSubscription,
      getPptSubscription: GetPptSubscription)(appConfig: AppConfig)(agent: AuthorisedAgent)(suppliedClientType: String) =
      Transition {
        case SelectClientTypeVat(ftr, continueUrl, isChanging) =>
          val isKnownFactRequired = ftr.knownFact.isDefined
          if (isKnownFactRequired) {
            val newState =
              CheckDetailsComplete(ftr.copy(service = Service.Vat, clientType = Some(ClientType.toEnum(suppliedClientType))), continueUrl)
            checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(createInvitation)(
              getAgentLink)(getClientName)(getAgencyEmail)(hasPendingInvitations)(hasActiveRelationship)(hasPartialAuthorisation)(hasPendingAltItsaInvitation)(
              legacySaRelationshipStatusFor)(appConfig)(agent)(Confirmation(true))
              .apply(newState)
          } else {
            val newState =
              CheckDetailsNoVatRegDate(ftr.copy(clientType = Some(ClientType.toEnum(suppliedClientType))), continueUrl)
            if (isChanging) checkedDetailsChangeInformation(agent).apply(newState)
            else checkedDetailsNoKnownFact(getCgtSubscription, getPptSubscription)(agent).apply(newState)
          }

        case SelectClientTypeCgt(ftr, continueUrl, isChanging) =>
          val newFtr = ftr.copy(clientType = Some(if (suppliedClientType == "trust") ClientType.Trust else ClientType.Personal))
          getCgtSubscription(CgtRef(ftr.clientIdentifier)).map {
            case Some(subscription) =>
              if (isChanging) {
                IdentifyCgtClient(newFtr, continueUrl)
              } else if (subscription.isUKBasedClient) {
                ftr.knownFact.fold(ConfirmPostcodeCgt(newFtr, continueUrl, subscription.postCode, subscription.name): State){
                  postcode => val desPostcode = withSpacesRemoved(subscription.postCode.getOrElse(""))
                    if(withSpacesRemoved(postcode) == desPostcode) ConfirmClientCgt(newFtr, continueUrl, subscription.name)
                    else KnownFactNotMatched(newFtr, continueUrl)
                }
              } else {
                ConfirmCountryCodeCgt(newFtr, continueUrl, subscription.countryCode, subscription.name)
              }
            case None =>
              CgtRefNotFound(newFtr, continueUrl)
          }

        case SelectClientTypePpt(ftr, continueUrl, isChanging) =>
          val newFtr = ftr.copy(clientType = Some(ClientType.toEnum(suppliedClientType)))
          getPptSubscription(PptRef(ftr.clientIdentifier)).map {
            case Some(subscription) =>
              if (isChanging)
                IdentifyPptClient(newFtr, continueUrl)
              else newFtr.knownFact
                .fold(ConfirmRegDatePpt(newFtr, continueUrl, subscription.dateOfApplication, subscription.customerName): State){
                  regDate => if(subscription.dateOfApplication == LocalDate.parse(regDate))
                    ConfirmClientPpt(newFtr, continueUrl, subscription.customerName)
                  else KnownFactNotMatched(newFtr, continueUrl)
                }
            case None =>
              PptRefNotFound(newFtr, continueUrl)
          }
      }
  }
}
