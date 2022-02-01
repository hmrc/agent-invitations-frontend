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
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR, _}
import uk.gov.hmrc.agentinvitationsfrontend.models.VatKnownFactCheckResult.{VatDetailsNotFound, VatKnownFactCheckOk, VatKnownFactNotMatched, VatRecordClientInsolvent, VatRecordMigrationInProgress}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.util.toFuture
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, CgtRef, PptRef, TrustTaxIdentifier, Urn, Utr, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.fsm.JourneyModel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object AgentInvitationJourneyModel extends JourneyModel with Logging {

  sealed trait State

  val root: State = SelectClientType(Set.empty)

  case class SelectClientType(basket: Basket) extends State

  trait SelectService extends State
  case class SelectPersonalService(services: Set[String], basket: Basket) extends SelectService
  case class SelectBusinessService(services: Set[String], basket: Basket) extends SelectService
  case class SelectTrustService(services: Set[String], basket: Basket) extends SelectService

  trait Identify extends State
  case class IdentifyClient(clientType: ClientType, service: String, basket: Basket) extends Identify

  case class PendingInvitationExists(clientType: ClientType, agentLink: String, basket: Basket) extends State

  trait AuthorisationExists extends State
  case class ActiveAuthorisationExists(clientType: ClientType, service: String, basket: Basket) extends AuthorisationExists
  case class PartialAuthorisationExists(basket: Basket) extends AuthorisationExists
  case class LegacyAuthorisationDetected(basket: Basket) extends State

  trait NotFound extends State
  case class KnownFactNotMatched(basket: Basket) extends NotFound
  case class TrustNotFound(basket: Basket) extends NotFound
  case class CgtRefNotFound(cgtRef: CgtRef, basket: Basket) extends NotFound
  case class PptRefNotFound(pptRef: PptRef, basket: Basket) extends NotFound

  case class CannotCreateRequest(basket: Basket) extends State

  trait Confirm extends State
  case class ConfirmClientItsa(request: AuthorisationRequest, basket: Basket) extends Confirm
  case class ConfirmClientPersonalVat(request: AuthorisationRequest, basket: Basket, clientInsolvent: Boolean = false) extends Confirm
  case class ConfirmClientBusinessVat(request: AuthorisationRequest, basket: Basket, clientInsolvent: Boolean = false) extends Confirm
  case class ConfirmClientTrust(request: AuthorisationRequest, basket: Basket) extends Confirm
  case class ConfirmClientTrustNT(clientName: String, urn: Urn) extends Confirm
  case class ConfirmClientCgt(request: AuthorisationRequest, basket: Basket) extends Confirm
  case class ConfirmClientPpt(request: AuthorisationRequest, basket: Basket) extends Confirm
  case class ConfirmPostcodeCgt(cgtRef: CgtRef, clientType: ClientType, basket: Basket, postcode: Option[String], clientName: String) extends Confirm
  case class ConfirmCountryCodeCgt(cgtRef: CgtRef, clientType: ClientType, basket: Basket, countryCode: String, clientName: String) extends Confirm

  trait Review extends State
  case class ReviewAuthorisations(clientType: ClientType, services: Set[String], basket: Basket) extends Review

  case class SomeAuthorisationsFailed(invitationLink: String, continueUrl: Option[String], agencyEmail: String, basket: Basket) extends State
  case class AllAuthorisationsFailed(basket: Basket) extends State
  case class DeleteAuthorisationRequest(clientType: ClientType, authorisationRequest: AuthorisationRequest, basket: Basket) extends State

  trait InvitationSent extends State
  case class InvitationSentPersonal(
    invitationLink: String,
    continueUrl: Option[String],
    agencyEmail: String,
    services: Set[String],
    isAltItsa: Boolean)
      extends InvitationSent

  case class InvitationSentBusiness(invitationLink: String, continueUrl: Option[String], agencyEmail: String, services: Set[String])
      extends InvitationSent
  case class InvitationSentTrust(invitationLink: String, continueUrl: Option[String], agencyEmail: String, services: Set[String])
      extends InvitationSent
  case class ClientNotSignedUp(service: String, basket: Basket) extends InvitationSent
  case object AllAuthorisationsRemoved extends State
  case class AgentSuspended(suspendedService: String, basket: Basket) extends State
  case class ClientNotRegistered(basket: Basket) extends State
  case object AlreadyCopiedAcrossItsa extends State
  case class ClientInsolvent(basket: Basket) extends State

  type Basket = Set[AuthorisationRequest]

  object Transitions {
    val start: AgentInvitationJourneyModel.Transition = AgentInvitationJourneyModel.start

    type HasPendingInvitations = (Arn, String, String) => Future[Boolean]
    type CreateInvitationSent = (String, String, Arn, Basket) => Future[State]
    type HasActiveRelationship = (Arn, String, String) => Future[Boolean]
    type HasPartialAuthorisation = (Arn, String) => Future[Boolean]
    type GetClientName = (String, String) => Future[Option[String]]
    type CheckPostcodeMatches = (Nino, String) => Future[Option[Boolean]]
    type CheckRegDateMatches = (Vrn, LocalDate) => Future[VatKnownFactCheckResult]
    type CreateMultipleInvitations =
      (Arn, Set[AuthorisationRequest]) => Future[Set[AuthorisationRequest]]
    type GetAgentLink = (Arn, Option[ClientType]) => Future[String]
    type GetAgencyEmail = () => Future[String]
    type GetTrustName = TrustTaxIdentifier => Future[TrustResponse]
    type GetCgtSubscription = CgtRef => Future[Option[CgtSubscription]]
    type GetPptSubscription = PptRef => Future[Option[PptSubscription]]
    type GetSuspensionDetails = () => Future[SuspensionDetails]
    type LegacySaRelationshipStatusFor = (Arn, String) => Future[LegacySaRelationshipResult]

    def selectedClientType(agent: AuthorisedAgent)(clientType: String): Transition = Transition {
      case SelectClientType(basket) =>
        clientType match {
          case "personal" => goto(SelectPersonalService(agent.personalServices, basket))
          case "business" => goto(SelectBusinessService(agent.businessServices, basket))
          case "trust"    => goto(SelectTrustService(agent.trustServices, basket))
        }
    }

    def gotoIdentify(
      serviceEnabled: Boolean,
      agentSuspensionEnabled: Boolean,
      getSuspensionDetails: GetSuspensionDetails,
      arn: Arn,
      service: String,
      identifyClientState: State,
      suspendedState: State): Future[State] =
      (serviceEnabled, agentSuspensionEnabled) match {
        case (true, true) =>
          getSuspensionDetails().flatMap { suspensionDetails =>
            if (suspensionDetails.isRegimeSuspended(service)) {
              goto(suspendedState)
            } else {
              goto(identifyClientState)
            }
          }
        case (true, false) => goto(identifyClientState)
        case (false, _)    => fail(new Exception(s"Service: $service feature flag is switched off"))
      }

    def selectedPersonalService(
      showItsaFlag: Boolean,
      showPirFlag: Boolean,
      showVatFlag: Boolean,
      showCgtFlag: Boolean,
      showPptFlag: Boolean,
      agentSuspensionEnabled: Boolean,
      getSuspensionDetails: GetSuspensionDetails)(agent: AuthorisedAgent)(service: String) = Transition {

      case SelectPersonalService(services, basket) =>
        if (service.isEmpty) { // user selected "no" to final service
          goto(ReviewAuthorisations(Personal, services, basket))
        } else if (services.contains(service)) {
          val flag = service match {
            case HMRCMTDIT  => showItsaFlag
            case HMRCPIR    => showPirFlag
            case HMRCMTDVAT => showVatFlag
            case HMRCCGTPD  => showCgtFlag
            case HMRCPPTORG => showPptFlag
          }
          gotoIdentify(
            flag,
            agentSuspensionEnabled,
            getSuspensionDetails,
            agent.arn,
            service,
            IdentifyClient(Personal, service, basket),
            AgentSuspended(service, basket))
        } else goto(SelectPersonalService(services, basket))
    }

    def selectedBusinessService(
      showVatFlag: Boolean,
      showPptFlag: Boolean,
      agentSuspensionEnabled: Boolean,
      getSuspensionDetails: GetSuspensionDetails)(agent: AuthorisedAgent)(service: String) = Transition {
      case SelectBusinessService(services, basket) =>
        if (service.isEmpty) { // user selected "no" to final service
          if (basket.isEmpty)
            goto(SelectClientType(basket)) // if no services in basket, and user also declined the final service, go back to SelectClientType
          else goto(ReviewAuthorisations(Business, services, basket)) // otherwise proceed to review
        } else if (services.contains(service)) {
          val flag = service match {
            case HMRCMTDVAT => showVatFlag
            case HMRCPPTORG => showPptFlag
          }
          gotoIdentify(
            flag,
            agentSuspensionEnabled,
            getSuspensionDetails,
            agent.arn,
            service,
            IdentifyClient(Business, service, basket),
            AgentSuspended(service, basket))
        } else goto(SelectBusinessService(services, basket))
    }

    def selectedTrustService(
      showTrustsFlag: Boolean,
      showCgtFlag: Boolean,
      showPptFlag: Boolean,
      agentSuspensionEnabled: Boolean,
      getSuspensionDetails: GetSuspensionDetails)(agent: AuthorisedAgent)(service: String) =
      Transition {
        case SelectTrustService(services, basket) =>
          if (service.isEmpty) { // user selected "no" to final service
            if (basket.nonEmpty)
              goto(ReviewAuthorisations(Trust, services, basket))
            else
              goto(root)

          } else if (services.contains(service)) {
            val flag = service match {
              case TRUST      => showTrustsFlag
              case HMRCCGTPD  => showCgtFlag
              case HMRCPPTORG => showPptFlag
            }
            gotoIdentify(
              flag,
              agentSuspensionEnabled,
              getSuspensionDetails,
              agent.arn,
              service,
              IdentifyClient(Trust, service, basket),
              AgentSuspended(service, basket))
          } else goto(SelectTrustService(services, basket))
      }

    def identifiedTrustClient(getTrustName: GetTrustName)(agent: AuthorisedAgent)(trustClient: TrustClient) =
      Transition {
        case IdentifyClient(Trust, TRUST, basket) =>
          getTrustName(trustClient.taxId).flatMap { trustResponse =>
            trustResponse.response match {
              case Right(TrustName(name)) => {
                trustClient.taxId match {
                  case Utr(_) =>
                    goto(
                      ConfirmClientTrust(AuthorisationRequest(name, TrustInvitation(Utr(trustClient.taxId.value))), basket)
                    )
                  case Urn(_) =>
                    goto(
                      ConfirmClientTrust(AuthorisationRequest(name, TrustNTInvitation(Urn(trustClient.taxId.value))), basket)
                    )
                }

              }
              case Left(invalidTrust) =>
                logger.warn(s"Des returned $invalidTrust response for utr: ${trustClient.taxId}")
                goto(TrustNotFound(basket))
            }
          }
      }

    def identifyCgtClient(getCgtSubscription: GetCgtSubscription)(agent: AuthorisedAgent)(
      cgtClient: CgtClient): AgentInvitationJourneyModel.Transition = {
      def handle(showPostcode: CgtSubscription => State, showCountryCode: CgtSubscription => State, basket: Basket) =
        getCgtSubscription(cgtClient.cgtRef).map {
          case Some(subscription) =>
            if (subscription.isUKBasedClient) {
              showPostcode(subscription)
            } else {
              showCountryCode(subscription)
            }
          case None =>
            CgtRefNotFound(cgtClient.cgtRef, basket)
        }

      Transition {
        case IdentifyClient(Trust, HMRCCGTPD, basket) =>
          handle(
            cgtSubscription => ConfirmPostcodeCgt(cgtClient.cgtRef, Trust, basket, cgtSubscription.postCode, cgtSubscription.name),
            cgtSubscription => ConfirmCountryCodeCgt(cgtClient.cgtRef, Trust, basket, cgtSubscription.countryCode, cgtSubscription.name),
            basket
          )

        case IdentifyClient(Personal, HMRCCGTPD, basket) =>
          handle(
            cgtSubscription => ConfirmPostcodeCgt(cgtClient.cgtRef, Personal, basket, cgtSubscription.postCode, cgtSubscription.name),
            cgtSubscription => ConfirmCountryCodeCgt(cgtClient.cgtRef, Personal, basket, cgtSubscription.countryCode, cgtSubscription.name),
            basket
          )
      }
    }

    def identifyPptClient(checkKnownFact: PptClient => Future[Boolean], getPptCustomerName: PptRef => Future[Option[String]])(agent: AuthorisedAgent)(
      pptClient: PptClient): AgentInvitationJourneyModel.Transition = {
      def handle(mkState: String => State, basket: Basket) =
        for {
          isCheckOK     <- checkKnownFact(pptClient)
          mCustomerName <- if (isCheckOK) getPptCustomerName(pptClient.pptRef) else Future.successful(None)
        } yield {
          mCustomerName match {
            case Some(customerName) => mkState(customerName)
            case None               => PptRefNotFound(pptClient.pptRef, basket)
          }
        }

      Transition {
        case IdentifyClient(Personal, HMRCPPTORG, basket) =>
          handle(
            customerName => ConfirmClientPpt(AuthorisationRequest(customerName, PptInvitation(pptClient.pptRef, Some(Personal))), basket),
            basket
          )
        case IdentifyClient(Business, HMRCPPTORG, basket) =>
          handle(
            customerName => ConfirmClientPpt(AuthorisationRequest(customerName, PptInvitation(pptClient.pptRef, Some(Business))), basket),
            basket
          )
        case IdentifyClient(Trust, HMRCPPTORG, basket) =>
          handle(
            customerName => ConfirmClientPpt(AuthorisationRequest(customerName, PptInvitation(pptClient.pptRef, Some(Trust))), basket),
            basket
          )
      }
    }

    private def removeSpaceFromPostcode(postcode: String): String =
      postcode.replace(" ", "")

    def confirmPostcodeCgt(getCgtSubscription: GetCgtSubscription)(agent: AuthorisedAgent)(postcode: Postcode): Transition =
      Transition {
        case ConfirmPostcodeCgt(cgtRef, clientType, basket, postcodeFromDes, name) => {
          val userPostcodeWithoutSpace = removeSpaceFromPostcode(postcode.value)
          val desPostcodeWithoutSpace = removeSpaceFromPostcode(postcodeFromDes.getOrElse("no_des_postcode"))

          if (desPostcodeWithoutSpace == userPostcodeWithoutSpace) {
            goto(ConfirmClientCgt(AuthorisationRequest(name, CgtInvitation(cgtRef, Some(clientType))), basket))
          } else {
            logger.warn(s"CGT postcode match failed. DES postcode was ${postcodeFromDes
              .getOrElse("not found")} and user entered ${postcode.value}")
            goto(KnownFactNotMatched(basket))
          }
        }
      }

    def confirmCountryCodeCgt(getCgtSubscription: GetCgtSubscription)(agent: AuthorisedAgent)(countryCode: CountryCode): Transition =
      Transition {
        case ConfirmCountryCodeCgt(cgtRef, clientType, basket, countryCodeFromDes, name) =>
          if (countryCodeFromDes.contains(countryCode.value)) {
            goto(ConfirmClientCgt(AuthorisationRequest(name, CgtInvitation(cgtRef, Some(clientType))), basket))
          } else {
            goto(KnownFactNotMatched(basket))
          }
      }

    // format: off
    def identifiedItsaClient(checkPostcodeMatches: CheckPostcodeMatches)
                            (hasPendingInvitationsFor: HasPendingInvitations)
                            (hasActiveRelationshipFor: HasActiveRelationship)
                            (getClientName: GetClientName)
                            (createMultipleInvitations: CreateMultipleInvitations)
                            (getAgentLink: GetAgentLink)
                            (getAgencyEmail: GetAgencyEmail)
                            (appConfig: AppConfig)
                            (agent: AuthorisedAgent)
                            (itsaClient: ItsaClient) = Transition {
      // format: on
      case IdentifyClient(Personal, HMRCMTDIT, basket) =>
        for {
          postcodeMatches <- checkPostcodeMatches(Nino(itsaClient.clientIdentifier), itsaClient.postcode)
          endState <- postcodeMatches match {
                       case Some(true) =>
                         getClientName(itsaClient.clientIdentifier, HMRCMTDIT).flatMap { clientName =>
                           goto(
                             ConfirmClientItsa(
                               AuthorisationRequest(clientName.getOrElse(""), ItsaInvitation(Nino(itsaClient.clientIdentifier))),
                               basket))
                         }
                       case Some(false) => goto(KnownFactNotMatched(basket))
                       case None        => if (appConfig.featuresAltItsa) goto(ClientNotRegistered(basket)) else goto(ClientNotSignedUp(HMRCMTDIT, basket))
                     }
        } yield endState
    }

    // format: off
    def identifiedVatClient(checkRegDateMatches: CheckRegDateMatches)
                           (hasPendingInvitationsFor: HasPendingInvitations)
                           (hasActiveRelationshipFor: HasActiveRelationship)
                           (getClientName: GetClientName)
                           (createMultipleInvitations: CreateMultipleInvitations)
                           (getAgentLink: GetAgentLink)
                           (getAgencyEmail: GetAgencyEmail)
                           (agent: AuthorisedAgent)
                           (vatClient: VatClient) = Transition {
      // format: on
      case IdentifyClient(Personal, HMRCMTDVAT, basket) =>
        for {
          regDateMatches <- checkRegDateMatches(Vrn(vatClient.clientIdentifier), LocalDate.parse(vatClient.registrationDate))
          endState <- regDateMatches match {
                       case kfcResponse @ (VatKnownFactCheckOk | VatRecordClientInsolvent) =>
                         getClientName(vatClient.clientIdentifier, HMRCMTDVAT).flatMap { clientName =>
                           goto(
                             ConfirmClientPersonalVat(
                               AuthorisationRequest(clientName.getOrElse(""), VatInvitation(Some(Personal), Vrn(vatClient.clientIdentifier))),
                               basket,
                               kfcResponse == VatRecordClientInsolvent
                             ))
                         }
                       case VatRecordMigrationInProgress => goto(CannotCreateRequest(basket))
                       case VatKnownFactNotMatched       => goto(KnownFactNotMatched(basket))
                       case VatDetailsNotFound           => goto(ClientNotSignedUp(HMRCMTDVAT, basket))
                     }
        } yield endState

      case IdentifyClient(Business, service, basket) =>
        for {
          regDateMatches <- checkRegDateMatches(Vrn(vatClient.clientIdentifier), LocalDate.parse(vatClient.registrationDate))
          endState <- regDateMatches match {
                       case kfcResponse @ (VatKnownFactCheckOk | VatRecordClientInsolvent) =>
                         getClientName(vatClient.clientIdentifier, HMRCMTDVAT).flatMap { clientName =>
                           goto(
                             ConfirmClientBusinessVat(
                               AuthorisationRequest(clientName.getOrElse(""), VatInvitation(Some(Business), Vrn(vatClient.clientIdentifier))),
                               basket,
                               kfcResponse == VatRecordClientInsolvent
                             )
                           )

                         }
                       case VatRecordMigrationInProgress => goto(CannotCreateRequest(basket))
                       case VatKnownFactNotMatched       => goto(KnownFactNotMatched(basket))
                       case VatDetailsNotFound           => goto(ClientNotSignedUp(HMRCMTDVAT, Set.empty))
                     }
        } yield endState
    }

    type CheckDOBMatches = (Nino, LocalDate) => Future[Option[Boolean]]

    // format: off
    def identifiedIrvClient(checkDobMatches: CheckDOBMatches)
                           (hasPendingInvitationsFor: HasPendingInvitations)
                           (hasActiveRelationshipFor: HasActiveRelationship)
                           (hasAltItsaInvitations: HasPartialAuthorisation)
                           (getClientName: GetClientName)
                           (createMultipleInvitations: CreateMultipleInvitations)
                           (getAgentLink: GetAgentLink)
                           (getAgencyEmail: GetAgencyEmail)
                           (legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)
                           (appConfig: AppConfig)
                           (agent: AuthorisedAgent)
                           (irvClient: IrvClient) = Transition {
      // format: on
      case IdentifyClient(Personal, HMRCPIR, basket) =>
        for {
          dobMatches <- checkDobMatches(Nino(irvClient.clientIdentifier), LocalDate.parse(irvClient.dob))

          endState <- dobMatches match {
                       case Some(true) =>
                         getClientName(irvClient.clientIdentifier, HMRCPIR)
                           .map { clientName =>
                             AuthorisationRequest(clientName.getOrElse(""), PirInvitation(Nino(irvClient.clientIdentifier)))
                           }
                           .flatMap { request =>
                             checkIfPendingOrActiveAndGoto(ReviewAuthorisations(Personal, agent.personalServices, basket + request))(
                               Personal,
                               agent.arn,
                               request,
                               HMRCPIR,
                               basket)(
                               hasPendingInvitationsFor,
                               hasActiveRelationshipFor,
                               hasAltItsaInvitations,
                               legacySaRelationshipStatusFor,
                               getAgentLink,
                               appConfig)
                           }
                       case Some(false) => goto(KnownFactNotMatched(basket))
                       case None        => goto(KnownFactNotMatched(basket))
                     }
        } yield endState
    }

    private def createAndProcessInvitations(
      agencyEmail: String,
      invitationLink: String,
      createInvitationSent: CreateInvitationSent,
      someFailedState: Basket => State,
      basket: Basket,
      createMultipleInvitations: CreateMultipleInvitations,
      arn: Arn) =
      for {
        processedRequests <- createMultipleInvitations(arn, basket)
        successState      <- createInvitationSent(agencyEmail, invitationLink, arn, basket)
        result <- if (AuthorisationRequest.eachHasBeenCreatedIn(processedRequests)) goto(successState)
                 else if (AuthorisationRequest.noneHaveBeenCreatedIn(processedRequests))
                   goto(AllAuthorisationsFailed(processedRequests))
                 else goto(someFailedState(processedRequests))
      } yield result

    private def checkIfPendingOrActiveAndGoto(
      successState: State)(clientType: ClientType, arn: Arn, request: AuthorisationRequest, service: String, basket: Basket)(
      hasPendingInvitationsFor: HasPendingInvitations,
      hasActiveRelationshipFor: HasActiveRelationship,
      hasPartialAuthorisationFor: HasPartialAuthorisation,
      legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor,
      getAgentLink: GetAgentLink,
      appConfig: AppConfig
    ): Future[State] =
      for {
        hasPendingInvitations <- if (basket.exists(_.invitation.service == service) &&
                                     basket.exists(_.invitation.clientId == request.invitation.clientId))
                                  Future.successful(true)
                                else
                                  hasPendingInvitationsFor(arn, request.invitation.clientId, service)
        result <- if (hasPendingInvitations) {
                   getAgentLink(arn, Some(clientType))
                     .flatMap(agentLink => goto(PendingInvitationExists(clientType, agentLink, basket)))
                 } else {
                   hasActiveRelationshipFor(arn, request.invitation.clientId, service).flatMap {
                     case true => goto(ActiveAuthorisationExists(clientType, service, basket))
                     case false =>
                       if (service == HMRCMTDIT) hasPartialAuthorisationFor(arn, request.invitation.clientId).flatMap {
                         case true =>
                           goto(PartialAuthorisationExists(basket))
                         case false =>
                           if (appConfig.featuresAltItsa) {
                             legacySaRelationshipStatusFor(arn, request.invitation.clientId).flatMap {
                               case LegacySaRelationshipFoundAndMapped => goto(AlreadyCopiedAcrossItsa)
                               case LegacySaRelationshipFoundNotMapped => goto(LegacyAuthorisationDetected(basket + request))
                               case LegacySaRelationshipNotFound       => goto(successState)
                             }
                           } else goto(successState)
                       } else goto(successState)
                   }
                 }
      } yield result

    /** User confirms that the client identified is the correct person.
      *  This transition may go to a review page (if there can be more than one item in the basket)
      *  Otherwise it will just go straight to creation of the invitation and the what's next page
      *
      * */
    // format: off
    def clientConfirmed(showCgtFlag: Boolean)
                       (createMultipleInvitations: CreateMultipleInvitations)
                       (getAgentLink: GetAgentLink)
                       (getAgencyEmail: GetAgencyEmail)
                       (hasPendingInvitationsFor: HasPendingInvitations)
                       (hasActiveRelationshipFor: HasActiveRelationship)
                       (hasPartialAuthorisationFor: HasPartialAuthorisation)
                       (legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor)
                       (appConfig: AppConfig)
                       (authorisedAgent: AuthorisedAgent)
                       (confirmation: Confirmation) =
    // format: on
    Transition {

      case ConfirmClientItsa(request, basket) =>
        if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(ReviewAuthorisations(Personal, authorisedAgent.personalServices, basket + request))(
            Personal,
            authorisedAgent.arn,
            request,
            HMRCMTDIT,
            basket)(
            hasPendingInvitationsFor,
            hasActiveRelationshipFor,
            hasPartialAuthorisationFor,
            legacySaRelationshipStatusFor,
            getAgentLink,
            appConfig)
        } else goto(IdentifyClient(Personal, HMRCMTDIT, basket))

      case ConfirmClientCgt(request, basket) => {
        val (reviewAuthState, state, clientType) = request.invitation.clientType match {
          case Some(Trust) =>
            (ReviewAuthorisations(Trust, authorisedAgent.trustServices, basket + request), IdentifyClient(Trust, HMRCCGTPD, basket), Trust)
          case Some(Personal) =>
            (
              ReviewAuthorisations(Personal, authorisedAgent.personalServices, basket + request),
              IdentifyClient(Personal, HMRCCGTPD, basket),
              Personal)
          case x => throw new RuntimeException("unexpected clientType in the AuthorisationRequest: " + x) //TODO
        }

        if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(reviewAuthState)(clientType, authorisedAgent.arn, request, HMRCCGTPD, basket)(
            hasPendingInvitationsFor,
            hasActiveRelationshipFor,
            hasPartialAuthorisationFor,
            legacySaRelationshipStatusFor,
            getAgentLink,
            appConfig)
        } else goto(state)
      }

      case ConfirmClientPersonalVat(request, basket, clientInsolvent) =>
        if (confirmation.choice) {
          if (clientInsolvent) goto(ClientInsolvent(basket))
          else
            checkIfPendingOrActiveAndGoto(ReviewAuthorisations(Personal, authorisedAgent.personalServices, basket + request))(
              Personal,
              authorisedAgent.arn,
              request,
              HMRCMTDVAT,
              basket)(
              hasPendingInvitationsFor,
              hasActiveRelationshipFor,
              hasPartialAuthorisationFor,
              legacySaRelationshipStatusFor,
              getAgentLink,
              appConfig)
        } else goto(IdentifyClient(Personal, HMRCMTDVAT, basket))

      case ConfirmClientBusinessVat(request, basket, isInsolvent) =>
        if (isInsolvent) goto(ClientInsolvent(basket))
        else if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(ReviewAuthorisations(Business, authorisedAgent.businessServices, basket + request))(
            Business,
            authorisedAgent.arn,
            request,
            HMRCMTDVAT,
            basket)(
            hasPendingInvitationsFor,
            hasActiveRelationshipFor,
            hasPartialAuthorisationFor,
            legacySaRelationshipStatusFor,
            getAgentLink,
            appConfig)
        } else goto(IdentifyClient(Business, HMRCMTDVAT, basket))

      case ConfirmClientTrust(request, basket) =>
        if (confirmation.choice) {
          if (showCgtFlag)
            // if CGT is enabled, we need to go to the review page (since we are multi-select)
            checkIfPendingOrActiveAndGoto(ReviewAuthorisations(Trust, authorisedAgent.trustServices, basket + request))(
              Trust,
              authorisedAgent.arn,
              request,
              request.invitation.service,
              basket)(
              hasPendingInvitationsFor,
              hasActiveRelationshipFor,
              hasPartialAuthorisationFor,
              legacySaRelationshipStatusFor,
              getAgentLink,
              appConfig)
          else
            // otherwise we go straight to create the invitation (no review necessary - only one service)
            for {
              hasPendingInvitations <- hasPendingInvitationsFor(authorisedAgent.arn, request.invitation.clientId, request.invitation.service)
              agentLink             <- getAgentLink(authorisedAgent.arn, Some(Trust))
              result <- if (hasPendingInvitations) {
                         goto(PendingInvitationExists(Trust, agentLink, Set.empty))
                       } else {
                         hasActiveRelationshipFor(authorisedAgent.arn, request.invitation.clientId, request.invitation.service)
                           .flatMap {
                             case true => goto(ActiveAuthorisationExists(Trust, request.invitation.service, Set.empty))
                             case false =>
                               getAgencyEmail().flatMap(
                                 agencyEmail =>
                                   createAndProcessInvitations(
                                     agentLink,
                                     agencyEmail,
                                     (_, _, _, _) => toFuture(InvitationSentTrust(agentLink, None, agencyEmail, Set(request.invitation.service))),
                                     (b: Basket) => SomeAuthorisationsFailed(agentLink, None, agencyEmail, b),
                                     Set(request),
                                     createMultipleInvitations,
                                     authorisedAgent.arn
                                 ))
                           }
                       }
            } yield result
        } else goto(IdentifyClient(Trust, TRUST, basket))

      case ConfirmClientPpt(request, basket) => {
        val (reviewAuthState, state, clientType) = request.invitation.clientType match {
          case Some(Trust) =>
            (ReviewAuthorisations(Trust, authorisedAgent.trustServices, basket + request), IdentifyClient(Trust, HMRCPPTORG, basket), Trust)
          case Some(Personal) =>
            (
              ReviewAuthorisations(Personal, authorisedAgent.personalServices, basket + request),
              IdentifyClient(Personal, HMRCPPTORG, basket),
              Personal)
          case Some(Business) =>
            (
              ReviewAuthorisations(Business, authorisedAgent.businessServices, basket + request),
              IdentifyClient(Business, HMRCPPTORG, basket),
              Business)
          case None => throw new RuntimeException("unexpected clientType in the AuthorisationRequest")
        }

        if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(reviewAuthState)(clientType, authorisedAgent.arn, request, HMRCPPTORG, basket)(
            hasPendingInvitationsFor,
            hasActiveRelationshipFor,
            hasPartialAuthorisationFor,
            legacySaRelationshipStatusFor,
            getAgentLink,
            appConfig)
        } else goto(state)
      }

    }

    /** User confirms that they have legacy authorisation with the client.
      *  This transition may go to agent-mapping
      *  Or review authorisations as if from confirm client
      * */
    def confirmedLegacyAuthorisation(authorisedAgent: AuthorisedAgent): Transition =
      Transition {
        case LegacyAuthorisationDetected(basket) =>
          goto(ReviewAuthorisations(Personal, authorisedAgent.personalServices, basket))
      }

    def continueSomeResponsesFailed(agent: AuthorisedAgent) = Transition {
      case SomeAuthorisationsFailed(invitationLink, continueUrl, agencyEmail, basket) =>
        val services = basket.filter(_.state == AuthorisationRequest.CREATED).map(_.invitation.service)
        goto(InvitationSentPersonal(invitationLink, continueUrl, agencyEmail, services, isAltItsa = false))
    }

    // format: off
    def authorisationsReviewed(createMultipleInvitations: CreateMultipleInvitations)
                              (getAgentLink: GetAgentLink)
                              (getAgencyEmail: GetAgencyEmail)
                              (createInvitationSent: CreateInvitationSent)
                              (agent: AuthorisedAgent)
                              (confirmation: Confirmation) =
      // format: on
    Transition {
      case ReviewAuthorisations(Trust, _, basket) =>
        if (confirmation.choice)
          goto(SelectTrustService(agent.trustServices, basket))
        else {
          for {
            agencyEmail    <- getAgencyEmail()
            invitationLink <- getAgentLink(agent.arn, Some(Trust))
            services = basket.map(_.invitation.service)
            result <- createAndProcessInvitations(
                       agencyEmail,
                       invitationLink,
                       (_, _, _, _) => toFuture(InvitationSentTrust(invitationLink, None, agencyEmail, services)),
                       (b: Basket) => SomeAuthorisationsFailed(invitationLink, None, agencyEmail, b),
                       basket,
                       createMultipleInvitations,
                       agent.arn
                     )
          } yield result
        }

      case ReviewAuthorisations(Personal, _, basket) =>
        if (confirmation.choice) {
          goto(SelectPersonalService(agent.personalServices, basket))
        } else {
          for {
            agencyEmail    <- getAgencyEmail()
            invitationLink <- getAgentLink(agent.arn, Some(Personal))
            result <- createAndProcessInvitations(
                       agencyEmail,
                       invitationLink,
                       createInvitationSent,
                       (b: Basket) => SomeAuthorisationsFailed(invitationLink, None, agencyEmail, b),
                       basket,
                       createMultipleInvitations,
                       agent.arn
                     )
          } yield result
        }

      case ReviewAuthorisations(business, _, basket) =>
        if (confirmation.choice) {
          goto(SelectBusinessService(agent.businessServices, basket))
        } else {
          for {
            agencyEmail    <- getAgencyEmail()
            invitationLink <- getAgentLink(agent.arn, Some(business))
            result <- createAndProcessInvitations(
                       agencyEmail,
                       invitationLink,
                       createInvitationSent,
                       (b: Basket) => SomeAuthorisationsFailed(invitationLink, None, agencyEmail, b),
                       basket,
                       createMultipleInvitations,
                       agent.arn
                     )
          } yield result
        }
    }

    def deleteAuthorisationRequest(itemId: String)(authorisedAgent: AuthorisedAgent) = {
      def findItem(basket: Basket): AuthorisationRequest =
        basket.find(_.itemId == itemId).getOrElse(throw new Exception("No Item to delete"))
      Transition {
        case ReviewAuthorisations(clientType, _, basket) =>
          goto(DeleteAuthorisationRequest(clientType, findItem(basket), basket))
      }
    }

    def confirmDeleteAuthorisationRequest(authorisedAgent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {

        case DeleteAuthorisationRequest(Personal, authorisationRequest, basket) =>
          if (confirmation.choice) {
            if ((basket - authorisationRequest).nonEmpty)
              goto(ReviewAuthorisations(Personal, authorisedAgent.personalServices, basket - authorisationRequest))
            else
              goto(AllAuthorisationsRemoved)
          } else {
            goto(ReviewAuthorisations(Personal, authorisedAgent.personalServices, basket))
          }

        case DeleteAuthorisationRequest(Business, authorisationRequest, basket) =>
          if (confirmation.choice) {
            if ((basket - authorisationRequest).nonEmpty)
              goto(ReviewAuthorisations(Business, authorisedAgent.businessServices, basket - authorisationRequest))
            else
              goto(AllAuthorisationsRemoved)
          } else {
            goto(ReviewAuthorisations(Business, authorisedAgent.businessServices, basket))
          }

        case DeleteAuthorisationRequest(Trust, authorisationRequest, basket) =>
          if (confirmation.choice) {
            if ((basket - authorisationRequest).nonEmpty)
              goto(ReviewAuthorisations(Trust, authorisedAgent.trustServices, basket - authorisationRequest))
            else
              goto(AllAuthorisationsRemoved)
          } else {
            goto(ReviewAuthorisations(Trust, authorisedAgent.trustServices, basket))
          }
      }
  }
}
