/*
 * Copyright 2020 HM Revenue & Customs
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
import uk.gov.hmrc.agentinvitationsfrontend.connectors.SuspensionResponse
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR, _}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, CgtRef, Utr, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.fsm.JourneyModel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object AgentInvitationJourneyModel extends JourneyModel {

  sealed trait State

  val root: State = State.SelectClientType(Set.empty)

  type Basket = Set[AuthorisationRequest]

  /* State should contain only minimal set of data required to proceed */
  object State {

    case class SelectClientType(basket: Basket) extends State

    case class SelectPersonalService(services: Set[String], basket: Basket) extends State
    case object SelectBusinessService extends State
    case class SelectTrustService(services: Set[String], basket: Basket) extends State

    case class IdentifyPersonalClient(service: String, basket: Basket) extends State
    case object IdentifyBusinessClient extends State
    case class IdentifyTrustClient(service: String, basket: Basket) extends State

    case class PendingInvitationExists(clientType: ClientType, basket: Basket) extends State
    case class ActiveAuthorisationExists(clientType: ClientType, service: String, basket: Basket) extends State
    case class KnownFactNotMatched(basket: Basket) extends State
    case class CannotCreateRequest(basket: Basket) extends State
    case class TrustNotFound(basket: Basket) extends State
    case class CgtRefNotFound(cgtRef: CgtRef, basket: Basket) extends State

    case class ConfirmClientItsa(request: AuthorisationRequest, basket: Basket) extends State
    case class ConfirmClientPersonalVat(request: AuthorisationRequest, basket: Basket) extends State
    case class ConfirmClientBusinessVat(request: AuthorisationRequest) extends State
    case class ConfirmClientTrust(request: AuthorisationRequest, basket: Basket) extends State
    case class ConfirmClientCgt(request: AuthorisationRequest, basket: Basket) extends State
    case class ConfirmPostcodeCgt(
      cgtRef: CgtRef,
      clientType: ClientType,
      basket: Basket,
      postcode: Option[String],
      clientName: String)
        extends State
    case class ConfirmCountryCodeCgt(
      cgtRef: CgtRef,
      clientType: ClientType,
      basket: Basket,
      countryCode: String,
      clientName: String)
        extends State

    case class ReviewAuthorisationsPersonal(services: Set[String], basket: Basket) extends State
    case class ReviewAuthorisationsTrust(services: Set[String], basket: Basket) extends State

    case class SomeAuthorisationsFailed(
      invitationLink: String,
      continueUrl: Option[String],
      agencyEmail: String,
      basket: Basket)
        extends State
    case class AllAuthorisationsFailed(basket: Basket) extends State
    case class DeleteAuthorisationRequestTrust(authorisationRequest: AuthorisationRequest, basket: Basket) extends State
    case class DeleteAuthorisationRequestPersonal(authorisationRequest: AuthorisationRequest, basket: Basket)
        extends State
    case class InvitationSentPersonal(invitationLink: String, continueUrl: Option[String], agencyEmail: String)
        extends State
    case class InvitationSentBusiness(
      invitationLink: String,
      continueUrl: Option[String],
      agencyEmail: String,
      service: String = HMRCMTDVAT)
        extends State
    case class ClientNotSignedUp(service: String, basket: Basket) extends State
    case object AllAuthorisationsRemoved extends State
    case class AgentSuspended(suspendedService: String, basket: Basket) extends State
  }

  object Transitions {
    import State._

    val start: AgentInvitationJourneyModel.Transition = AgentInvitationJourneyModel.start

    type HasPendingInvitations = (Arn, String, String) => Future[Boolean]
    type HasActiveRelationship = (Arn, String, String) => Future[Boolean]
    type GetClientName = (String, String) => Future[Option[String]]
    type CheckPostcodeMatches = (Nino, String) => Future[Option[Boolean]]
    type CheckRegDateMatches = (Vrn, LocalDate) => Future[Option[Int]]
    type CreateMultipleInvitations =
      (Arn, Set[AuthorisationRequest]) => Future[Set[AuthorisationRequest]]
    type GetAgentLink = (Arn, Option[ClientType]) => Future[String]
    type GetAgencyEmail = () => Future[String]
    type GetTrustName = Utr => Future[TrustResponse]
    type GetCgtSubscription = CgtRef => Future[Option[CgtSubscription]]
    type GetSuspensionStatus = Arn => Future[SuspensionResponse]

    def selectedClientType(agent: AuthorisedAgent)(clientType: String) = Transition {
      case SelectClientType(basket) =>
        clientType match {
          case "personal" => goto(SelectPersonalService(agent.personalServices, basket))
          case "business" => goto(SelectBusinessService)
          case "trust"    => goto(SelectTrustService(agent.trustServices, basket))
        }
    }

    def gotoIdentify(
      serviceEnabled: Boolean,
      agentSuspensionEnabled: Boolean,
      getSuspensionStatus: GetSuspensionStatus,
      arn: Arn,
      service: String,
      identifyClientState: State,
      suspendedState: State): Future[State] =
      (serviceEnabled, agentSuspensionEnabled) match {
        case (true, true) =>
          getSuspensionStatus(arn).flatMap { suspendedServices =>
            if (suspendedServices.isSuspendedService(service)) goto(suspendedState) else goto(identifyClientState)
          }
        case (true, false) => goto(identifyClientState)
        case (false, _)    => fail(new Exception(s"Service: $service feature flag is switched off"))
      }

    def selectedPersonalService(
      showItsaFlag: Boolean,
      showPirFlag: Boolean,
      showVatFlag: Boolean,
      showCgtFlag: Boolean,
      agentSuspensionEnabled: Boolean,
      getSuspensionStatus: GetSuspensionStatus)(agent: AuthorisedAgent)(service: String) = Transition {

      case SelectPersonalService(services, basket) =>
        if (service.isEmpty) { // user selected "no" to final service
          goto(ReviewAuthorisationsPersonal(services, basket))
        } else if (services.contains(service)) {
          val flag = service match {
            case HMRCMTDIT  => showItsaFlag
            case HMRCPIR    => showPirFlag
            case HMRCMTDVAT => showVatFlag
            case HMRCCGTPD  => showCgtFlag
          }
          gotoIdentify(
            flag,
            agentSuspensionEnabled,
            getSuspensionStatus,
            agent.arn,
            service,
            IdentifyPersonalClient(service, basket),
            AgentSuspended(service, basket))
        } else goto(SelectPersonalService(services, basket))
    }

    def selectedBusinessService(
      showVatFlag: Boolean,
      agentSuspensionEnabled: Boolean,
      getSuspensionStatus: GetSuspensionStatus)(agent: AuthorisedAgent)(service: String) = Transition {
      case SelectBusinessService =>
        if (service.nonEmpty) {
          gotoIdentify(
            showVatFlag,
            agentSuspensionEnabled,
            getSuspensionStatus,
            agent.arn,
            HMRCMTDVAT,
            IdentifyBusinessClient,
            AgentSuspended(service, Set.empty))
        } else {
          goto(root)
        }
    }

    def selectedTrustService(
      showTrustsFlag: Boolean,
      showCgtFlag: Boolean,
      agentSuspensionEnabled: Boolean,
      getSuspensionStatus: GetSuspensionStatus)(agent: AuthorisedAgent)(service: String) =
      Transition {
        case SelectTrustService(services, basket) =>
          if (service.isEmpty) { // user selected "no" to final service
            if (basket.nonEmpty)
              goto(ReviewAuthorisationsTrust(services, basket))
            else
              goto(root)

          } else if (services.contains(service)) {
            val flag = service match {
              case TRUST     => showTrustsFlag
              case HMRCCGTPD => showCgtFlag
            }
            gotoIdentify(
              flag,
              agentSuspensionEnabled,
              getSuspensionStatus,
              agent.arn,
              service,
              IdentifyTrustClient(service, basket),
              AgentSuspended(service, basket))
          } else goto(SelectTrustService(services, basket))
      }

    def identifiedTrustClient(getTrustName: GetTrustName)(agent: AuthorisedAgent)(trustClient: TrustClient) =
      Transition {
        case IdentifyTrustClient(TRUST, basket) =>
          getTrustName(trustClient.utr).flatMap { trustResponse =>
            trustResponse.response match {
              case Right(TrustName(name)) =>
                goto(
                  ConfirmClientTrust(AuthorisationRequest(name, TrustInvitation(trustClient.utr)), basket)
                )
              case Left(invalidTrust) =>
                Logger.warn(s"Des returned $invalidTrust response for utr: ${trustClient.utr}")
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
        case IdentifyTrustClient(HMRCCGTPD, basket) =>
          handle(
            cgtSubscription =>
              ConfirmPostcodeCgt(cgtClient.cgtRef, business, basket, cgtSubscription.postCode, cgtSubscription.name),
            cgtSubscription =>
              ConfirmCountryCodeCgt(
                cgtClient.cgtRef,
                business,
                basket,
                cgtSubscription.countryCode,
                cgtSubscription.name),
            basket
          )

        case IdentifyPersonalClient(HMRCCGTPD, basket) =>
          handle(
            cgtSubscription =>
              ConfirmPostcodeCgt(cgtClient.cgtRef, personal, basket, cgtSubscription.postCode, cgtSubscription.name),
            cgtSubscription =>
              ConfirmCountryCodeCgt(
                cgtClient.cgtRef,
                personal,
                basket,
                cgtSubscription.countryCode,
                cgtSubscription.name),
            basket
          )
      }
    }

    def confirmPostcodeCgt(getCgtSubscription: GetCgtSubscription)(agent: AuthorisedAgent)(
      postcode: Postcode): Transition =
      Transition {
        case ConfirmPostcodeCgt(cgtRef, clientType, basket, postcodeFromDes, name) =>
          if (postcodeFromDes.contains(postcode.value)) {
            goto(ConfirmClientCgt(AuthorisationRequest(name, CgtInvitation(cgtRef, Some(clientType))), basket))
          } else {
            goto(KnownFactNotMatched(basket))
          }
      }

    def confirmCountryCodeCgt(getCgtSubscription: GetCgtSubscription)(agent: AuthorisedAgent)(
      countryCode: CountryCode): Transition =
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
                            (agent: AuthorisedAgent)
                            (itsaClient: ItsaClient) = Transition {
      // format: on
      case IdentifyPersonalClient(HMRCMTDIT, basket) =>
        for {
          postcodeMatches <- checkPostcodeMatches(Nino(itsaClient.clientIdentifier), itsaClient.postcode)
          endState <- postcodeMatches match {
                       case Some(true) =>
                         getClientName(itsaClient.clientIdentifier, HMRCMTDIT).flatMap { clientName =>
                           goto(
                             ConfirmClientItsa(
                               AuthorisationRequest(
                                 clientName.getOrElse(""),
                                 ItsaInvitation(Nino(itsaClient.clientIdentifier))),
                               basket))
                         }
                       case Some(false) => goto(KnownFactNotMatched(basket))
                       case None        => goto(ClientNotSignedUp(HMRCMTDIT, basket))
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
      case IdentifyPersonalClient(HMRCMTDVAT, basket) =>
        for {
          regDateMatches <- checkRegDateMatches(
                             Vrn(vatClient.clientIdentifier),
                             LocalDate.parse(vatClient.registrationDate))
          endState <- regDateMatches match {
                       case Some(204) =>
                         getClientName(vatClient.clientIdentifier, HMRCMTDVAT).flatMap { clientName =>
                           goto(
                             ConfirmClientPersonalVat(
                               AuthorisationRequest(
                                 clientName.getOrElse(""),
                                 VatInvitation(Some(personal), Vrn(vatClient.clientIdentifier))),
                               basket
                             ))
                         }
                       case Some(423) => goto(CannotCreateRequest(basket))
                       case Some(_)   => goto(KnownFactNotMatched(basket))
                       case None      => goto(ClientNotSignedUp(HMRCMTDVAT, basket))
                     }
        } yield endState

      case IdentifyBusinessClient =>
        for {
          regDateMatches <- checkRegDateMatches(
                             Vrn(vatClient.clientIdentifier),
                             LocalDate.parse(vatClient.registrationDate))
          endState <- regDateMatches match {
                       case Some(204) =>
                         getClientName(vatClient.clientIdentifier, HMRCMTDVAT).flatMap { clientName =>
                           goto(
                             ConfirmClientBusinessVat(
                               AuthorisationRequest(
                                 clientName.getOrElse(""),
                                 VatInvitation(Some(business), Vrn(vatClient.clientIdentifier)))))

                         }
                       case Some(423) => goto(CannotCreateRequest(Set.empty))
                       case Some(_)   => goto(KnownFactNotMatched(Set.empty))
                       case None      => goto(ClientNotSignedUp(HMRCMTDVAT, Set.empty))
                     }
        } yield endState
    }

    type CheckDOBMatches = (Nino, LocalDate) => Future[Option[Boolean]]

    // format: off
    def identifiedIrvClient(checkDobMatches: CheckDOBMatches)
                           (hasPendingInvitationsFor: HasPendingInvitations)
                           (hasActiveRelationshipFor: HasActiveRelationship)
                           (getClientName: GetClientName)
                           (createMultipleInvitations: CreateMultipleInvitations)
                           (getAgentLink: GetAgentLink)
                           (getAgencyEmail: GetAgencyEmail)
                           (agent: AuthorisedAgent)
                           (irvClient: IrvClient) = Transition {
      // format: on
      case IdentifyPersonalClient(HMRCPIR, basket) =>
        for {
          dobMatches <- checkDobMatches(Nino(irvClient.clientIdentifier), LocalDate.parse(irvClient.dob))

          endState <- dobMatches match {
                       case Some(true) =>
                         getClientName(irvClient.clientIdentifier, HMRCPIR)
                           .map { clientName =>
                             AuthorisationRequest(
                               clientName.getOrElse(""),
                               PirInvitation(Nino(irvClient.clientIdentifier)))
                           }
                           .flatMap { request =>
                             checkIfPendingOrActiveAndGoto(
                               ReviewAuthorisationsPersonal(agent.personalServices, basket + request))(
                               personal,
                               agent.arn,
                               request.invitation.clientId,
                               HMRCPIR,
                               basket)(hasPendingInvitationsFor, hasActiveRelationshipFor)
                           }
                       case Some(false) => goto(KnownFactNotMatched(basket))
                       case None        => goto(KnownFactNotMatched(basket))
                     }
        } yield endState
    }

    private def createAndProcessInvitations(
      successState: State,
      someFailedState: Basket => State,
      basket: Basket,
      createMultipleInvitations: CreateMultipleInvitations,
      arn: Arn) =
      for {
        processedRequests <- createMultipleInvitations(arn, basket)
        result <- if (AuthorisationRequest.eachHasBeenCreatedIn(processedRequests)) goto(successState)
                 else if (AuthorisationRequest.noneHaveBeenCreatedIn(processedRequests))
                   goto(AllAuthorisationsFailed(processedRequests))
                 else goto(someFailedState(processedRequests))
      } yield result

    private def checkIfPendingOrActiveAndGoto(successState: State)(
      clientType: ClientType,
      arn: Arn,
      clientIdentifier: String,
      service: String,
      basket: Basket = Set.empty)(
      hasPendingInvitationsFor: HasPendingInvitations,
      hasActiveRelationshipFor: HasActiveRelationship): Future[State] =
      for {
        hasPendingInvitations <- if (basket.exists(_.invitation.service == service) &&
                                     basket.exists(_.invitation.clientId == clientIdentifier))
                                  Future.successful(true)
                                else
                                  hasPendingInvitationsFor(arn, clientIdentifier, service)
        result <- if (hasPendingInvitations) {
                   goto(PendingInvitationExists(clientType, basket))
                 } else {
                   hasActiveRelationshipFor(arn, clientIdentifier, service).flatMap {
                     case true  => goto(ActiveAuthorisationExists(clientType, service, basket))
                     case false => goto(successState)
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
                       (authorisedAgent: AuthorisedAgent)
                       (confirmation: Confirmation) =
    // format: on
    Transition {

      case ConfirmClientItsa(request, basket) =>
        if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(
            ReviewAuthorisationsPersonal(authorisedAgent.personalServices, basket + request))(
            personal,
            authorisedAgent.arn,
            request.invitation.clientId,
            HMRCMTDIT,
            basket)(hasPendingInvitationsFor, hasActiveRelationshipFor)
        } else goto(IdentifyPersonalClient(HMRCMTDIT, basket))

      case ConfirmClientCgt(request, basket) => {
        val (reviewAuthState, state, clientType) = request.invitation.clientType match {
          case Some(`business`) =>
            (
              ReviewAuthorisationsTrust(authorisedAgent.trustServices, basket + request),
              IdentifyTrustClient(HMRCCGTPD, basket),
              business)
          case Some(`personal`) =>
            (
              ReviewAuthorisationsPersonal(authorisedAgent.personalServices, basket + request),
              IdentifyPersonalClient(HMRCCGTPD, basket),
              personal)
          case None => throw new RuntimeException("unexpected clientType in the AuthorisationRequest") //TODO
        }

        if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(reviewAuthState)(
            clientType,
            authorisedAgent.arn,
            request.invitation.clientId,
            HMRCCGTPD,
            basket)(hasPendingInvitationsFor, hasActiveRelationshipFor)
        } else goto(state)
      }

      case ConfirmClientPersonalVat(request, basket) =>
        if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(
            ReviewAuthorisationsPersonal(authorisedAgent.personalServices, basket + request))(
            personal,
            authorisedAgent.arn,
            request.invitation.clientId,
            HMRCMTDVAT,
            basket)(hasPendingInvitationsFor, hasActiveRelationshipFor)
        } else goto(IdentifyPersonalClient(HMRCMTDVAT, basket))

      case ConfirmClientBusinessVat(request) =>
        if (confirmation.choice) {
          for {
            hasPendingInvitations <- hasPendingInvitationsFor(
                                      authorisedAgent.arn,
                                      request.invitation.clientId,
                                      HMRCMTDVAT)
            agentLink <- getAgentLink(authorisedAgent.arn, Some(business))
            result <- if (hasPendingInvitations) {
                       goto(PendingInvitationExists(business, Set.empty))
                     } else {
                       hasActiveRelationshipFor(authorisedAgent.arn, request.invitation.clientId, HMRCMTDVAT)
                         .flatMap {
                           case true => goto(ActiveAuthorisationExists(business, HMRCMTDVAT, Set.empty))
                           case false =>
                             getAgencyEmail().flatMap(
                               agencyEmail =>
                                 createAndProcessInvitations(
                                   InvitationSentBusiness(agentLink, None, agencyEmail),
                                   (b: Basket) => SomeAuthorisationsFailed(agentLink, None, agencyEmail, b),
                                   Set(request),
                                   createMultipleInvitations,
                                   authorisedAgent.arn
                               ))
                         }
                     }
          } yield result
        } else goto(IdentifyBusinessClient)

      case ConfirmClientTrust(request, basket) =>
        if (confirmation.choice) {
          if (showCgtFlag)
            // if CGT is enabled, we need to go to the review page (since we are multi-select)
            checkIfPendingOrActiveAndGoto(ReviewAuthorisationsTrust(authorisedAgent.trustServices, basket + request))(
              business,
              authorisedAgent.arn,
              request.invitation.clientId,
              TRUST,
              basket)(hasPendingInvitationsFor, hasActiveRelationshipFor)
          else
            // otherwise we go straight to create the invitation (no review necessary - only one service)
            for {
              hasPendingInvitations <- hasPendingInvitationsFor(authorisedAgent.arn, request.invitation.clientId, TRUST)
              agentLink             <- getAgentLink(authorisedAgent.arn, Some(business))
              result <- if (hasPendingInvitations) {
                         goto(PendingInvitationExists(business, Set.empty))
                       } else {
                         hasActiveRelationshipFor(authorisedAgent.arn, request.invitation.clientId, TRUST)
                           .flatMap {
                             case true => goto(ActiveAuthorisationExists(business, TRUST, Set.empty))
                             case false =>
                               getAgencyEmail().flatMap(
                                 agencyEmail =>
                                   createAndProcessInvitations(
                                     InvitationSentBusiness(agentLink, None, agencyEmail, TRUST),
                                     (b: Basket) => SomeAuthorisationsFailed(agentLink, None, agencyEmail, b),
                                     Set(request),
                                     createMultipleInvitations,
                                     authorisedAgent.arn
                                 ))
                           }
                       }
            } yield result
        } else goto(IdentifyTrustClient(TRUST, basket))
    }

    def continueSomeResponsesFailed(agent: AuthorisedAgent) = Transition {
      case SomeAuthorisationsFailed(invitationLink, continueUrl, agencyEmail, _) =>
        goto(InvitationSentPersonal(invitationLink, continueUrl, agencyEmail))
    }

    // format: off
    def authorisationsReviewed(createMultipleInvitations: CreateMultipleInvitations)
                              (getAgentLink: GetAgentLink)
                              (getAgencyEmail: GetAgencyEmail)
                              (agent: AuthorisedAgent)
                              (confirmation: Confirmation) =
      // format: on
    Transition {
      case ReviewAuthorisationsTrust(_, basket) =>
        if (confirmation.choice) {
          goto(SelectTrustService(agent.trustServices, basket))
        } else {
          for {
            agencyEmail    <- getAgencyEmail()
            invitationLink <- getAgentLink(agent.arn, Some(business))
            result <- createAndProcessInvitations(
                       InvitationSentBusiness(invitationLink, None, agencyEmail),
                       (b: Basket) => SomeAuthorisationsFailed(invitationLink, None, agencyEmail, b),
                       basket,
                       createMultipleInvitations,
                       agent.arn
                     )
          } yield result
        }

      case ReviewAuthorisationsPersonal(_, basket) =>
        if (confirmation.choice) {
          goto(SelectPersonalService(agent.personalServices, basket))
        } else {
          for {
            agencyEmail    <- getAgencyEmail()
            invitationLink <- getAgentLink(agent.arn, Some(personal))
            result <- createAndProcessInvitations(
                       InvitationSentPersonal(invitationLink, None, agencyEmail),
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
        case ReviewAuthorisationsPersonal(_, basket) =>
          goto(DeleteAuthorisationRequestPersonal(findItem(basket), basket))
        case ReviewAuthorisationsTrust(_, basket) =>
          goto(DeleteAuthorisationRequestPersonal(findItem(basket), basket))
      }
    }

    def confirmDeleteAuthorisationRequest(authorisedAgent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {

        case DeleteAuthorisationRequestPersonal(authorisationRequest, basket) =>
          if (confirmation.choice) {
            if ((basket - authorisationRequest).nonEmpty)
              goto(ReviewAuthorisationsPersonal(authorisedAgent.personalServices, basket - authorisationRequest))
            else
              goto(AllAuthorisationsRemoved)
          } else {
            goto(ReviewAuthorisationsPersonal(authorisedAgent.personalServices, basket))
          }

        case DeleteAuthorisationRequestTrust(authorisationRequest, basket) =>
          if (confirmation.choice) {
            if ((basket - authorisationRequest).nonEmpty)
              goto(ReviewAuthorisationsTrust(authorisedAgent.trustServices, basket - authorisationRequest))
            else
              goto(AllAuthorisationsRemoved)
          } else {
            goto(ReviewAuthorisationsTrust(authorisedAgent.trustServices, basket))
          }
      }
  }
}
