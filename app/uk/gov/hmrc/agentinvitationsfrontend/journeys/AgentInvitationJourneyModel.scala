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
    case object TrustNotFound extends State
    case object CgtRefNotFound extends State

    case class ConfirmClientItsa(request: AuthorisationRequest, basket: Basket) extends State
    case class ConfirmClientPersonalVat(request: AuthorisationRequest, basket: Basket) extends State
    case class ConfirmClientBusinessVat(request: AuthorisationRequest) extends State
    case class ConfirmClientTrust(request: AuthorisationRequest, basket: Basket) extends State
    case class ConfirmClientPersonalCgt(request: AuthorisationRequest, basket: Basket) extends State
    case class ConfirmClientTrustCgt(request: AuthorisationRequest, basket: Basket) extends State

    case class ReviewAuthorisationsPersonal(services: Set[String], basket: Basket) extends State
    case class ReviewAuthorisationsTrust(services: Set[String], basket: Basket) extends State

    case class SomeAuthorisationsFailed(
      invitationLink: String,
      continueUrl: Option[String],
      agencyEmail: String,
      basket: Basket)
        extends State
    case class AllAuthorisationsFailed(basket: Basket) extends State
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
    type GetCgtRefName = CgtRef => Future[String] // XXX placeholder until known fact check done

    def selectedClientType(agent: AuthorisedAgent)(clientType: String) = Transition {
      case SelectClientType(basket) =>
        clientType match {
          case "personal" => goto(SelectPersonalService(agent.personalServices, basket))
          case "business" => goto(SelectBusinessService)
          case "trust"    => goto(SelectTrustService(agent.trustServices, basket))
        }
    }

    def selectedPersonalService(
      showItsaFlag: Boolean,
      showPirFlag: Boolean,
      showVatFlag: Boolean,
      showCgtFlag: Boolean)(agent: AuthorisedAgent)(service: String) = Transition {

      case SelectPersonalService(services, basket) =>
        def gotoIdentify(serviceEnabled: Boolean, service: String): Future[State] =
          if (serviceEnabled)
            goto(IdentifyPersonalClient(service, basket))
          else
            fail(new Exception(s"Service: $service feature flag is switched off"))
        if (services.contains(service)) {
          service match {
            case HMRCMTDIT  => gotoIdentify(showItsaFlag, service)
            case HMRCPIR    => gotoIdentify(showPirFlag, service)
            case HMRCMTDVAT => gotoIdentify(showVatFlag, service)
            case HMRCCGTPD  => gotoIdentify(showCgtFlag, service)
          }
        } else goto(SelectPersonalService(services, basket))
    }

    def selectedBusinessService(showVatFlag: Boolean)(agent: AuthorisedAgent)(confirmed: Confirmation) = Transition {
      case SelectBusinessService =>
        if (confirmed.choice) {
          if (showVatFlag) goto(IdentifyBusinessClient)
          else fail(new Exception(s"Service: $HMRCMTDVAT feature flag is switched off"))
        } else {
          goto(root)
        }
    }

    def selectedTrustService(showTrustsFlag: Boolean)(agent: AuthorisedAgent)(confirmed: Confirmation) = Transition {
      case SelectTrustService(_, basket) =>
        if (confirmed.choice) {
          if (showTrustsFlag) goto(IdentifyTrustClient(TRUST, basket))
          else fail(new Exception(s"Service: $TRUST feature flag is switched off"))
        } else {
          goto(root)
        }
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
                goto(TrustNotFound)
            }
          }
      }

    // TODO build the actual call to DES to confirm Cgt Ref with known fact matching - for now this is stubbed out
    def identifiedCgtClient(getCgtRefName: GetCgtRefName)(agent: AuthorisedAgent)(cgtClient: CgtClient) =
      Transition {
        case IdentifyTrustClient(HMRCCGTPD, basket) =>
          getCgtRefName(cgtClient.cgtRef).flatMap { name =>
            goto(ConfirmClientTrustCgt(AuthorisationRequest(name, CgtInvitation(cgtClient.cgtRef)), basket))
          }
        case IdentifyPersonalClient(HMRCCGTPD, basket) =>
          getCgtRefName(cgtClient.cgtRef).flatMap { name =>
            goto(
              ConfirmClientPersonalCgt(
                AuthorisationRequest(name, CgtInvitation(cgtClient.cgtRef, clientType = Some(personal))),
                basket))
          }
      }

    def identifiedItsaClient(checkPostcodeMatches: CheckPostcodeMatches)(
      hasPendingInvitationsFor: HasPendingInvitations)(hasActiveRelationshipFor: HasActiveRelationship)(
      getClientName: GetClientName)(createMultipleInvitations: CreateMultipleInvitations)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(agent: AuthorisedAgent)(itsaClient: ItsaClient) = Transition {
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
                                 ItsaInvitation(Nino(itsaClient.clientIdentifier), Postcode(itsaClient.postcode))),
                               basket))
                         }
                       case Some(false) => goto(KnownFactNotMatched(basket))
                       case None        => goto(ClientNotSignedUp(HMRCMTDIT, basket))
                     }
        } yield endState
    }

    def identifiedVatClient(checkRegDateMatches: CheckRegDateMatches)(hasPendingInvitationsFor: HasPendingInvitations)(
      hasActiveRelationshipFor: HasActiveRelationship)(getClientName: GetClientName)(
      createMultipleInvitations: CreateMultipleInvitations)(getAgentLink: GetAgentLink)(getAgencyEmail: GetAgencyEmail)(
      agent: AuthorisedAgent)(vatClient: VatClient) = Transition {
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
                                 VatInvitation(
                                   Some(personal),
                                   Vrn(vatClient.clientIdentifier),
                                   VatRegDate(vatClient.registrationDate))),
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
                                 VatInvitation(
                                   Some(business),
                                   Vrn(vatClient.clientIdentifier),
                                   VatRegDate(vatClient.registrationDate)))))

                         }
                       case Some(423) => goto(CannotCreateRequest(Set.empty))
                       case Some(_)   => goto(KnownFactNotMatched(Set.empty))
                       case None      => goto(ClientNotSignedUp(HMRCMTDVAT, Set.empty))
                     }
        } yield endState
    }

    type CheckDOBMatches = (Nino, LocalDate) => Future[Option[Boolean]]

    def identifiedIrvClient(checkDobMatches: CheckDOBMatches)(hasPendingInvitationsFor: HasPendingInvitations)(
      hasActiveRelationshipFor: HasActiveRelationship)(getClientName: GetClientName)(
      createMultipleInvitations: CreateMultipleInvitations)(getAgentLink: GetAgentLink)(getAgencyEmail: GetAgencyEmail)(
      agent: AuthorisedAgent)(irvClient: IrvClient) = Transition {
      case IdentifyPersonalClient(HMRCPIR, basket) =>
        for {
          dobMatches <- checkDobMatches(Nino(irvClient.clientIdentifier), LocalDate.parse(irvClient.dob))

          endState <- dobMatches match {
                       case Some(true) =>
                         getClientName(irvClient.clientIdentifier, HMRCPIR)
                           .map { clientName =>
                             AuthorisationRequest(
                               clientName.getOrElse(""),
                               PirInvitation(Nino(irvClient.clientIdentifier), DOB(irvClient.dob)))
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
    def clientConfirmed(createMultipleInvitations: CreateMultipleInvitations)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(hasPendingInvitationsFor: HasPendingInvitations)(
      hasActiveRelationshipFor: HasActiveRelationship)(authorisedAgent: AuthorisedAgent)(confirmation: Confirmation) =
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

        case ConfirmClientPersonalCgt(request, basket) =>
          if (confirmation.choice) {
            checkIfPendingOrActiveAndGoto(
              ReviewAuthorisationsPersonal(authorisedAgent.personalServices, basket + request))(
              personal,
              authorisedAgent.arn,
              request.invitation.clientId,
              HMRCCGTPD,
              basket)(hasPendingInvitationsFor, hasActiveRelationshipFor)
          } else goto(IdentifyPersonalClient(HMRCCGTPD, basket))

        case ConfirmClientTrustCgt(request, basket) =>
          if (confirmation.choice) {
            checkIfPendingOrActiveAndGoto(
              ReviewAuthorisationsTrust(authorisedAgent.personalServices, basket + request))(
              personal,
              authorisedAgent.arn,
              request.invitation.clientId,
              HMRCCGTPD,
              basket)(hasPendingInvitationsFor, hasActiveRelationshipFor)
          } else goto(IdentifyPersonalClient(HMRCCGTPD, basket))

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

    def authorisationsReviewed(createMultipleInvitations: CreateMultipleInvitations)(getAgentLink: GetAgentLink)(
      getAgencyEmail: GetAgencyEmail)(agent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {
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

    def deleteAuthorisationRequest(itemId: String)(authorisedAgent: AuthorisedAgent) =
      Transition {
        case ReviewAuthorisationsPersonal(_, basket) =>
          val deleteItem: AuthorisationRequest =
            basket.find(_.itemId == itemId).getOrElse(throw new Exception("No Item to delete"))
          goto(DeleteAuthorisationRequestPersonal(deleteItem, basket))
      }

    def confirmDeleteAuthorisationRequest(authorisedAgent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {
        case DeleteAuthorisationRequestPersonal(authorisationRequest, basket) =>
          if (confirmation.choice) {
            if ((basket - authorisationRequest).nonEmpty)
              goto(ReviewAuthorisationsPersonal(authorisedAgent.personalServices, basket - authorisationRequest))
            else goto(AllAuthorisationsRemoved)
          } else goto(ReviewAuthorisationsPersonal(authorisedAgent.personalServices, basket))
      }
  }
}
