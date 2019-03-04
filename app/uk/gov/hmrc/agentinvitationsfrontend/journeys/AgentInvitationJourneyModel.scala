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
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Vrn}
import uk.gov.hmrc.domain.Nino

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object AgentInvitationJourneyModel extends JourneyModel {

  sealed trait State
  sealed trait Error

  val root: State = States.SelectClientType(Set.empty)

  type Basket = Set[AuthorisationRequest]

  /* State should contain only minimal set of data required to proceed */
  object States {
    case class SelectClientType(basket: Basket) extends State
    case class SelectPersonalService(services: Set[String], basket: Basket) extends State
    case object SelectBusinessService extends State
    case class IdentifyPersonalClient(service: String, basket: Basket) extends State
    case object IdentifyBusinessClient extends State
    case class PendingInvitationExists(clientType: ClientType, basket: Basket) extends State
    case class ActiveAuthorisationExists(clientType: ClientType, service: String, basket: Basket) extends State
    case class KnownFactNotMatched(basket: Basket) extends State
    case class ConfirmClientItsa(request: AuthorisationRequest, basket: Basket) extends State
    case class ConfirmClientIrv(request: AuthorisationRequest, basket: Basket) extends State
    case class ConfirmClientPersonalVat(request: AuthorisationRequest, basket: Basket) extends State
    case class ConfirmClientBusinessVat(request: AuthorisationRequest) extends State
    case class ReviewAuthorisationsPersonal(basket: Basket) extends State
    case class SomeAuthorisationsFailed(basket: Basket) extends State
    case class AllAuthorisationsFailed(basket: Basket) extends State
    case class DeleteAuthorisationRequestPersonal(authorisationRequest: AuthorisationRequest, basket: Basket)
        extends State
    case class InvitationSentPersonal(invitationLink: String, continueUrl: Option[String]) extends State
    case class InvitationSentBusiness(invitationLink: String, continueUrl: Option[String]) extends State
    case class ClientNotSignedUp(service: String, basket: Basket) extends State
    case object AllAuthorisationsRemoved extends State
  }

  object Transitions {
    import States._

    def showSelectClientType(agent: AuthorisedAgent) = Transition {
      case _ => goto(SelectClientType(Set.empty)) // clears basket
    }

    def selectedClientType(agent: AuthorisedAgent)(clientType: ClientType) = Transition {
      case SelectClientType(basket) =>
        clientType match {
          case ClientType.personal =>
            goto(
              SelectPersonalService(
                if (agent.isWhitelisted) Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT) else Set(HMRCMTDIT, HMRCMTDVAT),
                basket
              ))
          case ClientType.business => goto(SelectBusinessService)
        }
    }

    def selectedPersonalService(showItsaFlag: Boolean, showPirFlag: Boolean, showVatFlag: Boolean)(
      agent: AuthorisedAgent)(service: String) = Transition {
      case SelectPersonalService(services, basket) =>
        if (services.contains(service)) {
          service match {
            case HMRCMTDIT =>
              if (showItsaFlag) goto(IdentifyPersonalClient(service, basket))
              else fail(new Exception(s"Service: $service feature flag is switched off"))

            case HMRCPIR =>
              if (showPirFlag) goto(IdentifyPersonalClient(service, basket))
              else fail(new Exception(s"Service: $service feature flag is switched off"))

            case HMRCMTDVAT =>
              if (showVatFlag) goto(IdentifyPersonalClient(service, basket))
              else fail(new Exception(s"Service: $service feature flag is switched off"))
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

    type HasPendingInvitations = (Arn, String, String) => Future[Boolean]
    type HasActiveRelationship = (Arn, String, String) => Future[Boolean]
    type GetClientName = (String, String) => Future[Option[String]]

    private def checkIfPendingOrActiveThenKnownFactMatchAndGoto(successState: String => State)(
      clientType: ClientType,
      arn: Arn,
      clientIdentifier: String,
      service: String,
      basket: Basket = Set.empty,
      knownFactMatches: Option[Boolean],
      kfcFlag: Boolean)(
      hasPendingInvitationsFor: HasPendingInvitations,
      hasActiveRelationshipFor: HasActiveRelationship,
      getClientName: GetClientName): Future[State] = {
      val checkIfPendingOrActive = for {
        hasPendingInvitations <- if (basket.exists(_.invitation.service == service) &&
                                     basket.exists(_.invitation.clientId == clientIdentifier))
                                  Future.successful(true)
                                else
                                  hasPendingInvitationsFor(arn, clientIdentifier, service)
        result <- if (hasPendingInvitations) {
                   goto(PendingInvitationExists(clientType, basket))
                 } else {
                   hasActiveRelationshipFor(arn, clientIdentifier, service).flatMap {
                     case true => goto(ActiveAuthorisationExists(clientType, service, basket))
                     case false =>
                       getClientName(clientIdentifier, service).flatMap { clientNameOpt =>
                         val clientName = clientNameOpt.getOrElse("")
                         goto(successState(clientName))
                       }
                   }
                 }
      } yield result

      if (!kfcFlag) checkIfPendingOrActive
      else {
        knownFactMatches match {
          case Some(true)  => checkIfPendingOrActive
          case Some(false) => goto(KnownFactNotMatched(basket))
          case None        => goto(ClientNotSignedUp(service, basket))
        }

      }
    }

    type CheckPostcodeMatches = (Nino, String) => Future[Option[Boolean]]

    def identifiedItsaClient(checkPostcodeMatches: CheckPostcodeMatches)(
      hasPendingInvitationsFor: HasPendingInvitations)(hasActiveRelationshipFor: HasActiveRelationship)(
      redirectToConfirmItsaFlag: Boolean)(showKfcItsa: Boolean)(getClientName: GetClientName)(agent: AuthorisedAgent)(
      itsaClient: ItsaClient) = Transition {
      case IdentifyPersonalClient(HMRCMTDIT, basket) =>
        checkPostcodeMatches(Nino(itsaClient.clientIdentifier), itsaClient.postcode.getOrElse("")).flatMap {
          postcodeMatches =>
            checkIfPendingOrActiveThenKnownFactMatchAndGoto(
              clientName =>
                if (redirectToConfirmItsaFlag) {
                  ConfirmClientItsa(
                    AuthorisationRequest(
                      clientName,
                      ItsaInvitation(Nino(itsaClient.clientIdentifier), itsaClient.postcode.map(Postcode.apply))),
                    basket)
                } else
                  ReviewAuthorisationsPersonal(
                    basket + AuthorisationRequest(
                      clientName,
                      ItsaInvitation(Nino(itsaClient.clientIdentifier), itsaClient.postcode.map(Postcode.apply)))))(
              personal,
              agent.arn,
              itsaClient.clientIdentifier,
              HMRCMTDIT,
              basket,
              postcodeMatches,
              showKfcItsa)(hasPendingInvitationsFor, hasActiveRelationshipFor, getClientName)
        }
    }

    type CheckRegDateMatches = (Vrn, LocalDate) => Future[Option[Boolean]]
    type CreateMultipleInvitations =
      (Arn, Option[ClientType], Set[AuthorisationRequest]) => Future[Set[AuthorisationRequest]]

    def identifiedVatClient(checkRegDateMatches: CheckRegDateMatches)(hasPendingInvitationsFor: HasPendingInvitations)(
      hasActiveRelationshipFor: HasActiveRelationship)(redirectToConfirmVatFlag: Boolean)(showKfcVat: Boolean)(
      getClientName: GetClientName)(createMultipleInvitations: CreateMultipleInvitations)(getAgentLink: GetAgentLink)(
      agent: AuthorisedAgent)(vatClient: VatClient) = Transition {

      case IdentifyPersonalClient(HMRCMTDVAT, basket) =>
        checkRegDateMatches(Vrn(vatClient.clientIdentifier), LocalDate.parse(vatClient.registrationDate.getOrElse("")))
          .flatMap { vatRegDateMatches =>
            checkIfPendingOrActiveThenKnownFactMatchAndGoto(clientName =>
              if (redirectToConfirmVatFlag) {
                ConfirmClientPersonalVat(
                  AuthorisationRequest(
                    clientName,
                    VatInvitation(
                      Some(personal),
                      Vrn(vatClient.clientIdentifier),
                      vatClient.registrationDate.map(VatRegDate.apply))),
                  basket
                )
              } else
                ReviewAuthorisationsPersonal(
                  basket + AuthorisationRequest(
                    clientName,
                    VatInvitation(
                      Some(personal),
                      Vrn(vatClient.clientIdentifier),
                      vatClient.registrationDate.map(VatRegDate.apply)))))(
              personal,
              agent.arn,
              vatClient.clientIdentifier,
              HMRCMTDVAT,
              basket,
              vatRegDateMatches,
              showKfcVat)(hasPendingInvitationsFor, hasActiveRelationshipFor, getClientName)
          }
      case IdentifyBusinessClient =>
        checkRegDateMatches(Vrn(vatClient.clientIdentifier), LocalDate.parse(vatClient.registrationDate.getOrElse("")))
          .flatMap { vatRegDateMatches =>
            val futureSuccessState: Future[State] = {
              for {
                clientName     <- getClientName(vatClient.clientIdentifier, HMRCMTDVAT)
                invitationLink <- getAgentLink(agent.arn, Some(business))
                createInvitations <- createAndProcessInvitations(
                                      InvitationSentBusiness(invitationLink, None),
                                      Set(
                                        AuthorisationRequest(
                                          clientName.getOrElse(""),
                                          VatInvitation(
                                            Some(business),
                                            Vrn(vatClient.clientIdentifier),
                                            vatClient.registrationDate.map(VatRegDate.apply)))),
                                      createMultipleInvitations,
                                      agent.arn
                                    )
              } yield {
                if (redirectToConfirmVatFlag)
                  ConfirmClientBusinessVat(
                    AuthorisationRequest(
                      clientName.getOrElse(""),
                      VatInvitation(
                        Some(business),
                        Vrn(vatClient.clientIdentifier),
                        vatClient.registrationDate.map(VatRegDate.apply))))
                else createInvitations
              }
            }

            val checkIfPendingOrActive = for {
              hasPendingInvitations <- hasPendingInvitationsFor(agent.arn, vatClient.clientIdentifier, HMRCMTDVAT)
              result <- if (hasPendingInvitations) {
                         goto(PendingInvitationExists(business, Set.empty))
                       } else {
                         hasActiveRelationshipFor(agent.arn, vatClient.clientIdentifier, HMRCMTDVAT).flatMap {
                           case true  => goto(ActiveAuthorisationExists(business, HMRCMTDVAT, Set.empty))
                           case false => futureSuccessState.flatMap(successState => goto(successState))
                         }
                       }
            } yield result

            if (!showKfcVat) checkIfPendingOrActive
            else {
              vatRegDateMatches match {
                case Some(true)  => checkIfPendingOrActive
                case Some(false) => goto(KnownFactNotMatched(Set.empty))
                case None        => goto(ClientNotSignedUp(HMRCMTDVAT, Set.empty))
              }

            }

          }
    }

    type CheckDOBMatches = (Nino, LocalDate) => Future[Option[Boolean]]

    def identifiedIrvClient(checkDobMatches: CheckDOBMatches)(hasPendingInvitationsFor: HasPendingInvitations)(
      hasActiveRelationshipFor: HasActiveRelationship)(redirectToConfirmVatFlag: Boolean)(showKfcPir: Boolean)(
      getClientName: GetClientName)(agent: AuthorisedAgent)(irvClient: IrvClient) = Transition {
      case IdentifyPersonalClient(HMRCPIR, basket) =>
        checkDobMatches(Nino(irvClient.clientIdentifier), LocalDate.parse(irvClient.dob.getOrElse(""))).flatMap {
          dobMatches =>
            checkIfPendingOrActiveThenKnownFactMatchAndGoto(clientName =>
              if (redirectToConfirmVatFlag) {
                ConfirmClientIrv(
                  AuthorisationRequest(
                    clientName,
                    PirInvitation(Nino(irvClient.clientIdentifier), irvClient.dob.map(DOB.apply))),
                  basket)
              } else ReviewAuthorisationsPersonal(basket))(
              personal,
              agent.arn,
              irvClient.clientIdentifier,
              HMRCPIR,
              basket,
              dobMatches,
              showKfcPir)(hasPendingInvitationsFor, hasActiveRelationshipFor, getClientName)
        }
    }

    private def createAndProcessInvitations(
      successState: State,
      basket: Basket,
      createMultipleInvitations: CreateMultipleInvitations,
      arn: Arn) =
      for {
        processedRequests <- createMultipleInvitations(arn, Some(personal), basket)
        result <- if (AuthorisationRequest.eachHasBeenCreatedIn(processedRequests)) goto(successState)
                 else if (AuthorisationRequest.noneHaveBeenCreatedIn(processedRequests))
                   goto(AllAuthorisationsFailed(basket))
                 else goto(SomeAuthorisationsFailed(basket))
      } yield result

    def clientConfirmed(createMultipleInvitations: CreateMultipleInvitations)(getAgentLink: GetAgentLink)(
      authorisedAgent: AuthorisedAgent)(confirmation: Confirmation) = Transition {
      case ConfirmClientItsa(request, basket) =>
        if (confirmation.choice) goto(ReviewAuthorisationsPersonal(basket + request))
        else goto(IdentifyPersonalClient(HMRCMTDIT, basket))
      case ConfirmClientIrv(request, basket) =>
        if (confirmation.choice) goto(ReviewAuthorisationsPersonal(basket + request))
        else goto(IdentifyPersonalClient(HMRCPIR, basket))
      case ConfirmClientPersonalVat(request, basket) =>
        if (confirmation.choice) goto(ReviewAuthorisationsPersonal(basket + request))
        else goto(IdentifyPersonalClient(HMRCMTDVAT, basket))
      case ConfirmClientBusinessVat(request) =>
        if (confirmation.choice) {
          getAgentLink(authorisedAgent.arn, Some(business)).flatMap { invitationLink =>
            createAndProcessInvitations(
              InvitationSentBusiness(invitationLink, None),
              Set(request),
              createMultipleInvitations,
              authorisedAgent.arn)
          }
        } else goto(IdentifyBusinessClient)
    }

    type GetAgentLink = (Arn, Option[ClientType]) => Future[String]

    def authorisationsReviewed(createMultipleInvitations: CreateMultipleInvitations)(getAgentLink: GetAgentLink)(
      agent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {
        case ReviewAuthorisationsPersonal(basket) =>
          if (confirmation.choice)
            goto(
              SelectPersonalService(
                if (agent.isWhitelisted) Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT) else Set(HMRCMTDIT, HMRCMTDVAT), //shall we remove existing service from the list?
                basket))
          else
            getAgentLink(agent.arn, Some(personal)).flatMap { invitationLink =>
              createAndProcessInvitations(
                InvitationSentPersonal(invitationLink, None),
                basket,
                createMultipleInvitations,
                agent.arn)
            }
      }

    def deleteAuthorisationRequest(authorisedAgent: AuthorisedAgent)(itemId: String) =
      Transition {
        case ReviewAuthorisationsPersonal(basket) => {
          val deleteItem: AuthorisationRequest =
            basket.find(_.itemId == itemId).getOrElse(throw new Exception("No Item to delete"))
          goto(DeleteAuthorisationRequestPersonal(deleteItem, basket))
        }
      }

    def confirmDeleteAuthorisationRequest(authorisedAgent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {
        case DeleteAuthorisationRequestPersonal(authorisationRequest, basket) =>
          if (confirmation.choice) {
            if ((basket - authorisationRequest).nonEmpty)
              goto(ReviewAuthorisationsPersonal(basket - authorisationRequest))
            else goto(AllAuthorisationsRemoved)
          } else goto(ReviewAuthorisationsPersonal(basket))
      }
  }
}
