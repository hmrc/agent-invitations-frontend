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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.{States, goto}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Vrn}
import uk.gov.hmrc.domain.Nino

import play.api.mvc.{Call, Request, Result}

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
    case class SelectBusinessService(basket: Basket) extends State
    case class IdentifyPersonalClient(service: String, basket: Basket) extends State
    case class IdentifyBusinessClient(basket: Basket) extends State
    case class PendingInvitationExists(clientType: ClientType, basket: Basket) extends State
    case class ActiveAuthorisationExists(clientType: ClientType, service: String, basket: Basket) extends State
    case class KnownFactNotMatched(basket: Basket) extends State
    case class ConfirmClientItsa(request: AuthorisationRequest, basket: Basket) extends State
    case class ConfirmClientIrv(request: AuthorisationRequest, basket: Basket) extends State
    case class ConfirmClientPersonalVat(request: AuthorisationRequest, basket: Basket) extends State
    case class ConfirmClientBusinessVat(request: AuthorisationRequest, basket: Basket) extends State
    case class ReviewAuthorisationsPersonal(basket: Basket) extends State
    case class ReviewAuthorisationsBusiness(basket: Basket) extends State
    case class SomeAuthorisationsFailed(basket: Basket) extends State
    case class AllAuthorisationsFailed(basket: Basket) extends State
    case class DeleteAuthorisationRequestPersonal(authorisationRequest: AuthorisationRequest, basket: Basket)
        extends State
    case class DeleteAuthorisationRequestBusiness(authorisationRequest: AuthorisationRequest, basket: Basket)
        extends State
    case class InvitationSentPersonal(invitationLink: String, continueUrl: Option[String]) extends State
    case class InvitationSentBusiness(invitationLink: String, continueUrl: Option[String]) extends State
    case class ClientNotSignedUp(service: String, basket: Basket) extends State
    case class AllAuthorisationsRemoved() extends State
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
          case ClientType.business => goto(SelectBusinessService(basket))
        }
    }

    def selectedPersonalService(showItsaFlag: Boolean, showPirFlag: Boolean, showVatFlag: Boolean)(
      agent: AuthorisedAgent)(service: String) = Transition {
      case SelectPersonalService(services, basket) =>
        if (services.contains(service)) {
          service match {
            case HMRCMTDIT =>
              if (showItsaFlag) goto(IdentifyPersonalClient(service, basket))
              else fail(new Exception(s"Service: $service feature flagged is switched off"))

            case HMRCPIR =>
              if (showPirFlag) goto(IdentifyPersonalClient(service, basket))
              else fail(new Exception(s"Service: $service feature flagged is switched off"))

            case HMRCMTDVAT =>
              if (showVatFlag) goto(IdentifyPersonalClient(service, basket))
              else fail(new Exception(s"Service: $service feature flagged is switched off"))
          }
        } else goto(SelectPersonalService(services, basket))
    }

    def selectedBusinessService(showVatFlag: Boolean)(agent: AuthorisedAgent)(confirmed: Confirmation) = Transition {
      case SelectBusinessService(basket) =>
        if (confirmed.choice) {
          if (showVatFlag) goto(IdentifyBusinessClient(basket))
          else fail(new Exception(s"Service: $HMRCMTDVAT feature flagged is switched off"))
        } else {
          goto(root)
        }
    }

    type HasPendingInvitations = (Arn, String, String) => Future[Boolean]
    type HasActiveRelationship = (Arn, String, String) => Future[Boolean]
    type GetClientName = (String, String) => Future[Option[String]]

    private def checkIfPendingOrActiveAndGoto(successState: String => State)(
      clientType: ClientType,
      arn: Arn,
      clientIdentifier: String,
      service: String,
      basket: Basket)(
      hasPendingInvitationsFor: HasPendingInvitations,
      hasActiveRelationshipFor: HasActiveRelationship,
      getClientName: GetClientName): Future[State] =
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
                     case true => goto(ActiveAuthorisationExists(clientType, service, basket))
                     case false =>
                       getClientName(clientIdentifier, service).flatMap { clientNameOpt =>
                         val clientName = clientNameOpt.getOrElse("")
                         goto(successState(clientName))
                       }
                   }
                 }
      } yield result

    type CheckPostcodeMatches = (Nino, String) => Future[Option[Boolean]]

    def identifiedItsaClient(checkPostcodeMatches: CheckPostcodeMatches)(
      hasPendingInvitationsFor: HasPendingInvitations)(hasActiveRelationshipFor: HasActiveRelationship)(
      redirectToConfirmItsaFlag: Boolean)(getClientName: GetClientName)(agent: AuthorisedAgent)(
      itsaClient: ItsaClient) = Transition {
      case IdentifyPersonalClient(HMRCMTDIT, basket) =>
        checkPostcodeMatches(Nino(itsaClient.clientIdentifier), itsaClient.postcode.getOrElse("")).flatMap {
          case Some(true) =>
            checkIfPendingOrActiveAndGoto(clientName =>
              if (redirectToConfirmItsaFlag) {
                ConfirmClientItsa(
                  AuthorisationRequest(
                    clientName,
                    ItsaInvitation(Nino(itsaClient.clientIdentifier), itsaClient.postcode.map(Postcode.apply))),
                  basket)
              } else ReviewAuthorisationsPersonal(basket))(
              personal,
              agent.arn,
              itsaClient.clientIdentifier,
              HMRCMTDIT,
              basket)(hasPendingInvitationsFor, hasActiveRelationshipFor, getClientName)

          case Some(false) => goto(KnownFactNotMatched(basket))
          case None        => goto(ClientNotSignedUp(HMRCMTDIT, basket))
        }
    }

    type CheckRegDateMatches = (Vrn, LocalDate) => Future[Option[Boolean]]

    def identifiedVatClient(checkRegDateMatches: CheckRegDateMatches)(hasPendingInvitationsFor: HasPendingInvitations)(
      hasActiveRelationshipFor: HasActiveRelationship)(redirectToConfirmVatFlag: Boolean)(getClientName: GetClientName)(
      agent: AuthorisedAgent)(vatClient: VatClient) = Transition {

      case IdentifyPersonalClient(HMRCMTDVAT, basket) =>
        checkRegDateMatches(Vrn(vatClient.clientIdentifier), LocalDate.parse(vatClient.registrationDate.getOrElse("")))
          .flatMap {
            case Some(true) =>
              checkIfPendingOrActiveAndGoto(clientName =>
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
                } else ReviewAuthorisationsPersonal(basket))(
                personal,
                agent.arn,
                vatClient.clientIdentifier,
                HMRCMTDVAT,
                basket)(hasPendingInvitationsFor, hasActiveRelationshipFor, getClientName)

            case Some(false) => goto(KnownFactNotMatched(basket))
            case None        => goto(ClientNotSignedUp(HMRCMTDVAT, basket))
          }

      case IdentifyBusinessClient(basket) =>
        checkRegDateMatches(Vrn(vatClient.clientIdentifier), LocalDate.parse(vatClient.registrationDate.getOrElse("")))
          .flatMap {
            case Some(true) =>
              checkIfPendingOrActiveAndGoto(clientName =>
                if (redirectToConfirmVatFlag) {
                  ConfirmClientBusinessVat(
                    AuthorisationRequest(
                      clientName,
                      VatInvitation(
                        Some(business),
                        Vrn(vatClient.clientIdentifier),
                        vatClient.registrationDate.map(VatRegDate.apply))),
                    basket
                  )
                } else ReviewAuthorisationsBusiness(basket))(
                business,
                agent.arn,
                vatClient.clientIdentifier,
                HMRCMTDVAT,
                basket)(hasPendingInvitationsFor, hasActiveRelationshipFor, getClientName)

            case Some(false) => goto(KnownFactNotMatched(basket))
            case None        => goto(ClientNotSignedUp(HMRCMTDVAT, basket))
          }
    }

    type CheckDOBMatches = (Nino, LocalDate) => Future[Option[Boolean]]

    def identifiedIrvClient(checkDobMatches: CheckDOBMatches)(hasPendingInvitationsFor: HasPendingInvitations)(
      hasActiveRelationshipFor: HasActiveRelationship)(redirectToConfirmVatFlag: Boolean)(getClientName: GetClientName)(
      agent: AuthorisedAgent)(irvClient: IrvClient) = Transition {

      case IdentifyPersonalClient(HMRCPIR, basket) =>
        checkDobMatches(Nino(irvClient.clientIdentifier), LocalDate.parse(irvClient.dob.getOrElse(""))).flatMap {
          case Some(true) =>
            checkIfPendingOrActiveAndGoto(clientName =>
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
              basket)(hasPendingInvitationsFor, hasActiveRelationshipFor, getClientName)

          case Some(false) => goto(KnownFactNotMatched(basket))
          case None        => goto(ClientNotSignedUp(HMRCPIR, basket)) //dubious? citizen record not found
        }
    }

    def clientConfirmed(authorisedAgent: AuthorisedAgent)(confirmation: Confirmation) = Transition {
      case ConfirmClientItsa(request, basket) =>
        if (confirmation.choice) goto(ReviewAuthorisationsPersonal(basket + request))
        else goto(IdentifyPersonalClient(HMRCMTDIT, basket))
      case ConfirmClientIrv(request, basket) =>
        if (confirmation.choice) goto(ReviewAuthorisationsPersonal(basket + request))
        else goto(IdentifyPersonalClient(HMRCPIR, basket))
      case ConfirmClientPersonalVat(request, basket) =>
        if (confirmation.choice) goto(ReviewAuthorisationsPersonal(basket + request))
        else goto(IdentifyPersonalClient(HMRCMTDVAT, basket))
      case ConfirmClientBusinessVat(request, basket) =>
        if (confirmation.choice) goto(ReviewAuthorisationsBusiness(basket + request))
        else goto(IdentifyBusinessClient(basket))
    }

    type CreateMultipleInvitations =
      (Arn, Option[ClientType], Set[AuthorisationRequest]) => Future[Set[AuthorisationRequest]]
    type GetAgentLink = (Arn, Option[ClientType]) => Future[String]

    def authorisationsReviewed(createMultipleInvitations: CreateMultipleInvitations)(getAgentLink: GetAgentLink)(
      agent: AuthorisedAgent)(confirmation: Confirmation) = {

      def createAndProcessInvitations(successState: State, basket: Basket) =
        for {
          processedRequests <- createMultipleInvitations(agent.arn, Some(personal), basket)
          result <- if (AuthorisationRequest.eachHasBeenCreatedIn(processedRequests)) goto(successState)
                   else if (AuthorisationRequest.noneHaveBeenCreatedIn(processedRequests))
                     goto(AllAuthorisationsFailed(basket))
                   else goto(SomeAuthorisationsFailed(basket))
        } yield result

      Transition {
        case ReviewAuthorisationsPersonal(basket) =>
          if (confirmation.choice)
            goto(
              SelectPersonalService(
                if (agent.isWhitelisted) Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT) else Set(HMRCMTDIT, HMRCMTDVAT), //shall we remove existing service from the list?
                basket))
          else
            getAgentLink(agent.arn, Some(personal)).flatMap { invitationLink =>
              createAndProcessInvitations(InvitationSentPersonal(invitationLink, None), basket)
            }
        case ReviewAuthorisationsBusiness(basket) =>
          if (confirmation.choice) goto(SelectBusinessService(basket))
          else
            getAgentLink(agent.arn, Some(personal)).flatMap { invitationLink =>
              createAndProcessInvitations(InvitationSentBusiness(invitationLink, None), basket)
            }
      }
    }

    def deleteAuthorisationRequest(authorisedAgent: AuthorisedAgent)(itemId: String) =
      Transition {
        case ReviewAuthorisationsPersonal(basket) => {
          val deleteItem: AuthorisationRequest =
            basket.find(_.itemId == itemId).getOrElse(throw new Exception("No Item to delete"))
          goto(DeleteAuthorisationRequestPersonal(deleteItem, basket))
        }
        case ReviewAuthorisationsBusiness(basket) => {
          val deleteItem: AuthorisationRequest =
            basket.find(_.itemId == itemId).getOrElse(throw new Exception("No Item to delete"))
          goto(DeleteAuthorisationRequestBusiness(deleteItem, basket))
        }
      }

    def confirmDeleteAuthorisationRequest(authorisedAgent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {
        case DeleteAuthorisationRequestPersonal(authorisationRequest, basket) =>
          if (confirmation.choice) {
            if ((basket - authorisationRequest).nonEmpty)
              goto(ReviewAuthorisationsPersonal(basket - authorisationRequest))
            else goto(AllAuthorisationsRemoved())
          } else goto(ReviewAuthorisationsPersonal(basket))
        case DeleteAuthorisationRequestBusiness(authorisationRequest, basket) =>
          if (confirmation.choice) {
            if ((basket - authorisationRequest).nonEmpty)
              goto(ReviewAuthorisationsBusiness(basket - authorisationRequest))
            else goto(AllAuthorisationsRemoved())
          } else goto(ReviewAuthorisationsBusiness(basket))
      }
  }

}
