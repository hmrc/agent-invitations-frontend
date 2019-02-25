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

  def root: State = States.SelectClientType(Set.empty)

  def errorFor(ex: Exception): Error = Errors.GenericError(ex)
  def transitionNotAllowed(state: State, breadcrumbs: List[State], transition: Transition): Error =
    Errors.TransitionNotAllowed(state, breadcrumbs, transition)

  object Errors {
    case class TransitionNotAllowed(state: State, breadcrumbs: List[State], transition: Transition) extends Error
    case class GenericError(ex: Exception) extends Error
  }

  type Basket = Set[AuthorisationRequest]

  /* State should contain only minimal set of data required to proceed */
  object States {
    case class SelectClientType(basket: Basket) extends State
    case class SelectPersonalService(services: Set[String], basket: Basket) extends State
    case class SelectBusinessService(basket: Basket) extends State
    case class IdentifyPersonalClient(service: String, basket: Basket) extends State
    case class IdentifyBusinessClient(basket: Basket) extends State
    case class PendingInvitationExists(clientType: ClientType, basket: Basket) extends State
    case class ActiveRelationshipExists(clientType: ClientType, basket: Basket) extends State
    case class KnownFactNotMatched(basket: Basket) extends State
    case class ConfirmClientItsa(clientName: String, basket: Basket) extends State
    case class ConfirmClientIrv(clientName: String, basket: Basket) extends State
    case class ConfirmClientPersonalVat(clientName: String, basket: Basket) extends State
    case class ConfirmClientBusinessVat(clientName: String, basket: Basket) extends State
    case class ReviewAuthorisationsPersonal(basket: Basket) extends State
    case class ReviewAuthorisationsBusiness(basket: Basket) extends State
    case class SomeAuthorisationsFailed(basket: Basket) extends State
    case class AllAuthorisationsFailed(basket: Basket) extends State
    case class InvitationSentPersonal(invitationLink: String, continueUrl: Option[String]) extends State
    case class InvitationSentBusiness(invitationLink: String, continueUrl: Option[String]) extends State
  }

  object Transitions {
    import States._

    val startJourney = Transition {
      case _ => goto(SelectClientType(Set.empty))
    }

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

    def selectedPersonalService(agent: AuthorisedAgent)(service: String) = Transition {
      case SelectPersonalService(services, basket) =>
        if (services.contains(service)) goto(IdentifyPersonalClient(service, basket))
        else goto(SelectPersonalService(services, basket))
    }

    def selectedBusinessService(agent: AuthorisedAgent)(confirmed: Confirmation) = Transition {
      case SelectBusinessService(basket) =>
        if (confirmed.choice) {
          goto(IdentifyBusinessClient(basket))
        } else {
          goto(root)
        }
    }

    type HasPendingInvitations = (Arn, String, String) => Future[Boolean]
    type HasActiveRelationship = (Arn, String, String) => Future[Boolean]
    type GetClientName = (String, String) => Future[Option[String]]

    def identifiedItsaClient(checkPostcodeMatches: (Nino, String) => Future[Option[Boolean]])(
      hasPendingInvitationsFor: HasPendingInvitations)(hasActiveRelationshipFor: HasActiveRelationship)(
      getClientName: GetClientName)(agent: AuthorisedAgent)(itsaClient: ItsaClient) = Transition {
      case IdentifyPersonalClient(HMRCMTDIT, basket) =>
        checkPostcodeMatches(Nino(itsaClient.clientIdentifier), itsaClient.postcode.getOrElse("")).flatMap {
          case Some(true) => {
            for {
              hasPendingInvitations <- if (basket.exists(_.invitation.service == HMRCMTDIT) &&
                                           basket.exists(_.invitation.clientId == itsaClient.clientIdentifier))
                                        Future.successful(true)
                                      else
                                        hasPendingInvitationsFor(agent.arn, itsaClient.clientIdentifier, HMRCMTDIT)
              result <- if (hasPendingInvitations) {
                         goto(PendingInvitationExists(personal, basket))
                       } else {
                         hasActiveRelationshipFor(agent.arn, itsaClient.clientIdentifier, HMRCMTDIT).flatMap {
                           case true => goto(ActiveRelationshipExists(personal, basket))
                           case false =>
                             getClientName(itsaClient.clientIdentifier, HMRCMTDIT).flatMap { clientNameOpt =>
                               val clientName = clientNameOpt.getOrElse("")
                               goto(ConfirmClientItsa(clientName, basket))
                             }
                         }
                       }
            } yield result
          }
          case Some(false) => goto(KnownFactNotMatched(basket))
        }
    }

    def identifiedVatClient(checkRegDateMatches: (Vrn, LocalDate) => Future[Option[Boolean]])(
      getClientName: GetClientName)(agent: AuthorisedAgent)(vatClient: VatClient) = Transition {
      case IdentifyPersonalClient(HMRCMTDVAT, basket) =>
        checkRegDateMatches(Vrn(vatClient.clientIdentifier), LocalDate.parse(vatClient.registrationDate.getOrElse("")))
          .flatMap {
            case Some(true) =>
              getClientName(vatClient.clientIdentifier, HMRCMTDVAT).flatMap { clientNameOpt =>
                val clientName = clientNameOpt.getOrElse("")
                goto(ConfirmClientPersonalVat(clientName, basket))
              }
            case Some(false) => goto(KnownFactNotMatched(basket))
          }
      case IdentifyBusinessClient(basket) =>
        checkRegDateMatches(Vrn(vatClient.clientIdentifier), LocalDate.parse(vatClient.registrationDate.getOrElse("")))
          .flatMap {
            case Some(true) =>
              getClientName(vatClient.clientIdentifier, HMRCMTDVAT).flatMap { clientNameOpt =>
                val clientName = clientNameOpt.getOrElse("")
                goto(ConfirmClientBusinessVat(clientName, basket))
              }
            case Some(false) => goto(KnownFactNotMatched(basket))
          }
    }

    def identifiedIrvClient(checkDobMatches: (Nino, LocalDate) => Future[Option[Boolean]])(
      getClientName: GetClientName)(agent: AuthorisedAgent)(irvClient: IrvClient) = Transition {
      case IdentifyPersonalClient(HMRCPIR, basket) =>
        checkDobMatches(Nino(irvClient.clientIdentifier), LocalDate.parse(irvClient.dob.getOrElse(""))).flatMap {
          case Some(true) =>
            getClientName(irvClient.clientIdentifier, HMRCPIR).flatMap { clientNameOpt =>
              val clientName = clientNameOpt.getOrElse("")
              goto(ConfirmClientIrv(clientName, basket))
            }
          case Some(false) => goto(KnownFactNotMatched(basket))
        }
    }

    def clientConfirmed(authorisedAgent: AuthorisedAgent)(confirmation: Confirmation) = Transition {
      case ConfirmClientItsa(_, basket) =>
        if (confirmation.choice) goto(ReviewAuthorisationsPersonal(basket))
        else goto(IdentifyPersonalClient(HMRCMTDIT, basket))
      case ConfirmClientIrv(_, basket) =>
        if (confirmation.choice) goto(ReviewAuthorisationsPersonal(basket))
        else goto(IdentifyPersonalClient(HMRCPIR, basket))
      case ConfirmClientPersonalVat(_, basket) =>
        if (confirmation.choice) goto(ReviewAuthorisationsPersonal(basket))
        else goto(IdentifyPersonalClient(HMRCMTDVAT, basket))
      case ConfirmClientBusinessVat(_, basket) =>
        if (confirmation.choice) goto(ReviewAuthorisationsBusiness(basket))
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
                if (agent.isWhitelisted) Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT) else Set(HMRCMTDIT, HMRCMTDVAT), //FIXME shall we remove existing service from the list?
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
  }

}
