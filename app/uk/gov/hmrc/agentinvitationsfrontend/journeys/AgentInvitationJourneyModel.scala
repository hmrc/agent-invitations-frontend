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
import uk.gov.hmrc.agentmtdidentifiers.model.Arn

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration

object AgentInvitationJourneyModel extends JourneyModel {

  sealed trait State
  sealed trait Error

  def root: State = States.Start

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
    case object Start extends State
    case object SelectClientType extends State
    case class ClientTypeSelected(clientType: ClientType) extends State

    case class SelectPersonalService(basket: Basket, services: Set[String]) extends State
    case class SelectBusinessService(basket: Basket) extends State
    case class PersonalServiceSelected(service: String, basket: Basket) extends State
    case class BusinessServiceSelected(basket: Basket) extends State

    case class IdentifyPersonalClient(service: String, basket: Basket) extends State
    case class IdentifyBusinessClient(basket: Basket) extends State
    case class ItsaIdentifiedClient(clientIdentifier: String, postcode: Option[String], basket: Basket) extends State
    case class IrvIdentifiedClient(clientIdentifier: String, dob: Option[String], basket: Basket) extends State
    case class VatIdentifiedPersonalClient(clientIdentifier: String, registrationDate: Option[String], basket: Basket)
        extends State
    case class VatIdentifiedBusinessClient(clientIdentifier: String, registrationDate: Option[String], basket: Basket)
        extends State

    case class ConfirmClientItsa(clientName: String, basket: Basket) extends State
    case class ConfirmClientIrv(clientName: String, basket: Basket) extends State
    case class ConfirmClientPersonalVat(clientName: String, basket: Basket) extends State
    case class ConfirmClientBusinessVat(clientName: String, basket: Basket) extends State
    case class ClientConfirmedPersonal(basket: Basket) extends State
    case class ClientConfirmedBusiness(basket: Basket) extends State

    case class ReviewAuthorisationsPersonal(basket: Basket) extends State
    case class ReviewAuthorisationsBusiness(basket: Basket) extends State
    case object AuthorisationsReviewedPersonal extends State
    case object AuthorisationsReviewedBusiness extends State

    case class InvitationSentPersonal(invitationLink: String, continueUrl: Option[String]) extends State
    case class InvitationSentBusiness(invitationLink: String, continueUrl: Option[String]) extends State
  }

  object Transitions {
    import States._

    val startJourney = Transition {
      case _ => goto(Start)
    }

    def showSelectClientType(agent: AuthorisedAgent) = Transition {
      case _ => goto(SelectClientType) // clears basket
    }

    def selectedClientType(agent: AuthorisedAgent)(clientType: ClientType) = Transition {
      case SelectClientType => goto(ClientTypeSelected(clientType))
    }

    def showSelectService(agent: AuthorisedAgent) = Transition {
      case ClientTypeSelected(ClientType.personal) =>
        goto(
          SelectPersonalService(
            Set.empty,
            if (agent.isWhitelisted) Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT) else Set(HMRCMTDIT, HMRCMTDVAT)))
      case ClientTypeSelected(ClientType.business) => goto(SelectBusinessService(Set.empty))
    }

    def selectedPersonalService(agent: AuthorisedAgent)(service: String) = Transition {
      case SelectPersonalService(basket, services) =>
        if (services.contains(service)) goto(PersonalServiceSelected(service, basket))
        else goto(SelectPersonalService(basket, services))
    }

    def selectedBusinessService(agent: AuthorisedAgent)(confirmed: Confirmation) = Transition {
      case SelectBusinessService(basket) =>
        if (confirmed.choice) goto(BusinessServiceSelected(basket))
        else goto(SelectClientType)
    }

    def showIdentifyClient(agent: AuthorisedAgent) = Transition {
      case PersonalServiceSelected(service, basket) => goto(IdentifyPersonalClient(service, basket))
      case BusinessServiceSelected(basket: Basket)  => goto(IdentifyBusinessClient(basket))
    }

    def identifiedItsaClient(agent: AuthorisedAgent)(itsaClient: ItsaClient) = Transition {
      case IdentifyPersonalClient(HMRCMTDIT, basket) =>
        goto(ItsaIdentifiedClient(itsaClient.clientIdentifier, itsaClient.postcode, basket))
    }

    def identifiedVatClient(agent: AuthorisedAgent)(vatClient: VatClient) = Transition {
      case IdentifyPersonalClient(HMRCMTDVAT, basket) =>
        goto(VatIdentifiedPersonalClient(vatClient.clientIdentifier, vatClient.registrationDate, basket))
      case IdentifyBusinessClient(basket) =>
        goto(VatIdentifiedBusinessClient(vatClient.clientIdentifier, vatClient.registrationDate, basket))
    }

    def identifyIrvClient(agent: AuthorisedAgent)(irvClient: IrvClient) = Transition {
      case IdentifyPersonalClient(HMRCPIR, basket) =>
        goto(IrvIdentifiedClient(irvClient.clientIdentifier, irvClient.dob, basket))
    }

    def showConfirmClient(getClientName: (String, String) => Future[Option[String]])(agent: AuthorisedAgent) =
      Transition {
        case ItsaIdentifiedClient(clientId, _, basket) => {
          getClientName(clientId, HMRCMTDIT).flatMap { clientNameOpt =>
            val clientName = clientNameOpt.getOrElse("")
            goto(ConfirmClientItsa(clientName, basket))
          }
        }
        case IrvIdentifiedClient(clientId, _, basket) => {
          getClientName(clientId, HMRCPIR).flatMap { clientNameOpt =>
            val clientName = clientNameOpt.getOrElse("")
            goto(ConfirmClientIrv(clientName, basket))
          }
        }
        case VatIdentifiedPersonalClient(clientId, _, basket) => {
          getClientName(clientId, HMRCMTDVAT).flatMap { clientNameOpt =>
            val clientName = clientNameOpt.getOrElse("")
            goto(ConfirmClientPersonalVat(clientName, basket))
          }
        }
        case VatIdentifiedBusinessClient(clientId, _, basket) => {
          getClientName(clientId, HMRCMTDVAT).flatMap { clientNameOpt =>
            val clientName = clientNameOpt.getOrElse("")
            goto(ConfirmClientBusinessVat(clientName, basket))
          }
        }
      }

    def clientConfirmed(authorisedAgent: AuthorisedAgent)(confirmation: Confirmation) = Transition {
      case ConfirmClientItsa(_, basket) =>
        if (confirmation.choice) goto(ClientConfirmedPersonal(basket))
        else goto(IdentifyPersonalClient(HMRCMTDIT, basket))
      case ConfirmClientIrv(_, basket) =>
        if (confirmation.choice) goto(ClientConfirmedPersonal(basket))
        else goto(IdentifyPersonalClient(HMRCPIR, basket))
      case ConfirmClientPersonalVat(_, basket) =>
        if (confirmation.choice) goto(ClientConfirmedPersonal(basket))
        else goto(IdentifyPersonalClient(HMRCMTDVAT, basket))
      case ConfirmClientBusinessVat(_, basket) =>
        if (confirmation.choice) goto(ClientConfirmedBusiness(basket))
        else goto(IdentifyBusinessClient(basket))
    }

    def showReviewAuthorisations(agent: AuthorisedAgent) =
      Transition {
        case ClientConfirmedPersonal(basket) => goto(ReviewAuthorisationsPersonal(basket))
        case ClientConfirmedBusiness(basket) => goto(ReviewAuthorisationsBusiness(basket))
      }

    def authorisationsReviewed(agent: AuthorisedAgent)(confirmation: Confirmation) = Transition {
      case ReviewAuthorisationsPersonal(basket) =>
        if (confirmation.choice) {
          goto(
            SelectPersonalService(
              Set.empty,
              if (agent.isWhitelisted) Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT) else Set(HMRCMTDIT, HMRCMTDVAT)))
        } else goto(AuthorisationsReviewedPersonal)
      case ReviewAuthorisationsBusiness(basket) =>
        if (confirmation.choice) {
          goto(SelectBusinessService(Set.empty))
        } else goto(AuthorisationsReviewedBusiness)
    }

    //Continue url is only when coming from fast
    def showInvitationSent(getAgentLink: (Arn, Option[ClientType]) => Future[String])(agent: AuthorisedAgent) =
      Transition {
        case AuthorisationsReviewedPersonal =>
          getAgentLink(agent.arn, Some(personal)).flatMap { invitationLink =>
            goto(InvitationSentPersonal(invitationLink, Some("continue-url")))
          }
        case AuthorisationsReviewedBusiness =>
          getAgentLink(agent.arn, Some(business)).flatMap { invitationLink =>
            goto(InvitationSentBusiness(invitationLink, Some("continue-url")))
          }
      }
  }

}
