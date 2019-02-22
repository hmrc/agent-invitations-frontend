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
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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

  type Basket = Seq[AuthorisationRequest]

  /* State should contain only minimal set of data required to proceed */
  object States {
    case object Start extends State
    case object SelectClientType extends State
    case class ClientTypeSelected(clientType: ClientType) extends State
    case class SelectPersonalService(basket: Basket, services: Set[String]) extends State
    case class SelectBusinessService(basket: Basket) extends State
    case class PersonalServiceSelected(service: String, basket: Basket) extends State
    case class BusinessServiceSelected(basket: Basket) extends State
    case class IdentifyClient(service: String, basket: Basket) extends State
    case class ItsaIdentifiedClient(clientIdentifier: String, postcode: Option[String], basket: Basket)
        extends State
    case class VatIdentifiedClient(
      clientIdentifier: String,
      registrationDate: Option[String],
      basket: Basket)
        extends State
    case class IrvIdentifiedClient(clientIdentifier: String, dob: Option[String], basket: Basket)
        extends State
    case class ConfirmClient(service: String,
      clientName: String,
      basket: Basket)
        extends State
    case class ClientConfirmed(basket: Basket) extends State
    case class ReviewAuthorisations() extends State
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
            Seq.empty,
            if (agent.isWhitelisted) Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT) else Set(HMRCMTDIT, HMRCMTDVAT)))
      case ClientTypeSelected(ClientType.business) => goto(SelectBusinessService(Seq.empty))
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
      case PersonalServiceSelected(service, basket) => goto(IdentifyClient(service, basket))
      case BusinessServiceSelected(basket: Basket)  => goto(IdentifyClient(HMRCMTDVAT, basket))
    }

    def identifiedItsaClient(agent: AuthorisedAgent)(itsaClient: ItsaClient) = Transition {
      case IdentifyClient(HMRCMTDIT, basket) =>
        goto(ItsaIdentifiedClient(itsaClient.clientIdentifier, itsaClient.postcode, basket))
    }

    def identifiedVatClient(agent: AuthorisedAgent)(vatClient: VatClient) = Transition {
      case IdentifyClient(HMRCMTDVAT, basket) =>
        goto(VatIdentifiedClient(vatClient.clientIdentifier, vatClient.registrationDate, basket))
    }

    def identifyIrvClient(agent: AuthorisedAgent)(irvClient: IrvClient) = Transition {
      case IdentifyClient(HMRCPIR, basket) =>
        goto(IrvIdentifiedClient(irvClient.clientIdentifier, irvClient.dob, basket))
    }

    def showConfirmClient(getClientName: (String,String) => Future[Option[String]])(agent: AuthorisedAgent) =
      Transition {
        case ItsaIdentifiedClient(clientId, postcode, basket) => {
          getClientName(clientId, HMRCMTDIT).flatMap { clientNameOpt =>
            val clientName = clientNameOpt.getOrElse("")
            goto(ConfirmClient(HMRCMTDIT, clientName, basket))
          }
        }
        case VatIdentifiedClient(clientId, registrationDate, basket) => {
          getClientName(clientId, HMRCMTDVAT).flatMap { clientNameOpt =>
            val clientName = clientNameOpt.getOrElse("")
            goto(ConfirmClient(HMRCMTDVAT, clientName, basket))
          }
        }
        case IrvIdentifiedClient(clientId, dob, basket) => {
          getClientName(clientId, HMRCPIR).flatMap { clientNameOpt =>
            val clientName = clientNameOpt.getOrElse("")
            goto(ConfirmClient(HMRCPIR, clientName, basket))
          }
        }
      }

    def clientConfirmed(authorisedAgent: AuthorisedAgent)(confirmation: Confirmation) = Transition {
      case ConfirmClient(service, _, basket) => if(confirmation.choice) goto(ClientConfirmed(basket))
      else goto(IdentifyClient(service, basket))
    }

  }

}
