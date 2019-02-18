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
import uk.gov.hmrc.agentinvitationsfrontend.models.{AuthorisedAgent, ClientType}

object AgentInvitationJourneyModel extends JourneyModel {

  sealed trait State
  sealed trait Error

  def root: State = States.Start

  def unknownState: Error = Errors.UnknownState
  def errorFor(ex: Exception): Error = Errors.GenericError(ex)
  def transitionNotAllowed(state: State, breadcrumbs: List[State], transition: Transition): Error =
    Errors.TransitionNotAllowed(state, breadcrumbs, transition)

  object Errors {
    case object UnknownState extends Error
    case class TransitionNotAllowed(state: State, breadcrumbs: List[State], transition: Transition) extends Error
    case class GenericError(ex: Exception) extends Error
  }

  object States {
    case object Start extends State
    case object SelectClientType extends State
    case object SelectPersonalService extends State
    case object SelectBusinessService extends State
    case class SelectService(clientType: ClientType) extends State
  }

  object Transitions {
    import States._

    val startJourney = Transition {
      case _ => goto(Start)
    }

    def showSelectClientType(agent: AuthorisedAgent) = Transition {
      case _ => goto(SelectClientType)
    }

    def selectedClientType(agent: AuthorisedAgent)(clientType: ClientType) = Transition {
      case SelectClientType =>
        clientType match {
          case ClientType.personal => goto(SelectPersonalService)
          case ClientType.business => goto(SelectBusinessService)
        }
    }

  }

}
