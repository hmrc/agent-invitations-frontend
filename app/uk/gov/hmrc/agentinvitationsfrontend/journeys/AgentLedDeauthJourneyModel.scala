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
import uk.gov.hmrc.agentinvitationsfrontend.models.{AuthorisedAgent, ClientType}
import uk.gov.hmrc.play.fsm.JourneyModel

object AgentLedDeauthJourneyModel extends JourneyModel {

  sealed trait State

  override val root: State = State.SelectClientType

  object State {
    case object SelectClientType extends State
    case class SelectServicePersonal(enabledServices: Set[String]) extends State
    case object SelectServiceBusiness extends State
  }

  object Transitions {
    import State._
    import ClientType._

    def showSelectClientType(agent: AuthorisedAgent) = Transition {
      case _ => goto(SelectClientType)
    }

    def chosenClientType(agent: AuthorisedAgent)(clientType: ClientType) = Transition {
      case SelectClientType if clientType == personal => {
        val enabledPersonalServices =
          if (agent.isWhitelisted)
            Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)
          else
            Set(HMRCMTDIT, HMRCMTDVAT)
        goto(SelectServicePersonal(enabledPersonalServices))
      }
      case SelectClientType if clientType == business => goto(SelectServiceBusiness)
    }

  }

}
