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

object AgentInvitationJourneyModel extends JourneyModel {

  sealed trait State
  sealed trait Error

  def root: State = States.Start

  def errorFor(ex: Exception): Error = Errors.GenericError(ex)

  object States {

    case object UnknownState extends State
    case object Start extends State
    case object SelectClientType extends State
    case class SelectService(clientType: String) extends State

  }

  object Errors {

    case class GenericError(ex: Exception) extends Error
  }

  object Transitions {
    import States._

    val startJourney = Transition {
      case _ => goto(Start)
    }

  }

}
