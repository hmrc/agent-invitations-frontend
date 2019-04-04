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

import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State.{NotFoundInvitation, WarmUp}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.play.fsm.JourneyModel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ClientInvitationJourneyModel extends JourneyModel {

  sealed trait State

  val root: State = State.Start()

  /* State should contain only minimal set of data required to proceed */
  object State {
    case class Start() extends State
    case class WarmUp(clientType: ClientType, uid: String, agentName: String) extends State
    case class NotFoundInvitation() extends State
  }

  object Transitions {

    type GetAgentReferenceRecord = String => Future[Option[AgentReferenceRecord]]
    type GetAgencyName = Arn => Future[String]

    def start(clientType: String, uid: String, normalisedAgentName: String)(
      getAgentReferenceRecord: GetAgentReferenceRecord)(getAgencyName: GetAgencyName)(client: AuthorisedClient) =
      Transition {
        case _ =>
          for {
            record <- getAgentReferenceRecord(uid)
            result <- record match {
                       case Some(r) if r.normalisedAgentNames.contains(normalisedAgentName) =>
                         getAgencyName(r.arn).flatMap { name =>
                           goto(WarmUp(ClientType.toEnum(clientType), uid, name))
                         }
                       case None => goto(NotFoundInvitation())
                     }
          } yield result
      }
  }

}
