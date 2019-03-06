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

package journeys

import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.States._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.{State, Transition, start}
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationFastTrackJourneyModelSpec extends UnitSpec with StateMatchers[State] {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  case class given(initialState: State) extends AgentInvitationJourneyService with TestStorage[(State, List[State])] {
    await(save((initialState, Nil)))

    def when(transition: Transition): (State, List[State]) =
      await(super.apply(transition))
  }

  val authorisedAgent = AuthorisedAgent(Arn("TARN0000001"), isWhitelisted = true)
  val availableServices = Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)

  val nino = "AB123456A"
  val postCode = Some("BN114AW")
  val vrn = "123456"
  val vatRegDate = Some("2010-10-10")
  val dob = Some("1990-10-10")

  "AgentInvitationFastTrackJourneyService" when {
    "at state CheckDetails" should {
      "transition to InvitationSent if all fields are present, no pending or active invitations and known facts match" in {
        given(SelectClientType(emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }
      "transition to SelectClientType given showSelectClientType" in {
        given(SelectClientType(emptyBasket)) when showSelectClientType(authorisedAgent) should thenGo(
          SelectClientType(emptyBasket))
      }
      "transition to SelectPersonalService" in {
        given(SelectClientType(emptyBasket)) when selectedClientType(authorisedAgent)(ClientType.personal) should thenGo(
          SelectPersonalService(availableServices, emptyBasket))
      }
      "transition to SelectBusinessService" in {
        given(SelectClientType(emptyBasket)) when selectedClientType(authorisedAgent)(ClientType.business) should thenGo(
          SelectBusinessService)
      }
    }
  }
}
