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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.Errors.TransitionNotAllowed
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.State
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.States._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models.{AuthorisedAgent, ClientType}
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationJourneyModelSpec extends UnitSpec {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  case class JourneyAt(initialState: State)
      extends AgentInvitationJourneyService with TestStorage[(State, List[State])] {
    await(save((initialState, Nil)))
  }

  val authorisedAgent = AuthorisedAgent(Arn("TARN0000001"), isWhitelisted = true)

  "AgentInvitationJourneyService" when {
    "at state Start" should {
      "transition to Start given startJourney" in {
        await(JourneyAt(Start) apply startJourney) shouldBe Right((Start, Nil))
      }
      "transition to SelectClientType given showSelectClientType" in {
        await(JourneyAt(Start) apply showSelectClientType(authorisedAgent)) shouldBe Right(
          (SelectClientType, List(Start)))
      }
      "return error given selectedClientType(Personal)" in {
        await(JourneyAt(Start) apply selectedClientType(authorisedAgent)(ClientType.personal)) should matchPattern {
          case Left(TransitionNotAllowed(Start, Nil, _)) =>
        }
      }
    }
    "at state SelectClientType" should {
      "transition to Start given startJourney" in {
        await(JourneyAt(SelectClientType) apply startJourney) shouldBe Right((Start, List(SelectClientType)))
      }
      "transition to SelectClientType given showSelectClientType" in {
        await(JourneyAt(SelectClientType) apply showSelectClientType(authorisedAgent)) shouldBe Right(
          (SelectClientType, Nil))
      }
      "transition to SelectPersonalService given selectedClientType(personal)" in {
        await(JourneyAt(SelectClientType) apply selectedClientType(authorisedAgent)(ClientType.personal)) shouldBe Right(
          (SelectedClientType(ClientType.personal), List(SelectClientType)))
      }
      "transition to SelectBusinessService given selectedClientType(business)" in {
        await(JourneyAt(SelectClientType) apply selectedClientType(authorisedAgent)(ClientType.business)) shouldBe Right(
          (SelectedClientType(ClientType.business), List(SelectClientType)))
      }
    }
    "at state SelectedClientType" should {
      "transition to Start given startJourney" in {
        await(JourneyAt(SelectedClientType(ClientType.business)) apply startJourney) shouldBe Right(
          (Start, List(SelectedClientType(ClientType.business))))
      }
      "transition to SelectClientType given showSelectClientType" in {
        await(JourneyAt(SelectedClientType(ClientType.business)) apply showSelectClientType(authorisedAgent)) shouldBe Right(
          (SelectClientType, List(SelectedClientType(ClientType.business))))
      }
      "transition to SelectPersonalService with empty basket given showSelectService" in {
        await(JourneyAt(SelectedClientType(ClientType.personal)) apply showSelectService(authorisedAgent)) shouldBe Right(
          (
            SelectPersonalService(Seq.empty, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)),
            List(SelectedClientType(ClientType.personal))))
      }
      "transition to SelectBusinessService with empty basket given SelectedClientType(business)" in {
        await(JourneyAt(SelectedClientType(ClientType.business)) apply showSelectService(authorisedAgent)) shouldBe Right(
          (SelectBusinessService(Seq.empty), List(SelectedClientType(ClientType.business))))
      }
    }
  }

}
