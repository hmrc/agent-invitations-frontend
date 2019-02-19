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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.States._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.{Basket, Error, State, Transition}
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models.{AuthorisedAgent, ClientType, Confirmation}
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationJourneyModelSpec extends UnitSpec with StateMatchers[Error, State] {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  case class given(initialState: State) extends AgentInvitationJourneyService with TestStorage[(State, List[State])] {
    await(save((initialState, Nil)))

    def when(transition: Transition): Either[Error, (State, List[State])] =
      await(super.apply(transition))
  }

  val emptyBasket: Basket = Nil
  val authorisedAgent = AuthorisedAgent(Arn("TARN0000001"), isWhitelisted = true)

  "AgentInvitationJourneyService" when {
    "at state Start" should {
      "transition to Start given startJourney" in {
        given(Start) when startJourney should thenGo(Start)
      }
      "transition to SelectClientType given showSelectClientType" in {
        given(Start) when showSelectClientType(authorisedAgent) should thenGo(SelectClientType)
      }
      "return error given selectedClientType(Personal)" in {
        val selectedClientTypeT = selectedClientType(authorisedAgent)(ClientType.personal)
        given(Start) when selectedClientTypeT should transitionBeNotAllowed
      }
    }
    "at state SelectClientType" should {
      "transition to Start given startJourney" in {
        given(SelectClientType) when startJourney should thenGo(Start)
      }
      "transition to SelectClientType given showSelectClientType" in {
        given(SelectClientType) when showSelectClientType(authorisedAgent) should thenGo(SelectClientType)
      }
      "transition to SelectPersonalService given selectedClientType(personal)" in {
        given(SelectClientType) when selectedClientType(authorisedAgent)(ClientType.personal) should thenGo(
          ClientTypeSelected(ClientType.personal))
      }
      "transition to SelectBusinessService given selectedClientType(business)" in {
        given(SelectClientType) when selectedClientType(authorisedAgent)(ClientType.business) should thenGo(
          ClientTypeSelected(ClientType.business))
      }
    }
    "at state ClientTypeSelected" should {
      "transition to Start given startJourney" in {
        given(ClientTypeSelected(ClientType.business)) when startJourney should thenGo(Start)
      }
      "transition to SelectClientType given showSelectClientType" in {
        given(ClientTypeSelected(ClientType.business)) when showSelectClientType(authorisedAgent) should thenGo(
          SelectClientType)
      }
      "transition to SelectPersonalService with empty basket given showSelectService" in {
        given(ClientTypeSelected(ClientType.personal)) when showSelectService(authorisedAgent) should thenGo(
          SelectPersonalService(emptyBasket, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)))
      }
      "transition to SelectBusinessService with empty basket given ClientTypeSelected(business)" in {
        given(ClientTypeSelected(ClientType.business)) when showSelectService(authorisedAgent) should thenGo(
          SelectBusinessService(emptyBasket))
      }
    }
    "at state SelectPersonalService" should {
      "transition to Start given startJourney" in {
        given(SelectPersonalService(emptyBasket, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT))) when startJourney should thenGo(
          Start)
      }
      "transition to SelectPersonalService given showSelectPersonalService" in {
        await(
          given(SelectPersonalService(emptyBasket, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT))) when selectedPersonalService(
            authorisedAgent)(HMRCMTDIT)) should thenGo(PersonalServiceSelected(HMRCMTDIT, emptyBasket))
      }
      "throw an InvalidService error when the service is invalid" in {
        await(
          given(SelectPersonalService(emptyBasket, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT))) when selectedPersonalService(
            authorisedAgent)("foo")) should thenGo(
          SelectPersonalService(emptyBasket, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)))
      }
    }
    "at state SelectBusinessService" should {
      "transition to Start given startJourney" in {
        given(SelectBusinessService(emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to SelectBusinessService given selectedBusinessService when yes is selected" in {
        given(SelectBusinessService(emptyBasket)) when selectedBusinessService(authorisedAgent)(Confirmation(true)) should thenGo(
          BusinessServiceSelected(emptyBasket))
      }
      "transition to SelectClientType given selectedBusinessService when no is selected" in {
        given(SelectBusinessService(emptyBasket)) when selectedBusinessService(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType)
      }
    }
  }

}
