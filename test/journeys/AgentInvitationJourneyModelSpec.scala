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
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AgentInvitationJourneyModelSpec extends UnitSpec with StateMatchers[Error, State] {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  case class given(initialState: State) extends AgentInvitationJourneyService with TestStorage[(State, List[State])] {
    await(save((initialState, Nil)))

    def when(transition: Transition): Either[Error, (State, List[State])] =
      await(super.apply(transition))
  }

  val emptyBasket: Basket = Nil
  val authorisedAgent = AuthorisedAgent(Arn("TARN0000001"), isWhitelisted = true)

  val nino = "AB123456A"
  val postCode = Some("BN114AW")

  val vrn = "123456"
  val vatRegDate = Some("2010-10-10")

  val dob = Some("1990-10-10")

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
      "transition to PersonalServiceSelected given showSelectPersonalService" in {
        await(
          given(SelectPersonalService(emptyBasket, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT))) when selectedPersonalService(
            authorisedAgent)(HMRCMTDIT)) should thenGo(PersonalServiceSelected(HMRCMTDIT, emptyBasket))
      }
      "transition to SelectPersonalService when the service is invalid" in {
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
    "at PersonalServiceSelected" should {
      "transition to Start given startJourney" in {
        given(PersonalServiceSelected(HMRCMTDIT, Nil)) when startJourney should thenGo(Start)
      }
      "transition to IdentifyClient given showIdentifyClient for ITSA service" in {
        given(PersonalServiceSelected(HMRCMTDIT, Nil)) when showIdentifyClient(authorisedAgent) should thenGo(
          IdentifyClient(HMRCMTDIT, Nil))
      }
      "transition to IdentifyClient given showIdentifyClient for IRV service" in {
        given(PersonalServiceSelected(HMRCPIR, Nil)) when showIdentifyClient(authorisedAgent) should thenGo(
          IdentifyClient(HMRCPIR, Nil))
      }
    }
    "at BusinessServiceSelected" should {
      "transition to Start given startJourney" in {
        given(BusinessServiceSelected(Nil)) when startJourney should thenGo(Start)
      }
      "transition to IdentifyClient given showIdentifyClient" in {
        given(BusinessServiceSelected(Nil)) when showIdentifyClient(authorisedAgent) should thenGo(
          IdentifyClient(HMRCMTDVAT, Nil))
      }
    }
    "at IdentifyClient" should {
      "transition to Start given startJourney" in {
        given(IdentifyClient(HMRCMTDIT, emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to SubmitIdentifyItsaClient given submitIdentifyItsaClient" in {
        given(IdentifyClient(HMRCMTDIT, emptyBasket)) when identifiedItsaClient(authorisedAgent)(
          ItsaClient("AB123456A", Some("BN114AW"))) should
          thenGo(ItsaIdentifiedClient("AB123456A", Some("BN114AW"), Nil))
      }
      "transition to SubmitIdentifyVatClient given submitIdentifyVatClient" in {
        given(IdentifyClient(HMRCMTDVAT, emptyBasket)) when identifiedVatClient(authorisedAgent)(
          VatClient("123456", Some("2010-10-10"))) should
          thenGo(VatIdentifiedClient("123456", Some("2010-10-10"), Nil))
      }
      "transition to SubmitIdentifyIrvClient given submitIdentifyIrvClient" in {
        given(IdentifyClient(HMRCPIR, emptyBasket)) when identifyIrvClient(authorisedAgent)(
          IrvClient("AB123456A", Some("1990-10-10"))) should
          thenGo(IrvIdentifiedClient("AB123456A", Some("1990-10-10"), Nil))
      }
    }
    "at ItsaIdentifiedClient" should {
      "transition to Start given startJourney" in {
        given(ItsaIdentifiedClient(nino, postCode, emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to ConfirmClient given ItsaIdentifiedClient" in {
        def clientName(service: String, clientId: String) = Future(Some("Piglet"))

        given(ItsaIdentifiedClient(nino, postCode, emptyBasket)) when showConfirmClient(clientName)(authorisedAgent) should
          thenGo(ConfirmClient(HMRCMTDIT, "Piglet", Nil))
      }
    }
    "at VatIdentifiedClient" should {
      "transition to Start given startJourney" in {
        given(VatIdentifiedClient(vrn, vatRegDate, emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to ConfirmClient given VatIdentifiedClient" in {
        def clientName(service: String, clientId: String) = Future(Some("Piglet LTD"))

        given(VatIdentifiedClient(vrn, vatRegDate, emptyBasket)) when showConfirmClient(clientName)(authorisedAgent) should
          thenGo(ConfirmClient(HMRCMTDVAT, "Piglet LTD", Nil))
      }
    }
    "at IrvIdentifiedClient" should {
      "transition to Start given startJourney" in {
        given(IrvIdentifiedClient(nino, dob, emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to ConfirmClient given IrvIdentifiedClient" in {
        def clientName(service: String, clientId: String) = Future(Some("Piglet Junior"))

        given(IrvIdentifiedClient(nino, dob, emptyBasket)) when showConfirmClient(clientName)(authorisedAgent) should
          thenGo(ConfirmClient(HMRCPIR, "Piglet Junior", Nil))
      }
    }
    "at ConfirmClient" should {
      "transition to Start given startJourney" in {
        given(ConfirmClient(HMRCMTDIT, "Piglet", emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to ClientConfirmed given ConfirmClient when confirmation is true" in {
        given(ConfirmClient(HMRCMTDIT, "Piglet", emptyBasket)) when clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientConfirmed(emptyBasket))
      }
      "transition to IdentifyClient given ConfirmClient when confirmation is false" in {
        given(ConfirmClient(HMRCMTDIT, "Piglet", emptyBasket)) when clientConfirmed(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyClient(HMRCMTDIT, emptyBasket))
      }
    }
  }

}
