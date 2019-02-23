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

  val emptyBasket: Basket = Set.empty
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
        given(Start) when showSelectClientType(authorisedAgent) should thenGo(SelectClientType(emptyBasket))
      }
      "return error given selectedClientType(Personal)" in {
        val selectedClientTypeT = selectedClientType(authorisedAgent)(ClientType.personal)
        given(Start) when selectedClientTypeT should transitionBeNotAllowed
      }
    }
    "at state SelectClientType" should {
      "transition to Start given startJourney" in {
        given(SelectClientType(emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to SelectClientType given showSelectClientType" in {
        given(SelectClientType(emptyBasket)) when showSelectClientType(authorisedAgent) should thenGo(
          SelectClientType(emptyBasket))
      }
      "transition to SelectPersonalService given selectedClientType(personal)" in {
        given(SelectClientType(emptyBasket)) when selectedClientType(authorisedAgent)(ClientType.personal) should thenGo(
          ClientTypeSelected(ClientType.personal, emptyBasket))
      }
      "transition to SelectBusinessService given selectedClientType(business)" in {
        given(SelectClientType(emptyBasket)) when selectedClientType(authorisedAgent)(ClientType.business) should thenGo(
          ClientTypeSelected(ClientType.business, emptyBasket))
      }
    }
    "at state ClientTypeSelected" should {
      "transition to Start given startJourney" in {
        given(ClientTypeSelected(ClientType.business, emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to SelectClientType given showSelectClientType" in {
        given(ClientTypeSelected(ClientType.business, emptyBasket)) when showSelectClientType(authorisedAgent) should thenGo(
          SelectClientType(emptyBasket))
      }
      "transition to SelectPersonalService with empty basket given showSelectService" in {
        given(ClientTypeSelected(ClientType.personal, emptyBasket)) when showSelectService(authorisedAgent) should thenGo(
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), emptyBasket))
      }
      "transition to SelectBusinessService with empty basket given ClientTypeSelected(business)" in {
        given(ClientTypeSelected(ClientType.business, emptyBasket)) when showSelectService(authorisedAgent) should thenGo(
          SelectBusinessService(emptyBasket))
      }
    }
    "at state SelectPersonalService" should {
      "transition to Start given startJourney" in {
        given(SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), emptyBasket)) when startJourney should thenGo(
          Start)
      }
      "transition to PersonalServiceSelected given showSelectPersonalService" in {
        await(
          given(SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), emptyBasket)) when selectedPersonalService(
            authorisedAgent)(HMRCMTDIT)) should thenGo(PersonalServiceSelected(HMRCMTDIT, emptyBasket))
      }
      "transition to SelectPersonalService when the service is invalid" in {
        await(
          given(SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), emptyBasket)) when selectedPersonalService(
            authorisedAgent)("foo")) should thenGo(
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), emptyBasket))
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
      "transition to Start given selectedBusinessService when no is selected" in {
        given(SelectBusinessService(emptyBasket)) when selectedBusinessService(authorisedAgent)(Confirmation(false)) should thenGo(
          Start)
      }
    }
    "at state PersonalServiceSelected" should {
      "transition to Start given startJourney" in {
        given(PersonalServiceSelected(HMRCMTDIT, Set.empty)) when startJourney should thenGo(Start)
      }
      "transition to IdentifyClient given showIdentifyClient for ITSA service" in {
        given(PersonalServiceSelected(HMRCMTDIT, Set.empty)) when showIdentifyClient(authorisedAgent) should thenGo(
          IdentifyPersonalClient(HMRCMTDIT, Set.empty))
      }
      "transition to IdentifyClient given showIdentifyClient for IRV service" in {
        given(PersonalServiceSelected(HMRCPIR, Set.empty)) when showIdentifyClient(authorisedAgent) should thenGo(
          IdentifyPersonalClient(HMRCPIR, Set.empty))
      }
    }
    "at state BusinessServiceSelected" should {
      "transition to Start given startJourney" in {
        given(BusinessServiceSelected(Set.empty)) when startJourney should thenGo(Start)
      }
      "transition to IdentifyClient given showIdentifyClient" in {
        given(BusinessServiceSelected(Set.empty)) when showIdentifyClient(authorisedAgent) should thenGo(
          IdentifyBusinessClient(Set.empty))
      }
    }
    "at state IdentifyPersonalClient" should {
      "transition to Start" in {
        given(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to ItsaIdentifiedClient" in {
        given(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)) when identifiedItsaClient(authorisedAgent)(
          ItsaClient("AB123456A", Some("BN114AW"))) should
          thenGo(ItsaIdentifiedClient("AB123456A", Some("BN114AW"), Set.empty))
      }
      "transition to VatIdentifiedPersonalClient" in {
        given(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket)) when identifiedVatClient(authorisedAgent)(
          VatClient("123456", Some("2010-10-10"))) should
          thenGo(VatIdentifiedPersonalClient("123456", Some("2010-10-10"), Set.empty))
      }
      "transition to IrvIdentifiedClient" in {
        given(IdentifyPersonalClient(HMRCPIR, emptyBasket)) when identifyIrvClient(authorisedAgent)(
          IrvClient("AB123456A", Some("1990-10-10"))) should
          thenGo(IrvIdentifiedClient("AB123456A", Some("1990-10-10"), Set.empty))
      }
    }
    "at state IdentifyBusinessClient" should {
      "transition to Start" in {
        given(IdentifyBusinessClient(emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to VatIdentifiedBusinessClient" in {
        given(IdentifyBusinessClient(emptyBasket)) when identifiedVatClient(authorisedAgent)(
          VatClient("123456", Some("2010-10-10"))) should
          thenGo(VatIdentifiedBusinessClient("123456", Some("2010-10-10"), Set.empty))
      }
    }
    "at ItsaIdentifiedClient" should {
      "transition to Start" in {
        given(ItsaIdentifiedClient(nino, postCode, emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to ConfirmClientItsa" in {
        def clientName(service: String, clientId: String) = Future(Some("Piglet"))

        given(ItsaIdentifiedClient(nino, postCode, emptyBasket)) when showConfirmClient(clientName)(authorisedAgent) should
          thenGo(ConfirmClientItsa("Piglet", Set.empty))
      }
    }
    "at IrvIdentifiedClient" should {
      "transition to Start" in {
        given(IrvIdentifiedClient(nino, dob, emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to ConfirmClientIrv" in {
        def clientName(service: String, clientId: String) = Future(Some("Piglet Junior"))

        given(IrvIdentifiedClient(nino, dob, emptyBasket)) when showConfirmClient(clientName)(authorisedAgent) should
          thenGo(ConfirmClientIrv("Piglet Junior", Set.empty))
      }
    }
    "at VatIdentifiedPersonalClient" should {
      "transition to Start" in {
        given(VatIdentifiedPersonalClient(vrn, vatRegDate, emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to ConfirmClientPersonalVat" in {
        def clientName(service: String, clientId: String) = Future(Some("Piglet LTD"))

        given(VatIdentifiedPersonalClient(vrn, vatRegDate, emptyBasket)) when showConfirmClient(clientName)(
          authorisedAgent) should
          thenGo(ConfirmClientPersonalVat("Piglet LTD", Set.empty))
      }
    }
    "at VatIdentifiedBusinessClient" should {
      "transition to Start" in {
        given(VatIdentifiedBusinessClient(vrn, vatRegDate, emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to ConfirmClientBusinessVat" in {
        def clientName(service: String, clientId: String) = Future(Some("Piglet LTD"))

        given(VatIdentifiedBusinessClient(vrn, vatRegDate, emptyBasket)) when showConfirmClient(clientName)(
          authorisedAgent) should
          thenGo(ConfirmClientBusinessVat("Piglet LTD", Set.empty))
      }
    }
    "at ConfirmClientItsa" should {
      "transition to Start" in {
        given(ConfirmClientItsa("Piglet", emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to ClientConfirmedPersonal" in {
        given(ConfirmClientItsa("Piglet", emptyBasket)) when clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientConfirmedPersonal(emptyBasket))
      }
      "transition to PersonalServiceSelected" in {
        given(ConfirmClientItsa("Piglet", emptyBasket)) when clientConfirmed(authorisedAgent)(Confirmation(false)) should
          thenGo(PersonalServiceSelected(HMRCMTDIT, emptyBasket))
      }
    }
    "at ConfirmClientIrv" should {
      "transition to Start" in {
        given(ConfirmClientIrv("Piglet", emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to ClientConfirmedPersonal" in {
        given(ConfirmClientIrv("Piglet", emptyBasket)) when clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientConfirmedPersonal(emptyBasket))
      }
      "transition to PersonalServiceSelected" in {
        given(ConfirmClientIrv("Piglet", emptyBasket)) when clientConfirmed(authorisedAgent)(Confirmation(false)) should
          thenGo(PersonalServiceSelected(HMRCPIR, emptyBasket))
      }
    }
    "at ConfirmClientPersonalVat" should {
      "transition to Start" in {
        given(ConfirmClientPersonalVat("Piglet", emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to ClientConfirmedPersonal" in {
        given(ConfirmClientPersonalVat("Piglet", emptyBasket)) when clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientConfirmedPersonal(emptyBasket))
      }
      "transition to PersonalServiceSelected" in {
        given(ConfirmClientPersonalVat("Piglet", emptyBasket)) when clientConfirmed(authorisedAgent)(
          Confirmation(false)) should
          thenGo(PersonalServiceSelected(HMRCMTDVAT, emptyBasket))
      }
    }
    "at ConfirmClientBusinessVat" should {
      "transition to Start" in {
        given(ConfirmClientBusinessVat("Piglet", emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to ClientConfirmedBusiness" in {
        given(ConfirmClientBusinessVat("Piglet", emptyBasket)) when clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientConfirmedBusiness(emptyBasket))
      }
      "transition to BusinessServiceSelected" in {
        given(ConfirmClientBusinessVat("Piglet", emptyBasket)) when clientConfirmed(authorisedAgent)(
          Confirmation(false)) should
          thenGo(BusinessServiceSelected(emptyBasket))
      }
    }
    "at ClientConfirmedPersonal" should {
      "transition to Start" in {
        given(ClientConfirmedPersonal(emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to ReviewAuthorisations" in {
        given(ClientConfirmedPersonal(emptyBasket)) when showReviewAuthorisations(authorisedAgent) should thenGo(
          ReviewAuthorisationsPersonal(emptyBasket))
      }
    }
    "at ClientConfirmedBusiness" should {
      "transition to Start" in {
        given(ClientConfirmedBusiness(emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to ReviewAuthorisations" in {
        given(ClientConfirmedBusiness(emptyBasket)) when showReviewAuthorisations(authorisedAgent) should thenGo(
          ReviewAuthorisationsBusiness(emptyBasket))
      }
    }
    "at ReviewAuthorisationsPersonal" should {
      "transition to Start" in {
        given(ReviewAuthorisationsPersonal(emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to ClientTypeSelected" in {
        given(ReviewAuthorisationsPersonal(emptyBasket)) when authorisationsReviewed(authorisedAgent)(
          Confirmation(true)) should
          thenGo(ClientTypeSelected(ClientType.personal, emptyBasket))
      }
      "transition to AuthorisationsReviewedPersonal" in {
        given(ReviewAuthorisationsPersonal(emptyBasket)) when authorisationsReviewed(authorisedAgent)(
          Confirmation(false)) should
          thenGo(AuthorisationsReviewedPersonal)
      }
    }
    "at ReviewAuthorisationsBusiness" should {
      "transition to Start" in {
        given(ReviewAuthorisationsBusiness(emptyBasket)) when startJourney should thenGo(Start)
      }
      "transition to ClientTypeSelected" in {
        given(ReviewAuthorisationsBusiness(emptyBasket)) when authorisationsReviewed(authorisedAgent)(
          Confirmation(true)) should
          thenGo(ClientTypeSelected(ClientType.business, emptyBasket))
      }
      "transition to AuthorisationsReviewedBusiness" in {
        given(ReviewAuthorisationsBusiness(emptyBasket)) when authorisationsReviewed(authorisedAgent)(
          Confirmation(false)) should
          thenGo(AuthorisationsReviewedBusiness)
      }
    }
    "at AuthorisationsReviewedPersonal" should {
      "transition to Start" in {
        given(AuthorisationsReviewedPersonal) when startJourney should thenGo(Start)
      }
      "transition to InvitationSentPersonal" in {
        def invitationLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

        given(AuthorisationsReviewedPersonal) when showInvitationSent(invitationLink)(authorisedAgent) should
          thenGo(InvitationSentPersonal("invitation/link", Some("continue-url")))
      }
    }
    "at AuthorisationsReviewedBusiness" should {
      "transition to Start" in {
        given(AuthorisationsReviewedBusiness) when startJourney should thenGo(Start)
      }
      "transition to InvitationSentBusiness" in {
        def invitationLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

        given(AuthorisationsReviewedBusiness) when showInvitationSent(invitationLink)(authorisedAgent) should
          thenGo(InvitationSentBusiness("invitation/link", Some("continue-url")))
      }
    }
  }

}
