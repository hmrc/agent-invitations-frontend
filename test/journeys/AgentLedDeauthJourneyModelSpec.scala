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

import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel._
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global

class AgentLedDeauthJourneyModelSpec extends UnitSpec with StateMatchers[State] {
  import Services._

  implicit val hc: HeaderCarrier = HeaderCarrier()

  case class given(initialState: State) extends AgentLedDeauthJourneyService with TestStorage[(State, List[State])] {
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

  "AgentLedDeauthJourneyModel" when {
    "at state ClientType" should {
      "transition to SelectServicePersonal when personal is selected" in {
        given(SelectClientType) when chosenClientType(authorisedAgent)(ClientType.personal) should thenGo(
          SelectServicePersonal(availableServices))
      }
      "transition to SelectServiceBusiness when business is selected" in {
        given(SelectClientType) when chosenClientType(authorisedAgent)(ClientType.business) should thenGo(
          SelectServiceBusiness)
      }
    }
    "at state SelectServicePersonal" should {
      "transition to IdentifyClientPersonal when service is ITSA" in {
        given(SelectServicePersonal(availableServices)) when chosenPersonalService(authorisedAgent)(HMRCMTDIT) should thenGo(
          IdentifyClientPersonal(HMRCMTDIT)
        )
      }
      "transition to IdentifyClientPersonal when service is PIR" in {
        given(SelectServicePersonal(availableServices)) when chosenPersonalService(authorisedAgent)(HMRCPIR) should thenGo(
          IdentifyClientPersonal(HMRCPIR)
        )
      }
      "transition to IdentifyClientPersonal when service is VAT" in {
        given(SelectServicePersonal(availableServices)) when chosenPersonalService(authorisedAgent)(HMRCMTDVAT) should thenGo(
          IdentifyClientPersonal(HMRCMTDVAT)
        )
      }
    }
    "at state SelectServiceBusiness" should {
      "transition to IdentifyClientBusiness when YES is selected" in {
        given(SelectServiceBusiness) when chosenBusinessService(authorisedAgent)(Confirmation(true)) should thenGo(
          IdentifyClientBusiness
        )
      }
      "transition to ClientType when NO is selected" in {
        given(SelectServiceBusiness) when chosenBusinessService(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType
        )
      }
    }
  }
}
