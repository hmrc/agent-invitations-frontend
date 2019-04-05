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

import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.Transition
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ClientInvitationJourneyModelSpec extends UnitSpec with StateMatchers[State] {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  case class given(initialState: State) extends ClientInvitationJourneyService with TestStorage[(State, List[State])] {
    await(save((initialState, Nil)))

    def when(transition: Transition): (State, List[State]) =
      await(super.apply(transition))
  }

  val authorisedIndividualClient = AuthorisedClient("Individual", Seq(("personal", "clientId")))
  val authorisedBusinessClient = AuthorisedClient("Organisation", Seq(("business", "clientId")))
  val availableServices = Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)
  val featureFlags = FeatureFlags()

  val nino = "AB123456A"
  val postCode = Some("BN114AW")
  val vrn = "123456"
  val vatRegDate = Some("2010-10-10")
  val dob = Some("1990-10-10")

  "AgentInvitationFastTrackJourneyService" when {
    "at any state" should {
      "transition to WarmUp with agency name" in {
        def getAgentReferenceRecord(uid: String) =
          Future(
            Some(
              AgentReferenceRecord("uid", Arn("TARN0000001"), Seq("normalised%agent%name", "normalised%agent%name%2"))))
        def getAgencyName(arn: Arn) = Future("normalised agent name")

        given(Start()) when start("personal", "uid", "normalised%agent%name")(getAgentReferenceRecord)(getAgencyName)(
          authorisedIndividualClient) should
          thenGo(WarmUp(ClientType.personal, "uid", "normalised agent name"))
      }
      "transition to NotFoundInvitation when there is no agent reference record" in {
        def getAgentReferenceRecord(uid: String) =
          Future(None)
        def getAgencyName(arn: Arn) = Future("normalised agent name")

        given(Start()) when start("personal", "uid", "normalised%agent%name")(getAgentReferenceRecord)(getAgencyName)(
          authorisedIndividualClient) should
          thenGo(NotFoundInvitation)
      }
      "transition to IncorrectClientType when the affinity group does not match the client type" in {
        def getAgentReferenceRecord(uid: String) =
          Future(
            Some(
              AgentReferenceRecord("uid", Arn("TARN0000001"), Seq("normalised%agent%name", "normalised%agent%name%2"))))
        def getAgencyName(arn: Arn) = Future("normalised agent name")

        given(Start()) when start("personal", "uid", "normalised%agent%name")(getAgentReferenceRecord)(getAgencyName)(
          authorisedBusinessClient) should
          thenGo(IncorrectClientType(ClientType.personal))
      }
    }
    "at WarmUp state" should {
      "transition to Consent when the invitation is found" in {
        def getPendingInvitationIdsAndExpiryDates(uid: String, status: InvitationStatus) =
          Future(Seq(InvitationIdAndExpiryDate(InvitationId("A1BEOZEO7MNO6"), LocalDate.parse("2010-01-01"))))
        given(WarmUp(ClientType.personal, "uid", "normalised agent name")) when submitWarmUp(
          getPendingInvitationIdsAndExpiryDates)(authorisedIndividualClient)
      }
    }
  }
}
