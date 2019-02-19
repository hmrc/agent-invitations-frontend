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
import play.api.libs.json.{Format, JsArray, Json}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.State
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.States._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyStateFormats
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationJourneyStateFormatsSpec extends UnitSpec {

  implicit val formats: Format[State] = AgentInvitationJourneyStateFormats.formats

  "AgentsInvitationJourneyFormats" should {
    "serialize and deserialize state" when {
      "Start" in {
        Json.toJson(Start) shouldBe Json.obj("state" -> "Start")
        Json.parse("""{"state":"Start"}""").as[State] shouldBe Start
      }
      "SelectClientType" in {
        Json.toJson(SelectClientType) shouldBe Json.obj("state" -> "SelectClientType")
        Json.parse("""{"state":"SelectClientType"}""").as[State] shouldBe SelectClientType
      }
      "ClientTypeSelected" in {
        Json.toJson(ClientTypeSelected(ClientType.personal)) shouldBe Json
          .obj("state" -> "ClientTypeSelected", "properties" -> Json.obj("clientType" -> "personal"))
        Json
          .parse("""{"state":"ClientTypeSelected", "properties": {"clientType": "personal"}}""")
          .as[State] shouldBe ClientTypeSelected(ClientType.personal)
        Json
          .parse("""{"state":"ClientTypeSelected", "properties": {"clientType": "business"}}""")
          .as[State] shouldBe ClientTypeSelected(ClientType.business)
      }
      "SelectPersonalService" in {
        Json.toJson(SelectPersonalService(Seq.empty, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT))) shouldBe Json.obj(
          "state" -> "SelectPersonalService",
          "properties" -> Json
            .obj("basket" -> JsArray(), "services" -> Json.arr("PERSONAL-INCOME-RECORD", "HMRC-MTD-IT", "HMRC-MTD-VAT"))
        )
        Json
          .parse(
            """{"state":"SelectPersonalService", "properties": {"basket": [], "services": ["PERSONAL-INCOME-RECORD", "HMRC-MTD-IT", "HMRC-MTD-VAT"]}}""")
          .as[State] shouldBe SelectPersonalService(Seq.empty, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT))
      }
      "SelectBusinessService" in {
        Json.toJson(SelectBusinessService(Seq.empty)) shouldBe Json
          .obj("state" -> "SelectBusinessService", "properties" -> Json.obj("basket" -> JsArray()))
        Json
          .parse("""{"state":"SelectBusinessService", "properties": {"basket": []}}""")
          .as[State] shouldBe SelectBusinessService(Seq.empty)
      }
      "PersonalServiceSelected" in {
        Json.toJson(PersonalServiceSelected(HMRCMTDIT, Nil)) shouldBe Json.obj(
          "state"      -> "PersonalServiceSelected",
          "properties" -> Json.obj("service" -> "HMRC-MTD-IT", "basket" -> JsArray())
        )
        Json
          .parse("""{"state":"PersonalServiceSelected", "properties": {"basket": [], "service": "HMRC-MTD-IT"}}""")
          .as[State] shouldBe PersonalServiceSelected(HMRCMTDIT, Nil)
      }

      "BusinessServiceSelected" in {
        Json.toJson(BusinessServiceSelected(Nil)) shouldBe Json.obj(
          "state"      -> "BusinessServiceSelected",
          "properties" -> Json.obj("basket" -> JsArray())
        )
        Json
          .parse("""{"state":"BusinessServiceSelected", "properties": {"basket": []}}""")
          .as[State] shouldBe BusinessServiceSelected(Seq.empty)
      }
    }

  }

}
