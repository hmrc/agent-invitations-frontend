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
import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.State
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.States._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyStateFormats
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType
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
      "SelectPersonalService" in {
        Json.toJson(SelectPersonalService) shouldBe Json.obj("state" -> "SelectPersonalService")
        Json.parse("""{"state":"SelectPersonalService"}""").as[State] shouldBe SelectPersonalService
      }
      "SelectBusinessService" in {
        Json.toJson(SelectBusinessService) shouldBe Json.obj("state" -> "SelectBusinessService")
        Json.parse("""{"state":"SelectBusinessService"}""").as[State] shouldBe SelectBusinessService
      }
      "SelectService" in {
        Json.toJson(SelectService(ClientType.personal)) shouldBe Json
          .obj("state" -> "SelectService", "properties" -> Json.obj("clientType" -> "personal"))
        Json
          .parse("""{"state":"SelectService", "properties": {"clientType": "personal"}}""")
          .as[State] shouldBe SelectService(ClientType.personal)
        Json
          .parse("""{"state":"SelectService", "properties": {"clientType": "business"}}""")
          .as[State] shouldBe SelectService(ClientType.business)
      }
    }

  }

}
