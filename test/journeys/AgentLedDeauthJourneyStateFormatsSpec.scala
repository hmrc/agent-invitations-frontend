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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State.{SelectClientType, SelectServiceBusiness, SelectServicePersonal}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyStateFormats
import uk.gov.hmrc.play.test.UnitSpec

class AgentLedDeauthJourneyStateFormatsSpec extends UnitSpec {

  implicit val formats: Format[State] = AgentLedDeauthJourneyStateFormats.formats

  "AgentLedDeauthJourneyStateFormats" should {
    "serialize and deserialize state" when {

      "SelectClientType" in {
        val state = SelectClientType
        val json = Json.parse("""{"state":"SelectClientType"}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "SelectServicePersonal" in {
        val state = SelectServicePersonal(Set("approved", "services"))
        val json = Json.parse(
          """{"state":"SelectServicePersonal", "properties": {"enabledServices": ["approved", "services"]}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "SelectServiceBusiness" in {
        val state = SelectServiceBusiness
        val json = Json.parse("""{"state":"SelectServiceBusiness"}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
    }
  }
}
