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
import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State.{MultiConsent, NotFoundInvitation, WarmUp}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyStateFormats
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientConsent
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.personal
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId
import uk.gov.hmrc.play.test.UnitSpec

class ClientInvitationJourneyStateFormatsSpec extends UnitSpec {

  implicit val formats: Format[State] = ClientInvitationJourneyStateFormats.formats

  "ClientInvitationJourneyStateFormats" should {
    "serialize and deserialize state" when {
      "WarmUp" in {
        val state = WarmUp(personal, "uid", "agent name")
        val json = Json.parse(
          """{"state":"WarmUp","properties":{"clientType": "personal", "uid": "uid", "agentName": "agent name"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "NotFoundInvitation" in {
        val state = NotFoundInvitation
        val json = Json.parse("""{"state":"NotFoundInvitation"}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "Consent" in {
        val state = MultiConsent(
          personal,
          "uid",
          "agent name",
          Seq(
            ClientConsent(
              InvitationId("A1BEOZEO7MNO6"),
              LocalDate.parse("2010-01-01"),
              "itsa",
              consent = true,
              processed = false),
            ClientConsent(
              InvitationId("B1BEOZEO7MNO6"),
              LocalDate.parse("2010-02-02"),
              "afi",
              consent = true,
              processed = false)
          )
        )
        val json = Json.parse(
          """{"state":"Consent","properties":{"clientType": "personal", "uid": "uid", "agentName": "agent name", "consents":[
            |{
            |"invitationId": {
            |  "value": "A1BEOZEO7MNO6"
            |  },
            |"expiryDate": "2010-01-01",
            |"serviceKey": "itsa",
            |"consent": true,
            |"processed": false
            |}, {
            |"invitationId": {
            |  "value": "B1BEOZEO7MNO6"
            |  },
            |"expiryDate": "2010-02-02",
            |"serviceKey": "afi",
            |"consent": true,
            |"processed": false
            |}
            |]}}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

    }

  }

}
