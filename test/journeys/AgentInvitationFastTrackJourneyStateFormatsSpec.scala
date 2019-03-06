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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.State
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.States._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyStateFormats
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.personal
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.HMRCMTDIT
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationFastTrackJourneyStateFormatsSpec extends UnitSpec {

  implicit val formats: Format[State] = AgentInvitationFastTrackJourneyStateFormats.formats

  "AgentsInvitationJourneyFormats" should {
    "serialize and deserialize state" when {
      "CheckDetails" in {
        val state =
          CheckDetails(AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")))
        val json = Json.parse(
          """{"state":"CheckDetails","properties":{"request":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "MoreDetails" in {
        val state =
          MoreDetails(AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")))
        val json = Json.parse(
          """{"state":"MoreDetails","properties":{"request":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "SelectClientType" in {
        val state = SelectClientType(
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")))
        val json = Json.parse(
          """{"state":"SelectClientType","properties":{"request":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "IdentifyPersonalClient" in {
        val state = IdentifyPersonalClient(HMRCMTDIT)
        val json = Json.obj(
          "state"      -> "IdentifyPersonalClient",
          "properties" -> Json.obj("service" -> "HMRC-MTD-IT")
        )

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "IdentifyBusinessClient" in {
        val state = IdentifyBusinessClient
        val json = Json
          .parse("""{"state":"IdentifyBusinessClient"}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "InvitationSentPersonal" in {
        val state = InvitationSentPersonal("invitation/link", Some("continue/url"))
        val json = Json.obj(
          "state"      -> "InvitationSentPersonal",
          "properties" -> Json.obj("invitationLink" -> "invitation/link", "continueUrl" -> "continue/url")
        )

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "InvitationSentBusiness" in {
        val state = InvitationSentBusiness("invitation/link", Some("continue/url"))
        val json = Json.obj(
          "state"      -> "InvitationSentBusiness",
          "properties" -> Json.obj("invitationLink" -> "invitation/link", "continueUrl" -> "continue/url")
        )

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "KnownFactNotMatched" in {
        val state =
          KnownFactNotMatched(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")))
        val json = Json.parse(
          """{"state":"KnownFactNotMatched","properties":{"request":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "ClientNotSignedUp" in {
        val state =
          ClientNotSignedUp(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")))
        val json = Json.parse(
          """{"state":"ClientNotSignedUp","properties":{"request":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "PendingInvitationExists" in {
        val state =
          PendingInvitationExists(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")))
        val json = Json.parse(
          """{"state":"PendingInvitationExists","properties":{"request":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "ActiveAuthorisationExists" in {
        val state =
          ActiveAuthorisationExists(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")))
        val json = Json.parse(
          """{"state":"ActiveAuthorisationExists","properties":{"request":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
    }
  }
}
