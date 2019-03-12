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
          CheckDetailsCompleteItsa(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url"))
        val json = Json.parse(
          """{"state":"CheckDetails","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueURL": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "MoreDetails" in {
        val state =
          MoreDetails(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url"))
        val json = Json.parse(
          """{"state":"MoreDetails","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueURL": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "SelectClientType" in {
        val state = SelectClientType(
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
          Some("continue/url"))
        val json = Json.parse(
          """{"state":"SelectClientType","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueURL": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "IdentifyPersonalClient" in {
        val state = IdentifyPersonalClient(
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
          Some("continue/url"))
        val json = Json.parse(
          """{"state":"IdentifyPersonalClient","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueURL": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "IdentifyBusinessClient" in {
        val state = IdentifyBusinessClient(
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
          Some("continue/url"))
        val json = Json.parse(
          """{"state":"IdentifyBusinessClient","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueURL": "continue/url"}}""")

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
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url"))
        val json = Json.parse(
          """{"state":"KnownFactNotMatched","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "ClientNotSignedUp" in {
        val state =
          ClientNotSignedUp(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url"))
        val json = Json.parse(
          """{"state":"ClientNotSignedUp","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "PendingInvitationExists" in {
        val state =
          PendingInvitationExists(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url"))
        val json = Json.parse(
          """{"state":"PendingInvitationExists","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueURL": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "ActiveAuthorisationExists" in {
        val state =
          ActiveAuthorisationExists(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url"))
        val json = Json.parse(
          """{"state":"ActiveAuthorisationExists","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
    }
  }
}
