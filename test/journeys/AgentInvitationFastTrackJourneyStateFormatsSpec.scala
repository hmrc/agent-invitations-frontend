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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyStateFormats
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationFastTrackJourneyStateFormatsSpec extends UnitSpec {

  implicit val formats: Format[State] = AgentInvitationFastTrackJourneyStateFormats.formats

  "AgentsInvitationJourneyFormats" should {
    "serialize and deserialize state" when {
      "Prologue" in {
        val state =
          Prologue(Some("failure/url"))
        val json = Json.parse("""{"state":"Prologue","properties":{"failureUrl": "failure/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "CheckDetailsCompleteItsa" in {
        val state =
          CheckDetailsCompleteItsa(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url"))
        val json = Json.parse(
          """{"state":"CheckDetailsCompleteItsa","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "CheckDetailsCompleteIrv" in {
        val state =
          CheckDetailsCompleteIrv(
            AgentFastTrackRequest(Some(personal), HMRCPIR, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url"))
        val json = Json.parse(
          """{"state":"CheckDetailsCompleteIrv","properties":{"fastTrackRequest":{"clientType": "personal","service": "PERSONAL-INCOME-RECORD", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "CheckDetailsCompletePersonalVat" in {
        val state =
          CheckDetailsCompletePersonalVat(
            AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url"))
        val json = Json.parse(
          """{"state":"CheckDetailsCompletePersonalVat","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-VAT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "CheckDetailsCompleteBusinessVat" in {
        val state =
          CheckDetailsCompleteBusinessVat(
            AgentFastTrackRequest(Some(business), HMRCMTDVAT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url"))
        val json = Json.parse(
          """{"state":"CheckDetailsCompleteBusinessVat","properties":{"fastTrackRequest":{"clientType": "business","service": "HMRC-MTD-VAT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "CheckDetailsNoPostcode" in {
        val state =
          CheckDetailsNoPostcode(
            AgentFastTrackRequest(Some(business), HMRCMTDVAT, "ClientIdType", "ClientId", None),
            Some("continue/url"))
        val json = Json.parse(
          """{"state":"CheckDetailsNoPostcode","properties":{"fastTrackRequest":{"clientType": "business","service": "HMRC-MTD-VAT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId"}, "continueUrl": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "CheckDetailsNoClientTypeVat" in {
        val state =
          CheckDetailsNoClientTypeVat(
            AgentFastTrackRequest(None, HMRCMTDVAT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url"))
        val json = Json.parse(
          """{"state":"CheckDetailsNoClientTypeVat","properties":{"fastTrackRequest":{"service": "HMRC-MTD-VAT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "NoPostcode" in {
        val state =
          NoPostcode(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url"))
        val json = Json.parse(
          """{"state":"NoPostcode","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "NoDob" in {
        val state =
          NoDob(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url"))
        val json = Json.parse(
          """{"state":"NoDob","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "NoVatRegDate" in {
        val state =
          NoVatRegDate(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url"))
        val json = Json.parse(
          """{"state":"NoVatRegDate","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "SelectClientTypeVat" in {
        val state = SelectClientTypeVat(
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
          Some("continue/url"))
        val json = Json.parse(
          """{"state":"SelectClientTypeVat","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "IdentifyPersonalClient" in {
        val state = IdentifyPersonalClient(
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
          Some("continue/url"))
        val json = Json.parse(
          """{"state":"IdentifyPersonalClient","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "IdentifyBusinessClient" in {
        val state = IdentifyBusinessClient(
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
          Some("continue/url"))
        val json = Json.parse(
          """{"state":"IdentifyBusinessClient","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "InvitationSentPersonal" in {
        val state = InvitationSentPersonal("invitation/link", Some("continue/url"), "abc@xyz.com")
        val json = Json.obj(
          "state"      -> "InvitationSentPersonal",
          "properties" -> Json.obj("invitationLink" -> "invitation/link", "continueUrl" -> "continue/url")
        )

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "InvitationSentBusiness" in {
        val state = InvitationSentBusiness("invitation/link", Some("continue/url"), "abc@xyz.com")
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
          """{"state":"PendingInvitationExists","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}""")

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
