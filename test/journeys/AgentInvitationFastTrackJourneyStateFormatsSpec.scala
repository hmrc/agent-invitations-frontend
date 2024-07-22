/*
 * Copyright 2023 HM Revenue & Customs
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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyStateFormats
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{Business, Personal}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import support.UnitSpec
import uk.gov.hmrc.agentmtdidentifiers.model.{CgtRef, Service, Utr}
import uk.gov.hmrc.domain.Nino

class AgentInvitationFastTrackJourneyStateFormatsSpec extends UnitSpec {

  implicit val formats: Format[State] = AgentInvitationFastTrackJourneyStateFormats.formats

  "AgentsInvitationJourneyFormats" should {
    "serialize and deserialize state" when {
      "Prologue" in {
        val state =
          Prologue(Some("failure/url"), Some("referer/url"))
        val json =
          Json.parse("""{"state":"Prologue","properties":{"failureUrl": "failure/url", "refererUrl": "referer/url"}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }
      "CheckDetails" in {
        val state =
          CheckDetails(
            AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("KnownFact")),
            Some("continue/url")
          )
        val json = Json.parse("""{
                                |  "state":"CheckDetails",
                                |  "properties":{
                                |    "fastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientId": "Nino|AB123456A",
                                |      "knownFact": "KnownFact"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "MissingDetail" in {
        val state =
          MissingDetail(
            AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("KnownFact")),
            Some("continue/url")
          )
        val json = Json.parse("""{
                                |  "state":"MissingDetail",
                                |  "properties":{
                                |    "fastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientId": "Nino|AB123456A",
                                |      "knownFact": "KnownFact"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "SelectClientType" in {
        val state =
          SelectClientType(AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("KnownFact")), Some("continue/url"))
        val json = Json.parse("""{
                                |  "state":"SelectClientType",
                                |  "properties":{
                                |    "fastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientId": "Nino|AB123456A",
                                |      "knownFact": "KnownFact"
                                |    },
                                |    "continueUrl": "continue/url",
                                |    "isChanging": false
                                |  }
                                |}""".stripMargin)

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "IdentifyClient" in {
        val state = IdentifyClient(
          AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("KnownFact")),
          Some("continue/url")
        )
        val json = Json.parse("""{
                                |  "state":"IdentifyClient",
                                |  "properties":{
                                |    "fastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientId": "Nino|AB123456A",
                                |      "knownFact": "KnownFact"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ConfirmClientTrust" in {
        val state = ConfirmClientTrust(
          AgentFastTrackRequest(Some(Business), Service.Trust, Utr("1977030537"), None),
          Some("continue/url"),
          "some-trust-name"
        )
        val json = Json.parse("""{
                                |  "state":"ConfirmClientTrust",
                                |  "properties":{
                                |    "fastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-TERS-ORG",
                                |      "clientId": "Utr|1977030537"
                                |    },
                                |    "continueUrl": "continue/url",
                                |    "trustName": "some-trust-name"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ConfirmClientCgt" in {
        val state = ConfirmClientCgt(
          AgentFastTrackRequest(Some(Business), Service.CapitalGains, CgtRef("XMCGTP123456789"), None),
          Some("continue/url"),
          "some-cgt-name"
        )
        val json = Json.parse("""{
                                |  "state":"ConfirmClientCgt",
                                |  "properties":{
                                |    "fastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-CGT-PD",
                                |      "clientId": "CgtRef|XMCGTP123456789"
                                |    },
                                |    "continueUrl": "continue/url",
                                |    "clientName": "some-cgt-name"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ConfirmPostcodeCgt" in {
        val state = ConfirmPostcodeCgt(
          AgentFastTrackRequest(Some(Business), Service.CapitalGains, CgtRef("XMCGTP123456789"), None),
          Some("continue/url"),
          Some("some-postcode"),
          "some-cgt-name"
        )
        val json = Json.parse("""{
                                |  "state":"ConfirmPostcodeCgt",
                                |  "properties":{
                                |    "fastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-CGT-PD",
                                |      "clientId": "CgtRef|XMCGTP123456789"
                                |    },
                                |    "continueUrl": "continue/url",
                                |    "postcodeFromDes": "some-postcode",
                                |    "clientName": "some-cgt-name"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ConfirmCountryCodeCgt" in {
        val state = ConfirmCountryCodeCgt(
          AgentFastTrackRequest(Some(Business), Service.CapitalGains, CgtRef("XMCGTP123456789"), None),
          Some("continue/url"),
          "some-countryCode",
          "some-cgt-name"
        )
        val json = Json.parse("""{
                                |  "state":"ConfirmCountryCodeCgt",
                                |  "properties":{
                                |    "fastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-CGT-PD",
                                |      "clientId": "CgtRef|XMCGTP123456789"
                                |    },
                                |    "continueUrl": "continue/url",
                                |    "countryCode": "some-countryCode",
                                |    "clientName": "some-cgt-name"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "InvitationSent" in {
        val state =
          InvitationSent(ClientType.Personal, "invitation/link", Some("continue/url"), "abc@xyz.com", Service.MtdIt, isAltItsa = Some(false))
        val json = Json.obj(
          "state" -> "InvitationSent",
          "properties" -> Json
            .obj(
              "clientType"     -> "personal",
              "invitationLink" -> "invitation/link",
              "continueUrl"    -> "continue/url",
              "agencyEmail"    -> "abc@xyz.com",
              "service"        -> "HMRC-MTD-IT",
              "isAltItsa"      -> false
            )
        )

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ClientNotFound" in {
        val state = ClientNotFound(
          AgentFastTrackRequest(Some(Business), Service.Trust, Utr("1977030537"), None),
          Some("continue/url")
        )
        val json = Json.parse("""{
                                |  "state":"ClientNotFound",
                                |  "properties":{
                                |    "fastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-TERS-ORG",
                                |      "clientId": "Utr|1977030537"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ClientNotSignedUp" in {
        val state =
          ClientNotSignedUp(AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("KnownFact")), Some("continue/url"))
        val json = Json.parse(
          """{"state":"ClientNotSignedUp","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientId": "Nino|AB123456A", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}"""
        )

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }
      "PendingInvitationExists" in {
        val state =
          PendingInvitationExists(
            AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("KnownFact")),
            "invitation link",
            "Acme Ltd",
            Some("continue/url")
          )
        val json = Json.parse("""{"state":"PendingInvitationExists","properties":{"fastTrackRequest":{"clientType": "personal",
                                |"service": "HMRC-MTD-IT", "clientId": "Nino|AB123456A",
                                | "knownFact": "KnownFact"},
                                | "agentLink": "invitation link","clientName":"Acme Ltd",
                                |  "continueUrl": "continue/url"}}""".stripMargin)

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }
      "ActiveAuthorisationExists" in {
        val state =
          ActiveAuthorisationExists(AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("KnownFact")), Some("continue/url"))
        val json = Json.parse(
          """{"state":"ActiveAuthorisationExists","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientId": "Nino|AB123456A", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}"""
        )

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "PartialAuthorisationExists" in {
        val state =
          PartialAuthorisationExists(AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("KnownFact")), Some("continue/url"))
        val json = Json.parse(
          """{"state":"PartialAuthorisationExists","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientId": "Nino|AB123456A", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}"""
        )

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ClientNotRegistered" in {
        val state =
          ClientNotRegistered(AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("KnownFact")), Some("continue/url"))
        val json = Json.parse(
          """{"state":"ClientNotRegistered","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientId": "Nino|AB123456A", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}"""
        )

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }
      "TryAgainWithoutFastTrack" in {
        val state = TryAgainWithoutFastTrack
        val json = Json.parse("""{"state":"TryAgainWithoutFastTrack"}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "SuspendedAgent" in {
        val state = SuspendedAgent(Service.MtdIt, Some("continue/url"))
        val json = Json.parse(
          """{"state": "SuspendedAgent", "properties": {"service": "HMRC-MTD-IT", "continueUrl" : "continue/url"}}"""
        )
        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }
    }
  }
}
