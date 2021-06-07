/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCCGTPD, HMRCMTDIT, HMRCMTDVAT, HMRCPIR, TAXABLETRUST}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationFastTrackJourneyStateFormatsSpec extends UnitSpec {

  implicit val formats: Format[State] = AgentInvitationFastTrackJourneyStateFormats.formats

  "AgentsInvitationJourneyFormats" should {
    "serialize and deserialize state" when {
      "Prologue" in {
        val state =
          Prologue(Some("failure/url"), Some("referer/url"))
        val json =
          Json.parse("""{"state":"Prologue","properties":{"failureUrl": "failure/url", "refererUrl": "referer/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "CheckDetailsCompleteItsa" in {
        val state =
          CheckDetailsCompleteItsa(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "OriginalClientIdType", "OriginalClientId", Some("OriginalKnownFact")),
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url")
          )
        val json = Json.parse("""{
                                |  "state":"CheckDetailsCompleteItsa",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId",
                                |      "knownFact": "OriginalKnownFact"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId",
                                |      "knownFact": "KnownFact"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "CheckDetailsCompleteIrv" in {
        val state =
          CheckDetailsCompleteIrv(
            AgentFastTrackRequest(Some(personal), HMRCPIR, "OriginalClientIdType", "OriginalClientId", Some("OriginalKnownFact")),
            AgentFastTrackRequest(Some(personal), HMRCPIR, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url")
          )
        val json = Json.parse("""{
                                |  "state":"CheckDetailsCompleteIrv",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "PERSONAL-INCOME-RECORD",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId",
                                |      "knownFact": "OriginalKnownFact"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "PERSONAL-INCOME-RECORD",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId",
                                |      "knownFact": "KnownFact"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "CheckDetailsCompletePersonalVat" in {
        val state =
          CheckDetailsCompletePersonalVat(
            AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "OriginalClientIdType", "OriginalClientId", Some("OriginalKnownFact")),
            AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url")
          )
        val json = Json.parse("""{
                                |  "state":"CheckDetailsCompletePersonalVat",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-VAT",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId",
                                |      "knownFact": "OriginalKnownFact"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-VAT",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId",
                                |      "knownFact": "KnownFact"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "CheckDetailsCompleteBusinessVat" in {
        val state =
          CheckDetailsCompleteBusinessVat(
            AgentFastTrackRequest(Some(business), HMRCMTDVAT, "OriginalClientIdType", "OriginalClientId", Some("OriginalKnownFact")),
            AgentFastTrackRequest(Some(business), HMRCMTDVAT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url")
          )
        val json = Json.parse("""{
                                |  "state":"CheckDetailsCompleteBusinessVat",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-MTD-VAT",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId",
                                |      "knownFact": "OriginalKnownFact"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-MTD-VAT",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId",
                                |      "knownFact": "KnownFact"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "CheckDetailsCompleteTrust" in {
        val state =
          CheckDetailsCompleteTrust(
            AgentFastTrackRequest(Some(business), TAXABLETRUST, "OriginalClientIdType", "OriginalClientId", None),
            AgentFastTrackRequest(Some(business), TAXABLETRUST, "ClientIdType", "ClientId", None),
            Some("continue/url")
          )
        val json = Json.parse("""{
                                |  "state":"CheckDetailsCompleteTrust",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-TERS-ORG",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-TERS-ORG",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "CheckDetailsCompleteCgt" in {
        val state =
          CheckDetailsCompleteCgt(
            AgentFastTrackRequest(Some(business), HMRCCGTPD, "OriginalClientIdType", "OriginalClientId", None),
            AgentFastTrackRequest(Some(business), HMRCCGTPD, "ClientIdType", "ClientId", None),
            Some("continue/url")
          )
        val json = Json.parse("""{
                                |  "state":"CheckDetailsCompleteCgt",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-CGT-PD",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-CGT-PD",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "CheckDetailsNoPostcode" in {
        val state =
          CheckDetailsNoPostcode(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "OriginalClientIdType", "OriginalClientId", None),
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", None),
            Some("continue/url")
          )
        val json = Json.parse("""{
                                |  "state":"CheckDetailsNoPostcode",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "CheckDetailsNoDob" in {
        val state =
          CheckDetailsNoDob(
            AgentFastTrackRequest(Some(personal), HMRCPIR, "OriginalClientIdType", "OriginalClientId", None),
            AgentFastTrackRequest(Some(personal), HMRCPIR, "ClientIdType", "ClientId", None),
            Some("continue/url")
          )
        val json = Json.parse("""{
                                |  "state":"CheckDetailsNoDob",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "PERSONAL-INCOME-RECORD",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "PERSONAL-INCOME-RECORD",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "CheckDetailsNoVatRegDate" in {
        val state =
          CheckDetailsNoVatRegDate(
            AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "OriginalClientIdType", "OriginalClientId", None),
            AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ClientIdType", "ClientId", None),
            Some("continue/url")
          )
        val json = Json.parse("""{
                                |  "state":"CheckDetailsNoVatRegDate",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-VAT",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-VAT",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "CheckDetailsNoClientTypeVat" in {
        val state =
          CheckDetailsNoClientTypeVat(
            AgentFastTrackRequest(None, HMRCMTDVAT, "OriginalClientIdType", "OriginalClientId", Some("OriginalKnownFact")),
            AgentFastTrackRequest(None, HMRCMTDVAT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url")
          )
        val json = Json.parse("""{
                                |  "state":"CheckDetailsNoClientTypeVat",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "service": "HMRC-MTD-VAT",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId",
                                |      "knownFact": "OriginalKnownFact"
                                |    },
                                |    "fastTrackRequest":{
                                |      "service": "HMRC-MTD-VAT",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId",
                                |      "knownFact": "KnownFact"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "NoPostcode" in {
        val state =
          NoPostcode(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "OriginalClientIdType", "OriginalClientId", Some("OriginalKnownFact")),
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url")
          )
        val json = Json.parse("""{
                                |  "state":"NoPostcode",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId",
                                |      "knownFact": "OriginalKnownFact"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId",
                                |      "knownFact": "KnownFact"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "NoDob" in {
        val state =
          NoDob(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "OriginalClientIdType", "OriginalClientId", Some("OriginalKnownFact")),
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url")
          )
        val json = Json.parse("""{
                                |  "state":"NoDob",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId",
                                |      "knownFact": "OriginalKnownFact"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId",
                                |      "knownFact": "KnownFact"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "NoVatRegDate" in {
        val state =
          NoVatRegDate(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "OriginalClientIdType", "OriginalClientId", Some("OriginalKnownFact")),
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url")
          )
        val json = Json.parse("""{
                                |  "state":"NoVatRegDate",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId",
                                |      "knownFact": "OriginalKnownFact"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId",
                                |      "knownFact": "KnownFact"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "SelectClientTypeVat" in {
        val state = SelectClientTypeVat(
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "OriginalClientIdType", "OriginalClientId", Some("OriginalKnownFact")),
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
          Some("continue/url")
        )
        val json = Json.parse("""{
                                |  "state":"SelectClientTypeVat",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId",
                                |      "knownFact": "OriginalKnownFact"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId",
                                |      "knownFact": "KnownFact"
                                |    },
                                |    "continueUrl": "continue/url",
                                |    "isChanging": false
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "SelectClientTypeCgt" in {
        val state = SelectClientTypeCgt(
          AgentFastTrackRequest(Some(personal), HMRCCGTPD, "OriginalClientIdType", "OriginalClientId", Some("OriginalKnownFact")),
          AgentFastTrackRequest(Some(personal), HMRCCGTPD, "ClientIdType", "ClientId", Some("KnownFact")),
          Some("continue/url")
        )
        val json = Json.parse("""{
                                |  "state":"SelectClientTypeCgt",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-CGT-PD",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId",
                                |      "knownFact": "OriginalKnownFact"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-CGT-PD",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId",
                                |      "knownFact": "KnownFact"
                                |    },
                                |    "continueUrl": "continue/url",
                                |    "isChanging": false
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "IdentifyPersonalClient" in {
        val state = IdentifyPersonalClient(
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "OriginalClientIdType", "OriginalClientId", Some("OriginalKnownFact")),
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
          Some("continue/url")
        )
        val json = Json.parse("""{
                                |  "state":"IdentifyPersonalClient",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId",
                                |      "knownFact": "OriginalKnownFact"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId",
                                |      "knownFact": "KnownFact"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "IdentifyBusinessClient" in {
        val state = IdentifyBusinessClient(
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "OriginalClientIdType", "OriginalClientId", Some("OriginalKnownFact")),
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
          Some("continue/url")
        )
        val json = Json.parse("""{
                                |  "state":"IdentifyBusinessClient",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId",
                                |      "knownFact": "OriginalKnownFact"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId",
                                |      "knownFact": "KnownFact"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "IdentifyTrustClient" in {
        val state = IdentifyTrustClient(
          AgentFastTrackRequest(Some(business), TAXABLETRUST, "OriginalClientIdType", "OriginalClientId", None),
          AgentFastTrackRequest(Some(business), TAXABLETRUST, "ClientIdType", "ClientId", None),
          Some("continue/url")
        )
        val json = Json.parse("""{
                                |  "state":"IdentifyTrustClient",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-TERS-ORG",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-TERS-ORG",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "IdentifyCgtClient" in {
        val state = IdentifyCgtClient(
          AgentFastTrackRequest(Some(business), HMRCCGTPD, "OriginalClientIdType", "OriginalClientId", None),
          AgentFastTrackRequest(Some(business), HMRCCGTPD, "ClientIdType", "ClientId", None),
          Some("continue/url")
        )
        val json = Json.parse("""{
                                |  "state":"IdentifyCgtClient",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-CGT-PD",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-CGT-PD",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "IdentifyNoClientTypeClient" in {
        val state = IdentifyNoClientTypeClient(
          AgentFastTrackRequest(Some(business), TAXABLETRUST, "OriginalClientIdType", "OriginalClientId", None),
          AgentFastTrackRequest(Some(business), TAXABLETRUST, "ClientIdType", "ClientId", None),
          Some("continue/url")
        )
        val json = Json.parse("""{
                                |  "state":"IdentifyNoClientTypeClient",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-TERS-ORG",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-TERS-ORG",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "ConfirmClientTrust" in {
        val state = ConfirmClientTrust(
          AgentFastTrackRequest(Some(business), TAXABLETRUST, "OriginalClientIdType", "OriginalClientId", None),
          AgentFastTrackRequest(Some(business), TAXABLETRUST, "ClientIdType", "ClientId", None),
          Some("continue/url"),
          "some-trust-name"
        )
        val json = Json.parse("""{
                                |  "state":"ConfirmClientTrust",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-TERS-ORG",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-TERS-ORG",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId"
                                |    },
                                |    "continueUrl": "continue/url",
                                |    "trustName": "some-trust-name"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "ConfirmClientCgt" in {
        val state = ConfirmClientCgt(
          AgentFastTrackRequest(Some(business), HMRCCGTPD, "OriginalClientIdType", "OriginalClientId", None),
          AgentFastTrackRequest(Some(business), HMRCCGTPD, "ClientIdType", "ClientId", None),
          Some("continue/url"),
          "some-cgt-name"
        )
        val json = Json.parse("""{
                                |  "state":"ConfirmClientCgt",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-CGT-PD",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-CGT-PD",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId"
                                |    },
                                |    "continueUrl": "continue/url",
                                |    "clientName": "some-cgt-name"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "ConfirmPostcodeCgt" in {
        val state = ConfirmPostcodeCgt(
          AgentFastTrackRequest(Some(business), HMRCCGTPD, "OriginalClientIdType", "OriginalClientId", None),
          AgentFastTrackRequest(Some(business), HMRCCGTPD, "ClientIdType", "ClientId", None),
          Some("continue/url"),
          Some("some-postcode"),
          "some-cgt-name"
        )
        val json = Json.parse("""{
                                |  "state":"ConfirmPostcodeCgt",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-CGT-PD",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-CGT-PD",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId"
                                |    },
                                |    "continueUrl": "continue/url",
                                |    "postcodeFromDes": "some-postcode",
                                |    "clientName": "some-cgt-name"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "ConfirmCountryCodeCgt" in {
        val state = ConfirmCountryCodeCgt(
          AgentFastTrackRequest(Some(business), HMRCCGTPD, "OriginalClientIdType", "OriginalClientId", None),
          AgentFastTrackRequest(Some(business), HMRCCGTPD, "ClientIdType", "ClientId", None),
          Some("continue/url"),
          "some-countryCode",
          "some-cgt-name"
        )
        val json = Json.parse("""{
                                |  "state":"ConfirmCountryCodeCgt",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-CGT-PD",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-CGT-PD",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId"
                                |    },
                                |    "continueUrl": "continue/url",
                                |    "countryCode": "some-countryCode",
                                |    "clientName": "some-cgt-name"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "InvitationSentPersonal" in {
        val state = InvitationSentPersonal("invitation/link", Some("continue/url"), "abc@xyz.com", HMRCMTDIT, isAltItsa = false)
        val json = Json.obj(
          "state" -> "InvitationSentPersonal",
          "properties" -> Json
            .obj(
              "invitationLink" -> "invitation/link",
              "continueUrl"    -> "continue/url",
              "agencyEmail"    -> "abc@xyz.com",
              "service"        -> "HMRC-MTD-IT",
              "isAltItsa"      -> false)
        )

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "InvitationSentBusiness" in {
        val state = InvitationSentBusiness("invitation/link", Some("continue/url"), "abc@xyz.com")
        val json = Json.obj(
          "state" -> "InvitationSentBusiness",
          "properties" -> Json
            .obj("invitationLink" -> "invitation/link", "continueUrl" -> "continue/url", "agencyEmail" -> "abc@xyz.com", "service" -> "HMRC-MTD-VAT")
        )

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "KnownFactNotMatched" in {
        val state =
          KnownFactNotMatched(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "OriginalClientIdType", "OriginalClientId", Some("OriginalKnownFact")),
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
            Some("continue/url")
          )
        val json = Json.parse("""{
                                |  "state":"KnownFactNotMatched",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId",
                                |      "knownFact": "OriginalKnownFact"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "personal",
                                |      "service": "HMRC-MTD-IT",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId",
                                |      "knownFact": "KnownFact"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "TrustNotFound" in {
        val state = TrustNotFound(
          AgentFastTrackRequest(Some(business), TAXABLETRUST, "OriginalClientIdType", "OriginalClientId", None),
          AgentFastTrackRequest(Some(business), TAXABLETRUST, "ClientIdType", "ClientId", None),
          Some("continue/url")
        )
        val json = Json.parse("""{
                                |  "state":"TrustNotFound",
                                |  "properties":{
                                |    "originalFastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-TERS-ORG",
                                |      "clientIdentifierType": "OriginalClientIdType",
                                |      "clientIdentifier": "OriginalClientId"
                                |    },
                                |    "fastTrackRequest":{
                                |      "clientType": "business",
                                |      "service": "HMRC-TERS-ORG",
                                |      "clientIdentifierType": "ClientIdType",
                                |      "clientIdentifier": "ClientId"
                                |    },
                                |    "continueUrl": "continue/url"
                                |  }
                                |}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "ClientNotSignedUp" in {
        val state =
          ClientNotSignedUp(AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")), Some("continue/url"))
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

      "PartialAuthorisationExists" in {
        val state = PartialAuthorisationExists(
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")),
          Some("continue/url"))
        val json = Json.parse(
          """{"state":"PartialAuthorisationExists","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "ClientNotRegistered" in {
        val state =
          ClientNotRegistered(AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ClientIdType", "ClientId", Some("KnownFact")), Some("continue/url"))
        val json = Json.parse(
          """{"state":"ClientNotRegistered","properties":{"fastTrackRequest":{"clientType": "personal","service": "HMRC-MTD-IT", "clientIdentifierType": "ClientIdType", "clientIdentifier": "ClientId", "knownFact": "KnownFact"}, "continueUrl": "continue/url"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "TryAgainWithoutFastTrack" in {
        val state = TryAgainWithoutFastTrack
        val json = Json.parse("""{"state":"TryAgainWithoutFastTrack"}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "SuspendedAgent" in {
        val state = SuspendedAgent(HMRCMTDIT, Some("continue/url"))
        val json = Json.parse(
          """{"state": "SuspendedAgent", "properties": {"service": "HMRC-MTD-IT", "continueUrl" : "continue/url"}}"""
        )
        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
    }
  }
}
