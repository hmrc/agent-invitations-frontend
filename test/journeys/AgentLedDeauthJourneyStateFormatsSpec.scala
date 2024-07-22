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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyStateFormats
import uk.gov.hmrc.agentmtdidentifiers.model.{CgtRef, Service}
import uk.gov.hmrc.domain.Nino
import support.UnitSpec
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType

class AgentLedDeauthJourneyStateFormatsSpec extends UnitSpec {

  implicit val formats: Format[State] = AgentLedDeauthJourneyStateFormats.formats
  val cgtRef: CgtRef = CgtRef("XMCGTP123456789")

  "AgentLedDeauthJourneyStateFormats" should {
    "serialize and deserialize state" when {

      "SelectClientType" in {
        val state = SelectClientType
        val json = Json.parse("""{"state":"SelectClientType"}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }
      "SelectService" in {
        val state = SelectService(ClientType.Personal, Set(Service.MtdIt, Service.Vat))
        val json =
          Json.parse("""{"state":"SelectService", "properties": {"clientType": "personal", "availableServices": ["HMRC-MTD-IT", "HMRC-MTD-VAT"]}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ConfirmPostcodeCgt" in {
        val state = ConfirmPostcodeCgt(ClientType.Personal, cgtRef, Some("BN13 1FN"), "some-client")
        val json = Json.parse(
          """{"state":"ConfirmPostcodeCgt", "properties": {"clientType": "personal", "cgtRef": "XMCGTP123456789", "postcode": "BN13 1FN", "clientName": "some-client"}}"""
        )

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ConfirmCountryCodeCgt" in {
        val state = ConfirmCountryCodeCgt(ClientType.Personal, cgtRef, "GB", "some-client")
        val json = Json.parse(
          """{"state":"ConfirmCountryCodeCgt", "properties": {"clientType": "personal", "cgtRef": "XMCGTP123456789", "countryCode": "GB", "clientName": "some-client"}}"""
        )

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "IdentifyClient" in {
        val state = IdentifyClient(ClientType.Personal, Service.MtdIt)
        val json = Json.parse("""{"state":"IdentifyClient", "properties": {"clientType": "personal", "service": "HMRC-MTD-IT"}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ConfirmClient" in {
        val state = ConfirmClient(ClientType.Personal, Service.MtdIt, Some("Cersei not KEEPing well"), Nino("AB123456A"))
        val json = Json.parse(
          """{"state":"ConfirmClient","properties": {"clientType": "personal", "service": "HMRC-MTD-IT", "clientName":"Cersei not KEEPing well","clientId": "Nino|AB123456A"}}"""
        )

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ConfirmCancel" in {
        val state = ConfirmCancel(Service.MtdIt, Some("Cersei not KEEPing well"), "AB123456A")
        val json = Json.parse(
          """{"state":"ConfirmCancel","properties": {"service": "HMRC-MTD-IT", "clientName":"Cersei not KEEPing well","clientId": "AB123456A", "isPartialAuth": false}}"""
        )

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }
      "AuthorisationCancelled" in {
        val state = AuthorisationCancelled(Service.MtdIt, Some("Cersei not KEEPing well"), "agent name")
        val json = Json.parse(
          """{"state":"AuthorisationCancelled","properties": {"service": "HMRC-MTD-IT", "clientName":"Cersei not KEEPing well","agencyName": "agent name"}}"""
        )

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }
      "KnownFactNotMatched" in {
        val state = KnownFactNotMatched
        val json = Json.parse("""{"state":"KnownFactNotMatched"}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "TrustNotFound" in {
        val state = TrustNotFound
        val json = Json.parse("""{"state":"TrustNotFound"}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "CgtRefNotFound" in {
        val state = CgtRefNotFound(cgtRef)
        val json = Json.parse("""{"state":"CgtRefNotFound", "properties": {"cgtRef": "XMCGTP123456789"}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "NotSignedUp" in {
        val state = NotSignedUp(Service.MtdIt)
        val json = Json.parse("""{"state":"NotSignedUp", "properties": {"service":"HMRC-MTD-IT"}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }
      "NotAuthorised" in {
        val state = NotAuthorised(Service.MtdIt)
        val json = Json.parse("""{"state":"NotAuthorised", "properties": {"service":"HMRC-MTD-IT"}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }
      "ResponseFailed" in {
        val state = ResponseFailed(Service.MtdIt, Some("Holly Herndon"), "AB123456A")
        val json =
          Json.parse("""{"state":"ResponseFailed", "properties": {"service": "HMRC-MTD-IT", "clientName":"Holly Herndon","clientId": "AB123456A"}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }
    }
  }
}
