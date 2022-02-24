/*
 * Copyright 2022 HM Revenue & Customs
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
import uk.gov.hmrc.agentmtdidentifiers.model.{CgtRef, Service, Utr, Vrn}
import uk.gov.hmrc.domain.Nino
import support.UnitSpec

class AgentLedDeauthJourneyStateFormatsSpec extends UnitSpec {

  implicit val formats: Format[State] = AgentLedDeauthJourneyStateFormats.formats
  val cgtRef = CgtRef("XMCGTP123456789")

  "AgentLedDeauthJourneyStateFormats" should {
    "serialize and deserialize state" when {

      "SelectClientType" in {
        val state = SelectClientType
        val json = Json.parse("""{"state":"SelectClientType"}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }
      "SelectServicePersonal" in {
        val state = SelectServicePersonal(Set(Service.Vat, Service.MtdIt))
        val json = Json.parse("""{"state":"SelectServicePersonal", "properties": {"enabledServices": ["HMRC-MTD-VAT", "HMRC-MTD-IT"]}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }
      "SelectServiceBusiness" in {
        val state = SelectServiceBusiness(enabledServices = Set(Service.Vat, Service.Ppt))
        val json = Json.parse(s"""{"state":"SelectServiceBusiness", "properties": {"enabledServices": ["HMRC-MTD-VAT", "HMRC-PPT-ORG"]}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "SelectServiceTrust" in {
        val state = SelectServiceTrust(Set(Service.Trust, Service.CapitalGains))
        val json = Json.parse("""{"state":"SelectServiceTrust","properties":{"enabledServices":["HMRC-TERS-ORG","HMRC-CGT-PD"]}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ConfirmPostcodeCgt" in {
        val state = ConfirmPostcodeCgt(cgtRef, Some("BN13 1FN"), "some-client")
        val json = Json.parse(
          """{"state":"ConfirmPostcodeCgt", "properties": {"cgtRef": "XMCGTP123456789", "postcode": "BN13 1FN", "clientName": "some-client"}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ConfirmCountryCodeCgt" in {
        val state = ConfirmCountryCodeCgt(cgtRef, "GB", "some-client")
        val json = Json.parse(
          """{"state":"ConfirmCountryCodeCgt", "properties": {"cgtRef": "XMCGTP123456789", "countryCode": "GB", "clientName": "some-client"}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "IdentifyClientPersonal" in {
        val state = IdentifyClientPersonal(Service.MtdIt)
        val json = Json.parse("""{"state":"IdentifyClientPersonal", "properties": {"service": "HMRC-MTD-IT"}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }
      "IdentifyClientBusiness" in {
        val state = IdentifyClientBusiness(Service.Vat)
        val json = Json.parse(s"""{"state":"IdentifyClientBusiness", "properties": {"service": "${Service.Vat}"}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "IdentifyClientTrust" in {
        val state = IdentifyClientTrust
        val json = Json.parse("""{"state":"IdentifyClientTrust"}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ConfirmClientItsa" in {
        val state = ConfirmClientItsa(Some("Cersei not KEEPing well"), Nino("AB123456A"))
        val json = Json.parse("""{"state":"ConfirmClientItsa","properties": {"clientName":"Cersei not KEEPing well","nino": "AB123456A"}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }
      "ConfirmClientIrv" in {
        val state = ConfirmClientIrv(Some("Cersei not KEEPing well"), Nino("AB123456A"))
        val json = Json.parse("""{"state":"ConfirmClientIrv","properties": {"clientName":"Cersei not KEEPing well","nino": "AB123456A"}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }
      "ConfirmClientPersonalVat" in {
        val state = ConfirmClientPersonalVat(Some("Cersei not KEEPing well"), Vrn("123456"))
        val json = Json.parse(s"""{"state":"ConfirmClientPersonalVat","properties": {"clientName":"Cersei not KEEPing well","vrn": "123456"}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }
      "ConfirmClientBusiness" in {
        val state = ConfirmClientBusiness(Some("Cersei not KEEPing well"), Vrn("123456"))
        val json = Json.parse(s"""{"state":"ConfirmClientBusiness","properties": {"clientName":"Cersei not KEEPing well","vrn": "123456"}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ConfirmClientTrust" in {
        val state = ConfirmClientTrust("some-trust", Utr("1977030537"))
        val json = Json.parse(s"""{"state":"ConfirmClientTrust","properties": {"clientName":"some-trust","utr": "1977030537"}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ConfirmClientCgt" in {
        val state = ConfirmClientCgt(cgtRef, "some-client")
        val json = Json.parse(s"""{"state":"ConfirmClientCgt","properties": {"clientName":"some-client","cgtRef": "XMCGTP123456789"}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ConfirmCancel" in {
        val state = ConfirmCancel(Service.MtdIt, Some("Cersei not KEEPing well"), "AB123456A")
        val json = Json.parse(
          """{"state":"ConfirmCancel","properties": {"service": "HMRC-MTD-IT", "clientName":"Cersei not KEEPing well","clientId": "AB123456A", "isPartialAuth": false}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }
      "AuthorisationCancelled" in {
        val state = AuthorisationCancelled(Service.MtdIt, Some("Cersei not KEEPing well"), "agent name")
        val json = Json.parse(
          """{"state":"AuthorisationCancelled","properties": {"service": "HMRC-MTD-IT", "clientName":"Cersei not KEEPing well","agencyName": "agent name"}}""")

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
