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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyStateFormats
import uk.gov.hmrc.agentinvitationsfrontend.models.{DOB, Postcode, Services, VatRegDate}
import uk.gov.hmrc.agentinvitationsfrontend.support.TestDataCommonSupport
import uk.gov.hmrc.domain.{Nino, Vrn}
import uk.gov.hmrc.play.test.UnitSpec

class AgentLedDeauthJourneyStateFormatsSpec extends UnitSpec with TestDataCommonSupport {

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
      "IdentifyClientPersonal" in {
        val state = IdentifyClientPersonal(Services.HMRCMTDIT)
        val json = Json.parse("""{"state":"IdentifyClientPersonal", "properties": {"service": "HMRC-MTD-IT"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "IdentifyClientBusiness" in {
        val state = IdentifyClientBusiness
        val json = Json.parse("""{"state":"IdentifyClientBusiness"}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "ConfirmClientItsa" in {
        val state = ConfirmClientItsa("Cersei not KEEPing well", Nino("AB123456A"), Postcode("123"))
        val json = Json.parse("""{"state":"ConfirmClientItsa","properties": {"clientName":"Cersei not KEEPing well","nino": "AB123456A","postcode":{"value":"123"}}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "ConfirmClientIrv" in {
        val state = ConfirmClientIrv("Cersei not KEEPing well", Nino("AB123456A"), DOB("123"))
        val json = Json.parse("""{"state":"ConfirmClientIrv","properties": {"clientName":"Cersei not KEEPing well","nino": "AB123456A","dob":{"value":"123"}}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "ConfirmClientPersonalVat" in {
        val state = ConfirmClientPersonalVat("Cersei not KEEPing well", validVrn, VatRegDate("123"))
        val json = Json.parse(s"""{"state":"ConfirmClientPersonalVat","properties": {"clientName":"Cersei not KEEPing well","vrn": "${validVrn.value}","vatRegDate":{"value":"123"}}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "ConfirmClientBusiness" in {
        val state = ConfirmClientBusiness("Cersei not KEEPing well", validVrn, VatRegDate("123"))
        val json = Json.parse(s"""{"state":"ConfirmClientBusiness","properties": {"clientName":"Cersei not KEEPing well","vrn": "${validVrn.value}","vatRegDate":{"value":"123"}}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
    }
  }
}
