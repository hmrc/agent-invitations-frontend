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
import play.api.libs.json.{Format, JsArray, Json}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.State
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyStateFormats
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.Vrn
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationJourneyStateFormatsSpec extends UnitSpec {

  implicit val formats: Format[State] = AgentInvitationJourneyStateFormats.formats

  "AgentsInvitationJourneyFormats" should {
    "serialize and deserialize state" when {
      "SelectClientType" in {
        Json.toJson(SelectClientType(Set.empty)) shouldBe Json
          .obj("state" -> "SelectClientType", "properties" -> Json.obj("basket" -> JsArray()))
        Json
          .parse("""{"state":"SelectClientType", "properties": {"basket": []}}""")
          .as[State] shouldBe SelectClientType(Set.empty)
        Json
          .parse("""{"state":"SelectClientType", "properties": {"basket": []}}""")
          .as[State] shouldBe SelectClientType(Set.empty)
      }
      "SelectPersonalService" in {
        Json.toJson(SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty)) shouldBe Json.obj(
          "state" -> "SelectPersonalService",
          "properties" -> Json
            .obj("basket" -> JsArray(), "services" -> Json.arr("PERSONAL-INCOME-RECORD", "HMRC-MTD-IT", "HMRC-MTD-VAT"))
        )
        Json
          .parse(
            """{"state":"SelectPersonalService", "properties": {"basket": [], "services": ["PERSONAL-INCOME-RECORD", "HMRC-MTD-IT", "HMRC-MTD-VAT"]}}""")
          .as[State] shouldBe SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty)
      }
      "SelectBusinessService" in {
        Json.toJson(SelectBusinessService) shouldBe Json
          .obj("state" -> "SelectBusinessService")
        Json
          .parse("""{"state":"SelectBusinessService"}""")
          .as[State] shouldBe SelectBusinessService
      }
      "IdentifyPersonalClient" in {
        Json.toJson(IdentifyPersonalClient(HMRCMTDIT, Set.empty)) shouldBe Json.obj(
          "state"      -> "IdentifyPersonalClient",
          "properties" -> Json.obj("service" -> "HMRC-MTD-IT", "basket" -> JsArray())
        )
        Json
          .parse("""{"state":"IdentifyPersonalClient", "properties": {"basket": [], "service": "HMRC-MTD-IT"}}""")
          .as[State] shouldBe IdentifyPersonalClient(HMRCMTDIT, Set.empty)
      }
      "IdentifyBusinessClient" in {
        Json.toJson(IdentifyBusinessClient) shouldBe Json.obj("state" -> "IdentifyBusinessClient")
        Json
          .parse("""{"state":"IdentifyBusinessClient"}""")
          .as[State] shouldBe IdentifyBusinessClient
      }
      "ConfirmClientItsa" in {
        val state = ConfirmClientItsa(
          AuthorisationRequest(
            "Sylvia Plath",
            ItsaInvitation(Nino("AB123456A"), Some(Postcode("BN114AW"))),
            itemId = "ABC"),
          Set.empty)
        val json = Json.parse(
          """{"state":"ConfirmClientItsa","properties":{"request":{"clientName":"Sylvia Plath","invitation":{"type":"ItsaInvitation","data":{"clientType":"personal","service":"HMRC-MTD-IT","clientIdentifier":"AB123456A","clientIdentifierType":"ni","postcode":{"value":"BN114AW"}}},"state":"New","itemId":"ABC"},"basket":[]}}""")
        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "ConfirmClientIrv" in {
        val state = ConfirmClientIrv(
          AuthorisationRequest(
            "Sylvia Plath",
            PirInvitation(Nino("AB123456A"), Some(DOB("1990-10-10"))),
            itemId = "ABC"),
          Set.empty)
        val json = Json.parse(
          """{"state":"ConfirmClientIrv","properties":{"request":{"clientName":"Sylvia Plath","invitation":{"type":"PirInvitation","data":{"clientType":"personal","service":"PERSONAL-INCOME-RECORD","clientIdentifier":"AB123456A","clientIdentifierType":"ni","dob":{"value":"1990-10-10"}}},"state":"New","itemId":"ABC"},"basket":[]}}""")
        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state

      }
      "ConfirmClientPersonalVat" in {
        val state = ConfirmClientPersonalVat(
          AuthorisationRequest(
            "Sylvia Plath",
            VatInvitation(Some(personal), Vrn("123456"), Some(VatRegDate("2010-10-10"))),
            itemId = "ABC"),
          Set.empty)
        val json = Json.parse(
          """{"state":"ConfirmClientPersonalVat","properties":{"request":{"clientName":"Sylvia Plath","invitation":{"type":"VatInvitation","data":{"clientType":"personal","service":"HMRC-MTD-VAT","clientIdentifier":"123456","clientIdentifierType":"vrn","vatRegDate":{"value":"2010-10-10"}}},"state":"New","itemId":"ABC"},"basket":[]}}""")
        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "ConfirmClientBusinessVat" in {
        val state = ConfirmClientBusinessVat(
          AuthorisationRequest(
            "Sylvia Plath",
            VatInvitation(Some(business), Vrn("123456"), Some(VatRegDate("2010-10-10"))),
            itemId = "ABC"))
        val json = Json.parse(
          """{"state":"ConfirmClientBusinessVat","properties":{"request":{"clientName":"Sylvia Plath","invitation":{"type":"VatInvitation","data":{"clientType":"business","service":"HMRC-MTD-VAT","clientIdentifier":"123456","clientIdentifierType":"vrn","vatRegDate":{"value":"2010-10-10"}}},"state":"New","itemId":"ABC"}}}""")
        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "ReviewAuthorisationsPersonal" in {
        Json.toJson(ReviewAuthorisationsPersonal(Set.empty)) shouldBe Json.obj(
          "state"      -> "ReviewAuthorisationsPersonal",
          "properties" -> Json.obj("basket" -> JsArray())
        )
        Json
          .parse("""{"state":"ReviewAuthorisationsPersonal", "properties": {"basket": []}}""")
          .as[State] shouldBe ReviewAuthorisationsPersonal(Set.empty)
      }
      "InvitationSentPersonal" in {
        Json.toJson(InvitationSentPersonal("invitation/link", Some("continue/url"), "abc@xyz.com")) shouldBe Json.obj(
          "state" -> "InvitationSentPersonal",
          "properties" -> Json
            .obj("invitationLink" -> "invitation/link", "continueUrl" -> "continue/url", "agencyEmail" -> "abc@xyz.com")
        )
        Json
          .parse(
            """{"state":"InvitationSentPersonal", "properties": {"invitationLink": "invitation/link", "continueUrl": "continue/url", "agencyEmail": "abc@xyz.com"}}""")
          .as[State] shouldBe InvitationSentPersonal("invitation/link", Some("continue/url"), "abc@xyz.com")
      }
      "InvitationSentBusiness" in {
        Json.toJson(InvitationSentBusiness("invitation/link", Some("continue/url"), "abc@xyz.com")) shouldBe Json.obj(
          "state" -> "InvitationSentBusiness",
          "properties" -> Json
            .obj("invitationLink" -> "invitation/link", "continueUrl" -> "continue/url", "agencyEmail" -> "abc@xyz.com")
        )
        Json
          .parse(
            """{"state":"InvitationSentBusiness", "properties": {"invitationLink": "invitation/link", "continueUrl": "continue/url", "agencyEmail": "abc@xyz.com"}}""")
          .as[State] shouldBe InvitationSentBusiness("invitation/link", Some("continue/url"), "abc@xyz.com")
      }
      "KnownFactNotMatched" in {
        Json.toJson(KnownFactNotMatched(Set.empty)) shouldBe Json.obj(
          "state"      -> "KnownFactNotMatched",
          "properties" -> Json.obj("basket" -> JsArray())
        )
        Json
          .parse("""{"state":"KnownFactNotMatched", "properties": {"basket": []}}""")
          .as[State] shouldBe KnownFactNotMatched(Set.empty)
      }
      "CannotCreateRequest" in {
        Json.toJson(CannotCreateRequest(Set.empty)) shouldBe Json.obj(
          "state"      -> "CannotCreateRequest",
          "properties" -> Json.obj("basket" -> JsArray())
        )
        Json
          .parse("""{"state":"CannotCreateRequest", "properties": {"basket": []}}""")
          .as[State] shouldBe CannotCreateRequest(Set.empty)
      }
      "SomeAuthorisationsFailed" in {
        val state = SomeAuthorisationsFailed("invitation/link", None, "abc@xyz.com", Set.empty)
        val json = Json.parse(
          """{"state":"SomeAuthorisationsFailed","properties":{"invitationLink":"invitation/link", "agencyEmail": "abc@xyz.com", "basket": []}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "AllAuthorisationsFailed" in {
        Json.toJson(AllAuthorisationsFailed(Set.empty)) shouldBe Json.obj(
          "state"      -> "AllAuthorisationsFailed",
          "properties" -> Json.obj("basket" -> JsArray())
        )
        Json
          .parse("""{"state":"AllAuthorisationsFailed", "properties": {"basket": []}}""")
          .as[State] shouldBe AllAuthorisationsFailed(Set.empty)
      }
      "DeleteAuthorisationsPersonal" in {
        val state = DeleteAuthorisationRequestPersonal(
          AuthorisationRequest(
            "Sylvia Plath",
            ItsaInvitation(Nino("AB123456A"), Some(Postcode("BN114AW"))),
            itemId = "ABC"),
          Set.empty)
        val json = Json.parse(
          """{"state":"DeleteAuthorisationRequestPersonal","properties":{"authorisationRequest":{"clientName":"Sylvia Plath","invitation":{"type":"ItsaInvitation","data":{"clientType":"personal","service":"HMRC-MTD-IT","clientIdentifier":"AB123456A","clientIdentifierType":"ni","postcode":{"value":"BN114AW"}}},"state":"New","itemId":"ABC"},"basket":[]}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "AllAuthorisationsRemoved" in {
        val state = AllAuthorisationsRemoved
        val json = Json.parse("""{"state":"AllAuthorisationsRemoved"}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "ClientNotSignedUp" in {
        val state = ClientNotSignedUp(HMRCMTDIT, Set.empty)
        val json = Json.parse("""{"state":"ClientNotSignedUp","properties":{"service": "HMRC-MTD-IT", "basket": []}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "PendingInvitationExists" in {
        val state = PendingInvitationExists(personal, Set.empty)
        val json =
          Json.parse("""{"state":"PendingInvitationExists","properties":{"clientType": "personal", "basket": []}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "ActiveAuthorisationExists" in {
        val state = ActiveAuthorisationExists(personal, HMRCMTDIT, Set.empty)
        val json = Json.parse(
          """{"state":"ActiveAuthorisationExists","properties":{"clientType": "personal", "service": "HMRC-MTD-IT", "basket": []}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

    }

  }

}
