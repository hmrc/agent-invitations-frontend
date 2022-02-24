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
import play.api.libs.json.{Format, JsArray, Json}
import support.UnitSpec
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyStateFormats
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{Business, Personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{CgtRef, PptRef, Service, Urn, Utr, Vrn}
import uk.gov.hmrc.domain.Nino

class AgentInvitationJourneyStateFormatsSpec extends UnitSpec {

  implicit val formats: Format[State] = AgentInvitationJourneyStateFormats.formats

  "AgentsInvitationJourneyFormats" should {

    "serialize and deserialize state" when {

      "ConfirmClient" in {
        for {
          clientType <- List(ClientType.Personal, ClientType.Business, ClientType.Trust)
          (service, taxId) <- List(
                               (Service.MtdIt, Nino("AB123456A")),
                               (Service.Vat, Vrn("123456")),
                               (Service.PersonalIncomeRecord, Nino("AB123456A")),
                               (Service.Trust, Utr("4937455253")),
                               (Service.TrustNT, Urn("4937455253")),
                               (Service.CapitalGains, CgtRef("123456")),
                               (Service.Ppt, PptRef("XAPPT0000012345"))
                             )
        } yield {
          val state: State =
            ConfirmClient(AuthorisationRequest("Sylvia Plath", Invitation(Some(clientType), service, taxId), itemId = "ABC"), Set.empty)
          Json.toJson(state).as[State] shouldBe state
        }
      }

      "CgtRefNotFound" in {
        Json.toJson(CgtRefNotFound(CgtRef("cgtRef"), Set.empty): State) shouldBe Json
          .obj("state" -> "CgtRefNotFound", "properties" -> Json.obj("cgtRef" -> "cgtRef", "basket" -> JsArray()))
        Json
          .parse("""{"state":"CgtRefNotFound", "properties": {"cgtRef": "cgtRef", "basket": []}}""")
          .as[State] shouldBe CgtRefNotFound(CgtRef("cgtRef"), Set.empty)
      }

      "SelectClientType" in {
        Json.toJson(SelectClientType(Set.empty): State) shouldBe Json
          .obj("state" -> "SelectClientType", "properties" -> Json.obj("basket" -> JsArray()))
        Json
          .parse("""{"state":"SelectClientType", "properties": {"basket": []}}""")
          .as[State] shouldBe SelectClientType(Set.empty)
        Json
          .parse("""{"state":"SelectClientType", "properties": {"basket": []}}""")
          .as[State] shouldBe SelectClientType(Set.empty)
      }

      "SelectService" in {
        Json.toJson(SelectService(Personal, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty): State) shouldBe Json.obj(
          "state" -> "SelectService",
          "properties" -> Json
            .obj("clientType" -> "personal", "basket" -> JsArray(), "services" -> Json.arr("PERSONAL-INCOME-RECORD", "HMRC-MTD-IT", "HMRC-MTD-VAT"))
        )
        Json
          .parse(
            """{"state":"SelectService", "properties": {"clientType": "personal", "basket": [], "services": ["PERSONAL-INCOME-RECORD", "HMRC-MTD-IT", "HMRC-MTD-VAT"]}}""")
          .as[State] shouldBe SelectService(Personal, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty)
      }

      "IdentifyClient" in {
        Json.toJson(IdentifyClient(Business, HMRCMTDVAT, Set.empty): State) shouldBe Json.obj(
          "state"      -> "IdentifyClient",
          "properties" -> Json.obj("clientType" -> "business", "service" -> "HMRC-MTD-VAT", "basket" -> JsArray())
        )
        Json
          .parse("""{"state":"IdentifyClient", "properties": {"clientType": "business", "basket": [], "service": "HMRC-MTD-VAT"}}""")
          .as[State] shouldBe IdentifyClient(Business, HMRCMTDVAT, Set.empty)
      }

      "ConfirmPostcodeCgt" in {
        val state = ConfirmPostcodeCgt(CgtRef("123456"), Personal, Set.empty, Some("BN13 1FN"), "firstName lastName")
        val json = Json.parse(
          """{"state":"ConfirmPostcodeCgt","properties":{"cgtRef":"123456","clientType":"personal","basket":[], "postcode": "BN13 1FN", "clientName": "firstName lastName"}}""")
        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ConfirmCountryCodeCgt" in {
        val state = ConfirmCountryCodeCgt(CgtRef("123456"), Personal, Set.empty, "IN", "firstName lastName")
        val json = Json.parse(
          """{"state":"ConfirmCountryCodeCgt","properties":{"cgtRef":"123456","clientType":"personal","basket":[], "countryCode": "IN", "clientName": "firstName lastName"}}""")
        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ReviewAuthorisations" in {
        Json.toJson(ReviewAuthorisations(Personal, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty): State) shouldBe Json.obj(
          "state" -> "ReviewAuthorisations",
          "properties" -> Json
            .obj("clientType" -> "personal", "basket" -> JsArray(), "services" -> Json.arr("PERSONAL-INCOME-RECORD", "HMRC-MTD-IT", "HMRC-MTD-VAT"))
        )
        Json
          .parse(
            """{"state":"ReviewAuthorisations", "properties": {"clientType": "personal", "basket": [], "services": ["PERSONAL-INCOME-RECORD", "HMRC-MTD-IT", "HMRC-MTD-VAT"]}}""")
          .as[State] shouldBe ReviewAuthorisations(Personal, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty)
      }

      "InvitationSent" in {
        for {
          clientType <- List(ClientType.Personal, ClientType.Business, ClientType.Trust)
        } yield {
          val state = InvitationSent(clientType, "invitation/link", Some("continue/url"), "abc@xyz.com", Set(HMRCMTDIT, HMRCPIR), isAltItsa = None)
          Json.toJson(state: State).as[State] shouldBe state
        }
      }

      "KnownFactNotMatched" in {
        Json.toJson(KnownFactNotMatched(Set.empty): State) shouldBe Json.obj(
          "state"      -> "KnownFactNotMatched",
          "properties" -> Json.obj("basket" -> JsArray())
        )
        Json
          .parse("""{"state":"KnownFactNotMatched", "properties": {"basket": []}}""")
          .as[State] shouldBe KnownFactNotMatched(Set.empty)
      }

      "TrustNotFound" in {
        Json.toJson(TrustNotFound(Set.empty): State) shouldBe Json
          .obj("state" -> "TrustNotFound", "properties" -> Json.obj("basket" -> JsArray()))
        Json
          .parse("""{"state":"TrustNotFound", "properties": {"basket": []}}""")
          .as[State] shouldBe TrustNotFound(Set.empty)
      }

      "CannotCreateRequest" in {
        Json.toJson(CannotCreateRequest(Set.empty): State) shouldBe Json.obj(
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

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "AllAuthorisationsFailed" in {
        Json.toJson(AllAuthorisationsFailed(Set.empty): State) shouldBe Json.obj(
          "state"      -> "AllAuthorisationsFailed",
          "properties" -> Json.obj("basket" -> JsArray())
        )
        Json
          .parse("""{"state":"AllAuthorisationsFailed", "properties": {"basket": []}}""")
          .as[State] shouldBe AllAuthorisationsFailed(Set.empty)
      }

      "DeleteAuthorisationsPersonal" in {
        val state =
          DeleteAuthorisationRequest(
            Personal,
            AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, Nino("AB123456A")), itemId = "ABC"),
            Set.empty)

        Json.toJson(state: State).as[State] shouldBe state
      }

      "AllAuthorisationsRemoved" in {
        val state = AllAuthorisationsRemoved
        val json = Json.parse("""{"state":"AllAuthorisationsRemoved"}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ClientNotSignedUp" in {
        val state = ClientNotSignedUp(HMRCMTDIT, Set.empty)
        val json = Json.parse("""{"state":"ClientNotSignedUp","properties":{"service": "HMRC-MTD-IT", "basket": []}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "PendingInvitationExists" in {
        val state = PendingInvitationExists(Personal, "Acme Ltd", "invitation/link", Set.empty)
        val json =
          Json.parse("""{"state":"PendingInvitationExists",
                       |"properties":{"clientType": "personal",
                       |"agentLink":"invitation/link","clientName":"Acme Ltd",
                       |"basket": []}}""".stripMargin)

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ActiveAuthorisationExists" in {
        val state = ActiveAuthorisationExists(Personal, HMRCMTDIT, Set.empty)
        val json =
          Json.parse("""{"state":"ActiveAuthorisationExists","properties":{"clientType": "personal", "service": "HMRC-MTD-IT", "basket": []}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "PartialAuthorisationExists" in {
        val state = PartialAuthorisationExists(Set.empty)
        val json =
          Json.parse(s"""{"state": "PartialAuthorisationExists", "properties":{"basket":[]}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ClientNotRegistered" in {
        val state = ClientNotRegistered(Set.empty)
        val json =
          Json.parse(s"""{"state": "ClientNotRegistered", "properties":{"basket":[]}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "AgentSuspended" in {
        val state = AgentSuspended(HMRCMTDIT, Set.empty)
        val json =
          Json.parse("""{"state":"AgentSuspended","properties":{"suspendedService": "HMRC-MTD-IT", "basket": []}}""")

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

      "ClientInsolvent" in {
        val state = ClientInsolvent(Set.empty)
        val json =
          Json.parse("""{"state":"ClientInsolvent", "properties": {"basket": []}} """)

        Json.toJson(state: State) shouldBe json
        json.as[State] shouldBe state
      }

    }
  }
}
