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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.States._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyStateFormats
import uk.gov.hmrc.agentinvitationsfrontend.models.{AuthorisationRequest, ClientType, ItsaInvitation, Postcode}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
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
        Json.toJson(SelectBusinessService(Set.empty)) shouldBe Json
          .obj("state" -> "SelectBusinessService", "properties" -> Json.obj("basket" -> JsArray()))
        Json
          .parse("""{"state":"SelectBusinessService", "properties": {"basket": []}}""")
          .as[State] shouldBe SelectBusinessService(Set.empty)
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
        Json.toJson(IdentifyBusinessClient(Set.empty)) shouldBe Json.obj(
          "state"      -> "IdentifyBusinessClient",
          "properties" -> Json.obj("basket" -> JsArray())
        )
        Json
          .parse("""{"state":"IdentifyBusinessClient", "properties": {"basket": []}}""")
          .as[State] shouldBe IdentifyBusinessClient(Set.empty)
      }
      "ConfirmClientItsa" in {
        Json.toJson(ConfirmClientItsa(AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino("AB123456A"),Some(Postcode("BN114AW")))), Set.empty)) shouldBe Json.obj(
          "state"      -> "ConfirmClientItsa",
          "properties" -> Json.obj("clientName" -> "Piglet", "basket" -> JsArray())
        )
        Json
          .parse("""{"state":"ConfirmClientItsa", "properties": {"basket": [], "clientName": "Piglet"}}""")
          .as[State] shouldBe ConfirmClientItsa("Piglet", Set.empty)
      }
      "ConfirmClientIrv" in {
        Json.toJson(ConfirmClientIrv("Piglet", Set.empty)) shouldBe Json.obj(
          "state"      -> "ConfirmClientIrv",
          "properties" -> Json.obj("clientName" -> "Piglet", "basket" -> JsArray())
        )
        Json
          .parse("""{"state":"ConfirmClientIrv", "properties": {"basket": [], "clientName": "Piglet"}}""")
          .as[State] shouldBe ConfirmClientIrv("Piglet", Set.empty)
      }
      "ConfirmClientPersonalVat" in {
        Json.toJson(ConfirmClientPersonalVat("Piglet", Set.empty)) shouldBe Json.obj(
          "state"      -> "ConfirmClientPersonalVat",
          "properties" -> Json.obj("clientName" -> "Piglet", "basket" -> JsArray())
        )
        Json
          .parse("""{"state":"ConfirmClientPersonalVat", "properties": {"basket": [], "clientName": "Piglet"}}""")
          .as[State] shouldBe ConfirmClientPersonalVat("Piglet", Set.empty)
      }
      "ConfirmClientBusinessVat" in {
        Json.toJson(ConfirmClientBusinessVat("Piglet", Set.empty)) shouldBe Json.obj(
          "state"      -> "ConfirmClientBusinessVat",
          "properties" -> Json.obj("clientName" -> "Piglet", "basket" -> JsArray())
        )
        Json
          .parse("""{"state":"ConfirmClientBusinessVat", "properties": {"basket": [], "clientName": "Piglet"}}""")
          .as[State] shouldBe ConfirmClientBusinessVat("Piglet", Set.empty)
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
      "ReviewAuthorisationsBusiness" in {
        Json.toJson(ReviewAuthorisationsBusiness(Set.empty)) shouldBe Json.obj(
          "state"      -> "ReviewAuthorisationsBusiness",
          "properties" -> Json.obj("basket" -> JsArray())
        )
        Json
          .parse("""{"state":"ReviewAuthorisationsBusiness", "properties": {"basket": []}}""")
          .as[State] shouldBe ReviewAuthorisationsBusiness(Set.empty)
      }
      "InvitationSentPersonal" in {
        Json.toJson(InvitationSentPersonal("invitation/link", Some("continue/url"))) shouldBe Json.obj(
          "state"      -> "InvitationSentPersonal",
          "properties" -> Json.obj("invitationLink" -> "invitation/link", "continueUrl" -> "continue/url")
        )
        Json
          .parse(
            """{"state":"InvitationSentPersonal", "properties": {"invitationLink": "invitation/link", "continueUrl": "continue/url"}}""")
          .as[State] shouldBe InvitationSentPersonal("invitation/link", Some("continue/url"))
      }
      "InvitationSentBusiness" in {
        Json.toJson(InvitationSentBusiness("invitation/link", Some("continue/url"))) shouldBe Json.obj(
          "state"      -> "InvitationSentBusiness",
          "properties" -> Json.obj("invitationLink" -> "invitation/link", "continueUrl" -> "continue/url")
        )
        Json
          .parse(
            """{"state":"InvitationSentBusiness", "properties": {"invitationLink": "invitation/link", "continueUrl": "continue/url"}}""")
          .as[State] shouldBe InvitationSentBusiness("invitation/link", Some("continue/url"))
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
      "SomeAuthorisationsFailed" in {
        Json.toJson(SomeAuthorisationsFailed(Set.empty)) shouldBe Json.obj(
          "state"      -> "SomeAuthorisationsFailed",
          "properties" -> Json.obj("basket" -> JsArray())
        )
        Json
          .parse("""{"state":"SomeAuthorisationsFailed", "properties": {"basket": []}}""")
          .as[State] shouldBe SomeAuthorisationsFailed(Set.empty)
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
    }

  }

}
