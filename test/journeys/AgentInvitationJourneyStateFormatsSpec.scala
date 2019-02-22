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
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationJourneyStateFormatsSpec extends UnitSpec {

  implicit val formats: Format[State] = AgentInvitationJourneyStateFormats.formats

  "AgentsInvitationJourneyFormats" should {
    "serialize and deserialize state" when {
      "Start" in {
        Json.toJson(Start) shouldBe Json.obj("state" -> "Start")
        Json.parse("""{"state":"Start"}""").as[State] shouldBe Start
      }
      "SelectClientType" in {
        Json.toJson(SelectClientType) shouldBe Json.obj("state" -> "SelectClientType")
        Json.parse("""{"state":"SelectClientType"}""").as[State] shouldBe SelectClientType
      }
      "ClientTypeSelected" in {
        Json.toJson(ClientTypeSelected(ClientType.personal)) shouldBe Json
          .obj("state" -> "ClientTypeSelected", "properties" -> Json.obj("clientType" -> "personal"))
        Json
          .parse("""{"state":"ClientTypeSelected", "properties": {"clientType": "personal"}}""")
          .as[State] shouldBe ClientTypeSelected(ClientType.personal)
        Json
          .parse("""{"state":"ClientTypeSelected", "properties": {"clientType": "business"}}""")
          .as[State] shouldBe ClientTypeSelected(ClientType.business)
      }
      "SelectPersonalService" in {
        Json.toJson(SelectPersonalService(Set.empty, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT))) shouldBe Json.obj(
          "state" -> "SelectPersonalService",
          "properties" -> Json
            .obj("basket" -> JsArray(), "services" -> Json.arr("PERSONAL-INCOME-RECORD", "HMRC-MTD-IT", "HMRC-MTD-VAT"))
        )
        Json
          .parse(
            """{"state":"SelectPersonalService", "properties": {"basket": [], "services": ["PERSONAL-INCOME-RECORD", "HMRC-MTD-IT", "HMRC-MTD-VAT"]}}""")
          .as[State] shouldBe SelectPersonalService(Set.empty, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT))
      }
      "SelectBusinessService" in {
        Json.toJson(SelectBusinessService(Set.empty)) shouldBe Json
          .obj("state" -> "SelectBusinessService", "properties" -> Json.obj("basket" -> JsArray()))
        Json
          .parse("""{"state":"SelectBusinessService", "properties": {"basket": []}}""")
          .as[State] shouldBe SelectBusinessService(Set.empty)
      }
      "PersonalServiceSelected" in {
        Json.toJson(PersonalServiceSelected(HMRCMTDIT, Set.empty)) shouldBe Json.obj(
          "state"      -> "PersonalServiceSelected",
          "properties" -> Json.obj("service" -> "HMRC-MTD-IT", "basket" -> JsArray())
        )
        Json
          .parse("""{"state":"PersonalServiceSelected", "properties": {"basket": [], "service": "HMRC-MTD-IT"}}""")
          .as[State] shouldBe PersonalServiceSelected(HMRCMTDIT, Set.empty)
      }
      "BusinessServiceSelected" in {
        Json.toJson(BusinessServiceSelected(Set.empty)) shouldBe Json.obj(
          "state"      -> "BusinessServiceSelected",
          "properties" -> Json.obj("basket" -> JsArray())
        )
        Json
          .parse("""{"state":"BusinessServiceSelected", "properties": {"basket": []}}""")
          .as[State] shouldBe BusinessServiceSelected(Set.empty)
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
      "ItsaIdentifiedClient" in {
        Json.toJson(ItsaIdentifiedClient("AB123456A", Some("BN114AW"), Set.empty)) shouldBe Json.obj(
          "state" -> "ItsaIdentifiedClient",
          "properties" -> Json.obj(
            "clientIdentifier" -> "AB123456A",
            "postcode"         -> "BN114AW",
            "basket"           -> JsArray())
        )
        Json
          .parse(
            """{"state":"ItsaIdentifiedClient", "properties": {"basket": [], "clientIdentifier": "AB123456A", "postcode": "BN114AW"}}""")
          .as[State] shouldBe ItsaIdentifiedClient("AB123456A", Some("BN114AW"), Set.empty)
      }
      "IrvIdentifiedClient" in {
        Json.toJson(IrvIdentifiedClient("AB123456A", Some("1990-10-10"), Set.empty)) shouldBe Json.obj(
          "state" -> "IrvIdentifiedClient",
          "properties" -> Json.obj(
            "clientIdentifier" -> "AB123456A",
            "dob"              -> "1990-10-10",
            "basket"           -> JsArray())
        )
        Json
          .parse(
            """{"state":"IrvIdentifiedClient", "properties": {"basket": [], "clientIdentifier": "AB123456A", "dob": "1990-10-10"}}""")
          .as[State] shouldBe IrvIdentifiedClient("AB123456A", Some("1990-10-10"), Set.empty)
      }
      "VatIdentifiedPersonalClient" in {
        Json.toJson(VatIdentifiedPersonalClient("123456", Some("2010-01-01"), Set.empty)) shouldBe Json.obj(
          "state" -> "VatIdentifiedPersonalClient",
          "properties" -> Json.obj(
            "clientIdentifier" -> "123456",
            "registrationDate" -> "2010-01-01",
            "basket"           -> JsArray())
        )
        Json
          .parse(
            """{"state":"VatIdentifiedPersonalClient", "properties": {"basket": [], "clientIdentifier": "123456", "registrationDate": "2010-01-01"}}""")
          .as[State] shouldBe VatIdentifiedPersonalClient("123456", Some("2010-01-01"), Set.empty)
      }
      "VatIdentifiedBusinessClient" in {
        Json.toJson(VatIdentifiedBusinessClient("123456", Some("2010-01-01"), Set.empty)) shouldBe Json.obj(
          "state" -> "VatIdentifiedBusinessClient",
          "properties" -> Json.obj(
            "clientIdentifier" -> "123456",
            "registrationDate" -> "2010-01-01",
            "basket"           -> JsArray())
        )
        Json
          .parse(
            """{"state":"VatIdentifiedBusinessClient", "properties": {"basket": [], "clientIdentifier": "123456", "registrationDate": "2010-01-01"}}""")
          .as[State] shouldBe VatIdentifiedBusinessClient("123456", Some("2010-01-01"), Set.empty)
      }
      "ConfirmClientItsa" in {
        Json.toJson(ConfirmClientItsa("Piglet", Set.empty)) shouldBe Json.obj(
          "state" -> "ConfirmClientItsa",
          "properties" -> Json.obj(
            "clientName"              -> "Piglet",
            "basket"           -> JsArray())
        )
        Json
          .parse(
            """{"state":"ConfirmClientItsa", "properties": {"basket": [], "clientName": "Piglet"}}""")
          .as[State] shouldBe ConfirmClientItsa("Piglet", Set.empty)
      }
      "ConfirmClientIrv" in {
        Json.toJson(ConfirmClientIrv("Piglet", Set.empty)) shouldBe Json.obj(
          "state" -> "ConfirmClientIrv",
          "properties" -> Json.obj(
            "clientName"              -> "Piglet",
            "basket"           -> JsArray())
        )
        Json
          .parse(
            """{"state":"ConfirmClientIrv", "properties": {"basket": [], "clientName": "Piglet"}}""")
          .as[State] shouldBe ConfirmClientIrv("Piglet", Set.empty)
      }
      "ConfirmClientPersonalVat" in {
        Json.toJson(ConfirmClientPersonalVat("Piglet", Set.empty)) shouldBe Json.obj(
          "state" -> "ConfirmClientPersonalVat",
          "properties" -> Json.obj(
            "clientName"              -> "Piglet",
            "basket"           -> JsArray())
        )
        Json
          .parse(
            """{"state":"ConfirmClientPersonalVat", "properties": {"basket": [], "clientName": "Piglet"}}""")
          .as[State] shouldBe ConfirmClientPersonalVat("Piglet", Set.empty)
      }
      "ConfirmClientBusinessVat" in {
        Json.toJson(ConfirmClientBusinessVat("Piglet", Set.empty)) shouldBe Json.obj(
          "state" -> "ConfirmClientBusinessVat",
          "properties" -> Json.obj(
            "clientName"              -> "Piglet",
            "basket"           -> JsArray())
        )
        Json
          .parse(
            """{"state":"ConfirmClientBusinessVat", "properties": {"basket": [], "clientName": "Piglet"}}""")
          .as[State] shouldBe ConfirmClientBusinessVat("Piglet", Set.empty)
      }
      "ClientConfirmedPersonal" in {
        Json.toJson(ClientConfirmedPersonal(Set.empty)) shouldBe Json.obj(
          "state" -> "ClientConfirmedPersonal",
          "properties" -> Json.obj(
            "basket"           -> JsArray())
        )
        Json
          .parse(
            """{"state":"ClientConfirmedPersonal", "properties": {"basket": []}}""")
          .as[State] shouldBe ClientConfirmedPersonal(Set.empty)
      }
      "ClientConfirmedBusiness" in {
        Json.toJson(ClientConfirmedBusiness(Set.empty)) shouldBe Json.obj(
          "state" -> "ClientConfirmedBusiness",
          "properties" -> Json.obj(
            "basket"           -> JsArray())
        )
        Json
          .parse(
            """{"state":"ClientConfirmedBusiness", "properties": {"basket": []}}""")
          .as[State] shouldBe ClientConfirmedBusiness(Set.empty)
      }
      "ReviewAuthorisationsPersonal" in {
        Json.toJson(ReviewAuthorisationsPersonal(Set.empty)) shouldBe Json.obj(
          "state" -> "ReviewAuthorisationsPersonal",
          "properties" -> Json.obj(
            "basket"           -> JsArray())
        )
        Json
          .parse(
            """{"state":"ReviewAuthorisationsPersonal", "properties": {"basket": []}}""")
          .as[State] shouldBe ReviewAuthorisationsPersonal(Set.empty)
      }
      "ReviewAuthorisationsBusiness" in {
        Json.toJson(ReviewAuthorisationsBusiness(Set.empty)) shouldBe Json.obj(
          "state" -> "ReviewAuthorisationsBusiness",
          "properties" -> Json.obj(
            "basket"           -> JsArray())
        )
        Json
          .parse(
            """{"state":"ReviewAuthorisationsBusiness", "properties": {"basket": []}}""")
          .as[State] shouldBe ReviewAuthorisationsBusiness(Set.empty)
      }
      "AuthorisationsReviewedPersonal" in {
        Json.toJson(AuthorisationsReviewedPersonal) shouldBe Json.obj("state" -> "AuthorisationsReviewedPersonal")
        Json.parse("""{"state":"AuthorisationsReviewedPersonal"}""").as[State] shouldBe AuthorisationsReviewedPersonal
      }
      "AuthorisationsReviewedBusiness" in {
        Json.toJson(AuthorisationsReviewedBusiness) shouldBe Json.obj("state" -> "AuthorisationsReviewedBusiness")
        Json.parse("""{"state":"AuthorisationsReviewedBusiness"}""").as[State] shouldBe AuthorisationsReviewedBusiness
      }
      "InvitationSentPersonal" in {
        Json.toJson(InvitationSentPersonal("invitation/link", Some("continue/url"))) shouldBe Json.obj(
          "state" -> "InvitationSentPersonal",
          "properties" -> Json.obj(
            "invitationLink"              -> "invitation/link",
            "continueUrl"              -> "continue/url")
        )
        Json
          .parse(
            """{"state":"InvitationSentPersonal", "properties": {"invitationLink": "invitation/link", "continueUrl": "continue/url"}}""")
          .as[State] shouldBe InvitationSentPersonal("invitation/link", Some("continue/url"))
      }
      "InvitationSentBusiness" in {
        Json.toJson(InvitationSentBusiness("invitation/link", Some("continue/url"))) shouldBe Json.obj(
          "state" -> "InvitationSentBusiness",
          "properties" -> Json.obj(
            "invitationLink"              -> "invitation/link",
            "continueUrl"              -> "continue/url")
        )
        Json
          .parse(
            """{"state":"InvitationSentBusiness", "properties": {"invitationLink": "invitation/link", "continueUrl": "continue/url"}}""")
          .as[State] shouldBe InvitationSentBusiness("invitation/link", Some("continue/url"))
      }
    }

  }

}
