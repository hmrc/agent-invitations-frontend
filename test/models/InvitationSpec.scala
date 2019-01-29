package models

import play.api.libs.json.{JsSuccess, Json}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.Vrn
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.test.UnitSpec

class InvitationSpec extends UnitSpec {

  "Invitation.format" should {

    "read and write ItsaInvitation as expected" in {

      val itsaInvitation = ItsaInvitation(Nino("AB123456C"), Some(Postcode("AB12 3EF")))
      val jsValue = Json.parse("""{"type":"ItsaInvitation","data":{"clientType":"personal","service":"HMRC-MTD-IT","clientIdentifier":"AB123456C","clientIdentifierType":"ni","postcode":{"value":"AB12 3EF"}}}""")

      Invitation.format.writes(itsaInvitation) shouldBe jsValue
      Invitation.format.reads(jsValue)  shouldBe JsSuccess(itsaInvitation)
    }

    "read and write PirInvitation as expected" in {

      val pirInvitation = PirInvitation(Nino("AB123456C"), Some(DOB("10-10-2030")))
      val jsValue = Json.parse("""{"type":"PirInvitation","data":{"clientType":"personal","service":"PERSONAL-INCOME-RECORD","clientIdentifier":"AB123456C","clientIdentifierType":"ni","dob":{"value":"10-10-2030"}}}""")

      Invitation.format.writes(pirInvitation) shouldBe jsValue
      Invitation.format.reads(jsValue)  shouldBe JsSuccess(pirInvitation)
    }

    "read and write VatInvitation as expected" in {

      val vatInvitation = VatInvitation(Some("personal"), Vrn("329611751"), Some(VatRegDate("10-10-2030")))
      val jsValue = Json.parse("""{"type":"VatInvitation","data":{"clientType":"personal","service":"HMRC-MTD-VAT","clientIdentifier":"329611751","clientIdentifierType":"vrn","vatRegDate":{"value":"10-10-2030"}}}""")

      Invitation.format.writes(vatInvitation) shouldBe jsValue
      Invitation.format.reads(jsValue)  shouldBe JsSuccess(vatInvitation)
    }
  }

}
