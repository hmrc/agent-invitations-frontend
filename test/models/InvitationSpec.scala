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

package models

import play.api.libs.json.{JsSuccess, Json}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.Personal
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{PptRef, Vrn}
import uk.gov.hmrc.domain.Nino
import support.UnitSpec

class InvitationSpec extends UnitSpec {

  "Invitation.format" should {

    "read and write ItsaInvitation as expected" in {

      val itsaInvitation = ItsaInvitation(Nino("AB123456C"))
      val jsValue = Json.parse(
        """{"type":"ItsaInvitation","data":{"clientType":"personal","service":"HMRC-MTD-IT","clientIdentifier":"AB123456C","clientIdentifierType":"ni"}}""")

      Invitation.format.writes(itsaInvitation) shouldBe jsValue
      Invitation.format.reads(jsValue) shouldBe JsSuccess(itsaInvitation)
    }

    "read and write PirInvitation as expected" in {

      val pirInvitation = PirInvitation(Nino("AB123456C"))
      val jsValue = Json.parse(
        """{"type":"PirInvitation","data":{"clientType":"personal","service":"PERSONAL-INCOME-RECORD","clientIdentifier":"AB123456C","clientIdentifierType":"ni"}}""")

      Invitation.format.writes(pirInvitation) shouldBe jsValue
      Invitation.format.reads(jsValue) shouldBe JsSuccess(pirInvitation)
    }

    "read and write VatInvitation as expected" in {

      val vatInvitation = VatInvitation(Some(Personal), Vrn("329611751"))
      val jsValue = Json.parse(
        """{"type":"VatInvitation","data":{"clientType":"personal","service":"HMRC-MTD-VAT","clientIdentifier":"329611751","clientIdentifierType":"vrn"}}""")

      Invitation.format.writes(vatInvitation) shouldBe jsValue
      Invitation.format.reads(jsValue) shouldBe JsSuccess(vatInvitation)
    }

    "read and write PptInvitation as expected" in {

      val pptInvitation = PptInvitation(PptRef("XAPPT000012345"), Some(Personal))
      val jsValue = Json.parse(
        """{"type":"PptInvitation","data":{"clientType":"personal","service":"HMRC-PPT-ORG","clientIdentifier":"XAPPT000012345","clientIdentifierType":"EtmpRegistrationNumber"}}""")

      Invitation.format.writes(pptInvitation) shouldBe jsValue
      Invitation.format.reads(jsValue) shouldBe JsSuccess(pptInvitation)
    }
  }

}
