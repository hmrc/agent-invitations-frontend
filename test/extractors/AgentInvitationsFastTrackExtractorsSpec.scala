/*
 * Copyright 2018 HM Revenue & Customs
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

package extractors

import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.models.FastTrackInvitation
import uk.gov.hmrc.agentmtdidentifiers.model.{MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationsFastTrackExtractorsSpec extends UnitSpec {

  private val serviceITSA = "HMRC-MTD-IT"
  private val servicePIR = "PERSONAL-INCOME-RECORD"
  private val serviceVAT = "HMRC-MTD-VAT"
  private val mtdItId = MtdItId("ABCDEF123456789")
  private val vrn = Vrn("101747696")
  private val nino = Nino("AB123456A")
  private val validPostcode = "DH14EJ"
  val validRegDateForVrn97 = "2007-07-07"



  "The FastTrackInvitationItsaComplete extractor" should {
    "return Some" when {
      "the service is HMRC-MTD-IT and there is a valid Nino and Postcode" in {
        val itsaInvitation = FastTrackInvitation(Some(serviceITSA), Some("ni"), Some(nino.value), Some(validPostcode), None)
        FastTrackInvitationItsaComplete.unapply(itsaInvitation) shouldBe Some(itsaInvitation)
      }
    }

    "return None" when {
      "there is no service" in {
        val itsaInvitation = FastTrackInvitation(None, Some("ni"), Some(nino.value), Some(validPostcode), None)
        FastTrackInvitationItsaComplete.unapply(itsaInvitation) shouldBe None
      }
      "the service is not HMRC-MTD-IT" in {
        val itsaInvitation = FastTrackInvitation(Some(servicePIR), Some("ni"), Some(nino.value), Some(validPostcode), None)
        FastTrackInvitationItsaComplete.unapply(itsaInvitation) shouldBe None
      }

      "the service is HMRC-MTD-IT but there is no client identifier type" in {
        val itsaInvitation = FastTrackInvitation(Some(servicePIR), Some("ni"), Some(nino.value), Some(validPostcode), None)
        FastTrackInvitationItsaComplete.unapply(itsaInvitation) shouldBe None
      }

      "the service is HMRC-MTD-IT but the client identifier type is not 'ni'" in {
        val itsaInvitation = FastTrackInvitation(Some(serviceITSA), None, Some(nino.value), Some(validPostcode), None)
        FastTrackInvitationItsaComplete.unapply(itsaInvitation) shouldBe None
      }

      "the service is HMRC-MTD-IT but there is no Nino" in {
        val itsaInvitation = FastTrackInvitation(Some(serviceITSA), Some("ni"), None, Some(validPostcode), None)
        FastTrackInvitationItsaComplete.unapply(itsaInvitation) shouldBe None
      }

      "the service is HMRC-MTD-IT but the Nino is invalid" in {
        val itsaInvitation = FastTrackInvitation(Some(serviceITSA), Some("ni"), Some("Invalid_Nino"), Some(validPostcode), None)
        FastTrackInvitationItsaComplete.unapply(itsaInvitation) shouldBe None
      }

      "the service is HMRC-MTD-IT but there is no Postcode" in {
        val itsaInvitation = FastTrackInvitation(Some(serviceITSA), Some("ni"), Some(nino.value), None, None)
        FastTrackInvitationItsaComplete.unapply(itsaInvitation) shouldBe None
      }

      "the service is HMRC-MTD-IT but the Postcode is invalid" in {
        val itsaInvitation = FastTrackInvitation(Some(serviceITSA), Some("ni"), Some(nino.value), Some("Invalid_Postcode"), None)
        FastTrackInvitationItsaComplete.unapply(itsaInvitation) shouldBe None
      }
    }
  }

  "The FastTrackInvitationIrvComplete extractor" should {
    "return Some" when {
      "the service is PERSONAL-INCOME-RECORD and there is a valid Nino" in {
        val irvInvitation = FastTrackInvitation(Some(servicePIR), Some("ni"), Some(nino.value), None, None)
        FastTrackInvitationIrvComplete.unapply(irvInvitation) shouldBe Some(irvInvitation)
      }
    }

    "return None" when {
      "there is no service" in {
        val irvInvitation = FastTrackInvitation(None , Some("ni"), Some(nino.value), None, None)
        FastTrackInvitationIrvComplete.unapply(irvInvitation) shouldBe None
      }

      "the service is not PERSONAL-INCOME-RECORD" in {
        val irvInvitation = FastTrackInvitation(Some(serviceVAT), Some("ni"), Some(nino.value), None, None)
        FastTrackInvitationIrvComplete.unapply(irvInvitation) shouldBe None
      }

      "the service is PERSONAL-INCOME-RECORD but there is no clientIdentifierType" in {
        val irvInvitation = FastTrackInvitation(Some(servicePIR), None, Some(nino.value), None, None)
        FastTrackInvitationIrvComplete.unapply(irvInvitation) shouldBe None
      }

      "the service is PERSONAL-INCOME-RECORD but there is invalid clientIdentifierType" in {
        val irvInvitation = FastTrackInvitation(Some(servicePIR), Some("vrn"), Some(nino.value), None, None)
        FastTrackInvitationIrvComplete.unapply(irvInvitation) shouldBe None
      }

      "the service is PERSONAL-INCOME-RECORD but there is no clientIdentifier" in {
        val irvInvitation = FastTrackInvitation(Some(servicePIR), Some("ni"), None, None, None)
        FastTrackInvitationIrvComplete.unapply(irvInvitation) shouldBe None
      }

      "the service is PERSONAL-INCOME-RECORD but there is invalid clientIdentifier" in {
        val irvInvitation = FastTrackInvitation(Some(servicePIR), Some("ni"), Some("Invalid_Nino"), None, None)
        FastTrackInvitationIrvComplete.unapply(irvInvitation) shouldBe None
      }
    }
  }
  
  "The FastTrackInvitationVatComplete extractor" should {
    "return Some" when {
      "the service is HMRC-MTD-VAT and there is a valid Vrn and VatRegDate" in {
        val vatInvitation = FastTrackInvitation(Some(serviceVAT), Some("vrn"), Some(vrn.value), None, Some(validRegDateForVrn97))
        FastTrackInvitationVatComplete.unapply(vatInvitation) shouldBe Some(vatInvitation)
      }
    }
    
    "return None" when {
      "there is no service" in {
        val vatInvitation = FastTrackInvitation(None, Some("vrn"), Some(vrn.value), None, Some(validRegDateForVrn97))
        FastTrackInvitationVatComplete.unapply(vatInvitation) shouldBe None
      }

      "the service is not HMRC-MTD-VAT" in {
        val vatInvitation = FastTrackInvitation(Some(serviceITSA), Some("vrn"), Some(vrn.value), None, Some(validRegDateForVrn97))
        FastTrackInvitationVatComplete.unapply(vatInvitation) shouldBe None
      }

      "the service is HMRC-MTD-VAT but there is no clientIdentifierType" in {
        val vatInvitation = FastTrackInvitation(Some(serviceVAT), None, Some(vrn.value), None, Some(validRegDateForVrn97))
        FastTrackInvitationVatComplete.unapply(vatInvitation) shouldBe None
      }

      "the service is HMRC-MTD-VAT but there is invalid clientIdentifierType" in {
        val vatInvitation = FastTrackInvitation(Some(serviceVAT), Some("ni"), Some(vrn.value), None, Some(validRegDateForVrn97))
        FastTrackInvitationVatComplete.unapply(vatInvitation) shouldBe None
      }

      "the service is HMRC-MTD-VAT but there is no clientIdentifier" in {
        val vatInvitation = FastTrackInvitation(Some(serviceVAT), Some("vrn"), None, None, Some(validRegDateForVrn97))
        FastTrackInvitationVatComplete.unapply(vatInvitation) shouldBe None
      }

      "the service is HMRC-MTD-VAT but there is invalid clientIdentifier" in {
        val vatInvitation = FastTrackInvitation(Some(serviceVAT), Some("vrn"), Some("Invalid_Vrn"), None, Some(validRegDateForVrn97))
        FastTrackInvitationVatComplete.unapply(vatInvitation) shouldBe None
      }

      "the service is HMRC-MTD-VAT but there is no vat-reg-date" in {
        val vatInvitation = FastTrackInvitation(Some(serviceVAT), Some("vrn"), Some(vrn.value), None, None)
        FastTrackInvitationVatComplete.unapply(vatInvitation) shouldBe None
      }

      "the service is HMRC-MTD-VAT but there is invalid vat-reg-date" in {
        val vatInvitation = FastTrackInvitation(Some(serviceVAT), Some("vrn"), Some(vrn.value), None, Some("Invalid_Reg_Date"))
        FastTrackInvitationVatComplete.unapply(vatInvitation) shouldBe None
      }
    }
  }

  "The FastTrackInvitationInvalidClientIdentifier extractor" should {
    "return Some" when {
      "the service is HMRC-MTD-IT with invalid Nino" in {
        val invalidClientIdItsaInvitation = FastTrackInvitation(Some(serviceITSA), Some("ni"), Some("Invalid_Nino"), None, None)
        FastTrackInvitationNeedsClientIdentifier.unapply(invalidClientIdItsaInvitation) shouldBe
          Some(invalidClientIdItsaInvitation.copy(clientIdentifier = None))
      }

      "the service is HMRC-MTD-IT with no Nino" in {
        val invalidClientIdItsaInvitation = FastTrackInvitation(Some(serviceITSA), Some("ni"), None, None, None)
        FastTrackInvitationNeedsClientIdentifier.unapply(invalidClientIdItsaInvitation) shouldBe
          Some(invalidClientIdItsaInvitation.copy(clientIdentifierType = None, clientIdentifier = None))
      }

      "the service is PERSONAL-INCOME-RECORD with invalid Nino" in {
        val invalidClientIdIrvInvitation = FastTrackInvitation(Some(servicePIR), Some("ni"), Some("Invalid_Nino"), None, None)
        FastTrackInvitationNeedsClientIdentifier.unapply(invalidClientIdIrvInvitation) shouldBe
          Some(invalidClientIdIrvInvitation.copy(clientIdentifier = None))
      }

      "the service is PERSONAL-INCOME-RECORD with no Nino" in {
        val invalidClientIdIrvInvitation = FastTrackInvitation(Some(servicePIR), Some("ni"), None, None, None)
        FastTrackInvitationNeedsClientIdentifier.unapply(invalidClientIdIrvInvitation) shouldBe
          Some(invalidClientIdIrvInvitation.copy(clientIdentifierType = None, clientIdentifier = None))
      }

      "the service is HMRC-MTD-VAT with invalid Vrn" in {
        val invalidClientIdVatInvitation = FastTrackInvitation(Some(serviceVAT), Some("vrn"), Some("Invalid_Vrn"), None, None)
        FastTrackInvitationNeedsClientIdentifier.unapply(invalidClientIdVatInvitation) shouldBe
          Some(invalidClientIdVatInvitation.copy(clientIdentifier = None))
      }

      "the service is HMRC-MTD-VAT with no Vrn" in {
        val invalidClientIdVatInvitation = FastTrackInvitation(Some(serviceVAT), Some("vrn"), None, None, None)
        FastTrackInvitationNeedsClientIdentifier.unapply(invalidClientIdVatInvitation) shouldBe
          Some(invalidClientIdVatInvitation.copy(clientIdentifierType = None, clientIdentifier = None))
      }
    }

    "return None" when {
      "there is unsupported service" in {
        val invalidClientIdVatInvitation = FastTrackInvitation(Some("HMRC-AS-AGENT"), None, Some("agentReferenceNumber"), None, None)
        FastTrackInvitationNeedsClientIdentifier.unapply(invalidClientIdVatInvitation) shouldBe None
      }

      "there is no service" in {
        val invalidClientIdVatInvitation = FastTrackInvitation(None, None, None, None, None)
        FastTrackInvitationNeedsClientIdentifier.unapply(invalidClientIdVatInvitation) shouldBe None
      }

      "there is no service but there are other details" in {
        val invalidClientIdVatInvitation = FastTrackInvitation(None, Some("vrn"), Some(vrn.value), None, Some(validRegDateForVrn97))
        FastTrackInvitationNeedsClientIdentifier.unapply(invalidClientIdVatInvitation) shouldBe None

        val invalidClientIdNinoBasedInvitation = FastTrackInvitation(None, Some("ni"), Some(nino.value), Some(validPostcode), None)
        FastTrackInvitationNeedsClientIdentifier.unapply(invalidClientIdNinoBasedInvitation) shouldBe None
      }
    }
  }

  "The FastTrackInvitationNeedsKnownFact extractor" should {
    "return Some" when {
      "the service is HMRC-MTD-IT but there is no knownfact: postcode" in {
        val invalidClientIdNinoBasedInvitation = FastTrackInvitation(Some(serviceITSA), Some("ni"), Some(nino.value), None, None)
        FastTrackInvitationNeedsKnownFact.unapply(invalidClientIdNinoBasedInvitation) shouldBe Some(invalidClientIdNinoBasedInvitation)
      }

      "the service is HMRC-MTD-IT but there is an invalid postcode" in {
        val invalidClientIdNinoBasedInvitation = FastTrackInvitation(Some(serviceITSA), Some("ni"), Some(nino.value), Some("Invalid_Postcode"), None)
        FastTrackInvitationNeedsKnownFact.unapply(invalidClientIdNinoBasedInvitation) shouldBe Some(invalidClientIdNinoBasedInvitation.copy(postcode = None))
      }

      "the service is HMRC-MTD-VAT but there is no knownfact: vat-reg-date" in {
        val invalidClientIdVatBasedInvitation = FastTrackInvitation(Some(serviceVAT), Some("vrn"), Some(vrn.value), None, None)
        FastTrackInvitationNeedsKnownFact.unapply(invalidClientIdVatBasedInvitation) shouldBe Some(invalidClientIdVatBasedInvitation)
      }

      "the service is HMRC-MTD-VAT but there is an invalid vat-reg-date" in {
        val invalidClientIdVatBasedInvitation = FastTrackInvitation(Some(serviceVAT), Some("vrn"), Some(vrn.value), None, Some("Invalid_Date"))
        FastTrackInvitationNeedsKnownFact.unapply(invalidClientIdVatBasedInvitation) shouldBe Some(invalidClientIdVatBasedInvitation.copy(vatRegDate = None))
      }
    }

    "return None" when {
      "there is no service" in {
        val invalidClientIdVatBasedInvitation = FastTrackInvitation(None, None, None, None, None)
        FastTrackInvitationNeedsKnownFact.unapply(invalidClientIdVatBasedInvitation) shouldBe None
      }

      "there is invalid service" in {
        val invalidClientIdVatBasedInvitation = FastTrackInvitation("Invalid_Service")
        FastTrackInvitationNeedsKnownFact.unapply(invalidClientIdVatBasedInvitation) shouldBe None
      }

      "the service is PERSONAL-INCOME-RECORD" in {
        val invalidClientIdVatBasedInvitation = FastTrackInvitation(servicePIR)
        FastTrackInvitationNeedsKnownFact.unapply(invalidClientIdVatBasedInvitation) shouldBe None
      }
    }
  }

  "The FastTrackInvitationNeedsService extractor" should {
    "return Some" when {

      "there is no service but the details are for ITSA" in {
        val itsaInvitation = FastTrackInvitation(None, Some("ni"), Some(nino.value), Some(validPostcode), None)
        FastTrackInvitationNeedsService.unapply(itsaInvitation) shouldBe Some(itsaInvitation)
      }

      "there is no service but there is a Nino" in {
        val itsaInvitation = FastTrackInvitation(None, None, Some(nino.value), None, None)
        FastTrackInvitationNeedsService.unapply(itsaInvitation) shouldBe Some(itsaInvitation)
      }

      "there is no service but there is a Postcode" in {
        val itsaInvitation = FastTrackInvitation(None, None, None, Some(validPostcode), None)
        FastTrackInvitationNeedsService.unapply(itsaInvitation) shouldBe Some(itsaInvitation)
      }

      "there is no service but the details are for IRV" in {
        val irvInvitation = FastTrackInvitation(None, Some("ni"), Some(nino.value), None, None)
        FastTrackInvitationNeedsService.unapply(irvInvitation) shouldBe Some(irvInvitation)
      }

      "there is no service but the details are for VAT" in {
        val vatInvitation = FastTrackInvitation(None, Some("vrn"), Some(vrn.value), None, Some(validRegDateForVrn97))
        FastTrackInvitationNeedsService.unapply(vatInvitation) shouldBe Some(vatInvitation)
      }

      "there is no service but there is a VRN" in{
        val vatInvitation = FastTrackInvitation(None, None, Some(vrn.value), None, None)
        FastTrackInvitationNeedsService.unapply(vatInvitation) shouldBe Some(vatInvitation)
      }

      "there is no service but there is VatReg" in {
        val vatInvitation = FastTrackInvitation(None, None, None, None, Some(validRegDateForVrn97))
        FastTrackInvitationNeedsService.unapply(vatInvitation) shouldBe Some(vatInvitation)
      }
    }
  }
}
