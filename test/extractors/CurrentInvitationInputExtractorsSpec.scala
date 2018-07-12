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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.models.{CurrentInvitationInput, Postcode, VatRegDate}
import uk.gov.hmrc.agentmtdidentifiers.model.{MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.test.UnitSpec

class CurrentInvitationInputExtractorsSpec extends UnitSpec {

  private val serviceITSA = "HMRC-MTD-IT"
  private val servicePIR = "PERSONAL-INCOME-RECORD"
  private val serviceVAT = "HMRC-MTD-VAT"
  private val mtdItId = MtdItId("ABCDEF123456789")
  private val vrn = Vrn("101747696")
  private val nino = Nino("AB123456A")
  private val validPostcode = "DH14EJ"
  val validRegDateForVrn97 = "2007-07-07"

  "The FastTrackInvitationItsaComplete extractor" should {
    "return Some when show-kfc-mtd-it is on and" when {
      "the service is HMRC-MTD-IT and there is a valid Nino and Postcode" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags(showKfcMtdIt = true)
        val itsaInvitation =
          CurrentInvitationInput(Some(serviceITSA), Some("ni"), Some(nino.value), Some(validPostcode))
        val fti = CurrentInvitationInputItsaReady.unapply(itsaInvitation)
        fti.map(_.service) shouldBe Some("HMRC-MTD-IT")
        fti.map(_.clientIdentifierType) shouldBe Some("ni")
        fti.map(_.clientIdentifier) shouldBe Some(nino)
        fti.flatMap(_.postcode) shouldBe Some(Postcode(validPostcode))
      }
    }

    "return Some when show-kfc-mtd-it is off and" when {
      "the service is HMRC-MTD-IT and there is a valid Nino and Postcode" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags(showKfcMtdIt = false)
        val itsaInvitation =
          CurrentInvitationInput(Some(serviceITSA), Some("ni"), Some(nino.value), Some(validPostcode))
        val fti = CurrentInvitationInputItsaReady.unapply(itsaInvitation)
        fti.map(_.service) shouldBe Some("HMRC-MTD-IT")
        fti.map(_.clientIdentifierType) shouldBe Some("ni")
        fti.map(_.clientIdentifier) shouldBe Some(nino)
        fti.flatMap(_.postcode) shouldBe None
      }

      "the service is HMRC-MTD-IT and there is a valid Nino and invalid Postcode" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags(showKfcMtdIt = false)
        val itsaInvitation =
          CurrentInvitationInput(Some(serviceITSA), Some("ni"), Some(nino.value), Some("fooPostcode"))
        val fti = CurrentInvitationInputItsaReady.unapply(itsaInvitation)
        fti.map(_.service) shouldBe Some("HMRC-MTD-IT")
        fti.map(_.clientIdentifierType) shouldBe Some("ni")
        fti.map(_.clientIdentifier) shouldBe Some(nino)
        fti.flatMap(_.postcode) shouldBe None
      }

      "the service is HMRC-MTD-IT and there is a valid Nino and empty Postcode" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags(showKfcMtdIt = false)
        val itsaInvitation = CurrentInvitationInput(Some(serviceITSA), Some("ni"), Some(nino.value), Some(""))
        val fti = CurrentInvitationInputItsaReady.unapply(itsaInvitation)
        fti.map(_.service) shouldBe Some("HMRC-MTD-IT")
        fti.map(_.clientIdentifierType) shouldBe Some("ni")
        fti.map(_.clientIdentifier) shouldBe Some(nino)
        fti.flatMap(_.postcode) shouldBe None
      }

      "the service is HMRC-MTD-IT and there is a valid Nino and missing Postcode" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags(showKfcMtdIt = false)
        val itsaInvitation = CurrentInvitationInput(Some(serviceITSA), Some("ni"), Some(nino.value), None)
        val fti = CurrentInvitationInputItsaReady.unapply(itsaInvitation)
        fti.map(_.service) shouldBe Some("HMRC-MTD-IT")
        fti.map(_.clientIdentifierType) shouldBe Some("ni")
        fti.map(_.clientIdentifier) shouldBe Some(nino)
        fti.flatMap(_.postcode) shouldBe None
      }
    }

    "return None" when {
      implicit val featureFlags: FeatureFlags = FeatureFlags()

      "there is no service" in {
        val itsaInvitation = CurrentInvitationInput(None, Some("ni"), Some(nino.value), Some(validPostcode))
        CurrentInvitationInputItsaReady.unapply(itsaInvitation) shouldBe None
      }
      "the service is not HMRC-MTD-IT" in {
        val itsaInvitation =
          CurrentInvitationInput(Some(servicePIR), Some("ni"), Some(nino.value), Some(validPostcode))
        CurrentInvitationInputItsaReady.unapply(itsaInvitation) shouldBe None
      }

      "the service is HMRC-MTD-IT but there is no client identifier type" in {
        val itsaInvitation =
          CurrentInvitationInput(Some(servicePIR), Some("ni"), Some(nino.value), Some(validPostcode))
        CurrentInvitationInputItsaReady.unapply(itsaInvitation) shouldBe None
      }

      "the service is HMRC-MTD-IT but the client identifier type is not 'ni'" in {
        val itsaInvitation =
          CurrentInvitationInput(Some(serviceITSA), None, Some(nino.value), Some(validPostcode))
        CurrentInvitationInputItsaReady.unapply(itsaInvitation) shouldBe None
      }

      "the service is HMRC-MTD-IT but there is no Nino" in {
        val itsaInvitation = CurrentInvitationInput(Some(serviceITSA), Some("ni"), None, Some(validPostcode))
        CurrentInvitationInputItsaReady.unapply(itsaInvitation) shouldBe None
      }

      "the service is HMRC-MTD-IT but the Nino is invalid" in {
        val itsaInvitation =
          CurrentInvitationInput(Some(serviceITSA), Some("ni"), Some("Invalid_Nino"), Some(validPostcode))
        CurrentInvitationInputItsaReady.unapply(itsaInvitation) shouldBe None
      }

      "the service is HMRC-MTD-IT but there is no Postcode" in {
        val itsaInvitation = CurrentInvitationInput(Some(serviceITSA), Some("ni"), Some(nino.value), None)
        CurrentInvitationInputItsaReady.unapply(itsaInvitation) shouldBe None
      }

      "the service is HMRC-MTD-IT but the Postcode is invalid" in {
        val itsaInvitation =
          CurrentInvitationInput(Some(serviceITSA), Some("ni"), Some(nino.value), Some("Invalid_Postcode"))
        CurrentInvitationInputItsaReady.unapply(itsaInvitation) shouldBe None
      }
    }
  }

  "The FastTrackInvitationIrvComplete extractor" should {
    "return Some and show-kfc-personal-income is on and" when {
      "the service is PERSONAL-INCOME-RECORD and there is a valid Nino" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags()
        val irvInvitation = CurrentInvitationInput(Some(servicePIR), Some("ni"), Some(nino.value), None)
        val fti = CurrentInvitationInputPirReady.unapply(irvInvitation)
        fti.map(_.service) shouldBe Some("PERSONAL-INCOME-RECORD")
        fti.map(_.clientIdentifierType) shouldBe Some("ni")
        fti.map(_.clientIdentifier) shouldBe Some(nino)
        fti.flatMap(_.knownFact) shouldBe None
      }
    }

    "return None" when {
      implicit val featureFlags: FeatureFlags = FeatureFlags()

      "there is no service" in {
        val irvInvitation = CurrentInvitationInput(None, Some("ni"), Some(nino.value), None)
        CurrentInvitationInputPirReady.unapply(irvInvitation) shouldBe None
      }

      "the service is not PERSONAL-INCOME-RECORD" in {
        val irvInvitation = CurrentInvitationInput(Some(serviceVAT), Some("ni"), Some(nino.value), None)
        CurrentInvitationInputPirReady.unapply(irvInvitation) shouldBe None
      }

      "the service is PERSONAL-INCOME-RECORD but there is no clientIdentifierType" in {
        val irvInvitation = CurrentInvitationInput(Some(servicePIR), None, Some(nino.value), None)
        CurrentInvitationInputPirReady.unapply(irvInvitation) shouldBe None
      }

      "the service is PERSONAL-INCOME-RECORD but there is invalid clientIdentifierType" in {
        val irvInvitation = CurrentInvitationInput(Some(servicePIR), Some("vrn"), Some(nino.value), None)
        CurrentInvitationInputPirReady.unapply(irvInvitation) shouldBe None
      }

      "the service is PERSONAL-INCOME-RECORD but there is no clientIdentifier" in {
        val irvInvitation = CurrentInvitationInput(Some(servicePIR), Some("ni"), None, None)
        CurrentInvitationInputPirReady.unapply(irvInvitation) shouldBe None
      }

      "the service is PERSONAL-INCOME-RECORD but there is invalid clientIdentifier" in {
        val irvInvitation = CurrentInvitationInput(Some(servicePIR), Some("ni"), Some("Invalid_Nino"), None)
        CurrentInvitationInputPirReady.unapply(irvInvitation) shouldBe None
      }
    }
  }

  "The FastTrackInvitationVatComplete extractor" should {
    "return Some when show-kfc-mtd-vat in on and" when {
      "the service is HMRC-MTD-VAT and there is a valid Vrn and VatRegDate" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags()
        val vatInvitation =
          CurrentInvitationInput(Some(serviceVAT), Some("vrn"), Some(vrn.value), Some(validRegDateForVrn97))
        val fti = CurrentInvitationInputVatReady.unapply(vatInvitation)
        fti.map(_.service) shouldBe Some("HMRC-MTD-VAT")
        fti.map(_.clientIdentifierType) shouldBe Some("vrn")
        fti.map(_.clientIdentifier) shouldBe Some(vrn)
        fti.flatMap(_.vatRegDate) shouldBe Some(VatRegDate(validRegDateForVrn97))
      }
    }

    "return Some when show-kfc-mtd-vat in off and" when {
      "the service is HMRC-MTD-VAT and there is a valid Vrn and VatRegDate" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags(showKfcMtdVat = true)
        val vatInvitation =
          CurrentInvitationInput(Some(serviceVAT), Some("vrn"), Some(vrn.value), Some(validRegDateForVrn97))
        val fti = CurrentInvitationInputVatReady.unapply(vatInvitation)
        fti.map(_.service) shouldBe Some("HMRC-MTD-VAT")
        fti.map(_.clientIdentifierType) shouldBe Some("vrn")
        fti.map(_.clientIdentifier) shouldBe Some(vrn)
        fti.flatMap(_.vatRegDate) shouldBe Some(VatRegDate(validRegDateForVrn97))
      }

      "the service is HMRC-MTD-VAT and there is a valid Vrn and invalid VatRegDate" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags(showKfcMtdVat = false)
        val vatInvitation =
          CurrentInvitationInput(Some(serviceVAT), Some("vrn"), Some(vrn.value), Some("2018-13-1"))
        val fti = CurrentInvitationInputVatReady.unapply(vatInvitation)
        fti.map(_.service) shouldBe Some("HMRC-MTD-VAT")
        fti.map(_.clientIdentifierType) shouldBe Some("vrn")
        fti.map(_.clientIdentifier) shouldBe Some(vrn)
        fti.flatMap(_.vatRegDate) shouldBe None
      }

      "the service is HMRC-MTD-VAT and there is a valid Vrn and empty VatRegDate" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags(showKfcMtdVat = false)
        val vatInvitation = CurrentInvitationInput(Some(serviceVAT), Some("vrn"), Some(vrn.value), Some(""))
        val fti = CurrentInvitationInputVatReady.unapply(vatInvitation)
        fti.map(_.service) shouldBe Some("HMRC-MTD-VAT")
        fti.map(_.clientIdentifierType) shouldBe Some("vrn")
        fti.map(_.clientIdentifier) shouldBe Some(vrn)
        fti.flatMap(_.vatRegDate) shouldBe None
      }

      "the service is HMRC-MTD-VAT and there is a valid Vrn and missing VatRegDate" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags(showKfcMtdVat = false)
        val vatInvitation =
          CurrentInvitationInput(Some(serviceVAT), Some("vrn"), Some(vrn.value), Some(validPostcode))
        val fti = CurrentInvitationInputVatReady.unapply(vatInvitation)
        fti.map(_.service) shouldBe Some("HMRC-MTD-VAT")
        fti.map(_.clientIdentifierType) shouldBe Some("vrn")
        fti.map(_.clientIdentifier) shouldBe Some(vrn)
        fti.flatMap(_.vatRegDate) shouldBe None
      }
    }

    "return None" when {
      implicit val featureFlags: FeatureFlags = FeatureFlags()

      "there is no service" in {
        val vatInvitation = CurrentInvitationInput(None, Some("vrn"), Some(vrn.value), Some(validRegDateForVrn97))
        CurrentInvitationInputVatReady.unapply(vatInvitation) shouldBe None
      }

      "the service is not HMRC-MTD-VAT" in {
        val vatInvitation =
          CurrentInvitationInput(Some(serviceITSA), Some("vrn"), Some(vrn.value), Some(validRegDateForVrn97))
        CurrentInvitationInputVatReady.unapply(vatInvitation) shouldBe None
      }

      "the service is HMRC-MTD-VAT but there is no clientIdentifierType" in {
        val vatInvitation =
          CurrentInvitationInput(Some(serviceVAT), None, Some(vrn.value), Some(validRegDateForVrn97))
        CurrentInvitationInputVatReady.unapply(vatInvitation) shouldBe None
      }

      "the service is HMRC-MTD-VAT but there is invalid clientIdentifierType" in {
        val vatInvitation =
          CurrentInvitationInput(Some(serviceVAT), Some("ni"), Some(vrn.value), Some(validRegDateForVrn97))
        CurrentInvitationInputVatReady.unapply(vatInvitation) shouldBe None
      }

      "the service is HMRC-MTD-VAT but there is no clientIdentifier" in {
        val vatInvitation =
          CurrentInvitationInput(Some(serviceVAT), Some("vrn"), None, Some(validRegDateForVrn97))
        CurrentInvitationInputVatReady.unapply(vatInvitation) shouldBe None
      }

      "the service is HMRC-MTD-VAT but there is invalid clientIdentifier" in {
        val vatInvitation =
          CurrentInvitationInput(Some(serviceVAT), Some("vrn"), Some("Invalid_Vrn"), Some(validRegDateForVrn97))
        CurrentInvitationInputVatReady.unapply(vatInvitation) shouldBe None
      }

      "the service is HMRC-MTD-VAT but there is no vat-reg-date" in {
        val vatInvitation = CurrentInvitationInput(Some(serviceVAT), Some("vrn"), Some(vrn.value), None)
        CurrentInvitationInputVatReady.unapply(vatInvitation) shouldBe None
      }

      "the service is HMRC-MTD-VAT but there is invalid vat-reg-date" in {
        val vatInvitation =
          CurrentInvitationInput(Some(serviceVAT), Some("vrn"), Some(vrn.value), Some("Invalid_Reg_Date"))
        CurrentInvitationInputVatReady.unapply(vatInvitation) shouldBe None
      }
    }
  }

  "The FastTrackInvitationInvalidClientIdentifier extractor" should {
    "return Some" when {
      "the service is HMRC-MTD-IT with invalid Nino" in {
        val invalidClientIdItsaInvitation =
          CurrentInvitationInput(Some(serviceITSA), Some("ni"), Some("Invalid_Nino"), None)
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdItsaInvitation) shouldBe
          Some(invalidClientIdItsaInvitation.copy(clientIdentifier = None))
      }

      "the service is HMRC-MTD-IT with no Nino" in {
        val invalidClientIdItsaInvitation = CurrentInvitationInput(Some(serviceITSA), Some("ni"), None, None)
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdItsaInvitation) shouldBe
          Some(invalidClientIdItsaInvitation.copy(clientIdentifierType = None, clientIdentifier = None))
      }

      "the service is PERSONAL-INCOME-RECORD with invalid Nino" in {
        val invalidClientIdIrvInvitation =
          CurrentInvitationInput(Some(servicePIR), Some("ni"), Some("Invalid_Nino"), None)
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdIrvInvitation) shouldBe
          Some(invalidClientIdIrvInvitation.copy(clientIdentifier = None))
      }

      "the service is PERSONAL-INCOME-RECORD with no Nino" in {
        val invalidClientIdIrvInvitation = CurrentInvitationInput(Some(servicePIR), Some("ni"), None, None)
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdIrvInvitation) shouldBe
          Some(invalidClientIdIrvInvitation.copy(clientIdentifierType = None, clientIdentifier = None))
      }

      "the service is HMRC-MTD-VAT with invalid Vrn" in {
        val invalidClientIdVatInvitation =
          CurrentInvitationInput(Some(serviceVAT), Some("vrn"), Some("Invalid_Vrn"), None)
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdVatInvitation) shouldBe
          Some(invalidClientIdVatInvitation.copy(clientIdentifier = None))
      }

      "the service is HMRC-MTD-VAT with no Vrn" in {
        val invalidClientIdVatInvitation = CurrentInvitationInput(Some(serviceVAT), Some("vrn"), None, None)
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdVatInvitation) shouldBe
          Some(invalidClientIdVatInvitation.copy(clientIdentifierType = None, clientIdentifier = None))
      }
    }

    "return None" when {
      "there is unsupported service" in {
        val invalidClientIdVatInvitation =
          CurrentInvitationInput(Some("HMRC-AS-AGENT"), None, Some("agentReferenceNumber"), None)
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdVatInvitation) shouldBe None
      }

      "there is no service" in {
        val invalidClientIdVatInvitation = CurrentInvitationInput(None, None, None, None)
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdVatInvitation) shouldBe None
      }

      "there is no service but there are other details" in {
        val invalidClientIdVatInvitation =
          CurrentInvitationInput(None, Some("vrn"), Some(vrn.value), Some(validRegDateForVrn97))
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdVatInvitation) shouldBe None

        val invalidClientIdNinoBasedInvitation =
          CurrentInvitationInput(None, Some("ni"), Some(nino.value), Some(validPostcode))
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdNinoBasedInvitation) shouldBe None
      }
    }
  }

  "The FastTrackInvitationNeedsKnownFact extractor" should {
    "return Some" when {
      "the service is HMRC-MTD-IT but there is no knownfact: postcode" in {
        val invalidClientIdNinoBasedInvitation =
          CurrentInvitationInput(Some(serviceITSA), Some("ni"), Some(nino.value), None)
        CurrentInvitationInputNeedsKnownFact.unapply(invalidClientIdNinoBasedInvitation) shouldBe Some(
          invalidClientIdNinoBasedInvitation)
      }

      "the service is HMRC-MTD-IT but there is an invalid postcode" in {
        val invalidClientIdNinoBasedInvitation =
          CurrentInvitationInput(Some(serviceITSA), Some("ni"), Some(nino.value), Some("Invalid_Postcode"))
        CurrentInvitationInputNeedsKnownFact.unapply(invalidClientIdNinoBasedInvitation) shouldBe Some(
          invalidClientIdNinoBasedInvitation.copy(knownFact = None))
      }

      "the service is HMRC-MTD-VAT but there is no knownfact: vat-reg-date" in {
        val invalidClientIdVatBasedInvitation =
          CurrentInvitationInput(Some(serviceVAT), Some("vrn"), Some(vrn.value), None)
        CurrentInvitationInputNeedsKnownFact.unapply(invalidClientIdVatBasedInvitation) shouldBe Some(
          invalidClientIdVatBasedInvitation)
      }

      "the service is HMRC-MTD-VAT but there is an invalid vat-reg-date" in {
        val invalidClientIdVatBasedInvitation =
          CurrentInvitationInput(Some(serviceVAT), Some("vrn"), Some(vrn.value), Some("Invalid_Date"))
        CurrentInvitationInputNeedsKnownFact.unapply(invalidClientIdVatBasedInvitation) shouldBe Some(
          invalidClientIdVatBasedInvitation.copy(knownFact = None))
      }
    }

    "return None" when {
      "there is no service" in {
        val invalidClientIdVatBasedInvitation = CurrentInvitationInput(None, None, None, None)
        CurrentInvitationInputNeedsKnownFact.unapply(invalidClientIdVatBasedInvitation) shouldBe None
      }

      "there is invalid service" in {
        val invalidClientIdVatBasedInvitation = CurrentInvitationInput("Invalid_Service")
        CurrentInvitationInputNeedsKnownFact.unapply(invalidClientIdVatBasedInvitation) shouldBe None
      }

      "the service is PERSONAL-INCOME-RECORD" in {
        val invalidClientIdVatBasedInvitation = CurrentInvitationInput(servicePIR)
        CurrentInvitationInputNeedsKnownFact.unapply(invalidClientIdVatBasedInvitation) shouldBe None
      }
    }
  }

  "The FastTrackInvitationNeedsService extractor" should {
    "return Some" when {

      "there is no service but the details are for ITSA" in {
        val itsaInvitation = CurrentInvitationInput(None, Some("ni"), Some(nino.value), Some(validPostcode))
        CurrentInvitationInputNeedsService.unapply(itsaInvitation) shouldBe Some(itsaInvitation)
      }

      "there is no service but there is a Nino" in {
        val itsaInvitation = CurrentInvitationInput(None, None, Some(nino.value), None)
        CurrentInvitationInputNeedsService.unapply(itsaInvitation) shouldBe Some(itsaInvitation)
      }

      "there is no service but there is a Postcode" in {
        val itsaInvitation = CurrentInvitationInput(None, None, None, Some(validPostcode))
        CurrentInvitationInputNeedsService.unapply(itsaInvitation) shouldBe Some(itsaInvitation)
      }

      "there is no service but the details are for IRV" in {
        val irvInvitation = CurrentInvitationInput(None, Some("ni"), Some(nino.value), None)
        CurrentInvitationInputNeedsService.unapply(irvInvitation) shouldBe Some(irvInvitation)
      }

      "there is no service but the details are for VAT" in {
        val vatInvitation = CurrentInvitationInput(None, Some("vrn"), Some(vrn.value), Some(validRegDateForVrn97))
        CurrentInvitationInputNeedsService.unapply(vatInvitation) shouldBe Some(vatInvitation)
      }

      "there is no service but there is a VRN" in {
        val vatInvitation = CurrentInvitationInput(None, None, Some(vrn.value), None)
        CurrentInvitationInputNeedsService.unapply(vatInvitation) shouldBe Some(vatInvitation)
      }

      "there is no service but there is VatReg" in {
        val vatInvitation = CurrentInvitationInput(None, None, None, Some(validRegDateForVrn97))
        CurrentInvitationInputNeedsService.unapply(vatInvitation) shouldBe Some(vatInvitation)
      }
    }
  }
}
