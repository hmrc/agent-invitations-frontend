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
import uk.gov.hmrc.agentinvitationsfrontend.models.{CurrentInvitationInput, DOB, Postcode, VatRegDate}
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
  val dateOfBirth = "1980-07-07"
  val validRegDateForVrn97 = "2007-07-07"
  val personal = Some("personal")
  val business = Some("business")

  "The CurrentInvitationInputItsaReady extractor" should {
    "return Some when show-kfc-mtd-it is on and" when {
      "the service is HMRC-MTD-IT and there is a valid Nino and Postcode" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags(showKfcMtdIt = true)
        val itsaInvitation =
          CurrentInvitationInput(personal, serviceITSA, "ni", nino.value, Some(validPostcode))
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
          CurrentInvitationInput(personal, serviceITSA, "ni", nino.value, Some(validPostcode))
        val fti = CurrentInvitationInputItsaReady.unapply(itsaInvitation)
        fti.map(_.service) shouldBe Some("HMRC-MTD-IT")
        fti.map(_.clientIdentifierType) shouldBe Some("ni")
        fti.map(_.clientIdentifier) shouldBe Some(nino)
        fti.flatMap(_.postcode) shouldBe None
      }

      "the service is HMRC-MTD-IT and there is a valid Nino and invalid Postcode" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags(showKfcMtdIt = false)
        val itsaInvitation =
          CurrentInvitationInput(personal, serviceITSA, "ni", nino.value, Some("fooPostcode"))
        val fti = CurrentInvitationInputItsaReady.unapply(itsaInvitation)
        fti.map(_.service) shouldBe Some("HMRC-MTD-IT")
        fti.map(_.clientIdentifierType) shouldBe Some("ni")
        fti.map(_.clientIdentifier) shouldBe Some(nino)
        fti.flatMap(_.postcode) shouldBe None
      }

      "the service is HMRC-MTD-IT and there is a valid Nino and empty Postcode" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags(showKfcMtdIt = false)
        val itsaInvitation = CurrentInvitationInput(personal, serviceITSA, "ni", nino.value, Some(""))
        val fti = CurrentInvitationInputItsaReady.unapply(itsaInvitation)
        fti.map(_.service) shouldBe Some("HMRC-MTD-IT")
        fti.map(_.clientIdentifierType) shouldBe Some("ni")
        fti.map(_.clientIdentifier) shouldBe Some(nino)
        fti.flatMap(_.postcode) shouldBe None
      }

      "the service is HMRC-MTD-IT and there is a valid Nino and missing Postcode" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags(showKfcMtdIt = false)
        val itsaInvitation = CurrentInvitationInput(personal, serviceITSA, "ni", nino.value, None)
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
        val itsaInvitation = CurrentInvitationInput(None, "", "ni", nino.value, Some(validPostcode))
        CurrentInvitationInputItsaReady.unapply(itsaInvitation) shouldBe None
      }
      "the service is not HMRC-MTD-IT" in {
        val itsaInvitation =
          CurrentInvitationInput(None, servicePIR, "ni", nino.value, Some(validPostcode))
        CurrentInvitationInputItsaReady.unapply(itsaInvitation) shouldBe None
      }

      "the service is HMRC-MTD-IT but there is no client identifier type" in {
        val itsaInvitation =
          CurrentInvitationInput(None, servicePIR, "ni", nino.value, Some(validPostcode))
        CurrentInvitationInputItsaReady.unapply(itsaInvitation) shouldBe None
      }

      "the service is HMRC-MTD-IT but the client identifier type is not 'ni'" in {
        val itsaInvitation =
          CurrentInvitationInput(None, serviceITSA, "", nino.value, Some(validPostcode))
        CurrentInvitationInputItsaReady.unapply(itsaInvitation) shouldBe None
      }

      "the service is HMRC-MTD-IT but there is no Nino" in {
        val itsaInvitation = CurrentInvitationInput(None, serviceITSA, "ni", "", Some(validPostcode))
        CurrentInvitationInputItsaReady.unapply(itsaInvitation) shouldBe None
      }

      "the service is HMRC-MTD-IT but the Nino is invalid" in {
        val itsaInvitation =
          CurrentInvitationInput(None, serviceITSA, "ni", "Invalid_Nino", Some(validPostcode))
        CurrentInvitationInputItsaReady.unapply(itsaInvitation) shouldBe None
      }

      "the service is HMRC-MTD-IT but there is no Postcode" in {
        val itsaInvitation = CurrentInvitationInput(None, serviceITSA, "ni", nino.value, None)
        CurrentInvitationInputItsaReady.unapply(itsaInvitation) shouldBe None
      }

      "the service is HMRC-MTD-IT but the Postcode is invalid" in {
        val itsaInvitation =
          CurrentInvitationInput(None, serviceITSA, "ni", nino.value, Some("Invalid_Postcode"))
        CurrentInvitationInputItsaReady.unapply(itsaInvitation) shouldBe None
      }
    }
  }

  "The CurrentInvitationInputPirReady extractor" should {
    "return Some and show-kfc-personal-income is on and" when {
      "the service is PERSONAL-INCOME-RECORD and there is a valid Nino" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags()
        val irvInvitation = CurrentInvitationInput(personal, servicePIR, "ni", nino.value, Some(dateOfBirth))
        val fti = CurrentInvitationInputPirReady.unapply(irvInvitation)
        fti.map(_.service) shouldBe Some("PERSONAL-INCOME-RECORD")
        fti.map(_.clientIdentifierType) shouldBe Some("ni")
        fti.map(_.clientIdentifier) shouldBe Some(nino)
        fti.flatMap(_.knownFact) shouldBe Some(DOB(dateOfBirth))
      }
    }

    "return None" when {
      implicit val featureFlags: FeatureFlags = FeatureFlags()

      "there is no service" in {
        val irvInvitation = CurrentInvitationInput(None, "", "ni", nino.value, None)
        CurrentInvitationInputPirReady.unapply(irvInvitation) shouldBe None
      }

      "the service is not PERSONAL-INCOME-RECORD" in {
        val irvInvitation = CurrentInvitationInput(None, serviceVAT, "ni", nino.value, None)
        CurrentInvitationInputPirReady.unapply(irvInvitation) shouldBe None
      }

      "the service is PERSONAL-INCOME-RECORD but there is no clientIdentifierType" in {
        val irvInvitation = CurrentInvitationInput(None, servicePIR, "", nino.value, None)
        CurrentInvitationInputPirReady.unapply(irvInvitation) shouldBe None
      }

      "the service is PERSONAL-INCOME-RECORD but there is invalid clientIdentifierType" in {
        val irvInvitation = CurrentInvitationInput(None, servicePIR, "vrn", nino.value, None)
        CurrentInvitationInputPirReady.unapply(irvInvitation) shouldBe None
      }

      "the service is PERSONAL-INCOME-RECORD but there is no clientIdentifier" in {
        val irvInvitation = CurrentInvitationInput(None, servicePIR, "ni", "", None)
        CurrentInvitationInputPirReady.unapply(irvInvitation) shouldBe None
      }

      "the service is PERSONAL-INCOME-RECORD but there is invalid clientIdentifier" in {
        val irvInvitation = CurrentInvitationInput(None, servicePIR, "ni", "Invalid_Nino", None)
        CurrentInvitationInputPirReady.unapply(irvInvitation) shouldBe None
      }
    }
  }

  "The CurrentInvitationInputVatReady extractor" should {
    "return Some when show-kfc-mtd-vat in on and" when {
      "the service is HMRC-MTD-VAT and there is a valid Vrn and VatRegDate" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags()
        val vatInvitation =
          CurrentInvitationInput(business, serviceVAT, "vrn", vrn.value, Some(validRegDateForVrn97))
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
          CurrentInvitationInput(business, serviceVAT, "vrn", vrn.value, Some(validRegDateForVrn97))
        val fti = CurrentInvitationInputVatReady.unapply(vatInvitation)
        fti.map(_.service) shouldBe Some("HMRC-MTD-VAT")
        fti.map(_.clientIdentifierType) shouldBe Some("vrn")
        fti.map(_.clientIdentifier) shouldBe Some(vrn)
        fti.flatMap(_.vatRegDate) shouldBe Some(VatRegDate(validRegDateForVrn97))
      }

      "the service is HMRC-MTD-VAT and there is a valid Vrn and invalid VatRegDate" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags(showKfcMtdVat = false)
        val vatInvitation =
          CurrentInvitationInput(business, serviceVAT, "vrn", vrn.value, Some("2018-13-1"))
        val fti = CurrentInvitationInputVatReady.unapply(vatInvitation)
        fti.map(_.service) shouldBe Some("HMRC-MTD-VAT")
        fti.map(_.clientIdentifierType) shouldBe Some("vrn")
        fti.map(_.clientIdentifier) shouldBe Some(vrn)
        fti.flatMap(_.vatRegDate) shouldBe None
      }

      "the service is HMRC-MTD-VAT and there is a valid Vrn and empty VatRegDate" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags(showKfcMtdVat = false)
        val vatInvitation = CurrentInvitationInput(business, serviceVAT, "vrn", vrn.value, Some(""))
        val fti = CurrentInvitationInputVatReady.unapply(vatInvitation)
        fti.map(_.service) shouldBe Some("HMRC-MTD-VAT")
        fti.map(_.clientIdentifierType) shouldBe Some("vrn")
        fti.map(_.clientIdentifier) shouldBe Some(vrn)
        fti.flatMap(_.vatRegDate) shouldBe None
      }

      "the service is HMRC-MTD-VAT and there is a valid Vrn and missing VatRegDate" in {
        implicit val featureFlags: FeatureFlags = FeatureFlags(showKfcMtdVat = false)
        val vatInvitation =
          CurrentInvitationInput(business, serviceVAT, "vrn", vrn.value, Some(validPostcode))
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
        val vatInvitation = CurrentInvitationInput(None, "", "vrn", vrn.value, Some(validRegDateForVrn97))
        CurrentInvitationInputVatReady.unapply(vatInvitation) shouldBe None
      }

      "the service is not HMRC-MTD-VAT" in {
        val vatInvitation =
          CurrentInvitationInput(None, serviceITSA, "vrn", vrn.value, Some(validRegDateForVrn97))
        CurrentInvitationInputVatReady.unapply(vatInvitation) shouldBe None
      }

      "the service is HMRC-MTD-VAT but there is no clientIdentifierType" in {
        val vatInvitation =
          CurrentInvitationInput(None, serviceVAT, "", vrn.value, Some(validRegDateForVrn97))
        CurrentInvitationInputVatReady.unapply(vatInvitation) shouldBe None
      }

      "the service is HMRC-MTD-VAT but there is invalid clientIdentifierType" in {
        val vatInvitation =
          CurrentInvitationInput(None, serviceVAT, "ni", vrn.value, Some(validRegDateForVrn97))
        CurrentInvitationInputVatReady.unapply(vatInvitation) shouldBe None
      }

      "the service is HMRC-MTD-VAT but there is no clientIdentifier" in {
        val vatInvitation =
          CurrentInvitationInput(None, serviceVAT, "vrn", "", Some(validRegDateForVrn97))
        CurrentInvitationInputVatReady.unapply(vatInvitation) shouldBe None
      }

      "the service is HMRC-MTD-VAT but there is invalid clientIdentifier" in {
        val vatInvitation =
          CurrentInvitationInput(None, serviceVAT, "vrn", "Invalid_Vrn", Some(validRegDateForVrn97))
        CurrentInvitationInputVatReady.unapply(vatInvitation) shouldBe None
      }

      "the service is HMRC-MTD-VAT but there is no vat-reg-date" in {
        val vatInvitation = CurrentInvitationInput(None, serviceVAT, "vrn", vrn.value, None)
        CurrentInvitationInputVatReady.unapply(vatInvitation) shouldBe None
      }

      "the service is HMRC-MTD-VAT but there is invalid vat-reg-date" in {
        val vatInvitation =
          CurrentInvitationInput(None, serviceVAT, "vrn", vrn.value, Some("Invalid_Reg_Date"))
        CurrentInvitationInputVatReady.unapply(vatInvitation) shouldBe None
      }
    }
  }

  "The CurrentInvitationInputNeedsClientIdentifier extractor" should {
    "return Some" when {
      "the service is HMRC-MTD-IT with invalid Nino" in {
        val invalidClientIdItsaInvitation =
          CurrentInvitationInput(None, serviceITSA, "ni", "Invalid_Nino", None)
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdItsaInvitation) shouldBe
          Some(invalidClientIdItsaInvitation.copy(clientIdentifier = ""))
      }

      "the service is HMRC-MTD-IT with no Nino" in {
        val invalidClientIdItsaInvitation = CurrentInvitationInput(None, serviceITSA, "ni", "", None)
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdItsaInvitation) shouldBe
          Some(invalidClientIdItsaInvitation.copy(clientIdentifier = ""))
      }

      "the service is PERSONAL-INCOME-RECORD with invalid Nino" in {
        val invalidClientIdIrvInvitation =
          CurrentInvitationInput(None, servicePIR, "ni", "Invalid_Nino", None)
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdIrvInvitation) shouldBe
          Some(invalidClientIdIrvInvitation.copy(clientIdentifier = ""))
      }

      "the service is PERSONAL-INCOME-RECORD with no Nino" in {
        val invalidClientIdIrvInvitation = CurrentInvitationInput(None, servicePIR, "ni", "", None)
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdIrvInvitation) shouldBe
          Some(invalidClientIdIrvInvitation.copy(clientIdentifier = ""))
      }

      "the service is HMRC-MTD-VAT with invalid Vrn" in {
        val invalidClientIdVatInvitation =
          CurrentInvitationInput(None, serviceVAT, "vrn", "Invalid_Vrn", None)
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdVatInvitation) shouldBe
          Some(invalidClientIdVatInvitation.copy(clientIdentifier = ""))
      }

      "the service is HMRC-MTD-VAT with no Vrn" in {
        val invalidClientIdVatInvitation = CurrentInvitationInput(None, serviceVAT, "vrn", "", None)
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdVatInvitation) shouldBe
          Some(invalidClientIdVatInvitation.copy(clientIdentifier = ""))
      }
    }

    "return None" when {
      "there is unsupported service" in {
        val invalidClientIdVatInvitation =
          CurrentInvitationInput(None, "HMRC-AS-AGENT", "", "agentReferenceNumber", None)
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdVatInvitation) shouldBe None
      }

      "there is no service" in {
        val invalidClientIdVatInvitation = CurrentInvitationInput(None, "", "", "", None)
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdVatInvitation) shouldBe None
      }

      "there is no service but there are other details" in {
        val invalidClientIdVatInvitation =
          CurrentInvitationInput(None, "", "vrn", vrn.value, Some(validRegDateForVrn97))
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdVatInvitation) shouldBe None

        val invalidClientIdNinoBasedInvitation =
          CurrentInvitationInput(None, "", "ni", nino.value, Some(validPostcode))
        CurrentInvitationInputNeedsClientIdentifier.unapply(invalidClientIdNinoBasedInvitation) shouldBe None
      }
    }
  }

  "The CurrentInvitationInputNeedsKnownFact extractor" should {
    "return Some" when {
      "the service is HMRC-MTD-IT but there is no knownfact: postcode" in {
        val invalidClientIdNinoBasedInvitation =
          CurrentInvitationInput(personal, serviceITSA, "ni", nino.value, None)
        CurrentInvitationInputNeedsKnownFact.unapply(invalidClientIdNinoBasedInvitation) shouldBe Some(
          invalidClientIdNinoBasedInvitation)
      }

      "the service is HMRC-MTD-IT but there is an invalid postcode" in {
        val invalidClientIdNinoBasedInvitation =
          CurrentInvitationInput(personal, serviceITSA, "ni", nino.value, Some("Invalid_Postcode"))
        CurrentInvitationInputNeedsKnownFact.unapply(invalidClientIdNinoBasedInvitation) shouldBe Some(
          invalidClientIdNinoBasedInvitation.copy(knownFact = None))
      }

      "the service is HMRC-MTD-VAT but there is no knownfact: vat-reg-date" in {
        val invalidClientIdVatBasedInvitation =
          CurrentInvitationInput(business, serviceVAT, "vrn", vrn.value, None)
        CurrentInvitationInputNeedsKnownFact.unapply(invalidClientIdVatBasedInvitation) shouldBe
          Some(invalidClientIdVatBasedInvitation)
      }

      "the service is PERSONAL-INCOME-RECORD but there is no knownfact: date-of-birth" in {
        val invalidClientIdVatBasedInvitation =
          CurrentInvitationInput(personal, servicePIR, "ni", nino.value, None)
        CurrentInvitationInputNeedsKnownFact.unapply(invalidClientIdVatBasedInvitation) shouldBe
          Some(invalidClientIdVatBasedInvitation)
      }

      "the service is HMRC-MTD-VAT but there is an invalid vat-reg-date" in {
        val invalidClientIdVatBasedInvitation =
          CurrentInvitationInput(business, serviceVAT, "vrn", vrn.value, Some("Invalid_Date"))
        CurrentInvitationInputNeedsKnownFact.unapply(invalidClientIdVatBasedInvitation) shouldBe Some(
          invalidClientIdVatBasedInvitation.copy(knownFact = None))
      }
    }

    "return None" when {
      "there is no service" in {
        val invalidNoServiceBasedInvitation = CurrentInvitationInput(None, "", "", "", None)
        CurrentInvitationInputNeedsKnownFact.unapply(invalidNoServiceBasedInvitation) shouldBe None
      }

      "there is invalid service" in {
        val invalidServiceBasedInvitation = CurrentInvitationInput(Some("Invalid_Client_Type"), "Invalid_Service")
        CurrentInvitationInputNeedsKnownFact.unapply(invalidServiceBasedInvitation) shouldBe None
      }
    }
  }

  "The CurrentInvitationInputNeedService extractor" should {
    "return Some" when {
      "service is missing" in {
        val missingServiceInvitation =
          CurrentInvitationInput(personal, "", "", "", None)
        CurrentInvitationInputNeedService.unapply(missingServiceInvitation) shouldBe Some(missingServiceInvitation)
      }
    }

//    TODO Bring back when mandatory
//    "return None" when {
//      "there is no client-type" in {
//        val noClientTypeInvitation =
//          CurrentInvitationInput(None, "", "", "", None)
//        CurrentInvitationInputNeedService.unapply(noClientTypeInvitation) shouldBe None
//      }
//
//      "there is invalid client-type" in {
//        val invalidClientTypeInvitation =
//          CurrentInvitationInput(Some("Invalid_Client_Type"), "", "", "", None)
//        CurrentInvitationInputNeedService.unapply(invalidClientTypeInvitation) shouldBe None
//      }
//
//      "there is invalid client-type: empty String" in {
//        val invalidClientTypeInvitation =
//          CurrentInvitationInput(Some(""), "", "", "", None)
//        CurrentInvitationInputNeedService.unapply(invalidClientTypeInvitation) shouldBe None
//      }
//    }
  }
}
