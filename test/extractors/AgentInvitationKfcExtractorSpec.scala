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

package extractors

import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.models.{UserInputNinoAndPostcode, UserInputVrnAndRegDate}
import uk.gov.hmrc.agentmtdidentifiers.model.{MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationKfcExtractorSpec extends UnitSpec {

  private val featureFlagsAllOn = new FeatureFlags(
    showHmrcMtdIt = true,
    showPersonalIncome = true,
    showHmrcMtdVat = true,
    showKfcMtdIt = true,
    showKfcPersonalIncome = true,
    showKfcMtdVat = true,
    enableFastTrack = true)

  private val featureFlagsAllOff = new FeatureFlags(
    showHmrcMtdIt = true,
    showPersonalIncome = true,
    showHmrcMtdVat = true,
    showKfcMtdIt = false,
    showKfcPersonalIncome = false,
    showKfcMtdVat = false,
    enableFastTrack = false
  )

  private val serviceITSA = "HMRC-MTD-IT"
  private val servicePIR = "PERSONAL-INCOME-RECORD"
  private val serviceVAT = "HMRC-MTD-VAT"
  private val mtdItId = MtdItId("ABCDEF123456789")
  private val vrn = Vrn("101747696")
  private val nino = Nino("AB123456A")
  val personal = Some("personal")
  val business = Some("business")

  "The ClientForMtdItWithFlagOn extractor" should {
    "Return a client identifier for HMRC-MTD-IT service when details match and KFC feature flag is on" in {
      val args = (UserInputNinoAndPostcode(personal, serviceITSA, Some(mtdItId.value), None), featureFlagsAllOn)
      ClientForMtdItWithFlagOn.unapply(args) shouldBe Some(mtdItId.value)
    }
    "Return None for HMRC-MTD-IT service when KFC feature flag is off" in {
      val args = (UserInputNinoAndPostcode(personal, serviceITSA, Some(mtdItId.value), None), featureFlagsAllOff)
      ClientForMtdItWithFlagOn.unapply(args) shouldBe None
    }
    "Return None for HMRC-MTD-IT service when details don't match and KFC feature flag is on" in {
      val args = (UserInputNinoAndPostcode(personal, serviceITSA, None, None), featureFlagsAllOn)
      ClientForMtdItWithFlagOn.unapply(args) shouldBe None
    }
  }

  "The ClientForPirWithFlagOn extractor" should {
    "Return true for PERSONAL-INCOME-RECORD service when details match and KFC feature flag is on" in {
      val args = (UserInputNinoAndPostcode(personal, servicePIR, Some(nino.value), None), featureFlagsAllOn)
      ClientForPirWithFlagOn.unapply(args) shouldBe Some(())
    }
    "Return None for PERSONAL-INCOME-RECORD service when KFC feature flag is off" in {
      val args = (UserInputNinoAndPostcode(personal, servicePIR, None, None), featureFlagsAllOff)
      ClientForPirWithFlagOn.unapply(args) shouldBe None
    }
    "Return None for PERSONAL-INCOME-RECORD service when details don't match and KFC feature flag is on" in {
      val args = (UserInputNinoAndPostcode(personal, serviceITSA, None, None), featureFlagsAllOn)
      ClientForPirWithFlagOn.unapply(args) shouldBe None
    }
  }

  "The ClientForVatWithFlagOn extractor" should {
    "Return a client identifier for HMRC-MTD-VAT service when details match and KFC feature flag is on" in {
      val args = (UserInputVrnAndRegDate(business, serviceVAT, Some(vrn.value), None), featureFlagsAllOn)
      ClientForVatWithFlagOn.unapply(args) shouldBe Some(vrn.value)
    }
    "Return None for HMRC-MTD-VAT service when KFC feature flag is off" in {
      val args = (UserInputVrnAndRegDate(business, serviceVAT, Some(vrn.value), None), featureFlagsAllOff)
      ClientForVatWithFlagOn.unapply(args) shouldBe None
    }
    "Return None for HMRC-MTD-VAT service when details don't match and KFC feature flag is on" in {
      val args = (UserInputVrnAndRegDate(business, serviceVAT, None, None), featureFlagsAllOn)
      ClientForVatWithFlagOn.unapply(args) shouldBe None
    }
  }

  "The ClientWithFlagOff" should {
    "Return true for HMRC-MTD-IT service when details match and KFC feature flag is off" in {
      val args = (UserInputNinoAndPostcode(personal, serviceITSA, Some(mtdItId.value), None), featureFlagsAllOff)
      ClientWithItsaOrPirFlagOff.unapply(args) shouldBe Some(())
    }
    "Return true for PERSONAL-INCOME-RECORD service when details match and KFC feature flag is off" in {
      val args = (UserInputNinoAndPostcode(personal, servicePIR, Some(nino.value), None), featureFlagsAllOff)
      ClientWithItsaOrPirFlagOff.unapply(args) shouldBe Some(())
    }
    "Return None for HMRC-MTD-IT service when details don't match and KFC feature flag is off" in {
      val args = (UserInputNinoAndPostcode(personal, serviceITSA, None, None), featureFlagsAllOff)
      ClientWithItsaOrPirFlagOff.unapply(args) shouldBe None
    }
    "Return None for PERSONAL-INCOME-RECORD service when details don't match and KFC feature flag is off" in {
      val args = (UserInputNinoAndPostcode(personal, servicePIR, None, None), featureFlagsAllOff)
      ClientWithItsaOrPirFlagOff.unapply(args) shouldBe None
    }
  }

  "The ClientWithVatFlagOff" should {
    "Return true for HMRC-MTD-VAT service when details match and KFC feature flag is off" in {
      val args = (UserInputVrnAndRegDate(business, serviceVAT, Some(vrn.value), None), featureFlagsAllOff)
      ClientWithVatFlagOff.unapply(args) shouldBe Some(())
    }
    "Return None for HMRC-MTD-VAT service when details don't match and KFC feature flag is off" in {
      val args = (UserInputVrnAndRegDate(business, serviceVAT, None, None), featureFlagsAllOff)
      ClientWithVatFlagOff.unapply(args) shouldBe None
    }
  }
}
