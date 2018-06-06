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

package forms

import play.api.data.FormError
import play.api.libs.json.Json
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.agentInvitationPostCodeForm
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{AgentsInvitationController, FeatureFlags}
import uk.gov.hmrc.agentinvitationsfrontend.models.UserInputNinoAndPostcode
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationPostcodeFormSpec extends UnitSpec {

  val postcodeEmptyMessage: String = "error.postcode.required"
  val postcodeFormatMessage: String = "enter-postcode.invalid-format"
  val postcodeEmptyFormError: FormError = FormError("postcode", List(postcodeEmptyMessage))
  val postcodeFormatFormError: FormError = FormError("postcode", List(postcodeFormatMessage))
  val serviceITSA = "HMRC-MTD-IT"
  val servicePIR = "PERSONAL-INCOME-RECORD"

  "PostCodeForm" should {

    val featureFlags = FeatureFlags()
    val agentInvitationPostCodeForm = AgentsInvitationController.agentInvitationPostCodeForm(featureFlags)

    "return no error message for valid postcode" in {
      val data = Json.obj("clientIdentifier" -> "WM123456C", "service" -> serviceITSA, "postcode" -> "W12 7TQ")
      val postcodeForm = agentInvitationPostCodeForm.bind(data)
      postcodeForm.errors.isEmpty shouldBe true
    }

    "return no error message for valid postcode with spaces" in {
      val data = Json.obj("clientIdentifier" -> "WM123456C", "service" -> serviceITSA, "postcode" -> "  W12 7TQ  ")
      val postcodeForm = agentInvitationPostCodeForm.bind(data)
      postcodeForm.errors.isEmpty shouldBe true
    }

    "return no error message for valid lower case postcode" in {
      val data = Json.obj("clientIdentifier" -> "WM123456C", "service" -> serviceITSA, "postcode" -> "w12 7tq")
      val postcodeForm = agentInvitationPostCodeForm.bind(data)
      postcodeForm.errors.isEmpty shouldBe true
    }

    "return an error message for invalid postcode" in {
      val data = Json.obj("clientIdentifier" -> "WM123456C", "service" -> serviceITSA, "postcode" -> "W12")
      val postcodeForm = agentInvitationPostCodeForm.bind(data)
      postcodeForm.errors.contains(postcodeFormatFormError) shouldBe true
      postcodeForm.errors.length shouldBe 1
    }

    "return an error message for form with empty postcode" in {
      val data = Json.obj("clientIdentifier" -> "WM123456C", "service" -> serviceITSA, "postcode" -> "")
      val postcodeForm = agentInvitationPostCodeForm.bind(data)
      postcodeForm.errors.contains(postcodeEmptyFormError) shouldBe true
      postcodeForm.errors.length shouldBe 1
    }

    "return no errors when unbinding the form" in {
      val unboundForm = agentInvitationPostCodeForm.mapping.unbind(UserInputNinoAndPostcode(serviceITSA, Some(Nino("AE123456C")), Some("AA1 1AA")))
      unboundForm("postcode") shouldBe "AA1 1AA"
    }
  }
}
