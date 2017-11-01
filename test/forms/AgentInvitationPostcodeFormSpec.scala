/*
 * Copyright 2017 HM Revenue & Customs
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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.InvitationsController.agentInvitationPostCodeForm
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationPostcodeFormSpec extends UnitSpec {

  val postcodeEmptyMessage: String = "enter-postcode.error-empty"
  val postcodeFormatMessage: String = "enter-postcode.invalid-format"
  val postcodeEmptyFormError: FormError = FormError("postcode", List(postcodeEmptyMessage))
  val postcodeFormatFormError: FormError = FormError("postcode", List(postcodeFormatMessage))

  "PostCodeForm" should {
    "return no error message for valid postcode" in {
      val data = Json.obj("nino" -> "WM123456C", "postcode" -> "W12 7TQ")
      val postcodeForm = agentInvitationPostCodeForm.bind(data)
      postcodeForm.errors.isEmpty shouldBe true
    }

    "return an error message for invalid postcode" in {
      val data = Json.obj("nino" -> "WM123456C", "postcode" -> "W12")
      val postcodeForm = agentInvitationPostCodeForm.bind(data)
      postcodeForm.errors.contains(postcodeFormatFormError) shouldBe true
      postcodeForm.errors.length shouldBe 1
    }

    "return an error message for empty form" in {
      val data = Json.obj("nino" -> "WM123456C", "postcode" -> "")
      val postcodeForm = agentInvitationPostCodeForm.bind(data)
      postcodeForm.errors.contains(postcodeEmptyFormError) shouldBe true
      postcodeForm.errors.length shouldBe 2
    }
  }
}
