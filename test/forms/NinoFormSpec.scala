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
import uk.gov.hmrc.agentinvitationsfrontend.form.NinoForm
import uk.gov.hmrc.play.test.UnitSpec

class NinoFormSpec extends UnitSpec {

  val ninoEmptyMessage: String = "enter-nino.error-empty"
  val ninoFormatMessage: String = "enter-nino.invalid-format"
  val ninoEmptyFormError: FormError = FormError("nino", List(ninoEmptyMessage))
  val ninoFormatFormError: FormError = FormError("nino", List(ninoFormatMessage))

  "NinoForm" should {
    "return no error message for valid Nino" in {
      val data = Json.obj("nino" -> "WM123456C")
      val ninoForm = NinoForm.ninoForm.bind(data)
      ninoForm.errors.isEmpty shouldBe true
    }

    "return an error message for invalid Nino" in {
      val data = Json.obj("nino" -> "12345")
      val ninoForm = NinoForm.ninoForm.bind(data)
      ninoForm.errors.contains(ninoFormatFormError) shouldBe true
      ninoForm.errors.length shouldBe 1
    }

    "return an error message for empty form" in {
      val data = Json.obj("nino" -> "")
      val ninoForm = NinoForm.ninoForm.bind(data)
      ninoForm.errors.contains(ninoEmptyFormError) shouldBe true
      ninoForm.errors.length shouldBe 2
    }
  }

}
