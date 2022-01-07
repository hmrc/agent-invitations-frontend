/*
 * Copyright 2022 HM Revenue & Customs
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
import uk.gov.hmrc.agentinvitationsfrontend.forms.ServiceTypeForm
import support.UnitSpec

class AgentInvitationServiceFormSpec extends UnitSpec {

  val serviceEmptyMessage: String = "service.type.invalid"
  val serviceEmptyFormError: FormError = FormError("serviceType", List(serviceEmptyMessage))
  val serviceITSA = "HMRC-MTD-IT"
  val servicePIR = "PERSONAL-INCOME-RECORD"
  val serviceVAT = "HMRC-MTD-VAT"

  "ServiceForm" should {
    "return no error message for valid service ITSA" in {
      val data =
        Json.obj("serviceType" -> serviceITSA)
      val serviceForm = ServiceTypeForm.form.bind(data)
      serviceForm.errors.isEmpty shouldBe true
    }

    "return no error message for valid service PIR" in {
      val data =
        Json.obj("serviceType" -> servicePIR)
      val serviceForm = ServiceTypeForm.form.bind(data)
      serviceForm.errors.isEmpty shouldBe true
    }

    "return no error message for valid service VAT" in {
      val data =
        Json.obj("serviceType" -> serviceVAT)
      val serviceForm = ServiceTypeForm.form.bind(data)
      serviceForm.errors.isEmpty shouldBe true
    }

    "return an error message for form with empty service" in {
      val data = Json.obj("serviceType" -> "")
      val serviceForm = ServiceTypeForm.form.bind(data)
      serviceForm.errors.contains(serviceEmptyFormError) shouldBe true
      serviceForm.errors.length shouldBe 1
    }
  }

}
