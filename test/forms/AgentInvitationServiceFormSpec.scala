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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentInvitationUserInput
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationServiceFormSpec extends UnitSpec{
  
  val serviceEmptyMessage: String = "error.service.required"
  val serviceEmptyFormError: FormError = FormError("service", List(serviceEmptyMessage))
  val serviceITSA = "HMRC-MTD-IT"
  val servicePIR = "personal-income-record"

  "ServiceForm" should {
    "return no error message for valid service ITSA" in {
      val data = Json.obj("nino" -> "WM123456C", "service" -> serviceITSA, "postcode" -> "")
      val serviceForm = agentInvitationServiceForm.bind(data)
      serviceForm.errors.isEmpty shouldBe true
    }

    "return no error message for valid service PIR" in {
      val data = Json.obj("nino" -> "WM123456C", "service" -> servicePIR, "postcode" -> "")
      val serviceForm = agentInvitationServiceForm.bind(data)
      serviceForm.errors.isEmpty shouldBe true
    }

    "return an error message for form with empty service" in {
      val data = Json.obj("nino" -> "WM123456C", "service" -> "", "postcode" -> "")
      val serviceForm = agentInvitationServiceForm.bind(data)
      serviceForm.errors.contains(serviceEmptyFormError) shouldBe true
      serviceForm.errors.length shouldBe 1
    }

    "return an error message for form with missing service" in {
      val data = Json.obj("nino" -> "WM123456C", "postcode" -> "")
      val serviceForm = agentInvitationServiceForm.bind(data)
      serviceForm.errors.contains(serviceEmptyFormError) shouldBe true
      serviceForm.errors.length shouldBe 1
    }

    "return no errors when unbinding the form" in {
      val unboundForm = agentInvitationServiceForm.mapping.unbind(AgentInvitationUserInput(Nino("AE123456C"), Some(serviceITSA), ""))
      unboundForm("service") shouldBe serviceITSA
    }
  }

}
