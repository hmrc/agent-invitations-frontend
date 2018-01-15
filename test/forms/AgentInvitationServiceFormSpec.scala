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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentInvitationUserInput
import uk.gov.hmrc.agentmtdidentifiers.model.Vrn
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationServiceFormSpec extends UnitSpec{
  
  val serviceEmptyMessage: String = "error.service.required"
  val serviceEmptyFormError: FormError = FormError("service", List(serviceEmptyMessage))
  val serviceITSA = "HMRC-MTD-IT"
  val servicePIR = "PERSONAL-INCOME-RECORD"
  val serviceVAT = "HMRC-MTD-VAT"

  "ServiceForm" should {
    "return no error message for valid service ITSA" in {
      val data = Json.obj("service" -> serviceITSA, "taxIdentifier" -> "", "postcode" -> "")
      val serviceForm = agentInvitationServiceForm.bind(data)
      serviceForm.errors.isEmpty shouldBe true
    }

    "return no error message for valid service PIR" in {
      val data = Json.obj("service" -> servicePIR, "taxIdentifier" -> "", "postcode" -> "")
      val serviceForm = agentInvitationServiceForm.bind(data)
      serviceForm.errors.isEmpty shouldBe true
    }

    "return no error message for valid service VAT" in {
      val data = Json.obj("service" -> serviceVAT, "taxIdentifier" -> "", "postcode" -> "")
      val serviceForm = agentInvitationServiceForm.bind(data)
      serviceForm.errors.isEmpty shouldBe true
    }

    "return an error message for form with empty service" in {
      val data = Json.obj("service" -> "", "taxIdentifier" -> "", "postcode" -> "")
      val serviceForm = agentInvitationServiceForm.bind(data)
      serviceForm.errors.contains(serviceEmptyFormError) shouldBe true
      serviceForm.errors.length shouldBe 1
    }

    "return no errors when unbinding the form" in {
      val unboundFormITSA = agentInvitationServiceForm.mapping.unbind(AgentInvitationUserInput(serviceITSA, Some(Nino("AE123456C")), None))
      unboundFormITSA("service") shouldBe serviceITSA

      val unboundFormAFI = agentInvitationServiceForm.mapping.unbind(AgentInvitationUserInput(servicePIR, Some(Nino("AE123456C")), None))
      unboundFormAFI("service") shouldBe servicePIR

      val unboundFormVAT = agentInvitationServiceForm.mapping.unbind(AgentInvitationUserInput(serviceVAT, Some(Vrn("101747696")), None))
      unboundFormVAT("service") shouldBe serviceVAT
    }
  }

}
