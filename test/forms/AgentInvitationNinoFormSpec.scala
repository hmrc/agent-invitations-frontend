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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.agentInvitationNinoForm
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentInvitationUserInput
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationNinoFormSpec extends UnitSpec {

  val ninoEmptyMessage: String = "error.nino.required"
  val ninoFormatMessage: String = "enter-nino.invalid-format"
  val ninoEmptyFormError: FormError = FormError("clientIdentifier", List(ninoEmptyMessage))
  val ninoFormatFormError: FormError = FormError("clientIdentifier", List(ninoFormatMessage))

  "NinoForm" should {
    "return no error message for valid Nino" in {
      val data = Json.obj("service"-> "someService", "clientIdentifier" -> "WM123456C", "postcode" -> "")
      val ninoForm = agentInvitationNinoForm.bind(data)
      ninoForm.errors.isEmpty shouldBe true
    }

    "return no error message for valid Nino with spaces" in {
      val data = Json.obj("service" -> "someService", "clientIdentifier" -> "  WM123456C  ", "postcode" -> "")
      val ninoForm = agentInvitationNinoForm.bind(data)
      ninoForm.errors.isEmpty shouldBe true
    }

    "return no error message for valid lower case Nino" in {
      val data = Json.obj("service" -> "someService", "clientIdentifier" -> "wn123456c", "postcode" -> "")
      val ninoForm = agentInvitationNinoForm.bind(data)
      ninoForm.errors.isEmpty shouldBe true
    }

    "return an error message for invalid Nino" in {
      val data = Json.obj("service" -> "someService", "clientIdentifier" -> "12345", "postcode" -> "")
      val ninoForm = agentInvitationNinoForm.bind(data)
      ninoForm.errors.contains(ninoFormatFormError) shouldBe true
      ninoForm.errors.length shouldBe 1
    }

    "return an error message for empty form" in {
      val data = Json.obj("service" -> "someService", "clientIdentifier" -> "", "postcode" -> "")
      val ninoForm = agentInvitationNinoForm.bind(data)
      ninoForm.errors.contains(ninoEmptyFormError) shouldBe true
      ninoForm.errors.length shouldBe 1
    }

    "return no errors when unbinding the form" in {
      val unboundForm = agentInvitationNinoForm.mapping.unbind(AgentInvitationUserInput("", Some(Nino("AE123456C")), None))
      unboundForm("clientIdentifier") shouldBe "AE123456C"
    }

  }

}
