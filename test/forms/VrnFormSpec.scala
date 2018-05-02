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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.agentInvitationVrnForm
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentInvitationNinoForm, AgentInvitationVatForm}
import uk.gov.hmrc.agentmtdidentifiers.model.Vrn
import uk.gov.hmrc.play.test.UnitSpec


class VrnFormSpec extends UnitSpec {

  val validVrn97 = Vrn("101747696")
  val invalidVrn = Vrn("101747692")
  val validVrn9755 = Vrn("101747641")
  val vrnEmptyMessage: String = "error.vrn.required"
  val vrnRegexFailure: String = "enter-vrn.regex-failure"
  val vrnChecksumFailure: String = "enter-vrn.checksum-failure"
  val vrnEmptyFormError: FormError = FormError("clientIdentifier", List(vrnEmptyMessage))
  val vrnRegexFormError: FormError = FormError("clientIdentifier", List(vrnRegexFailure))
  val vrnChecksumFormError: FormError = FormError("clientIdentifier", List(vrnChecksumFailure))

  "VrnForm" should {
    "return no error message for valid Vrn" in {
      val data = Json.obj("service"-> "someService", "clientIdentifier" -> s"${validVrn97.value}", "postcode" -> "")
      val vrnForm = agentInvitationVrnForm.bind(data)
      vrnForm.errors.isEmpty shouldBe true
    }

    "return no error message for valid vrn with spaces" in {
      val data = Json.obj("service" -> "someService", "clientIdentifier" -> s"  ${validVrn97.value}  ", "postcode" -> "")
      val vrnForm = agentInvitationVrnForm.bind(data)
      vrnForm.errors.isEmpty shouldBe true
    }

    "return an error message for invalid vrn" in {
      val data = Json.obj("service" -> "someService", "clientIdentifier" -> s"${invalidVrn.value}", "postcode" -> "")
      val vrnForm = agentInvitationVrnForm.bind(data)
      vrnForm.errors.contains(vrnChecksumFormError) shouldBe true
      vrnForm.errors.length shouldBe 1
    }

    "return an error message for invalid vrn less than 9 digits" in {
      val data = Json.obj("service" -> "someService", "clientIdentifier" -> "12345", "postcode" -> "")
      val vrnForm = agentInvitationVrnForm.bind(data)
      vrnForm.errors.contains(vrnRegexFormError) shouldBe true
      vrnForm.errors.length shouldBe 1
    }

    "return an error message for empty form" in {
      val data = Json.obj("service" -> "someService", "clientIdentifier" -> "", "postcode" -> "")
      val vrnForm = agentInvitationVrnForm.bind(data)
      vrnForm.errors.contains(vrnEmptyFormError) shouldBe true
      vrnForm.errors.length shouldBe 1
    }

    "return no errors when unbinding the form" in {
      val unboundForm = agentInvitationVrnForm.mapping.unbind(AgentInvitationVatForm("", Some(validVrn97), None))
      unboundForm("clientIdentifier") shouldBe validVrn97.value
    }

  }


}
