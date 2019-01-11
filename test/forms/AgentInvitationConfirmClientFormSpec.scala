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

package forms

import play.api.data.FormError
import play.api.libs.json.Json
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.agentConfirmationForm
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationConfirmClientFormSpec extends UnitSpec {

  val confirmEmptyMessage: String = "error.confirm-client.required"
  val confirmEmptyFormError: FormError = FormError("accepted", List(confirmEmptyMessage))

  "NinoForm" should {
    "return no error message for selecting Yes" in {
      val data = Json.obj("accepted" -> "true")
      val ninoForm = agentConfirmationForm(confirmEmptyMessage).bind(data)
      ninoForm.errors.isEmpty shouldBe true
    }

    "return no error message for selecting No" in {
      val data = Json.obj("accepted" -> "false")
      val ninoForm = agentConfirmationForm(confirmEmptyMessage).bind(data)
      ninoForm.errors.isEmpty shouldBe true
    }

    "return an error message for not selecting an option" in {
      val data = Json.obj("accepted" -> "")
      val ninoForm = agentConfirmationForm(confirmEmptyMessage).bind(data)
      ninoForm.errors.contains(confirmEmptyFormError) shouldBe true
      ninoForm.errors.length shouldBe 1
    }
  }

}
