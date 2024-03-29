/*
 * Copyright 2023 HM Revenue & Customs
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
import uk.gov.hmrc.agentinvitationsfrontend.forms.ClientTypeForm
import support.UnitSpec

class AgentSelectClientTypeFormSpec extends UnitSpec {

  val authorisationClientTypeEmptyMessage: String = "error.client-type.empty"
  val deauthorisationClientTypeEmptyMessage: String = "error.cancel-authorisation.client-type.empty"
  def clientTypeEmptyFormError(emptyError: String): FormError = FormError("clientType", List(emptyError))

  "ClientType Form" should {
    "return no error message for valid clientType personal" in {
      val data = Json.obj("clientType" -> "personal")
      val clientTypeWithTrustsForm = ClientTypeForm.authorisationForm.bind(data)
      clientTypeWithTrustsForm.errors.isEmpty shouldBe true
    }

    "return no error message for valid clientType business" in {
      val data = Json.obj("clientType" -> "business")
      val clientTypeWithTrustsForm = ClientTypeForm.authorisationForm.bind(data)
      clientTypeWithTrustsForm.errors.isEmpty shouldBe true
    }

    "return no error message for valid clientType trust" in {
      val data = Json.obj("clientType" -> "trust")
      val clientTypeWithTrustsForm = ClientTypeForm.authorisationForm.bind(data)
      clientTypeWithTrustsForm.errors.isEmpty shouldBe true
    }

    "return an error message for an authorisation form with empty clientType" in {
      val data = Json.obj("clientType" -> "")
      val clientTypeWithTrustsForm = ClientTypeForm.authorisationForm.bind(data)
      clientTypeWithTrustsForm.errors.contains(clientTypeEmptyFormError(authorisationClientTypeEmptyMessage)) shouldBe true
      clientTypeWithTrustsForm.errors.length shouldBe 1
    }

    "return an error message for a deauthorisation form with empty clientType" in {
      val data = Json.obj("clientType" -> "")
      val clientTypeWithTrustsForm = ClientTypeForm.deAuthorisationForm.bind(data)
      clientTypeWithTrustsForm.errors.contains(clientTypeEmptyFormError(deauthorisationClientTypeEmptyMessage)) shouldBe true
      clientTypeWithTrustsForm.errors.length shouldBe 1
    }
  }
}
