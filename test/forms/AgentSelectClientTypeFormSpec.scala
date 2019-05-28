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
import uk.gov.hmrc.agentinvitationsfrontend.forms.ClientTypeForm
import uk.gov.hmrc.play.test.UnitSpec

class AgentSelectClientTypeFormSpec extends UnitSpec {

  val clientTypeEmptyMessage: String = "client.type.invalid"
  val clientTypeEmptyFormError: FormError = FormError("clientType", List(clientTypeEmptyMessage))

  "ClientType Form" should {
    "return no error message for valid clientType Personal" in {
      val data = Json.obj("clientType" -> "personal")
      val clientTypeForm = ClientTypeForm.form.bind(data)
      clientTypeForm.errors.isEmpty shouldBe true
    }

    "return no error message for valid clientType Business" in {
      val data = Json.obj("clientType" -> "business")
      val clientTypeForm = ClientTypeForm.form.bind(data)
      clientTypeForm.errors.isEmpty shouldBe true
    }

    "return an error message for form with empty clientType" in {
      val data = Json.obj("clientType" -> "")
      val clientTypeForm = ClientTypeForm.form.bind(data)
      clientTypeForm.errors.contains(clientTypeEmptyFormError) shouldBe true
      clientTypeForm.errors.length shouldBe 1
    }
  }
}
