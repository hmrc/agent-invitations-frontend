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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.agentInvitationSelectClientTypeForm
import uk.gov.hmrc.agentinvitationsfrontend.models.UserInputNinoAndPostcode
import uk.gov.hmrc.play.test.UnitSpec

class AgentClientTypeFormSpec extends UnitSpec {

  val clientTypeEmptyMessage: String = "error.client-type.required"
  val clientTypeEmptyFormError: FormError = FormError("clientType", List(clientTypeEmptyMessage))
  val serviceITSA = "HMRC-MTD-IT"
  val servicePIR = "PERSONAL-INCOME-RECORD"
  val serviceVAT = "HMRC-MTD-VAT"
  val personal = Some("personal")
  val business = Some("business")

  "ClientType Form" should {
    "return no error message for valid clientType Personal" in {
      val data = Json.obj("clientType" -> "personal", "service" -> "", "clientIdentifier" -> "", "knownFact" -> "")
      val clientTypeForm = agentInvitationSelectClientTypeForm.bind(data)
      clientTypeForm.errors.isEmpty shouldBe true
    }

    "return no error message for valid clientType Business" in {
      val data = Json.obj("clientType" -> "personal", "service" -> "", "clientIdentifier" -> "", "knownFact" -> "")
      val clientTypeForm = agentInvitationSelectClientTypeForm.bind(data)
      clientTypeForm.errors.isEmpty shouldBe true
    }

    "return an error message for form with empty clientType" in {
      val data = Json.obj("clientType" -> "", "service" -> "", "clientIdentifier" -> "", "knownFact" -> "")
      val clientTypeForm = agentInvitationSelectClientTypeForm.bind(data)
      clientTypeForm.errors.contains(clientTypeEmptyFormError) shouldBe true
      clientTypeForm.errors.length shouldBe 1
    }

    "return no errors when unbinding the form" in {
      val unboundFormITSA =
        agentInvitationSelectClientTypeForm.mapping.unbind(
          UserInputNinoAndPostcode(personal, serviceITSA, Some("AE123456C"), None))
      unboundFormITSA("clientType") shouldBe "personal"

      val unboundFormAFI =
        agentInvitationSelectClientTypeForm.mapping.unbind(
          UserInputNinoAndPostcode(personal, servicePIR, Some("AE123456C"), None))
      unboundFormAFI("clientType") shouldBe "personal"

      val unboundFormVAT =
        agentInvitationSelectClientTypeForm.mapping.unbind(
          UserInputNinoAndPostcode(business, serviceVAT, Some("101747696"), None))
      unboundFormVAT("clientType") shouldBe "business"
    }
  }
}
