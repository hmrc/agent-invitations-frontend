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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{AgentsInvitationController, FeatureFlags}
import uk.gov.hmrc.agentinvitationsfrontend.models.UserInputNinoAndPostcode
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationPostcodeFormSpec extends UnitSpec {

  val postcodeEmptyMessage: String = "error.postcode.required"
  val postcodeFormatMessage: String = "enter-postcode.invalid-format"
  val postcodeCharactersMessage: String = "enter-postcode.invalid-characters"
  val postcodeEmptyFormError: FormError = FormError("knownFact", List(postcodeEmptyMessage))
  val postcodeFormatFormError: FormError = FormError("knownFact", List(postcodeFormatMessage))
  val postcodeCharacterFormError: FormError = FormError("knownFact", List(postcodeCharactersMessage))
  val serviceITSA = "HMRC-MTD-IT"
  val servicePIR = "PERSONAL-INCOME-RECORD"
  val personal = Some("personal")
  val business = Some("business")

  "PostCodeForm with KFC flags on" should {

    val featureFlags = FeatureFlags()
    val agentInvitationPostCodeForm = AgentsInvitationController.agentInvitationPostCodeForm(featureFlags)

    "return no error message for valid postcode" in {
      val data = Json.obj(
        "clientType"       -> "personal",
        "clientIdentifier" -> "WM123456C",
        "service"          -> serviceITSA,
        "knownFact"        -> "W12 7TQ")
      val postcodeForm = agentInvitationPostCodeForm.bind(data)
      postcodeForm.errors.isEmpty shouldBe true
    }

    "return no error message for valid postcode with spaces" in {
      val data = Json.obj(
        "clientType"       -> "personal",
        "clientIdentifier" -> "WM123456C",
        "service"          -> serviceITSA,
        "knownFact"        -> "  W12 7TQ  ")
      val postcodeForm = agentInvitationPostCodeForm.bind(data)
      postcodeForm.errors.isEmpty shouldBe true
    }

    "return no error message for valid lower case postcode" in {
      val data = Json.obj(
        "clientType"       -> "personal",
        "clientIdentifier" -> "WM123456C",
        "service"          -> serviceITSA,
        "knownFact"        -> "w12 7tq")
      val postcodeForm = agentInvitationPostCodeForm.bind(data)
      postcodeForm.errors.isEmpty shouldBe true
    }

    "return an error message for invalid format postcode" in {
      val data = Json.obj(
        "clientType"       -> "personal",
        "clientIdentifier" -> "WM123456C",
        "service"          -> serviceITSA,
        "knownFact"        -> "W12")
      val postcodeForm = agentInvitationPostCodeForm.bind(data)
      postcodeForm.errors.contains(postcodeFormatFormError) shouldBe true
      postcodeForm.errors.length shouldBe 1
    }

    "return an error message for postcode with invalid characters" in {
      val data = Json.obj(
        "clientType"       -> "personal",
        "clientIdentifier" -> "WM123456C",
        "service"          -> serviceITSA,
        "knownFact"        -> "$%$%$%")
      val postcodeForm = agentInvitationPostCodeForm.bind(data)
      postcodeForm.errors.contains(postcodeCharacterFormError) shouldBe true
      postcodeForm.errors.length shouldBe 1
    }

    "return an error message for form with empty postcode" in {
      val data = Json
        .obj("clientType" -> "personal", "clientIdentifier" -> "WM123456C", "service" -> serviceITSA, "knownFact" -> "")
      val postcodeForm = agentInvitationPostCodeForm.bind(data)
      postcodeForm.errors.contains(postcodeEmptyFormError) shouldBe true
      postcodeForm.errors.length shouldBe 1
    }

    "return no errors when unbinding the form" in {
      val unboundForm = agentInvitationPostCodeForm.mapping.unbind(
        UserInputNinoAndPostcode(personal, serviceITSA, Some("AE123456C"), Some("AA1 1AA")))
      unboundForm("knownFact") shouldBe "AA1 1AA"
    }
  }

  "PostcodeForm with Kfc flags off" should {
    val featureFlags = FeatureFlags().copy(showKfcMtdIt = false)
    val agentInvitationPostCodeForm = AgentsInvitationController.agentInvitationPostCodeForm(featureFlags)

    "return no errors when postcode is valid" in {
      val data = Json.obj(
        "clientType"       -> "personal",
        "clientIdentifier" -> "WM123456C",
        "service"          -> serviceITSA,
        "knownFact"        -> "W12 7TQ")
      val postcodeForm = agentInvitationPostCodeForm.bind(data)
      postcodeForm.errors.isEmpty shouldBe true
    }

    "return no errors when postcode is invalid" in {
      val data = Json.obj(
        "clientType"       -> "personal",
        "clientIdentifier" -> "WM123456C",
        "service"          -> serviceITSA,
        "knownFact"        -> "INVALID")
      val postcodeForm = agentInvitationPostCodeForm.bind(data)
      postcodeForm.errors.isEmpty shouldBe true
    }

    "return no errors when postcode is empty" in {
      val data = Json
        .obj("clientType" -> "personal", "clientIdentifier" -> "WM123456C", "service" -> serviceITSA, "knownFact" -> "")
      val postcodeForm = agentInvitationPostCodeForm.bind(data)
      postcodeForm.errors.isEmpty shouldBe true
    }

  }
}
