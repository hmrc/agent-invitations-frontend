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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.agentInvitationIdentifyClientFormIrv
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.models.UserInputNinoAndDob
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationIdentifyClientFormIrvSpec extends UnitSpec {

  val ninoEmptyMessage: String = "error.nino.required"
  val ninoFormatMessage: String = "enter-nino.invalid-format"

  val ninoEmptyFormError: FormError = FormError("clientIdentifier", List(ninoEmptyMessage))
  val ninoFormatFormError: FormError = FormError("clientIdentifier", List(ninoFormatMessage))

  val dayFormatMessage: String = "error.day.invalid-format"
  val monthFormatMessage: String = "error.month.invalid-format"
  val yearFormatMessage: String = "error.year.invalid-format"
  val dateRequiredMessage: String = "error.irv-date-of-birth.required"

  val dayFormatFormError: FormError = FormError("knownFact.day", List(dayFormatMessage))
  val monthFormatFormError: FormError = FormError("knownFact.month", List(monthFormatMessage))
  val yearFormatFormError: FormError = FormError("knownFact.year", List(yearFormatMessage))
  val dateRequiredFormError: FormError = FormError("knownFact", List(dateRequiredMessage))

  "agentInvitationIdentifyClientFormIrv" when {

    "featureFlags are on" when {

      val validData: Map[String, String] = Map(
        "clientType"       -> "someClientType",
        "clientIdentifier" -> "WM123456C",
        "service"          -> "someService",
        "knownFact.year"   -> "2000",
        "knownFact.month"  -> "1",
        "knownFact.day"    -> "1"
      )

      val featureFlags = FeatureFlags()
      val agentInvitationIdentifyClientForm = agentInvitationIdentifyClientFormIrv(featureFlags)

      "return no error message" when {
        "return no error message for valid Nino" in {
          val ninoForm = agentInvitationIdentifyClientForm.bind(validData)
          ninoForm.errors.isEmpty shouldBe true
        }

        "return no error message for valid Nino with spaces" in {
          val ninoForm = agentInvitationIdentifyClientForm.bind(validData + ("clientIdentifier" -> "  WM123456C  "))
          ninoForm.errors.isEmpty shouldBe true
        }

        "return no error message for valid lower case Nino" in {
          val ninoForm = agentInvitationIdentifyClientForm.bind(validData + ("clientIdentifier" -> "wn123456c"))
          ninoForm.errors.isEmpty shouldBe true
        }

        "return no errors when unbinding the form" in {
          val unboundForm =
            agentInvitationIdentifyClientForm.mapping.unbind(
              UserInputNinoAndDob(Some("personal"), "PERSONAL-INCOME-RECORD", Some("AE123456C"), Some("1980-01-01")))
          unboundForm("clientIdentifier") shouldBe "AE123456C"
        }
      }

      "return an error message" when {

        "return an error message for invalid Nino" in {
          val ninoForm = agentInvitationIdentifyClientForm.bind(validData + ("clientIdentifier" -> "12345"))
          ninoForm.errors.contains(ninoFormatFormError) shouldBe true
          ninoForm.errors.length shouldBe 1
        }

        "return an error message for empty Nino" in {
          val ninoForm = agentInvitationIdentifyClientForm.bind(validData + ("clientIdentifier" -> ""))
          ninoForm.errors.contains(ninoEmptyFormError) shouldBe true
          ninoForm.errors.length shouldBe 1
        }

        "return an error message for invalid characters" in {
          val invalidDate: Map[String, String] = Map(
            "clientType"       -> "personal",
            "clientIdentifier" -> "WM123456C",
            "service"          -> "PERSONAL-INCOME-RECORD",
            "knownFact.year"   -> "abdc",
            "knownFact.month"  -> "ef",
            "knownFact.day"    -> "gh"
          )
          val ninoForm = agentInvitationIdentifyClientForm.bind(invalidDate)
          ninoForm.errors shouldBe Seq(yearFormatFormError, monthFormatFormError, dayFormatFormError)
          ninoForm.errors.length shouldBe 3
        }

        "return an error message for no date" in {
          val invalidDate: Map[String, String] = Map(
            "clientType"       -> "personal",
            "clientIdentifier" -> "WM123456C",
            "service"          -> "PERSONAL-INCOME-RECORD",
            "knownFact.year"   -> "",
            "knownFact.month"  -> "",
            "knownFact.day"    -> ""
          )
          val ninoForm = agentInvitationIdentifyClientForm.bind(invalidDate)
          ninoForm.errors shouldBe Seq(dateRequiredFormError)
          ninoForm.errors.length shouldBe 1
        }

        "return an error message for empty form" in {
          val invalidData: Map[String, String] = Map(
            "clientType"       -> "",
            "clientIdentifier" -> "",
            "service"          -> "PERSONAL-INCOME-RECORD",
            "knownFact.year"   -> "",
            "knownFact.month"  -> "",
            "knownFact.day"    -> "")
          val ninoForm = agentInvitationIdentifyClientForm.bind(invalidData)
          ninoForm.errors shouldBe Seq(ninoEmptyFormError, dateRequiredFormError)
          ninoForm.errors.length shouldBe 2
        }
      }
    }

    "featureFlags are off" when {
      val featureFlags = FeatureFlags().copy(showKfcPersonalIncome = false)
      val agentInvitationIdentifyClientForm = agentInvitationIdentifyClientFormIrv(featureFlags)

    }
  }
}
