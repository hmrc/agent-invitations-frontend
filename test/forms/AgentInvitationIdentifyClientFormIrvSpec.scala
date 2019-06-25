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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.forms.IrvClientForm
import uk.gov.hmrc.agentinvitationsfrontend.models.IrvClient
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

  val dayFormatFormError: FormError = FormError("dob.day", List(dayFormatMessage))
  val monthFormatFormError: FormError = FormError("dob.month", List(monthFormatMessage))
  val yearFormatFormError: FormError = FormError("dob.year", List(yearFormatMessage))
  val dateRequiredFormError: FormError = FormError("dob", List(dateRequiredMessage))

  "agentInvitationIdentifyClientFormIrv" when {

    "featureFlags are on" when {

      val validData: Map[String, String] = Map(
        "clientIdentifier" -> "WM123456C",
        "dob.year"         -> "2000",
        "dob.month"        -> "1",
        "dob.day"          -> "1"
      )

      val featureFlags = FeatureFlags()
      val agentInvitationIdentifyClientForm = IrvClientForm.form

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
            agentInvitationIdentifyClientForm.mapping.unbind(IrvClient("AE123456C", "1980-01-01"))
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
            "clientIdentifier" -> "WM123456C",
            "dob.year"         -> "abdc",
            "dob.month"        -> "ef",
            "dob.day"          -> "gh"
          )
          val ninoForm = agentInvitationIdentifyClientForm.bind(invalidDate)
          ninoForm.errors shouldBe Seq(yearFormatFormError, monthFormatFormError, dayFormatFormError)
          ninoForm.errors.length shouldBe 3
        }

        "return an error message for no date" in {
          val invalidDate: Map[String, String] = Map(
            "clientIdentifier" -> "WM123456C",
            "dob.year"         -> "",
            "dob.month"        -> "",
            "dob.day"          -> ""
          )
          val ninoForm = agentInvitationIdentifyClientForm.bind(invalidDate)
          ninoForm.errors shouldBe Seq(dateRequiredFormError)
          ninoForm.errors.length shouldBe 1
        }

        "return an error message for empty form" in {
          val invalidData: Map[String, String] =
            Map("clientIdentifier" -> "", "dob.year" -> "", "dob.month" -> "", "dob.day" -> "")
          val ninoForm = agentInvitationIdentifyClientForm.bind(invalidData)
          ninoForm.errors shouldBe Seq(ninoEmptyFormError, dateRequiredFormError)
          ninoForm.errors.length shouldBe 2
        }
      }
    }
  }
}
