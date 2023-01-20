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
import uk.gov.hmrc.agentinvitationsfrontend.forms.IrvClientForm
import uk.gov.hmrc.agentinvitationsfrontend.models.IrvClient
import support.UnitSpec

class AgentInvitationIdentifyClientFormIrvSpec extends UnitSpec {

  val ninoEmptyMessage: String = "error.nino.required"
  val ninoFormatMessage: String = "enter-nino.invalid-format"

  val ninoEmptyFormError: FormError = FormError("clientIdentifier", List(ninoEmptyMessage))
  val ninoFormatFormError: FormError = FormError("clientIdentifier", List(ninoFormatMessage))

  val dayFormatMessage: String = "error.irv-date-of-birth-date.day"
  val monthFormatMessage: String = "error.irv-date-of-birth-date.month"
  val yearFormatMessage: String = "error.irv-date-of-birth-date.year"
  val dateRequiredMessage: String = "error.irv-date-of-birth-date.required"
  val dateInvalidMessage: String = "enter-irv-date-of-birth-date.invalid-format"
  val dayMonthFormatMessage: String = "error.irv-date-of-birth-date.day-month"
  val dayYearFormatMessage: String = "error.irv-date-of-birth-date.day-year"
  val monthYearFormatMessage: String = "error.irv-date-of-birth-date.month-year"

  val dayFormatFormError: FormError = FormError("dob", List(dayFormatMessage), Seq("inputFieldClass"     -> "day"))
  val monthFormatFormError: FormError = FormError("dob", List(monthFormatMessage), Seq("inputFieldClass" -> "month"))
  val yearFormatFormError: FormError = FormError("dob", List(yearFormatMessage), Seq("inputFieldClass"   -> "year"))
  val dateRequiredFormError: FormError =
    FormError("dob", List(dateRequiredMessage), Seq("inputFieldClass" -> "day-month-year"))
  val invalidDateFormError: FormError =
    FormError("dob", List(dateInvalidMessage), Seq("inputFieldClass" -> "day-month-year"))
  val dayMonthFormatFormError: FormError =
    FormError("dob", List(dayMonthFormatMessage), Seq("inputFieldClass" -> "day-month"))
  val dayYearFormatFormError: FormError =
    FormError("dob", List(dayYearFormatMessage), Seq("inputFieldClass" -> "day-year"))
  val monthYearFormatFormError: FormError =
    FormError("dob", List(monthYearFormatMessage), Seq("inputFieldClass" -> "month-year"))

  "agentInvitationIdentifyClientFormIrv" when {
    val validData: Map[String, String] = Map(
      "clientIdentifier" -> "WM123456C",
      "dob.year"         -> "2000",
      "dob.month"        -> "1",
      "dob.day"          -> "1"
    )

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
        ninoForm.errors shouldBe Seq(invalidDateFormError)
        ninoForm.errors.length shouldBe 1
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

      "return an error message for day field error" in {
        val invalidData: Map[String, String] =
          Map("clientIdentifier" -> "WM123456C", "dob.year" -> "2012", "dob.month" -> "12", "dob.day" -> "dd")
        val ninoForm = agentInvitationIdentifyClientForm.bind(invalidData)
        ninoForm.errors shouldBe Seq(dayFormatFormError)
        ninoForm.errors.length shouldBe 1

      }

      "return an error message for month field error" in {
        val invalidData: Map[String, String] =
          Map("clientIdentifier" -> "WM123456C", "dob.year" -> "2012", "dob.month" -> "dd", "dob.day" -> "2")
        val ninoForm = agentInvitationIdentifyClientForm.bind(invalidData)
        ninoForm.errors shouldBe Seq(monthFormatFormError)
        ninoForm.errors.length shouldBe 1

      }

      "return an error message for year field error" in {
        val invalidData: Map[String, String] =
          Map("clientIdentifier" -> "WM123456C", "dob.year" -> "", "dob.month" -> "12", "dob.day" -> "2")
        val ninoForm = agentInvitationIdentifyClientForm.bind(invalidData)
        ninoForm.errors shouldBe Seq(yearFormatFormError)
        ninoForm.errors.length shouldBe 1

      }

      "return an error message for day and month field error" in {
        val invalidData: Map[String, String] =
          Map("clientIdentifier" -> "WM123456C", "dob.year" -> "20", "dob.month" -> "", "dob.day" -> "dd")
        val ninoForm = agentInvitationIdentifyClientForm.bind(invalidData)
        ninoForm.errors shouldBe Seq(dayMonthFormatFormError)
        ninoForm.errors.length shouldBe 1

      }

      "return an error message for day and year field error" in {
        val invalidData: Map[String, String] =
          Map("clientIdentifier" -> "WM123456C", "dob.year" -> "ss", "dob.month" -> "12", "dob.day" -> "dd")
        val ninoForm = agentInvitationIdentifyClientForm.bind(invalidData)
        ninoForm.errors shouldBe Seq(dayYearFormatFormError)
        ninoForm.errors.length shouldBe 1

      }

      "return an error message for month and year field error" in {
        val invalidData: Map[String, String] =
          Map("clientIdentifier" -> "WM123456C", "dob.year" -> "kdhs", "dob.month" -> "jdgh", "dob.day" -> "31")
        val ninoForm = agentInvitationIdentifyClientForm.bind(invalidData)
        ninoForm.errors shouldBe Seq(monthYearFormatFormError)
        ninoForm.errors.length shouldBe 1

      }

      "return an error message when date does not parse as LocalDate" in {
        val invalidData: Map[String, String] =
          Map("clientIdentifier" -> "WM123456C", "dob.year" -> "2012", "dob.month" -> "2", "dob.day" -> "31")
        val ninoForm = agentInvitationIdentifyClientForm.bind(invalidData)
        ninoForm.errors shouldBe Seq(invalidDateFormError)
        ninoForm.errors.length shouldBe 1
      }
    }
  }
}
