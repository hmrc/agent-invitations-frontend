/*
 * Copyright 2022 HM Revenue & Customs
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
import uk.gov.hmrc.agentinvitationsfrontend.forms.VatClientForm
import uk.gov.hmrc.agentinvitationsfrontend.models.VatClient
import support.UnitSpec

class AgentInvitationIdentifyClientFormVatSpec extends UnitSpec {

  val dayFormatMessage: String = "error.vat-registration-date.day"
  val monthFormatMessage: String = "error.vat-registration-date.month"
  val yearFormatMessage: String = "error.vat-registration-date.year"
  val dateRequiredMessage: String = "error.vat-registration-date.required"
  val dateInvalidMessage: String = "enter-vat-registration-date.invalid-format"
  val dayMonthFormatMessage: String = "error.vat-registration-date.day-month"
  val dayYearFormatMessage: String = "error.vat-registration-date.day-year"
  val monthYearFormatMessage: String = "error.vat-registration-date.month-year"

  val dayFormatFormError: FormError =
    FormError("registrationDate", List(dayFormatMessage), Seq("inputFieldClass" -> "day"))
  val monthFormatFormError: FormError =
    FormError("registrationDate", List(monthFormatMessage), Seq("inputFieldClass" -> "month"))
  val yearFormatFormError: FormError =
    FormError("registrationDate", List(yearFormatMessage), Seq("inputFieldClass" -> "year"))
  val dateRequiredFormError: FormError =
    FormError("registrationDate", List(dateRequiredMessage), Seq("inputFieldClass" -> "day-month-year"))
  val invalidDateFormError: FormError =
    FormError("registrationDate", List(dateInvalidMessage), Seq("inputFieldClass" -> "day-month-year"))
  val dayMonthFormatFormError: FormError =
    FormError("registrationDate", List(dayMonthFormatMessage), Seq("inputFieldClass" -> "day-month"))
  val dayYearFormatFormError: FormError =
    FormError("registrationDate", List(dayYearFormatMessage), Seq("inputFieldClass" -> "day-year"))
  val monthYearFormatFormError: FormError =
    FormError("registrationDate", List(monthYearFormatMessage), Seq("inputFieldClass" -> "month-year"))

  "agentInvitationIdentifyClientFormVat" when {

    val agentInvitationIdentifyClientForm = VatClientForm.form
    val validData: Map[String, String] = Map(
      "clientIdentifier"       -> "101747696",
      "registrationDate.year"  -> "2000",
      "registrationDate.month" -> "1",
      "registrationDate.day"   -> "1"
    )

    "return no error message" when {
      "VRN and registrationDate are valid" in {
        agentInvitationIdentifyClientForm.bind(validData).errors.isEmpty shouldBe true
      }

      "VRN and registrationDate are valid, but registrationDate contains spaces" in {
        val dataWithRegistrationDateSpaces = validData + ("registrationDate" -> "  2000-01-01  ")
        agentInvitationIdentifyClientForm.bind(dataWithRegistrationDateSpaces).errors.isEmpty shouldBe true
      }

      "VRN and registrationDate are valid, but VRN has spaces" in {
        val dataWithRegistrationDateLowercase = validData + ("clientIdentifier" -> "  101747696  ")
        agentInvitationIdentifyClientForm.bind(dataWithRegistrationDateLowercase).errors.isEmpty shouldBe true
      }

      "VRN looks valid but the checksum digit is invalid (i.e. checksum digits are not validated)" in {
        val vrnWithBadChecksum = "101747697"
        val dataWithRegistrationDateLowercase = validData + ("clientIdentifier" -> vrnWithBadChecksum)
        agentInvitationIdentifyClientForm.bind(dataWithRegistrationDateLowercase).errors.isEmpty shouldBe true
      }

      "unbinding the form" in {
        val unboundForm = VatClientForm.form.mapping
          .unbind(
            VatClient("101747696", "2000-01-01")
          )
        unboundForm("registrationDate.year") shouldBe "2000"
        unboundForm("registrationDate.month") shouldBe "1"
        unboundForm("registrationDate.day") shouldBe "1"
        unboundForm("clientIdentifier") shouldBe "101747696"
      }
    }

    "return an error message" when {
      "registrationDate is invalid" in {
        val dataWithInvalidRegistrationDate = Map(
          "clientIdentifier"       -> "101747696",
          "registrationDate.year"  -> "2000",
          "registrationDate.month" -> "13",
          "registrationDate.day"   -> "1"
        )
        val registrationDateForm = agentInvitationIdentifyClientForm.bind(dataWithInvalidRegistrationDate)
        registrationDateForm.errors shouldBe Seq(monthFormatFormError)
      }

      "registrationDate is partially empty" in {
        val dataWithEmptyRegistrationDate = Map(
          "clientIdentifier"       -> "101747696",
          "registrationDate.year"  -> "2000",
          "registrationDate.month" -> "",
          "registrationDate.day"   -> "1"
        )
        val registrationDateForm = agentInvitationIdentifyClientForm.bind(dataWithEmptyRegistrationDate)
        registrationDateForm.errors shouldBe Seq(monthFormatFormError)
      }

      "registrationDate has month and year invalid chars" in {
        val dataWithEmptyRegistrationDate = Map(
          "clientIdentifier"       -> "101747696",
          "registrationDate.year"  -> "fdfd",
          "registrationDate.month" -> "dd",
          "registrationDate.day"   -> "1"
        )
        val registrationDateForm = agentInvitationIdentifyClientForm.bind(dataWithEmptyRegistrationDate)
        registrationDateForm.errors shouldBe Seq(monthYearFormatFormError)
      }

      "registrationDate has day and year invalid chars" in {
        val dataWithEmptyRegistrationDate = Map(
          "clientIdentifier"       -> "101747696",
          "registrationDate.year"  -> "ewe",
          "registrationDate.month" -> "2",
          "registrationDate.day"   -> "dd"
        )
        val registrationDateForm = agentInvitationIdentifyClientForm.bind(dataWithEmptyRegistrationDate)
        registrationDateForm.errors shouldBe Seq(dayYearFormatFormError)
      }

      "registrationDate has day and month invalid chars" in {
        val dataWithEmptyRegistrationDate = Map(
          "clientIdentifier"       -> "101747696",
          "registrationDate.year"  -> "2010",
          "registrationDate.month" -> "fdf",
          "registrationDate.day"   -> "dd"
        )
        val registrationDateForm = agentInvitationIdentifyClientForm.bind(dataWithEmptyRegistrationDate)
        registrationDateForm.errors shouldBe Seq(dayMonthFormatFormError)
      }

      "registrationDate does not parse into LocalDate" in {
        val dataWithEmptyRegistrationDate = Map(
          "clientIdentifier"       -> "101747696",
          "registrationDate.year"  -> "2010",
          "registrationDate.month" -> "2",
          "registrationDate.day"   -> "31"
        )
        val registrationDateForm = agentInvitationIdentifyClientForm.bind(dataWithEmptyRegistrationDate)
        registrationDateForm.errors shouldBe Seq(invalidDateFormError)
      }

      "registrationDate is empty" in {
        val dataWithEmptyRegistrationDate =
          Map("clientIdentifier" -> "101747696", "registrationDate.year" -> "", "registrationDate.month" -> "", "registrationDate.day" -> "")
        val registrationDateForm = agentInvitationIdentifyClientForm.bind(dataWithEmptyRegistrationDate)
        registrationDateForm.errors shouldBe Seq(dateRequiredFormError)
      }

      "registrationDate is missing" in {
        val dataWithEmptyRegistrationDate = Map("clientIdentifier" -> "101747696", "service" -> "HMRC-MTD-VAT")
        val registrationDateForm = agentInvitationIdentifyClientForm.bind(dataWithEmptyRegistrationDate)
        registrationDateForm.errors should contain(FormError("registrationDate.year", List("error.required")))
        registrationDateForm.errors should contain(FormError("registrationDate.month", List("error.required")))
        registrationDateForm.errors should contain(FormError("registrationDate.day", List("error.required")))
      }

      "registrationDate has invalid characters" in {
        val dataWithInvalidCharactersRegistrationDate = Map(
          "clientIdentifier"       -> "101747696",
          "registrationDate.year"  -> "abcd",
          "registrationDate.month" -> "ef",
          "registrationDate.day"   -> "gh"
        )
        val registrationDateForm = agentInvitationIdentifyClientForm.bind(dataWithInvalidCharactersRegistrationDate)
        registrationDateForm.errors should contain(invalidDateFormError)
      }

      "VRN is invalid for regex" in {
        val dataWithInvalidVrn = Map(
          "clientIdentifier"       -> "12345",
          "registrationDate.year"  -> "2000",
          "registrationDate.month" -> "1",
          "registrationDate.day"   -> "1"
        )
        val vrnForm = agentInvitationIdentifyClientForm.bind(dataWithInvalidVrn)
        vrnForm.errors shouldBe Seq(FormError("clientIdentifier", List("enter-vrn.regex-failure")))
      }

      "VRN is empty" in {
        val dataWithEmptyVrn =
          Map("clientIdentifier" -> "", "registrationDate.year" -> "2000", "registrationDate.month" -> "1", "registrationDate.day" -> "1")
        val vrnForm = agentInvitationIdentifyClientForm.bind(dataWithEmptyVrn)
        vrnForm.errors shouldBe Seq(FormError("clientIdentifier", List("error.vrn.required")))
      }

      "VRN and registrationDate are valid, but VRN has spaces" in {
        val dataWithRegistrationDateLowercase = validData + ("clientIdentifier" -> "  101747696  ")
        agentInvitationIdentifyClientForm.bind(dataWithRegistrationDateLowercase).errors.isEmpty shouldBe true
      }

      "unbinding the form" in {
        val unboundForm = VatClientForm.form.mapping
          .unbind(
            VatClient("101747696", "2000-01-01")
          )
        unboundForm("registrationDate.year") shouldBe "2000"
        unboundForm("registrationDate.month") shouldBe "1"
        unboundForm("registrationDate.day") shouldBe "1"
        unboundForm("clientIdentifier") shouldBe "101747696"
      }
    }
  }
}
