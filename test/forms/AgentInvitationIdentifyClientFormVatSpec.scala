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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{AgentsInvitationController, DateFieldHelper, FeatureFlags}
import uk.gov.hmrc.agentinvitationsfrontend.models.UserInputVrnAndRegDate
import uk.gov.hmrc.domain.Vrn
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationIdentifyClientFormVatSpec extends UnitSpec {

  "agentInvitationIdentifyClientFormVat" when {

    val featureFlags = FeatureFlags()
    val agentInvitationIdentifyClientForm = AgentsInvitationController.agentInvitationIdentifyClientFormVat(featureFlags)

    "service is VAT" should {
      val validData: Map[String, String] = Map("clientIdentifier" -> "101747696", "service" -> "HMRC-MTD-VAT", "registrationDate.year" -> "2000", "registrationDate.month" -> "1", "registrationDate.day" -> "1")

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

        "unbinding the form" in {
          val unboundForm = agentInvitationIdentifyClientForm.mapping.unbind(
            UserInputVrnAndRegDate("HMRC-MTD-VAT", Some("101747696"), Some("2000-01-01"))
          )
          unboundForm("registrationDate.year") shouldBe "2000"
          unboundForm("registrationDate.month") shouldBe "1"
          unboundForm("registrationDate.day") shouldBe "1"
          unboundForm("clientIdentifier") shouldBe "101747696"
        }
      }

      "return an error message" when {
        "registrationDate is invalid" in {
          val dataWithInvalidRegistrationDate = Map("clientIdentifier" -> "101747696", "service" -> "HMRC-MTD-VAT", "registrationDate.year" -> "2000", "registrationDate.month" -> "13", "registrationDate.day" -> "1")
          val registrationDateForm = agentInvitationIdentifyClientForm.bind(dataWithInvalidRegistrationDate)
          registrationDateForm.errors shouldBe Seq(FormError("registrationDate", List("enter-vat-registration-date.invalid-format")))
        }

        "registrationDate is partially empty" in {
          val dataWithEmptyRegistrationDate = Map("clientIdentifier" -> "101747696", "service" -> "HMRC-MTD-VAT", "registrationDate.year" -> "2000", "registrationDate.month" -> "", "registrationDate.day" -> "1")
          val registrationDateForm = agentInvitationIdentifyClientForm.bind(dataWithEmptyRegistrationDate)
          registrationDateForm.errors shouldBe Seq(FormError("registrationDate", List("error.vat-registration-date.required")))
        }

        "registrationDate is empty" in {
          val dataWithEmptyRegistrationDate = Map("clientIdentifier" -> "101747696", "service" -> "HMRC-MTD-VAT", "registrationDate.year" -> "", "registrationDate.month" -> "", "registrationDate.day" -> "")
          val registrationDateForm = agentInvitationIdentifyClientForm.bind(dataWithEmptyRegistrationDate)
          registrationDateForm.errors shouldBe Seq(FormError("registrationDate", List("error.vat-registration-date.required")))
        }

        "VRN is invalid for regex" in {
          val dataWithInvalidVrn = Map("clientIdentifier" -> "12345", "service" -> "HMRC-MTD-VAT", "registrationDate.year" -> "2000", "registrationDate.month" -> "1", "registrationDate.day" -> "1")
          val vrnForm = agentInvitationIdentifyClientForm.bind(dataWithInvalidVrn)
          vrnForm.errors shouldBe Seq(FormError("clientIdentifier", List("enter-vrn.regex-failure")))
        }

        "VRN is invalid for checksum" in {
          val dataWithInvalidVrn = Map("clientIdentifier" -> "101747697", "service" -> "HMRC-MTD-VAT", "registrationDate.year" -> "2000", "registrationDate.month" -> "1", "registrationDate.day" -> "1")
          val vrnForm = agentInvitationIdentifyClientForm.bind(dataWithInvalidVrn)
          vrnForm.errors shouldBe Seq(FormError("clientIdentifier", List("enter-vrn.checksum-failure")))
        }

        "VRN is empty" in {
          val dataWithEmptyVrn = Map("clientIdentifier" -> "", "service" -> "HMRC-MTD-VAT", "registrationDate.year" -> "2000", "registrationDate.month" -> "1", "registrationDate.day" -> "1")
          val vrnForm = agentInvitationIdentifyClientForm.bind(dataWithEmptyVrn)
          vrnForm.errors shouldBe Seq(FormError("clientIdentifier", List("error.vrn.required")))
        }
      }
    }

    "parseDateIntoFields" should {
      "Convert a date string into it's day, month and year fields" in {
        DateFieldHelper.parseDateIntoFields("2000-01-01") shouldBe Some(("2000", "1", "1"))
      }

      "Return None when date cannot be converted" in {
        DateFieldHelper.parseDateIntoFields("20010101") shouldBe None
      }
    }

    "formatDateFromFields" should {
      "Convert a date in fields to a string" in {
        DateFieldHelper.formatDateFromFields("2000", "1", "1") shouldBe "2000-01-01"
      }
    }
  }
}
