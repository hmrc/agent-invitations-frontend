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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{AgentsInvitationController, DateFieldHelper, FeatureFlags}
import uk.gov.hmrc.agentinvitationsfrontend.models.UserInputVrnAndRegDate
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationIdentifyClientFormVatSpec extends UnitSpec {

  "agentInvitationIdentifyClientFormVat" when {

    "featureFlags are on" when {

      val featureFlags = FeatureFlags()
      val agentInvitationIdentifyClientForm =
        AgentsInvitationController.agentInvitationIdentifyClientFormVat(featureFlags)
      val validData: Map[String, String] = Map(
        "clientType"       -> "business",
        "clientIdentifier" -> "101747696",
        "service"          -> "HMRC-MTD-VAT",
        "knownFact.year"   -> "2000",
        "knownFact.month"  -> "1",
        "knownFact.day"    -> "1"
      )

      "return no error message" when {
        "VRN and registrationDate are valid" in {
          agentInvitationIdentifyClientForm.bind(validData).errors.isEmpty shouldBe true
        }

        "VRN and registrationDate are valid, but registrationDate contains spaces" in {
          val dataWithRegistrationDateSpaces = validData + ("knownFact" -> "  2000-01-01  ")
          agentInvitationIdentifyClientForm.bind(dataWithRegistrationDateSpaces).errors.isEmpty shouldBe true
        }

        "VRN and registrationDate are valid, but VRN has spaces" in {
          val dataWithRegistrationDateLowercase = validData + ("clientIdentifier" -> "  101747696  ")
          agentInvitationIdentifyClientForm.bind(dataWithRegistrationDateLowercase).errors.isEmpty shouldBe true
        }

        "unbinding the form" in {
          val unboundForm = agentInvitationIdentifyClientForm.mapping.unbind(
            UserInputVrnAndRegDate(Some("business"), "HMRC-MTD-VAT", Some("101747696"), Some("2000-01-01"))
          )
          unboundForm("knownFact.year") shouldBe "2000"
          unboundForm("knownFact.month") shouldBe "1"
          unboundForm("knownFact.day") shouldBe "1"
          unboundForm("clientIdentifier") shouldBe "101747696"
        }
      }

      "return an error message" when {
        "registrationDate is invalid" in {
          val dataWithInvalidRegistrationDate = Map(
            "clientType"       -> "business",
            "clientIdentifier" -> "101747696",
            "service"          -> "HMRC-MTD-VAT",
            "knownFact.year"   -> "2000",
            "knownFact.month"  -> "13",
            "knownFact.day"    -> "1"
          )
          val registrationDateForm = agentInvitationIdentifyClientForm.bind(dataWithInvalidRegistrationDate)
          registrationDateForm.errors shouldBe Seq(
            FormError("knownFact", List("enter-vat-registration-date.invalid-format")))
        }

        "registrationDate is partially empty" in {
          val dataWithEmptyRegistrationDate = Map(
            "clientType"       -> "business",
            "clientIdentifier" -> "101747696",
            "service"          -> "HMRC-MTD-VAT",
            "knownFact.year"   -> "2000",
            "knownFact.month"  -> "",
            "knownFact.day"    -> "1"
          )
          val registrationDateForm = agentInvitationIdentifyClientForm.bind(dataWithEmptyRegistrationDate)
          registrationDateForm.errors shouldBe Seq(FormError("knownFact", List("error.vat-registration-date.required")))
        }

        "registrationDate is empty" in {
          val dataWithEmptyRegistrationDate = Map(
            "clientType"       -> "business",
            "clientIdentifier" -> "101747696",
            "service"          -> "HMRC-MTD-VAT",
            "knownFact.year"   -> "",
            "knownFact.month"  -> "",
            "knownFact.day"    -> "")
          val registrationDateForm = agentInvitationIdentifyClientForm.bind(dataWithEmptyRegistrationDate)
          registrationDateForm.errors shouldBe Seq(FormError("knownFact", List("error.vat-registration-date.required")))
        }

        "registrationDate is missing" in {
          val dataWithEmptyRegistrationDate = Map("clientIdentifier" -> "101747696", "service" -> "HMRC-MTD-VAT")
          val registrationDateForm = agentInvitationIdentifyClientForm.bind(dataWithEmptyRegistrationDate)
          registrationDateForm.errors should contain(FormError("knownFact.year", List("error.required")))
          registrationDateForm.errors should contain(FormError("knownFact.month", List("error.required")))
          registrationDateForm.errors should contain(FormError("knownFact.day", List("error.required")))
        }

        "registrationDate has invalid characters" in {
          val dataWithInvalidCharactersRegistrationDate = Map(
            "clientType"       -> "business",
            "clientIdentifier" -> "101747696",
            "service"          -> "HMRC-MTD-VAT",
            "knownFact.year"   -> "abcd",
            "knownFact.month"  -> "ef",
            "knownFact.day"    -> "gh"
          )
          val registrationDateForm = agentInvitationIdentifyClientForm.bind(dataWithInvalidCharactersRegistrationDate)
          registrationDateForm.errors should contain(FormError("knownFact.day", List("error.day.invalid-format")))
          registrationDateForm.errors should contain(FormError("knownFact.month", List("error.month.invalid-format")))
          registrationDateForm.errors should contain(FormError("knownFact.year", List("error.year.invalid-format")))

        }

        "VRN is invalid for regex" in {
          val dataWithInvalidVrn = Map(
            "clientType"       -> "business",
            "clientIdentifier" -> "12345",
            "service"          -> "HMRC-MTD-VAT",
            "knownFact.year"   -> "2000",
            "knownFact.month"  -> "1",
            "knownFact.day"    -> "1"
          )
          val vrnForm = agentInvitationIdentifyClientForm.bind(dataWithInvalidVrn)
          vrnForm.errors shouldBe Seq(FormError("clientIdentifier", List("enter-vrn.regex-failure")))
        }

        "VRN is invalid for checksum" in {
          val dataWithInvalidVrn = Map(
            "clientType"       -> "business",
            "clientIdentifier" -> "101747697",
            "service"          -> "HMRC-MTD-VAT",
            "knownFact.year"   -> "2000",
            "knownFact.month"  -> "1",
            "knownFact.day"    -> "1"
          )
          val vrnForm = agentInvitationIdentifyClientForm.bind(dataWithInvalidVrn)
          vrnForm.errors shouldBe Seq(FormError("clientIdentifier", List("enter-vrn.checksum-failure")))
        }

        "VRN is empty" in {
          val dataWithEmptyVrn = Map(
            "clientType"       -> "business",
            "clientIdentifier" -> "",
            "service"          -> "HMRC-MTD-VAT",
            "knownFact.year"   -> "2000",
            "knownFact.month"  -> "1",
            "knownFact.day"    -> "1")
          val vrnForm = agentInvitationIdentifyClientForm.bind(dataWithEmptyVrn)
          vrnForm.errors shouldBe Seq(FormError("clientIdentifier", List("error.vrn.required")))
        }
      }
    }

    "feature flags are off" when {
      val featureFlags = FeatureFlags().copy(showKfcMtdVat = false)
      val agentInvitationIdentifyClientForm =
        AgentsInvitationController.agentInvitationIdentifyClientFormVat(featureFlags)
      val validData: Map[String, String] = Map(
        "clientType"       -> "business",
        "clientIdentifier" -> "101747696",
        "service"          -> "HMRC-MTD-VAT",
        "knownFact.year"   -> "2000",
        "knownFact.month"  -> "1",
        "knownFact.day"    -> "1"
      )

      "return no error message" when {
        "VRN and vat registration date are valid" in {
          agentInvitationIdentifyClientForm.bind(validData).errors.isEmpty shouldBe true
        }

        "VRN is valid but vat registration date is not" in {
          val dataWithInvalidRegistrationDate = Map(
            "clientType"       -> "business",
            "clientIdentifier" -> "101747696",
            "service"          -> "HMRC-MTD-VAT",
            "knownFact.year"   -> "2000",
            "knownFact.month"  -> "13",
            "knownFact.day"    -> "1"
          )
          agentInvitationIdentifyClientForm.bind(dataWithInvalidRegistrationDate).errors.isEmpty shouldBe true
        }

        "VRN is valid and vat registration date is empty" in {
          val dataWithEmptyRegistrationDate = Map(
            "clientType"       -> "business",
            "clientIdentifier" -> "101747696",
            "service"          -> "HMRC-MTD-VAT",
            "knownFact.year"   -> "",
            "knownFact.month"  -> "",
            "knownFact.day"    -> "")
          agentInvitationIdentifyClientForm.bind(dataWithEmptyRegistrationDate).errors.isEmpty shouldBe true
        }

        "VRN is valid and vat registration date is missing" in {
          val dataWithMissingRegistrationDate =
            Map("clientType" -> "business", "clientIdentifier" -> "101747696", "service" -> "HMRC-MTD-VAT")
          agentInvitationIdentifyClientForm.bind(dataWithMissingRegistrationDate).errors.isEmpty shouldBe true
        }
      }

      "return an error message" when {
        "VRN is invalid" in {
          val dataWithInvalidVrn = Map(
            "clientType"       -> "business",
            "clientIdentifier" -> "12345",
            "service"          -> "HMRC-MTD-VAT",
            "knownFact.year"   -> "2000",
            "knownFact.month"  -> "1",
            "knownFact.day"    -> "1"
          )
          agentInvitationIdentifyClientForm.bind(dataWithInvalidVrn).errors shouldBe Seq(
            FormError("clientIdentifier", List("enter-vrn.regex-failure")))
        }
        "VRN is empty" in {
          val dataWithEmptyVrn = Map(
            "clientType"       -> "business",
            "clientIdentifier" -> "",
            "service"          -> "HMRC-MTD-VAT",
            "knownFact.year"   -> "2000",
            "knownFact.month"  -> "1",
            "knownFact.day"    -> "1")
          agentInvitationIdentifyClientForm.bind(dataWithEmptyVrn).errors shouldBe Seq(
            FormError("clientIdentifier", List("error.vrn.required")))
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
