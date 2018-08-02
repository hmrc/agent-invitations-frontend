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
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.{agentFastTrackDateOfBirthForm, agentFastTrackGenericForm, agentFastTrackGenericFormKnownFact, agentFastTrackPostcodeForm, agentFastTrackVatRegDateForm}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._

class AgentFastTrackFormSpec extends UnitSpec {

  "agentFastTrackGenericForm" when {
    "feature flags are on" when {
      val featureFlags = FeatureFlags()

      "return no error message" when {
        "provided correct ITSA Data" in {
          val data = Json.obj(
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "DH14EJ"
          )
          val fastTrackForm = agentFastTrackGenericForm(featureFlags).bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided correct IRV Data" in {
          val data = Json.obj(
            "service"              -> HMRCPIR,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "2000-01-01"
          )
          val fastTrackForm = agentFastTrackGenericForm(featureFlags).bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided correct VAT Data" in {
          val data = Json.obj(
            "service"              -> HMRCMTDVAT,
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> "101747696",
            "knownFact"            -> "1970-01-01"
          )
          val fastTrackForm = agentFastTrackGenericForm(featureFlags).bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided no known fact for ITSA" in {
          val data = Json.obj(
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> ""
          )
          val fastTrackForm = agentFastTrackGenericForm(featureFlags).bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided no known fact for IRV" in {
          val data = Json.obj(
            "service"              -> HMRCPIR,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> ""
          )
          val fastTrackForm = agentFastTrackGenericForm(featureFlags).bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided no known fact for VAT" in {
          val data = Json.obj(
            "service"              -> HMRCMTDVAT,
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> "101747696",
            "knownFact"            -> ""
          )
          val fastTrackForm = agentFastTrackGenericForm(featureFlags).bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided nonsense known fact for ITSA" in {
          val data = Json.obj(
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "foo"
          )
          val fastTrackForm = agentFastTrackGenericForm(featureFlags).bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided nonsense known fact for IRV" in {
          val data = Json.obj(
            "service"              -> HMRCPIR,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "foo"
          )
          val fastTrackForm = agentFastTrackGenericForm(featureFlags).bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided nonsense known fact for VAT" in {
          val data = Json.obj(
            "service"              -> HMRCMTDVAT,
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> "101747696",
            "knownFact"            -> "foo"
          )
          val fastTrackForm = agentFastTrackGenericForm(featureFlags).bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }
      }

      "return error message" when {
        "provided incorrect NINO" in {
          val data = Json.obj(
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "ZZ123456A",
            "knownFact"            -> "DH14EJ"
          )
          val fastTrackForm = agentFastTrackGenericForm(featureFlags).bind(data)
          fastTrackForm.errors.nonEmpty shouldBe true
          fastTrackForm.errors shouldBe Seq(FormError("clientIdentifier", List("Invalid Nino")))
        }

        "provided incorrect VRN" in {
          val data = Json.obj(
            "service"              -> HMRCMTDVAT,
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> "101747695",
            "knownFact"            -> "1970-01-01"
          )
          val fastTrackForm = agentFastTrackGenericForm(featureFlags).bind(data)
          fastTrackForm.errors.nonEmpty shouldBe true
          fastTrackForm.errors shouldBe Seq(FormError("clientIdentifier", List("Invalid Vrn")))
        }

        "provided incorrect clientIdentifierType" in {
          val data = Json.obj(
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "invalid type",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "DH14EJ"
          )
          val fastTrackForm = agentFastTrackGenericForm(featureFlags).bind(data)
          fastTrackForm.errors.nonEmpty shouldBe true
        }

        "provided no clientIdentifier" in {
          val data = Json.obj(
            "service"              -> HMRCMTDVAT,
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> "",
            "knownFact"            -> "1970-01-01"
          )
          val fastTrackForm = agentFastTrackGenericForm(featureFlags).bind(data)
          fastTrackForm.errors.nonEmpty shouldBe true
          fastTrackForm.errors shouldBe Seq(
            FormError("clientIdentifier", List("A Valid Client Identifier is Required. Received: Nothing")))
        }

        "provided mixed data" in {
          val data = Json.obj(
            "service"              -> HMRCMTDVAT,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "101747696",
            "knownFact"            -> "DH14EJ"
          )
          val fastTrackForm = agentFastTrackGenericForm(featureFlags).bind(data)
          fastTrackForm.errors.nonEmpty shouldBe true
          fastTrackForm.errors shouldBe Seq(
            FormError("", List("Fast Track Form was submitted with mixed or invalid data")))
        }
      }
    }

    "when feature flags are switched off" when {
      val featureFlags = FeatureFlags(showKfcMtdIt = false, showKfcMtdVat = false, showKfcPersonalIncome = false)
      "return no error message" when {
        "provided correct ITSA Data without postcode" in {
          val data = Json.obj(
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> ""
          )
          val fastTrackForm = agentFastTrackGenericForm(featureFlags).bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided correct IRV Data without Date of birth" in {
          val data = Json.obj(
            "service"              -> HMRCPIR,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> ""
          )
          val fastTrackForm = agentFastTrackGenericForm(featureFlags).bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided correct VAT Data without Vat Registation Date" in {
          val data = Json.obj(
            "service"              -> HMRCMTDVAT,
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> "101747696",
            "knownFact"            -> ""
          )
          val fastTrackForm = agentFastTrackGenericForm(featureFlags).bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }
      }
    }
  }

  "agentFastTrackGenericFormKnownFact" when {
    "feature flags are on" when {
      val featureFlags = FeatureFlags()

      "return no error message" when {
        "provided correct ITSA Data" in {
          val data = Json.obj(
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "DH14EJ"
          )
          val fastTrackForm = agentFastTrackGenericFormKnownFact(featureFlags).bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided correct IRV Data" in {
          val data = Json.obj(
            "service"              -> HMRCPIR,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "2000-01-01"
          )
          val fastTrackForm = agentFastTrackGenericFormKnownFact(featureFlags).bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided correct VAT Data" in {
          val data = Json.obj(
            "service"              -> HMRCMTDVAT,
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "2000-01-01"
          )
          val fastTrackForm = agentFastTrackGenericFormKnownFact(featureFlags).bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }
      }

      "return error messages" when {
        "there is an unsupported service" in {
          val data = Json.obj(
            "service"              -> "foo",
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "DH14EJ"
          )
          val fastTrackForm = agentFastTrackGenericFormKnownFact(featureFlags).bind(data)
          fastTrackForm.errors shouldBe Seq(FormError("service", List("Unsupported Service")))
        }

        "there is an unsupported client identifier" in {
          val data = Json.obj(
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "foo",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "DH14EJ"
          )
          val fastTrackForm = agentFastTrackGenericFormKnownFact(featureFlags).bind(data)
          fastTrackForm.errors shouldBe Seq(FormError("clientIdentifierType", List("Unsupported Client Type")))
        }

        "clientId is invalid" in {
          val data = Json.obj(
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "foo",
            "knownFact"            -> "DH14EJ"
          )
          val fastTrackForm = agentFastTrackGenericFormKnownFact(featureFlags).bind(data)
          fastTrackForm.errors shouldBe Seq(
            FormError("clientIdentifier", List("A Valid Client Identifier is Required. Received: foo")))
        }
      }
    }
  }
  "agentFastTrackPostcodeForm" when {
    "feature flags are on" when {
      val featureFlags = FeatureFlags()

      "return no error message" when {
        "provided correct ITSA Data" in {
          val data = Json.obj(
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "DH14EJ"
          )
          val fastTrackForm = agentFastTrackPostcodeForm(featureFlags).bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }
      }
      "return error message" when {
        "provided empty known fact for ITSA" in {
          val data = Json.obj(
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> ""
          )
          val fastTrackForm = agentFastTrackPostcodeForm(featureFlags).bind(data)
          fastTrackForm.errors shouldBe Seq(FormError("knownFact", List("error.postcode.required")))
        }
        "provided invalid characters in known fact for ITSA" in {
          val data = Json.obj(
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "DH!4EJ"
          )
          val fastTrackForm = agentFastTrackPostcodeForm(featureFlags).bind(data)
          fastTrackForm.errors shouldBe Seq(FormError("knownFact", List("enter-postcode.invalid-characters")))
        }
        "provided invalid format in known fact for ITSA" in {
          val data = Json.obj(
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "DH14EJXXX"
          )
          val fastTrackForm = agentFastTrackPostcodeForm(featureFlags).bind(data)
          fastTrackForm.errors shouldBe Seq(FormError("knownFact", List("enter-postcode.invalid-format")))
        }

      }
    }

  }

  "agentFastTrackDateOfBirthForm" when {
    "feature flags are on" when {
      val featureFlags = FeatureFlags()

      "return no error message" when {
        "provided correct IRV Data" in {
          val data = Json.obj(
            "service"              -> HMRCPIR,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact.year"       -> "2000",
            "knownFact.month"      -> "01",
            "knownFact.day"        -> "01"
          )
          val fastTrackForm = agentFastTrackDateOfBirthForm(featureFlags).bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }
      }
      "return error message" when {
        "provided empty known fact for IRV" in {
          val data = Json.obj(
            "service"              -> HMRCPIR,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact.year"       -> "",
            "knownFact.month"      -> "",
            "knownFact.day"        -> ""
          )
          val fastTrackForm = agentFastTrackDateOfBirthForm(featureFlags).bind(data)
          fastTrackForm.errors shouldBe Seq(FormError("knownFact", List("error.irv-date-of-birth.required")))
        }
        "provided invalid known fact for IRV" in {
          val data = Json.obj(
            "service"              -> HMRCPIR,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact.year"       -> "aaaa",
            "knownFact.month"      -> "aa",
            "knownFact.day"        -> "aa"
          )
          val fastTrackForm = agentFastTrackDateOfBirthForm(featureFlags).bind(data)
          fastTrackForm.errors shouldBe Seq(
            FormError("knownFact.year", List("error.year.invalid-format")),
            FormError("knownFact.month", List("error.month.invalid-format")),
            FormError("knownFact.day", List("error.day.invalid-format"))
          )
        }
      }
    }
  }

  "agentFastTrackVatRegDateForm" when {
    "feature flags are on" when {
      val featureFlags = FeatureFlags()

      "return no error message" when {
        "provided correct VAT Data" in {
          val data = Json.obj(
            "service"              -> HMRCMTDVAT,
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> "101747696",
            "knownFact.year"       -> "2000",
            "knownFact.month"      -> "01",
            "knownFact.day"        -> "01"
          )
          val fastTrackForm = agentFastTrackVatRegDateForm(featureFlags).bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }
      }
      "return error message" when {
        "provided empty known fact for IRV" in {
          val data = Json.obj(
            "service"              -> HMRCMTDVAT,
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> "101747696",
            "knownFact.year"       -> "",
            "knownFact.month"      -> "",
            "knownFact.day"        -> ""
          )
          val fastTrackForm = agentFastTrackVatRegDateForm(featureFlags).bind(data)
          fastTrackForm.errors shouldBe Seq(FormError("knownFact", List("error.vat-registration-date.required")))
        }
        "provided invalid known fact for IRV" in {
          val data = Json.obj(
            "service"              -> HMRCMTDVAT,
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> "101747696",
            "knownFact.year"       -> "aaaa",
            "knownFact.month"      -> "aa",
            "knownFact.day"        -> "aa"
          )
          val fastTrackForm = agentFastTrackVatRegDateForm(featureFlags).bind(data)
          fastTrackForm.errors shouldBe Seq(
            FormError("knownFact.year", List("error.year.invalid-format")),
            FormError("knownFact.month", List("error.month.invalid-format")),
            FormError("knownFact.day", List("error.day.invalid-format"))
          )
        }
      }
    }
  }
}
