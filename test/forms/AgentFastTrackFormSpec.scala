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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.retired.AgentsFastTrackInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.play.test.UnitSpec

class AgentFastTrackFormSpec extends UnitSpec {

  "agentFastTrackGenericForm" when {
    "feature flags are on" when {

      "return no error message" when {
        "provided correct ITSA Data" in {
          val data = Json.obj(
            "clientType"           -> "personal",
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "DH14EJ"
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided correct IRV Data" in {
          val data = Json.obj(
            "clientType"           -> "personal",
            "service"              -> HMRCPIR,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "2000-01-01"
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided correct VAT Data" in {
          val data = Json.obj(
            "clientType"           -> "business",
            "service"              -> HMRCMTDVAT,
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> "101747696",
            "knownFact"            -> "1970-01-01"
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided no known fact for ITSA" in {
          val data = Json.obj(
            "clientType"           -> "personal",
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> ""
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided incorrect VRN" in {
          val data = Json.obj(
            "clientType"           -> "business",
            "service"              -> HMRCMTDVAT,
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> "101747695",
            "knownFact"            -> "1970-01-01"
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.nonEmpty shouldBe true
          fastTrackForm.errors shouldBe Seq(FormError("clientIdentifier", List("INVALID_VRN")))
        }

        "provided no known fact for IRV" in {
          val data = Json.obj(
            "clientType"           -> "personal",
            "service"              -> HMRCPIR,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> ""
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.isEmpty shouldBe true

        }

        "provided no known fact for VAT" in {
          val data = Json.obj(
            "clientType"           -> "business",
            "service"              -> HMRCMTDVAT,
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> "101747696",
            "knownFact"            -> ""
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided nonsense known fact for ITSA" in {
          val data = Json.obj(
            "clientType"           -> "personal",
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "foo"
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided nonsense known fact for IRV" in {
          val data = Json.obj(
            "clientType"           -> "personal",
            "service"              -> HMRCPIR,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "foo"
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided nonsense known fact for VAT" in {
          val data = Json.obj(
            "clientType"           -> "business",
            "service"              -> HMRCMTDVAT,
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> "101747696",
            "knownFact"            -> "foo"
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided client type PERSONAL for IRV" in {
          val data = Json.obj(
            "clientType"           -> "PERSONAL",
            "service"              -> HMRCPIR,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "foo"
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }

        "provided client type BUSINESS for VAT" in {
          val data = Json.obj(
            "clientType"           -> "BUSINESS",
            "service"              -> HMRCMTDVAT,
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> "101747696",
            "knownFact"            -> "foo"
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.isEmpty shouldBe true
        }
      }

      "return error message" when {

        "provided incorrect clientType" in {
          val data = Json.obj(
            "clientType"           -> "foo",
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "DH14EJ"
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.nonEmpty shouldBe true
          fastTrackForm.errors shouldBe Seq(FormError("clientType", List("UNSUPPORTED_CLIENT_TYPE")))
        }

        "provided incorrect NINO" in {
          val data = Json.obj(
            "clientType"           -> "personal",
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "ZZ123456A",
            "knownFact"            -> "DH14EJ"
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.nonEmpty shouldBe true
          fastTrackForm.errors shouldBe Seq(FormError("clientIdentifier", List("INVALID_NINO")))
        }

        "provided incorrect VRN" in {
          val data = Json.obj(
            "clientType"           -> "business",
            "service"              -> HMRCMTDVAT,
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> "101747695",
            "knownFact"            -> "1970-01-01"
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.nonEmpty shouldBe true
          fastTrackForm.errors shouldBe Seq(FormError("clientIdentifier", List("INVALID_VRN")))
        }

        "provided incorrect clientIdentifierType" in {
          val data = Json.obj(
            "clientType"           -> "personal",
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "invalid type",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "DH14EJ"
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.nonEmpty shouldBe true
        }

        "provided no clientIdentifier" in {
          val data = Json.obj(
            "clientType"           -> "business",
            "service"              -> HMRCMTDVAT,
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> "",
            "knownFact"            -> "1970-01-01"
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.nonEmpty shouldBe true
          fastTrackForm.errors shouldBe Seq(FormError("clientIdentifier", List("INVALID_CLIENT_ID_RECEIVED:NOTHING")))
        }

        "provided mixed data" in {
          val data = Json.obj(
            "clientType"           -> "personal",
            "service"              -> HMRCMTDVAT,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "101747696",
            "knownFact"            -> "DH14EJ"
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.nonEmpty shouldBe true
          fastTrackForm.errors shouldBe Seq(FormError("", List("INVALID_SUBMISSION")))
        }

        "provided incorrect clientType for ITSA" in {
          val data = Json.obj(
            "clientType"           -> "business",
            "service"              -> HMRCMTDIT,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "DH14EJ"
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.nonEmpty shouldBe true
          fastTrackForm.errors shouldBe Seq(FormError("", List("INVALID_SUBMISSION")))
        }

        "provided incorrect clientType for PIR" in {
          val data = Json.obj(
            "clientType"           -> "business",
            "service"              -> HMRCPIR,
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "WM123456C",
            "knownFact"            -> "2000-01-01"
          )
          val fastTrackForm = agentFastTrackForm.bind(data)
          fastTrackForm.errors.nonEmpty shouldBe true
          fastTrackForm.errors shouldBe Seq(FormError("", List("INVALID_SUBMISSION")))
        }
      }
    }

    "agentFastTrackKnownFactForm" when {
      "feature flags are on" when {
        val featureFlags = FeatureFlags()

        "return no error message" when {
          "provided correct ITSA Data" in {
            val data = Json.obj("knownFact" -> "DH14EJ")
            val fastTrackForm =
              knownFactsForm(postcodeMapping).bind(data)
            fastTrackForm.errors.isEmpty shouldBe true
          }

          "provided correct IRV Data" in {
            val data = Json.obj(
              "knownFact.year"  -> "2000",
              "knownFact.month" -> "01",
              "knownFact.day"   -> "01"
            )
            val fastTrackForm =
              knownFactsForm(dateOfBirthMapping).bind(data)
            fastTrackForm.errors.isEmpty shouldBe true
          }

          "provided correct VAT Data" in {
            val data = Json.obj(
              "knownFact.year"  -> "2000",
              "knownFact.month" -> "01",
              "knownFact.day"   -> "01"
            )
            val fastTrackForm = knownFactsForm(vatRegDateMapping).bind(data)
            fastTrackForm.errors.isEmpty shouldBe true
          }
        }

        "return error message" when {
          "provided empty known fact for ITSA" in {
            val data = Json.obj("knownFact" -> "")
            val fastTrackForm =
              knownFactsForm(postcodeMapping).bind(data)
            fastTrackForm.errors shouldBe Seq(FormError("knownFact", List("error.postcode.required")))
          }
          "provided invalid characters in known fact for ITSA" in {
            val data = Json.obj("knownFact" -> "DH!4EJ")
            val fastTrackForm =
              knownFactsForm(postcodeMapping).bind(data)
            fastTrackForm.errors shouldBe Seq(FormError("knownFact", List("enter-postcode.invalid-characters")))
          }
          "provided invalid format in known fact for ITSA" in {
            val data = Json.obj("knownFact" -> "DH14EJXXX")
            val fastTrackForm =
              knownFactsForm(postcodeMapping).bind(data)
            fastTrackForm.errors shouldBe Seq(FormError("knownFact", List("enter-postcode.invalid-format")))
          }
          "provided empty known fact for IRV" in {
            val data = Json.obj("knownFact.year" -> "", "knownFact.month" -> "", "knownFact.day" -> "")
            val fastTrackForm =
              knownFactsForm(dateOfBirthMapping).bind(data)
            fastTrackForm.errors shouldBe Seq(FormError("knownFact", List("error.irv-date-of-birth.required")))
          }
          "provided invalid known fact for IRV" in {
            val data = Json.obj("knownFact.year" -> "aaaa", "knownFact.month" -> "aa", "knownFact.day" -> "aa")
            val fastTrackForm =
              knownFactsForm(dateOfBirthMapping).bind(data)
            fastTrackForm.errors shouldBe Seq(
              FormError("knownFact.year", List("error.year.invalid-format")),
              FormError("knownFact.month", List("error.month.invalid-format")),
              FormError("knownFact.day", List("error.day.invalid-format"))
            )
          }
          "provided empty known fact for VAT" in {
            val data = Json.obj("knownFact.year" -> "", "knownFact.month" -> "", "knownFact.day" -> "")
            val fastTrackForm = knownFactsForm(vatRegDateMapping).bind(data)
            fastTrackForm.errors shouldBe Seq(FormError("knownFact", List("error.vat-registration-date.required")))
          }
          "provided invalid known fact for VAT" in {
            val data = Json.obj("knownFact.year" -> "aaaa", "knownFact.month" -> "aa", "knownFact.day" -> "aa")
            val fastTrackForm = knownFactsForm(vatRegDateMapping).bind(data)
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
}
