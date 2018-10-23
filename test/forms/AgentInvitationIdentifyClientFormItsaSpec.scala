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
import play.api.libs.json.{JsString, Json}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{AgentsInvitationController, FeatureFlags}
import uk.gov.hmrc.agentinvitationsfrontend.models.UserInputNinoAndPostcode
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationIdentifyClientFormItsaSpec extends UnitSpec {

  "IdentifyClientForm" when {

    "feature flags are on" when {

      val featureFlags = FeatureFlags()
      val agentInvitationIdentifyClientForm =
        AgentsInvitationController.agentInvitationIdentifyClientFormItsa(featureFlags)
      val validData = Json.obj(
        "clientType"       -> "individual",
        "clientIdentifier" -> "WM123456C",
        "service"          -> "HMRC-MTD-IT",
        "knownFact"        -> "W12 7TQ")

      "return no error message" when {
        "NINO and postcode are valid" in {
          agentInvitationIdentifyClientForm.bind(validData).errors.isEmpty shouldBe true
        }

        "NINO and postcode are valid, but postcode contains spaces" in {
          val dataWithPostcodeSpaces = validData + ("knownFact" -> JsString("  W12 7TQ  "))
          agentInvitationIdentifyClientForm.bind(dataWithPostcodeSpaces).errors.isEmpty shouldBe true
        }

        "NINO and postcode are valid, but knownFact is lower case" in {
          val dataWithPostcodeLowercase = validData + ("knownFact" -> JsString("w12 7tq"))
          agentInvitationIdentifyClientForm.bind(dataWithPostcodeLowercase).errors.isEmpty shouldBe true
        }

        "NINO and postcode are valid, but NINO has spaces" in {
          val dataWithPostcodeLowercase = validData + ("clientIdentifier" -> JsString("  WM123456C  "))
          agentInvitationIdentifyClientForm.bind(dataWithPostcodeLowercase).errors.isEmpty shouldBe true
        }

        "NINO and postcode are valid, but NINO is lower case" in {
          val dataWithPostcodeLowercase = validData + ("clientIdentifier" -> JsString("wn123456c"))
          agentInvitationIdentifyClientForm.bind(dataWithPostcodeLowercase).errors.isEmpty shouldBe true
        }

        "unbinding the form" in {
          val unboundForm = agentInvitationIdentifyClientForm.mapping.unbind(
            UserInputNinoAndPostcode("individual", "HMRC-MTD-IT", Some("AE123456C"), Some("AA1 1AA"))
          )
          unboundForm("knownFact") shouldBe "AA1 1AA"
          unboundForm("clientIdentifier") shouldBe "AE123456C"
        }
      }

      "return an error message" when {
        "postcode is invalid" in {
          val dataWithInvalidPostcode = validData + ("knownFact" -> JsString("W12"))
          val postcodeForm = agentInvitationIdentifyClientForm.bind(dataWithInvalidPostcode)
          postcodeForm.errors shouldBe Seq(FormError("knownFact", List("enter-postcode.invalid-format")))
        }

        "postcode is empty" in {
          val dataWithEmptyPostcode = validData + ("knownFact" -> JsString(""))
          val postcodeForm = agentInvitationIdentifyClientForm.bind(dataWithEmptyPostcode)
          postcodeForm.errors shouldBe Seq(FormError("knownFact", List("error.postcode.required")))
        }

        "NINO is invalid" in {
          val dataWithInvalidNino = validData + ("clientIdentifier" -> JsString("12345"))
          val ninoForm = agentInvitationIdentifyClientForm.bind(dataWithInvalidNino)
          ninoForm.errors shouldBe Seq(FormError("clientIdentifier", List("enter-nino.invalid-format")))
        }

        "NINO is empty" in {
          val dataWithEmptyNino = validData + ("clientIdentifier" -> JsString(""))
          val ninoForm = agentInvitationIdentifyClientForm.bind(dataWithEmptyNino)
          ninoForm.errors shouldBe Seq(FormError("clientIdentifier", List("error.nino.required")))
        }
      }
    }

    "feature flags are off" when {
      val featureFlags = FeatureFlags().copy(showKfcMtdIt = false)
      val agentInvitationIdentifyClientForm =
        AgentsInvitationController.agentInvitationIdentifyClientFormItsa(featureFlags)
      val validData = Json.obj(
        "clientType"       -> "individual",
        "clientIdentifier" -> "WM123456C",
        "service"          -> "HMRC-MTD-IT",
        "knownFact"        -> "W12 7TQ")

      "return no error message" when {
        "NINO and postcode are valid" in {
          agentInvitationIdentifyClientForm.bind(validData).errors.isEmpty shouldBe true
        }

        "NINO is valid but postcode is not" in {
          val dataWithInvalidPostcode = validData + ("knownFact" -> JsString("W12"))
          agentInvitationIdentifyClientForm.bind(dataWithInvalidPostcode).errors.isEmpty shouldBe true
        }

        "NINO is valid and postcode is empty" in {
          val dataWithEmptyPostcode = validData + ("knownFact" -> JsString(""))
          agentInvitationIdentifyClientForm.bind(dataWithEmptyPostcode).errors.isEmpty shouldBe true
        }
      }

      "return an error message" when {
        "NINO is invalid" in {
          val dataWithInvalidPostcode = validData + ("clientIdentifier" -> JsString("12345"))
          agentInvitationIdentifyClientForm.bind(dataWithInvalidPostcode).errors shouldBe Seq(
            FormError("clientIdentifier", List("enter-nino.invalid-format")))
        }
        "NINO is empty" in {
          val dataWithEmptyNino = validData + ("clientIdentifier" -> JsString(""))
          agentInvitationIdentifyClientForm.bind(dataWithEmptyNino).errors shouldBe Seq(
            FormError("clientIdentifier", List("error.nino.required")))
        }
      }
    }
  }
}
