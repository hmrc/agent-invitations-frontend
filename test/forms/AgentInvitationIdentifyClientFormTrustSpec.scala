/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.agentinvitationsfrontend.forms.TrustClientForm
import uk.gov.hmrc.agentinvitationsfrontend.models.TrustClient
import uk.gov.hmrc.agentmtdidentifiers.model.{Urn, Utr}
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationIdentifyClientFormTrustSpec extends UnitSpec {

  val trustClientFormUrnEnabled = TrustClientForm.form(true)

  val validUtrFormData = Json.obj("taxId"   -> "8571842718")
  val validUrnFormData = Json.obj("taxId"   -> "XATRUST59123128")
  val invalidUtrFormData = Json.obj("taxId" -> "012345")
  val invalidUrnFormData = Json.obj("taxId" -> "XXXXXXX12345678")

  val errUtrOnlyEmpty = "error.utr.required"
  val errUtrOnlyInvalid = "enter-utr.invalid-format"
  val errUrnEnabledEmpty = "error.urn.required"
  val errUrnEnabledInvalid = "enter-urn.invalid-format"

  "TrustClientForm - URN enabled = false" when {

    val trustClientForm = TrustClientForm.form(false)

    "return no error message" when {
      "UTR is valid" in {
        trustClientForm.bind(validUtrFormData).errors.isEmpty shouldBe true
      }
      "unbinding the form" in {
        val unboundForm = trustClientForm.mapping.unbind(TrustClient(Utr("8571842718")))
        unboundForm("taxId") shouldBe "8571842718"
      }
    }

    "return an error message" when {
      "Utr is invalid" in {
        trustClientForm.bind(invalidUtrFormData).errors shouldBe Seq(FormError("taxId", List(errUtrOnlyInvalid)))
      }

      "nothing entered" in {
        trustClientForm.bind(Map("taxId" -> "")).errors shouldBe Seq(FormError("taxId", List(errUtrOnlyEmpty)))
      }

      "Urn is entered" in {
        trustClientForm.bind(validUrnFormData).errors shouldBe Seq(FormError("taxId", List(errUtrOnlyInvalid)))
      }
    }
  }

  "TrustClientForm - URN enabled = true" when {
    val trustClientForm = TrustClientForm.form(true)

    "return no error message" when {
      "UTR is valid" in {
        trustClientForm.bind(validUtrFormData).errors.isEmpty shouldBe true
      }

      "URN is valid" in {
        trustClientForm.bind(validUrnFormData).errors.isEmpty shouldBe true
      }
      "unbinding the form with Utr" in {
        val unboundForm = trustClientForm.mapping.unbind(TrustClient(Utr("8571842718")))
        unboundForm("taxId") shouldBe "8571842718"
      }

      "unbinding the form with Urn" in {
        val unboundForm = trustClientForm.mapping.unbind(TrustClient(Urn("XXTRUST12345678")))
        unboundForm("taxId") shouldBe "XXTRUST12345678"
      }
    }

    "return an error message" when {
      "Utr is invalid" in {
        trustClientForm.bind(invalidUtrFormData).errors shouldBe Seq(FormError("taxId", List(errUrnEnabledInvalid)))
      }

      "Urn is invalid" in {
        trustClientForm.bind(invalidUrnFormData).errors shouldBe Seq(FormError("taxId", List(errUrnEnabledInvalid)))
      }

      "nothing entered" in {
        trustClientForm.bind(Map("taxId" -> "")).errors shouldBe Seq(FormError("taxId", List(errUrnEnabledEmpty)))
      }
    }
  }
}
