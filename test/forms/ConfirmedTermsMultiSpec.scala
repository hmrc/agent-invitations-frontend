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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ClientsMultiInvitationController.confirmTermsMultiForm
import uk.gov.hmrc.agentinvitationsfrontend.models.ConfirmedTerms
import uk.gov.hmrc.play.test.UnitSpec

class ConfirmedTermsMultiSpec extends UnitSpec {

  "ConfirmTermsMultiForm" should {

    "return no errors when filling the form" in {
      val unboundForm =
        confirmTermsMultiForm.fill(ConfirmedTerms(itsaConsent = true, afiConsent = true, vatConsent = true))
      unboundForm("confirmedTerms.itsa").value shouldBe Some("true")
      unboundForm("confirmedTerms.afi").value.get shouldBe "true"
      unboundForm("confirmedTerms.vat").value.get shouldBe "true"

      val unboundForm2 =
        confirmTermsMultiForm.fill(ConfirmedTerms(itsaConsent = true, afiConsent = false, vatConsent = true))
      unboundForm2("confirmedTerms.itsa").value.get shouldBe "true"
      unboundForm2("confirmedTerms.afi").value.get shouldBe "false"
      unboundForm2("confirmedTerms.vat").value.get shouldBe "true"

      val unboundForm3 =
        confirmTermsMultiForm.fill(ConfirmedTerms(itsaConsent = false, afiConsent = false, vatConsent = false))
      unboundForm3("confirmedTerms.itsa").value.get shouldBe "false"
      unboundForm3("confirmedTerms.afi").value.get shouldBe "false"
      unboundForm3("confirmedTerms.vat").value.get shouldBe "false"
    }
  }

}
