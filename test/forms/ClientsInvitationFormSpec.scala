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

import play.api.libs.json.Json
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ClientsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ConfirmForm
import uk.gov.hmrc.play.test.UnitSpec

class ClientsInvitationFormSpec extends UnitSpec {

  "ConfirmInvite form" should {
    "return no error for the input - Yes" in {
      val result = confirmInvitationForm.bind(Json.obj("confirmInvite" -> "true"))

      result.errors.isEmpty shouldBe true
    }

    "return no error for the input - No" in {
      val result = confirmInvitationForm.bind(Json.obj("confirmInvite" -> "false"))

      result.errors.isEmpty shouldBe true
    }

    "return an error for an invalid input" in {
      val result = confirmInvitationForm.bind(Json.obj("confirmInvite" -> ""))

      result.errors.length shouldBe 1
      result.errors.map(_.message).contains("error.confirmInvite.invalid") shouldBe true
    }

    "return no errors when unbinding the form" in {
      val unboundForm = confirmInvitationForm.mapping.unbind(ConfirmForm(Some(true)))
      unboundForm("confirmInvite") shouldBe "true"
    }
  }

  "ConfirmTerms form" should {
    "return no error for a valid input - Yes" in {
      val result = confirmTermsForm.bind(Json.obj("confirmTerms" -> "true"))

      result.errors.isEmpty shouldBe true
    }

    "return no error for a valid input - No" in {
      val result = confirmTermsForm.bind(Json.obj("confirmTerms" -> "false"))

      result.errors.isEmpty shouldBe true
    }

    "return an error for an invalid input" in {
      val result = confirmTermsForm.bind(Json.obj("confirmTerms" -> ""))

      result.errors.length shouldBe 1
      result.errors.map(_.message).contains("error.confirmTerms.invalid") shouldBe true
    }

    "return no errors when unbinding the form" in {
      val unboundForm = confirmTermsForm.mapping.unbind(ConfirmForm(Some(true)))
      unboundForm("confirmTerms") shouldBe "true"
    }
  }

}
