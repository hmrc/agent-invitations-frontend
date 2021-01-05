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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.CancelRequestForm
import uk.gov.hmrc.agentinvitationsfrontend.controllers.testing.TestEndpointsController._
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId
import uk.gov.hmrc.play.test.UnitSpec

class CancelRequestFormSpec extends UnitSpec {

  val itsaService = "HMRC-MTD-IT"
  val invitationIdITSA = InvitationId("ABERULMHCKKW3")
  val clientName = "Gemma Collins"

  "Cancel Request form" should {
    "return no error with valid input" in {
      val result =
        testCancelRequestForm.bind(
          Json.obj("invitationId" -> invitationIdITSA.value, "service" -> itsaService, "clientType" -> "personal", "clientName" -> clientName))

      result.errors.isEmpty shouldBe true
    }

    "return an error when service is invalid" in {
      val result =
        testCancelRequestForm.bind(
          Json.obj("invitationId" -> invitationIdITSA.value, "service" -> "foo", "clientType" -> "personal", "clientName" -> clientName))

      result.errors shouldBe Seq(FormError("service", List("Unsupported Service")))
    }

    "return an error when invitationId is invalid" in {
      val result =
        testCancelRequestForm.bind(
          Json.obj("invitationId" -> "foo", "service" -> itsaService, "clientType" -> "personal", "clientName" -> clientName))

      result.errors shouldBe Seq(FormError("invitationId", List("Invalid invitation Id")))
    }

    "return no errors when unbinding the form" in {
      val unboundForm =
        testCancelRequestForm.mapping.unbind(CancelRequestForm(invitationIdITSA.value, itsaService, "personal", clientName))
      unboundForm("service") shouldBe itsaService
      unboundForm("invitationId") shouldBe invitationIdITSA.value
      unboundForm("clientName") shouldBe clientName
      unboundForm("clientType") shouldBe "personal"
    }
  }
}
