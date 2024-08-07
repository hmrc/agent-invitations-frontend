/*
 * Copyright 2023 HM Revenue & Customs
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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.CancelAuthorisationForm
import uk.gov.hmrc.agentinvitationsfrontend.controllers.testing.TestEndpointsController._
import support.UnitSpec

class CancelAuthorisationFormSpec extends UnitSpec {

  val itsaService = "HMRC-MTD-IT"
  val clientId = "AB123456A"
  val clientName = "Samantha Mumba"
  val clientType = "personal"
  val invitationId = "ALFE93Y9KAELF"

  "ConfirmInvite form" should {
    "return no error with valid input" in {
      val result =
        testCancelAuthorisationForm.bind(
          Json.obj(
            "service"      -> itsaService,
            "clientId"     -> clientId,
            "clientName"   -> clientName,
            "clientType"   -> clientType,
            "invitationId" -> invitationId,
            "status"       -> "Partialauth"
          )
        )

      result.errors.isEmpty shouldBe true
    }

    "return an error when service is invalid" in {
      val result =
        testCancelAuthorisationForm.bind(
          Json
            .obj(
              "service"      -> "foo",
              "clientId"     -> clientId,
              "clientName"   -> clientName,
              "clientType"   -> clientType,
              "invitationId" -> invitationId,
              "status"       -> "Partialauth"
            )
        )

      result.errors shouldBe Seq(FormError("service", List("Unsupported Service")))
    }

    "return an error when clientId is invalid" in {
      val result =
        testCancelAuthorisationForm.bind(
          Json
            .obj(
              "service"      -> itsaService,
              "clientId"     -> "foo",
              "clientName"   -> clientName,
              "clientType"   -> clientType,
              "invitationId" -> invitationId,
              "status"       -> "Partialauth"
            )
        )

      result.errors shouldBe Seq(FormError("clientId", List("INVALID_CLIENT_ID_RECEIVED:foo")))
    }

    "return and error when clientType is invalid" in {
      val result =
        testCancelAuthorisationForm.bind(
          Json
            .obj(
              "service"      -> itsaService,
              "clientId"     -> clientId,
              "clientName"   -> clientName,
              "clientType"   -> "abc",
              "invitationId" -> invitationId,
              "status"       -> "Partialauth"
            )
        )

      result.errors shouldBe Seq(FormError("clientType", List("Unsupported ClientType")))
    }

    "return and error when status is invalid" in {
      val result =
        testCancelAuthorisationForm.bind(
          Json
            .obj(
              "service"      -> itsaService,
              "clientId"     -> clientId,
              "clientName"   -> clientName,
              "clientType"   -> clientType,
              "invitationId" -> invitationId,
              "status"       -> "foo"
            )
        )

      result.errors shouldBe Seq(FormError("status", List("Unexpected InvitationStatus")))
    }

    "return no errors when unbinding the form" in {
      val unboundForm =
        testCancelAuthorisationForm.mapping.unbind(CancelAuthorisationForm(itsaService, clientId, clientType, clientName, invitationId, "Accepted"))
      unboundForm("service") shouldBe itsaService
      unboundForm("clientId") shouldBe clientId
      unboundForm("clientName") shouldBe clientName
      unboundForm("clientType") shouldBe clientType
      unboundForm("invitationId") shouldBe invitationId
      unboundForm("status") shouldBe "Accepted"
    }
  }

}
