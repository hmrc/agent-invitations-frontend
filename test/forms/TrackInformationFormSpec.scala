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

import org.joda.time.{DateTimeZone, LocalDate}
import play.api.data.FormError
import play.api.libs.json.Json
import uk.gov.hmrc.agentinvitationsfrontend.controllers.TrackResendForm
import uk.gov.hmrc.agentinvitationsfrontend.controllers.testing.TestEndpointsController._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId}
import uk.gov.hmrc.play.test.UnitSpec

class TrackInformationFormSpec extends UnitSpec {

  val itsaService = "HMRC-MTD-IT"
  val invitationIdITSA = InvitationId("ABERULMHCKKW3")
  val expiryDate = LocalDate.now(DateTimeZone.UTC).plusDays(5).toString

  "ConfirmInvite form" should {
    "return no error with valid input" in {
      val result =
        testTrackInformationForm.bind(
          Json.obj("service" -> itsaService, "invitationId" -> invitationIdITSA.value, "expiryDate" -> expiryDate))

      result.errors.isEmpty shouldBe true
    }

    "return an error when service is invalid" in {
      val result =
        testTrackInformationForm.bind(
          Json.obj("service" -> "foo", "invitationId" -> invitationIdITSA.value, "expiryDate" -> expiryDate))

      result.errors shouldBe Seq(FormError("service", List("Unsupported Service")))
    }

    "return and error when invitationId is invalid" in {
      val result =
        testTrackInformationForm.bind(
          Json.obj("service" -> itsaService, "invitationId" -> "foo", "expiryDate" -> expiryDate))

      result.errors shouldBe Seq(FormError("invitationId", List("Invalid invitation Id")))
    }

    "return an error when expiryDate is invalid" in {
      val result =
        testTrackInformationForm.bind(
          Json.obj("service" -> itsaService, "invitationId" -> invitationIdITSA.value, "expiryDate" -> "foo"))

      result.errors shouldBe Seq(FormError("expiryDate", List("Invalid date format")))
    }

    "return no errors when unbinding the form" in {
      val unboundForm =
        testTrackInformationForm.mapping.unbind(TrackResendForm(itsaService, invitationIdITSA.value, expiryDate))
      unboundForm("service") shouldBe itsaService
      unboundForm("invitationId") shouldBe invitationIdITSA.value
      unboundForm("expiryDate") shouldBe expiryDate
    }
  }

}
