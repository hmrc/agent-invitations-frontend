/*
 * Copyright 2020 HM Revenue & Customs
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
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType
import uk.gov.hmrc.play.test.UnitSpec

class TrackInformationFormSpec extends UnitSpec {

  val itsaService = "HMRC-MTD-IT"
  val clientTypePersonal = "personal"
  val expiryDate = LocalDate.now(DateTimeZone.UTC).plusDays(5).toString

  "ConfirmInvite form" should {
    "return no error with valid input" in {
      val result =
        testTrackInformationForm.bind(
          Json.obj("service" -> itsaService, "clientType" -> clientTypePersonal, "expiryDate" -> expiryDate))

      result.errors.isEmpty shouldBe true
    }

    "return an error when service is invalid" in {
      val result =
        testTrackInformationForm.bind(
          Json.obj("service" -> "foo", "clientType" -> clientTypePersonal, "expiryDate" -> expiryDate))

      result.errors shouldBe Seq(FormError("service", List("Unsupported Service")))
    }

    "return and error when clientType is invalid" in {
      val result =
        testTrackInformationForm.bind(
          Json.obj("service" -> itsaService, "clientType" -> "foo", "expiryDate" -> expiryDate))

      result.errors shouldBe Seq(FormError("clientType", List("Unsupported client type")))
    }

    "return an error when expiryDate is invalid" in {
      val result =
        testTrackInformationForm.bind(
          Json.obj("service" -> itsaService, "clientType" -> clientTypePersonal, "expiryDate" -> "foo"))

      result.errors shouldBe Seq(FormError("expiryDate", List("Invalid date format")))
    }

    "return no errors when unbinding the form" in {
      val unboundForm =
        testTrackInformationForm.mapping.unbind(TrackResendForm(itsaService, Some(ClientType.personal), expiryDate))
      unboundForm("service") shouldBe itsaService
      unboundForm("clientType") shouldBe "personal"
      unboundForm("expiryDate") shouldBe expiryDate
    }
  }

}
