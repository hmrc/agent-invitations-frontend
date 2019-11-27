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

package models

import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.connectors.SuspensionResponse
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCCGTPD, HMRCMTDIT, HMRCMTDVAT}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId}
import uk.gov.hmrc.play.test.UnitSpec

class SuspendedServicesSpec extends UnitSpec {

  val nino = "AB123456A"
  val arn = Arn("TARN0000001")
  val postCode = Some("BN114AW")
  val vrn = "123456"
  val vatRegDate = Some("2010-10-10")
  val dob = Some("1990-10-10")
  val uid = "uid123"
  val invitationIdItsa = InvitationId("A1BEOZEO7MNO6")
  val invitationIdIrv = InvitationId("B1BEOZEO7MNO6")
  val invitationIdVat = InvitationId("C1BEOZEO7MNO6")
  val invitationIdTrust = InvitationId("D1BEOZEO7MNO6")
  val invitationIdCgt = InvitationId("E1BEOZEO7MNO6")
  val expiryDate = LocalDate.parse("2010-01-01")

  "getSuspendedServices helper method" should {

    "getSuspendedServices helper method" should {

      "return only the suspended services" in {
        SuspensionResponse(Set(HMRCMTDVAT, HMRCMTDIT)).getSuspendedServices(Set(HMRCMTDIT, HMRCCGTPD)) shouldBe Set(
          HMRCMTDIT)
      }
    }

    "isAllSuspended helper method" should {

      "return true if all the services are suspended" in {
        SuspensionResponse(Set(HMRCMTDVAT, HMRCMTDIT)).isAllSuspended(Set(HMRCMTDIT, HMRCMTDVAT)) shouldBe true
      }

      "return false if not all the services are suspended" in {
        SuspensionResponse(Set(HMRCMTDVAT, HMRCMTDIT)).isAllSuspended(Set(HMRCMTDIT, HMRCCGTPD)) shouldBe false
      }
    }

    "isSuspended helper method" should {

      "return true when the service is suspended" in {
        SuspensionResponse(Set(HMRCMTDVAT, HMRCMTDIT)).isSuspendedService(HMRCMTDIT) shouldBe true
      }

      "return false when the service is not suspended" in {
        SuspensionResponse(Set(HMRCMTDVAT, HMRCMTDIT)).isSuspendedService(HMRCCGTPD) shouldBe false
      }
    }
  }
}
