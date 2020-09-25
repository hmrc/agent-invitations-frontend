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

package models

import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCCGTPD, HMRCMTDIT}
import uk.gov.hmrc.agentinvitationsfrontend.models.SuspensionDetails
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId}
import uk.gov.hmrc.play.test.UnitSpec

class SuspensionDetailsSpec extends UnitSpec {

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

  "getSuspendedRegimes helper method" should {
    "return only the suspended regimes" in {
      SuspensionDetails(suspensionStatus = true, Some(Set("ITSA", "VATC")))
        .getSuspendedRegimes(Set(HMRCMTDIT, HMRCCGTPD)) shouldBe Set("ITSA")
    }

    "return all of the regimes if ALL are suspended" in {
      SuspensionDetails(suspensionStatus = true, Some(Set("ALL")))
        .getSuspendedRegimes(Set(HMRCMTDIT, HMRCCGTPD)) shouldBe Set("ITSA", "CGT")
    }
  }

  "isAgentSuspended helper method" should {
    "return true if the agent is suspended for any regimes in the consents" in {
      SuspensionDetails(suspensionStatus = true, Some(Set("ITSA", "VATC")))
        .isAgentSuspended(Set(HMRCMTDIT)) shouldBe true
    }
    "return false if the agent is not suspended for any service in the consents" in {
      SuspensionDetails(suspensionStatus = true, Some(Set("ITSA", "VATC")))
        .isAgentSuspended(Set(HMRCCGTPD)) shouldBe false
    }
  }

  "isRegimeSuspended helper method" should {

    "return true when the regime is suspended" in {
      SuspensionDetails(suspensionStatus = true, Some(Set("ITSA", "VATC"))).isRegimeSuspended(HMRCMTDIT) shouldBe true
    }

    "return true when ALL regimes are suspended" in {
      SuspensionDetails(suspensionStatus = true, Some(Set("ALL"))).isRegimeSuspended(HMRCMTDIT) shouldBe true
    }

    "return false when the regime is not suspended" in {
      SuspensionDetails(suspensionStatus = true, Some(Set("ITSA", "VATC")))
        .isRegimeSuspended(HMRCCGTPD) shouldBe false
    }
  }
}
