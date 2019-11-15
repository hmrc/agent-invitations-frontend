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
import uk.gov.hmrc.agentinvitationsfrontend.connectors.SuspendedServices
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State.{SingleConsent, SuspendedAgent}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientConsent
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.personal
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

  "intersectConsentAndSuspension helper method" should {
    val itsaConsent = ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false)
    val afiConsent = ClientConsent(invitationIdIrv, expiryDate, "afi", consent = false)
    val vatConsent = ClientConsent(invitationIdVat, expiryDate, "vat", consent = false)
    val cgtConsent = ClientConsent(invitationIdCgt, expiryDate, "cgt", consent = false)
    "go to suspended state when agent is suspended for all consent services" in {
      await(
        SuspendedServices(Set(HMRCMTDIT, HMRCMTDVAT, HMRCCGTPD)).intersectConsentAndSuspension(
          Seq(itsaConsent, vatConsent, cgtConsent),
          nonSuspendedConsents =>
            SingleConsent(personal, "uid", "agent-name", itsaConsent, Seq(itsaConsent, vatConsent, cgtConsent))
        )) shouldBe SuspendedAgent(Set("itsa", "vat", "cgt"))
    }

    "go to target state with non suspended services when some consent services are suspended and some are not" in {
      await(
        SuspendedServices(Set(HMRCMTDIT, HMRCMTDVAT, HMRCCGTPD)) intersectConsentAndSuspension (
          Seq(itsaConsent, afiConsent, vatConsent, cgtConsent),
          nonSuspendedConsents => SingleConsent(personal, "uid", "agent-name", itsaConsent, nonSuspendedConsents)
        )) shouldBe SingleConsent(personal, "uid", "agent-name", itsaConsent, Seq(afiConsent))
    }

    "go to target state with all services none of the consent services are suspended" in {
      await(
        SuspendedServices(Set(HMRCCGTPD)).intersectConsentAndSuspension(
          Seq(itsaConsent, afiConsent, vatConsent),
          nonSuspendedConsents => SingleConsent(personal, "uid", "agent-name", itsaConsent, nonSuspendedConsents)
        )) shouldBe SingleConsent(personal, "uid", "agent-name", itsaConsent, Seq(itsaConsent, afiConsent, vatConsent))
    }
  }
}
