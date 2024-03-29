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

package models

import java.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.models.{ClientConsent, ClientConsentsJourneyState}
import uk.gov.hmrc.agentmtdidentifiers.model.{InvitationId, Service}
import support.UnitSpec

class ClientConsentsJourneyStateSpec extends UnitSpec {

  val expiryDate = LocalDate.now()

  "ClientConsentsJourneyState" should {
    "have allDeclinedProcessed" in {
      ClientConsentsJourneyState(
        Seq(
          ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, Service.MtdIt, consent = false, true),
          ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, Service.PersonalIncomeRecord, consent = false, true),
          ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, Service.Vat, consent = false, true)
        ),
        Some("My Agency Name")
      ).allDeclinedProcessed shouldBe true

      ClientConsentsJourneyState(
        Seq(
          ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, Service.MtdIt, consent = false, true),
          ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, Service.PersonalIncomeRecord, consent = false, false),
          ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, Service.Vat, consent = false, true)
        ),
        Some("My Agency Name")
      ).allDeclinedProcessed shouldBe true

      ClientConsentsJourneyState(
        Seq(
          ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, Service.MtdIt, consent = true, true),
          ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, Service.PersonalIncomeRecord, consent = true, true),
          ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, Service.Vat, consent = true, true)
        ),
        Some("My Agency Name")
      ).allDeclinedProcessed shouldBe false
    }
    "have allProcessed" in {
      ClientConsentsJourneyState(
        Seq(
          ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, Service.MtdIt, consent = false, true),
          ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, Service.PersonalIncomeRecord, consent = true, true),
          ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, Service.Vat, consent = true, true)
        ),
        Some("My Agency Name")
      ).allProcessed shouldBe true

      ClientConsentsJourneyState(
        Seq(
          ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, Service.MtdIt, consent = false, true),
          ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, Service.PersonalIncomeRecord, consent = true, false),
          ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, Service.Vat, consent = true, true)
        ),
        Some("My Agency Name")
      ).allProcessed shouldBe false

      ClientConsentsJourneyState(
        Seq(
          ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, Service.MtdIt, consent = false, processed = true),
          ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, Service.PersonalIncomeRecord, consent = false, processed = true),
          ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, Service.Vat, consent = false, processed = true)
        ),
        Some("My Agency Name")
      ).allProcessed shouldBe true
    }
    "have allAcceptanceFailed" in {
      ClientConsentsJourneyState(
        Seq(
          ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, Service.MtdIt, consent = false, processed = true),
          ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, Service.PersonalIncomeRecord, consent = true, processed = true),
          ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, Service.Vat, consent = true, processed = true)
        ),
        Some("My Agency Name")
      ).allAcceptanceFailed shouldBe false

      ClientConsentsJourneyState(
        Seq(
          ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, Service.MtdIt, consent = true, processed = false),
          ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, Service.PersonalIncomeRecord, consent = true, processed = false),
          ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, Service.Vat, consent = true, processed = false)
        ),
        Some("My Agency Name")
      ).allAcceptanceFailed shouldBe true
    }
    "have someAcceptanceFailed" in {
      ClientConsentsJourneyState(
        Seq(
          ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, Service.MtdIt, consent = false, processed = true),
          ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, Service.PersonalIncomeRecord, consent = true, processed = false),
          ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, Service.Vat, consent = true, processed = true)
        ),
        Some("My Agency Name")
      ).someAcceptanceFailed shouldBe true

      ClientConsentsJourneyState(
        Seq(
          ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, Service.MtdIt, consent = false, processed = true),
          ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, Service.PersonalIncomeRecord, consent = true, processed = true),
          ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, Service.Vat, consent = true, processed = true)
        ),
        Some("My Agency Name")
      ).someAcceptanceFailed shouldBe false
    }
  }
}
