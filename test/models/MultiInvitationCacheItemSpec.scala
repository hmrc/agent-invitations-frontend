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

package models

import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.models.{Consent, MultiInvitationsCacheItem}
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId
import uk.gov.hmrc.play.test.UnitSpec

class MultiInvitationCacheItemSpec extends UnitSpec {

  val expiryDate = LocalDate.now()

  "MultiInvitationsCacheItem" should {
    "have allDeclinedProcessed" in {
      MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false, true),
        Consent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false, true),
        Consent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false, true)), Some("My Agency Name")).allDeclinedProcessed shouldBe true

      MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false, true),
        Consent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false, false),
        Consent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false, true)), Some("My Agency Name")).allDeclinedProcessed shouldBe true

      MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = true, true),
        Consent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true, true),
        Consent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true, true)), Some("My Agency Name")).allDeclinedProcessed shouldBe false
    }
    "have allProcessed" in {
      MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false, true),
        Consent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true, true),
        Consent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true, true)), Some("My Agency Name")).allProcessed shouldBe true

      MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false, true),
        Consent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true, false),
        Consent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true, true)), Some("My Agency Name")).allProcessed shouldBe false

      MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false, processed = true),
        Consent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false, processed = true),
        Consent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false, processed = true)), Some("My Agency Name")).allProcessed shouldBe true
    }
    "have allAcceptanceFailed" in {
      MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false, processed = true),
        Consent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true, processed = true),
        Consent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true, processed = true)), Some("My Agency Name")).allAcceptanceFailed shouldBe false

      MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = true, processed = false),
        Consent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true, processed = false),
        Consent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true, processed = false)), Some("My Agency Name")).allAcceptanceFailed shouldBe true
    }
    "have someAcceptanceFailed" in {
      MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false, processed = true),
        Consent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true, processed = false),
        Consent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true, processed = true)), Some("My Agency Name")).someAcceptanceFailed shouldBe true

      MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false, processed = true),
        Consent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true, processed = true),
        Consent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true, processed = true)), Some("My Agency Name")).someAcceptanceFailed shouldBe false
    }
  }
}
