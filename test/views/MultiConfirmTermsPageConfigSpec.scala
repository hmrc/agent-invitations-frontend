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

package views

import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientConsent
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.agentinvitationsfrontend.views.clients.MultiConfirmTermsPageConfig
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId

class MultiConfirmTermsPageConfigSpec extends UnitSpec {

  val invitationIdITSA = InvitationId("ABERULMHCKKW3")

  val invitationIdPIR = InvitationId("B9SCS2T4NZBAX")

  val invitationIdVAT = InvitationId("CZTW1KY6RTAAT")

  val expiryDate = LocalDate.now().plusDays(10)

  "MultiConfirmTermsPageConfig" should {
    "sort invitations by expiry date, descending order" in {
      val consent1 = ClientConsent(invitationIdITSA, expiryDate, "itsa", false)
      val consent2 = ClientConsent(invitationIdPIR, expiryDate.minusDays(3), "afi", false)
      val consent3 = ClientConsent(invitationIdVAT, expiryDate.minusDays(5), "vat", false)
      val consent4 = ClientConsent(invitationIdVAT, expiryDate.minusDays(3), "vat", false)
      val consent5 = ClientConsent(invitationIdITSA, expiryDate.plusDays(1), "itsa", false)

      val consents = Seq(consent1, consent4, consent2, consent5, consent3)
      val config = MultiConfirmTermsPageConfig("Impala Boolean Ltd", "personal", "12345678", consents)

      config.serviceKeyAndExpiryDateSeq should contain.only(consent4, consent2, consent5)

      config.expiryDateDescending(consent2, consent1) shouldBe true
      config.expiryDateDescending(consent3, consent2) shouldBe true
      config.expiryDateDescending(consent3, consent1) shouldBe true

      config.expiryDateDescending(consent1, consent2) shouldBe false
      config.expiryDateDescending(consent2, consent3) shouldBe false
      config.expiryDateDescending(consent1, consent3) shouldBe false
    }
  }

}
