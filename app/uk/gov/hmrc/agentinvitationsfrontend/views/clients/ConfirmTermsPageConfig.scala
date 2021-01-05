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

package uk.gov.hmrc.agentinvitationsfrontend.views.clients

import org.joda.time.LocalDate
import play.api.mvc.Call
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientConsent

case class ConfirmTermsPageConfig(
  agencyName: String,
  clientType: String,
  uid: String,
  consentSeq: Seq[ClientConsent],
  submitUrl: Call,
  checkAnswersUrl: Call,
  backLink: Call) {

  val expiryDateDescending: (ClientConsent, ClientConsent) => Boolean = (c1, c2) => c2.expiryDate.isAfter(c1.expiryDate)

  val serviceKeyAndExpiryDateSeq: Seq[ClientConsent] = {
    consentSeq
      .sortWith(expiryDateDescending)
      .map(consent => consent.serviceKey -> consent)
      .toMap
      .values
      .toSeq
      .sortWith(expiryDateDescending)
  }

  def isPending(consent: ClientConsent): Boolean =
    if (consent.expiryDate.isBefore(LocalDate.now())) false else true

  val isSingleConsent = consentSeq.size == 1
}
