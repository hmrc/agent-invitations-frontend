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

package uk.gov.hmrc.agentinvitationsfrontend.models

import org.joda.time.LocalDate
import play.api.libs.json.Json
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId
import uk.gov.hmrc.http.controllers.RestFormats.localDateFormats

case class ClientConsent(invitationId: InvitationId, expiryDate: LocalDate, serviceKey: String, consent: Boolean, processed: Boolean = false) {

  def service: String = Services.determineServiceFromServiceMessageKey(this.serviceKey)
}

object ClientConsent {
  implicit val format = Json.format[ClientConsent]

  def allAcceptedProcessed(consents: Seq[ClientConsent]): Boolean = {
    val acceptedConsents = consents.filter(_.consent)
    acceptedConsents.nonEmpty && acceptedConsents.forall(_.processed == true)
  }

  def allDeclinedProcessed(consents: Seq[ClientConsent]): Boolean = {
    val declinedConsents = consents.filter(_.consent == false)
    declinedConsents.nonEmpty && declinedConsents.forall(_.processed == true)
  }

  def allFailed(consents: Seq[ClientConsent]): Boolean = consents.forall(_.processed == false)

  def someFailed(consents: Seq[ClientConsent]): Boolean = consents.exists(_.processed == false)
}
