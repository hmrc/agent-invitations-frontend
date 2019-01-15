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

package uk.gov.hmrc.agentinvitationsfrontend.views.clients

import uk.gov.hmrc.agentinvitationsfrontend.models.ClientConsent
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId

sealed trait CompletePageConfig {
  def agencyName: String
  def consents: Seq[ClientConsent]
  def isSingle: Boolean
}

case class MultiCompletePageConfig(agencyName: String, consents: Seq[ClientConsent]) extends CompletePageConfig {

  override val isSingle: Boolean = false

}

case class SingleCompletePageConfig(agencyName: String, invitationId: InvitationId, consent: ClientConsent)
    extends CompletePageConfig {

  override def consents: Seq[ClientConsent] = Seq(consent)

  override val isSingle: Boolean = true

}
