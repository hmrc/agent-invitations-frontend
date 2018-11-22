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

package uk.gov.hmrc.agentinvitationsfrontend.views.clients

import play.api.mvc.Call
import uk.gov.hmrc.agentinvitationsfrontend.controllers.routes
import uk.gov.hmrc.agentinvitationsfrontend.models.Consent
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId

case class MultiConfirmTermsPageConfig(
  agencyName: String,
  clientType: String,
  uid: String,
  invitationIdWithServiceKeySeq: Seq[Consent]) {

  val backUrl: Call =
    routes.ClientsMultiInvitationController.warmUp(clientType, uid, agencyName)

  val submitUrl: Call =
    routes.ClientsMultiInvitationController.submitMultiConfirmTerms(clientType, uid)

}

case class SingleConfirmTermsPageConfig(agencyName: String, invitationId: InvitationId, serviceKey: String) {

  val backUrl: Call = routes.ClientsInvitationController.start(invitationId)

  val submitUrl: Call = routes.ClientsInvitationController.submitConfirmTerms(invitationId)

}
