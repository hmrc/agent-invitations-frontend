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
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentReferenceRecord
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId

sealed trait ConfirmDeclinePageConfig {
  def agencyName: String
  def backUrl: Call
  def submitUrl: Call
  def serviceKeys: Seq[String]
  def isSingle: Boolean
}

case class MultiConfirmDeclinePageConfig(
  clientType: String,
  agentReferenceRecord: AgentReferenceRecord,
  serviceKeys: Seq[String])
    extends ConfirmDeclinePageConfig {

  override val agencyName: String = agentReferenceRecord.normalisedAgentNames.last

  override val backUrl: Call =
    routes.ClientsInvitationController.warmUp(clientType, agentReferenceRecord.uid, agencyName)

  override val submitUrl: Call = routes.ClientsInvitationController.submitMultiConfirmDecline

  override val isSingle: Boolean = false

}

case class SingleConfirmDeclinePageConfig(agencyName: String, invitationId: InvitationId, serviceKey: String)
    extends ConfirmDeclinePageConfig {

  override def serviceKeys: Seq[String] = Seq(serviceKey)

  override val backUrl: Call = routes.ClientsInvitationController.start(invitationId)

  override val submitUrl: Call = routes.ClientsInvitationController.submitConfirmDecline(invitationId)

  override val isSingle: Boolean = true

}
