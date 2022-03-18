/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.agentinvitationsfrontend.views.agents

import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{FeatureFlags, routes}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.Basket
import uk.gov.hmrc.agentinvitationsfrontend.models.{AuthorisedAgent, ClientType, InvitationsBasket, Services}
import uk.gov.hmrc.agentmtdidentifiers.model.Service

case class SelectServicePageConfig(
  clientType: ClientType,
  basket: Basket,
  featureFlags: FeatureFlags,
  services: Set[Service],
  backLink: String,
  reviewAuthsCall: Call)(implicit messages: Messages) {

  /** Whether to show the multiple service selection form for this client type journey */
  val showMultiSelect: Boolean = availableServices.size > 1

  protected def firstService: Service =
    availableServices.head._1

  def remainingService: Service = firstService

  def submitCall: Call = (clientType, showMultiSelect) match {
    case (clientType, false) => routes.AgentInvitationJourneyController.submitSelectServiceSingle(remainingService.id, clientType.toString)
    case (clientType, true)  => routes.AgentInvitationJourneyController.submitSelectServiceMulti()
  }

  /** The list of available services for the user to select,
    * based on what is available according to feature enablement
    * and what they have already selected in their basket
    * */
  def availableServices: Seq[(Service, String)] =
    InvitationsBasket(clientType, services, basket, featureFlags).availableServices.toSeq.sortBy(_._1)(Services.serviceDisplayOrdering)

  /** The header to use when multiple trust service selections available */
  def selectHeaderMessage(implicit messages: Messages): String = Messages("select-service.header")

  /** The header to use when only a single service is available or left to choose */
  def selectSingleHeaderMessage: String =
    Messages(s"select-single-service.${firstService.id}.$clientType.header")

}
