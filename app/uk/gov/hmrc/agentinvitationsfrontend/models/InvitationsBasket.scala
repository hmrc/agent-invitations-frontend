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

package uk.gov.hmrc.agentinvitationsfrontend.models

import play.api.i18n.Messages
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.Basket
import uk.gov.hmrc.agentmtdidentifiers.model.Service

/** The set of services available that an agent can choose for authorisation
  *
  */
case class InvitationsBasket(clientType: ClientType, services: Set[Service], basket: Basket, featureFlags: FeatureFlags) {

  /** Returns a set of pairs of string which can be used to populate a selection form
    *  depends on what the set of services are, and whether any are in your basket already
    * */
  def availableServices(implicit messages: Messages): Set[(Service, String)] =
    Services
      .supportedServicesFor(clientType)
      .filter(showService)
      .map(service => service -> Messages(s"select-service.${service.id}.${clientType.toString}"))

  def showService(service: Service): Boolean =
    featureFlags.isServiceEnabled(service, None) && serviceAvailableForSelection(service)

  protected def serviceAvailableForSelection(service: Service): Boolean =
    if (service == Service.Trust) {
      services.contains(service) && (!basket.exists(_.invitation.service == Service.Trust) && !basket.exists(_.invitation.service == Service.TrustNT))
    } else
      services.contains(service) && !basket.exists(_.invitation.service == service)
}
