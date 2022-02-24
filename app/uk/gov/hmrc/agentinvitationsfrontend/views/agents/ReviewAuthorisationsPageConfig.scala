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
import uk.gov.hmrc.agentinvitationsfrontend.models.{AuthorisationRequest, BusinessInvitationsBasket, ClientType, PersonalInvitationsBasket, TrustInvitationsBasket}
import uk.gov.hmrc.agentmtdidentifiers.model.Service

case class ReviewAuthorisationsPageConfig(
  clientType: ClientType,
  basket: Basket,
  featureFlags: FeatureFlags,
  services: Set[Service],
  submitCall: Call)(implicit messages: Messages) {

  def clientNameOf(authorisationRequest: AuthorisationRequest, noNameMessage: String): String =
    authorisationRequest.invitation.service match {
      case Service.PersonalIncomeRecord => noNameMessage
      case _                            => authorisationRequest.clientName.stripSuffix(".")
    }

  val numberOfItems: Int = basket.size

  val clientNamesAreDifferent: Boolean = basket.toSeq.map(_.clientName).distinct.length != 1

  val basketFull: Boolean = clientType match {
    case ClientType.Personal => new PersonalInvitationsBasket(services, basket, featureFlags).availableServices.isEmpty
    case ClientType.Business => new BusinessInvitationsBasket(services, basket, featureFlags).availableServices.isEmpty
    case ClientType.Trust    => new TrustInvitationsBasket(services, basket, featureFlags).availableServices.isEmpty
  }

  def showDeleteCall(itemId: String): Call =
    routes.AgentInvitationJourneyController.showDeleteAuthorisation(itemId)
}
