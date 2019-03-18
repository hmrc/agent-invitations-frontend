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

package uk.gov.hmrc.agentinvitationsfrontend.views.agents

import play.api.mvc.Call
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{FeatureFlags, routes}
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentSession, AuthorisationRequest}

case class ReviewAuthorisationsPageConfig(
  requests: Set[AuthorisationRequest],
  featureFlags: FeatureFlags,
  submitCall: Call) {

  def clientNameOf(authorisationRequest: AuthorisationRequest, noNameMessage: String) =
    authorisationRequest.invitation.service match {
      case "HMRC-MTD-IT" if !featureFlags.enableMtdItToConfirm          => noNameMessage
      case "PERSONAL-INCOME-RECORD" if !featureFlags.enableIrvToConfirm => noNameMessage
      case "HMRC-MTD-VAT" if !featureFlags.enableMtdVatToConfirm        => noNameMessage
      case _                                                            => authorisationRequest.clientName
    }

  val numberOfItems: Int = requests.size

  val clientNamesAreDifferent: Boolean = requests.toSeq.map(_.clientName).distinct.length != 1

  def showDeleteCall(itemId: String) = routes.AgentsInvitationController.showDelete(itemId)

}
