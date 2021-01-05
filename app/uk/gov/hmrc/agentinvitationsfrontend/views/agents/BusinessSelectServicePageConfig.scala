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

package uk.gov.hmrc.agentinvitationsfrontend.views.agents

import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.Basket
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._

case class BusinessSelectServicePageConfig(
  basket: Basket = Set.empty,
  featureFlags: FeatureFlags,
  services: Set[String] = Set(HMRCMTDVAT),
  submitCall: Call,
  backLink: String,
  reviewAuthsCall: Call)(implicit messages: Messages)
    extends SelectServicePageConfig {

  // We only need the service key here
  override def availableServices: Seq[(String, String)] = Seq(HMRCMTDVAT -> "unused")

  def selectSingleHeaderMessage: String =
    Messages(s"select-single-service.$firstServiceKey.business.header")

}
