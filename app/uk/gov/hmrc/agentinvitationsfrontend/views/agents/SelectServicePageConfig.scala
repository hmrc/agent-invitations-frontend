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

package uk.gov.hmrc.agentinvitationsfrontend.views.agents

import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.Basket

trait SelectServicePageConfig {

  def basket: Basket
  def featureFlags: FeatureFlags
  def services: Set[String]
  def submitCall: Call
  def backLink: String
  def reviewAuthsCall: Call

  /** Whether to show the multiple service selection form for this client type journey */
  def showMultiSelect: Boolean = availableServices.size > 1

  def remainingService: String = firstServiceKey

  /** The list of available services for the user to select,
    * based on what is available according to feature enablement
    * and what they have already selected in their basket
    * */
  def availableServices: Seq[(String, String)]

  /** The header to use when multiple trust service selections available */
  def selectHeaderMessage(implicit messages: Messages): String = Messages("select-service.header")

  /** The header to use when only a single service is available or left to choose */
  def selectSingleHeaderMessage: String

  protected def firstServiceKey: String =
    availableServices.headOption.map(_._1).getOrElse("notfound")

  def isCgtOnly: Boolean =
    availableServices.exists(_._1 == "HMRC-CGT-PD") && availableServices.size == 1

}
