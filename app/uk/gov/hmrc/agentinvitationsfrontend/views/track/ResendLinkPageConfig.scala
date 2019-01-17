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

package uk.gov.hmrc.agentinvitationsfrontend.views.track

import org.joda.time.{DateTimeZone, Days, LocalDate}
import play.api.mvc.Call
import uk.gov.hmrc.agentinvitationsfrontend.controllers.routes

case class ResendLinkPageConfig(externalUrl: String, agentLink: String, clientType: String, expiryDate: String) {

  val expiryDateAsLocalDate = LocalDate.parse(expiryDate)

  val daysToExpiry: String = {
    val expirationDate: LocalDate = LocalDate.parse(expiryDate)
    val todayDate: LocalDate = LocalDate.now(DateTimeZone.UTC)

    Days.daysBetween(todayDate, expirationDate).getDays.toString
  }

  def fullAgentLink: String = s"$externalUrl$agentLink"

  def trackLink: Call =
    routes.AgentsRequestTrackingController.showTrackRequests()

  def newRequestLink: Call =
    routes.AgentsInvitationController.showClientType()

  def continueLink: Call = routes.AgentsInvitationController.continueAfterInvitationSent()
}
