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

package uk.gov.hmrc.agentinvitationsfrontend.views.track

import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.agentinvitationsfrontend.controllers.routes
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentmtdidentifiers.model.Service

import java.time.LocalDate

case class ResendLinkPageConfig(
  externalUrl: String,
  agentLink: String,
  clientType: String,
  expiryDate: String,
  maybeService: Option[Service],
  agencyEmail: String,
  backLinkUrl: String,
  isAltItsa: Boolean = false)(implicit externalUrls: ExternalUrls, messages: Messages) {

  val expiryDateAsLocalDate = LocalDate.parse(expiryDate)

  def fullAgentLink: String = s"$externalUrl$agentLink"

  def trackLink: Call =
    routes.AgentsRequestTrackingController.showTrackRequests()

  def newRequestLink: Call =
    routes.AgentInvitationJourneyController.showClientType

  val asaUrl = externalUrls.agentServicesAccountUrl

  val step1Instructions: Option[String] = if (clientType == "personal") {
    maybeService.flatMap(
      service =>
        if (service == Service.PersonalIncomeRecord) Some(Messages("invitation-sent.step1.personal.paye"))
        else if (service == Service.Vat) Some(Messages("invitation-sent.step1.personal.vat"))
        else None)
  } else if (clientType == "business") {
    maybeService.flatMap(
      service =>
        if (service == Service.Vat) Some(Messages("invitation-sent.step1.business.vat"))
        else if (service == Service.Trust || service == Service.TrustNT) Some(Messages("invitation-sent.step1.business.trust"))
        else None)
  } else None
}
