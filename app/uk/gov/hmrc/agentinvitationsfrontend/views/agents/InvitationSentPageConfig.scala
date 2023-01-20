/*
 * Copyright 2023 HM Revenue & Customs
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
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.controllers.routes
import uk.gov.hmrc.agentmtdidentifiers.model.Service

import java.time.LocalDate

case class InvitationSentPageConfig(
  relativeInvitationUrl: String,
  continueUrlOpt: Option[String],
  hasContinueUrl: Boolean,
  clientType: String,
  expiryDate: LocalDate,
  agencyEmail: String,
  services: Set[Service],
  isAltItsa: Boolean)(implicit externalUrls: ExternalUrls, messages: Messages) {

  val continueUrl: String = continueUrlOpt match {
    case Some(url) => url
    case None      => externalUrls.agentServicesAccountUrl
  }

  val trackUrl: Call = routes.AgentsRequestTrackingController.showTrackRequests()

  val clientTypeUrl: Call = routes.AgentInvitationJourneyController.showClientType

  val step1Instructions: Option[String] = if (clientType == "personal") {
    if (services(Service.PersonalIncomeRecord) && services(Service.Vat)) Some(Messages("invitation-sent.step1.personal.paye-vat"))
    else if (services(Service.PersonalIncomeRecord)) Some(Messages("invitation-sent.step1.personal.paye"))
    else if (services(Service.Vat)) Some(Messages("invitation-sent.step1.personal.vat"))
    else None
  } else if (clientType == "business") {
    if (services(Service.Vat)) Some(Messages("invitation-sent.step1.business.vat"))
    else if (services(Service.Trust)) Some(Messages("invitation-sent.step1.business.trust"))
    else None
  } else None

  val showWarning: Boolean = !isAltItsa || isAltItsa && services.size > 1
}
