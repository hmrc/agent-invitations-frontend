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

package uk.gov.hmrc.agentinvitationsfrontend.models

import play.api.i18n.Messages
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.Basket
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}

/** The set of services available that an agent can choose for authorisation
  */
class InvitationsBasket(services: Set[String], basket: Basket, featureFlags: FeatureFlags) {

  def availablePersonalServices(implicit messages: Messages): Seq[(String, String)] = {

    val seq = collection.mutable.ArrayBuffer[(String, String)]()

    if (showServiceHmrcMtdIt)
      seq.append(HMRCMTDIT -> Messages("personal-select-service.itsa"))

    if (showServicePersonalIncome)
      seq.append(HMRCPIR -> Messages("personal-select-service.personal-income-viewer"))

    if (showServiceMtdVat)
      seq.append(HMRCMTDVAT -> Messages("select-service.vat"))

    seq
  }

  private def showServiceMtdVat: Boolean =
    featureFlags.showHmrcMtdVat && serviceAvailableForSelection(HMRCMTDVAT)

  private def showServicePersonalIncome: Boolean =
    featureFlags.showPersonalIncome && serviceAvailableForSelection(HMRCPIR)

  private def showServiceHmrcMtdIt: Boolean =
    featureFlags.showHmrcMtdIt && serviceAvailableForSelection(HMRCMTDIT)

  private def serviceAvailableForSelection(service: String): Boolean =
    services.contains(service) && !basket.exists(_.invitation.service == service)
}
