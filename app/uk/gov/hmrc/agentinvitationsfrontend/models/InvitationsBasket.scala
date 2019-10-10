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
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCCGTPD, HMRCMTDIT, HMRCMTDVAT, HMRCPIR, TRUST}

/** The set of services available that an agent can choose for authorisation
  *
  */
abstract class InvitationsBasket(services: Set[String], basket: Basket, featureFlags: FeatureFlags) {

  /** Returns a set of pairs of string which can be used to populate a selection form
    *  depends on what the set of services are, and whether any are in your basket already
    * */
  def availableServices(implicit messages: Messages): Seq[(String, String)]

  protected def showServiceTrust: Boolean =
    featureFlags.showHmrcTrust && serviceAvailableForSelection(TRUST)

  protected def showServiceCgt: Boolean =
    featureFlags.showHmrcCgt && serviceAvailableForSelection(HMRCCGTPD)

  protected def showServiceMtdVat: Boolean =
    featureFlags.showHmrcMtdVat && serviceAvailableForSelection(HMRCMTDVAT)

  protected def showServicePersonalIncome: Boolean =
    featureFlags.showPersonalIncome && serviceAvailableForSelection(HMRCPIR)

  protected def showServiceHmrcMtdIt: Boolean =
    featureFlags.showHmrcMtdIt && serviceAvailableForSelection(HMRCMTDIT)

  protected def serviceAvailableForSelection(service: String): Boolean =
    services.contains(service) && !basket.exists(_.invitation.service == service)
}

class TrustInvitationsBasket(services: Set[String], basket: Basket, featureFlags: FeatureFlags)
    extends InvitationsBasket(services, basket, featureFlags) {

  def availableServices(implicit messages: Messages): Seq[(String, String)] = {

    val seq = collection.mutable.ArrayBuffer[(String, String)]()

    if (showServiceTrust)
      seq.append(TRUST -> Messages("select-service.trust"))

    if (showServiceCgt)
      seq.append(HMRCCGTPD -> Messages("select-service.cgt"))

    seq
  }

}

class PersonalInvitationsBasket(services: Set[String], basket: Basket, featureFlags: FeatureFlags)
    extends InvitationsBasket(services, basket, featureFlags) {

  def availableServices(implicit messages: Messages): Seq[(String, String)] = {

    val seq = collection.mutable.ArrayBuffer[(String, String)]()

    if (showServiceHmrcMtdIt)
      seq.append(HMRCMTDIT -> Messages("select-service.itsa"))

    if (showServicePersonalIncome)
      seq.append(HMRCPIR -> Messages("select-service.personal-income-viewer"))

    if (showServiceMtdVat)
      seq.append(HMRCMTDVAT -> Messages("select-service.vat"))

    if (showServiceCgt)
      seq.append(HMRCCGTPD -> Messages("select-service.cgt"))

    seq
  }
}
