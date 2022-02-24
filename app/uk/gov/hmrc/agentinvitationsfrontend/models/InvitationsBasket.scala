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
abstract class InvitationsBasket(services: Set[Service], basket: Basket, featureFlags: FeatureFlags) {

  /** Returns a set of pairs of string which can be used to populate a selection form
    *  depends on what the set of services are, and whether any are in your basket already
    * */
  def availableServices(implicit messages: Messages): Seq[(Service, String)]

  protected def showServiceTrust: Boolean =
    featureFlags.showHmrcTrust && serviceAvailableForSelection(Service.Trust)

  protected def showServiceCgt: Boolean =
    featureFlags.showHmrcCgt && serviceAvailableForSelection(Service.CapitalGains)

  protected def showServiceMtdVat: Boolean =
    featureFlags.showHmrcMtdVat && serviceAvailableForSelection(Service.Vat)

  protected def showServicePersonalIncome: Boolean =
    featureFlags.showPersonalIncome && serviceAvailableForSelection(Service.PersonalIncomeRecord)

  protected def showServiceHmrcMtdIt: Boolean =
    featureFlags.showHmrcMtdIt && serviceAvailableForSelection(Service.MtdIt)

  protected def showServicePlasticPackagingTax: Boolean =
    featureFlags.showPlasticPackagingTax && serviceAvailableForSelection(Service.Ppt)

  protected def serviceAvailableForSelection(service: Service): Boolean =
    if (service == Service.Trust) {
      services.contains(service) && (!basket.exists(_.invitation.service == Service.Trust) && !basket.exists(_.invitation.service == Service.TrustNT))
    } else
      services.contains(service) && !basket.exists(_.invitation.service == service)
}

class TrustInvitationsBasket(services: Set[Service], basket: Basket, featureFlags: FeatureFlags)
    extends InvitationsBasket(services, basket, featureFlags) {

  /** Available service selections for trust clients - based on what is configured and already in basket
    * */
  def availableServices(implicit messages: Messages): Seq[(Service, String)] = {

    val seq = collection.mutable.ArrayBuffer[(Service, String)]()

    if (showServiceTrust) {
      seq.append(Service.Trust -> Messages("select-service.HMRC-TERS-ORG.business"))
    }

    if (showServiceCgt)
      seq.append(Service.CapitalGains -> Messages("select-service.HMRC-CGT-PD.business"))

    if (showServicePlasticPackagingTax)
      seq.append(Service.Ppt -> Messages("select-service.HMRC-PPT-ORG.trust"))

    seq
  }

}

class PersonalInvitationsBasket(services: Set[Service], basket: Basket, featureFlags: FeatureFlags)
    extends InvitationsBasket(services, basket, featureFlags) {

  /** Available service selections for personal clients - based on what is configured and already in basket
    * */
  def availableServices(implicit messages: Messages): Seq[(Service, String)] = {

    val seq = collection.mutable.ArrayBuffer[(Service, String)]()

    if (showServiceHmrcMtdIt)
      seq.append(Service.MtdIt -> Messages("select-service.HMRC-MTD-IT.personal"))

    if (showServicePersonalIncome)
      seq.append(Service.PersonalIncomeRecord -> Messages("select-service.PERSONAL-INCOME-RECORD.personal"))

    if (showServiceMtdVat)
      seq.append(Service.Vat -> Messages("select-service.HMRC-MTD-VAT.personal"))

    if (showServiceCgt)
      seq.append(Service.CapitalGains -> Messages("select-service.HMRC-CGT-PD.personal"))

    if (showServicePlasticPackagingTax)
      seq.append(Service.Ppt -> Messages("select-service.HMRC-PPT-ORG.personal"))

    seq
  }
}

class BusinessInvitationsBasket(services: Set[Service], basket: Basket, featureFlags: FeatureFlags)
    extends InvitationsBasket(services, basket, featureFlags) {

  /** Available service selections for personal clients - based on what is configured and already in basket
    * */
  def availableServices(implicit messages: Messages): Seq[(Service, String)] = {

    val seq = collection.mutable.ArrayBuffer[(Service, String)]()

    if (showServiceMtdVat)
      seq.append(Service.Vat -> Messages("select-service.HMRC-MTD-VAT.business"))

    if (showServicePlasticPackagingTax)
      seq.append(Service.Ppt -> Messages("select-service.HMRC-PPT-ORG.business"))

    seq
  }
}
