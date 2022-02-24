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

package uk.gov.hmrc.agentinvitationsfrontend.views.agents.cancelAuthorisation

import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentmtdidentifiers.model.Service

case class SelectServicePageConfigCancel(featureFlags: FeatureFlags, services: Set[Service], submitCall: Call, backLink: String)(
  implicit messages: Messages) {

  private val serviceDisplayOrdering: Ordering[Service] = new Ordering[Service] {
    val correctOrdering = List(Service.MtdIt, Service.PersonalIncomeRecord, Service.Vat, Service.Trust, Service.CapitalGains, Service.Ppt)
    override def compare(x: Service, y: Service): Int = correctOrdering.indexOf(x) - correctOrdering.indexOf(y)
  }

  val enabledPersonalServices: Seq[(Service, String)] = {
    val map = collection.mutable.Map[Service, String]()

    if (featureFlags.showPersonalIncome && services.contains(Service.PersonalIncomeRecord))
      map.update(Service.PersonalIncomeRecord, Messages("select-service.PERSONAL-INCOME-RECORD.personal"))

    if (featureFlags.showHmrcMtdIt && services.contains(Service.MtdIt))
      map.update(Service.MtdIt, Messages("cancel-authorisation.select-service.itsa"))

    if (featureFlags.showHmrcMtdVat && services.contains(Service.Vat))
      map.update(Service.Vat, Messages("cancel-authorisation.select-service.vat"))

    if (featureFlags.showHmrcCgt && services.contains(Service.CapitalGains))
      map.update(Service.CapitalGains, Messages("cancel-authorisation.select-service.cgt"))

    if (featureFlags.showPlasticPackagingTax && services.contains(Service.Ppt))
      map.update(Service.Ppt, Messages("cancel-authorisation.select-service.ppt"))

    map.toSeq.sortBy(_._1)(serviceDisplayOrdering)
  }

  val enabledBusinessServices: Seq[(Service, String)] = {
    val map = collection.mutable.Map[Service, String]()

    if (featureFlags.showHmrcMtdVat && services.contains(Service.Vat))
      map.update(Service.Vat, Messages("cancel-authorisation.select-service.vat"))

    if (featureFlags.showPlasticPackagingTax && services.contains(Service.Ppt))
      map.update(Service.Ppt, Messages("cancel-authorisation.select-service.ppt"))

    map.toSeq.sortBy(_._1)(serviceDisplayOrdering)
  }

  val enabledTrustServices: Seq[(Service, String)] = {
    val map = collection.mutable.Map[Service, String]()

    if (featureFlags.showHmrcTrust && services.contains(Service.Trust))
      map.update(Service.Trust, Messages("cancel-authorisation.select-service.trust"))

    if (featureFlags.showHmrcCgt && services.contains(Service.CapitalGains))
      map.update(Service.CapitalGains, Messages("cancel-authorisation.select-service.cgt"))

    if (featureFlags.showPlasticPackagingTax && services.contains(Service.Ppt))
      map.update(Service.Ppt, Messages("cancel-authorisation.select-service.ppt"))

    map.toSeq.sortBy(_._1)(serviceDisplayOrdering)
  }

}
