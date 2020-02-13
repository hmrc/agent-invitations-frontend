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

package uk.gov.hmrc.agentinvitationsfrontend.views.agents.cancelAuthorisation

import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCCGTPD, HMRCMTDIT, HMRCMTDVAT, HMRCPIR, TRUST}

case class SelectServicePageConfigCancel(
  featureFlags: FeatureFlags,
  services: Set[String],
  submitCall: Call,
  backLink: String)(implicit messages: Messages) {

  val enabledPersonalServices: Seq[(String, String)] = {
    val map = collection.mutable.Map[String, String]()

    if (featureFlags.showPersonalIncome && services.contains(HMRCPIR))
      map.update(HMRCPIR, Messages("select-service.PERSONAL-INCOME-RECORD.personal"))

    if (featureFlags.showHmrcMtdIt && services.contains(HMRCMTDIT))
      map.update(HMRCMTDIT, Messages("cancel-authorisation.select-service.itsa"))

    if (featureFlags.showHmrcMtdVat && services.contains(HMRCMTDVAT))
      map.update(HMRCMTDVAT, Messages("cancel-authorisation.select-service.vat"))

    if (featureFlags.showHmrcCgt && services.contains(HMRCCGTPD))
      map.update(HMRCCGTPD, Messages("cancel-authorisation.select-service.cgt"))

    map.toSeq match {
      case Seq(cgt, vat, irv, itsa) => Seq(itsa, irv, vat, cgt)
      case Seq(cgt, vat, itsa)      => Seq(itsa, vat, cgt)
      case Seq(vat, itsa)           => Seq(itsa, vat)
      case Seq(cgt)                 => Seq(cgt)
      case Seq()                    => Seq()
    }
  }

  val enabledTrustServices: Seq[(String, String)] = {
    val map = collection.mutable.Map[String, String]()

    if (featureFlags.showHmrcTrust && services.contains(TRUST))
      map.update(TRUST, Messages("cancel-authorisation.select-service.trust"))

    if (featureFlags.showHmrcCgt && services.contains(HMRCCGTPD))
      map.update(HMRCCGTPD, Messages("cancel-authorisation.select-service.cgt"))

    map.toSeq match {
      case Seq(cgt, trust) => Seq(trust, cgt)
      case Seq(trust)      => Seq(trust)
      case Seq()           => Seq()
    }
  }

}
