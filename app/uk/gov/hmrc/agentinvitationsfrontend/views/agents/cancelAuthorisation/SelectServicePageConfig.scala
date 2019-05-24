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

package uk.gov.hmrc.agentinvitationsfrontend.views.agents.cancelAuthorisation

import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}

case class SelectServicePageConfig(
  featureFlags: FeatureFlags,
  services: Set[String],
  submitCall: Call,
  backLink: String)(implicit messages: Messages) {

  val enabledPersonalServices: Seq[(String, String)] = {
    val map = collection.mutable.Map[String, String]()

    if (featureFlags.showPersonalIncome && services.contains(HMRCPIR))
      map.update(HMRCPIR, Messages("personal-select-service.personal-income-viewer"))

    if (featureFlags.showHmrcMtdIt && services.contains(HMRCMTDIT))
      map.update(HMRCMTDIT, Messages("cancel-authorisation.select-service.itsa"))

    if (featureFlags.showHmrcMtdVat && services.contains(HMRCMTDVAT))
      map.update(HMRCMTDVAT, Messages("cancel-authorisation.select-service.vat"))

    map.toSeq match {
      case Seq(vat, irv, itsa) => Seq(itsa, irv, vat)
      case Seq(vat, itsa)      => Seq(itsa, vat)
    }
  }

}
