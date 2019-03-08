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

package uk.gov.hmrc.agentinvitationsfrontend.views.agents

import play.api.mvc.Call
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{FeatureFlags, routes}
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentFastTrackRequest, AgentSession, Services}

case class CheckDetailsPageConfig(fastTrackRequest: AgentFastTrackRequest, featureFlags: FeatureFlags) {

  private val shouldShowKF: Boolean = {
    fastTrackRequest.service match {
      case "HMRC-MTD-IT" if featureFlags.showKfcMtdIt                     => true
      case "PERSONAL-INCOME-RECORD" if featureFlags.showKfcPersonalIncome => true
      case "HMRC-MTD-VAT" if featureFlags.showKfcMtdVat                   => true
      case _                                                              => false
    }
  }

  val serviceMessageKey: String = {
    fastTrackRequest.service match {
      case "HMRC-MTD-IT"            => "itsa"
      case "PERSONAL-INCOME-RECORD" => "afi"
      case "HMRC-MTD-VAT"           => "vat"
    }
  }

  val needClientType: Boolean = fastTrackRequest.clientType.isEmpty

  val needKnownFact: Boolean = shouldShowKF && fastTrackRequest.knownFact.getOrElse("").isEmpty

  val clientTypeUrl: Call = routes.AgentsInvitationController.showClientType()

  val knownFactUrl: Call = routes.AgentsFastTrackInvitationController.showKnownFact()

  def changeDetailsUrl(service: String): Call =
    service match {
      case Services.HMRCMTDIT  => routes.AgentsInvitationController.submitIdentifyClientItsa()
      case Services.HMRCPIR    => routes.AgentsInvitationController.submitIdentifyClientIrv()
      case Services.HMRCMTDVAT => routes.AgentsInvitationController.submitIdentifyClientVat()
      case _                   => throw new Exception("service not supported")
    }

  val showKnownFact: Boolean = fastTrackRequest.knownFact.getOrElse("").nonEmpty && shouldShowKF

}
