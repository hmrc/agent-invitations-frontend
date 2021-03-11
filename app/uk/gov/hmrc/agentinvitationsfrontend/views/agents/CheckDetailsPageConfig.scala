/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentFastTrackRequest, Services}

case class CheckDetailsPageConfig(
  fastTrackRequest: AgentFastTrackRequest,
  featureFlags: FeatureFlags,
  clientTypeUrl: Call,
  knownFactUrl: Call,
  changeDetailsCall: Call,
  submitFormCall: Call,
  backLinkUrl: Option[String]) {

  val serviceMessageKey: String = {
    fastTrackRequest.service match {
      case "HMRC-MTD-IT"            => "itsa"
      case "PERSONAL-INCOME-RECORD" => "afi"
      case "HMRC-MTD-VAT"           => "vat"
      case "HMRC-TERS-ORG"          => "trust"
      case "HMRC-TERSNT-ORG"        => "trust"
      case "HMRC-CGT-PD"            => "cgt"
    }
  }

  val needClientType: Boolean = fastTrackRequest.clientType.isEmpty

  val needKnownFact
    : Boolean = fastTrackRequest.service != Services.TAXABLETRUST && fastTrackRequest.service != Services.NONTAXABLETRUST && fastTrackRequest.knownFact
    .getOrElse("")
    .isEmpty

  val showKnownFact: Boolean = fastTrackRequest.knownFact.getOrElse("").nonEmpty

}
