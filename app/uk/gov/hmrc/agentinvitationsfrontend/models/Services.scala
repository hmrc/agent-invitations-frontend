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

import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId

sealed trait Service

case object InvalidService extends Service

case class ValidService(
  serviceName: String,
  enrolmentName: String,
  enrolmentIdentifier: String,
  apiIdentifier: String,
  messageKey: String)
    extends Service

// TODO why are these all strings?  Make this set of services a sealed trait so it can be type safe!
object Services {

  val HMRCMTDIT = "HMRC-MTD-IT"
  val MTDITID = "MTDITID"
  val messageKeyForITSA = "itsa"

  val HMRCNI = "HMRC-NI"
  val NINO = "NINO"
  val NI = "NI"
  val messageKeyForAfi = "afi"
  val HMRCPIR = "PERSONAL-INCOME-RECORD"

  val HMRCMTDVAT = "HMRC-MTD-VAT"
  val VRN = "VRN"
  val messageKeyForVAT = "vat"

  val TRUST = "HMRC-TERS-ORG"
  val messageKeyForTrust = "trust"

  val supportedServices = List(HMRCMTDIT, HMRCPIR, HMRCMTDVAT, TRUST)
  val supportedTypes = List("ni", "vrn", "utr")

  val supportedClientTypes = List("personal", "business", "trust")

  def determineServiceMessageKey(invitationId: InvitationId): String =
    invitationId.value.head match {
      case 'A' => messageKeyForITSA
      case 'B' => messageKeyForAfi
      case 'C' => messageKeyForVAT
      case 'D' => messageKeyForTrust
      case _   => "Service is missing"
    }

  def determineServiceMessageKeyFromService(service: String): String =
    service match {
      case HMRCMTDIT  => messageKeyForITSA
      case HMRCPIR    => messageKeyForAfi
      case HMRCMTDVAT => messageKeyForVAT
      case TRUST      => messageKeyForTrust
    }
}
