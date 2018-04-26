/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.agentinvitationsfrontend.controllers

import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId

trait Service
case class ValidService(serviceName: String, enrolmentName: String, enrolmentIdentifier: String, apiIdentifier: String, messageKey: String) extends Service
case object InvalidService extends Service

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
  val VAT = "VAT"
  val messageKeyForVAT = "vat"

  val supportedServices = List(HMRCMTDIT, HMRCMTDVAT, HMRCPIR)

  def isSupportedService(service: String): Boolean = supportedServices.contains(service)

  def determineService(invitationId: InvitationId): Service = {
    invitationId.value.head match {
      case 'A' => ValidService(HMRCMTDIT, HMRCMTDIT, MTDITID, MTDITID, messageKeyForITSA)
      case 'B' => ValidService(HMRCPIR, HMRCNI, NINO, NI, messageKeyForAfi)
      case 'C' => ValidService(HMRCMTDVAT, HMRCMTDVAT, VRN, VAT, messageKeyForVAT)
      case _ => InvalidService
    }
  }
}