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

package uk.gov.hmrc.agentinvitationsfrontend.models

import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId

sealed trait Service

object Services {

  // TODO make a sealed trait
  val HMRCMTDIT = "HMRC-MTD-IT"
  val HMRCPIR = "PERSONAL-INCOME-RECORD"
  val HMRCMTDVAT = "HMRC-MTD-VAT"
  val TRUST = "HMRC-TERS-ORG"
  val HMRCCGTPD = "HMRC-CGT-PD"

  val supportedServices = List(HMRCMTDIT, HMRCPIR, HMRCMTDVAT, TRUST, HMRCCGTPD)
  val supportedClientIdentifierTypes = List("ni", "vrn", "utr", "CGTPDRef")

  def determineServiceMessageKey(invitationId: InvitationId): String =
    invitationId.value.head match {
      case 'A' => "itsa"
      case 'B' => "afi"
      case 'C' => "vat"
      case 'D' => "trust"
      case 'E' => "cgt"
      case _   => "Service is missing"
    }

  def determineServiceMessageKeyFromService(service: String): String =
    service match {
      case HMRCMTDIT  => "itsa"
      case HMRCPIR    => "afi"
      case HMRCMTDVAT => "vat"
      case TRUST      => "trust"
      case HMRCCGTPD  => "cgt"
    }

  def determineServiceFromServiceMessageKey(serviceMessageKey: String): String =
    serviceMessageKey match {
      case "itsa"  => HMRCMTDIT
      case "afi"   => HMRCPIR
      case "vat"   => HMRCMTDVAT
      case "trust" => TRUST
      case "cgt"   => HMRCCGTPD
    }
}
