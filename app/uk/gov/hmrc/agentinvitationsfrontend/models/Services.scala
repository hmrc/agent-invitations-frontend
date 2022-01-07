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

import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId

sealed trait Service

object Services {

  // TODO make a sealed trait
  val HMRCMTDIT = "HMRC-MTD-IT"
  val HMRCPIR = "PERSONAL-INCOME-RECORD"
  val HMRCMTDVAT = "HMRC-MTD-VAT"
  val TAXABLETRUST = "HMRC-TERS-ORG"
  val NONTAXABLETRUST = "HMRC-TERSNT-ORG"
  val HMRCCGTPD = "HMRC-CGT-PD"
  val HMRCNI = "HMRC-NI"
  val TRUST = "TRUST"
  val HMRCPPTORG = "HMRC-PPT-ORG"

  val supportedServicesWithAnyTrust = List(HMRCMTDIT, HMRCPIR, HMRCMTDVAT, TRUST, HMRCCGTPD, HMRCPPTORG)
  val supportedServices = List(HMRCMTDIT, HMRCPIR, HMRCMTDVAT, TAXABLETRUST, NONTAXABLETRUST, HMRCCGTPD, HMRCPPTORG)
  val supportedClientIdentifierTypes = List("ni", "vrn", "utr", "CGTPDRef", "urn", "EtmpRegistrationNumber")
  val supportedEnrolmentKeys = Set(HMRCMTDIT, HMRCNI, HMRCMTDVAT, TAXABLETRUST, NONTAXABLETRUST, HMRCCGTPD)
  val allSupportedEnrolmentKeysForIndividual = Set(HMRCMTDIT, HMRCMTDVAT, HMRCCGTPD, HMRCNI, HMRCPPTORG)
  val allSupportedEnrolmentKeysForBusiness = Set(HMRCMTDVAT, HMRCPPTORG)
  val allSupportedEnrolmentKeysForTrustOrEstate = Set(HMRCCGTPD, TAXABLETRUST, NONTAXABLETRUST, HMRCPPTORG)

  def determineServiceMessageKey(invitationId: InvitationId): String =
    invitationId.value.head match {
      case 'A' => "itsa"
      case 'B' => "afi"
      case 'C' => "vat"
      case 'D' => "trust"
      case 'E' => "cgt"
      case 'F' => "trustNT"
      case 'G' => "ppt"
      case x   => "Service is missing: " + x
    }

  def determineServiceMessageKeyFromService(service: String): String =
    service match {
      case HMRCMTDIT       => "itsa"
      case HMRCPIR         => "afi"
      case HMRCMTDVAT      => "vat"
      case TAXABLETRUST    => "trust"
      case NONTAXABLETRUST => "trustNT"
      case HMRCCGTPD       => "cgt"
      case HMRCPPTORG      => "ppt"
    }

  def determineServiceFromServiceMessageKey(serviceMessageKey: String): String =
    serviceMessageKey match {
      case "itsa"    => HMRCMTDIT
      case "afi"     => HMRCPIR
      case "vat"     => HMRCMTDVAT
      case "trust"   => TAXABLETRUST
      case "trustNT" => NONTAXABLETRUST
      case "cgt"     => HMRCCGTPD
      case "ppt"     => HMRCPPTORG
    }

  def clientIdType(service: String) =
    service match {
      case HMRCMTDIT       => "ni"
      case HMRCMTDVAT      => "vrn"
      case HMRCCGTPD       => "CGTPDRef"
      case TAXABLETRUST    => "utr"
      case NONTAXABLETRUST => "urn"
      case HMRCPIR         => "ni"
      case HMRCPPTORG      => "EtmpRegistrationNumber"
    }
}
