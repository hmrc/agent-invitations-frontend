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

import uk.gov.hmrc.agentmtdidentifiers.model.{InvitationId, Service}

object Services {

  val supportedServices =
    List(Service.MtdIt, Service.PersonalIncomeRecord, Service.Vat, Service.Trust, Service.TrustNT, Service.CapitalGains, Service.Ppt)
  val supportedClientIdentifierTypes = List("ni", "vrn", "utr", "CGTPDRef", "urn", "EtmpRegistrationNumber")
  val supportedEnrolmentKeys: Set[Service] = Set(Service.MtdIt, Service.Vat, Service.Trust, Service.TrustNT, Service.CapitalGains)
  val allSupportedEnrolmentKeysForIndividual: Set[Service] = Set(Service.MtdIt, Service.Vat, Service.CapitalGains, Service.Ppt)
  val allSupportedEnrolmentKeysForBusiness: Set[Service] = Set(Service.Vat, Service.Ppt)
  val allSupportedEnrolmentKeysForTrustOrEstate: Set[Service] = Set(Service.CapitalGains, Service.Trust, Service.TrustNT, Service.Ppt)

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

  def determineServiceMessageKeyFromService(service: Service): String =
    service match {
      case Service.MtdIt                => "itsa"
      case Service.PersonalIncomeRecord => "afi"
      case Service.Vat                  => "vat"
      case Service.Trust                => "trust"
      case Service.TrustNT              => "trustNT"
      case Service.CapitalGains         => "cgt"
      case Service.Ppt                  => "ppt"
    }

  def determineServiceFromServiceMessageKey(serviceMessageKey: String): Service =
    serviceMessageKey match {
      case "itsa"    => Service.MtdIt
      case "afi"     => Service.PersonalIncomeRecord
      case "vat"     => Service.Vat
      case "trust"   => Service.Trust
      case "trustNT" => Service.TrustNT
      case "cgt"     => Service.CapitalGains
      case "ppt"     => Service.Ppt
    }

  def clientIdType(service: Service) =
    service match {
      case Service.MtdIt                => "ni"
      case Service.Vat                  => "vrn"
      case Service.CapitalGains         => "CGTPDRef"
      case Service.Trust                => "utr"
      case Service.TrustNT              => "urn"
      case Service.PersonalIncomeRecord => "ni"
      case Service.Ppt                  => "EtmpRegistrationNumber"
    }
}
