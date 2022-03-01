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

  val supportedServices: List[Service] =
    List(Service.MtdIt, Service.PersonalIncomeRecord, Service.Vat, Service.Trust, Service.TrustNT, Service.CapitalGains, Service.Ppt)
  val supportedClientIdentifierTypes = List("ni", "vrn", "utr", "CGTPDRef", "urn", "EtmpRegistrationNumber")

  // These are the options that the user will be shown on the 'select service' page.
  // TODO: Can they be merged with the 'all supported enrolment keys' above?
  val supportedPersonalServices: Set[Service] = Set(Service.MtdIt, Service.PersonalIncomeRecord, Service.Vat, Service.CapitalGains, Service.Ppt)
  val supportedBusinessServices: Set[Service] = Set(Service.Vat, Service.Ppt)
  val supportedTrustServices: Set[Service] = Set(Service.CapitalGains, Service.Trust, Service.Ppt)
  def supportedServicesFor(clientType: ClientType): Set[Service] = clientType match {
    case ClientType.Personal => supportedPersonalServices
    case ClientType.Business => supportedBusinessServices
    case ClientType.Trust    => supportedTrustServices
  }

  def supportedEnrolmentKeys: Set[String] = supportedServices.map(_.enrolmentKey).toSet
  def supportedEnrolmentKeysFor(clientType: ClientType): Set[String] = supportedServicesFor(clientType).map(_.enrolmentKey)

  def isSupported(clientType: ClientType, service: Service): Boolean = (clientType, service) match {
    case (ClientType.Trust, Service.TrustNT) => true // must check this separately as it is not separately listed in the supported services
    case _                                   => supportedServicesFor(clientType).contains(service)
  }

  // This is the order in which the services are to be displayed on the 'select service' page.
  val serviceDisplayOrdering: Ordering[Service] = new Ordering[Service] {
    val correctOrdering = List(Service.MtdIt, Service.PersonalIncomeRecord, Service.Vat, Service.Trust, Service.CapitalGains, Service.Ppt)
    override def compare(x: Service, y: Service): Int = correctOrdering.indexOf(x) - correctOrdering.indexOf(y)
  }

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
