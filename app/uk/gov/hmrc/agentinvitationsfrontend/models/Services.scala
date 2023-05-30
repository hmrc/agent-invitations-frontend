/*
 * Copyright 2023 HM Revenue & Customs
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

import uk.gov.hmrc.agentmtdidentifiers.model._

object Services {

  val supportedServices: List[Service] =
    List(
      Service.MtdIt,
      Service.PersonalIncomeRecord,
      Service.Vat,
      Service.Trust,
      Service.TrustNT,
      Service.CapitalGains,
      Service.Ppt,
      Service.Cbc,
      Service.CbcNonUk)

  // These are the options that the user will be shown on the 'select service' page.
  // TODO: Can they be merged with the 'all supported enrolment keys' above?
  val supportedPersonalServices: Set[Service] = Set(Service.MtdIt, Service.PersonalIncomeRecord, Service.Vat, Service.CapitalGains, Service.Ppt)
  val supportedBusinessServices: Set[Service] = Set(Service.Vat, Service.Ppt, Service.Cbc /* implies also CbcNonUk */ )
  val supportedTrustServices: Set[Service] =
    Set(Service.CapitalGains, Service.Trust /* implies also TrustNT */, Service.Ppt, Service.Cbc /* implies also CbcNonUk */ )
  def supportedServicesFor(clientType: ClientType): Set[Service] = clientType match {
    case ClientType.Personal => supportedPersonalServices
    case ClientType.Business => supportedBusinessServices
    case ClientType.Trust    => supportedTrustServices
  }

  def supportedEnrolmentKeys: Set[String] = supportedServices.map(_.enrolmentKey).toSet
  def supportedEnrolmentKeysFor(clientType: ClientType): Set[String] = {
    val enrolmentKeys = supportedServicesFor(clientType).map(_.enrolmentKey)
    clientType match {
      case ClientType.Trust    => enrolmentKeys + Service.TrustNT.enrolmentKey // add the non-taxable trust enrolment key if the client is a trust
      case ClientType.Business => enrolmentKeys + Service.PersonalIncomeRecord.enrolmentKey // NINO is sometimes found on Org type creds
      case _                   => enrolmentKeys
    }
  }

  def isSupported(clientType: ClientType, service: Service): Boolean = (clientType, service) match {
    case (clientType, Service.TrustNT) =>
      isSupported(clientType, Service.Trust) // must check this separately as it is not separately listed in the supported services
    case (clientType, Service.CbcNonUk) =>
      isSupported(clientType, Service.Cbc) // must check this separately as it is not separately listed in the supported services
    case _ => supportedServicesFor(clientType).contains(service)
  }

  def supportedClientTypesFor(service: Service): Seq[ClientType] = ClientType.clientTypes.filter(ct => isSupported(ct, service))

  // This is the order in which the services are to be displayed on the 'select service' page.
  val serviceDisplayOrdering: Ordering[Service] = new Ordering[Service] {
    val correctOrdering =
      List(Service.MtdIt, Service.PersonalIncomeRecord, Service.Vat, Service.Trust, Service.CapitalGains, Service.Ppt, Service.Cbc)
    override def compare(x: Service, y: Service): Int = correctOrdering.indexOf(x) - correctOrdering.indexOf(y)
  }

  val serviceMessageKeys: Map[Service, String] = Map(
    Service.MtdIt                -> "itsa",
    Service.PersonalIncomeRecord -> "afi",
    Service.Vat                  -> "vat",
    Service.Trust                -> "trust",
    Service.TrustNT              -> "trustNT",
    Service.CapitalGains         -> "cgt",
    Service.Ppt                  -> "ppt",
    Service.Cbc                  -> "cbc",
    Service.CbcNonUk             -> "cbcNonUk"
  )

  def determineService(invitationId: InvitationId): Service = {
    val prefix = invitationId.value.head
    Service.supportedServices
      .find(_.invitationIdPrefix == prefix)
      .getOrElse(throw new IllegalArgumentException(s"No service corresponding to prefix '$prefix'"))
  }
}
