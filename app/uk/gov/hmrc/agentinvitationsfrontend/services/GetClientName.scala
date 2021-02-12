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

package uk.gov.hmrc.agentinvitationsfrontend.services

import play.api.Logging
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{AgentClientAuthorisationConnector, Citizen, CitizenDetailsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.models.{ServiceAndClient, Services}
import uk.gov.hmrc.agentmtdidentifiers.model.{CgtRef, Urn, Utr, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait GetClientName extends Logging {

  def citizenDetailsConnector: CitizenDetailsConnector
  def acaConnector: AgentClientAuthorisationConnector

  def getClientNameByService(invitation: ServiceAndClient)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    getClientNameByService(invitation.clientId, invitation.service)

  def getClientNameByService(clientId: String, service: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    service match {
      case Services.HMRCMTDIT =>
        if (clientId.isEmpty) {
          logger.warn(s"no client Id as Nino available so not calling getTradingName")
          Future successful None
        } else getItsaTradingName(Nino(clientId))
      case Services.HMRCPIR    => getCitizenName(Nino(clientId))
      case Services.HMRCMTDVAT => getVatName(Vrn(clientId))
      case Services.TRUST      => getTrustName(clientId)
      case Services.TRUSTNT    => getTrustName(clientId)
      case Services.HMRCCGTPD  => getCgtClientName(CgtRef(clientId))
      case _                   => Future successful None
    }

  def getItsaTradingName(nino: Nino)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    acaConnector
      .getTradingName(nino)
      .flatMap {
        case name if name.isDefined => Future successful name
        case None                   => getCitizenName(nino)
      }

  def getCitizenName(nino: Nino)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    getCitizenRecord(nino).map(_.name)

  def getCitizenRecord(nino: Nino)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Citizen] =
    citizenDetailsConnector.getCitizenDetails(nino)

  def getVatName(vrn: Vrn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    acaConnector.getCustomerDetails(vrn).map { customerDetails =>
      customerDetails.tradingName
        .orElse(customerDetails.organisationName)
        .orElse(customerDetails.individual.map(_.name))
    }

  def getTrustName(trustTaxIdentifier: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    acaConnector.getTrustName(trustTaxIdentifier).map(_.response).map {
      case Right(trustName) => Some(trustName.name)
      case Left(invalidTrust) =>
        logger.warn(s"error during retrieving trust name for utr/urn: $trustTaxIdentifier , error: $invalidTrust")
        None
    }

  def getCgtClientName(cgtRef: CgtRef)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    acaConnector.getCgtSubscription(cgtRef).map {
      case Some(cgtSubscription) => Some(cgtSubscription.name)
      case None =>
        logger.warn(s"no cgtSubscription found to retrieve name for reference: ${cgtRef.value}")
        None
    }

}
