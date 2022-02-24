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

package uk.gov.hmrc.agentinvitationsfrontend.services

import play.api.Logging
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{AgentClientAuthorisationConnector, Citizen, CitizenDetailsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.models.{ServiceAndClient, Services}
import uk.gov.hmrc.agentmtdidentifiers.model.{CgtRef, PptRef, Service, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait GetClientName extends Logging {

  def citizenDetailsConnector: CitizenDetailsConnector
  def acaConnector: AgentClientAuthorisationConnector

  def getClientNameByService(invitation: ServiceAndClient)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    getClientNameByService(invitation.clientId, invitation.service)

  def getClientNameByService(clientId: String, service: Service)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    service match {
      case Service.MtdIt =>
        if (clientId.isEmpty) {
          logger.warn(s"no client Id as Nino available so not calling getTradingName")
          Future successful None
        } else getItsaTradingName(Nino(clientId))
      case Service.PersonalIncomeRecord => getCitizenName(Nino(clientId))
      case Service.Vat                  => getVatName(Vrn(clientId))
      case Service.Trust                => getTrustName(clientId)
      case Service.TrustNT              => getTrustName(clientId)
      case Service.CapitalGains         => getCgtClientName(CgtRef(clientId))
      case Service.Ppt                  => getPptClientName(PptRef(clientId))
      case _                            => Future successful None
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

  def getPptClientName(pptRef: PptRef)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    acaConnector.getPptSubscription(pptRef).map {
      case Some(pptSubscription) => Some(pptSubscription.customerName)
      case None =>
        logger.warn(s"no pptSubscription found to retrieve name for reference: ${pptRef.value}")
        None
    }
}
