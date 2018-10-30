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

package uk.gov.hmrc.agentinvitationsfrontend.services

import uk.gov.hmrc.agentinvitationsfrontend.connectors.{AgentServicesAccountConnector, Citizen, CitizenDetailsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.models.{ServiceAndClient, Services}
import uk.gov.hmrc.agentmtdidentifiers.model.Vrn
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait GetClientName {

  def agentServicesAccountConnector: AgentServicesAccountConnector
  def citizenDetailsConnector: CitizenDetailsConnector

  def getClientNameByService(
    invitation: ServiceAndClient)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    getClientNameByService(invitation.clientId, invitation.service)

  def getClientNameByService(clientId: String, service: String)(
    implicit c: HeaderCarrier,
    ec: ExecutionContext): Future[Option[String]] =
    service match {
      case Services.HMRCMTDIT if Nino.isValid(clientId) => getItsaTradingName(Nino(clientId))
      case Services.HMRCPIR if Nino.isValid(clientId)   => getCitizenName(Nino(clientId))
      case Services.HMRCMTDVAT if Vrn.isValid(clientId) => getVatName(Vrn(clientId))
      case _                                            => Future successful None
    }

  def getItsaTradingName(nino: Nino)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    agentServicesAccountConnector
      .getTradingName(nino)
      .flatMap {
        case name if name.isDefined => Future successful name
        case None                   => getCitizenName(nino)
      }

  def getCitizenName(nino: Nino)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    getCitizenRecord(nino).map(_.name)

  def getCitizenRecord(nino: Nino)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Citizen] =
    citizenDetailsConnector.getCitizenDetails(nino)

  def getVatName(vrn: Vrn)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    agentServicesAccountConnector.getCustomerDetails(vrn).map { customerDetails =>
      customerDetails.tradingName
        .orElse(customerDetails.organisationName)
        .orElse(customerDetails.individual.map(_.name))
    }

}
