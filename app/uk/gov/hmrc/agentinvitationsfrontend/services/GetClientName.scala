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

package uk.gov.hmrc.agentinvitationsfrontend.services

import play.api.Logger
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{AgentServicesAccountConnector, Citizen, CitizenDetailsConnector, InvitationsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.models.{ServiceAndClient, Services}
import uk.gov.hmrc.agentmtdidentifiers.model.{Utr, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait GetClientName {

  def agentServicesAccountConnector: AgentServicesAccountConnector
  def citizenDetailsConnector: CitizenDetailsConnector
  def invitationsConnector: InvitationsConnector

  def getClientNameByService(
    invitation: ServiceAndClient)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    getClientNameByService(invitation.clientId, invitation.service)

  def getClientNameByService(clientId: String, service: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[String]] =
    service match {
      case Services.HMRCMTDIT if Nino.isValid(clientId) => getItsaTradingName(Nino(clientId))
      case Services.HMRCPIR if Nino.isValid(clientId)   => getCitizenName(Nino(clientId))
      case Services.HMRCMTDVAT if Vrn.isValid(clientId) => getVatName(Vrn(clientId))
      case Services.TRUST if Utr.isValid(clientId)      => getTrustName(Utr(clientId))
      case _                                            => Future successful None
    }

  def getItsaTradingName(nino: Nino)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    agentServicesAccountConnector
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
    agentServicesAccountConnector.getCustomerDetails(vrn).map { customerDetails =>
      customerDetails.tradingName
        .orElse(customerDetails.organisationName)
        .orElse(customerDetails.individual.map(_.name))
    }

  def getTrustName(utr: Utr)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    invitationsConnector.getTrustName(utr).map(_.response).map {
      case Right(trustName) => Some(trustName.name)
      case Left(invalidTrust) =>
        Logger.warn(s"error during retrieving trust name for utr: ${utr.value} , error: $invalidTrust")
        None
    }

}
