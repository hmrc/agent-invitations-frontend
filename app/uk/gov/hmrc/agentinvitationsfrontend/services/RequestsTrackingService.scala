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

import javax.inject.{Inject, Singleton}
import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{AgentServicesAccountConnector, CitizenDetailsConnector, InvitationsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.models.{Services, StoredInvitation, TrackedInvitation}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global

@Singleton
class RequestsTrackingService @Inject()(
  invitationsConnector: InvitationsConnector,
  agentServicesAccountConnector: AgentServicesAccountConnector,
  citizenDetailsConnector: CitizenDetailsConnector) {

  def getRecentAgentInvitations(arn: Arn, isPirWhitelisted: Boolean, showLastDays: Int)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Seq[TrackedInvitation]] =
    invitationsConnector
      .getAllInvitations(arn, LocalDate.now().minusDays(showLastDays))
      .map(_.filter(whitelistedInvitation(isPirWhitelisted)).map(TrackedInvitation.apply))
      .flatMap(ii => Future.sequence(ii.map(addClientName)))

  def whitelistedInvitation(isPirWhitelisted: Boolean): StoredInvitation => Boolean =
    i => isPirWhitelisted || i.service != Services.HMRCPIR

  def addClientName(
    invitation: TrackedInvitation)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[TrackedInvitation] =
    clientNameFor(invitation).map(cn => invitation.copy(clientName = cn))

  def clientNameFor(
    invitation: TrackedInvitation)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    invitation.service match {
      case Services.HMRCMTDIT  => getItsaTradingName(Nino(invitation.clientId))
      case Services.HMRCPIR    => getCitizenName(Nino(invitation.clientId))
      case Services.HMRCMTDVAT => getVatName(Vrn(invitation.clientId))
      case _                   => Future successful None
    }

  def getItsaTradingName(nino: Nino)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    agentServicesAccountConnector.getTradingName(nino)

  def getCitizenName(nino: Nino)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    citizenDetailsConnector.getCitizenDetails(nino).map(_.name)

  def getVatName(vrn: Vrn)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    agentServicesAccountConnector.getCustomerDetails(vrn).map { customerDetails =>
      customerDetails.tradingName
        .orElse(customerDetails.organisationName)
        .orElse(customerDetails.individual.map(_.name))
    }

}
