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
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentInvitation, StoredInvitation}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class InvitationsService @Inject()(
  invitationsConnector: InvitationsConnector,
  val agentServicesAccountConnector: AgentServicesAccountConnector,
  val citizenDetailsConnector: CitizenDetailsConnector)
    extends GetClientName {

  def createInvitation(
    arn: Arn,
    service: String,
    clientIdentifierType: String,
    clientIdentifier: TaxIdentifier,
    postcode: Option[String])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[StoredInvitation] = {
    val agentInvitation = AgentInvitation(service, clientIdentifierType, clientIdentifier.value, postcode)

    for {
      locationOpt <- invitationsConnector.createInvitation(arn, agentInvitation)
      invitation <- invitationsConnector
                     .getInvitation(locationOpt.getOrElse {
                       throw new Exception("Invitation location expected; but missing.")
                     })
    } yield invitation
  }

  def acceptITSAInvitation(invitationId: InvitationId, mtdItId: MtdItId)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Int] =
    invitationsConnector.acceptITSAInvitation(mtdItId, invitationId)

  def rejectITSAInvitation(invitationId: InvitationId, mtdItId: MtdItId)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Int] =
    invitationsConnector.rejectITSAInvitation(mtdItId, invitationId)

  def acceptAFIInvitation(invitationId: InvitationId, nino: Nino)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Int] =
    invitationsConnector.acceptAFIInvitation(nino, invitationId)

  def rejectAFIInvitation(invitationId: InvitationId, nino: Nino)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Int] =
    invitationsConnector.rejectAFIInvitation(nino, invitationId)

  def acceptVATInvitation(invitationId: InvitationId, vrn: Vrn)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Int] =
    invitationsConnector.acceptVATInvitation(vrn, invitationId)

  def rejectVATInvitation(invitationId: InvitationId, vrn: Vrn)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Int] =
    invitationsConnector.rejectVATInvitation(vrn, invitationId)

  def getClientInvitation(clientId: String, invitationId: InvitationId, apiIdentifier: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[StoredInvitation] =
    invitationsConnector.getInvitation(clientInvitationUrl(invitationId, clientId, apiIdentifier))

  def getAgencyName(arn: Arn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[String] =
    agentServicesAccountConnector.getAgencyName(arn.value).map {
      case Some(name) => name
      case None       => throw new Exception("Agency name not found")
    }

  def checkPostcodeMatches(nino: Nino, postcode: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] =
    invitationsConnector.checkPostcodeForClient(nino, postcode)

  def checkVatRegistrationDateMatches(vrn: Vrn, userInputRegistrationDate: LocalDate)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] =
    invitationsConnector.checkVatRegisteredClient(vrn, userInputRegistrationDate)

  private def clientInvitationUrl(invitationId: InvitationId, clientId: String, apiIdentifier: String): String =
    s"/agent-client-authorisation/clients/$apiIdentifier/$clientId/invitations/received/${invitationId.value}"

}
