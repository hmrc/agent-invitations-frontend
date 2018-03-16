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

import uk.gov.hmrc.agentinvitationsfrontend.connectors.{AgentServicesAccountConnector, AgentStubsConnector, InvitationsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentInvitation, Invitation}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class InvitationsService @Inject() (invitationsConnector: InvitationsConnector,
                                    agentServicesAccountConnector: AgentServicesAccountConnector,
                                    agentStubsConnector: AgentStubsConnector) {

  def createInvitation(arn: Arn, service: String, clientIdentifierType: Option[String], clientIdentifier: Option[String], postcode: Option[String])
                      (implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Invitation] = {
    val agentInvitation = AgentInvitation(service,
      clientIdentifierType.getOrElse(throw new IllegalStateException("clientIdentifierType is Missing")),
      clientIdentifier.getOrElse(throw new Exception("clientIdentifier is missing")),
      postcode)

    for {
      locationOpt <- invitationsConnector.createInvitation(arn, agentInvitation)
      invitation <- invitationsConnector
        .getInvitation(locationOpt.getOrElse { throw new Exception("Invitation location expected; but missing.") })
    } yield invitation
  }

  def acceptITSAInvitation(invitationId: InvitationId, mtdItId: MtdItId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Int] =
    invitationsConnector.acceptITSAInvitation(mtdItId, invitationId)

  def rejectITSAInvitation(invitationId: InvitationId, mtdItId: MtdItId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Int] =
    invitationsConnector.rejectITSAInvitation(mtdItId, invitationId)

  def acceptAFIInvitation(invitationId: InvitationId, nino: Nino)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Int] =
    invitationsConnector.acceptAFIInvitation(nino, invitationId)

  def rejectAFIInvitation(invitationId: InvitationId, nino: Nino)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Int] =
    invitationsConnector.rejectAFIInvitation(nino, invitationId)

  def acceptVATInvitation(invitationId: InvitationId, vrn: Vrn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Int] =
    invitationsConnector.acceptVATInvitation(vrn, invitationId)

  def rejectVATInvitation(invitationId: InvitationId, vrn: Vrn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Int] =
    invitationsConnector.rejectVATInvitation(vrn, invitationId)

  def getClientInvitation(clientId: String, invitationId: InvitationId, apiIdentifier: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Invitation] = {
    invitationsConnector.getInvitation(clientInvitationUrl(invitationId, clientId, apiIdentifier))
  }

  def getAgencyName(arn: Arn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[String] =
    agentServicesAccountConnector.getAgencyName(arn.value).map {
      case Some(name) => name
      case None => throw new Exception("Agency name not found")
  }

  def checkVatRegistrationDateMatches(vrn: String, userInputRegistrationDate: String): Future[(Boolean,Boolean)] = {
    implicit val hc: HeaderCarrier = HeaderCarrier()
    agentStubsConnector.getVatRegisteredClient(vrn).map{
        case Some(r) => (true,if(r.registrationDate.equals(userInputRegistrationDate)) true else false)
        case None => (false, false)
    }
  }

  private def clientInvitationUrl(invitationId: InvitationId, clientId: String, apiIdentifier: String): String = {
      s"/agent-client-authorisation/clients/$apiIdentifier/$clientId/invitations/received/${invitationId.value}"
  }

}
