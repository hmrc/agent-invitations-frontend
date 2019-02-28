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

import javax.inject.{Inject, Singleton}

import org.joda.time.LocalDate
import play.api.Logger
import play.api.mvc.Request
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.connectors._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{FeatureFlags, routes}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.{HeaderCarrier, NotFoundException}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@Singleton
class InvitationsService @Inject()(
  invitationsConnector: InvitationsConnector,
  featureFlags: FeatureFlags,
  val agentServicesAccountConnector: AgentServicesAccountConnector,
  val citizenDetailsConnector: CitizenDetailsConnector,
  auditService: AuditService)
    extends GetClientName {

  def createInvitation(arn: Arn, invitation: Invitation, featureFlags: FeatureFlags)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext,
    request: Request[_]): Future[InvitationId] = {

    val agentInvitation =
      AgentInvitation(invitation.clientType, invitation.service, invitation.clientIdentifierType, invitation.clientId)

    (for {
      locationOpt <- invitationsConnector.createInvitation(arn, agentInvitation)
      invitation <- invitationsConnector
                     .getInvitation(locationOpt.getOrElse {
                       throw new Exception("Invitation location expected; but missing.")
                     })
    } yield invitation)
      .map(storedInvitation => {
        val id = storedInvitation.selfUrl.toString.split("/").toStream.last
        if ((storedInvitation.service == Services.HMRCMTDIT && featureFlags.showKfcMtdIt)
              | (storedInvitation.service == Services.HMRCPIR && featureFlags.showKfcPersonalIncome)
              | (storedInvitation.service == Services.HMRCMTDVAT && featureFlags.showKfcMtdVat)) {
          invitationsConnector
            .getAgentReferenceRecord(storedInvitation.arn)
            .map(agentRefRecord =>
              auditService.sendAgentInvitationSubmitted(arn, id, invitation, agentRefRecord.uid, "Success"))
        } else auditService.sendAgentInvitationSubmitted(arn, id, invitation, "", "Not Required")
        InvitationId(id)
      })
      .recoverWith {
        case NonFatal(e) =>
          Logger(getClass).warn(s"Invitation Creation Failed: ${e.getMessage}")
          auditService.sendAgentInvitationSubmitted(arn, "", invitation, "", "Fail", Option(e.getMessage))
          Future.failed(e)
      }
  }

  def createMultipleInvitations(arn: Arn, clientType: Option[ClientType], requests: Set[AuthorisationRequest])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext,
    request: Request[_]): Future[Set[AuthorisationRequest]] =
    Future.sequence(requests.map(authRequest => {
      val agentInvitation =
        AgentInvitation(
          authRequest.invitation.clientType,
          authRequest.invitation.service,
          authRequest.invitation.clientIdentifierType,
          authRequest.invitation.clientId)

      (for {
        locationOpt <- invitationsConnector.createInvitation(arn, agentInvitation)
        invitation <- invitationsConnector
                       .getInvitation(locationOpt.getOrElse {
                         throw new Exception("Invitation location expected; but missing.")
                       })
      } yield invitation)
        .map(invitation => {
          val id = invitation.selfUrl.toString.split("/").toStream.last
          if ((invitation.service == Services.HMRCMTDIT && featureFlags.showKfcMtdIt)
                | (invitation.service == Services.HMRCPIR && featureFlags.showKfcPersonalIncome)
                | (invitation.service == Services.HMRCMTDVAT && featureFlags.showKfcMtdVat)) {
            invitationsConnector
              .getAgentReferenceRecord(invitation.arn)
              .map(agentRefRecord =>
                auditService
                  .sendAgentInvitationSubmitted(arn, id, authRequest.invitation, agentRefRecord.uid, "Success"))
          } else auditService.sendAgentInvitationSubmitted(arn, id, authRequest.invitation, "", "Not Required")
          authRequest.copy(state = AuthorisationRequest.CREATED)
        })
        .recover {
          case NonFatal(e) =>
            Logger(getClass).warn(s"Invitation Creation Failed: ${e.getMessage}")
            auditService
              .sendAgentInvitationSubmitted(arn, "", authRequest.invitation, "", "Fail", Option(e.getMessage))
            authRequest.copy(state = AuthorisationRequest.FAILED)
        }
    }))

  def createAgentLink(arn: Arn, clientType: Option[ClientType])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[String] =
    clientType
      .map(ct =>
        invitationsConnector.createAgentLink(arn, ClientType.fromEnum(ct)).map {
          case Some(multiInv) => multiInv
          case None           => throw new Exception("Creating multi-invitation link failed")
      })
      .getOrElse(throw new Exception("Creating multi-invitation link failed because of missing clientType"))

  def acceptITSAInvitation(invitationId: InvitationId, mtdItId: MtdItId)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Boolean] =
    invitationsConnector.acceptITSAInvitation(mtdItId, invitationId)

  def rejectITSAInvitation(invitationId: InvitationId, mtdItId: MtdItId)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Boolean] =
    invitationsConnector.rejectITSAInvitation(mtdItId, invitationId)

  def acceptAFIInvitation(invitationId: InvitationId, nino: Nino)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Boolean] =
    invitationsConnector.acceptAFIInvitation(nino, invitationId)

  def rejectAFIInvitation(invitationId: InvitationId, nino: Nino)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Boolean] =
    invitationsConnector.rejectAFIInvitation(nino, invitationId)

  def acceptVATInvitation(invitationId: InvitationId, vrn: Vrn)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Boolean] =
    invitationsConnector.acceptVATInvitation(vrn, invitationId)

  def rejectVATInvitation(invitationId: InvitationId, vrn: Vrn)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Boolean] =
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

  def checkCitizenRecordMatches(nino: Nino, dob: LocalDate)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] =
    invitationsConnector.checkCitizenRecord(nino, dob)

  def acceptInvitation(invitationId: InvitationId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    for {
      invitation <- invitationsConnector.getInvitation(invitationId)
      result: Boolean <- invitation match {
                          case None =>
                            Future.failed(new NotFoundException(s"Invitation ${invitationId.value} not found"))
                          case Some(i) =>
                            Services.determineServiceMessageKey(invitationId) match {
                              case "itsa" =>
                                invitationsConnector.acceptITSAInvitation(MtdItId(i.clientId), invitationId)
                              case "afi" => invitationsConnector.acceptAFIInvitation(Nino(i.clientId), invitationId)
                              case "vat" => invitationsConnector.acceptVATInvitation(Vrn(i.clientId), invitationId)
                            }
                        }
    } yield result

  def rejectInvitation(invitationId: InvitationId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    for {
      invitation <- invitationsConnector.getInvitation(invitationId)
      result: Boolean <- invitation match {
                          case None =>
                            Future.failed(new NotFoundException(s"Invitation ${invitationId.value} not found"))
                          case Some(i) =>
                            Services.determineServiceMessageKey(invitationId) match {
                              case "itsa" =>
                                invitationsConnector.rejectITSAInvitation(MtdItId(i.clientId), invitationId)
                              case "afi" => invitationsConnector.rejectAFIInvitation(Nino(i.clientId), invitationId)
                              case "vat" => invitationsConnector.rejectVATInvitation(Vrn(i.clientId), invitationId)
                            }
                        }
    } yield result

  def hasPendingInvitationsFor(arn: Arn, clientId: String, service: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Boolean] =
    for {
      hasPendingInvitations <- invitationsConnector
                                .getAllPendingInvitationsForClient(arn, clientId, service)
                                .map(s => s.nonEmpty)
    } yield hasPendingInvitations

  private def clientInvitationUrl(invitationId: InvitationId, clientId: String, apiIdentifier: String): String =
    s"/agent-client-authorisation/clients/$apiIdentifier/$clientId/invitations/received/${invitationId.value}"

}
