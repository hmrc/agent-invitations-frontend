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

import javax.inject.{Inject, Singleton}
import org.joda.time.LocalDate
import play.api.Logging
import play.api.mvc.Request
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.connectors._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.{HeaderCarrier, NotFoundException}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@Singleton
class InvitationsService @Inject()(
  val acaConnector: AgentClientAuthorisationConnector,
  val citizenDetailsConnector: CitizenDetailsConnector,
  auditService: AuditService)
    extends GetClientName with Logging {

  def createInvitation(
    arn: Arn,
    invitation: Invitation)(implicit hc: HeaderCarrier, ec: ExecutionContext, request: Request[_]): Future[InvitationId] = {

    val agentInvitation =
      AgentInvitation(invitation.clientType, invitation.service, invitation.clientIdentifierType, invitation.clientId)

    (for {
      locationOpt <- acaConnector.createInvitation(arn, agentInvitation)
      invitation <- acaConnector
                     .getInvitation(locationOpt.getOrElse {
                       throw new Exception("location expected after invitation creation ; but missing.")
                     })
    } yield invitation)
      .map(storedInvitation => {
        val id = storedInvitation.selfUrl.toString.split("/").toStream.last
        acaConnector
          .getAgentReferenceRecord(storedInvitation.arn)
          .map(agentRefRecord => auditService.sendAgentInvitationSubmitted(arn, id, invitation, agentRefRecord.uid, "Success"))
        InvitationId(id)
      })
      .recoverWith {
        case NonFatal(e) =>
          logger.warn(s"Invitation Creation Failed: ${e.getMessage}")
          auditService.sendAgentInvitationSubmitted(arn, "", invitation, "", "Fail", Option(e.getMessage))
          Future.failed(e)
      }
  }

  def createMultipleInvitations(
    arn: Arn,
    requests: Set[AuthorisationRequest])(implicit hc: HeaderCarrier, ec: ExecutionContext, request: Request[_]): Future[Set[AuthorisationRequest]] =
    Future.sequence(requests.map(authRequest => {
      val agentInvitation =
        AgentInvitation(
          authRequest.invitation.clientType,
          authRequest.invitation.service,
          authRequest.invitation.clientIdentifierType,
          authRequest.invitation.clientId)

      (for {
        locationOpt <- acaConnector.createInvitation(arn, agentInvitation)
        invitation <- acaConnector
                       .getInvitation(locationOpt.getOrElse {
                         throw new Exception("Invitation location expected; but missing.")
                       })
      } yield invitation)
        .map(invitation => {
          val id = invitation.selfUrl.toString.split("/").toStream.last
          acaConnector
            .getAgentReferenceRecord(invitation.arn)
            .map(agentRefRecord =>
              auditService
                .sendAgentInvitationSubmitted(arn, id, authRequest.invitation, agentRefRecord.uid, "Success"))
          authRequest.copy(state = AuthorisationRequest.CREATED)
        })
        .recover {
          case NonFatal(e) =>
            logger.warn(s"Invitation Creation Failed: ${e.getMessage}")
            auditService
              .sendAgentInvitationSubmitted(arn, "", authRequest.invitation, "", "Fail", Option(e.getMessage))
            authRequest.copy(state = AuthorisationRequest.FAILED)
        }
    }))

  def createAgentLink(arn: Arn, clientType: Option[ClientType])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[String] =
    clientType
      .map(ct =>
        acaConnector.createAgentLink(arn, ClientType.fromEnum(ct)).map {
          case Some(multiInv) => multiInv
          case None           => throw new Exception("Creating multi-invitation link failed")
      })
      .getOrElse(throw new Exception("Creating multi-invitation link failed because of missing clientType"))

  def acceptITSAInvitation(invitationId: InvitationId, mtdItId: MtdItId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    acaConnector.acceptITSAInvitation(mtdItId, invitationId)

  def rejectITSAInvitation(invitationId: InvitationId, mtdItId: MtdItId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    acaConnector.rejectITSAInvitation(mtdItId, invitationId)

  def acceptAFIInvitation(invitationId: InvitationId, nino: Nino)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    acaConnector.acceptAFIInvitation(nino, invitationId)

  def rejectAFIInvitation(invitationId: InvitationId, nino: Nino)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    acaConnector.rejectAFIInvitation(nino, invitationId)

  def acceptVATInvitation(invitationId: InvitationId, vrn: Vrn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    acaConnector.acceptVATInvitation(vrn, invitationId)

  def rejectVATInvitation(invitationId: InvitationId, vrn: Vrn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    acaConnector.rejectVATInvitation(vrn, invitationId)

  def getAgencyName(arn: Arn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[String] =
    acaConnector.getAgencyName(arn.value).map {
      case Some(name) => name
      case None       => throw new Exception("Agency name not found")
    }

  def checkPostcodeMatches(nino: Nino, postcode: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Boolean]] =
    acaConnector.checkPostcodeForClient(nino, postcode)

  def checkVatRegistrationDateMatches(vrn: Vrn, userInputRegistrationDate: LocalDate)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Int]] =
    acaConnector.checkVatRegisteredClient(vrn, userInputRegistrationDate)

  def checkCitizenRecordMatches(nino: Nino, dob: LocalDate)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Boolean]] =
    acaConnector.checkCitizenRecord(nino, dob)

  private def determineInvitationResponse(invitationId: InvitationId, si: StoredInvitation, agentName: String, response: String)(
    implicit request: Request[_],
    hc: HeaderCarrier,
    ec: ExecutionContext) =
    for {
      result <- Services.determineServiceMessageKey(invitationId) match {
                 case "itsa" =>
                   if (response == "Accepted")
                     acaConnector.acceptITSAInvitation(MtdItId(si.clientId), invitationId)
                   else acaConnector.rejectITSAInvitation(MtdItId(si.clientId), invitationId)

                 case "afi" =>
                   if (response == "Accepted") acaConnector.acceptAFIInvitation(Nino(si.clientId), invitationId)
                   else acaConnector.rejectAFIInvitation(Nino(si.clientId), invitationId)

                 case "vat" =>
                   if (response == "Accepted") acaConnector.acceptVATInvitation(Vrn(si.clientId), invitationId)
                   else acaConnector.rejectVATInvitation(Vrn(si.clientId), invitationId)

                 case "trust" =>
                   if (response == "Accepted")
                     acaConnector.acceptTrustInvitation(Utr(si.clientId), invitationId)
                   else acaConnector.rejectTrustInvitation(Utr(si.clientId), invitationId)

                 case "cgt" =>
                   if (response == "Accepted")
                     acaConnector.acceptCgtInvitation(CgtRef(si.clientId), invitationId)
                   else acaConnector.rejectCgtInvitation(CgtRef(si.clientId), invitationId)
               }

      _ <- auditService.sendAgentInvitationResponse(invitationId.value, si.arn, response, si.clientIdType, si.clientId, si.service, agentName)
    } yield result

  def acceptInvitation(invitationId: InvitationId)(
    agentName: String)(implicit request: Request[_], hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    for {
      invitation <- acaConnector.getInvitation(invitationId)
      result: Boolean <- invitation match {
                          case None =>
                            Future.failed(new NotFoundException(s"Invitation ${invitationId.value} not found"))
                          case Some(i) => determineInvitationResponse(invitationId, i, agentName, "Accepted")
                        }
    } yield result

  def rejectInvitation(invitationId: InvitationId)(
    agentName: String)(implicit request: Request[_], hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    for {
      invitation <- acaConnector.getInvitation(invitationId)
      result: Boolean <- invitation match {
                          case None =>
                            Future.failed(new NotFoundException(s"Invitation ${invitationId.value} not found"))
                          case Some(i) => determineInvitationResponse(invitationId, i, agentName, "Rejected")
                        }
    } yield result

  def hasPendingInvitationsFor(arn: Arn, clientId: String, service: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    for {
      hasPendingInvitations <- acaConnector
                                .getAllPendingInvitationsForClient(arn, clientId, service)
                                .map(s => s.nonEmpty)
    } yield hasPendingInvitations
}
