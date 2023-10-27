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

package uk.gov.hmrc.agentinvitationsfrontend.services

import javax.inject.{Inject, Singleton}
import play.api.Logging
import play.api.mvc.{Request, RequestHeader}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.connectors._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.{InvitationSent, State}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.util.toFuture
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.{HeaderCarrier, NotFoundException}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendHeaderCarrierProvider

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@Singleton
class InvitationsService @Inject()(
  val acaConnector: AgentClientAuthorisationConnector,
  val citizenDetailsConnector: CitizenDetailsConnector,
  val relationshipsConnector: RelationshipsConnector,
  auditService: AuditService)
    extends GetClientName with FrontendHeaderCarrierProvider with Logging {

  def createInvitation(
    arn: Arn,
    invitation: Invitation)(implicit hc: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[InvitationId] = {

    val agentInvitation =
      AgentInvitation(invitation.clientType, invitation.service.id, invitation.service.supportedSuppliedClientIdType.id, invitation.clientId)

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
          .map(agentRefRecord =>
            auditService.sendAgentInvitationSubmitted(arn, id, invitation, agentRefRecord.uid, "Success", None, storedInvitation.altItsa))
        InvitationId(id)
      })
      .recoverWith {
        case NonFatal(e) =>
          logger.warn(s"Invitation Creation Failed: ${e.getMessage}")
          auditService.sendAgentInvitationSubmitted(arn, "", invitation, "", "Fail", Option(e.getMessage), None)
          Future.failed(e)
      }
  }

  def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest])(
    implicit rh: RequestHeader,
    ec: ExecutionContext): Future[Set[AuthorisationRequest]] =
    Future.sequence(requests.map(authRequest => {
      val agentInvitation =
        AgentInvitation(
          authRequest.invitation.clientType,
          authRequest.invitation.service.id,
          authRequest.invitation.service.supportedSuppliedClientIdType.id,
          authRequest.invitation.clientId
        )

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
                .sendAgentInvitationSubmitted(arn, id, authRequest.invitation, agentRefRecord.uid, "Success", None, invitation.altItsa))
          authRequest.copy(state = AuthorisationRequest.CREATED)
        })
        .recover {
          case NonFatal(e) =>
            logger.warn(s"Invitation Creation Failed: ${e.getMessage}")
            auditService
              .sendAgentInvitationSubmitted(arn, "", authRequest.invitation, "", "Fail", Option(e.getMessage), None)
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

  def getAgencyName(arn: Arn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[String] =
    acaConnector.getAgencyName(arn.value).map {
      case Some(name) => name
      case None       => throw new Exception("Agency name not found")
    }

  def isAltItsa(i: StoredInvitation): Boolean =
    i.service == Service.MtdIt && i.clientId == i.suppliedClientId

  private def respondToInvitation(invitationId: InvitationId, si: StoredInvitation, agentName: String, isAccepted: Boolean)(
    implicit request: Request[_],
    hc: HeaderCarrier,
    ec: ExecutionContext) =
    for {
      service <- Future.successful(Services.determineService(invitationId))
      result <- service match {
                 case Service.MtdIt if isAltItsa(si) => // special case handling for alt-ITSA :(
                   acaConnector.respondToInvitation(Service.MtdIt, Nino(si.clientId), invitationId, accepted = isAccepted)
                 case Service.MtdIt =>
                   acaConnector.respondToInvitation(Service.MtdIt, MtdItId(si.clientId), invitationId, accepted = isAccepted)

                 case service =>
                   acaConnector
                     .respondToInvitation(service, service.supportedClientIdType.createUnderlying(si.clientId), invitationId, accepted = isAccepted)
               }

      _ <- auditService.sendAgentInvitationResponse(invitationId.value, si.arn, isAccepted, si.clientIdType, si.clientId, si.service, agentName)
    } yield result

  def respondToInvitation(invitationId: InvitationId, agentName: String, isAccepted: Boolean)(
    implicit request: Request[_],
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[Boolean] =
    for {
      invitation <- acaConnector.getInvitation(invitationId)
      result: Boolean <- invitation match {
                          case None =>
                            Future.failed(new NotFoundException(s"Invitation ${invitationId.value} not found"))
                          case Some(i) => respondToInvitation(invitationId, i, agentName, isAccepted = isAccepted)
                        }
    } yield result

  def hasPendingInvitationsFor(arn: Arn, clientId: String, service: Service)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    acaConnector.getAllPendingInvitationsForClient(arn, clientId, service.id).map(s => s.nonEmpty)

  def isAltItsa(arn: Arn, clientId: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    acaConnector.getAllPendingInvitationsForClient(arn, clientId, Service.MtdIt.id).map { pendingInvitations =>
      pendingInvitations.exists { storedInvitation =>
        storedInvitation.clientId == storedInvitation.suppliedClientId && storedInvitation.clientIdType == "ni"
      }
    }

  def createInvitationSent(agencyEmail: String, invitationLink: String, arn: Arn, basket: Set[AuthorisationRequest])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[State] = {

    val services = basket.map(_.invitation.service)

    basket
      .find(_.invitation.service == Service.MtdIt)
      .map(_.invitation.clientId)
      .fold(toFuture(false)) { clientId =>
        isAltItsa(arn, clientId)
      }
      .map { hasAltItsa =>
        InvitationSent(ClientType.Personal, invitationLink, None, agencyEmail, services, Some(hasAltItsa))
      }

  }

  def hasPartialAuthorisationFor(arn: Arn, clientId: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    acaConnector.getPartialAuthorisationsForClient(arn, clientId).map(s => s.nonEmpty)

  def legacySaRelationshipStatusFor(arn: Arn, nino: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[LegacySaRelationshipResult] =
    relationshipsConnector.getLegacySaRelationshipStatusFor(arn, nino)

  def setRelationshipEnded(arn: Arn, clientId: String, service: Service)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Boolean]] =
    acaConnector.setRelationshipEnded(arn, clientId, service.id)
}
