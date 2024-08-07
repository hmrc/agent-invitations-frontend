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

package uk.gov.hmrc.agentinvitationsfrontend.audit

import javax.inject.{Inject, Singleton}
import play.api.mvc.RequestHeader
import uk.gov.hmrc.agentinvitationsfrontend.audit.AgentInvitationEvent.AgentInvitationEvent
import uk.gov.hmrc.agentinvitationsfrontend.models.Invitation
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Service}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.AuditExtensions._
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.DataEvent

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object AgentInvitationEvent extends Enumeration {
  val AgentClientAuthorisationRequestCreated, AgentClientInvitationResponse = Value
  type AgentInvitationEvent = Value
}

@Singleton
class AuditService @Inject() (val auditConnector: AuditConnector) {

  def sendAgentInvitationSubmitted(
    arn: Arn,
    invitationId: String,
    invitation: Invitation,
    uid: String,
    result: String,
    failure: Option[String] = None,
    altItsa: Option[Boolean] = None
  )(implicit hc: HeaderCarrier, request: RequestHeader, ec: ExecutionContext): Future[Unit] =
    auditEvent(
      AgentInvitationEvent.AgentClientAuthorisationRequestCreated,
      "Agent client service authorisation request created",
      Seq(
        "factCheck"            -> result,
        "invitationId"         -> invitationId,
        "agentReferenceNumber" -> arn.value,
        "clientType"           -> invitation.clientType.map(_.toString).getOrElse(""),
        "clientIdType"         -> invitation.service.supportedClientIdType.id,
        "clientId"             -> invitation.clientId,
        "service"              -> invitation.service.id,
        "uid"                  -> uid
      ).filter(_._2.nonEmpty)
        ++ failure.map(e => Seq("failureDescription" -> e)).getOrElse(Seq.empty)
        ++ altItsa.map(r => Seq("altITSASource" -> r)).getOrElse(Seq.empty)
    )

  def sendAgentInvitationResponse(
    invitationId: String,
    arn: Arn,
    isAccepted: Boolean,
    clientIdType: String,
    clientId: String,
    service: Service,
    agencyName: String
  )(implicit hc: HeaderCarrier, request: RequestHeader, ec: ExecutionContext): Future[Unit] =
    auditEvent(
      AgentInvitationEvent.AgentClientInvitationResponse,
      "agent-client-invitation-response",
      Seq(
        "invitationId"         -> invitationId,
        "agentReferenceNumber" -> arn.value,
        "agencyName"           -> agencyName,
        "clientIdType"         -> clientIdType,
        "clientId"             -> clientId,
        "service"              -> service,
        "clientResponse"       -> (if (isAccepted) "Accepted" else "Rejected")
      )
    )

  private[audit] def auditEvent(event: AgentInvitationEvent, transactionName: String, details: Seq[(String, Any)] = Seq.empty)(implicit
    hc: HeaderCarrier,
    request: RequestHeader,
    ec: ExecutionContext
  ): Future[Unit] =
    send(createEvent(event, transactionName, details: _*))

  private def createEvent(event: AgentInvitationEvent, transactionName: String, details: (String, Any)*)(implicit
    hc: HeaderCarrier,
    request: RequestHeader
  ): DataEvent = {

    val detail = hc.toAuditDetails(details.map(pair => pair._1 -> pair._2.toString): _*)
    val tags = hc.toAuditTags(transactionName, request.path)
    DataEvent(auditSource = "agent-invitations-frontend", auditType = event.toString, tags = tags, detail = detail)
  }

  private def send(events: DataEvent*)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    Future {
      events.foreach { event =>
        Try(auditConnector.sendEvent(event))
      }
    }
}
