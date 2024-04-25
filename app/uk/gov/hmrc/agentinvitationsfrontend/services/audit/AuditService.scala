/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.agentinvitationsfrontend.services.audit

import play.api.mvc.RequestHeader
import uk.gov.hmrc.agentinvitationsfrontend.models.audit._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.AuditExtensions.auditHeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.DataEvent

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AuditService @Inject()(auditConnector: AuditConnector) {

  def sendAgentInvitationSubmitted(arn: String,
                                   invitationId: String,
                                   clientIdType: String,
                                   clientType: String,
                                   clientId: String,
                                   serviceId: String,
                                   uid: String,
                                   result: String,
                                   failure: Option[String] = None,
                                   altItsa: Option[Boolean] = None
                                  )(implicit hc: HeaderCarrier,
                                    request: RequestHeader,
                                    ec: ExecutionContext): Future[AuditResult] =
    auditEvent(
      AgentClientAuthorisationRequestCreated,
      "Agent client service authorisation request created",
      Map(
        "factCheck" -> result,
        "invitationId" -> invitationId,
        "agentReferenceNumber" -> arn,
        "clientType" -> clientType,
        "clientIdType" -> clientIdType,
        "clientId" -> clientId,
        "service" -> serviceId,
        "uid" -> uid
      ).filter(_._2.nonEmpty)
        ++ failure.map(e => Seq("failureDescription" -> e)).getOrElse(Map.empty)
        ++ altItsa.map(r => Seq("altITSASource" -> r.toString)).getOrElse(Map.empty)
    )

  def sendAgentInvitationResponse(arn: String,
                                  invitationId: String,
                                  isAccepted: Boolean,
                                  clientIdType: String,
                                  clientId: String,
                                  service: String,
                                  agencyName: String
                                 )(implicit hc: HeaderCarrier,
                                   request: RequestHeader,
                                   ec: ExecutionContext): Future[AuditResult] =
    auditEvent(
      AgentClientInvitationResponse,
      "agent-client-invitation-response",
      Map(
        "invitationId" -> invitationId,
        "agentReferenceNumber" -> arn,
        "agencyName" -> agencyName,
        "clientIdType" -> clientIdType,
        "clientId" -> clientId,
        "service" -> service,
        "clientResponse" -> (if (isAccepted) "Accepted" else "Rejected")
      )
    )

  private def auditEvent(event: AgentInvitationAuditEvent,
                         transactionName: String,
                         details: Map[String, String]
                        )(implicit hc: HeaderCarrier,
                          request: RequestHeader,
                          ec: ExecutionContext): Future[AuditResult] =
    auditConnector.sendEvent(
      DataEvent(
        auditSource = "agent-invitations-frontend",
        auditType = event.toString,
        tags = hc.toAuditTags(transactionName, request.path),
        detail = details
      )
    )

}
