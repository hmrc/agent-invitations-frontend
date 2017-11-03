package uk.gov.hmrc.agentinvitationsfrontend.audit

import javax.inject.{Inject, Singleton}

import play.api.mvc.Request
import uk.gov.hmrc.agentinvitationsfrontend.audit.AgentInvitationEvent.AgentInvitationEvent
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentInvitationUserInput, Invitation}
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.DataEvent
import uk.gov.hmrc.play.audit.AuditExtensions._

import scala.concurrent.Future
import scala.util.Try

object AgentInvitationEvent extends Enumeration {
  val agentInvitationSubmitted = Value
  type AgentInvitationEvent = Value
}

@Singleton
class AuditService @Inject()(val auditConnector: AuditConnector){

  def sendAgentInvitationSubmitted(arn: Arn, invitationId: String, agentInvitationUserInput: AgentInvitationUserInput, result: String)(implicit hc: HeaderCarrier, request: Request[Any]): Unit = {
    auditEvent(AgentInvitationEvent.agentInvitationSubmitted, "agent-invitation-submitted",
      Seq("result" -> result,
        "invitationId" -> invitationId,
        "agentReferenceNumber" -> arn.value,
        "regimeId" -> agentInvitationUserInput.nino.value,
        "regime" -> "HMRC-MTD-IT",
        "authorization" -> "",
        "token" -> "")
    )
  }

  private[audit] def auditEvent(event: AgentInvitationEvent, transactionName: String, details: Seq[(String, Any)] = Seq.empty)
                               (implicit hc: HeaderCarrier, request: Request[Any]): Future[Unit] = {
    send(createEvent(event, transactionName, details: _*))
  }

  private def createEvent(event: AgentInvitationEvent, transactionName: String, details: (String, Any)*)
                         (implicit hc: HeaderCarrier, request: Request[Any]): DataEvent = {

    val detail = hc.toAuditDetails(details.map(pair => pair._1 -> pair._2.toString): _*)
    val tags = hc.toAuditTags(transactionName, request.path)
    DataEvent(auditSource = "agent-invitation-frontend",
      auditType = event.toString,
      tags = tags,
      detail = detail
    )
  }

  private def send(events: DataEvent*)(implicit hc: HeaderCarrier): Future[Unit] = {
    Future {
      events.foreach { event =>
        Try(auditConnector.sendEvent(event))
      }
    }
  }

}
