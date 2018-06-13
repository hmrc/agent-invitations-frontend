package uk.gov.hmrc.agentinvitationsfrontend.support

import uk.gov.hmrc.agentinvitationsfrontend.models.CurrentInvitationInput
import uk.gov.hmrc.agentinvitationsfrontend.services.FastTrackCache
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

class TestFastTrackCache extends FastTrackCache {

  class Session(var currentInvitationInput: Option[CurrentInvitationInput] = None)

  private val sessions = collection.mutable.Map[String, Session]()

  private def sessionKey(implicit hc: HeaderCarrier): String = hc.userId match {
    case None         => "default"
    case Some(userId) => userId.toString
  }

  def currentSession(implicit hc: HeaderCarrier): Session =
    sessions.getOrElseUpdate(sessionKey, new Session())

  def clear(): Unit =
    sessions.clear()

  def allSessionsRemoved: Boolean =
    sessions.isEmpty

  override def fetch()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[CurrentInvitationInput]] =
    Future successful currentSession.currentInvitationInput

  override def fetchAndClear()(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[CurrentInvitationInput]] = {
    val entry = currentSession.currentInvitationInput
    currentSession.currentInvitationInput = Some(CurrentInvitationInput())
    Future successful entry
  }

  override def save(
    currentInvitationInput: CurrentInvitationInput)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    Future successful (currentSession.currentInvitationInput = Some(currentInvitationInput))

}
