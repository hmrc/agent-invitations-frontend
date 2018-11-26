package uk.gov.hmrc.agentinvitationsfrontend.support

import uk.gov.hmrc.agentinvitationsfrontend.models.{MultiInvitationsCache, MultiInvitationsCacheItem}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

class TestMultiInvitationsCache extends MultiInvitationsCache {

  class Session(var multiInvitationsCacheItem: Option[MultiInvitationsCacheItem] = None)

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

  override def fetch()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[MultiInvitationsCacheItem]] =
    Future successful currentSession.multiInvitationsCacheItem

  override def fetchAndClear()(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[MultiInvitationsCacheItem]] = {
    val entry = currentSession.multiInvitationsCacheItem
    currentSession.multiInvitationsCacheItem = Some(MultiInvitationsCacheItem(Seq.empty, None))
    Future successful entry
  }

  override def save(
                     multiInvitationsCacheItem: MultiInvitationsCacheItem)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    Future successful (currentSession.multiInvitationsCacheItem = Some(multiInvitationsCacheItem))

}
