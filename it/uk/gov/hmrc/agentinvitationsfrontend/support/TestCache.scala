package uk.gov.hmrc.agentinvitationsfrontend.support
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait TestCache[T] {

  class Session(var item: Option[T] = None)

  protected val sessions = collection.mutable.Map[String, Session]()

  protected def sessionKey(implicit hc: HeaderCarrier): String = hc.userId match {
    case None         => "default"
    case Some(userId) => userId.toString
  }

  def currentSession(implicit hc: HeaderCarrier): Session =
    sessions.getOrElseUpdate(sessionKey, new Session())

  def clear(): Unit =
    sessions.clear()

  protected def allSessionsRemoved: Boolean =
    sessions.isEmpty

  def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[T]] =
    Future successful currentSession.item

  def fetchAndClear(
                     implicit hc: HeaderCarrier,
                     ec: ExecutionContext): Future[Option[T]] = {
    val entry = currentSession.item
    currentSession.item = None
    Future successful entry
  }

  def save(item: T)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[T] =
    Future.successful(currentSession.item = Some(item)).map(_ => item)
}
