package uk.gov.hmrc.agentinvitationsfrontend.support

import uk.gov.hmrc.agentinvitationsfrontend.services.ContinueUrlStoreService
import uk.gov.hmrc.play.binders.ContinueUrl
import uk.gov.hmrc.agentinvitationsfrontend.models.FastTrackInvitation
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

import scala.concurrent.ExecutionContext

class TestContinueUrlKeyStoreCache extends ContinueUrlStoreService(null) {

  class Session (var continueUrl: Option[ContinueUrl] = None)

  private val sessions = collection.mutable.Map[String,Session]()

  private def sessionKey(implicit hc: HeaderCarrier): String = hc.userId match {
    case None => "default"
    case Some(userId) => userId.toString
  }

  def currentSession(implicit hc: HeaderCarrier): Session = sessions.getOrElseUpdate(sessionKey, new Session())

  def clear():Unit = sessions.clear()

  def allSessionsRemoved: Boolean = sessions.isEmpty

  override def fetchContinueUrl(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[ContinueUrl]] =
    Future successful currentSession.continueUrl

  override def cacheContinueUrl(continueUrl: ContinueUrl)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    Future successful(currentSession.continueUrl = Some(continueUrl))

}
