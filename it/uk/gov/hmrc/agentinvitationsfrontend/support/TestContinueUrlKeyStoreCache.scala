package uk.gov.hmrc.agentinvitationsfrontend.support

import uk.gov.hmrc.agentinvitationsfrontend.services.ContinueUrlCache
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.binders.ContinueUrl

import scala.concurrent.{ExecutionContext, Future}

class TestContinueUrlKeyStoreCache extends ContinueUrlCache with TestCache[ContinueUrl] {

  override def remove()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] = Future {
    sessions.remove(sessionKey)
  }

  override def fetchErrorUrl(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[ContinueUrl]] =
    Future successful currentSession.item

  override def cacheErrorUrl(continueUrl: ContinueUrl)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    Future successful (currentSession.item = Some(continueUrl))

  override def cacheAndFetchErrorUrl(url: ContinueUrl)
                                    (implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[ContinueUrl]] =
    for {
      _ <- cacheErrorUrl(url)
      url <- fetchErrorUrl
    } yield url
}
