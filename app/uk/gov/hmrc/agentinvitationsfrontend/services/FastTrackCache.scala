package uk.gov.hmrc.agentinvitationsfrontend.services

import com.google.inject.ImplementedBy
import javax.inject.{Inject, Named, Singleton}
import uk.gov.hmrc.agentinvitationsfrontend.models.FastTrackInvitation
import uk.gov.hmrc.http.{HeaderCarrier, HttpVerbs}
import uk.gov.hmrc.http.cache.client.{CacheMap, SessionCache}

import scala.concurrent.Future

@ImplementedBy(classOf[FastTrackKeyStoreCache])
trait FastTrackCache {
  def fetchAndGetEntry()(implicit hc: HeaderCarrier): Future[Option[FastTrackInvitation]]

  def save(fastTrackInvitation: FastTrackInvitation)(implicit hc: HeaderCarrier): Future[CacheMap]
}

@Singleton
class FastTrackKeyStoreCache @Inject()(session: SessionCache) extends FastTrackCache {

  val id = "fast-track-aggregate-input"

  def fetchAndGetEntry()(implicit hc: HeaderCarrier): Future[Option[FastTrackInvitation]] = {
    session.fetchAndGetEntry[FastTrackInvitation](id)
  }

  def save(fastTrackInvitation: FastTrackInvitation)(implicit hc: HeaderCarrier): Future[CacheMap] =
    session.cache(id, fastTrackInvitation)
}
