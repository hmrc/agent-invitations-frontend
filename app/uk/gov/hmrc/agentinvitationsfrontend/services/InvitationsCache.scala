package uk.gov.hmrc.agentinvitationsfrontend.services

import javax.inject.{Inject, Singleton}

import com.google.inject.ImplementedBy
import uk.gov.hmrc.agentinvitationsfrontend.models.FastTrackInvitation
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.{CacheMap, SessionCache}

import scala.concurrent.Future

@ImplementedBy(classOf[FastTrackKeyStoreCache])
trait InvitationsCache {
  def fetchAndGetEntry()(implicit hc: HeaderCarrier): Future[Option[FastTrackInvitation]]

  def save(fastTrackInvitation: FastTrackInvitation)(implicit hc: HeaderCarrier): Future[CacheMap]
}

@Singleton
class FastTrackKeyStoreCache @Inject()(session: SessionCache) extends InvitationsCache {

  val id = "fast-track-aggregate-input"

  override def fetchAndGetEntry()(implicit hc: HeaderCarrier): Future[Option[FastTrackInvitation]] = {
    session.fetchAndGetEntry[FastTrackInvitation](id)
  }

  override def save(invitation: FastTrackInvitation)(implicit hc: HeaderCarrier): Future[CacheMap] =
    session.cache(id, invitation)
}
