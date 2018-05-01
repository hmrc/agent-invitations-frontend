package uk.gov.hmrc.agentinvitationsfrontend.services

import uk.gov.hmrc.agentinvitationsfrontend.models.FastTrackInvitation
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global


class FastTrackKeyStoreCacheSpec extends BaseISpec {

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session1234356")))

  val fastTrackInvitation = FastTrackInvitation(Some("service"), Some("clientIdentifierType"), Some("clientIdentifier"), Some("postcode"), Some("vatRegDate"))

  "FastTrackKeyStoreCache" should {
    "store and fetch saved fast-track invitation from keystore" in {
      await(fastTrackKeyStoreCache.save(fastTrackInvitation))
      val result = await(fastTrackKeyStoreCache.fetchAndGetEntry())

      result shouldBe Some(fastTrackInvitation)
    }

    "return nothing if no fast-track invitation was stored in keystore" in {
      val result = await(fastTrackKeyStoreCache.fetchAndGetEntry())
      result shouldBe None
    }
  }
}
