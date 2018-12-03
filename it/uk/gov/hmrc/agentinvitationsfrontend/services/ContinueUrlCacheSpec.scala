package uk.gov.hmrc.agentinvitationsfrontend.services

import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId
import uk.gov.hmrc.play.binders.ContinueUrl

import scala.concurrent.ExecutionContext.Implicits.global

class ContinueUrlCacheSpec extends BaseISpec {

  private implicit val hc = HeaderCarrier(sessionId = Some(SessionId("sessionId123456")))

  val url = ContinueUrl("http://localhost:9996/tax-history/select-client")

  "ContinueUrlStoreService" should {
    "store continue url" in {
      await(continueUrlKeyStoreCache.save(url))
      await(continueUrlKeyStoreCache.fetch) shouldBe Some(url)
    }

    "return nothing if there is no continue url" in {
      await(continueUrlKeyStoreCache.fetch) shouldBe None
    }

    "return nothing when ContinueUrl is removed" in {
      await(continueUrlKeyStoreCache.save(url))
      await(continueUrlKeyStoreCache.remove())
      await(continueUrlKeyStoreCache.fetch) shouldBe None
    }
  }
}
