package uk.gov.hmrc.agentinvitationsfrontend.services

import uk.gov.hmrc.agentinvitationsfrontend.models.CancelAuthorisationRequest
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class CancelAuthorisationCacheImplSpec extends BaseISpec {

  private implicit val hc = HeaderCarrier(sessionId = Some(SessionId("sessionId123456")))

  "fetch" should {
    "return an optional entry when there is something in the cache" in {
      val cache = new CancelAuthorisationCacheImpl(new TestSessionCache())

      val cancelAuthRequest = CancelAuthorisationRequest(Some("personal"), Some("HMRC-MTD-IT"))

      await(cache.save(cancelAuthRequest))

      await(cache.fetch) shouldBe Some(cancelAuthRequest)
    }

    "return None when there is nothing in the cache" in {
      val cache = new CancelAuthorisationCacheImpl(new TestSessionCache())

      await(cache.fetch) shouldBe None
    }
  }

  "fetchAndClear" should {
    "return an optional journey state when there is something in the cache and generate an empty cache" in {
      val cache = new CancelAuthorisationCacheImpl(new TestSessionCache())

      val cancelAuthRequest = CancelAuthorisationRequest(Some("personal"), Some("HMRC-MTD-IT"))

      await(cache.save(cancelAuthRequest))

      await(cache.fetchAndClear) shouldBe Some(cancelAuthRequest)

      await(cache.fetch) shouldBe Some(CancelAuthorisationRequest(None, None))
    }

    "return None when there is nothing in the cache" in {
      val cache = new CancelAuthorisationCacheImpl(new TestSessionCache())

      await(cache.fetchAndClear) shouldBe None
    }
  }

  "save" should {
    "return the entry you are saving and overwrite a previous cache entry with the new one" in {
      val cache = new CancelAuthorisationCacheImpl(new TestSessionCache())

      val cancelAuthRequest1 = CancelAuthorisationRequest(Some("personal"), Some("HMRC-MTD-IT"))
      val cancelAuthRequest2 = CancelAuthorisationRequest(Some("business"), Some("HMRC-MTD-VAT"))

      await(cache.save(cancelAuthRequest1)) shouldBe cancelAuthRequest1
      await(cache.save(cancelAuthRequest2)) shouldBe cancelAuthRequest2

      await(cache.fetch) shouldBe Some(cancelAuthRequest2)
    }
  }

}
