package uk.gov.hmrc.agentinvitationsfrontend.services

import uk.gov.hmrc.agentinvitationsfrontend.models.CurrentAuthorisationRequest
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class CurrentAuthorisationRequestKeyStoreCacheSpec extends BaseISpec {

  private implicit val hc = HeaderCarrier(sessionId = Some(SessionId("sessionId123456")))

  "fetch" should {
    "return an optional entry when there is something in the cache" in {
      val cache = new CurrentAuthorisationRequestKeyStoreCache(new TestSessionCache())

      val request = CurrentAuthorisationRequest(Some("personal"), "HMRC-MTD-IT", "ni", "AB123456A", Some("BN114AW"), false)

      await(cache.save(request))

      await(cache.fetch) shouldBe Some(request)
    }

    "return None when there is nothing in the cache" in {
      val cache = new CurrentAuthorisationRequestKeyStoreCache(new TestSessionCache())

      await(cache.fetch) shouldBe None
    }
  }

  "fetchAndClear" should {
    "return an optional journey state when there is something in the cache and generate an empty cache" in {
      val cache = new CurrentAuthorisationRequestKeyStoreCache(new TestSessionCache())

      val request = CurrentAuthorisationRequest(Some("personal"), "HMRC-MTD-IT", "ni", "AB123456A", Some("BN114AW"), false)

      await(cache.save(request))

      await(cache.fetchAndClear) shouldBe Some(request)

      await(cache.fetch) shouldBe Some(CurrentAuthorisationRequest(None, "", "", "", None, false))
    }

    "return None when there is nothing in the cache" in {
      val cache = new CurrentAuthorisationRequestKeyStoreCache(new TestSessionCache())

      await(cache.fetchAndClear) shouldBe None
    }
  }

  "save" should {
    "return the entry you are saving and overwrite a previous cache entry with the new one" in {
      val cache = new CurrentAuthorisationRequestKeyStoreCache(new TestSessionCache())

      val request1 = CurrentAuthorisationRequest(Some("personal"), "HMRC-MTD-IT", "ni", "AB123456A", Some("BN114AW"), false)
      val request2 = CurrentAuthorisationRequest(Some("personal"), "HMRC-MTD-IT", "ni", "AB123456A", Some("BN114AW"), false)

      await(cache.save(request1)) shouldBe request1
      await(cache.save(request2)) shouldBe request2

      await(cache.fetch) shouldBe Some(request2)
    }
  }

}
