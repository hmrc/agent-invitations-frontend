package uk.gov.hmrc.agentinvitationsfrontend.services

import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId
import uk.gov.hmrc.play.binders.ContinueUrl

import scala.concurrent.ExecutionContext.Implicits.global

class ContinueUrlKeyStoreCacheSpec extends BaseISpec {

  private implicit val hc = HeaderCarrier(sessionId = Some(SessionId("sessionId123456")))

  "fetch" should {
    "return an optional entry when there is something in the cache" in {
      val cache = new ContinueUrlKeyStoreCache(new TestSessionCache())
      val continueUrl = ContinueUrl("http://localhost:9996/tax-history/select-client")

      await(cache.save(continueUrl))

      await(cache.fetch) shouldBe Some(continueUrl)
    }

    "return None when there is nothing in the cache" in {
      val cache = new ContinueUrlKeyStoreCache(new TestSessionCache())

      await(cache.fetch) shouldBe None
    }
  }

  "remove" should {
    "clear the cache" in {
      val cache = new ContinueUrlKeyStoreCache(new TestSessionCache())
      val continueUrl = ContinueUrl("http://localhost:9996/tax-history/select-client")

      await(cache.save(continueUrl))

      await(cache.fetch) shouldBe Some(continueUrl)

      await(cache.remove)

      await(cache.fetch) shouldBe None
    }
  }

  "fetchErrorUrl" should {
    "return the error url when it exists in cache" in {
      val cache = new ContinueUrlKeyStoreCache(new TestSessionCache())
      val errorUrl = ContinueUrl("http://localhost:9996/tax-history/select-client?problem")

      await(cache.cacheErrorUrl(errorUrl))

      await(cache.fetchErrorUrl) shouldBe Some(errorUrl)
    }

    "return None when there is no errorUrl in the cache" in {
      val cache = new ContinueUrlKeyStoreCache(new TestSessionCache())

      await(cache.fetchErrorUrl) shouldBe None
    }
  }

  "cacheErrorUrl" should {
    "cache the error url with id errorUrl" in {
      val cache = new ContinueUrlKeyStoreCache(new TestSessionCache())
      val errorUrl = ContinueUrl("http://localhost:9996/tax-history/select-client?problem")

      await(cache.cacheErrorUrl(errorUrl))

      await(cache.fetchErrorUrl) shouldBe Some(errorUrl)
    }
  }

  "cacheAndFetchErrorUrl" should {
    "cache the error url and then return it" in {
      val cache = new ContinueUrlKeyStoreCache(new TestSessionCache())
      val errorUrl = ContinueUrl("http://localhost:9996/tax-history/select-client?problem")

      await(cache.cacheAndFetchErrorUrl(errorUrl)) shouldBe Some(errorUrl)
    }
  }

  "fetchAndClear" should {
    "return an optional journey state when there is something in the cache and generate an empty cache" in {
      val cache = new ContinueUrlKeyStoreCache(new TestSessionCache())

      val errorUrl = ContinueUrl("http://localhost:9996/tax-history/select-client?problem")

      await(cache.cacheErrorUrl(errorUrl))

      await(cache.fetchAndClear) shouldBe Some(errorUrl)

      await(cache.fetch) shouldBe None
    }

    "return None when there is nothing in the cache" in {
      val cache = new ContinueUrlKeyStoreCache(new TestSessionCache())

      await(cache.fetchAndClear) shouldBe None
    }
  }

  "save" should {
    "return the entry you are saving and overwrite a previous cache entry with the new one" in {
      val cache = new ContinueUrlKeyStoreCache(new TestSessionCache())

      val continueUrl1 = ContinueUrl("http://localhost:9996/tax-history/select-client")
      val continueUrl2 = ContinueUrl("http://localhost:9996/tax-history/select-client/2")

      await(cache.save(continueUrl1)) shouldBe continueUrl1
      await(cache.save(continueUrl2)) shouldBe continueUrl2

      await(cache.fetch) shouldBe Some(continueUrl2)
    }
  }

}
