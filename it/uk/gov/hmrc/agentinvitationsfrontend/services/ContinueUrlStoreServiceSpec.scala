package uk.gov.hmrc.agentinvitationsfrontend.services

import play.api.libs.json.{JsValue, Reads, Writes}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.cache.client.{CacheMap, NoSessionException, SessionCache}
import uk.gov.hmrc.http.logging.SessionId
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.binders.ContinueUrl
import uk.gov.hmrc.play.test.UnitSpec

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class ContinueUrlStoreServiceSpec extends BaseISpec {

  private implicit val hc = HeaderCarrier(sessionId = Some(SessionId("sessionId123456")))

  val url = ContinueUrl("http://localhost:9996/tax-history/select-client")

  "ContinueUrlStoreService" should {
    "store continue url" in {

      await(continueUrlKeyStoreCache.cacheContinueUrl(url))

      await(continueUrlKeyStoreCache.fetchContinueUrl) shouldBe Some(url)
    }

    "return nothing if there is no continue url" in {

      await(continueUrlKeyStoreCache.fetchContinueUrl) shouldBe None
    }
  }

}