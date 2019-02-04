package uk.gov.hmrc.agentinvitationsfrontend.services

import play.api.libs.json.{JsValue, Reads, Writes}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.http.cache.client.{CacheMap, NoSessionException, SessionCache}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

class TestSessionCache extends SessionCache {
  override def defaultSource = ???
  override def baseUri = ???
  override def domain = ???
  override def http = ???

  private val store = mutable.Map[String, JsValue]()

  private val noSession = Future.failed[String](NoSessionException)

  private def testCacheId(implicit hc: HeaderCarrier): Future[String] =
    hc.sessionId.fold(noSession)(c => Future.successful(c.value))

  override def cache[A](formId: String, body: A)(implicit wts: Writes[A], hc: HeaderCarrier, executionContext : ExecutionContext): Future[CacheMap] =
    testCacheId.map { c =>
      store.put(formId, wts.writes(body))
      CacheMap(c, store.toMap)
    }

  override def fetch()(implicit hc: HeaderCarrier, executionContext : ExecutionContext): Future[Option[CacheMap]] =
    testCacheId.map(c => Some(CacheMap(c, store.toMap)))

  override def fetchAndGetEntry[T](key: String)(implicit hc: HeaderCarrier, rds: Reads[T], executionContext : ExecutionContext): Future[Option[T]] =
    Future {
      store.get(key).flatMap(jsValue => rds.reads(jsValue).asOpt)
    }

  override def remove()(implicit hc: HeaderCarrier, executionContext : ExecutionContext): Future[HttpResponse] =
    Future {
      store.clear()
      null
    }
}
