/*
 * Copyright 2018 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.agentinvitationsfrontend.services

import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import uk.gov.hmrc.agentinvitationsfrontend.models.ContinueUrlJsonFormat._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.SessionCache
import uk.gov.hmrc.play.binders.ContinueUrl

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[ContinueUrlKeyStoreCache])
trait ContinueUrlCache extends Cache[ContinueUrl] {

  def fetchErrorUrl(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[ContinueUrl]]

  def cacheErrorUrl(url: ContinueUrl)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit]

  def cacheAndFetchErrorUrl(
    url: ContinueUrl)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[ContinueUrl]]

  def remove()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit]

}

@Singleton
class ContinueUrlKeyStoreCache @Inject()(session: SessionCache) extends ContinueUrlCache {

  val id = "continueUrl"

  def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[ContinueUrl]] =
    session.fetchAndGetEntry[ContinueUrl](id)

  def remove()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    session.remove().map(_ => ())

  def fetchErrorUrl(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[ContinueUrl]] =
    session.fetchAndGetEntry[ContinueUrl]("errorUrl")

  def cacheErrorUrl(url: ContinueUrl)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    session.cache("errorUrl", url).map(_ => ())

  def cacheAndFetchErrorUrl(
    url: ContinueUrl)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[ContinueUrl]] =
    for {
      _   <- cacheErrorUrl(url)
      url <- fetchErrorUrl
    } yield url

  def fetchAndClear(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[ContinueUrl]] =
    for {
      url <- fetchErrorUrl
      _   <- remove()
    } yield url

  def save(url: ContinueUrl)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[ContinueUrl] =
    session.cache(id, url).map(_ => url)

}
