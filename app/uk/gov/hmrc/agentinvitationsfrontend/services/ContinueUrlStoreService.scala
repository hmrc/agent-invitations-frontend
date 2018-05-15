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

import javax.inject.{Inject, Singleton}
import play.api.mvc.Action
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.SessionCache
import uk.gov.hmrc.play.binders.ContinueUrl
import uk.gov.hmrc.agentinvitationsfrontend.models.ContinueUrlJsonFormat._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ContinueUrlStoreService @Inject()(continueUrlCache: SessionCache) {

  def fetchContinueUrl(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[ContinueUrl]] =
    continueUrlCache.fetchAndGetEntry[ContinueUrl]("continueUrl")

  def cacheContinueUrl(url: ContinueUrl)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    continueUrlCache.cache("continueUrl", url).map(_ => ())

  def remove()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    continueUrlCache.remove().map(_ => ())

}
