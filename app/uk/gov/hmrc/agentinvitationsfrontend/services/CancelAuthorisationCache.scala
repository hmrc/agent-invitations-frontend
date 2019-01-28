/*
 * Copyright 2019 HM Revenue & Customs
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
import uk.gov.hmrc.agentinvitationsfrontend.models.CancelAuthorisationRequest
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.SessionCache

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[CancelAuthorisationCacheImpl])
trait CancelAuthorisationCache extends Cache[CancelAuthorisationRequest]

@Singleton
class CancelAuthorisationCacheImpl @Inject()(session: SessionCache) extends CancelAuthorisationCache {

  val id = "cancel-authorisation-aggregate-input"

  def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[CancelAuthorisationRequest]] =
    session.fetchAndGetEntry[CancelAuthorisationRequest](id)

  def fetchAndClear(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[CancelAuthorisationRequest]] =
    for {
      entry <- session.fetchAndGetEntry[CancelAuthorisationRequest](id)
      _     <- session.cache(id, CancelAuthorisationRequest())
    } yield entry

  def save(request: CancelAuthorisationRequest)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[CancelAuthorisationRequest] =
    session.cache(id, request).map(_ => request)
}