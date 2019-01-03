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

import javax.inject.{Inject, Singleton}

import com.google.inject.ImplementedBy
import uk.gov.hmrc.agentinvitationsfrontend.models.CurrentAuthorisationRequest
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.SessionCache

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[CurrentAuthorisationRequestKeyStoreCache])
trait CurrentAuthorisationRequestCache extends Cache[CurrentAuthorisationRequest]

@Singleton
class CurrentAuthorisationRequestKeyStoreCache @Inject()(session: SessionCache)
    extends CurrentAuthorisationRequestCache {

  val id = "fast-track-aggregate-input"

  def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[CurrentAuthorisationRequest]] =
    session.fetchAndGetEntry[CurrentAuthorisationRequest](id)

  def fetchAndClear(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[CurrentAuthorisationRequest]] =
    for {
      entry <- session.fetchAndGetEntry[CurrentAuthorisationRequest](id)
      _     <- session.cache(id, CurrentAuthorisationRequest())
    } yield entry

  def save(invitation: CurrentAuthorisationRequest)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[CurrentAuthorisationRequest] =
    session.cache(id, invitation).map(_ => invitation)
}
