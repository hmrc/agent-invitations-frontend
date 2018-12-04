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

import com.google.inject.ImplementedBy
import uk.gov.hmrc.agentinvitationsfrontend.models.CurrentInvitationInput
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.SessionCache

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[FastTrackKeyStoreCache])
trait FastTrackCache extends Cache[CurrentInvitationInput]

@Singleton
class FastTrackKeyStoreCache @Inject()(session: SessionCache) extends FastTrackCache {

  val id = "fast-track-aggregate-input"

  def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[CurrentInvitationInput]] =
    session.fetchAndGetEntry[CurrentInvitationInput](id)

  def fetchAndClear(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[CurrentInvitationInput]] =
    for {
      entry <- session.fetchAndGetEntry[CurrentInvitationInput](id)
      _     <- session.cache(id, CurrentInvitationInput())
    } yield entry

  def save(invitation: CurrentInvitationInput)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[CurrentInvitationInput] =
    session.cache(id, invitation).map(_ => invitation)
}
