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
import uk.gov.hmrc.agentinvitationsfrontend.models.FastTrackInvitation
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.SessionCache

import scala.concurrent.{ExecutionContext, Future}

trait InvitationsCache[T] {
  def fetch()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[T]]

  def fetchAndClear()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[T]]

  def save(fastTrackInvitation: T)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit]
}

@ImplementedBy(classOf[FastTrackKeyStoreCache])
trait FastTrackCache extends InvitationsCache[FastTrackInvitation]

@Singleton
class FastTrackKeyStoreCache @Inject()(session: SessionCache) extends FastTrackCache {

  val id = "fast-track-aggregate-input"

  def fetch()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[FastTrackInvitation]] = {
    session.fetchAndGetEntry[FastTrackInvitation](id)
  }

  def fetchAndClear()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[FastTrackInvitation]] = for {
    entry <- session.fetchAndGetEntry[FastTrackInvitation](id)
    _ <- session.cache(id, FastTrackInvitation())
  } yield entry

  def save(invitation: FastTrackInvitation)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    session.cache(id, invitation).map(_ => ())
}
