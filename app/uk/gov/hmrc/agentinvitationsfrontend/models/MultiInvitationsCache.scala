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

package uk.gov.hmrc.agentinvitationsfrontend.models

import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import play.api.libs.json.Json
import uk.gov.hmrc.agentinvitationsfrontend.services.Cache
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.SessionCache

import scala.concurrent.{ExecutionContext, Future}

case class MultiInvitationsCacheItem(consents: Seq[Consent], agencyName: Option[String]) {

  def allDeclinedProcessed = consents.forall(_.consent == false)

  def allAcceptanceFailed = consents.filter(_.consent).forall(_.processed == false)

  def someAcceptanceFailed = consents.filter(_.consent).exists(_.processed == false)

  def allProcessed = consents.forall(_.processed)
}

object MultiInvitationsCacheItem {
  implicit val format = Json.format[MultiInvitationsCacheItem]
}

@ImplementedBy(classOf[MultiInvitationsKeyStoreCache])
trait MultiInvitationsCache extends Cache[MultiInvitationsCacheItem]

@Singleton
class MultiInvitationsKeyStoreCache @Inject()(session: SessionCache) extends MultiInvitationsCache {

  val id = "multi-invitation-aggregate-input"

  def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[MultiInvitationsCacheItem]] =
    session.fetchAndGetEntry[MultiInvitationsCacheItem](id)

  def fetchAndClear(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[MultiInvitationsCacheItem]] =
    for {
      entry <- session.fetchAndGetEntry[MultiInvitationsCacheItem](id)
      _     <- session.cache(id, MultiInvitationsCacheItem(Seq.empty, None))
    } yield entry

  def save(input: MultiInvitationsCacheItem)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[MultiInvitationsCacheItem] =
    session.cache(id, input).map(_ => input)
}
