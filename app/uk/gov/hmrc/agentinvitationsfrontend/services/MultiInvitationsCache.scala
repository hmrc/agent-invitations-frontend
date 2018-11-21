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
import play.api.libs.json.Json
import uk.gov.hmrc.agentinvitationsfrontend.models.CurrentInvitationInput
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.SessionCache

import scala.concurrent.{ExecutionContext, Future}

trait MultiInvitationsCache[T] {
  def fetch()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[T]]

  def fetchAndClear()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[T]]

  def updateIsSelected(selecteds: Seq[Boolean])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit]

  def save(input: T)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit]
}

case class MultiInvitationsCacheInput(
  invitationId: Seq[InvitationId],
  //messageKey: Option[String],
  isSelected: Seq[Boolean])

object MultiInvitationsCacheInput {
  implicit val format = Json.format[MultiInvitationsCacheInput]
}

@ImplementedBy(classOf[MultiInvitationKeyStoreCache])
trait MultiInvitationCache extends MultiInvitationsCache[MultiInvitationsCacheInput]

@Singleton
class MultiInvitationKeyStoreCache @Inject()(session: SessionCache) extends MultiInvitationCache {

  val id = "multi-invitation-aggregate-input"

  def fetch()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[MultiInvitationsCacheInput]] =
    session.fetchAndGetEntry[MultiInvitationsCacheInput](id)

  def fetchAndClear()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[MultiInvitationsCacheInput]] =
    for {
      entry <- session.fetchAndGetEntry[MultiInvitationsCacheInput](id)
      _     <- session.cache(id, MultiInvitationsCacheInput(Seq.empty, Seq.empty))
    } yield entry

  def updateIsSelected(isSelecteds: Seq[Boolean])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    session
      .fetchAndGetEntry[MultiInvitationsCacheInput](id)
      .map(entry => session.cache(id, MultiInvitationsCacheInput(entry.get.invitationId, isSelecteds)))
      .map(_ => ())

  def save(input: MultiInvitationsCacheInput)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    session.cache(id, input).map(_ => ())
}
