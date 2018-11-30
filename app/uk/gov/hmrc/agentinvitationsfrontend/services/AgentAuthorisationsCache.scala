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
import uk.gov.hmrc.agentinvitationsfrontend.models.{AuthorisationRequest, CurrentInvitationInput}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.SessionCache

import scala.concurrent.{ExecutionContext, Future}

trait AgentAuthorisationsCache[T] {
  def fetch()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[T]]

  def fetchAndClear()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[T]]

  def save(agentAuthorisationInput: T)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit]

}

@ImplementedBy(classOf[AgentAuthoristionsKeyStoreCache])
trait AuthorisationRequestCache extends AgentAuthorisationsCache[Seq[AuthorisationRequest]]

@Singleton
class AgentAuthoristionsKeyStoreCache @Inject()(session: SessionCache) extends AuthorisationRequestCache {

  val id = "agent-aggregate-input"

  def fetch()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Seq[AuthorisationRequest]]] =
    session.fetchAndGetEntry[Seq[AuthorisationRequest]](id)

  def fetchAndClear()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Seq[AuthorisationRequest]]] =
    for {
      entry <- session.fetchAndGetEntry[Seq[AuthorisationRequest]](id)
      _     <- session.cache(id, Seq[AuthorisationRequest]())
    } yield entry

  def save(agentAuthorisationInput: Seq[AuthorisationRequest])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Unit] =
    session.cache(id, agentAuthorisationInput).map(_ => ())

}
