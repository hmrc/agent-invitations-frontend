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
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentSession
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.SessionCache

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[AgentSessionCacheImpl])
trait AgentSessionCache extends Cache[AgentSession]

@Singleton
class AgentSessionCacheImpl @Inject()(session: SessionCache) extends AgentSessionCache {

  val id = "agent-session"

  def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[AgentSession]] =
    session.fetchAndGetEntry[AgentSession](id)

  def fetchAndClear(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[AgentSession]] =
    for {
      entry <- session.fetchAndGetEntry[AgentSession](id)
      _     <- session.cache(id, AgentSession())
    } yield entry

  def save(agentSession: AgentSession)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[AgentSession] =
    session.cache(id, agentSession).map(_ => agentSession)

}
