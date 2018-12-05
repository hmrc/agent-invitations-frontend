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

@ImplementedBy(classOf[AgentAuthorisationsKeyStoreCache])
trait AuthorisationRequestCache extends Cache[AuthorisationRequest]

@Singleton
class AgentAuthorisationsKeyStoreCache @Inject()(session: SessionCache) extends AuthorisationRequestCache {

  val id = "agent-aggregate-input"

  def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[AuthorisationRequest]] =
    session.fetchAndGetEntry[AuthorisationRequest](id)

  def fetchAndClear(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[AuthorisationRequest]] =
    for {
      entry <- session.fetchAndGetEntry[AuthorisationRequest](id)
      _     <- session.cache(id, AuthorisationRequest("", Set.empty))
    } yield entry

  def save(agentAuthorisationInput: AuthorisationRequest)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[AuthorisationRequest] =
    session.cache(id, agentAuthorisationInput).map(_ => agentAuthorisationInput)

}
