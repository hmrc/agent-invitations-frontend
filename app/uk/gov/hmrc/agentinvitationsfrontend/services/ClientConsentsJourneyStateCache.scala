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
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientConsentsJourneyState
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.SessionCache

import scala.concurrent.{ExecutionContext, Future}
@ImplementedBy(classOf[ClientConsentsJourneyStateKeyStoreCache])
trait ClientConsentsJourneyStateCache extends Cache[ClientConsentsJourneyState]

@Singleton
class ClientConsentsJourneyStateKeyStoreCache @Inject()(session: SessionCache) extends ClientConsentsJourneyStateCache {

  val id = "multi-invitation-aggregate-input"

  def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[ClientConsentsJourneyState]] =
    session.fetchAndGetEntry[ClientConsentsJourneyState](id)

  def fetchAndClear(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[ClientConsentsJourneyState]] =
    for {
      entry <- session.fetchAndGetEntry[ClientConsentsJourneyState](id)
      _     <- session.cache(id, ClientConsentsJourneyState(Seq.empty, None))
    } yield entry

  def save(input: ClientConsentsJourneyState)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[ClientConsentsJourneyState] =
    session.cache(id, input).map(_ => input)
}
