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

package uk.gov.hmrc.agentinvitationsfrontend.journeys
import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import play.api.libs.json._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.SessionCache

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[KeystoreCachedAgentInvitationJourneyService])
trait AgentInvitationJourneyService extends PersistentJourneyService {

  override val model = AgentInvitationJourneyModel
}

@Singleton
class KeystoreCachedAgentInvitationJourneyService @Inject()(session: SessionCache)
    extends AgentInvitationJourneyService {

  val id = "agent-invitation-journey"

  case class PersistentState(state: model.State, breadcrumbs: List[model.State])

  implicit val formats1: Format[model.State] = AgentInvitationJourneyStateFormats.formats
  implicit val formats2: Format[PersistentState] = Json.format[PersistentState]

  protected def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[StateAndBreadcrumbs]] =
    session.fetchAndGetEntry[PersistentState](id).map(_.map(ps => (ps.state, ps.breadcrumbs)))

  protected def save(
    state: StateAndBreadcrumbs)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[StateAndBreadcrumbs] =
    session.cache(id, PersistentState(state._1, state._2)).map(_ => state)

}
