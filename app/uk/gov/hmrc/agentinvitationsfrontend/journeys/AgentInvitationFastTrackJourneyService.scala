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
import javax.inject.{Inject, Named, Singleton}
import play.api.libs.json._
import play.modules.reactivemongo.ReactiveMongoComponent
import uk.gov.hmrc.agentinvitationsfrontend.repository.SessionCache
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[MongoDBCachedAgentInvitationFastTrackJourneyService])
trait AgentInvitationFastTrackJourneyService extends PersistentJourneyService {

  override val model = AgentInvitationFastTrackJourneyModel
}

@Singleton
class MongoDBCachedAgentInvitationFastTrackJourneyService @Inject()(
  @Named("mongodb.session.expireAfterSeconds") val _expireAfterSeconds: Int,
  _mongo: ReactiveMongoComponent,
  _ec: ExecutionContext)
    extends AgentInvitationFastTrackJourneyService {

  val id = "agentInvitationFastTrackJourney"

  case class PersistentState(state: model.State, breadcrumbs: List[model.State])

  implicit val formats1: Format[model.State] = AgentInvitationFastTrackJourneyStateFormats.formats
  implicit val formats2: Format[PersistentState] = Json.format[PersistentState]

  final val cache = new SessionCache[PersistentState] {
    override val expireAfterSeconds: Int = _expireAfterSeconds
    override val mongo: ReactiveMongoComponent = _mongo
    override val sessionName: String = id
    override implicit val ec: ExecutionContext = _ec
  }

  protected def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[StateAndBreadcrumbs]] =
    cache.fetch.map(_.map(ps => (ps.state, ps.breadcrumbs)))

  protected def save(
    state: StateAndBreadcrumbs)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[StateAndBreadcrumbs] =
    cache.save(PersistentState(state._1, state._2)).map(_ => state)

}
