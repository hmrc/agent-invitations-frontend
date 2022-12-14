/*
 * Copyright 2022 HM Revenue & Customs
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
import play.api.libs.json._
import play.api.mvc.Request
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State.WarmUpSessionRequired
import uk.gov.hmrc.agentinvitationsfrontend.repository.{SessionCache, SessionCacheRepository}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.fsm.PersistentJourneyService

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[MongoDBCachedClientInvitationJourneyService])
trait ClientInvitationJourneyService extends PersistentJourneyService[HeaderCarrier] {

  override val journeyKey = "clientInvitationJourney"

  override val model = ClientInvitationJourneyModel

  // do not keep errors or root in the journey history
  override val breadcrumbsRetentionStrategy: Breadcrumbs => Breadcrumbs =
    _.filterNot(s => s.isInstanceOf[model.IsError] || s == model.State.MissingJourneyHistory)

  override def updateBreadcrumbs(
    newState: model.State,
    currentState: model.State,
    currentBreadcrumbs: Breadcrumbs
  ): Breadcrumbs =
    if (newState.getClass == currentState.getClass)
      currentBreadcrumbs
    else if (currentBreadcrumbs.nonEmpty && currentBreadcrumbs.head.getClass() == newState.getClass())
      currentBreadcrumbs.tail
    else
      currentState match {
        case _: WarmUpSessionRequired => breadcrumbsRetentionStrategy(currentBreadcrumbs)
        case _                        => currentState :: breadcrumbsRetentionStrategy(currentBreadcrumbs)
      }
}

@Singleton
class MongoDBCachedClientInvitationJourneyService @Inject()(_cacheRepository: SessionCacheRepository) extends ClientInvitationJourneyService {

  case class PersistentState(state: model.State, breadcrumbs: List[model.State])

  implicit val formats1: Format[model.State] = ClientInvitationJourneyStateFormats.formats
  implicit val formats2: Format[PersistentState] = Json.format[PersistentState]

  final val cache = new SessionCache[PersistentState] {
    override val sessionName: String = journeyKey
    override val cacheRepository: SessionCacheRepository = _cacheRepository
    override def getSessionId(implicit hc: HeaderCarrier): Option[String] =
      hc.extraHeaders.collectFirst({ case (headerName, headerValue) if headerName == journeyKey => headerValue })
  }

  protected def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[StateAndBreadcrumbs]] =
    cache.fetch.map(_.map(ps => (ps.state, ps.breadcrumbs)))

  protected def save(state: StateAndBreadcrumbs)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[StateAndBreadcrumbs] =
    cache.save(PersistentState(state._1, state._2)).map(_ => state)

  override def clear(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    cache.delete().map(_ => ())

}
