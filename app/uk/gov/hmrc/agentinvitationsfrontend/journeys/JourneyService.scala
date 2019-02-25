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
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

/**
  * JourneyService is an abstract base of components exposing journey to the application (controller)
  */
trait JourneyService {

  val model: JourneyModel

  type StateAndBreadcrumbs = (model.State, List[model.State])

  /**
    * Applies transition to the current state and returns new state or error.
    */
  def apply(transition: model.Transition)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[StateAndBreadcrumbs]

  def currentState(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[StateAndBreadcrumbs]]

}

trait PersistentJourneyService extends JourneyService {

  protected def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[StateAndBreadcrumbs]]
  protected def save(
    state: StateAndBreadcrumbs)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[StateAndBreadcrumbs]

  override def apply(
    transition: model.Transition)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[StateAndBreadcrumbs] =
    for {
      initialState <- fetch
      endStateOrError <- initialState.getOrElse((model.root, Nil)) match {
                          case (state, breadcrumbs) =>
                            if (transition.apply.isDefinedAt(state)) transition.apply(state) flatMap { endState =>
                              save((endState, if (endState == state) breadcrumbs else state :: breadcrumbs.take(9)))
                            } else
                              model.fail(model.TransitionNotAllowed(state, breadcrumbs, transition))
                        }
    } yield endStateOrError

  override def currentState(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[StateAndBreadcrumbs]] =
    fetch

}
