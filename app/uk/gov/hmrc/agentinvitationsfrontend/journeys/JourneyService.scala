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

  /**
    * Applies transition to the current state and returns new state or error.
    */
  def apply(transition: model.Transition)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Either[model.Error, model.State]]

}

trait PersistentJourneyService extends JourneyService {

  protected def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[model.State]]
  protected def save(state: model.State)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[model.State]

  override def apply(transition: model.Transition)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Either[model.Error, model.State]] =
    for {
      initialState <- fetch
      endStateOrError <- initialState match {
                          case Some(state) =>
                            transition.apply(state) flatMap {
                              case Right(endState) => save(endState).map(Right.apply)
                              case Left(error)     => model.fail(error)
                            }
                          case None => model.fail(model.unknownState)
                        }
    } yield endStateOrError

}
