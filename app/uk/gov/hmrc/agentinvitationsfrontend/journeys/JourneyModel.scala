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
import scala.concurrent.Future

/**
  * JourneyModel is an abstract base of process logic definition in terms of Finite State Machine pattern.
  *
  * @see https://brilliant.org/wiki/finite-state-machines/
  */
trait JourneyModel {

  type State
  type Error

  class Transition private (val apply: PartialFunction[State, Future[Either[Error, State]]])

  protected object Transition {
    def apply(rules: PartialFunction[State, Future[Either[Error, State]]]): Transition = new Transition(rules)
  }

  /** Where your journey starts by default */
  def root: State

  /** Error to report when current state is unknown. To be used by persistence service. */
  def unknownState: Error

  /** Error to report an attempt to make invalid transition */
  def transitionNotAllowed(state: State, breadcrumbs: List[State], transition: Transition): Error

  /** Converts or wraps exception thrown during transition into an Error */
  def errorFor(ex: Exception): Error

  final def goto(state: State): Future[Either[Error, State]] = Future.successful(Right(state))

  final def fail(error: Error): Future[Either[Error, State]] = Future.successful(Left(error))

}
