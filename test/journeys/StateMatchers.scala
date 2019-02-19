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

package journeys
import org.scalatest.matchers.{MatchResult, Matcher}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.Errors.TransitionNotAllowed

trait StateMatchers[E, S] {

  def thenGo(state: S): Matcher[Either[E, (S, List[S])]] =
    new Matcher[Either[E, (S, List[S])]] {
      override def apply(result: Either[E, (S, List[S])]): MatchResult = result match {
        case Left(error) =>
          MatchResult(false, s"State $state has been expected but got error $error", s"")
        case Right((thisState, _)) if state != thisState =>
          MatchResult(false, s"State $state has been expected but got state $thisState", s"")
        case Right((thisState, _)) if state == thisState =>
          MatchResult(true, "", s"")
      }
    }

  def thenFailWith(error: E): Matcher[Either[E, (S, List[S])]] =
    new Matcher[Either[E, (S, List[S])]] {
      override def apply(result: Either[E, (S, List[S])]): MatchResult = result match {
        case Left(thisError) if thisError != error =>
          MatchResult(false, s"Error $error has been expected but got error $thisError", s"")
        case Right((state, _)) =>
          MatchResult(false, s"Error $error has been expected but got state $state", s"")
        case Left(thisError) if thisError == error =>
          MatchResult(true, s"", s"")
      }
    }

  val transitionBeNotAllowed: Matcher[Either[E, (S, List[S])]] =
    new Matcher[Either[E, (S, List[S])]] {
      override def apply(result: Either[E, (S, List[S])]): MatchResult = result match {
        case Left(TransitionNotAllowed(_, _, _)) =>
          MatchResult(true, s"", s"")
        case Left(thisError) =>
          MatchResult(false, s"Error TransitionNotAllowed has been expected but got error $thisError", s"")
        case Right((state, _)) =>
          MatchResult(false, s"Error TransitionNotAllowed has been expected but got state $state", s"")
      }
    }

}
