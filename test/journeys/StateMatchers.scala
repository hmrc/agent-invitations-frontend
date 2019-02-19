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
