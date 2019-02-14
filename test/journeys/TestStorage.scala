package journeys
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait TestStorage[State] {

  @volatile
  private var state: Option[State] = None

  def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[State]] = Future.successful(state)
  def save(newState: State)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[State] = Future {
    state = Some(newState); newState
  }
}
