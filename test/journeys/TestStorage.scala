package journeys
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait TestStorage[S] {

  @volatile
  private var state: Option[S] = None

  def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[S]] = Future.successful(state)
  def save(newState: S)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[S] = Future {
    state = Some(newState); newState
  }
}
