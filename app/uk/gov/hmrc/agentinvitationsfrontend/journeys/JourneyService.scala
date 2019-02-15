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
                              case Left(error)     => Future.successful(Left(error))
                            }
                          case None => model.fail(model.unknownState)
                        }
    } yield endStateOrError

}
