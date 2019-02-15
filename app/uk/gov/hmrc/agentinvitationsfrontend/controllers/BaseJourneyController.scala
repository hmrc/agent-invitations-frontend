package uk.gov.hmrc.agentinvitationsfrontend.controllers

import play.api.mvc.{Action, AnyContent, Result}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.JourneyService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

abstract class BaseJourneyController(val journeyService: JourneyService)(implicit ec: ExecutionContext)
    extends FrontendController {

  import journeyService.model.{Error, State, Transition}

  val handleError: Error => Future[Result]
  val renderState: State => Result

  protected final def simpleAction(transition: Transition): Action[AnyContent] =
    Action.async { implicit request =>
      apply(transition)
    }

  protected final def apply(transition: Transition)(implicit hc: HeaderCarrier): Future[Result] =
    journeyService
      .apply(transition)
      .recover {
        case e: Exception => Left(journeyService.model.errorFor(e))
      }
      .flatMap(
        _.fold(
          handleError,
          state => Future.successful(renderState(state))
        ))

}
