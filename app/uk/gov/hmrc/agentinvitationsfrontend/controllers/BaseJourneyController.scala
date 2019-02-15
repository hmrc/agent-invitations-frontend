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

package uk.gov.hmrc.agentinvitationsfrontend.controllers

import play.api.mvc.{Action, AnyContent, Request, Result}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.JourneyService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

abstract class BaseJourneyController(val journeyService: JourneyService)(implicit ec: ExecutionContext)
    extends FrontendController {

  import journeyService.model.{Error, State, Transition}

  def handleError(error: Error)(implicit request: Request[_]): Future[Result]
  def renderState(state: State)(implicit request: Request[_]): Result

  protected final def simpleAction(transition: Transition): Action[AnyContent] =
    Action.async { implicit request =>
      apply(transition)
    }

  protected final def apply(transition: Transition)(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
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
