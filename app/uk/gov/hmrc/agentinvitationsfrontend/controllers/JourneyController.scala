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

import play.api.data.Form
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, Request, Result}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.JourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.AuthorisedAgent
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

trait FormValidationError

/**
  * Base controller for journeys based on Finite State Machine.
  *
  * Provides 3 extension points:
  *   - renderState: how to represent current state
  *   - handleError: how to handle transition error
  *   - handleFormValidationError: how to handle form validation error
  *
  * and few action creation helpers:
  *   - simpleAction
  *   - authorisedAgentAction
  *   - authorisedAgentActionWithForm
  */
abstract class JourneyController(implicit ec: ExecutionContext)
    extends FrontendController with I18nSupport with AuthActions {

  val journeyService: JourneyService

  import journeyService.model.{Error, State, Transition}

  type Route = Request[_] => Result

  def renderState(state: State, breadcrumbs: List[State]): Route // implement this to render state after transition
  def handleError(error: Error): Route // implement this to handle model errors
  def handleFormValidationError(error: FormValidationError, breadcrumbs: List[State]): Route // implement this to handle form validation errors

  protected final def apply(transition: Transition)(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
    journeyService
      .apply(transition)
      .recover {
        case e: Exception => Left(journeyService.model.errorFor(e))
      }
      .map(_.fold(
        error => handleError(error)(request), { case (state, breadcrumbs) => renderState(state, breadcrumbs)(request) }
      ))

  protected final def simpleAction(transition: Transition): Action[AnyContent] =
    Action.async { implicit request =>
      apply(transition)
    }

  protected final def authorisedAgentAction(transition: AuthorisedAgent => Transition): Action[AnyContent] =
    Action.async { implicit request =>
      withAuthorisedAsAgent { (arn, isWhitelisted) =>
        apply(transition(AuthorisedAgent(arn, isWhitelisted)))
      }
    }

  protected final def authorisedAgentActionWithForm[T](form: Form[T])(transition: AuthorisedAgent => T => Transition)(
    validationError: Form[T] => FormValidationError): Action[AnyContent] =
    Action.async { implicit request =>
      withAuthorisedAsAgent { (arn, isWhitelisted) =>
        bindForm(form, transition(AuthorisedAgent(arn, isWhitelisted)), validationError)
      }
    }

  private def bindForm[T](form: Form[T], transition: T => Transition, validationError: Form[T] => FormValidationError)(
    implicit hc: HeaderCarrier,
    request: Request[_]): Future[Result] =
    form
      .bindFromRequest()
      .fold(
        formWithErrors =>
          journeyService.currentState.map {
            case Some((_, breadcrumbs)) =>
              handleFormValidationError(validationError(formWithErrors), breadcrumbs)(request)
            case None => handleFormValidationError(validationError(formWithErrors), Nil)(request)
        },
        userInput => apply(transition(userInput))
      )

}
