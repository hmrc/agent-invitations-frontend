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
import play.api.mvc._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.JourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.AuthorisedAgent
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

/**
  * Base controller for journeys based on Finite State Machine.
  *
  * Provides 2 extension points:
  *   - getCallFor: what is the endpoint representing given state
  *   - renderState: how to render given state
  *
  * and few action creation helpers:
  *   - simpleAction
  *   - authorisedAgentCurrentStateAction
  *   - authorisedAgentAction
  *   - authorisedAgentActionWithHC
  *   - authorisedAgentActionWithForm
  *   - authorisedAgentActionWithFormWithHC
  *   - authorisedAgentActionWithFormWithHCWithRequest
  */
abstract class JourneyController(implicit ec: ExecutionContext)
    extends FrontendController with I18nSupport with AuthActions {

  val journeyService: JourneyService

  import journeyService.model.{State, Transition, TransitionNotAllowed}
  import journeyService.StateAndBreadcrumbs

  type Route = Request[_] => Result

  /** implement this to map states into endpoints for redirection and back linking */
  def getCallFor(state: State): Call

  /** implement this to render state after transition or when form validation fails */
  def renderState(state: State, breadcrumbs: List[State], formWithErrors: Option[Form[_]]): Route

  type RouteFactory = StateAndBreadcrumbs => Route

  /** displays template for the state and breadcrumbs */
  final val display: RouteFactory = (state: StateAndBreadcrumbs) =>
    (request: Request[_]) => renderState(state._1, state._2, None)(request)

  /** redirects to the endpoint matching state */
  final val redirect: RouteFactory =
    (state: StateAndBreadcrumbs) => (_: Request[_]) => Redirect(getCallFor(state._1))

  private def apply(transition: Transition, routeFactory: RouteFactory)(
    implicit hc: HeaderCarrier,
    request: Request[_]): Future[Result] =
    journeyService
      .apply(transition)
      .map(routeFactory)
      .map(_(request))
      .recover {
        case TransitionNotAllowed(origin, breadcrumbs, _) =>
          renderState(origin, breadcrumbs, None)(request) // renders current state back
      }

  protected final def simpleAction(transition: Transition)(afterTransition: RouteFactory): Action[AnyContent] =
    Action.async { implicit request =>
      apply(transition, afterTransition)
    }

  protected final def authorisedAgentCurrentStateAction(routeFactory: RouteFactory): Action[AnyContent] =
    Action.async { implicit request =>
      withAuthorisedAsAgent { (_, _) =>
        journeyService.currentState.map {
          case Some(stateAndBreadcrumbs) => routeFactory(stateAndBreadcrumbs)(request)
          case None                      => renderState(journeyService.model.root, Nil, None)(request)
        }
      }
    }

  protected final def authorisedAgentAction(transition: AuthorisedAgent => Transition)(
    routeFactory: RouteFactory): Action[AnyContent] =
    Action.async { implicit request =>
      withAuthorisedAsAgent { (arn, isWhitelisted) =>
        apply(transition(AuthorisedAgent(arn, isWhitelisted)), routeFactory)
      }
    }

  protected final def authorisedAgentActionWithHC(transition: HeaderCarrier => AuthorisedAgent => Transition)(
    routeFactory: RouteFactory): Action[AnyContent] =
    Action.async { implicit request =>
      withAuthorisedAsAgent { (arn, isWhitelisted) =>
        apply(transition(implicitly[HeaderCarrier])(AuthorisedAgent(arn, isWhitelisted)), routeFactory)
      }
    }

  private def bindForm[T](form: Form[T], transition: T => Transition)(
    implicit hc: HeaderCarrier,
    request: Request[_]): Future[Result] =
    form
      .bindFromRequest()
      .fold(
        formWithErrors =>
          journeyService.currentState.map {
            case Some((state, breadcrumbs)) =>
              renderState(state, breadcrumbs, Some(formWithErrors))(request)
            case None =>
              renderState(journeyService.model.root, Nil, None)(request)
        },
        userInput => apply(transition(userInput), redirect)
      )

  protected final def authorisedAgentActionWithForm[T](form: Form[T])(
    transition: AuthorisedAgent => T => Transition): Action[AnyContent] =
    Action.async { implicit request =>
      withAuthorisedAsAgent { (arn, isWhitelisted) =>
        bindForm(form, transition(AuthorisedAgent(arn, isWhitelisted)))
      }
    }

  protected final def authorisedAgentActionWithFormWithHC[T](form: Form[T])(
    transition: HeaderCarrier => AuthorisedAgent => T => Transition): Action[AnyContent] =
    Action.async { implicit request =>
      withAuthorisedAsAgent { (arn, isWhitelisted) =>
        bindForm(form, transition(implicitly[HeaderCarrier])(AuthorisedAgent(arn, isWhitelisted)))
      }
    }

  protected final def authorisedAgentActionWithFormWithHCWithRequest[T](form: Form[T])(
    transition: HeaderCarrier => Request[Any] => AuthorisedAgent => T => Transition): Action[AnyContent] =
    Action.async { implicit request =>
      withAuthorisedAsAgent { (arn, isWhitelisted) =>
        bindForm(
          form,
          transition(implicitly[HeaderCarrier])(implicitly[Request[Any]])(AuthorisedAgent(arn, isWhitelisted)))
      }
    }

}

object OptionalFormOps {
  implicit class OptionalForm(val formOpt: Option[Form[_]]) extends AnyVal {
    def or[T](other: Form[T]): Form[T] = formOpt.map(_.asInstanceOf[Form[T]]).getOrElse(other)
  }
}
