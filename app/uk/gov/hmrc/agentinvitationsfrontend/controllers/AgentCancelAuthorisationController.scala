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

import javax.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Form
import play.api.mvc.{Action, AnyContent, Request}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.forms.{ClientTypeForm, ServiceTypeForm}
import uk.gov.hmrc.agentinvitationsfrontend.models.CancelAuthorisationRequest
import uk.gov.hmrc.agentinvitationsfrontend.services.CancelAuthorisationCache
import uk.gov.hmrc.agentinvitationsfrontend.util.toFuture
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.cancelAuthorisation.{client_type, select_service}
import uk.gov.hmrc.auth.core.AuthConnector

@Singleton
class AgentCancelAuthorisationController @Inject()(
  withVerifiedPasscode: PasscodeVerification,
  authConnector: AuthConnector,
  val cancelAuthorisationCache: CancelAuthorisationCache,
  featureFlags: FeatureFlags)(
  implicit externalUrls: ExternalUrls,
  messagesApi: play.api.i18n.MessagesApi,
  configuration: Configuration)
    extends BaseController(withVerifiedPasscode, authConnector, featureFlags) {

  def showClientType: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      Ok(client_type(ClientTypeForm.form, clientTypes))
    }
  }

  def submitClientType: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      ClientTypeForm.form
        .bindFromRequest()
        .fold(
          formWithErrors => Ok(client_type(formWithErrors, clientTypes)),
          userInput => {
            val updateAggregate = cancelAuthorisationCache.fetch
              .map(_.getOrElse(CancelAuthorisationRequest()))
              .map(_.copy(clientType = Some(userInput)))

            updateAggregate.flatMap(
              update =>
                cancelAuthorisationCache
                  .save(update)
                  .map(_ => Redirect(routes.AgentCancelAuthorisationController.showSelectService())))
          }
        )
    }
  }

  def showSelectService: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, isWhitelisted) =>
      getSelectServicePage(isWhitelisted, ServiceTypeForm.form)
    }
  }

  private def getSelectServicePage(isWhitelisted: Boolean, form: Form[String])(implicit request: Request[_]) =
    cancelAuthorisationCache.fetch.flatMap(_.flatMap(_.clientType)).flatMap {
      case Some("personal") => Ok(select_service(form, enabledPersonalServicesForCancelAuth(isWhitelisted)))
      case Some("business") => Ok(select_service(form, enabledBusinessServicesForCancelAuthorisation))
      case _                => Redirect(routes.AgentCancelAuthorisationController.showClientType().url)
    }

  def submitSelectService: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, isWhitelisted) =>
      ServiceTypeForm.form
        .bindFromRequest()
        .fold(
          formWithErrors => getSelectServicePage(isWhitelisted, formWithErrors),
          userInput => {
            val updateAggregate = cancelAuthorisationCache.fetch
              .map(_.getOrElse(CancelAuthorisationRequest()))
              .map(_.copy(service = Some(userInput)))

            updateAggregate.flatMap(
              update =>
                cancelAuthorisationCache
                  .save(update)
                  .map(_ => Redirect(routes.AgentCancelAuthorisationController.showClientType())))
          }
        )
    }
  }

}
