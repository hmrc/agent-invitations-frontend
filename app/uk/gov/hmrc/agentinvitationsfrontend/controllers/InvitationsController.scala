/*
 * Copyright 2017 HM Revenue & Customs
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

import javax.inject.{Inject, Named, Singleton}

import play.api.Configuration
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.agentinvitationsfrontend.form.NinoForm.ninoForm
import uk.gov.hmrc.agentinvitationsfrontend.form.PostcodeForm.postCodeForm
import uk.gov.hmrc.agentinvitationsfrontend.views.html
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future

@Singleton
class InvitationsController @Inject()(@Named("company-auth.login-url") ggLoginUrl: String,
                                      @Named("agent-invitations-frontend.start-url") startInvitationsUrl: String,
                                      @Named("appName") originUrl: String,
                                      val messagesApi: play.api.i18n.MessagesApi,
                                      val authConnector: AuthConnector)(implicit val configuration: Configuration)
  extends FrontendController with I18nSupport with AuthActions {

  def enterNino: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
        Future successful Ok(html.agents.enter_nino(ninoForm))
    }
  }

  def submitNino: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      ninoForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(html.agents.enter_nino(formWithErrors))
        },
        nino =>
          Future successful Redirect(routes.InvitationsController.enterPostcode()).addingToSession("USER_NINO" -> s"$nino")
      )
    }
  }

  def enterPostcode: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
        Future successful Ok(html.agents.enter_postcode(postCodeForm))
    }
  }

  def submitPostcode: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      postCodeForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(html.agents.enter_postcode(formWithErrors))
        },
        postcode =>
          Future successful Redirect(routes.InvitationsController.confirmInvitation())
      )
    }
  }


  def confirmInvitation: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      Future successful Ok(html.agents.confirm_invitation())
    }
  }
}
