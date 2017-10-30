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

import javax.inject.{Inject, Singleton}

import play.api.Configuration
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.agentinvitationsfrontend.form.NinoForm.ninoForm
import uk.gov.hmrc.agentinvitationsfrontend.form.PostcodeForm.postCodeForm
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.agentinvitationsfrontend.views.html
import uk.gov.hmrc.auth.core.AuthProvider.GovernmentGateway
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.Retrievals.authorisedEnrolments

import scala.concurrent.Future

@Singleton
class InvitationsController @Inject()(val messagesApi: play.api.i18n.MessagesApi,
                                      val authConnector: AuthConnector)(implicit val configuration: Configuration)
  extends FrontendController with I18nSupport with AuthorisedFunctions {

  def enterNino: Action[AnyContent] = Action.async { implicit request =>
    authorised(AffinityGroup.Agent and Enrolment("HMRC-AS-AGENT") and AuthProviders(GovernmentGateway)).retrieve(authorisedEnrolments) {
      enrolments => Future successful Ok(html.agents.enter_nino(ninoForm))
    }
  }

  def submitNino: Action[AnyContent] = Action.async { implicit request =>
    ninoForm.bindFromRequest().fold(
      formWithErrors => {
        Future successful Ok(html.agents.enter_nino(formWithErrors))
      },
      nino => Future successful Redirect(routes.InvitationsController.enterPostcode())
    )
  }

  def enterPostcode: Action[AnyContent] = Action.async { implicit request =>
    Future successful Ok(html.agents.enter_postcode(postCodeForm))
  }

  def submitPostcode: Action[AnyContent] = Action.async { implicit request =>
    postCodeForm.bindFromRequest().fold(
      formWithErrors => {
        Future successful Ok(html.agents.enter_postcode(formWithErrors))
      },
      postcode => Future successful Redirect(routes.InvitationsController.confirmInvitation())
    )
  }

  def confirmInvitation: Action[AnyContent] = Action.async { implicit request =>
    Future successful Ok(html.agents.confirm_invitation())
  }
}
