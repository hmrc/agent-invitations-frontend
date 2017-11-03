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

import javax.inject.{ Inject, Singleton }

import play.api.Configuration
import play.api.i18n.I18nSupport
import play.api.mvc.{ Action, AnyContent }
import uk.gov.hmrc.agentinvitationsfrontend.views.html.clients._
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future

@Singleton
class ClientsInvitationController @Inject() (val messagesApi: play.api.i18n.MessagesApi)(implicit val configuration: Configuration)
  extends FrontendController with I18nSupport {

  def start(token: String): Action[AnyContent] = Action.async { implicit request =>
    Future successful Ok(landing_page())
  }

  def submitStart: Action[AnyContent] = Action.async { implicit request =>
    Future successful Redirect(routes.ClientsInvitationController.getConfirmInvitation())
  }

  def getConfirmInvitation: Action[AnyContent] = Action.async { implicit request =>
    Future successful Ok(confirm_invitation())
  }

  def submitConfirmInvitation: Action[AnyContent] = Action.async { implicit request =>
    Future successful Redirect(routes.ClientsInvitationController.getConfirmTerms())
  }

  def getConfirmTerms: Action[AnyContent] = Action.async { implicit request =>
    Future successful Ok(confirm_terms())
  }

  def submitConfirmTerms: Action[AnyContent] = Action.async { implicit request =>
    Future successful Redirect(routes.ClientsInvitationController.getCompletePage())
  }

  def getCompletePage: Action[AnyContent] = Action.async { implicit request =>
    Future successful Ok(complete())
  }

}