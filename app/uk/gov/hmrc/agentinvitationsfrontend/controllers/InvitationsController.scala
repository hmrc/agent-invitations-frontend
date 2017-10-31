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

import play.api.{Configuration, Logger}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, Result}
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentInvitation, AgentInvitationUserInput}
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentInvitationsForm.{agentInvitationNinoForm, agentInvitationPostCodeForm}
import uk.gov.hmrc.agentinvitationsfrontend.services.InvitationsService
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future

@Singleton
class InvitationsController @Inject()(@Named("company-auth.login-url") ggLoginUrl: String,
                                      @Named("agent-invitations-frontend.start-url") startInvitationsUrl: String,
                                      @Named("appName") originUrl: String,
                                      @Named("agent-invitations-frontend.external-url") externalUrl: String,
                                      invitationsService: InvitationsService,
                                      val messagesApi: play.api.i18n.MessagesApi,
                                      val authConnector: AuthConnector)(implicit val configuration: Configuration)
  extends FrontendController with I18nSupport with AuthActions {

  def enterNino: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
        Future successful Ok(enter_nino(agentInvitationNinoForm))
    }
  }

  def submitNino: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      agentInvitationNinoForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(enter_nino(formWithErrors))
        },
        userInput => {
          Logger.info(s"User Details $userInput")
          Future successful Ok(enter_postcode(agentInvitationNinoForm.fill(userInput)))
        }
      )
    }
  }

  def submitPostcode: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      agentInvitationPostCodeForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(enter_postcode(formWithErrors))
        },
        userInput => {
          Logger.info(s"User Details $userInput")
//          createInvitation(arn, userInput)
          Future successful Ok(confirm_invitation("localhost:9999/yourinvitation.com"))
        }
      )
    }
  }

//  private def createInvitation(arn: Arn, agentInvitation: AgentInvitationUserInput): Future[Result] = {
//    invitationsService.createInvitation(arn, agentInvitation).map {
//      case Some(invitation) => Ok(confirm_invitation())
//      case None => NotImplemented
//    }
//  }

//  private def clientInvitationUrl(id: String) = {
//    s"$externalUrl${routes.InvitationsController.viewInvitation(id)}"
//  }

  private def extractInvitationId(url: String) = url.substring(url.lastIndexOf("/") + 1)


  def confirmInvitation(invitationUrl: String): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      Future successful Ok(confirm_invitation(invitationUrl))
    }
  }


}
