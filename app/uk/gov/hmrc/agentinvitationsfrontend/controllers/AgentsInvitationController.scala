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
import play.api.data.Form
import play.api.data.Forms.{ mapping, text }
import play.api.i18n.I18nSupport
import play.api.mvc.{ Action, AnyContent }
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentInvitationUserInput
import uk.gov.hmrc.agentinvitationsfrontend.services.InvitationsService
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.domain.Nino.isValid
import uk.gov.hmrc.http.Upstream4xxResponse
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future

@Singleton
class AgentInvitationsController @Inject() (
  invitationsService: InvitationsService,
  val messagesApi: play.api.i18n.MessagesApi,
  val authConnector: AuthConnector)(implicit val configuration: Configuration)
  extends FrontendController with I18nSupport with AuthActions {

  import AgentInvitationsController._

  def agentsRoot: Action[AnyContent] = Action { implicit request =>
    Redirect(routes.AgentInvitationsController.enterNino().url)
  }

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
          Future successful Ok(enter_postcode(agentInvitationNinoForm.fill(userInput)))
        })
    }
  }

  def submitPostcode: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      agentInvitationPostCodeForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(enter_postcode(formWithErrors))
        },
        userInput => {
          invitationsService
            .createInvitation(arn, userInput)
            .map(invitation =>
              Ok(invitation_sent(invitation.selfUrl.toString)))
            .recoverWith {
              case noMtdItId: Upstream4xxResponse if noMtdItId.message.contains("CLIENT_REGISTRATION_NOT_FOUND") =>
                Future.successful(Forbidden(not_enrolled()))
              case noPostCode: Upstream4xxResponse if noPostCode.message.contains("POSTCODE_DOES_NOT_MATCH") =>
                Future.successful(Forbidden(no_match()))
            }
        })
    }
  }
}

object AgentInvitationsController {

  private def postcodeCheck(postcode: String) =
    postcode.matches("^[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}$|BFPO\\s?[0-9]{1,5}$")

  val agentInvitationNinoForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "nino" -> text
        .verifying("enter-nino.error-empty", _.nonEmpty)
        .verifying("enter-nino.invalid-format", nino => isValid(nino)),
      "postcode" -> text)({ (nino, postcode) => AgentInvitationUserInput(Nino(nino), postcode) })({ user => Some((user.nino.value, user.postcode)) }))
  }

  val agentInvitationPostCodeForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "nino" -> text,
      "postcode" -> text
        .verifying("enter-postcode.error-empty", _.nonEmpty)
        .verifying("enter-postcode.invalid-format", postcode => postcodeCheck(postcode)))({ (nino, postcode) => AgentInvitationUserInput(Nino(nino), postcode) })({ user => Some((user.nino.value, user.postcode)) }))
  }

}
