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

import play.api.{Configuration, Logger}
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.i18n.{I18nSupport, Messages}
import play.api.i18n.Messages.Message
import play.api.mvc.{Action, AnyContent}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.services.InvitationsService
import uk.gov.hmrc.agentinvitationsfrontend.views.html.clients._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.error_template
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.http.Upstream4xxResponse
import uk.gov.hmrc.auth.core.{AuthConnector, InsufficientEnrolments}
import uk.gov.hmrc.http.Upstream4xxResponse
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future

case class ConfirmForm(value: Option[Boolean])

@Singleton
class ClientsInvitationController @Inject()(invitationsService: InvitationsService,
                                             auditService: AuditService,
                                             val messagesApi: play.api.i18n.MessagesApi,
                                             val authConnector: AuthConnector)(implicit val configuration: Configuration)
  extends FrontendController with I18nSupport with AuthActions {

  import ClientsInvitationController._

  def start(invitationId: String): Action[AnyContent] = Action.async { implicit request =>
    Future successful Ok(landing_page()).withSession(request.session + (("invitationId", invitationId)))
  }

  def submitStart: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsClient { mtdItId =>
      request.session.get("invitationId") match {
        case Some(invitationId) =>
          invitationsService.getClientInvitation(mtdItId, invitationId) map {
            case Some(invitation) =>
              auditService.sendAgentInvitationResponse(invitationId, invitation.arn, "Accepted", mtdItId)
              if (!invitation.status.contains("Pending"))
                Redirect(routes.ClientsInvitationController.invitationAlreadyResponded())
              else Redirect(routes.ClientsInvitationController.getConfirmInvitation())
            case None =>
              Redirect(routes.ClientsInvitationController.notFoundInvitation())
          } recoverWith {
            case ex: Upstream4xxResponse if ex.message.contains("NO_PERMISSION_ON_CLIENT") =>
              Future successful Redirect(routes.ClientsInvitationController.incorrectInvitation())
          }
        case None =>
          Future successful Redirect(routes.ClientsInvitationController.incorrectInvitation())
      }
    }.recoverWith {
      case _: InsufficientEnrolments =>
        Future successful Redirect(routes.ClientsInvitationController.notSignedUp())
    }
  }

  def getInvitationDeclined: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsClient { mtdItId =>
      invitationsService.rejectInvitation(request.session.get("invitationId").getOrElse(""), mtdItId).map { _ =>
        Ok(invitation_declined())
      } recover {
        case ex: Upstream4xxResponse if ex.message.contains("INVALID_INVITATION_STATUS") =>
          Redirect(routes.ClientsInvitationController.invitationAlreadyResponded)
        case _ =>
          val title = Messages("global.error.500.heading")
          Ok(error_template(title, title, Messages("global.error.500.message")))
      }
    }
  }

  def getConfirmInvitation: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsClient { mtdItId =>
      Future successful Ok(confirm_invitation(confirmInvitationForm))
    }
  }

  def submitConfirmInvitation: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsClient { mtdItId =>
      confirmInvitationForm.bindFromRequest().fold(
        formWithErrors => {
          Future.successful(Ok(confirm_invitation(formWithErrors)))
        }, data => {
          val result = if (data.value.getOrElse(false))
            Redirect(routes.ClientsInvitationController.getConfirmTerms())
          else
            Redirect(routes.ClientsInvitationController.getInvitationDeclined())

          Future.successful(result)
        })
    }
  }

  def getConfirmTerms: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsClient { _ =>
      Future successful Ok(confirm_terms(confirmTermsForm))
    }
  }

  def submitConfirmTerms: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsClient { mtdItId =>
      confirmTermsForm.bindFromRequest().fold(
        formWithErrors => {
          Future.successful(Ok(confirm_terms(formWithErrors)))
        }, data => {
          if (data.value.getOrElse(false)) {
            val invitationId = request.session.get("invitationId").getOrElse("")
            invitationsService.acceptInvitation(mtdItId,invitationId).map { _ =>
              Redirect(routes.ClientsInvitationController.getCompletePage())
            }
          }
          else
            Future.successful(NotImplemented) //TODO - should we ever actually get Some(false) ?
        })
    }
  }

  def getCompletePage: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsClient { _ =>
      Future successful Ok(complete())
    }
  }

  def notSignedUp: Action[AnyContent] = Action.async { implicit request =>
    Future successful Forbidden(not_signed_up())
  }

  def incorrectInvitation: Action[AnyContent] = Action.async { implicit request =>
    Future successful Forbidden(incorrect_invitation())
  }

  def notFoundInvitation: Action[AnyContent] = Action.async { implicit request =>
    Future successful NotFound(not_found_invitation())
  }

  def invitationAlreadyResponded: Action[AnyContent] = Action.async { implicit request =>
    Future successful Forbidden(invitation_already_responded())
  }

}

object ClientsInvitationController {
  def invitationChoice: Constraint[Option[Boolean]] = Constraint[Option[Boolean]] { fieldValue: Option[Boolean] =>
    if (fieldValue.isDefined)
      Valid
    else
      Invalid(ValidationError("error.confirmInvite.invalid"))
  }

  def termsChoice: Constraint[Option[Boolean]] = Constraint[Option[Boolean]] { fieldValue: Option[Boolean] =>
    fieldValue match {
      case Some(true) => Valid
      case _ => Invalid(ValidationError("error.confirmTerms.invalid"))
    }
  }

  val confirmInvitationForm: Form[ConfirmForm] = Form[ConfirmForm](
    mapping("confirmInvite" -> optional(boolean)
      .verifying(invitationChoice))(ConfirmForm.apply)(ConfirmForm.unapply))

  val confirmTermsForm: Form[ConfirmForm] = Form[ConfirmForm](
    mapping("confirmTerms" -> optional(boolean)
      .verifying(termsChoice))(ConfirmForm.apply)(ConfirmForm.unapply))
}