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
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, Request, Result}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.controllers.Services._
import uk.gov.hmrc.agentinvitationsfrontend.services.InvitationsService
import uk.gov.hmrc.agentinvitationsfrontend.views.html.clients._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId}
import uk.gov.hmrc.auth.core.{AuthConnector, InsufficientEnrolments}
import uk.gov.hmrc.http.{HeaderCarrier, Upstream4xxResponse}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.Future

case class ConfirmForm(value: Option[Boolean])

@Singleton
class ClientsInvitationController @Inject()(invitationsService: InvitationsService,
                                            auditService: AuditService,
                                            val messagesApi: play.api.i18n.MessagesApi,
                                            val authConnector: AuthConnector)(implicit val configuration: Configuration)
  extends FrontendController with I18nSupport with AuthActions {

  import ClientsInvitationController._

  def start(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineServices(invitationId) match {
      case ValidService(value) if value.nonEmpty => Future successful Ok(landing_page(invitationId, value))
      case _ => Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
    }
  }

  def submitStart(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    Future successful Redirect(routes.ClientsInvitationController.getConfirmInvitation(invitationId))
  }

  def getInvitationDeclined(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsClient { mtdItId =>
      withValidInvitation(mtdItId, invitationId) { arn =>
        auditService.sendAgentInvitationResponse(invitationId.value, arn, "Declined", mtdItId)

        for {
          name <- invitationsService.getAgencyName(arn)
          _ <- invitationsService.rejectInvitation(invitationId, mtdItId)
        } yield Ok(invitation_declined(name, invitationId))
      }
    }
  }

  def getConfirmInvitation(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsClient { mtdItId =>
      withValidInvitation(mtdItId, invitationId) { arn =>
        auditService.sendAgentInvitationResponse(invitationId.value, arn, "Accepted", mtdItId)
        invitationsService.getAgencyName(arn).map { name =>
          Ok(confirm_invitation(confirmInvitationForm, name, invitationId, determineServices(invitationId).value))
        }
      }
    }.recoverWith {
      case _: InsufficientEnrolments =>
        Future successful Redirect(routes.ClientsInvitationController.notSignedUp())
    }
  }

  def submitConfirmInvitation(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsClient { mtdItId =>
      withValidInvitation(mtdItId, invitationId) { arn =>
        confirmInvitationForm.bindFromRequest().fold(
          formWithErrors => {
            invitationsService.getAgencyName(arn).map(name => Ok(confirm_invitation(formWithErrors, name, invitationId, determineServices(invitationId).value)))
          }, data => {
            val result = if (data.value.getOrElse(false))
                           Redirect(routes.ClientsInvitationController.getConfirmTerms(invitationId))
                         else
                           Redirect(routes.ClientsInvitationController.getInvitationDeclined(invitationId))

            Future successful result
          })
      }
    }
  }

  def getConfirmTerms(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsClient { mtdItId =>
      withValidInvitation(mtdItId, invitationId) { arn =>
        invitationsService.getAgencyName(arn).map(name => Ok(confirm_terms(confirmTermsForm, name, invitationId)))
      }
    }
  }

  def submitConfirmTerms(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsClient { mtdItId =>
      withValidInvitation(mtdItId, invitationId) { arn =>
        confirmTermsForm.bindFromRequest().fold(
          formWithErrors => {
            invitationsService.getAgencyName(arn).map(name => Ok(confirm_terms(formWithErrors, name, invitationId)))
          }, _ => {
            invitationsService.acceptInvitation(invitationId, mtdItId).map { _ =>
              Redirect(routes.ClientsInvitationController.getCompletePage(invitationId))
            }
          })
      }
    }
  }

  def getCompletePage(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsClient { mtdItId =>
      invitationsService.getClientInvitation(mtdItId, invitationId).flatMap {
        case Some(invitation) => invitationsService.getAgencyName(invitation.arn).map(name => Ok(complete(name)))
        case None => Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
      } recover {
        case ex: Upstream4xxResponse if ex.message.contains("NO_PERMISSION_ON_CLIENT") =>
          Redirect(routes.ClientsInvitationController.incorrectInvitation())
        case ex: Upstream4xxResponse if ex.message.contains("INVALID_INVITATION_STATUS") =>
          Redirect(routes.ClientsInvitationController.invitationAlreadyResponded())
        case ex: Upstream4xxResponse if ex.message.contains("INVITATION_NOT_FOUND") =>
          Redirect(routes.ClientsInvitationController.notFoundInvitation())
      }
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

  def invitationExpired: Action[AnyContent] = Action.async { implicit request =>
    Future successful Ok(invitation_expired())
  }

  private def withValidInvitation[A](mtdItId: MtdItId, invitationId: InvitationId)(f: Arn => Future[Result])(implicit request: Request[A], hc: HeaderCarrier): Future[Result] = {
    invitationsService.getClientInvitation(mtdItId, invitationId).flatMap {
      case Some(invitation) if invitation.status.contains("Expired") =>
        Future successful Redirect(routes.ClientsInvitationController.invitationExpired())
      case Some(invitation) if !invitation.status.contains("Pending") =>
        Future successful Redirect(routes.ClientsInvitationController.invitationAlreadyResponded())
      case Some(invitation) =>
        f(invitation.arn)
      case None =>
        Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
    } recover {
      case ex: Upstream4xxResponse if ex.message.contains("NO_PERMISSION_ON_CLIENT") =>
        Redirect(routes.ClientsInvitationController.incorrectInvitation())
      case ex: Upstream4xxResponse if ex.message.contains("INVALID_INVITATION_STATUS") =>
        Redirect(routes.ClientsInvitationController.invitationAlreadyResponded())
      case ex: Upstream4xxResponse if ex.message.contains("INVITATION_NOT_FOUND") =>
        Redirect(routes.ClientsInvitationController.notFoundInvitation())
    }
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