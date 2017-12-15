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
import uk.gov.hmrc.domain.Nino
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
    determineService(invitationId) match {
      case ValidService(_, _, _, messageKey) if messageKey.nonEmpty => Future successful Ok(landing_page(invitationId, messageKey))
      case _ => Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
    }
  }

  def submitStart(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    Future successful Redirect(routes.ClientsInvitationController.getConfirmInvitation(invitationId))
  }

  def getInvitationDeclined(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(serviceName, serviceIdentifier, apiIdentifier, messageKey) =>
        withAuthorisedAsClient(serviceName, serviceIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier) { arn =>
            invitationsService.getAgencyName(arn).flatMap { agencyName =>
              auditService.sendAgentInvitationResponse(invitationId.value, arn, "Declined", clientId, serviceName, agencyName)
              for {
                name <- invitationsService.getAgencyName(arn)
                _ <- rejectInvitation(serviceName, invitationId, clientId)
              } yield Ok(invitation_declined(name, invitationId))
            }
          }
        }
      case InvalidService => Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
    }
  }

  def getConfirmInvitation(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(serviceName, serviceIdentifier, apiIdentifier, messageKey) =>
        withAuthorisedAsClient(serviceName, serviceIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier) {
            arn =>
              invitationsService.getAgencyName(arn).map {
                agencyName =>
                auditService.sendAgentInvitationResponse(invitationId.value, arn, "Accepted", clientId, serviceName, agencyName)
                  Ok(confirm_invitation(confirmInvitationForm, agencyName, invitationId, messageKey))
              }
          }
        }.recoverWith {
          case _: InsufficientEnrolments =>
            Future successful Redirect(routes.ClientsInvitationController.notSignedUp())
        }
      case InvalidService => Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
    }
  }

  def submitConfirmInvitation(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(serviceName, serviceIdentifier, apiIdentifier, messageKey) =>
        withAuthorisedAsClient(serviceName, serviceIdentifier) { clientId =>
            withValidInvitation(clientId, invitationId, apiIdentifier) {
              arn =>
                confirmInvitationForm.bindFromRequest().fold(
                  formWithErrors => {
                    invitationsService.getAgencyName(arn).map(name => Ok(confirm_invitation(formWithErrors, name, invitationId, messageKey)))
                  }, data => {
                    val result = if (data.value.getOrElse(false))
                      Redirect(routes.ClientsInvitationController.getConfirmTerms(invitationId))
                    else
                      Redirect(routes.ClientsInvitationController.getInvitationDeclined(invitationId))

                    Future successful result
                  })
            }
        }
      case InvalidService => Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
    }
  }

  def getConfirmTerms(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(serviceName, serviceIdentifier, apiIdentifier, (messageKey)) =>
        withAuthorisedAsClient(serviceName, serviceIdentifier) { clientId =>
            withValidInvitation(clientId, invitationId, apiIdentifier) {
              arn =>
                invitationsService.getAgencyName(arn).map(name => Ok(confirm_terms(confirmTermsForm, name, invitationId, messageKey)))
            }
        }
      case InvalidService => Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
    }
  }

  def submitConfirmTerms(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(serviceName, serviceIdentifier, apiIdentifier, messageKey) =>
      withAuthorisedAsClient(serviceName, serviceIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier) {
            arn =>
              confirmTermsForm.bindFromRequest().fold(
                formWithErrors => {
                  invitationsService.getAgencyName(arn).map(name => Ok(confirm_terms(formWithErrors, name, invitationId, messageKey)))
                }, _ => {
                  acceptInvitation(serviceName, invitationId, clientId).map { _ =>
                      Redirect(routes.ClientsInvitationController.getCompletePage(invitationId))
                  }
                })
          }
      }
    case InvalidService => Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
    }
  }

  def getCompletePage(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(serviceName, serviceIdentifier, apiIdentifier, messageKey) =>
      withAuthorisedAsClient(serviceName, serviceIdentifier) { clientId =>
          invitationsService.getClientInvitation(clientId, invitationId, apiIdentifier).flatMap {
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
      case InvalidService => Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
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

  private def acceptInvitation(service: String, invitationId: InvitationId, clientId: String)(implicit hc: HeaderCarrier) = {
    service match {
      case "HMRC-MTD-IT" => invitationsService.acceptITSAInvitation(invitationId, MtdItId(clientId))
      case "HMRC-NI" => invitationsService.acceptAFIInvitation(invitationId, Nino(clientId))
      case _ => throw new IllegalStateException("Unsupported Service")
    }
  }

  private def rejectInvitation(service: String, invitationId: InvitationId, clientId: String)(implicit hc: HeaderCarrier) = {
    service match {
      case "HMRC-MTD-IT" => invitationsService.rejectITSAInvitation(invitationId, MtdItId(clientId))
      case "HMRC-NI" => invitationsService.rejectAFIInvitation(invitationId, Nino(clientId))
      case _ => throw new IllegalStateException("Unsupported Service")
    }
  }

  private def withValidInvitation[A](clientId: String, invitationId: InvitationId, apiIdentifier: String)(f: Arn => Future[Result])(implicit request: Request[A], hc: HeaderCarrier): Future[Result] = {
    invitationsService.getClientInvitation(clientId, invitationId, apiIdentifier).flatMap {
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