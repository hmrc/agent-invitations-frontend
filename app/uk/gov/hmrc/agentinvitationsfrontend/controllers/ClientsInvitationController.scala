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

import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, Request, Result}
import play.api.{Configuration, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.controllers.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models.Invitation
import uk.gov.hmrc.agentinvitationsfrontend.services.InvitationsService
import uk.gov.hmrc.agentinvitationsfrontend.views.html.clients._
import uk.gov.hmrc.agentmtdidentifiers.model.{InvitationId, MtdItId}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.{HeaderCarrier, NotFoundException, Upstream4xxResponse}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.Future

case class ConfirmForm(value: Option[Boolean])

@Singleton
class ClientsInvitationController @Inject()(@Named("personal-tax-account.external-url") continueUrl: String,
                                            invitationsService: InvitationsService,
                                            auditService: AuditService,
                                            val messagesApi: play.api.i18n.MessagesApi,
                                            val authConnector: AuthConnector,
                                            val withVerifiedPasscode: PasscodeVerification)(implicit val configuration: Configuration)
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
          withValidInvitation(clientId, invitationId, apiIdentifier)(checkInvitationIsPending { invitation =>
            invitationsService.getAgencyName(invitation.arn).flatMap { agencyName =>
              rejectInvitation(serviceName, invitationId, clientId).map {
                  case NO_CONTENT => {
                    auditService.sendAgentInvitationResponse(invitationId.value, invitation.arn, "Rejected", clientId, serviceName, agencyName)
                    Ok(invitation_declined(agencyName, invitationId, messageKey, continueUrl))
                  }
                  case status => throw new Exception(s"Invitation rejection failed with status $status")
                }
            }
          })
        }
      case InvalidService => Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
    }
  }

  def getConfirmInvitation(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(serviceName, serviceIdentifier, apiIdentifier, messageKey) =>
        withAuthorisedAsClient(serviceName, serviceIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier)(checkInvitationIsPending { invitation =>
            invitationsService.getAgencyName(invitation.arn).map {
              agencyName =>
                auditService.sendAgentInvitationResponse(invitationId.value, invitation.arn, "Accepted", clientId, serviceName, agencyName)
                Ok(confirm_invitation(confirmInvitationForm, agencyName, invitationId, messageKey))
            }
          })
        }
      case InvalidService => Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
    }
  }

  def submitConfirmInvitation(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(serviceName, serviceIdentifier, apiIdentifier, messageKey) =>
        withAuthorisedAsClient(serviceName, serviceIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier)(checkInvitationIsPending { invitation =>
            confirmInvitationForm.bindFromRequest().fold(
              formWithErrors => {
                invitationsService.getAgencyName(invitation.arn).map(name => Ok(confirm_invitation(formWithErrors, name, invitationId, messageKey)))
              }, data => {
                val result = if (data.value.getOrElse(false))
                  Redirect(routes.ClientsInvitationController.getConfirmTerms(invitationId))
                else
                  Redirect(routes.ClientsInvitationController.getInvitationDeclined(invitationId))

                Future successful result
              })
          })
        }
      case InvalidService => Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
    }
  }

  def getConfirmTerms(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(serviceName, serviceIdentifier, apiIdentifier, (messageKey)) =>
        withAuthorisedAsClient(serviceName, serviceIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier)(checkInvitationIsPending {
            invitation: Invitation =>
              invitationsService.getAgencyName(invitation.arn).map(name => Ok(confirm_terms(confirmTermsForm, name, invitationId, messageKey)))
          })
        }
      case InvalidService => Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
    }
  }

  def submitConfirmTerms(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(serviceName, serviceIdentifier, apiIdentifier, messageKey) =>
        withAuthorisedAsClient(serviceName, serviceIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier)(checkInvitationIsPending { invitation =>
            confirmTermsForm.bindFromRequest().fold(
              formWithErrors => {
                invitationsService.getAgencyName(invitation.arn).map(name => Ok(confirm_terms(formWithErrors, name, invitationId, messageKey)))
              }, _ => {
                acceptInvitation(serviceName, invitationId, clientId).map {
                  case NO_CONTENT => Redirect(routes.ClientsInvitationController.getCompletePage(invitationId))
                  case status => throw new Exception(s"Invitation acceptance failed with status $status")
                }
              })
          })
        }
      case InvalidService => Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
    }
  }

  def getCompletePage(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(serviceName, serviceIdentifier, apiIdentifier, messageKey) =>
        withAuthorisedAsClient(serviceName, serviceIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier)(checkInvitationIsAccepted { invitation =>
            invitationsService.getAgencyName(invitation.arn).map(name =>
              Ok(complete(name, messageKey, continueUrl)))
          })
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

  private def acceptInvitation(service: String, invitationId: InvitationId, clientId: String)(implicit hc: HeaderCarrier): Future[Int] = {
    service match {
      case "HMRC-MTD-IT" => invitationsService.acceptITSAInvitation(invitationId, MtdItId(clientId))
      case "HMRC-NI" => invitationsService.acceptAFIInvitation(invitationId, Nino(clientId))
      case _ => throw new IllegalStateException("Unsupported Service")
    }
  }

  private def rejectInvitation(service: String, invitationId: InvitationId, clientId: String)(implicit hc: HeaderCarrier): Future[Int] = {
    service match {
      case "HMRC-MTD-IT" => invitationsService.rejectITSAInvitation(invitationId, MtdItId(clientId))
      case "HMRC-NI" => invitationsService.rejectAFIInvitation(invitationId, Nino(clientId))
      case _ => throw new IllegalStateException("Unsupported Service")
    }
  }

  def checkInvitationIsPending(f: Invitation => Future[Result]): Invitation => Future[Result] = {
    case invitation if invitation.status.contains("Pending") =>
      f(invitation)
    case invitation if invitation.status.contains("Expired") =>
      Future successful Redirect(routes.ClientsInvitationController.invitationExpired())
    case _ =>
      Future successful Redirect(routes.ClientsInvitationController.invitationAlreadyResponded())
  }

  def checkInvitationIsAccepted(f: Invitation => Future[Result]): Invitation => Future[Result] = {
    case invitation if invitation.status.contains("Accepted") =>
      f(invitation)
    case _ =>
      Future successful Redirect(routes.ClientsInvitationController.invitationAlreadyResponded())
  }

  private def withValidInvitation[A](clientId: String, invitationId: InvitationId, apiIdentifier: String)
                                    (body: Invitation => Future[Result])(implicit request: Request[A], hc: HeaderCarrier): Future[Result] = {
    invitationsService.getClientInvitation(clientId, invitationId, apiIdentifier)
      .flatMap(body)
      .recover {
        case ex: Upstream4xxResponse if ex.message.contains("NO_PERMISSION_ON_CLIENT") =>
          Logger.warn(s"${invitationId.value} Has been access by the wrong Client.")
          Redirect(routes.ClientsInvitationController.incorrectInvitation())
        case ex: Upstream4xxResponse if ex.message.contains("INVALID_INVITATION_STATUS") =>
          Logger.warn(s"${invitationId.value} Has already been responded.")
          Redirect(routes.ClientsInvitationController.invitationAlreadyResponded())
        case ex: Upstream4xxResponse if ex.message.contains("INVITATION_NOT_FOUND") =>
          Logger.warn(s"${invitationId.value} is not found.")
          Redirect(routes.ClientsInvitationController.notFoundInvitation())
        case _: NotFoundException =>
          Logger.warn(s"${invitationId.value} is not found.")
          Redirect(routes.ClientsInvitationController.notFoundInvitation())
      }
  }
}

object ClientsInvitationController {

  val invitationChoice: Constraint[Option[Boolean]] = Constraint[Option[Boolean]] { fieldValue: Option[Boolean] =>
    if (fieldValue.isDefined)
      Valid
    else
      Invalid(ValidationError("error.confirmInvite.invalid"))
  }

  val termsChoice: Constraint[Option[Boolean]] = Constraint[Option[Boolean]] { fieldValue: Option[Boolean] =>
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