/*
 * Copyright 2018 HM Revenue & Customs
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
import uk.gov.hmrc.agentinvitationsfrontend.connectors.AgencyNameNotFound
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.controllers.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models.Invitation
import uk.gov.hmrc.agentinvitationsfrontend.services.InvitationsService
import uk.gov.hmrc.agentinvitationsfrontend.views.html.clients._
import uk.gov.hmrc.agentmtdidentifiers.model.{InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.{HeaderCarrier, NotFoundException, Upstream4xxResponse}
import uk.gov.hmrc.play.bootstrap.controller.{ActionWithMdc, FrontendController}

import scala.concurrent.Future

case class ConfirmForm(value: Option[Boolean])

@Singleton
class ClientsInvitationController @Inject()(invitationsService: InvitationsService,
                                            auditService: AuditService,
                                            val messagesApi: play.api.i18n.MessagesApi,
                                            val authConnector: AuthConnector,
                                            val withVerifiedPasscode: PasscodeVerification)
                                           (implicit val configuration: Configuration, val externalUrls: ExternalUrls)
  extends FrontendController with I18nSupport with AuthActions {

  import ClientsInvitationController._

  def start(invitationId: InvitationId): Action[AnyContent] = ActionWithMdc { implicit request =>
    determineService(invitationId) match {
      case ValidService(_, _, _, _, messageKey) if messageKey.nonEmpty => Ok(landing_page(invitationId, messageKey))
      case _ => Redirect(routes.ClientsInvitationController.notFoundInvitation())
    }
  }

  def submitStart(invitationId: InvitationId): Action[AnyContent] = ActionWithMdc { implicit request =>
    Redirect(routes.ClientsInvitationController.getConfirmInvitation(invitationId))
  }

  def getInvitationDeclined(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(serviceName, enrolmentName, enrolmentIdentifier, apiIdentifier, messageKey) =>
        withAuthorisedAsClient(enrolmentName, enrolmentIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier)(checkInvitationIsPending { invitation =>
            invitationsService.getAgencyName(invitation.arn).flatMap { agencyName =>
              rejectInvitation(serviceName, invitationId, clientId).map {
                case NO_CONTENT => {
                  auditService.sendAgentInvitationResponse(invitationId.value, invitation.arn, "Declined", clientIdentifierType(invitation.clientId), clientId, serviceName, agencyName)
                  Ok(invitation_declined(agencyName, invitationId, messageKey))
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
      case ValidService(_, enrolmentName, enrolmentIdentifier, apiIdentifier, messageKey) =>
        withAuthorisedAsClient(enrolmentName, enrolmentIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier)(checkInvitationIsPending { invitation =>
            invitationsService.getAgencyName(invitation.arn).map { agencyName =>
                Ok(confirm_invitation(confirmInvitationForm, agencyName, invitationId, messageKey)).addingToSession("agencyName" -> agencyName)
            }
          })
        }
      case InvalidService => Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
    }
  }

  def submitConfirmInvitation(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(_, enrolmentName, enrolmentIdentifier, apiIdentifier, messageKey) =>
        withAuthorisedAsClient(enrolmentName, enrolmentIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier)(checkInvitationIsPending { invitation =>
            confirmInvitationForm.bindFromRequest().fold(
              formWithErrors => {
                val name = request.session.get("agencyName").getOrElse(throw AgencyNameNotFound())
                  Future successful Ok(confirm_invitation(formWithErrors, name, invitationId, messageKey))
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
      case ValidService(_, enrolmentName, enrolmentIdentifier, apiIdentifier, messageKey) =>
        withAuthorisedAsClient(enrolmentName, enrolmentIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier)(checkInvitationIsPending { _ =>
              val name = request.session.get("agencyName").getOrElse(throw AgencyNameNotFound())
              Future successful Ok(confirm_terms(confirmTermsForm, name, invitationId, messageKey))
          })
        }
      case InvalidService => Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
    }
  }

  def submitConfirmTerms(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(serviceName, enrolmentName, enrolmentIdentifier, apiIdentifier, messageKey) =>
        withAuthorisedAsClient(enrolmentName, enrolmentIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier)(checkInvitationIsPending { invitation =>
            val name = request.session.get("agencyName").getOrElse(throw AgencyNameNotFound())
            confirmTermsForm.bindFromRequest().fold(
            formWithErrors => {
                Future successful Ok(confirm_terms(formWithErrors, name, invitationId, messageKey))
              }, _ => {
                acceptInvitation(serviceName, invitationId, clientId).map {
                  case NO_CONTENT =>
                    auditService.sendAgentInvitationResponse(invitationId.value, invitation.arn, "Accepted", clientIdentifierType(clientId), clientId, serviceName, name)
                    Redirect(routes.ClientsInvitationController.getCompletePage(invitationId))
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
      case ValidService(_, enrolmentName, enrolmentIdentifier, apiIdentifier, messageKey) =>
        withAuthorisedAsClient(enrolmentName, enrolmentIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier)(checkInvitationIsAccepted { _ =>
            val name = request.session.get("agencyName").getOrElse(throw AgencyNameNotFound())
            Future successful Ok (complete (name, messageKey) ).removingFromSession("agencyName")
          })
        }
      case InvalidService => Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
    }
  }

  val notSignedUp: Action[AnyContent] = ActionWithMdc { implicit request =>
    Forbidden(not_signed_up())
  }

  val notAuthorised:Action[AnyContent] = ActionWithMdc { implicit request =>
    Forbidden(not_authorised())
  }

  val incorrectInvitation: Action[AnyContent] = ActionWithMdc { implicit request =>
    Forbidden(incorrect_invitation())
  }

  val notFoundInvitation: Action[AnyContent] = ActionWithMdc { implicit request =>
    NotFound(not_found_invitation())
  }

  val invitationAlreadyResponded: Action[AnyContent] = ActionWithMdc { implicit request =>
    Forbidden(invitation_already_responded())
  }

  val invitationExpired: Action[AnyContent] = ActionWithMdc { implicit request =>
    Ok(invitation_expired())
  }

  private def acceptInvitation(service: String, invitationId: InvitationId, clientId: String)(implicit hc: HeaderCarrier): Future[Int] = {
    service match {
      case HMRCMTDIT => invitationsService.acceptITSAInvitation(invitationId, MtdItId(clientId))
      case HMRCPIR => invitationsService.acceptAFIInvitation(invitationId, Nino(clientId))
      case HMRCMTDVAT => invitationsService.acceptVATInvitation(invitationId, Vrn(clientId))
      case _ => throw new IllegalStateException("Unsupported Service")
    }
  }

  private def rejectInvitation(service: String, invitationId: InvitationId, clientId: String)(implicit hc: HeaderCarrier): Future[Int] = {
    service match {
      case HMRCMTDIT => invitationsService.rejectITSAInvitation(invitationId, MtdItId(clientId))
      case HMRCPIR => invitationsService.rejectAFIInvitation(invitationId, Nino(clientId))
      case HMRCMTDVAT => invitationsService.rejectVATInvitation(invitationId, Vrn(clientId))
      case _ => throw new IllegalStateException("Unsupported Service")
    }
  }

  private def checkInvitationIsPending(f: Invitation => Future[Result]): Invitation => Future[Result] = {
    case invitation if invitation.status.contains("Pending") =>
      f(invitation)
    case invitation if invitation.status.contains("Expired") =>
      Future successful Redirect(routes.ClientsInvitationController.invitationExpired())
    case _ =>
      Future successful Redirect(routes.ClientsInvitationController.invitationAlreadyResponded())
  }

  private def checkInvitationIsAccepted(f: Invitation => Future[Result]): Invitation => Future[Result] = {
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

  private def clientIdentifierType(clientId: String): String = clientId match {
    case maybeVrn if Vrn.isValid(maybeVrn) => "vrn"
    case maybeNino if Nino.isValid(maybeNino) || MtdItId.isValid(maybeNino) => "ni"
    case _ => throw new IllegalStateException(s"Unsupported ClientIdType")
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
