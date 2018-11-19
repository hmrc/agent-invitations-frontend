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

import javax.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.{Action, AnyContent, Request, Result}
import play.api.{Configuration, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.InvitationsConnector
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{messageKeyForAfi, messageKeyForITSA, _}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services.InvitationsService
import uk.gov.hmrc.agentinvitationsfrontend.views.clients.{MultiConfirmDeclinePageConfig, MultiInvitationDeclinedPageConfig, SingleConfirmDeclinePageConfig, SingleInvitationDeclinedPageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.clients._
import uk.gov.hmrc.agentmtdidentifiers.model.{InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.{HeaderCarrier, NotFoundException, Upstream4xxResponse}
import uk.gov.hmrc.play.bootstrap.controller.{ActionWithMdc, FrontendController}

import scala.concurrent.Future

case class ConfirmForm(value: Option[Boolean])

case class ConfirmAuthForm(confirmAuthorisation: Option[String])

@Singleton
class ClientsInvitationController @Inject()(
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  auditService: AuditService,
  val messagesApi: play.api.i18n.MessagesApi,
  val authConnector: AuthConnector,
  val withVerifiedPasscode: PasscodeVerification)(
  implicit val configuration: Configuration,
  val externalUrls: ExternalUrls)
    extends FrontendController with I18nSupport with AuthActions {

  import ClientsInvitationController._

  def warmUp(clientType: String, uid: String, normalisedAgentName: String) = Action.async { implicit request =>
    for {
      record <- invitationsConnector.getAgentReferenceRecord(uid)
      result <- record match {
                 case Some(r) if r.normalisedAgentNames.contains(normalisedAgentName) => {
                   invitationsService.getAgencyName(r.arn).map { name =>
                     Ok(warm_up(name, clientType, uid))
                   }
                 }
                 case None => Future successful BadRequest
               }

    } yield result
  }

  def start(invitationId: InvitationId): Action[AnyContent] = ActionWithMdc { implicit request =>
    determineService(invitationId) match {
      case IsServiceMessageKeyValid(messageKey) =>
        Ok(landing_page(invitationId, messageKey, confirmAuthorisationForm))
      case _ =>
        Redirect(routes.ClientsInvitationController.notFoundInvitation())
          .addingToSession("clientService" -> determineServiceMessageKey(invitationId))

    }
  }

  def submitStart(invitationId: InvitationId): Action[AnyContent] = ActionWithMdc { implicit request =>
    confirmAuthorisationForm
      .bindFromRequest()
      .fold(
        formWithErrors => {
          determineService(invitationId) match {
            case IsServiceMessageKeyValid(messageKey) =>
              Ok(landing_page(invitationId, messageKey, formWithErrors))
            case _ =>
              Redirect(routes.ClientsInvitationController.notFoundInvitation())
                .addingToSession("clientService" -> determineServiceMessageKey(invitationId))
          }
        },
        data => {
          val result = data.confirmAuthorisation.getOrElse("") match {
            case "yes"   => Redirect(routes.ClientsInvitationController.getConfirmTerms(invitationId))
            case "no"    => Redirect(routes.ClientsInvitationController.getConfirmDecline(invitationId))
            case "maybe" => Redirect(routes.ClientsInvitationController.getDecideLater(invitationId))
            case _       => throw new Exception("Invalid authorisation choice")
          }
          result
        }
      )
  }

  def getDecideLater(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case IsServiceMessageKeyValid(messageKey) =>
        Future successful Ok(decide_later(invitationId, messageKey))
      case InvalidService =>
        Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
          .addingToSession("clientService" -> determineServiceMessageKey(invitationId))
    }
  }

  def getInvitationDeclined(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(serviceName, enrolmentName, enrolmentIdentifier, apiIdentifier, messageKey) =>
        withAuthorisedAsClient(enrolmentName, enrolmentIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier, messageKey)(checkInvitationIsPending(messageKey) {
            invitation =>
              invitationsService.getAgencyName(invitation.arn).flatMap {
                agencyName =>
                  rejectInvitation(serviceName, invitationId, clientId).map {
                    case NO_CONTENT => {
                      auditService.sendAgentInvitationResponse(
                        invitationId.value,
                        invitation.arn,
                        "Declined",
                        clientIdentifierType(invitation.clientId),
                        clientId,
                        serviceName,
                        agencyName)
                      Ok(invitation_declined(SingleInvitationDeclinedPageConfig(agencyName, messageKey)))
                    }
                    case status => throw new Exception(s"Invitation rejection failed with status $status")
                  }
              }
          })
        }
      case InvalidService =>
        Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
          .addingToSession("clientService" -> determineServiceMessageKey(invitationId))
    }
  }

  def getConfirmDecline(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(_, enrolmentName, enrolmentIdentifier, apiIdentifier, messageKey) =>
        withAuthorisedAsClient(enrolmentName, enrolmentIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier, messageKey)(checkInvitationIsPending(messageKey) {
            invitation =>
              invitationsService.getAgencyName(invitation.arn).map { agencyName =>
                Ok(
                  confirm_decline(
                    confirmDeclineForm,
                    SingleConfirmDeclinePageConfig(agencyName, invitationId, messageKey)))
              }
          })
        }
      case InvalidService =>
        Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
          .addingToSession("clientService" -> determineServiceMessageKey(invitationId))
    }
  }

  def getMultiConfirmDecline(clientType: String, uid: String): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAnyClient { _ =>
      for {
        agentReferenceRecordOpt <- invitationsConnector.getAgentReferenceRecord(uid)
        result <- agentReferenceRecordOpt match {
                   case Some(agentReferenceRecord) =>
                     for {
                       invitationIds <- invitationsConnector.getAllPendingInvitationIds(uid)
                       agencyName    <- invitationsService.getAgencyName(agentReferenceRecord.arn)
                       serviceKeys = invitationIds.map(id => Services.determineServiceMessageKey(id))
                     } yield
                       if (serviceKeys.nonEmpty)
                         Ok(
                           confirm_decline(
                             confirmDeclineForm,
                             MultiConfirmDeclinePageConfig(agencyName, clientType, uid, serviceKeys)))
                       else
                         Redirect(routes.ClientsInvitationController.notFoundInvitation())

                   case None =>
                     Future.failed(new Exception(s"Agent Reference Record not found for $uid"))
                 }
      } yield result
    }
  }

  def submitConfirmDecline(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(_, enrolmentName, enrolmentIdentifier, apiIdentifier, messageKey) =>
        withAuthorisedAsClient(enrolmentName, enrolmentIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier, messageKey)(checkInvitationIsPending(messageKey) {
            invitation =>
              confirmDeclineForm
                .bindFromRequest()
                .fold(
                  formWithErrors => {
                    invitationsService.getAgencyName(invitation.arn).map { agencyName =>
                      Ok(
                        confirm_decline(
                          formWithErrors,
                          SingleConfirmDeclinePageConfig(agencyName, invitationId, messageKey)))
                    }
                  },
                  data => {
                    val result =
                      if (data.value.getOrElse(false))
                        Redirect(routes.ClientsInvitationController.getInvitationDeclined(invitationId))
                      else
                        Redirect(routes.ClientsInvitationController.getConfirmTerms(invitationId))

                    Future successful result
                  }
                )
          })
        }
      case InvalidService =>
        Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
          .addingToSession("clientService" -> determineServiceMessageKey(invitationId))
    }
  }

  private def getAgencyNameViaClient(uid: String)(implicit hc: HeaderCarrier): Future[String] =
    for {
      record <- invitationsConnector.getAgentReferenceRecord(uid)
      name <- record match {
               case Some(rec) => invitationsService.getAgencyName(rec.arn)
               case None      => Future.failed(new Exception(s"Agent Reference Record not found for $uid"))
             }
    } yield name

  private def rejectPendingInvitation(clientId: String, invitationId: InvitationId, apiIdentifier: String)(implicit hc: HeaderCarrier) =
    for {
      isPending <- invitationsService
                    .getClientInvitation(clientId, invitationId, apiIdentifier)
                    .map(inv => inv.status == "Pending")
    } yield
      if (isPending) {
        apiIdentifier match {
          case MTDITID =>
            invitationsService.rejectITSAInvitation(invitationId, MtdItId(clientId))
          case NI =>
            invitationsService.rejectAFIInvitation(invitationId, Nino(clientId))
          case VRN =>
            invitationsService.rejectVATInvitation(invitationId, Vrn(clientId))
        }
      }

  private def getPageComponents(uid: String)(body: (Seq[InvitationId], String, Seq[String]) => Result)(implicit hc: HeaderCarrier) =
    for {
      invitationIds <- invitationsConnector.getAllPendingInvitationIds(uid)
      agencyName    <- getAgencyNameViaClient(uid)
      serviceKeys = invitationIds.map(id => Services.determineServiceMessageKey(id))
    } yield body(invitationIds, agencyName, serviceKeys)

  def submitMultiConfirmDecline(clientType: String, uid: String): Action[AnyContent] = Action.async {
    implicit request =>
      withAuthorisedAsAnyClient { identifiers =>
        val reformatIdentifiers: Seq[(String, String)] = identifiers.map(id => (id._1, id._2.replaceAll(" ", "")))
        confirmDeclineForm
          .bindFromRequest()
          .fold(
            formWithErrors => {

              getPageComponents(uid) { (_, agencyName, serviceKeys) =>
                Ok(
                  confirm_decline(
                    formWithErrors,
                    MultiConfirmDeclinePageConfig(agencyName, clientType, uid, serviceKeys)))
              }
            },
            data => {
              if (data.value.getOrElse(false)) {
                getPageComponents(uid) {
                  (invitationIds, agencyName, serviceKeys) =>
                    Future.traverse(invitationIds) {
                      invitationId =>
                        determineService(invitationId) match {
                          case ValidService(serviceName, _, _, _, _) if supportedServices.contains(serviceName) =>
                            Future.traverse(reformatIdentifiers) {
                              case identifier
                                  if identifier._1 == MTDITID && MtdItId
                                    .isValid(identifier._2) && serviceName == HMRCMTDIT =>
                                rejectPendingInvitation(identifier._2, invitationId, MTDITID)

                              case identifier
                                  if identifier._1 == NINO && Nino.isValid(identifier._2) && serviceName == HMRCPIR =>
                                rejectPendingInvitation(identifier._2, invitationId, NI)

                              case identifier
                                  if identifier._1 == VRN && Vrn
                                    .isValid(identifier._2) && serviceName == HMRCMTDVAT =>
                                rejectPendingInvitation(identifier._2, invitationId, VRN)

                              case _ =>
                                Future failed new Exception("Unable to reject invitations due to mixed data")
                            }

                          case InvalidService =>
                            Future failed new Exception("Unable to reject invitations due to Unsupported Service")

                        }

                    }
                    Ok(invitation_declined(MultiInvitationDeclinedPageConfig(agencyName, serviceKeys)))
                }
              } else {
                //Confirm Terms Multi Page Not There Yet
                Future.successful(NotImplemented)
              }
            }
          )
      }
  }

  def getConfirmTerms(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(_, enrolmentName, enrolmentIdentifier, apiIdentifier, messageKey) =>
        withAuthorisedAsClient(enrolmentName, enrolmentIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier, messageKey)(checkInvitationIsPending(messageKey) {
            invitation =>
              invitationsService.getAgencyName(invitation.arn).map { agencyName =>
                Ok(confirm_terms(confirmTermsForm, agencyName, invitationId, messageKey))
              }
          })
        }
      case InvalidService =>
        Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
          .addingToSession("clientService" -> determineServiceMessageKey(invitationId))
    }
  }

  def submitConfirmTerms(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(serviceName, enrolmentName, enrolmentIdentifier, apiIdentifier, messageKey) =>
        withAuthorisedAsClient(enrolmentName, enrolmentIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier, messageKey)(checkInvitationIsPending(messageKey) {
            invitation =>
              invitationsService.getAgencyName(invitation.arn).flatMap {
                agencyName =>
                  confirmTermsForm
                    .bindFromRequest()
                    .fold(
                      formWithErrors => {
                        Future successful Ok(confirm_terms(formWithErrors, agencyName, invitationId, messageKey))
                      },
                      data => {
                        if (data.value.getOrElse(false)) {
                          acceptInvitation(serviceName, invitationId, clientId).map {
                            case NO_CONTENT =>
                              auditService.sendAgentInvitationResponse(
                                invitationId.value,
                                invitation.arn,
                                "Accepted",
                                clientIdentifierType(clientId),
                                clientId,
                                serviceName,
                                agencyName)
                              Redirect(routes.ClientsInvitationController.getCompletePage(invitationId))
                            case status => throw new Exception(s"Invitation acceptance failed with status $status")
                          }
                        } else {
                          Future successful Redirect(routes.ClientsInvitationController.getConfirmDecline(invitationId))
                        }
                      }
                    )
              }
          })
        }
      case InvalidService =>
        Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
          .addingToSession("clientService" -> determineServiceMessageKey(invitationId))

    }
  }

  def getCompletePage(invitationId: InvitationId): Action[AnyContent] = Action.async { implicit request =>
    determineService(invitationId) match {
      case ValidService(_, enrolmentName, enrolmentIdentifier, apiIdentifier, messageKey) =>
        withAuthorisedAsClient(enrolmentName, enrolmentIdentifier) { clientId =>
          withValidInvitation(clientId, invitationId, apiIdentifier, messageKey)(checkInvitationIsAccepted(messageKey) {
            invitation =>
              invitationsService.getAgencyName(invitation.arn).map { agencyName =>
                Ok(complete(agencyName, messageKey))
              }
          })
        }
      case InvalidService =>
        Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
          .addingToSession("clientService" -> determineServiceMessageKey(invitationId))
    }
  }

  val notSignedUp: Action[AnyContent] = ActionWithMdc { implicit request =>
    request.session.get("clientService") match {
      case Some(Services.HMRCMTDVAT) =>
        Forbidden(not_signed_up(Messages("not-signed-up-vat.description"), Services.messageKeyForVAT))
      case Some(Services.HMRCMTDIT) =>
        Forbidden(not_signed_up(Messages("not-signed-up.description"), Services.messageKeyForITSA))
      case _ =>
        Forbidden(not_signed_up(Messages("not-signed-up.description"), "Service is Missing"))
    }
  }

  val notAuthorised: Action[AnyContent] = ActionWithMdc { implicit request =>
    Forbidden(
      not_authorised(
        Messages("not-authorised.header"),
        Messages("not-authorised.description"),
        Services.messageKeyForAfi))
  }

  val incorrectInvitation: Action[AnyContent] = ActionWithMdc { implicit request =>
    val serviceMessageKey = request.session.get("clientService").getOrElse("Service Is Missing")
    Forbidden(incorrect_invitation(serviceMessageKey))
  }

  val notFoundInvitation: Action[AnyContent] = ActionWithMdc { implicit request =>
    val serviceMessageKey = request.session.get("clientService").getOrElse("Service Is Missing")
    NotFound(not_found_invitation(serviceMessageKey))
  }

  val invitationAlreadyResponded: Action[AnyContent] = ActionWithMdc { implicit request =>
    val serviceMessageKey = request.session.get("clientService").getOrElse("Service Is Missing")
    Forbidden(invitation_already_responded(serviceMessageKey))
  }

  val invitationExpired: Action[AnyContent] = ActionWithMdc { implicit request =>
    val serviceMessageKey = request.session.get("clientService").getOrElse("Service Is Missing")
    Ok(invitation_expired(serviceMessageKey))
  }

  val requestCancelled: Action[AnyContent] = ActionWithMdc { implicit request =>
    val serviceMessageKey = request.session.get("clientService").getOrElse("Service Is Missing")
    Ok(request_cancelled(serviceMessageKey))
  }

  private def acceptInvitation(service: String, invitationId: InvitationId, clientId: String)(
    implicit hc: HeaderCarrier): Future[Int] =
    service match {
      case HMRCMTDIT  => invitationsService.acceptITSAInvitation(invitationId, MtdItId(clientId))
      case HMRCPIR    => invitationsService.acceptAFIInvitation(invitationId, Nino(clientId))
      case HMRCMTDVAT => invitationsService.acceptVATInvitation(invitationId, Vrn(clientId))
      case _          => throw new IllegalStateException("Unsupported Service")
    }

  private def rejectInvitation(service: String, invitationId: InvitationId, clientId: String)(
    implicit hc: HeaderCarrier): Future[Int] =
    service match {
      case HMRCMTDIT  => invitationsService.rejectITSAInvitation(invitationId, MtdItId(clientId))
      case HMRCPIR    => invitationsService.rejectAFIInvitation(invitationId, Nino(clientId))
      case HMRCMTDVAT => invitationsService.rejectVATInvitation(invitationId, Vrn(clientId))
      case _          => throw new IllegalStateException("Unsupported Service")
    }

  private def checkInvitationIsPending(serviceMessageKey: String)(f: StoredInvitation => Future[Result])(
    implicit request: Request[_]): StoredInvitation => Future[Result] = {
    case invitation if invitation.status.contains("Pending") =>
      f(invitation)
    case invitation if invitation.status.contains("Expired") =>
      Future successful Redirect(routes.ClientsInvitationController.invitationExpired())
        .addingToSession("clientService" -> serviceMessageKey)
    case invitation if invitation.status.contains("Cancelled") =>
      Future successful Redirect(routes.ClientsInvitationController.requestCancelled())
        .addingToSession("clientService" -> serviceMessageKey)
    case _ =>
      Future successful Redirect(routes.ClientsInvitationController.invitationAlreadyResponded())
        .addingToSession("clientService" -> serviceMessageKey)
  }

  private def checkInvitationIsAccepted(serviceMessageKey: String)(f: StoredInvitation => Future[Result])(
    implicit request: Request[_]): StoredInvitation => Future[Result] = {
    case invitation if invitation.status.contains("Accepted") =>
      f(invitation)
    case _ =>
      Future successful Redirect(routes.ClientsInvitationController.invitationAlreadyResponded())
        .addingToSession("clientService" -> serviceMessageKey)

  }

  private def withValidInvitation[A](
    clientId: String,
    invitationId: InvitationId,
    apiIdentifier: String,
    serviceMessageKey: String)(
    body: StoredInvitation => Future[Result])(implicit request: Request[A], hc: HeaderCarrier): Future[Result] =
    invitationsService
      .getClientInvitation(clientId, invitationId, apiIdentifier)
      .flatMap(body)
      .recover {
        case ex: Upstream4xxResponse if ex.message.contains("NO_PERMISSION_ON_CLIENT") =>
          Logger(getClass).warn(s"${invitationId.value} Has been access by the wrong Client.")
          Redirect(routes.ClientsInvitationController.incorrectInvitation())
            .addingToSession("clientService" -> serviceMessageKey)
        case ex: Upstream4xxResponse if ex.message.contains("INVALID_INVITATION_STATUS") =>
          Logger(getClass).warn(s"${invitationId.value} Has already been responded.")
          Redirect(routes.ClientsInvitationController.invitationAlreadyResponded())
            .addingToSession("clientService" -> serviceMessageKey)
        case ex: Upstream4xxResponse if ex.message.contains("INVITATION_NOT_FOUND") =>
          Logger(getClass).warn(s"${invitationId.value} is not found.")
          Redirect(routes.ClientsInvitationController.notFoundInvitation())
            .addingToSession("clientService" -> serviceMessageKey)
        case _: NotFoundException =>
          Logger(getClass).warn(s"${invitationId.value} is not found.")
          Redirect(routes.ClientsInvitationController.notFoundInvitation())
            .addingToSession("clientService" -> serviceMessageKey)
      }

  private def clientIdentifierType(clientId: String): String = clientId match {
    case maybeVrn if Vrn.isValid(maybeVrn)                                  => "vrn"
    case maybeNino if Nino.isValid(maybeNino) || MtdItId.isValid(maybeNino) => "ni"
    case _                                                                  => throw new IllegalStateException(s"Unsupported ClientIdType")
  }
}

object ClientsInvitationController {

  def radioChoice[A](invalidError: String): Constraint[Option[A]] = Constraint[Option[A]] { fieldValue: Option[A] =>
    if (fieldValue.isDefined)
      Valid
    else
      Invalid(ValidationError(invalidError))
  }

  val declineChoice: Constraint[Option[Boolean]] = radioChoice("error.confirmDecline.invalid")

  val termsChoice: Constraint[Option[Boolean]] = radioChoice("error.confirmTerms.invalid")

  val authChoice: Constraint[Option[String]] = radioChoice("error.confirmAuthorisation.invalid")

  val confirmDeclineForm: Form[ConfirmForm] = Form[ConfirmForm](
    mapping("confirmDecline" -> optional(boolean)
      .verifying(declineChoice))(ConfirmForm.apply)(ConfirmForm.unapply))

  val confirmTermsForm: Form[ConfirmForm] = Form[ConfirmForm](
    mapping("confirmTerms" -> optional(boolean)
      .verifying(termsChoice))(ConfirmForm.apply)(ConfirmForm.unapply))

  val confirmAuthorisationForm: Form[ConfirmAuthForm] = Form[ConfirmAuthForm](
    mapping(
      "confirmAuthorisation" -> optional(text)
        .verifying(authChoice))(ConfirmAuthForm.apply)(ConfirmAuthForm.unapply)
  )

  object IsServiceMessageKeyValid {
    def unapply(service: Service): Option[String] =
      service match {
        case validService @ ValidService(_, _, _, _, messageKey) if messageKey.nonEmpty =>
          Some(validService.messageKey)
        case _ =>
          None
      }
  }

}
