/*
 * Copyright 2019 HM Revenue & Customs
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

import com.google.inject.Provider
import javax.inject.{Inject, Named, Singleton}

import org.joda.time.LocalDate
import play.api.data.Form
import play.api.data.Forms.{boolean, mapping, optional, text}
import play.api.data.validation.Constraint
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent}
import play.api.{Configuration, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{InvitationsConnector, PirRelationshipConnector, RelationshipsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ClientsInvitationController.radioChoice
import uk.gov.hmrc.agentinvitationsfrontend.models.Services
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.supportedServices
import uk.gov.hmrc.agentinvitationsfrontend.services.{InvitationsService, TrackService}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.track._
import uk.gov.hmrc.agentinvitationsfrontend.views.track.{ResendLinkPageConfig, TrackPageConfig}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, Vrn}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._

import scala.concurrent.{ExecutionContext, Future}

case class TrackResendForm(service: String, clientType: Option[String], expiryDate: String)

case class CancelRequestForm(invitationId: String, service: String, clientName: String)

case class CancelAuthorisationForm(service: String, clientId: String, clientName: String)

@Singleton
class AgentsRequestTrackingController @Inject()(
  val messagesApi: play.api.i18n.MessagesApi,
  val authConnector: AuthConnector,
  val withVerifiedPasscode: PasscodeVerification,
  val featureFlags: FeatureFlags,
  val trackService: TrackService,
  val invitationsService: InvitationsService,
  val invitationsConnector: InvitationsConnector,
  val relationshipsConnector: RelationshipsConnector,
  val pirRelationshipConnector: PirRelationshipConnector,
  @Named("track-requests-show-last-days") val trackRequestsShowLastDays: Int,
  @Named("agent-invitations-frontend.external-url") externalUrl: String)(
  implicit val externalUrls: ExternalUrls,
  configuration: Configuration,
  ec: ExecutionContext)
    extends FrontendController with I18nSupport with AuthActions {

  val showTrackRequests: Action[AnyContent] = Action.async { implicit request =>
    if (featureFlags.enableTrackRequests) {
      withAuthorisedAsAgent { (arn, isWhitelisted) =>
        implicit val now: LocalDate = LocalDate.now()
        for {
          invitationsAndRelationships <- trackService.bindInvitationsAndRelationships(
                                          arn,
                                          isWhitelisted,
                                          trackRequestsShowLastDays)
        } yield
          Ok(
            track(
              TrackPageConfig(
                invitationsAndRelationships,
                trackRequestsShowLastDays,
                featureFlags.enableTrackCancelAuth)))
      }
    } else {
      Logger(getClass).warn("Feature flag to enable track page is off")
      Future successful BadRequest
    }
  }

  def submitToResendLink: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, _) =>
      trackInformationForm
        .bindFromRequest()
        .fold(
          _ => {
            Logger(getClass).error("Error in form when redirecting to resend-link page.")
            Future successful BadRequest
          },
          data => {
            for {
              agentLink <- invitationsService.createAgentLink(arn, data.clientType.getOrElse(""))
            } yield
              Ok(
                resend_link(
                  ResendLinkPageConfig(externalUrl, agentLink, data.clientType.getOrElse(""), data.expiryDate)))
          }
        )
    }
  }

  def submitToConfirmCancel: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      cancelRequestForm
        .bindFromRequest()
        .fold(
          _ => {
            Logger(getClass).error("Error when redirecting to confirm cancel page.")
            Future successful BadRequest
          },
          data =>
            Future successful Redirect(routes.AgentsRequestTrackingController.showConfirmCancel()).addingToSession(
              "invitationId" -> data.invitationId,
              "service"      -> data.service,
              "clientName"   -> data.clientName)
        )
    }
  }

  def showConfirmCancel: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      val service = request.session.get("service").getOrElse("")
      Future successful Ok(confirm_cancel(service, confirmCancelForm))
    }
  }

  def submitConfirmCancel: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, _) =>
      request.session.get("invitationId") match {
        case None => Future successful Redirect(routes.AgentsRequestTrackingController.showTrackRequests())
        case Some(id) =>
          val invitationId = InvitationId(id)
          val service = Services.determineServiceMessageKey(invitationId)
          confirmCancelForm
            .bindFromRequest()
            .fold(
              formWithErrors => {
                Future successful Ok(confirm_cancel(service, formWithErrors))
              },
              data => {
                if (data.value.getOrElse(true)) {
                  invitationsConnector
                    .cancelInvitation(arn, invitationId)
                    .map {
                      case Some(true)  => Redirect(routes.AgentsRequestTrackingController.showRequestCancelled())
                      case Some(false) => NotFound
                      case _           => Forbidden
                    }
                } else {
                  Future successful Redirect(routes.AgentsRequestTrackingController.showTrackRequests())
                }
              }
            )
      }

    }
  }

  def showRequestCancelled: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      val service = request.session.get("service").getOrElse("")
      val clientName = request.session.get("clientName").getOrElse("")
      Future successful Ok(request_cancelled(service, clientName))
    }
  }

  def submitToCancelAuthorisationConfirm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      cancelAuthorisationForm
        .bindFromRequest()
        .fold(
          _ => {
            Logger(getClass).error("Error in form when redirecting to resend-link page.")
            Future successful BadRequest
          },
          data =>
            Future successful Redirect(routes.AgentsRequestTrackingController.showCancelAuthorisationConfirm())
              .addingToSession("service" -> data.service, "clientId" -> data.clientId, "clientName" -> data.clientName)
        )
    }
  }

  def showCancelAuthorisationConfirm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      val service = request.session.get("service").getOrElse("")
      Future successful Ok(confirm_cancel_authorisation(confirmCancelAuthorisationForm, service))
    }
  }

  def submitCancelAuthorisationConfirm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, _) =>
      val clientId = request.session.get("clientId").getOrElse("")
      val service = request.session.get("service").getOrElse("")
      confirmCancelAuthorisationForm
        .bindFromRequest()
        .fold(
          formWithErrors => Future successful Ok(confirm_cancel_authorisation(formWithErrors, service)),
          data =>
            if (data.value.getOrElse(true)) {
              deleteRelationshipForService(service, arn, clientId).map {
                case Some(true)  => Redirect(routes.AgentsRequestTrackingController.showAuthorisationCancelled())
                case Some(false) => NotFound
                case _           => Ok(cancel_authorisation_problem())
              }
            } else Future successful Redirect(routes.AgentsRequestTrackingController.showTrackRequests())
        )
    }
  }

  def showAuthorisationCancelled: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, _) =>
      val service = request.session.get("service").getOrElse("")
      val clientName = request.session.get("clientName").getOrElse("")
      val clientId = request.session.get("clientId").getOrElse("")
      Future successful Ok(authorisation_cancelled(service, clientId, clientName))
    }
  }

  def deleteRelationshipForService(service: String, arn: Arn, clientId: String)(implicit hc: HeaderCarrier) =
    service match {
      case "HMRC-MTD-IT"            => relationshipsConnector.deleteRelationshipItsa(arn, Nino(clientId))
      case "PERSONAL-INCOME-RECORD" => pirRelationshipConnector.deleteRelationship(arn, service, clientId)
      case "HMRC-MTD-VAT"           => relationshipsConnector.deleteRelationshipVat(arn, Vrn(clientId))
      case _                        => throw new Error("Service not supported")
    }

  val trackInformationForm: Form[TrackResendForm] = {
    Form(
      mapping(
        "service" -> text.verifying("Unsupported Service", service => supportedServices.contains(service)),
        "clientType" -> optional(text)
          .verifying("Unsupported client type", clientType => Services.supportedClientTypes.contains(clientType)),
        "expiryDate" -> text.verifying("Invalid date format", expiryDate => DateFieldHelper.parseDate(expiryDate))
      )(TrackResendForm.apply)(TrackResendForm.unapply))
  }

  val cancelRequestForm: Form[CancelRequestForm] = {
    Form(
      mapping(
        "invitationId" -> text
          .verifying("Invalid invitation Id", invitationId => InvitationId.isValid(invitationId)),
        "service"    -> text.verifying("Unsupported Service", service => supportedServices.contains(service)),
        "clientName" -> text
      )(CancelRequestForm.apply)(CancelRequestForm.unapply)
    )
  }

  val cancelChoice: Constraint[Option[Boolean]] = radioChoice("error.confirmCancel.invalid")

  val confirmCancelForm: Form[ConfirmForm] = Form[ConfirmForm](
    mapping(
      "confirmCancel" -> optional(boolean)
        .verifying(cancelChoice))(ConfirmForm.apply)(ConfirmForm.unapply)
  )

  val confirmCancelAuthorisationForm: Form[ConfirmForm] = Form[ConfirmForm](
    mapping(
      "confirmCancelAuthorisation" -> optional(boolean)
        .verifying(cancelChoice))(ConfirmForm.apply)(ConfirmForm.unapply)
  )

  val cancelAuthorisationForm: Form[CancelAuthorisationForm] = {
    Form(
      mapping(
        "service"    -> text.verifying("Unsupported Service", service => supportedServices.contains(service)),
        "clientId"   -> normalizedText.verifying(validateClientId),
        "clientName" -> text
      )(CancelAuthorisationForm.apply)(CancelAuthorisationForm.unapply))
  }

}
