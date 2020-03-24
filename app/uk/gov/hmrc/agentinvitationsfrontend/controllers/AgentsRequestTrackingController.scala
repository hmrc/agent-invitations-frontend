/*
 * Copyright 2020 HM Revenue & Customs
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
import org.joda.time.LocalDate
import play.api.data.Form
import play.api.data.Forms.{boolean, mapping, optional, text}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import play.api.{Configuration, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.config.{AppConfig, ExternalUrls}
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{AgentClientAuthorisationConnector, PirRelationshipConnector, RelationshipsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.forms.ClientTypeForm
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.personal
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.supportedServices
import uk.gov.hmrc.agentinvitationsfrontend.models.{ClientType, PageInfo, Services}
import uk.gov.hmrc.agentinvitationsfrontend.services.{InvitationsService, TrackService}
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.track.{confirm_cancel, _}
import uk.gov.hmrc.agentinvitationsfrontend.views.track.{RequestCancelledPageConfig, ResendLinkPageConfig, TrackPageConfig}
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

case class TrackResendForm(service: String, clientType: Option[ClientType], expiryDate: String)

case class CancelRequestForm(invitationId: String, service: String, clientType: String, clientName: String)

case class CancelAuthorisationForm(
  service: String,
  clientId: String,
  clientType: String,
  clientName: String,
  invitationId: String)

case class ConfirmForm(value: Option[Boolean])

@Singleton
class AgentsRequestTrackingController @Inject()(
  val authActions: AuthActionsImpl,
  val featureFlags: FeatureFlags,
  val trackService: TrackService,
  val invitationsService: InvitationsService,
  val relationshipsConnector: RelationshipsConnector,
  val pirRelationshipConnector: PirRelationshipConnector,
  acaConnector: AgentClientAuthorisationConnector,
  trackView: track,
  resendLinkView: resend_link,
  confirmCancelView: confirm_cancel,
  requestCancelledView: request_cancelled,
  confirmCancelAuthView: confirm_cancel_authorisation,
  authorisationCancelledView: authorisation_cancelled,
  cancelAuthProblemView: cancel_authorisation_problem)(
  implicit val externalUrls: ExternalUrls,
  configuration: Configuration,
  ec: ExecutionContext,
  val cc: MessagesControllerComponents,
  appConfig: AppConfig)
    extends FrontendController(cc) with I18nSupport {
  import authActions._

  def showTrackRequests(page: Int): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { agent =>
      implicit val now: LocalDate = LocalDate.now()
      val pageInfo = PageInfo(math.max(page, 1), appConfig.trackRequestsPerPage)
      for {
        trackResultsPage <- trackService.bindInvitationsAndRelationships(
                             agent.arn,
                             agent.isWhitelisted,
                             appConfig.trackRequestsShowLastDays,
                             pageInfo)
      } yield {
        val config = TrackPageConfig(
          trackResultsPage.results,
          appConfig.trackRequestsShowLastDays,
          featureFlags.enableTrackCancelAuth,
          pageInfo,
          trackResultsPage.totalResults)
        if (trackResultsPage.totalResults > 0 && page > config.numberOfPages) {
          Redirect(routes.AgentsRequestTrackingController.showTrackRequests(page = config.numberOfPages))
        } else {
          Ok(trackView(config))
        }
      }

    }
  }

  def submitToResendLink: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { agent =>
      trackInformationForm
        .bindFromRequest()
        .fold(
          _ => {
            Logger(getClass).error("Error in form when redirecting to resend-link page.")
            Future successful BadRequest
          },
          data => {
            for {
              agentLink   <- invitationsService.createAgentLink(agent.arn, data.clientType)
              agencyEmail <- acaConnector.getAgencyEmail()
            } yield
              Ok(
                resendLinkView(ResendLinkPageConfig(
                  appConfig.agentInvitationsFrontendExternalUrl,
                  agentLink,
                  data.clientType.map(ClientType.fromEnum).getOrElse(""),
                  data.expiryDate,
                  if (data.clientType.contains(personal)) "personal"
                  else data.service,
                  agencyEmail,
                  routes.AgentsRequestTrackingController.showTrackRequests(1).url
                )))
          }
        )
    }
  }

  def submitToConfirmCancel: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
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
              "clientType"   -> data.clientType,
              "clientName"   -> data.clientName)
        )
    }
  }

  def showConfirmCancel: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      request.session.get("service") match {
        case Some(service) => {
          val clientType = request.session.get("clientType").map(ClientType.toEnum).getOrElse(personal)
          Future successful Ok(
            confirmCancelView(
              service,
              clientType,
              confirmCancelForm,
              routes.AgentsRequestTrackingController.showTrackRequests(1).url))
        }
        case None => Future successful Redirect(routes.AgentsRequestTrackingController.showTrackRequests())
      }
    }
  }

  def submitConfirmCancel: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { agent =>
      request.session.get("invitationId") match {
        case None => Future successful Redirect(routes.AgentsRequestTrackingController.showTrackRequests())
        case Some(id) =>
          val invitationId = InvitationId(id)
          request.session.get("service") match {
            case Some(service) => {
              val clientType = request.session.get("clientType").map(ClientType.toEnum).getOrElse(personal)
              confirmCancelForm
                .bindFromRequest()
                .fold(
                  formWithErrors => {
                    Future successful Ok(
                      confirmCancelView(
                        service,
                        clientType,
                        formWithErrors,
                        routes.AgentsRequestTrackingController.showTrackRequests(1).url))
                  },
                  data => {
                    if (data.value.getOrElse(true)) {
                      acaConnector
                        .cancelInvitation(agent.arn, invitationId)
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
            case None => Future successful Redirect(routes.AgentsRequestTrackingController.showTrackRequests())
          }
      }
    }
  }

  def showRequestCancelled: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      val service = request.session.get("service").getOrElse("")
      val clientType = request.session.get("clientType").getOrElse("")
      val clientName = request.session.get("clientName").getOrElse("")
      Future successful Ok(requestCancelledView(RequestCancelledPageConfig(service, clientType, clientName)))
    }
  }

  def submitToCancelAuthorisationConfirm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      cancelAuthorisationForm
        .bindFromRequest()
        .fold(
          _ => {
            Logger(getClass).error("Error in form when redirecting to resend-link page.")
            Future successful BadRequest
          },
          (data: CancelAuthorisationForm) =>
            Future successful Redirect(routes.AgentsRequestTrackingController.showCancelAuthorisationConfirm())
              .addingToSession(
                "service"      -> data.service,
                "clientId"     -> data.clientId,
                "clientName"   -> data.clientName,
                "clientType"   -> data.clientType,
                "invitationId" -> data.invitationId)
        )
    }
  }

  def showCancelAuthorisationConfirm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      val service = request.session.get("service").getOrElse("")
      val clientType = request.session.get("clientType").map(ClientType.toEnum).getOrElse(personal)
      Future successful Ok(
        confirmCancelAuthView(
          confirmCancelAuthorisationForm,
          service,
          clientType,
          routes.AgentsRequestTrackingController.showTrackRequests(1).url))
    }
  }

  def submitCancelAuthorisationConfirm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { agent =>
      val clientId = request.session.get("clientId").getOrElse("")
      val service = request.session.get("service").getOrElse("")
      val clientType = request.session.get("clientType").map(ClientType.toEnum).getOrElse(personal)
      val invitationId = InvitationId(request.session.get("invitationId").getOrElse(""))
      confirmCancelAuthorisationForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Future successful Ok(
              confirmCancelAuthView(
                formWithErrors,
                service,
                clientType,
                routes.AgentsRequestTrackingController.showTrackRequests(1).url)),
          data =>
            if (data.value.getOrElse(true)) {
              deleteRelationshipForService(service, agent.arn, clientId).map {
                case Some(bool) => {
                  acaConnector.cancelInvitation(agent.arn, invitationId)
                  Redirect(routes.AgentsRequestTrackingController.showAuthorisationCancelled())
                }
                case _ => Ok(cancelAuthProblemView())
              }
            } else Future successful Redirect(routes.AgentsRequestTrackingController.showTrackRequests())
        )
    }
  }

  def showAuthorisationCancelled: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      val service = request.session.get("service").getOrElse("")
      val clientName = request.session.get("clientName").getOrElse("")
      val clientType = request.session.get("clientType").map(ClientType.toEnum).getOrElse(personal)
      val clientId = request.session.get("clientId").getOrElse("")
      Future successful Ok(authorisationCancelledView(service, clientId, clientName, clientType))
    }
  }

  def deleteRelationshipForService(service: String, arn: Arn, clientId: String)(implicit hc: HeaderCarrier) =
    service match {
      case "HMRC-MTD-IT"            => relationshipsConnector.deleteRelationshipItsa(arn, Nino(clientId))
      case "PERSONAL-INCOME-RECORD" => pirRelationshipConnector.deleteRelationship(arn, service, clientId)
      case "HMRC-MTD-VAT"           => relationshipsConnector.deleteRelationshipVat(arn, Vrn(clientId))
      case "HMRC-TERS-ORG"          => relationshipsConnector.deleteRelationshipTrust(arn, Utr(clientId))
      case "HMRC-CGT-PD"            => relationshipsConnector.deleteRelationshipCgt(arn, CgtRef(clientId))
      case _                        => throw new Error("Service not supported")
    }

  val trackInformationForm: Form[TrackResendForm] = {
    Form(
      mapping(
        "service" -> text.verifying("Unsupported Service", service => supportedServices.contains(service)),
        "clientType" -> optional(text
          .verifying("Unsupported client type", clientType => ClientTypeForm.supportedClientTypes.contains(clientType))
          .transform(ClientType.toEnum, ClientType.fromEnum)),
        "expiryDate" -> text.verifying("Invalid date format", expiryDate => DateFieldHelper.parseDate(expiryDate))
      )(TrackResendForm.apply)(TrackResendForm.unapply))
  }

  val cancelRequestForm: Form[CancelRequestForm] = {
    Form(
      mapping(
        "invitationId" -> text
          .verifying("Invalid invitation Id", invitationId => InvitationId.isValid(invitationId)),
        "service" -> text.verifying("Unsupported Service", service => supportedServices.contains(service)),
        "clientType" -> text
          .verifying("Unsupported ClientType", clientType => ClientTypeForm.supportedClientTypes.contains(clientType)),
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
        "service"  -> text.verifying("Unsupported Service", service => supportedServices.contains(service)),
        "clientId" -> normalizedText.verifying(validateClientId),
        "clientType" -> text
          .verifying("Unsupported ClientType", clientType => ClientTypeForm.supportedClientTypes.contains(clientType)),
        "clientName"   -> text,
        "invitationId" -> text
      )(CancelAuthorisationForm.apply)(CancelAuthorisationForm.unapply))
  }

  def radioChoice[A](invalidError: String): Constraint[Option[A]] = Constraint[Option[A]] { fieldValue: Option[A] =>
    if (fieldValue.isDefined)
      Valid
    else
      Invalid(ValidationError(invalidError))
  }

}
