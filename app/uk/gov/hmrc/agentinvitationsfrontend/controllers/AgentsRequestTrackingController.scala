/*
 * Copyright 2022 HM Revenue & Customs
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
import play.api.i18n.Messages
import play.api.data.Forms.{boolean, mapping, optional, text}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import play.api.{Configuration, Logging}
import uk.gov.hmrc.hmrcfrontend.config.ContactFrontendConfig
import uk.gov.hmrc.agentinvitationsfrontend.config.{AppConfig, ExternalUrls}
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{AgentClientAuthorisationConnector, PirRelationshipConnector, RelationshipsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.forms.{ClientTypeForm, FilterTrackRequestsForm}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.Personal
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.supportedServices
import uk.gov.hmrc.agentinvitationsfrontend.models.{AuthorisedAgent, ClientType, FilterFormStatus, PageInfo}
import uk.gov.hmrc.agentinvitationsfrontend.services.{InvitationsService, TrackService}
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.track.{confirm_cancel, _}
import uk.gov.hmrc.agentinvitationsfrontend.views.track.{RequestCancelledPageConfig, ResendLinkPageConfig, TrackPageConfig}
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

case class TrackResendForm(service: String, clientType: Option[ClientType], expiryDate: String)

case class CancelRequestForm(invitationId: String, service: String, clientType: String, clientName: String)

case class CancelAuthorisationForm(service: String, clientId: String, clientType: String, clientName: String, invitationId: String, status: String)

case class ConfirmForm(value: Option[Boolean])

@Singleton
class AgentsRequestTrackingController @Inject()(
  val authActions: AuthActionsImpl,
  val featureFlags: FeatureFlags,
  val trackService: TrackService,
  val invitationsService: InvitationsService,
  val relationshipsConnector: RelationshipsConnector,
  val agentClientAuthorisationConnector: AgentClientAuthorisationConnector,
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
  implicit val contactFrontendConfig: ContactFrontendConfig,
  configuration: Configuration,
  ec: ExecutionContext,
  val cc: MessagesControllerComponents,
  appConfig: AppConfig)
    extends FrontendController(cc) with I18nSupport with Logging {
  import authActions._

  def showTrackRequests(page: Int, client: Option[String], status: Option[FilterFormStatus]): Action[AnyContent] =
    Action.async { implicit request =>
      withAuthorisedAsAgent { agent =>
        implicit val now: LocalDate = LocalDate.now()
        for {
          _ <- agentClientAuthorisationConnector.updateAltItsaAuthorisation(agent.arn).recover {
                case e =>
                  logger.warn("Error updating alt-itsa authorisations from track page", e)
                  ()
              }
          config <- trackPageConfig(page, agent, client, status)
        } yield {
          if (config.totalResults > 0 && page > config.numberOfPages) {
            Redirect(routes.AgentsRequestTrackingController.showTrackRequests(page = config.numberOfPages))
          } else {
            Ok(trackView(config))
          }
        }
      }
    }

  private def trackPageConfig(page: Int, agent: AuthorisedAgent, client: Option[String], status: Option[FilterFormStatus])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext,
    messages: Messages) = {
    val pageInfo = PageInfo(math.max(page, 1), appConfig.trackRequestsPerPage)
    for {
      trackResultsPage <- trackService.bindInvitationsAndRelationships(
                           agent.arn,
                           agent.isAllowlisted,
                           appConfig.trackRequestsShowLastDays,
                           pageInfo,
                           client,
                           status
                         )
    } yield {
      TrackPageConfig(
        trackResultsPage.results,
        appConfig.trackRequestsShowLastDays,
        featureFlags.enableTrackCancelAuth,
        pageInfo,
        trackResultsPage.totalResults,
        trackResultsPage.clientSet,
        client,
        status,
        None
      )
    }
  }

  def submitFilterTrackRequests: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { agent =>
      implicit val now: LocalDate = LocalDate.now()
      trackService
        .clientNames(agent.arn, agent.isAllowlisted, appConfig.trackRequestsShowLastDays)
        .flatMap { clientNames =>
          FilterTrackRequestsForm
            .form(clientNames)
            .bindFromRequest
            .fold(
              formWithError => {
                trackPageConfig(1, agent, None, None).map { config =>
                  Ok(trackView(config.copy(filterForm = Some(formWithError))))
                }
              },
              filterForm => {
                val filterAndStatus: (Option[String], Option[FilterFormStatus]) = request.body.asFormUrlEncoded
                  .fold(Seq.empty: Seq[String])(someMap => someMap.getOrElse("filter", Seq.empty))
                  .headOption match {
                  case Some("filter") => (filterForm.client, filterForm.status)
                  case Some("clear")  => (None, None)
                  case e              => throw new RuntimeException(s"unexpected value found in submitFilterTrackRequests $e")
                }
                Future successful Redirect(routes.AgentsRequestTrackingController.showTrackRequests(1, filterAndStatus._1, filterAndStatus._2))
              }
            )
        }
    }
  }

  def showResendLink: Action[AnyContent] =
    Action.async { implicit request =>
      withAuthorisedAsAgent { _ =>
        Future successful Ok(
          resendLinkView(ResendLinkPageConfig(
            appConfig.agentInvitationsFrontendExternalUrl,
            request.session.get("agentLink").getOrElse(""),
            request.session.get("clientType").getOrElse(""),
            request.session.get("expiryDate").getOrElse(""),
            request.session.get("service").flatMap(srv => Try(Service.forId(srv)).toOption),
            request.session.get("agencyEmail").getOrElse(""),
            routes.AgentsRequestTrackingController.showTrackRequests(1).url
          )))
      }
    }

  def submitToResendLink: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { agent =>
      trackInformationForm
        .bindFromRequest()
        .fold(
          _ => {
            logger.error("Error in form when redirecting to resend-link page.")
            Future successful BadRequest
          },
          data => {
            for {
              agentLink   <- invitationsService.createAgentLink(agent.arn, data.clientType)
              agencyEmail <- acaConnector.getAgencyEmail()
            } yield {
              val service = if (data.clientType.contains(Personal)) "personal" else data.service
              Redirect(routes.AgentsRequestTrackingController.showResendLink()).addingToSession(
                "agentLink"   -> agentLink,
                "clientType"  -> data.clientType.map(ClientType.fromEnum).getOrElse(""),
                "expiryDate"  -> data.expiryDate,
                "service"     -> service,
                "agencyEmail" -> agencyEmail
              )
            }
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
            logger.error("Error when redirecting to confirm cancel page.")
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
        case Some(service) =>
          val clientType = request.session.get("clientType").map(ClientType.toEnum).getOrElse(Personal)
          Future successful Ok(
            confirmCancelView(service, clientType, confirmCancelForm, routes.AgentsRequestTrackingController.showTrackRequests(1).url))
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
            case Some(service) =>
              val clientType = request.session.get("clientType").map(ClientType.toEnum).getOrElse(Personal)
              confirmCancelForm
                .bindFromRequest()
                .fold(
                  formWithErrors => {
                    Future successful Ok(
                      confirmCancelView(service, clientType, formWithErrors, routes.AgentsRequestTrackingController.showTrackRequests(1).url))
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
            logger.error("Error in form when redirecting to resend-link page.")
            Future successful BadRequest
          },
          (data: CancelAuthorisationForm) =>
            Future successful Redirect(routes.AgentsRequestTrackingController.showCancelAuthorisationConfirm())
              .addingToSession(
                "service"      -> data.service,
                "clientId"     -> data.clientId,
                "clientName"   -> data.clientName,
                "clientType"   -> data.clientType,
                "invitationId" -> data.invitationId,
                "status"       -> data.status
            )
        )
    }
  }

  def showCancelAuthorisationConfirm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      val service = Service.forId(request.session.get("service").getOrElse(throw new RuntimeException("Service not in session")))
      val clientType = request.session.get("clientType").map(ClientType.toEnum).getOrElse(Personal)
      Future successful Ok(
        confirmCancelAuthView(confirmCancelAuthorisationForm, service, clientType, routes.AgentsRequestTrackingController.showTrackRequests(1).url))
    }
  }

  def submitCancelAuthorisationConfirm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { agent =>
      val clientId = request.session.get("clientId").getOrElse("")
      val service = Service.forId(request.session.get("service").getOrElse(throw new RuntimeException("Service not in session")))
      val clientType = request.session.get("clientType").map(ClientType.toEnum).getOrElse(Personal)
      val status = request.session.get("status").getOrElse("Accepted")
      val isAltItsa = status == "Partialauth"
      confirmCancelAuthorisationForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Future successful Ok(
              confirmCancelAuthView(formWithErrors, service, clientType, routes.AgentsRequestTrackingController.showTrackRequests(1).url)),
          data =>
            if (data.value.getOrElse(true)) {
              for {
                response <- if (isAltItsa) acaConnector.setRelationshipEnded(agent.arn, clientId, service.id)
                           else deleteRelationshipForService(service, agent.arn, clientId)
                success = if (isAltItsa) response.exists(x => x) else response.isDefined
              } yield {
                if (success)
                  Redirect(routes.AgentsRequestTrackingController.showAuthorisationCancelled())
                else
                  Ok(cancelAuthProblemView())
              }
            } else Future successful Redirect(routes.AgentsRequestTrackingController.showTrackRequests())
        )
    }
  }

  def showAuthorisationCancelled: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      val service = Service.forId(request.session.get("service").getOrElse(throw new RuntimeException("Service not in session")))
      val clientName = request.session.get("clientName").getOrElse("")
      val clientType = request.session.get("clientType").map(ClientType.toEnum).getOrElse(Personal)
      val clientId = request.session.get("clientId").getOrElse("")
      Future successful Ok(authorisationCancelledView(service, clientId, clientName, clientType))
    }
  }

  def deleteRelationshipForService(service: Service, arn: Arn, clientId: String)(implicit hc: HeaderCarrier) =
    service match {
      case Service.MtdIt                => relationshipsConnector.deleteRelationshipItsa(arn, Nino(clientId))
      case Service.PersonalIncomeRecord => pirRelationshipConnector.deleteRelationship(arn, service, clientId)
      case Service.Vat                  => relationshipsConnector.deleteRelationshipVat(arn, Vrn(clientId))
      case Service.Trust                => relationshipsConnector.deleteRelationshipTrust(arn, Utr(clientId))
      case Service.TrustNT              => relationshipsConnector.deleteRelationshipTrustNT(arn, Urn(clientId))
      case Service.CapitalGains         => relationshipsConnector.deleteRelationshipCgt(arn, CgtRef(clientId))
      case Service.Ppt                  => relationshipsConnector.deleteRelationshipPpt(arn, PptRef(clientId))
    }

  val trackInformationForm: Form[TrackResendForm] = {
    Form(
      mapping(
        "service" -> text.verifying("Unsupported Service", service => supportedServices.exists(_.id == service)),
        "clientType" -> optional(
          text
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
        "service" -> text.verifying("Unsupported Service", service => supportedServices.exists(_.id == service)),
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
        "service"  -> text.verifying("Unsupported Service", service => supportedServices.exists(_.id == service)),
        "clientId" -> normalizedText.verifying(validateClientId),
        "clientType" -> text
          .verifying("Unsupported ClientType", clientType => ClientTypeForm.supportedClientTypes.contains(clientType)),
        "clientName"   -> text,
        "invitationId" -> text,
        "status"       -> text.verifying("Unexpected invitationStatus", status => status == "Accepted" || status == "Partialauth")
      )(CancelAuthorisationForm.apply)(CancelAuthorisationForm.unapply))
  }

  def radioChoice[A](invalidError: String): Constraint[Option[A]] = Constraint[Option[A]] { fieldValue: Option[A] =>
    if (fieldValue.isDefined)
      Valid
    else
      Invalid(ValidationError(invalidError))
  }

}
