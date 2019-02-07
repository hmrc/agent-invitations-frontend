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

import javax.inject.{Inject, Singleton}

import play.api.Configuration
import play.api.data.Form
import play.api.mvc._
import play.twirl.api.HtmlFormat
import play.twirl.api.HtmlFormat.Appendable
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.InvitationsConnector
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{PirRelationshipConnector, RelationshipsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.agentConfirmationForm
import uk.gov.hmrc.agentinvitationsfrontend.forms.ServiceTypeForm
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.util.toFuture
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.{business_select_service, cancelAuthorisation}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Vrn}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AgentLedDeAuthController @Inject()(
  withVerifiedPasscode: PasscodeVerification,
  authConnector: AuthConnector,
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  relationshipsService: RelationshipsService,
  relationshipsConnector: RelationshipsConnector,
  pirRelationshipConnector: PirRelationshipConnector,
  journeyStateCache: AgentMultiAuthorisationJourneyStateCache,
  val currentAuthorisationRequestCache: CurrentAuthorisationRequestCache,
  auditService: AuditService)(
  implicit featureFlags: FeatureFlags,
  externalUrls: ExternalUrls,
  val messagesApi: play.api.i18n.MessagesApi,
  configuration: Configuration,
  ec: ExecutionContext)
    extends BaseInvitationController(
      withVerifiedPasscode,
      authConnector,
      invitationsService,
      invitationsConnector,
      relationshipsService,
      journeyStateCache,
      currentAuthorisationRequestCache,
      auditService
    ) {

  val agentsLedDeAuthRootUrl: Call = routes.AgentLedDeAuthController.showClientType()

  val agentLedDeAuthRoot: Action[AnyContent] = Action { implicit request =>
    Redirect(agentsLedDeAuthRootUrl)
  }

  def showClientType: Action[AnyContent] = Action.async { implicit request =>
    handleGetClientType
  }

  def submitClientType: Action[AnyContent] = Action.async { implicit request =>
    handleSubmitClientType
  }

  def showSelectService: Action[AnyContent] = Action.async { implicit request =>
    handleGetSelectServicePage
  }

  def submitSelectService: Action[AnyContent] = Action.async { implicit request =>
    handleSubmitSelectService
  }

  def showIdentifyClient: Action[AnyContent] = Action.async { implicit request =>
    handleShowIdentifyClient
  }

  def submitIdentifyClient: Action[AnyContent] = Action.async { implicit request =>
    handleSubmitIdentifyClient
  }

  def showConfirmClient(): Action[AnyContent] = Action.async { implicit request =>
    handleShowConfirmClient
  }

  def submitConfirmClient(): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      currentAuthorisationRequestCache.fetch.flatMap {
        case Some(cache) =>
          //TODO: Fix this , its a duplicated call to getClientNameByService, we could cache the clientName instead of calling the endpoint twice
          invitationsService.getClientNameByService(cache.clientIdentifier, cache.service).flatMap { name =>
            val clientName = name.getOrElse("")
            agentConfirmationForm("cancel-authorisation.error.confirm-cancel.required")
              .bindFromRequest()
              .fold(
                formWithErrors => Ok(cancelAuthorisation.confirm_client(clientName, formWithErrors)),
                data => {
                  if (data.choice) {
                    Redirect(routes.AgentLedDeAuthController.showConfirmCancel())
                  } else {
                    Redirect(agentsLedDeAuthRootUrl)
                  }
                }
              )
          }
        case None => Redirect(agentsLedDeAuthRootUrl)
      }
    }
  }

  def showConfirmCancel: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      currentAuthorisationRequestCache.fetch.flatMap {
        case Some(cache) =>
          invitationsService.getClientNameByService(cache.clientIdentifier, cache.service).flatMap { name =>
            val clientName = name.getOrElse("")
            Ok(
              cancelAuthorisation.confirm_cancel(
                cache.service,
                clientName,
                agentConfirmationForm("cancel-authorisation.error.confirm-cancel.required")))
          }
        case None => Redirect(agentsLedDeAuthRootUrl)
      }
    }
  }

  def submitConfirmCancel: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, _) =>
      currentAuthorisationRequestCache.fetch.flatMap {
        case Some(cache) =>
          val service = cache.service
          val clientId = cache.clientIdentifier
          invitationsService.getClientNameByService(clientId, service).flatMap { name =>
            val clientName = name.getOrElse("")
            agentConfirmationForm("cancel-authorisation.error.confirm-cancel.required")
              .bindFromRequest()
              .fold(
                formWithErrors => {
                  Ok(cancelAuthorisation.confirm_cancel(service, clientName, formWithErrors))
                },
                data => {
                  if (data.choice) {
                    deleteRelationshipForService(service, arn, clientId).map {
                      case Some(true)  => Redirect(routes.AgentLedDeAuthController.showCancelled())
                      case Some(false) => NotFound //TODO: should be fixed in Sprint 36
                      case _           => InternalServerError //TODO: should be fixed in Sprint 36
                    }
                  } else {
                    Redirect(agentsLedDeAuthRootUrl)
                  }
                }
              )
          }
        case None => Redirect(agentsLedDeAuthRootUrl)
      }
    }
  }

  def showCancelled: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, _) =>
      currentAuthorisationRequestCache.fetch.flatMap {
        case Some(cache) =>
          val service = cache.service
          val clientId = cache.clientIdentifier

          val result = for {
            clientName <- invitationsService.getClientNameByService(clientId, service)
            agencyName <- invitationsService.getAgencyName(arn)
          } yield (clientName.getOrElse(""), agencyName)

          result.map {
            case (clientName, agencyName) =>
              Ok(cancelAuthorisation.authorisation_cancelled(service, clientName, agencyName, agentServicesAccountUrl))
          }

        case None => Redirect(agentsLedDeAuthRootUrl)
      }
    }
  }

  private def deleteRelationshipForService(service: String, arn: Arn, clientId: String)(implicit hc: HeaderCarrier) =
    service match {
      case HMRCMTDIT  => relationshipsConnector.deleteRelationshipItsa(arn, Nino(clientId))
      case HMRCPIR    => pirRelationshipConnector.deleteRelationship(arn, service, clientId)
      case HMRCMTDVAT => relationshipsConnector.deleteRelationshipVat(arn, Vrn(clientId))
      case e          => throw new Error(s"Unsupported service for deleting relationship: $e")
    }

  override def redirectOrShowConfirmClient(
    currentAuthorisationRequest: CurrentAuthorisationRequest,
    featureFlags: FeatureFlags)(body: => Future[Result])(implicit request: Request[_]): Future[Result] =
    withAuthorisedAsAgent { (_, _) =>
      currentAuthorisationRequest.service match {
        case HMRCMTDIT if featureFlags.enableMtdItToConfirm   => Redirect(confirmClientCall)
        case HMRCMTDVAT if featureFlags.enableMtdVatToConfirm => Redirect(confirmClientCall)
        case HMRCPIR if featureFlags.enableIrvToConfirm       => Redirect(confirmClientCall)
        case _                                                =>
          //TODO: Fix this loose check
          currentAuthorisationRequest.clientType match {
            case Some("personal") => Redirect(routes.AgentLedDeAuthController.showConfirmCancel())
            case Some("business") => Redirect(confirmClientCall)
            case _                => Redirect(clientTypeCall)
          }
      }
    }

  override def clientTypeCall: Call = agentsLedDeAuthRootUrl

  override def clientTypePage(form: Form[String])(implicit request: Request[_]): HtmlFormat.Appendable =
    cancelAuthorisation.client_type(form, clientTypes)

  override def selectServiceCall: Call = routes.AgentLedDeAuthController.showSelectService()

  override def submitServiceCall: Call = routes.AgentLedDeAuthController.submitSelectService()

  override def selectServicePage(
    form: Form[String] = ServiceTypeForm.form,
    enabledServices: Seq[(String, String)],
    basketFlag: Boolean)(implicit request: Request[_]): Appendable =
    cancelAuthorisation.select_service(form, enabledServices, false)

  override def identifyClientCall: Call = routes.AgentLedDeAuthController.showIdentifyClient()

  override def submitIdentifyClientCall: Call = routes.AgentLedDeAuthController.submitIdentifyClient()

  override def confirmClientCall: Call = routes.AgentLedDeAuthController.showConfirmClient()

  override def showConfirmClientPage(name: Option[String])(implicit request: Request[_]): Appendable =
    cancelAuthorisation.confirm_client(name.getOrElse(""), agentConfirmationForm("error.confirm-client.required"))
}
