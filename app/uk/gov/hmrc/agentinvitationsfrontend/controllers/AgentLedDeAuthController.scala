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
import play.api.data.Form
import play.api.mvc._
import play.api.{Configuration, Logger}
import play.twirl.api.HtmlFormat
import play.twirl.api.HtmlFormat.Appendable
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{InvitationsConnector, PirRelationshipConnector, RelationshipsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.agentConfirmationForm
import uk.gov.hmrc.agentinvitationsfrontend.forms.ServiceTypeForm
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.repository.AgentSessionCache
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.util.toFuture
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.{ClientTypePageConfig, SelectServicePageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.cancelAuthorisation
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
  val agentSessionCache: AgentSessionCache,
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
      agentSessionCache,
      relationshipsConnector,
      auditService
    ) {

  val agentsLedDeAuthRootUrl: Call = routes.AgentLedDeAuthController.showClientType()

  val agentLedDeAuthRoot: Action[AnyContent] = Action { implicit request =>
    Redirect(agentsLedDeAuthRootUrl)
  }

  private def ifShowDeAuthFlag(result: Future[Result]) =
    if (featureFlags.showAgentLedDeAuth) result
    else {
      Logger(getClass).warn("Agent led de authorisation feature is disabled.")
      Future successful NotImplemented
    }

  def showClientType: Action[AnyContent] = Action.async { implicit request =>
    ifShowDeAuthFlag(handleGetClientType(true))
  }

  def submitClientType: Action[AnyContent] = Action.async { implicit request =>
    ifShowDeAuthFlag(handleSubmitClientType)
  }

  def showSelectService: Action[AnyContent] = Action.async { implicit request =>
    ifShowDeAuthFlag(
      handleGetSelectServicePage(agentConfirmationForm("cancel-authorisation.error.business-service.required")))
  }

  def submitSelectPersonalService: Action[AnyContent] = Action.async { implicit request =>
    ifShowDeAuthFlag(handleSubmitSelectServicePersonal)
  }

  def submitSelectBusinessService: Action[AnyContent] = Action.async { implicit request =>
    ifShowDeAuthFlag(
      handleSubmitSelectServiceBusiness(agentConfirmationForm("cancel-authorisation.error.business-service.required")))
  }

  def showIdentifyClient: Action[AnyContent] = Action.async { implicit request =>
    ifShowDeAuthFlag(handleShowIdentifyClient)
  }

  def submitIdentifyClientItsa: Action[AnyContent] = Action.async { implicit request =>
    ifShowDeAuthFlag(identifyItsaClient)
  }

  def submitIdentifyClientIrv: Action[AnyContent] = Action.async { implicit request =>
    ifShowDeAuthFlag(identifyIrvClient)
  }

  def submitIdentifyClientVat: Action[AnyContent] = Action.async { implicit request =>
    ifShowDeAuthFlag(identifyVatClient)
  }

  def showConfirmClient(): Action[AnyContent] = Action.async { implicit request =>
    ifShowDeAuthFlag(handleShowConfirmClient)
  }

  def submitConfirmClient(): Action[AnyContent] = Action.async { implicit request =>
    ifShowDeAuthFlag(withAuthorisedAsAgent { (arn, _) =>
      agentSessionCache.fetch.flatMap {
        case Some(cache) =>
          val service = cache.service.getOrElse("")
          val clientId = cache.clientIdentifier.getOrElse("")
          //TODO: Fix this , its a duplicated call to getClientNameByService, we could cache the clientName instead of calling the endpoint twice
          invitationsService
            .getClientNameByService(clientId, service)
            .flatMap {
              name =>
                val clientName = name.getOrElse("")
                agentConfirmationForm("cancel-authorisation.error.confirm-cancel.required")
                  .bindFromRequest()
                  .fold(
                    formWithErrors =>
                      Ok(cancelAuthorisation.confirm_client(clientName, formWithErrors, identifyClientCall.url)),
                    data => {
                      if (data.choice) {
                        checkRelationshipExistsForService(arn, service, clientId).map {
                          case true  => Redirect(routes.AgentLedDeAuthController.showConfirmCancel())
                          case false => Redirect(routes.AgentsErrorController.notAuthorised())
                        }
                      } else {
                        Redirect(agentsLedDeAuthRootUrl)
                      }
                    }
                  )
            }
        case None => Redirect(agentsLedDeAuthRootUrl)
      }
    })
  }

  def showConfirmCancel: Action[AnyContent] = Action.async { implicit request =>
    ifShowDeAuthFlag(withAuthorisedAsAgent { (_, _) =>
      agentSessionCache.fetch.flatMap {
        case Some(cache) =>
          invitationsService
            .getClientNameByService(cache.clientIdentifier.getOrElse(""), cache.service.getOrElse(""))
            .flatMap { name =>
              val clientName = name.getOrElse("")
              Ok(cancelAuthorisation.confirm_cancel(
                cache.service.getOrElse(""),
                clientName,
                agentConfirmationForm("cancel-authorisation.error.confirm-cancel.required"),
                backLinkForConfirmCancelPage(cache.service.getOrElse(""))
              ))
            }
        case None => Redirect(agentsLedDeAuthRootUrl)
      }
    })
  }

  def submitConfirmCancel: Action[AnyContent] = Action.async { implicit request =>
    ifShowDeAuthFlag(withAuthorisedAsAgent { (arn, _) =>
      agentSessionCache.fetch.flatMap {
        case Some(cache) =>
          val service = cache.service.getOrElse("")
          val clientId = cache.clientIdentifier.getOrElse("")
          invitationsService.getClientNameByService(clientId, service).flatMap {
            name =>
              val clientName = name.getOrElse("")
              agentConfirmationForm("cancel-authorisation.error.confirm-cancel.required")
                .bindFromRequest()
                .fold(
                  formWithErrors => {
                    Ok(cancelAuthorisation
                      .confirm_cancel(service, clientName, formWithErrors, backLinkForConfirmCancelPage(service)))
                  },
                  data => {
                    if (data.choice) {
                      deleteRelationshipForService(service, arn, clientId).map {
                        case Some(true)  => Redirect(routes.AgentLedDeAuthController.showCancelled())
                        case Some(false) => NotFound //TODO: should be fixed in Sprint 36
                        case _           => Redirect(routes.AgentLedDeAuthController.responseFailed())
                      }
                    } else {
                      Redirect(agentsLedDeAuthRootUrl)
                    }
                  }
                )
          }
        case None => Redirect(agentsLedDeAuthRootUrl)
      }
    })
  }

  def showCancelled: Action[AnyContent] = Action.async { implicit request =>
    ifShowDeAuthFlag(withAuthorisedAsAgent { (arn, _) =>
      agentSessionCache.fetch.flatMap {
        case Some(cache) =>
          val service = cache.service.getOrElse("")
          val clientId = cache.clientIdentifier.getOrElse("")

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
    })
  }

  private def deleteRelationshipForService(service: String, arn: Arn, clientId: String)(implicit hc: HeaderCarrier) =
    service match {
      case HMRCMTDIT  => relationshipsConnector.deleteRelationshipItsa(arn, Nino(clientId))
      case HMRCPIR    => pirRelationshipConnector.deleteRelationship(arn, service, clientId)
      case HMRCMTDVAT => relationshipsConnector.deleteRelationshipVat(arn, Vrn(clientId))
      case e          => throw new Error(s"Unsupported service for deleting relationship: $e")
    }

  override def redirectOrShowConfirmClient(agentSession: AgentSession, featureFlags: FeatureFlags)(
    body: => Future[Result])(implicit request: Request[_]): Future[Result] =
    withAuthorisedAsAgent { (_, _) =>
      agentSession.service match {
        case Some(HMRCMTDIT) if featureFlags.enableMtdItToConfirm   => Redirect(confirmClientCall)
        case Some(HMRCMTDVAT) if featureFlags.enableMtdVatToConfirm => Redirect(confirmClientCall)
        case Some(HMRCPIR) if featureFlags.enableIrvToConfirm       => Redirect(confirmClientCall)
        case _                                                      =>
          //TODO: Fix this loose check
          agentSession.clientType match {
            case Some(ClientType.personal) => Redirect(routes.AgentLedDeAuthController.showConfirmCancel())
            case Some(ClientType.business) => Redirect(confirmClientCall)
            case _                         => Redirect(clientTypeCall)
          }
      }
    }

  def noClientFound(): Action[AnyContent] = Action.async { implicit request =>
    ifShowDeAuthFlag(withAuthorisedAsAgent { (arn, _) =>
      Ok(cancelAuthorisation.no_client_found())
    })
  }

  def responseFailed(): Action[AnyContent] = Action.async { implicit request =>
    ifShowDeAuthFlag(withAuthorisedAsAgent { (arn, _) =>
      Ok(cancelAuthorisation.response_failed())
    })
  }

  override def clientTypeCall: Call = agentsLedDeAuthRootUrl

  override def clientTypePage(form: Form[ClientType], backLinkUrl: String)(
    implicit request: Request[_]): HtmlFormat.Appendable =
    cancelAuthorisation.client_type(form, ClientTypePageConfig(None))

  override def selectServiceCall: Call = routes.AgentLedDeAuthController.showSelectService()

  override def submitServicePersonalCall: Call = routes.AgentLedDeAuthController.submitSelectPersonalService()

  override def submitServiceBusinessCall: Call = routes.AgentLedDeAuthController.submitSelectBusinessService()

  override def selectServicePage(
    form: Form[String] = ServiceTypeForm.form,
    enabledServices: Set[String],
    basketFlag: Boolean)(implicit request: Request[_]): Appendable =
    cancelAuthorisation.select_service(form, SelectServicePageConfig(false, featureFlags, enabledServices))

  override def businessSelectServicePage(
    form: Form[Confirmation] = agentConfirmationForm("cancel-authorisation.error.business-service.required"),
    basketFlag: Boolean,
    backLink: String)(implicit request: Request[_]): Appendable =
    cancelAuthorisation.business_select_service(form, basketFlag, submitServiceBusinessCall)

  override def identifyClientCall: Call = routes.AgentLedDeAuthController.showIdentifyClient()

  override def submitIdentifyClientItsaCall: Call = routes.AgentLedDeAuthController.submitIdentifyClientItsa()

  override def submitIdentifyClientIrvCall: Call = routes.AgentLedDeAuthController.submitIdentifyClientIrv()

  override def submitIdentifyClientVatCall: Call = routes.AgentLedDeAuthController.submitIdentifyClientVat()

  override def confirmClientCall: Call = routes.AgentLedDeAuthController.showConfirmClient()

  override def notMatchedCall: Call = routes.AgentLedDeAuthController.noClientFound()

  override def showConfirmClientPage(name: Option[String], backLinkUrl: String)(
    implicit request: Request[_]): Appendable =
    cancelAuthorisation
      .confirm_client(name.getOrElse(""), agentConfirmationForm("error.confirm-client.required"), backLinkUrl)
}
