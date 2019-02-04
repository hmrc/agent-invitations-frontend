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
import play.api.mvc.{Action, AnyContent, Call, Request}
import play.twirl.api.HtmlFormat
import play.twirl.api.HtmlFormat.Appendable
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.forms.ServiceTypeForm
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.cancelAuthorisation
import uk.gov.hmrc.auth.core.AuthConnector

import scala.concurrent.ExecutionContext

@Singleton
class AgentCancelAuthorisationController @Inject()(
  withVerifiedPasscode: PasscodeVerification,
  authConnector: AuthConnector,
  invitationsService: InvitationsService,
  relationshipsService: RelationshipsService,
  journeyStateCache: AgentMultiAuthorisationJourneyStateCache,
  val currentAuthorisationRequestCache: CurrentAuthorisationRequestCache,
  auditService: AuditService)(
  implicit featureFlags: FeatureFlags,
  externalUrls: ExternalUrls,
  messagesApi: play.api.i18n.MessagesApi,
  configuration: Configuration,
  ec: ExecutionContext)
    extends BaseInvitationController(
      withVerifiedPasscode,
      authConnector,
      invitationsService,
      relationshipsService,
      journeyStateCache,
      currentAuthorisationRequestCache,
      auditService) {

  val agentsLedDeAuthRootUrl: Call = routes.AgentCancelAuthorisationController.showClientType()

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

  override def clientTypeCall: Call = agentsLedDeAuthRootUrl

  override def clientTypePage(form: Form[String])(implicit request: Request[_]): HtmlFormat.Appendable =
    cancelAuthorisation.client_type(form, clientTypes)

  override def selectServiceCall: Call = routes.AgentCancelAuthorisationController.showSelectService()

  override def selectServicePage(
    form: Form[String] = ServiceTypeForm.form,
    enabledServices: Seq[(String, String)],
    basketFlag: Boolean)(implicit request: Request[_]): Appendable =
    cancelAuthorisation.select_service(form, enabledServices, false)

  override def identifyClientCall: Call = routes.AgentCancelAuthorisationController.showIdentifyClient()
}
