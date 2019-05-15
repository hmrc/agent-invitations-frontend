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
import javax.inject.Inject
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.single
import play.api.i18n.I18nSupport
import play.api.mvc.{Call, Request, RequestHeader, Result}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentInvitationJourneyController.SelectPersonalServiceForm
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State.{SelectClientType, SelectServiceBusiness, SelectServicePersonal}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.{AuthorisedAgent, ClientType}
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators.lowerCaseText
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.{ClientTypePageConfig, SelectServicePageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.{client_type, select_service}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.play.fsm.JourneyController

import scala.concurrent.ExecutionContext

class AgentLedDeauthJourneyController @Inject()(
  override val journeyService: AgentLedDeauthJourneyService,
  authActions: AuthActionsImpl
)(
  implicit ec: ExecutionContext,
  configuration: Configuration,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  val messagesApi: play.api.i18n.MessagesApi)
    extends FrontendController with JourneyController[HeaderCarrier] with I18nSupport {

  import AgentLedDeauthJourneyController._
  import authActions._
  import uk.gov.hmrc.play.fsm.OptionalFormOps._

  val AsAgent: WithAuthorised[AuthorisedAgent] = { implicit request: Request[Any] =>
    withAuthorisedAsAgent(_)
  }

  val showClientType = action { implicit request =>
    whenAuthorised(AsAgent)(showSelectClientType)(display)
  }

  val submitClientType = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(clientTypeForm)(chosenClientType)
  }

  val showSelectService = actionShowStateWhenAuthorised(AsAgent) {
    case _: SelectServicePersonal =>
    case SelectServiceBusiness    =>
  }

  override def getCallFor(state: journeyService.model.State)(implicit request: Request[_]): Call = state match {
    case SelectClientType         => routes.AgentLedDeauthJourneyController.showClientType()
    case _: SelectServicePersonal => routes.AgentLedDeauthJourneyController.showSelectService()
    case SelectServiceBusiness    => routes.AgentLedDeauthJourneyController.showSelectService()
    case _                        => throw new Exception(s"Link not found for $state")

  }

  override def renderState(
    state: journeyService.model.State,
    breadcrumbs: journeyService.Breadcrumbs,
    formWithErrors: Option[Form[_]])(implicit request: Request[_]): Result = state match {

    case SelectClientType =>
      Ok(client_type(
        formWithErrors.or(clientTypeForm),
        ClientTypePageConfig(backLinkFor(breadcrumbs).url, routes.AgentLedDeauthJourneyController.submitClientType())))

    case SelectServicePersonal(enabledServices) =>
      Ok(
        select_service(
          formWithErrors.or(SelectPersonalServiceForm), // TODO
          SelectServicePageConfig(
            basketFlag = false,
            featureFlags,
            enabledServices,
            routes.AgentInvitationJourneyController.submitPersonalSelectService(),
            backLinkFor(breadcrumbs).url,
            routes.AgentInvitationJourneyController.showReviewAuthorisations()
          )
        ))

  }

  override def context(implicit rh: RequestHeader): HeaderCarrier = ???
}

object AgentLedDeauthJourneyController {
  val clientTypeForm: Form[ClientType] = Form(
    single(
      "clientType" -> lowerCaseText.verifying("client.type.invalid", Set("personal", "business").contains _)
    ).transform(ClientType.toEnum, ClientType.fromEnum)
  )
}
