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

package uk.gov.hmrc.agentinvitationsfrontend.controllers.journeys

import javax.inject.{Inject, Named, Singleton}
import play.api.data.Form
import play.api.data.Forms.single
import play.api.mvc.Call
import play.api.{Configuration, Environment}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.InvitationsConnector
import uk.gov.hmrc.agentinvitationsfrontend.controllers._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators.lowerCaseText
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.ClientTypePageConfig
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.auth.core.AuthConnector

import scala.concurrent.ExecutionContext

@Singleton
class AgentInvitationJourneyController @Inject()(
  @Named("invitation.expiryDuration") expiryDuration: String,
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  relationshipsService: RelationshipsService,
  auditService: AuditService,
  val env: Environment,
  val authConnector: AuthConnector,
  val continueUrlActions: ContinueUrlActions,
  val withVerifiedPasscode: PasscodeVerification,
  override val journeyService: AgentInvitationJourneyService)(
  implicit configuration: Configuration,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  val messagesApi: play.api.i18n.MessagesApi,
  ec: ExecutionContext)
    extends JourneyController {

  import AgentInvitationJourneyController._
  import journeyService.model.States._
  import journeyService.model.{Error, Errors, State, Transitions}

  /* Here we decide how to handle HTTP request and transition the state of the journey */
  val agentsRoot = simpleAction(Transitions.startJourney)
  val showClientType = authorisedAgentAction(Transitions.showSelectClientType)
  val submitClientType =
    authorisedAgentActionWithForm(SelectClientTypeForm)(Transitions.selectedClientType)(
      SelectClientTypeFormValidationFailed)
  val showSelectService = authorisedAgentAction(Transitions.showSelectService)

  /* Here we handle errors thrown during state transition */
  override def handleError(error: Error): Route = { implicit request =>
    error match {
      case Errors.TransitionNotAllowed(origin, breadcrumbs, _) =>
        renderState(origin, breadcrumbs)(request) // renders current state back
      case Errors.GenericError(ex) => throw ex //delegates to the global ErrorHandler
    }
  }

  /* Here we decide how to render or where to redirect after state transition */
  override def renderState(state: State, breadcrumbs: List[State]): Route = { implicit request =>
    state match {
      case Start => Redirect(getCallFor(SelectClientType))
      case SelectClientType =>
        Ok(client_type(SelectClientTypeForm, ClientTypePageConfig(backLinkFor(breadcrumbs))))
      case SelectedClientType(ClientType.personal) => Redirect(getCallFor(SelectPersonalService))
      case SelectedClientType(ClientType.business) => Redirect(getCallFor(SelectBusinessService))
      case SelectPersonalService =>
        Ok(client_type(SelectClientTypeForm, ClientTypePageConfig(backLinkFor(breadcrumbs))))
      case SelectBusinessService => ???
    }
  }

  /* Here we handle form validation errors */
  override def handleFormValidationError(error: FormValidationError, breadcrumbs: List[State]): Route = {
    implicit request =>
      error match {
        case SelectClientTypeFormValidationFailed(form) =>
          Ok(client_type(form, ClientTypePageConfig(backLinkFor(breadcrumbs))))
      }
  }

  private def getCallFor(state: State): Call = state match {
    case Start                 => routes.AgentInvitationJourneyController.agentsRoot()
    case SelectClientType      => routes.AgentInvitationJourneyController.showClientType()
    case SelectPersonalService => routes.AgentInvitationJourneyController.showSelectService()
    case SelectBusinessService => routes.AgentInvitationJourneyController.showSelectService()
  }

  private def backLinkFor(breadcrumbs: List[State]): Option[String] =
    breadcrumbs.headOption.map(getCallFor).map(_.url)

}

object AgentInvitationJourneyController {

  val SelectClientTypeForm: Form[ClientType] = Form(
    single(
      "clientType" -> lowerCaseText.verifying("client.type.invalid", Set("personal", "business").contains _)
    ).transform(ClientType.toEnum, ClientType.fromEnum)
  )

  case class SelectClientTypeFormValidationFailed(form: Form[ClientType]) extends FormValidationError

}
