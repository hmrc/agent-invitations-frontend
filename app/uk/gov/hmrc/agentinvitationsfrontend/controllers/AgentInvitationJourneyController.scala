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
import javax.inject.{Inject, Named, Singleton}
import play.api.data.Form
import play.api.data.Forms.single
import play.api.i18n.I18nSupport
import play.api.mvc._
import play.api.{Configuration, Environment}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.InvitationsConnector
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators.lowerCaseText
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.auth.core.AuthConnector

import scala.concurrent.{ExecutionContext, Future}

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
    extends BaseJourneyController(journeyService) with I18nSupport with AuthActions {

  import AgentInvitationJourneyController._
  import journeyService.model.States._
  import journeyService.model.{Error, Errors, State, Transitions}

  /* Here we decide how to turn HTTP request into transition of the current state */
  val agentsRoot: Action[AnyContent] = simpleAction(Transitions.startJourney)

  /* Here we handle unexpected transition errors */
  override def handleError(error: Error)(implicit request: Request[_]): Future[Result] = error match {
    case Errors.UnknownState     => Future.successful(Redirect(routes.AgentsInvitationController.agentsRoot()))
    case Errors.GenericError(ex) => Future.failed(throw ex)
  }

  /* Here we decide how to render our current state */
  override def renderState(state: State)(implicit request: Request[_]): Result = state match {

    case Start            => Redirect(routes.AgentsInvitationController.showClientType())
    case SelectClientType => Ok(client_type(SelectClientTypeForm, null, null, null))
  }

}

object AgentInvitationJourneyController {

  val SelectClientTypeForm = Form(
    single(
      "clientType" -> lowerCaseText.verifying("client.type.invalid", Set("personal", "business").contains _)
    )
  )

}
