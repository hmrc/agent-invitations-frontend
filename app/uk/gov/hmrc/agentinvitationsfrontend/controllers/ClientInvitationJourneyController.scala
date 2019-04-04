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
import play.api.mvc.{Call, Request, Result}
import play.api.i18n.I18nSupport
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.InvitationsConnector
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State.{Consent, NotFoundInvitation, WarmUp}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.views.clients.{ConfirmTermsPageConfig, WarmUpPageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.clients.{confirm_terms_multi, not_found_invitation, warm_up}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.play.fsm.JourneyController

import scala.concurrent.ExecutionContext

@Singleton
class ClientInvitationJourneyController @Inject()(
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  authActions: AuthActions,
  override val journeyService: ClientInvitationJourneyService)(
  implicit configuration: Configuration,
  val externalUrls: ExternalUrls,
  val messagesApi: play.api.i18n.MessagesApi,
  featureFlags: FeatureFlags,
  ec: ExecutionContext)
    extends FrontendController with JourneyController with I18nSupport {

  import authActions._
  import invitationsConnector._
  import invitationsService._
  import journeyService.model.{State, Transitions}

  override val root: Call = routes.ClientInvitationJourneyController.warmUp("", "", "")

  val AsClient: WithAuthorised[AuthorisedClient] = { implicit request: Request[Any] =>
    withAuthorisedAsAnyClient
  }

  /* Here we decide how to handle HTTP request and transition the state of the journey */

  def warmUp(clientType: String, uid: String, normalisedAgentName: String) =
    action { implicit request =>
      authorised(AsClient)(
        Transitions.start(clientType: String, uid: String, normalisedAgentName: String)(getAgentReferenceRecord)(
          getAgencyName))(display)
    }

  def submitWarmUp = action { implicit request =>
    authorised(AsClient)(Transitions.submitWarmUp(invitationsConnector.getAllClientInvitationsInfoForAgentAndStatus))(redirect)
  }

  def showConsent = showCurrentStateWhenAuthorised(AsClient) {
    case _: Consent =>
  }

  def showNotFoundInvitation = showCurrentStateWhenAuthorised(AsClient) {
    case NotFoundInvitation =>
  }

  /* Here we map states to the GET endpoints for redirecting and back linking */
  override def getCallFor(state: State)(implicit request: Request[_]): Call = state match {
    case WarmUp(clientType, uid, agentName) =>
      routes.ClientInvitationJourneyController.warmUp(ClientType.fromEnum(clientType), uid, agentName)
    case NotFoundInvitation => routes.ClientInvitationJourneyController.showNotFoundInvitation
    case Consent => routes.ClientInvitationJourneyController.showConsent
    case _                  => throw new Exception(s"Link not found for $state")
  }

  /* Here we decide what to render after state transition */
  override def renderState(state: State, breadcrumbs: List[State], formWithErrors: Option[Form[_]])(
    implicit request: Request[_]): Result = state match {

    case WarmUp(clientType, uid, agentName) =>
      Ok(
        warm_up(
          WarmUpPageConfig(
            agentName,
            clientType,
            uid,
            routes.ClientInvitationJourneyController.warmUp(ClientType.fromEnum(clientType), uid, agentName),
            routes.ClientInvitationJourneyController.warmUp(ClientType.fromEnum(clientType), uid, agentName)
          )))

    case NotFoundInvitation =>
      val serviceMessageKey = request.session.get("clientService").getOrElse("Service Is Missing")
      Ok(not_found_invitation(serviceMessageKey))

    case Consent(clientType, agentName, uid, consents) =>
      Ok(confirm_terms_multi(form, ConfirmTermsPageConfig(agentName, clientName, uid, consents, )))

  }
}
