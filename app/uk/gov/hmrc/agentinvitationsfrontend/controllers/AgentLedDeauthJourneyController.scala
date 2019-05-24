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
import play.api.{Configuration, Logger}
import play.api.data.Form
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.forms.{IrvClientForm, ItsaClientForm, VatClientForm, _}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services.{InvitationsService, RelationshipsService}
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.cancelAuthorisation.SelectServicePageConfig
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.{CannotCreateRequestConfig, ClientTypePageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.play.fsm.JourneyController

import scala.concurrent.ExecutionContext

class AgentLedDeauthJourneyController @Inject()(
  override val journeyService: AgentLedDeauthJourneyService,
  authActions: AuthActionsImpl,
  invitationsService: InvitationsService,
  relationshipsService: RelationshipsService
)(
  implicit ec: ExecutionContext,
  configuration: Configuration,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  val messagesApi: play.api.i18n.MessagesApi)
    extends FrontendController with JourneyController[HeaderCarrier] with I18nSupport {

  override def context(implicit rh: RequestHeader): HeaderCarrier = hc

  import authActions._
  import cancelAuthorisation._
  import invitationsService._
  import relationshipsService._
  import uk.gov.hmrc.play.fsm.OptionalFormOps._

  val AsAgent: WithAuthorised[AuthorisedAgent] = { implicit request: Request[Any] =>
    withAuthorisedAsAgent(_)
  }

  val agentLedDeauthRoot: Action[AnyContent] = Action(Redirect(routes.AgentLedDeauthJourneyController.showClientType()))

  val showClientType: Action[AnyContent] =
    if (featureFlags.showAgentLedDeAuth)
      actionShowStateWhenAuthorised(AsAgent) {
        case SelectClientType =>
      } else {
      Logger(getClass).warn("Agent led de authorisation feature is disabled.")
      Action(NotImplemented)
    }

  val submitClientType: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ClientTypeForm.form)(chosenClientType)
  }

  val showSelectService: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: SelectServicePersonal =>
    case SelectServiceBusiness    =>
  }

  val submitPersonalService: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ServiceTypeForm.form)(
      chosenPersonalService(featureFlags.showHmrcMtdIt, featureFlags.showPersonalIncome, featureFlags.showHmrcMtdVat))
  }

  val submitBusinessService: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(CommonConfirmationForms.serviceBusinessForm)(
      chosenBusinessService(featureFlags.showHmrcMtdVat))
  }

  val showIdentifyClient: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: IdentifyClientPersonal =>
    case IdentifyClientBusiness    =>
  }

  val submitIdentifyItsaClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ItsaClientForm.form(featureFlags.showHmrcMtdIt))(
      submitIdentifyClientItsa(checkPostcodeMatches, getClientNameByService, hasActiveRelationshipFor)(
        featureFlags.showKfcMtdIt,
        featureFlags.enableMtdItToConfirm))
  }

  val submitIdentifyIrvClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(IrvClientForm.form(featureFlags.showPersonalIncome))(
      submitIdentifyClientIrv(checkCitizenRecordMatches, getClientNameByService, hasActiveRelationshipFor)(
        featureFlags.showKfcPersonalIncome,
        featureFlags.enableIrvToConfirm
      )
    )
  }

  val submitIdentifyVatClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(VatClientForm.form(featureFlags.showKfcMtdVat))(
      submitIdentifyClientVat(checkVatRegistrationDateMatches, getClientNameByService, hasActiveRelationshipFor)(
        featureFlags.showKfcMtdVat,
        featureFlags.enableMtdVatToConfirm
      )
    )
  }

  val showConfirmClient: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: ConfirmClientItsa        =>
    case _: ConfirmClientIrv         =>
    case _: ConfirmClientPersonalVat =>
    case _: ConfirmClientBusiness    =>
  }

  val submitConfirmClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(CommonConfirmationForms.confirmCancelForm)(
      clientConfirmed(hasActiveRelationshipFor))
  }

  val showConfirmCancel: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: ConfirmCancel =>
  }

  val submitConfirmCancel: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(CommonConfirmationForms.confirmCancelForm)(
      cancelConfirmed(deleteRelationshipForService, getAgencyName))
  }

  val showAuthorisationCancelled: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: AuthorisationCancelled =>
  }

  val showKnownFactNotMatched: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case KnownFactNotMatched =>
  }

  val showNotSignedUp: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: NotSignedUp =>
  }

  val showCannotCreateRequest: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case CannotCreateRequest =>
  }

  val showNotAuthorised: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: NotAuthorised =>
  }

  val showResponseFailed: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case ResponseFailed =>
  }

  override def getCallFor(state: journeyService.model.State)(implicit request: Request[_]): Call = state match {
    case SelectClientType            => routes.AgentLedDeauthJourneyController.showClientType()
    case _: SelectServicePersonal    => routes.AgentLedDeauthJourneyController.showSelectService()
    case SelectServiceBusiness       => routes.AgentLedDeauthJourneyController.showSelectService()
    case _: IdentifyClientPersonal   => routes.AgentLedDeauthJourneyController.showIdentifyClient()
    case IdentifyClientBusiness      => routes.AgentLedDeauthJourneyController.showIdentifyClient()
    case _: ConfirmClientItsa        => routes.AgentLedDeauthJourneyController.showConfirmClient()
    case _: ConfirmClientIrv         => routes.AgentLedDeauthJourneyController.showConfirmClient()
    case _: ConfirmClientPersonalVat => routes.AgentLedDeauthJourneyController.showConfirmClient()
    case _: ConfirmClientBusiness    => routes.AgentLedDeauthJourneyController.showConfirmClient()
    case _: ConfirmCancel            => routes.AgentLedDeauthJourneyController.showConfirmCancel()
    case _: AuthorisationCancelled   => routes.AgentLedDeauthJourneyController.showAuthorisationCancelled()
    case KnownFactNotMatched         => routes.AgentLedDeauthJourneyController.showKnownFactNotMatched()
    case _: NotSignedUp              => routes.AgentLedDeauthJourneyController.showNotSignedUp()
    case CannotCreateRequest         => routes.AgentLedDeauthJourneyController.showCannotCreateRequest()
    case _: NotAuthorised            => routes.AgentLedDeauthJourneyController.showNotAuthorised()
    case ResponseFailed              => routes.AgentLedDeauthJourneyController.showResponseFailed()
    case _                           => throw new Exception(s"Link not found for $state")
  }

  override def renderState(
    state: journeyService.model.State,
    breadcrumbs: journeyService.Breadcrumbs,
    formWithErrors: Option[Form[_]])(implicit request: Request[_]): Result = state match {

    case SelectClientType => {
      println(s"BREADCRUMBS at the beginning $breadcrumbs")
      def backLinkForClientType(implicit request: Request[_]): String =
        breadcrumbs.headOption.fold(s"${externalUrls.agentServicesAccountUrl}/agent-services-account")(
          getCallFor(_).url)

      Ok(
        client_type(
          formWithErrors.or(ClientTypeForm.form),
          ClientTypePageConfig(backLinkForClientType, routes.AgentLedDeauthJourneyController.submitClientType())))
    }

    case SelectServicePersonal(enabledServices) =>
      Ok(
        select_service(
          formWithErrors.or(ServiceTypeForm.form),
          SelectServicePageConfig(
            featureFlags,
            enabledServices,
            routes.AgentLedDeauthJourneyController.submitPersonalService(),
            backLinkFor(breadcrumbs).url
          )
        ))

    case SelectServiceBusiness =>
      Ok(
        business_select_service(
          formWithErrors.or(CommonConfirmationForms.serviceBusinessForm),
          routes.AgentLedDeauthJourneyController.submitBusinessService(),
          backLinkFor(breadcrumbs).url
        )
      )

    case IdentifyClientPersonal(service) =>
      service match {
        case Services.HMRCMTDIT =>
          Ok(
            identify_client_itsa(
              ItsaClientForm.form(featureFlags.showKfcMtdIt),
              featureFlags.showKfcMtdIt,
              routes.AgentLedDeauthJourneyController.submitIdentifyItsaClient(),
              backLinkFor(breadcrumbs).url
            ))
        case Services.HMRCPIR =>
          Ok(
            identify_client_irv(
              IrvClientForm.form(featureFlags.showKfcPersonalIncome),
              featureFlags.showKfcPersonalIncome,
              routes.AgentLedDeauthJourneyController.submitIdentifyIrvClient(),
              backLinkFor(breadcrumbs).url
            )
          )
        case Services.HMRCMTDVAT =>
          Ok(
            identify_client_vat(
              formWithErrors.or(VatClientForm.form(featureFlags.showKfcMtdVat)),
              featureFlags.showKfcMtdVat,
              routes.AgentLedDeauthJourneyController.submitIdentifyVatClient(),
              backLinkFor(breadcrumbs).url
            ))
      }

    case IdentifyClientBusiness =>
      Ok(
        identify_client_vat(
          formWithErrors.or(VatClientForm.form(featureFlags.showKfcMtdVat)),
          featureFlags.showKfcMtdVat,
          routes.AgentLedDeauthJourneyController.submitBusinessService(),
          backLinkFor(breadcrumbs).url
        ))

    case ConfirmClientItsa(clientName, _) =>
      Ok(
        confirm_client(
          clientName.getOrElse(""),
          formWithErrors.or(CommonConfirmationForms.confirmCancelForm),
          routes.AgentLedDeauthJourneyController.submitConfirmClient(),
          backLinkFor(breadcrumbs).url
        ))

    case ConfirmClientIrv(clientName, _) =>
      Ok(
        confirm_client(
          clientName.getOrElse(""),
          formWithErrors.or(CommonConfirmationForms.confirmCancelForm),
          routes.AgentLedDeauthJourneyController.submitConfirmClient(),
          backLinkFor(breadcrumbs).url
        ))

    case ConfirmClientPersonalVat(clientName, _) =>
      Ok(
        confirm_client(
          clientName.getOrElse(""),
          formWithErrors.or(CommonConfirmationForms.confirmCancelForm),
          routes.AgentLedDeauthJourneyController.submitConfirmClient(),
          backLinkFor(breadcrumbs).url
        ))

    case ConfirmClientBusiness(clientName, _) =>
      Ok(
        confirm_client(
          clientName.getOrElse(""),
          formWithErrors.or(CommonConfirmationForms.confirmCancelForm),
          routes.AgentLedDeauthJourneyController.submitConfirmClient(),
          backLinkFor(breadcrumbs).url
        ))

    case ConfirmCancel(service, clientName, _) => {
      println(s"BREADCRUMBS at nearly the end $breadcrumbs")

      Ok(
        confirm_cancel(
          service,
          clientName.getOrElse(""),
          formWithErrors.or(CommonConfirmationForms.confirmCancelForm),
          routes.AgentLedDeauthJourneyController.submitConfirmCancel(),
          backLinkFor(breadcrumbs).url
        ))
    }

    case AuthorisationCancelled(service, clientName, agencyName) => {
      println(s"BREADCRUMBS at the end $breadcrumbs")
      journeyService.cleanBreadcrumbs(_ => Nil)
      Ok(
        authorisation_cancelled(
          service,
          clientName.getOrElse(""),
          agencyName,
          s"${externalUrls.agentServicesAccountUrl}/agent-services-account"))
    }

    case KnownFactNotMatched =>
      Ok(no_client_found())

    case NotSignedUp(service) =>
      Ok(not_signed_up(service, hasRequests = false))

    case CannotCreateRequest =>
      Ok(
        cannot_create_request(
          CannotCreateRequestConfig(hasRequests = false, fromFastTrack = false, backLinkFor(breadcrumbs).url)))

    case NotAuthorised(service) => Ok(not_authorised(service))

    case ResponseFailed => Ok(response_failed())

  }
}
