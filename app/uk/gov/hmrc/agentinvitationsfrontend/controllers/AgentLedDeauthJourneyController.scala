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
import play.api.data.Forms.{mapping, optional, single, text}
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ValidateHelper.optionalIf
import uk.gov.hmrc.agentinvitationsfrontend.forms.{IrvClientForm, ItsaClientForm, VatClientForm}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.State.ConfirmClientBusinessVat
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.supportedServices
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services.{InvitationsService, RelationshipsService}
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.{CannotCreateRequestConfig, ClientTypePageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.cancelAuthorisation.SelectServicePageConfig
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.cancelAuthorisation._
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

  import AgentLedDeauthJourneyController._
  import authActions._
  import uk.gov.hmrc.play.fsm.OptionalFormOps._
  import invitationsService._
  import relationshipsService._
  import cancelAuthorisation._

  val AsAgent: WithAuthorised[AuthorisedAgent] = { implicit request: Request[Any] =>
    withAuthorisedAsAgent(_)
  }

  val agentLedDeauthRoot: Action[AnyContent] = Action(Redirect(routes.AgentLedDeauthJourneyController.showClientType()))

  val showClientType: Action[AnyContent] = action { implicit request =>
    whenAuthorised(AsAgent)(showSelectClientType)(display)
  }

  val submitClientType: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(clientTypeForm)(chosenClientType)
  }

  val showSelectService: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: SelectServicePersonal =>
    case SelectServiceBusiness    =>
  }

  val submitPersonalService: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(servicePersonalForm)(chosenPersonalService)
  }

  val submitBusinessService: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(serviceBusinessForm)(chosenBusinessService)
  }

  val showIdentifyClient: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: IdentifyClientPersonal =>
    case IdentifyClientBusiness    =>
  }

  val submitIdentifyItsaClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(identifyClientItsaForm(featureFlags.showHmrcMtdIt))(
      submitIdentifyClientItsa(checkPostcodeMatches, getClientNameByService))
  }

  val submitIdentifyIrvClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(identifyClientIrvForm(featureFlags.showPersonalIncome))(
      submitIdentifyClientIrv(checkCitizenRecordMatches, getClientNameByService)
    )
  }

  val submitIdentifyVatClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(identifyClientVatForm(featureFlags.showKfcMtdVat))(
      submitIdentityClientVat(checkVatRegistrationDateMatches, getClientNameByService)
    )
  }

  val showConfirmClient: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: ConfirmClientItsa        =>
    case _: ConfirmClientIrv         =>
    case _: ConfirmClientPersonalVat =>
    case _: ConfirmClientBusinessVat =>
  }

  val submitConfirmClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(cancelForm)(clientConfirmed(hasActiveRelationshipFor))
  }

  val showConfirmCancel: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: ConfirmCancel =>
  }

  val submitConfirmCancel: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(cancelForm)(cancelConfirmed(deleteRelationshipForService, getAgencyName))
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

    case SelectClientType =>
      Ok(client_type(
        formWithErrors.or(clientTypeForm),
        ClientTypePageConfig(backLinkFor(breadcrumbs).url, routes.AgentLedDeauthJourneyController.submitClientType())))

    case SelectServicePersonal(enabledServices) =>
      Ok(
        select_service(
          formWithErrors.or(servicePersonalForm),
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
          formWithErrors.or(serviceBusinessForm),
          routes.AgentLedDeAuthController.showClientType(), //change
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
              routes.AgentLedDeAuthController.showSelectService().url
            ))
        case Services.HMRCPIR =>
          Ok(
            identify_client_irv(
              IrvClientForm.form(featureFlags.showKfcPersonalIncome),
              featureFlags.showKfcPersonalIncome,
              routes.AgentLedDeauthJourneyController.submitIdentifyIrvClient(),
              routes.AgentLedDeAuthController.showSelectService().url
            )
          )
        case Services.HMRCMTDVAT =>
          Ok(
            identify_client_vat(
              formWithErrors.or(VatClientForm.form(featureFlags.showKfcMtdVat)),
              featureFlags.showKfcMtdVat,
              routes.AgentLedDeauthJourneyController.submitIdentifyVatClient(),
              routes.AgentLedDeAuthController.showSelectService().url
            ))
      }

    case IdentifyClientBusiness =>
      Ok(
        identify_client_vat(
          formWithErrors.or(VatClientForm.form(featureFlags.showKfcMtdVat)),
          featureFlags.showKfcMtdVat,
          routes.AgentLedDeAuthController.submitIdentifyClientVat(),
          routes.AgentLedDeAuthController.showSelectService().url
        ))

    case ConfirmClientItsa(clientName, _, _) =>
      Ok(confirm_client(clientName.getOrElse(""), formWithErrors.or(cancelForm), backLinkFor(breadcrumbs).url))

    case ConfirmClientIrv(clientName, _, _) =>
      Ok(confirm_client(clientName.getOrElse(""), formWithErrors.or(cancelForm), backLinkFor(breadcrumbs).url))

    case ConfirmClientPersonalVat(clientName, _, _) =>
      Ok(confirm_client(clientName.getOrElse(""), formWithErrors.or(cancelForm), backLinkFor(breadcrumbs).url))

    case ConfirmClientBusiness(clientName, _, _) =>
      Ok(confirm_client(clientName.getOrElse(""), formWithErrors.or(cancelForm), backLinkFor(breadcrumbs).url))

    case ConfirmCancel(service, clientName, _) =>
      Ok(confirm_cancel(service, clientName.getOrElse(""), cancelForm, backLinkFor(breadcrumbs).url))

    case AuthorisationCancelled(service, clientName, agencyName) =>
      Ok(
        authorisation_cancelled(
          service,
          clientName.getOrElse(""),
          agencyName,
          s"${externalUrls.agentServicesAccountUrl}/agent-services-account"))

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

  override def context(implicit rh: RequestHeader): HeaderCarrier = hc
}

object AgentLedDeauthJourneyController {

  val clientTypeForm: Form[ClientType] = Form(
    single(
      "clientType" -> lowerCaseText.verifying("client.type.invalid", Set("personal", "business").contains _)
    ).transform(ClientType.toEnum, ClientType.fromEnum)
  )

  val servicePersonalForm: Form[String] = Form(
    single(
      "serviceType" -> text.verifying("service.type.invalid", supportedServices.contains _)
    )
  )

  def agentConfirmationForm(errorMessage: String): Form[Confirmation] =
    Form(
      mapping(
        "accepted" -> optional(normalizedText)
          .transform[String](_.getOrElse(""), s => Some(s))
          .verifying(confirmationChoice(errorMessage))
      )(choice => Confirmation(choice.toBoolean))(confirmation => Some(confirmation.choice.toString)))

  val serviceBusinessForm: Form[Confirmation] = agentConfirmationForm(
    "cancel-authorisation.error.business-service.required")

  def identifyClientItsaForm(showKfcMtdIt: Boolean): Form[ItsaClient] =
    Form(
      mapping(
        "clientIdentifier" -> normalizedText.verifying(validNino()),
        "postcode"         -> postcodeMapping(showKfcMtdIt)
      )(ItsaClient.apply)(ItsaClient.unapply))

  def identifyClientIrvForm(showKfcPersonalIncome: Boolean): Form[IrvClient] =
    Form(
      mapping(
        "clientIdentifier" -> normalizedText.verifying(validNino()),
        "dob"              -> dateOfBirthMapping(showKfcPersonalIncome)
      )(IrvClient.apply)(IrvClient.unapply))

  def identifyClientVatForm(showKfcMtdVat: Boolean): Form[VatClient] =
    Form(
      mapping(
        "clientIdentifier" -> normalizedText.verifying(validVrn),
        "registrationDate" -> optionalIf(showKfcMtdVat, DateFieldHelper.dateFieldsMapping(validVatDateFormat))
      )(VatClient.apply)(VatClient.unapply))

  val cancelForm: Form[Confirmation] = agentConfirmationForm("cancel-authorisation.error.confirm-cancel.required")

}
