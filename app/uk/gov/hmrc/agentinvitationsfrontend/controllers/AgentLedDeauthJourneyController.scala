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
import play.api.mvc.{Call, Request, RequestHeader, Result}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.forms.{IrvClientForm, ItsaClientForm, VatClientForm}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.supportedServices
import uk.gov.hmrc.agentinvitationsfrontend.models.{AuthorisedAgent, ClientType, Confirmation, ItsaClient, Services}
import uk.gov.hmrc.agentinvitationsfrontend.services.InvitationsService
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators.{confirmationChoice, lowerCaseText, normalizedText, postcodeMapping, validNino}
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.{ClientTypePageConfig, SelectServicePageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.cancelAuthorisation.{business_select_service, client_type, select_service}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.{identify_client_irv, identify_client_itsa, identify_client_vat}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.play.fsm.JourneyController

import scala.concurrent.ExecutionContext

class AgentLedDeauthJourneyController @Inject()(
  override val journeyService: AgentLedDeauthJourneyService,
  authActions: AuthActionsImpl,
  invitationsService: InvitationsService
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

  val submitPersonalService = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(servicePersonalForm)(chosenPersonalService)
  }

  val submitBusinessService = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(serviceBusinessForm)(chosenBusinessService)
  }

  val showIdentifyClient = actionShowStateWhenAuthorised(AsAgent) {
    case _: IdentifyClientPersonal =>
    case IdentifyClientBusiness    =>
  }

  val submitIdentifyItsaClient = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(identifyClientItsaForm(featureFlags.showHmrcMtdIt))(
      submitIdentifyClientItsa(checkPostcodeMatches, getClientNameByService))
  }

  val showKnownFactNotMatched = actionShowStateWhenAuthorised(AsAgent) {
    case KnownFactNotMatched =>
  }

  val showNotSignedUp = actionShowStateWhenAuthorised(AsAgent) {
    case _: NotSignedUp =>
  }

  override def getCallFor(state: journeyService.model.State)(implicit request: Request[_]): Call = state match {
    case SelectClientType          => routes.AgentLedDeauthJourneyController.showClientType()
    case _: SelectServicePersonal  => routes.AgentLedDeauthJourneyController.showSelectService()
    case SelectServiceBusiness     => routes.AgentLedDeauthJourneyController.showSelectService()
    case _: IdentifyClientPersonal => routes.AgentLedDeauthJourneyController.showIdentifyClient()
    case IdentifyClientBusiness    => routes.AgentLedDeauthJourneyController.showIdentifyClient()
    case KnownFactNotMatched       => routes.AgentLedDeauthJourneyController.showKnownFactNotMatched()
    case _: NotSignedUp            => routes.AgentLedDeauthJourneyController.showNotSignedUp()
    case _                         => throw new Exception(s"Link not found for $state")

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
            basketFlag = false,
            featureFlags,
            enabledServices,
            routes.AgentInvitationJourneyController.submitPersonalSelectService(),
            backLinkFor(breadcrumbs).url,
            routes.AgentInvitationJourneyController.showReviewAuthorisations()
          )
        ))

    case SelectServiceBusiness =>
      Ok(
        business_select_service(
          formWithErrors.or(serviceBusinessForm),
          basketFlag = false,
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
              routes.AgentLedDeAuthController.showClientType(), //change
              routes.AgentLedDeAuthController.showSelectService().url
            ))
        case Services.HMRCPIR =>
          Ok(
            identify_client_irv(
              IrvClientForm.form(featureFlags.showKfcPersonalIncome),
              featureFlags.showKfcPersonalIncome,
              routes.AgentLedDeAuthController.showClientType(), //change
              routes.AgentLedDeAuthController.showSelectService().url
            )
          )
        case Services.HMRCMTDVAT =>
          Ok(
            identify_client_vat(
              formWithErrors.or(VatClientForm.form(featureFlags.showKfcMtdVat)), //change form
              featureFlags.showKfcMtdVat,
              routes.AgentLedDeAuthController.showClientType(), //change to submit identify client call
              routes.AgentLedDeAuthController.showSelectService().url
            ))
      }

    case IdentifyClientBusiness =>
      Ok(
        identify_client_vat(
          formWithErrors.or(VatClientForm.form(featureFlags.showKfcMtdVat)), //change form
          featureFlags.showKfcMtdVat,
          routes.AgentLedDeAuthController.showClientType(), //change to submit identify client call
          routes.AgentLedDeAuthController.showSelectService().url
        ))

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

  def identifyClientItsaForm(showKfcMtdIt: Boolean): Form[ItsaClient] = Form(
    mapping(
      "clientIdentifier" -> normalizedText.verifying(validNino()),
      "postcode"         -> postcodeMapping(showKfcMtdIt)
    )(ItsaClient.apply)(ItsaClient.unapply)
  )
}
