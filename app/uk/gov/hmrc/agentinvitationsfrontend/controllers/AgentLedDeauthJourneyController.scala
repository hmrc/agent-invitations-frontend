/*
 * Copyright 2023 HM Revenue & Customs
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

import play.api.data.Form
import play.api.i18n.I18nSupport
import play.api.mvc._
import play.api.Configuration
import uk.gov.hmrc.agentinvitationsfrontend.config.{CountryNamesLoader, ExternalUrls}
import uk.gov.hmrc.agentinvitationsfrontend.forms.CommonConfirmationForms._
import uk.gov.hmrc.agentinvitationsfrontend.forms._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.Transitions
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{Business, Personal}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services.{InvitationsService, KnownFactService, RelationshipsService}
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.cancelAuthorisation.SelectServicePageConfigCancel
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.{ClientTypePageConfig, NotSignedUpPageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.cancelAuthorisation.{authorisation_cancelled, business_select_single_service, client_type, confirm_cancel, confirm_client, no_client_found, response_failed, select_service}
import uk.gov.hmrc.agentmtdidentifiers.model.Service
import uk.gov.hmrc.hmrcfrontend.config.ContactFrontendConfig
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.play.fsm.JourneyController

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class AgentLedDeauthJourneyController @Inject() (
  override val journeyService: AgentLedDeauthJourneyService,
  authActions: AuthActions,
  invitationsService: InvitationsService,
  relationshipsService: RelationshipsService,
  knownFactService: KnownFactService,
  countryNamesLoader: CountryNamesLoader,
  notSignedUpPageConfig: NotSignedUpPageConfig,
  clientTypeView: client_type,
  businessSelectSingleServiceView: business_select_single_service,
  identifyClientItsaView: identify_client_itsa,
  identifyClientIrvView: identify_client_irv,
  identifyClientVatView: identify_client_vat,
  identifyClientTrustView: identify_client_trust,
  identifyClientCgtView: identify_client_cgt,
  identifyClientPptView: identify_client_ppt,
  identifyClientCbcView: identify_client_cbc,
  identifyClientPillar2View: identify_client_pillar2,
  confirmClientView: confirm_client,
  confirmCountryCodeCgtView: confirm_countryCode_cgt,
  confirmPostcodeCgtView: confirm_postcode_cgt,
  authCancelledView: authorisation_cancelled,
  notAuthorisedView: not_authorised,
  selectServiceView: select_service,
  noClientFoundView: no_client_found,
  notSignedupView: not_signed_up,
  responseFailedView: response_failed,
  confirmCancelView: confirm_cancel,
  override val actionBuilder: DefaultActionBuilder
)(implicit
  ec: ExecutionContext,
  implicit val contactFrontendConfig: ContactFrontendConfig,
  configuration: Configuration,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  val cc: MessagesControllerComponents
) extends FrontendController(cc) with JourneyController[HeaderCarrier] with I18nSupport {

  override def context(implicit rh: RequestHeader): HeaderCarrier = hc

  import authActions._
  import uk.gov.hmrc.play.fsm.OptionalFormOps._

  private val countries = countryNamesLoader.load
  private val validCountryCodes = countries.keys.toSet

  val AsAgent: WithAuthorised[AuthorisedAgent] = { implicit request: Request[Any] =>
    withAuthorisedAsAgent(_)
  }

  def transitions()(implicit ec: ExecutionContext, request: RequestHeader): Transitions = Transitions(
    featureFlags = featureFlags,
    hasActiveRelationshipFor = relationshipsService.hasActiveRelationshipFor,
    hasPartialAuthorisationFor = invitationsService.hasPartialAuthorisationFor,
    getClientName = invitationsService.getClientNameByService,
    deleteRelationship = relationshipsService.deleteRelationshipForService,
    setRelationshipEnded = invitationsService.setRelationshipEnded,
    getAgencyName = invitationsService.getAgencyName,
    getCgtSubscription = invitationsService.acaConnector.getCgtSubscription,
    getPptSubscription = invitationsService.acaConnector.getPptSubscription,
    getCbcSubscription = invitationsService.acaConnector.getCbcSubscription,
    checkKnownFact = knownFactService.checkKnownFact
  )

  val agentLedDeauthRoot: Action[AnyContent] = Action(Redirect(routes.AgentLedDeauthJourneyController.showClientType))

  def showClientType: Action[AnyContent] = if (appConfig.enableAcrfRedirects) {
    Action(Redirect(appConfig.cancelAuthRequestUrl))
  } else {
    actions.whenAuthorised(AsAgent).show[SelectClientType.type]
  }

  def submitClientType: Action[AnyContent] =
    actions.whenAuthorisedWithRetrievals(AsAgent).bindForm(ClientTypeForm.deAuthorisationForm) applyWithRequest (implicit request =>
      transitions.selectedClientType
    )

  def showSelectService: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[SelectService].orRollback

  val submitSelectService: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ServiceTypeForm.form)
      .applyWithRequest(implicit request => transitions.chosenServiceMulti)

  def submitBusinessServiceSingle: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ServiceTypeForm.selectSingleServiceForm(Service.Vat, Business))
      .applyWithRequest(implicit request => transitions.chosenService)

  def showIdentifyClient: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[IdentifyClient].orRollback

  def submitIdentifyItsaClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ItsaClientForm.form)
      .applyWithRequest(implicit request => transitions.submitIdentifyClientItsa)

  def submitIdentifyIrvClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(IrvClientForm.form)
      .applyWithRequest(implicit request => transitions.submitIdentifyClientIrv)

  def submitIdentifyVatClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(VatClientForm.form)
      .applyWithRequest(implicit request => transitions.submitIdentifyClientVat)

  val submitIdentifyTrustClient: Action[AnyContent] = actions
    .whenAuthorisedWithRetrievals(AsAgent)
    .bindForm(TrustClientForm.form)
    .applyWithRequest(implicit request => transitions.submitIdentifyClientTrust)

  val submitIdentifyCgtClient: Action[AnyContent] = actions
    .whenAuthorisedWithRetrievals(AsAgent)
    .bindForm(CgtClientForm.form())
    .applyWithRequest(implicit request => transitions.submitIdentifyClientCgt)

  val submitIdentifyPptClient: Action[AnyContent] = actions
    .whenAuthorisedWithRetrievals(AsAgent)
    .bindForm(PptClientForm.form)
    .applyWithRequest(implicit request => transitions.submitIdentifyClientPpt)

  val submitIdentifyCbcClient: Action[AnyContent] = actions
    .whenAuthorisedWithRetrievals(AsAgent)
    .bindForm(CbcClientForm.form)
    .applyWithRequest(implicit request => transitions.submitIdentifyClientCbc)

  val submitIdentifyPillar2Client: Action[AnyContent] = actions
    .whenAuthorisedWithRetrievals(AsAgent)
    .bindForm(Pillar2ClientForm.form)
    .applyWithRequest(implicit request => transitions.submitIdentifyClientPillar2)

  def showPostcodeCgt: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmPostcodeCgt].orRollback

  def submitConfirmCgtPostcode: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(PostcodeForm.form)
      .applyWithRequest(implicit request => transitions.confirmPostcodeCgt)

  def showCountryCodeCgt: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmCountryCodeCgt].orRollback

  def submitConfirmCgtCountryCode: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(CountrycodeForm.form(validCountryCodes))
      .applyWithRequest(implicit request => transitions.confirmCountryCodeCgt)

  def showConfirmClient: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmClient].orRollback

  def submitConfirmClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(confirmCancelForm)
      .applyWithRequest(implicit request => transitions.clientConfirmed)

  def showConfirmCancel: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmCancel].orRollback

  def submitConfirmCancel: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(confirmCancelForm)
      .applyWithRequest(implicit request => transitions.cancelConfirmed)

  def showAuthorisationCancelled: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[AuthorisationCancelled]

  def showKnownFactNotMatched: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ErrorState]

  def showNotSignedUp: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[NotSignedUp]

  def showNotAuthorised: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[NotAuthorised]

  def showResponseFailed: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ResponseFailed]

  override def getCallFor(state: journeyService.model.State)(implicit request: Request[_]): Call = state match {
    case SelectClientType          => routes.AgentLedDeauthJourneyController.showClientType
    case _: SelectService          => routes.AgentLedDeauthJourneyController.showSelectService
    case _: IdentifyClient         => routes.AgentLedDeauthJourneyController.showIdentifyClient
    case _: ConfirmPostcodeCgt     => routes.AgentLedDeauthJourneyController.showPostcodeCgt
    case _: ConfirmCountryCodeCgt  => routes.AgentLedDeauthJourneyController.showCountryCodeCgt
    case _: ConfirmClient          => routes.AgentLedDeauthJourneyController.showConfirmClient
    case _: ConfirmCancel          => routes.AgentLedDeauthJourneyController.showConfirmCancel
    case _: AuthorisationCancelled => routes.AgentLedDeauthJourneyController.showAuthorisationCancelled
    case KnownFactNotMatched       => routes.AgentLedDeauthJourneyController.showKnownFactNotMatched
    case TrustNotFound             => routes.AgentLedDeauthJourneyController.showKnownFactNotMatched
    case _: CgtRefNotFound         => routes.AgentLedDeauthJourneyController.showKnownFactNotMatched
    case _: NotSignedUp            => routes.AgentLedDeauthJourneyController.showNotSignedUp
    case _: NotAuthorised          => routes.AgentLedDeauthJourneyController.showNotAuthorised
    case _: ResponseFailed         => routes.AgentLedDeauthJourneyController.showResponseFailed
    case _                         => throw new Exception(s"Link not found for $state")
  }

  override def renderState(state: journeyService.model.State, breadcrumbs: journeyService.Breadcrumbs, formWithErrors: Option[Form[_]])(implicit
    request: Request[_]
  ): Result = state match {

    case SelectClientType =>
      def backLinkForClientType(implicit request: Request[_]): String =
        breadcrumbs.headOption.fold(externalUrls.agentServicesAccountUrl)(getCallFor(_).url)

      Ok(
        clientTypeView(
          formWithErrors.or(ClientTypeForm.deAuthorisationForm),
          ClientTypePageConfig(backLinkForClientType, routes.AgentLedDeauthJourneyController.submitClientType)
        )
      )

    case SelectService(clientType, availableServices) =>
      val pageConfig = SelectServicePageConfigCancel(
        clientType,
        availableServices,
        routes.AgentLedDeauthJourneyController.submitSelectService,
        backLinkFor(breadcrumbs).url
      )
      availableServices.size match {
        case 1 =>
          Ok(
            businessSelectSingleServiceView(
              formWithErrors.or(ServiceTypeForm.selectSingleServiceForm(Services.supportedServicesFor(clientType).head, clientType)),
              routes.AgentLedDeauthJourneyController.submitBusinessServiceSingle,
              backLinkFor(breadcrumbs).url
            )
          )
        case x if x > 1 =>
          Ok(
            selectServiceView(
              formWithErrors.or(ServiceTypeForm.form),
              pageConfig
            )
          )
      }

    case IdentifyClient(ClientType.Personal, service) =>
      service match {
        case Service.MtdIt =>
          Ok(
            identifyClientItsaView(
              formWithErrors.or(ItsaClientForm.form),
              routes.AgentLedDeauthJourneyController.submitIdentifyItsaClient,
              backLinkFor(breadcrumbs).url,
              isDeAuthJourney = true
            )
          )
        case Service.PersonalIncomeRecord =>
          Ok(
            identifyClientIrvView(
              formWithErrors.or(IrvClientForm.form),
              routes.AgentLedDeauthJourneyController.submitIdentifyIrvClient,
              backLinkFor(breadcrumbs).url,
              isDeAuthJourney = true
            )
          )
        case Service.Vat =>
          Ok(
            identifyClientVatView(
              formWithErrors.or(VatClientForm.form),
              routes.AgentLedDeauthJourneyController.submitIdentifyVatClient,
              backLinkFor(breadcrumbs).url,
              isDeAuthJourney = true
            )
          )
        case Service.CapitalGains =>
          Ok(
            identifyClientCgtView(
              formWithErrors.or(CgtClientForm.form()),
              routes.AgentLedDeauthJourneyController.submitIdentifyCgtClient,
              backLinkFor(breadcrumbs).url,
              isDeAuthJourney = true
            )
          )
        case Service.Ppt =>
          Ok(
            identifyClientPptView(
              formWithErrors.or(PptClientForm.form),
              routes.AgentLedDeauthJourneyController.submitIdentifyPptClient,
              backLinkFor(breadcrumbs).url,
              isDeAuthJourney = true
            )
          )
      }

    case IdentifyClient(ClientType.Business, Service.Vat) =>
      Ok(
        identifyClientVatView(
          formWithErrors.or(VatClientForm.form),
          routes.AgentLedDeauthJourneyController.submitIdentifyVatClient,
          backLinkFor(breadcrumbs).url,
          isDeAuthJourney = true
        )
      )

    case IdentifyClient(ClientType.Trust, Service.Trust | Service.TrustNT) =>
      Ok(
        identifyClientTrustView(
          formWithErrors.or(TrustClientForm.form),
          routes.AgentLedDeauthJourneyController.submitIdentifyTrustClient,
          backLinkFor(breadcrumbs).url,
          isDeAuthJourney = true
        )
      )

    case IdentifyClient(_, Service.CapitalGains) =>
      Ok(
        identifyClientCgtView(
          formWithErrors.or(CgtClientForm.form()),
          routes.AgentLedDeauthJourneyController.submitIdentifyCgtClient,
          backLinkFor(breadcrumbs).url,
          isDeAuthJourney = true
        )
      )

    case IdentifyClient(_, Service.Ppt) =>
      Ok(
        identifyClientPptView(
          formWithErrors.or(PptClientForm.form),
          routes.AgentLedDeauthJourneyController.submitIdentifyPptClient,
          backLinkFor(breadcrumbs).url,
          isDeAuthJourney = true
        )
      )

    case IdentifyClient(_, Service.Cbc | Service.CbcNonUk) =>
      Ok(
        identifyClientCbcView(
          formWithErrors.or(CbcClientForm.form),
          routes.AgentLedDeauthJourneyController.submitIdentifyCbcClient,
          backLinkFor(breadcrumbs).url,
          isDeAuthJourney = true
        )
      )

    case IdentifyClient(_, Service.Pillar2) =>
      Ok(
        identifyClientPillar2View(
          formWithErrors.or(Pillar2ClientForm.form),
          routes.AgentLedDeauthJourneyController.submitIdentifyPillar2Client,
          backLinkFor(breadcrumbs).url,
          isDeAuthJourney = true
        )
      )

    case ConfirmClient(_, _, clientName, _) =>
      Ok(
        confirmClientView(
          clientName.getOrElse(""),
          formWithErrors.or(confirmCancelForm),
          routes.AgentLedDeauthJourneyController.submitConfirmClient,
          backLinkFor(breadcrumbs).url
        )
      )

    case _: ConfirmCountryCodeCgt =>
      Ok(
        confirmCountryCodeCgtView(
          Personal,
          countries,
          formWithErrors.or(CountrycodeForm.form(validCountryCodes)),
          backLinkFor(breadcrumbs).url,
          fromFastTrack = false,
          isDeAuth = true
        )
      )

    case _: ConfirmPostcodeCgt =>
      Ok(confirmPostcodeCgtView(Personal, formWithErrors.or(PostcodeForm.form), backLinkFor(breadcrumbs).url, fromFastTrack = false, isDeAuth = true))

    case ConfirmCancel(service, clientName, _, _) =>
      Ok(
        confirmCancelView(
          formWithErrors.or(confirmCancelForm),
          service,
          clientName.getOrElse(""),
          routes.AgentLedDeauthJourneyController.submitConfirmCancel,
          backLinkFor(breadcrumbs).url
        )
      )

    case AuthorisationCancelled(service, clientName, agencyName) =>
      journeyService.cleanBreadcrumbs(_ => Nil)
      Ok(authCancelledView(service, clientName.getOrElse(""), agencyName, externalUrls.agentServicesAccountUrl))

    case KnownFactNotMatched =>
      Ok(noClientFoundView(routes.AgentLedDeauthJourneyController.showClientType))

    case TrustNotFound =>
      Ok(noClientFoundView(routes.AgentLedDeauthJourneyController.showClientType))

    case CgtRefNotFound(_) =>
      Ok(noClientFoundView(routes.AgentLedDeauthJourneyController.showClientType))

    case NotSignedUp(service) =>
      Ok(notSignedupView(service, hasRequests = false, isDeAuthJourney = true, notSignedUpPageConfig.render(service)))

    case NotAuthorised(service) =>
      Ok(notAuthorisedView(service, routes.AgentLedDeauthJourneyController.showClientType, isDeAuthJourney = true))

    case _: ResponseFailed => Ok(responseFailedView(routes.AgentLedDeauthJourneyController.submitConfirmCancel))

  }
}
