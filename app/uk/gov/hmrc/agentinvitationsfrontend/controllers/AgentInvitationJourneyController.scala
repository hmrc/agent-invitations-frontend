/*
 * Copyright 2021 HM Revenue & Customs
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
import org.joda.time.LocalDate
import play.api.Configuration
import play.api.data.Form
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.agentinvitationsfrontend.config.{AppConfig, CountryNamesLoader, ExternalUrls}
import uk.gov.hmrc.agentinvitationsfrontend.connectors.AgentClientAuthorisationConnector
import uk.gov.hmrc.agentinvitationsfrontend.forms._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.support.CallOps
import uk.gov.hmrc.agentinvitationsfrontend.views.agents._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.{confirm_client, _}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.timed_out
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.play.fsm.JourneyController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AgentInvitationJourneyController @Inject()(
  invitationsService: InvitationsService,
  relationshipsService: RelationshipsService,
  acaConnector: AgentClientAuthorisationConnector,
  val authActions: AuthActionsImpl,
  override val journeyService: AgentInvitationJourneyService,
  notSignedUpPageConfig: NotSignedUpPageConfig,
  countryNamesLoader: CountryNamesLoader,
  clientTypeView: client_type,
  cgtRefNotFoundView: cgtRef_notFound,
  activeAuthExistsView: active_authorisation_exists,
  knownFactsView: known_fact,
  identifyClientItsaView: identify_client_itsa,
  identifyClientIrvView: identify_client_irv,
  identifyClientVatView: identify_client_vat,
  identifyClientTrustView: identify_client_trust,
  identifyClientCgtView: identify_client_cgt,
  confirmClientView: confirm_client,
  confirmCountryCodeCgtView: confirm_countryCode_cgt,
  confirmPostcodeCgtView: confirm_postcode_cgt,
  notMatchedView: not_matched,
  pendingAuthExistsView: pending_authorisation_exists,
  invitationSentView: invitation_sent,
  timedOutView: timed_out,
  selectFromServicesView: select_from_services,
  selectSingleServiceView: select_single_service,
  reviewAuthView: review_authorisations,
  alreadyCopiedAcrossView: already_copied_across_itsa,
  deleteView: delete,
  notSignedupView: not_signed_up,
  cannotCreateRequestView: cannot_create_request,
  invitationCreationFailedView: invitation_creation_failed,
  allAuthRemovedView: all_authorisations_removed,
  agentSuspendedView: agent_suspended,
  partialAuthExistsView: partial_auth_exists,
  clientNotRegisteredView: client_not_registered)(
  implicit configuration: Configuration,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  ec: ExecutionContext,
  val cc: MessagesControllerComponents,
  appConfig: AppConfig)
    extends FrontendController(cc) with JourneyController[HeaderCarrier] with I18nSupport {

  import AgentInvitationJourneyController._
  import acaConnector._
  import authActions._
  import invitationsService._
  import journeyService.model._
  import uk.gov.hmrc.play.fsm.OptionalFormOps._

  override implicit def context(implicit rh: RequestHeader): HeaderCarrier = hc

  //TODO Add local date service to provide flexibility for testing
  private def inferredExpiryDate = LocalDate.now().plusDays(appConfig.invitationExpirationDuration.toDays.toInt)

  private val countries = countryNamesLoader.load
  private val validCountryCodes = countries.keys.toSet
  private val urnEnabled = appConfig.featuresEnableTrustURNIdentifier

  val AsAgent: WithAuthorised[AuthorisedAgent] = { implicit request: Request[Any] =>
    withAuthorisedAsAgent(_)
  }

  /* Here we decide how to handle HTTP request and transition the state of the journey */
  def agentsRoot: Action[AnyContent] = Action(Redirect(routes.AgentInvitationJourneyController.showClientType()))

  def showClientType: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: SelectClientType =>
  }

  def submitClientType: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ClientTypeForm.authorisationForm)(Transitions.selectedClientType)
  }

  def showSelectService: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: SelectPersonalService =>
    case SelectBusinessService    =>
    case _: SelectTrustService    =>
  }

  def submitPersonalSelectService: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ServiceTypeForm.form)(
      Transitions.selectedPersonalService(
        featureFlags.showHmrcMtdIt,
        featureFlags.showPersonalIncome,
        featureFlags.showHmrcMtdVat,
        featureFlags.showHmrcCgt,
        featureFlags.agentSuspensionEnabled,
        getAgencySuspensionDetails
      ))
  }

  def submitPersonalSelectSingle(service: String): Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ServiceTypeForm.selectSingleServiceForm(service, personal))(
      Transitions.selectedPersonalService(
        featureFlags.showHmrcMtdIt,
        featureFlags.showPersonalIncome,
        featureFlags.showHmrcMtdVat,
        featureFlags.showHmrcCgt,
        featureFlags.agentSuspensionEnabled,
        getAgencySuspensionDetails
      ))
  }

  def submitBusinessSelectService: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ServiceTypeForm.selectSingleServiceForm(HMRCMTDVAT, business))(
      Transitions
        .selectedBusinessService(featureFlags.showHmrcMtdVat, featureFlags.agentSuspensionEnabled, getAgencySuspensionDetails))
  }

  def submitTrustSelectSingle(service: String): Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ServiceTypeForm.selectSingleServiceForm(service, business))(
      Transitions
        .selectedTrustService(featureFlags.showHmrcTrust, featureFlags.showHmrcCgt, featureFlags.agentSuspensionEnabled, getAgencySuspensionDetails))
  }

  // this is only for multi-select option forms
  def submitTrustSelectServiceMultiple: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ServiceTypeForm.form)(
      Transitions
        .selectedTrustService(featureFlags.showHmrcTrust, featureFlags.showHmrcCgt, featureFlags.agentSuspensionEnabled, getAgencySuspensionDetails))
  }

  def identifyClientRedirect: Action[AnyContent] =
    Action(Redirect(routes.AgentInvitationJourneyController.showIdentifyClient()))

  def showIdentifyClient: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: IdentifyPersonalClient | IdentifyBusinessClient | _: IdentifyTrustClient =>
  }

  def showConfirmCgtPostcode: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: ConfirmPostcodeCgt =>
  }

  def submitConfirmCgtPostcode: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(PostcodeForm.form)(Transitions.confirmPostcodeCgt(cgtRef => acaConnector.getCgtSubscription(cgtRef)))
  }

  def showConfirmCgtCountryCode: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: ConfirmCountryCodeCgt =>
  }

  def submitConfirmCgtCountryCode: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(CountrycodeForm.form(validCountryCodes))(Transitions.confirmCountryCodeCgt(cgtRef =>
      acaConnector.getCgtSubscription(cgtRef)))
  }

  def submitIdentifyItsaClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ItsaClientForm.form)(
      Transitions.identifiedItsaClient(checkPostcodeMatches)(hasPendingInvitationsFor)(relationshipsService.hasActiveRelationshipFor)(
        getClientNameByService)(createMultipleInvitations)(invitationsService.createAgentLink)(getAgencyEmail)(appConfig)
    )
  }

  def submitIdentifyVatClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(VatClientForm.form)(
      Transitions.identifiedVatClient(checkVatRegistrationDateMatches)(hasPendingInvitationsFor)(relationshipsService.hasActiveRelationshipFor)(
        getClientNameByService)(createMultipleInvitations)(invitationsService.createAgentLink)(getAgencyEmail)
    )
  }

  def submitIdentifyIrvClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(IrvClientForm.form)(
      Transitions.identifiedIrvClient(checkCitizenRecordMatches)(hasPendingInvitationsFor)(relationshipsService.hasActiveRelationshipFor)(
        hasPartialAuthorisationFor)(getClientNameByService)(createMultipleInvitations)(invitationsService.createAgentLink)(getAgencyEmail)(
        hasLegacyMapping)(appConfig)
    )
  }

  def submitIdentifyTrustClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(TrustClientForm.form(urnEnabled))(
      Transitions.identifiedTrustClient(taxId => acaConnector.getTrustName(taxId.value))
    )
  }

  def submitIdentifyCgtClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(CgtClientForm.form())(
      Transitions.identifyCgtClient(cgtRef => acaConnector.getCgtSubscription(cgtRef))
    )
  }

  def showConfirmClient: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: ConfirmClientItsa        =>
    case _: ConfirmClientPersonalVat =>
    case _: ConfirmClientBusinessVat =>
    case _: ConfirmClientTrust       =>
    case _: ConfirmClientTrustNT     =>
    case _: ConfirmClientCgt         =>
  }

  def submitConfirmClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ConfirmClientForm)(
      Transitions.clientConfirmed(featureFlags.showHmrcCgt)(createMultipleInvitations)(invitationsService.createAgentLink)(getAgencyEmail)(
        hasPendingInvitationsFor)(relationshipsService.hasActiveRelationshipFor)(hasPartialAuthorisationFor)(hasLegacyMapping)(appConfig)
    )
  }

  // TODO review whether we only need one state/page here?
  def showReviewAuthorisations: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: ReviewAuthorisationsPersonal | _: ReviewAuthorisationsTrust =>
  }

  def submitReviewAuthorisations: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ReviewAuthorisationsForm)(
      Transitions.authorisationsReviewed(createMultipleInvitations)(invitationsService.createAgentLink)(getAgencyEmail)(createInvitationSent))
  }

  def showDeleteAuthorisation(itemId: String): Action[AnyContent] = action { implicit request =>
    whenAuthorised(AsAgent)(Transitions.deleteAuthorisationRequest(itemId))(display)
  }

  def submitDeleteAuthorisation: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(DeleteAuthorisationForm)(Transitions.confirmDeleteAuthorisationRequest)
  }

  def showInvitationSent: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: InvitationSentPersonal | _: InvitationSentBusiness =>
  }

  def showNotMatched: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: KnownFactNotMatched =>
    case _: TrustNotFound       =>
    case _: CgtRefNotFound      =>
  }

  def showCannotCreateRequest: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: CannotCreateRequest =>
  }

  def showSomeAuthorisationsFailed: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: SomeAuthorisationsFailed =>
  }

  def submitSomeAuthorisationsFailed: Action[AnyContent] = action { implicit request =>
    whenAuthorised(AsAgent)(Transitions.continueSomeResponsesFailed)(redirect)
  }

  def showAllAuthorisationsFailed: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: AllAuthorisationsFailed =>
  }

  def showClientNotSignedUp: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: ClientNotSignedUp =>
  }

  def showClientNotRegistered: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: ClientNotRegistered =>
  }

  def showPendingAuthorisationExists: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: PendingInvitationExists =>
  }
  def showActiveAuthorisationExists: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: ActiveAuthorisationExists  =>
    case _: PartialAuthorisationExists =>
  }

  def showAllAuthorisationsRemoved: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case AllAuthorisationsRemoved =>
  }

  def showAgentSuspended: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: AgentSuspended =>
  }

  def showAlreadyCopiedAcrossItsa: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case AlreadyCopiedAcrossItsa =>
  }

  private def signOutUrl(implicit request: Request[AnyContent]): Future[String] =
    journeyService.initialState
      .map { is =>
        val uri = externalUrls.agentServicesAccountUrl
        val continueUrl = CallOps
          .localFriendlyUrl(env, appConfig)(uri, request.host)
        s"$ggLoginUrl?continue=$continueUrl"
      }

  def signOut: Action[AnyContent] = Action.async { implicit request =>
    signOutUrl(request).map(Redirect(_).withNewSession)
  }

  def timedOut: Action[AnyContent] = Action.async { implicit request =>
    signOutUrl(request).map(url => Forbidden(timedOutView(url)).withNewSession)
  }

  /* Here we map states to the GET endpoints for redirecting and back linking */
  override def getCallFor(state: State)(implicit request: Request[_]): Call = state match {
    case _: SelectClientType             => routes.AgentInvitationJourneyController.showClientType()
    case _: SelectPersonalService        => routes.AgentInvitationJourneyController.showSelectService()
    case SelectBusinessService           => routes.AgentInvitationJourneyController.showSelectService()
    case _: SelectTrustService           => routes.AgentInvitationJourneyController.showSelectService()
    case _: IdentifyPersonalClient       => routes.AgentInvitationJourneyController.showIdentifyClient()
    case IdentifyBusinessClient          => routes.AgentInvitationJourneyController.showIdentifyClient()
    case _: IdentifyTrustClient          => routes.AgentInvitationJourneyController.showIdentifyClient()
    case _: ConfirmClientItsa            => routes.AgentInvitationJourneyController.showConfirmClient()
    case _: ConfirmClientPersonalVat     => routes.AgentInvitationJourneyController.showConfirmClient()
    case _: ConfirmClientBusinessVat     => routes.AgentInvitationJourneyController.showConfirmClient()
    case _: ConfirmClientTrust           => routes.AgentInvitationJourneyController.showConfirmClient()
    case _: ConfirmClientCgt             => routes.AgentInvitationJourneyController.showConfirmClient()
    case _: ConfirmPostcodeCgt           => routes.AgentInvitationJourneyController.showConfirmCgtPostcode()
    case _: ConfirmCountryCodeCgt        => routes.AgentInvitationJourneyController.showConfirmCgtCountryCode()
    case _: ReviewAuthorisationsPersonal => routes.AgentInvitationJourneyController.showReviewAuthorisations()
    case _: ReviewAuthorisationsTrust    => routes.AgentInvitationJourneyController.showReviewAuthorisations()
    case DeleteAuthorisationRequestPersonal(authorisationRequest, _) =>
      routes.AgentInvitationJourneyController.showDeleteAuthorisation(authorisationRequest.itemId)
    case DeleteAuthorisationRequestTrust(authorisationRequest, _) =>
      routes.AgentInvitationJourneyController.showDeleteAuthorisation(authorisationRequest.itemId)
    case _: InvitationSentPersonal     => routes.AgentInvitationJourneyController.showInvitationSent()
    case _: InvitationSentBusiness     => routes.AgentInvitationJourneyController.showInvitationSent()
    case _: KnownFactNotMatched        => routes.AgentInvitationJourneyController.showNotMatched()
    case _: TrustNotFound              => routes.AgentInvitationJourneyController.showNotMatched()
    case _: CgtRefNotFound             => routes.AgentInvitationJourneyController.showNotMatched()
    case _: CannotCreateRequest        => routes.AgentInvitationJourneyController.showCannotCreateRequest()
    case _: SomeAuthorisationsFailed   => routes.AgentInvitationJourneyController.showSomeAuthorisationsFailed()
    case _: AllAuthorisationsFailed    => routes.AgentInvitationJourneyController.showAllAuthorisationsFailed()
    case _: ClientNotSignedUp          => routes.AgentInvitationJourneyController.showClientNotSignedUp()
    case _: PendingInvitationExists    => routes.AgentInvitationJourneyController.showPendingAuthorisationExists()
    case _: ActiveAuthorisationExists  => routes.AgentInvitationJourneyController.showActiveAuthorisationExists()
    case _: PartialAuthorisationExists => routes.AgentInvitationJourneyController.showActiveAuthorisationExists()
    case AllAuthorisationsRemoved      => routes.AgentInvitationJourneyController.showAllAuthorisationsRemoved()
    case _: AgentSuspended             => routes.AgentInvitationJourneyController.showAgentSuspended()
    case _: ClientNotRegistered        => routes.AgentInvitationJourneyController.showClientNotRegistered()
    case AlreadyCopiedAcrossItsa       => routes.AgentInvitationJourneyController.showAlreadyCopiedAcrossItsa()
    case _                             => throw new Exception(s"Link not found for $state")
  }

  /* Here we decide what to render after state transition */
  override def renderState(state: State, breadcrumbs: List[State], formWithErrors: Option[Form[_]])(implicit request: Request[_]): Result =
    state match {

      case SelectClientType(_) =>
        def backLinkForClientType(implicit request: Request[_]): String =
          breadcrumbs.headOption.fold(externalUrls.agentServicesAccountUrl)(getCallFor(_).url)

        Ok(
          clientTypeView(
            formWithErrors.or(ClientTypeForm.authorisationForm),
            ClientTypePageConfig(backLinkForClientType, routes.AgentInvitationJourneyController.submitClientType(), featureFlags.showHmrcTrust)
          ))

      case SelectPersonalService(services, basket) =>
        val config = PersonalSelectServicePageConfig(
          basket,
          featureFlags,
          services,
          backLinkFor(breadcrumbs).url,
          routes.AgentInvitationJourneyController.showReviewAuthorisations()
        )

        if (config.showMultiSelect)
          Ok(selectFromServicesView(formWithErrors.or(ServiceTypeForm.form), config))
        else
          Ok(selectSingleServiceView(formWithErrors.or(ServiceTypeForm.selectSingleServiceForm(config.remainingService, personal)), config))

      case SelectBusinessService =>
        Ok(
          selectSingleServiceView(
            formWithErrors.or(ServiceTypeForm.selectSingleServiceForm(HMRCMTDVAT, business)),
            BusinessSelectServicePageConfig(
              Set.empty,
              featureFlags,
              submitCall = routes.AgentInvitationJourneyController.submitBusinessSelectService(),
              backLink = backLinkFor(breadcrumbs).url,
              reviewAuthsCall = routes.AgentInvitationJourneyController.showReviewAuthorisations()
            )
          ))

      case SelectTrustService(services, basket) =>
        val config = TrustSelectServicePageConfig(
          basket,
          featureFlags,
          services,
          backLinkFor(breadcrumbs).url,
          routes.AgentInvitationJourneyController.showReviewAuthorisations())
        if (config.showMultiSelect) {
          Ok(selectFromServicesView(formWithErrors.or(ServiceTypeForm.form), config))
        } else {
          Ok(selectSingleServiceView(formWithErrors.or(ServiceTypeForm.selectSingleServiceForm(config.remainingService, business)), config))
        }

      case IdentifyTrustClient(Services.TRUST, _) =>
        Ok(
          identifyClientTrustView(
            trustClientForm = formWithErrors.or(TrustClientForm.form(urnEnabled)),
            submitFormCall = routes.AgentInvitationJourneyController.submitIdentifyTrustClient(),
            backLinkUrl = backLinkFor(breadcrumbs).url,
            isDeAuthJourney = false,
            showUrnEnabledContent = urnEnabled
          )
        )
      case IdentifyTrustClient(Services.HMRCCGTPD, _) =>
        Ok(
          identifyClientCgtView(
            formWithErrors.or(CgtClientForm.form()),
            routes.AgentInvitationJourneyController.submitIdentifyCgtClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyPersonalClient(Services.HMRCMTDIT, _) =>
        Ok(
          identifyClientItsaView(
            formWithErrors.or(ItsaClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyItsaClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyPersonalClient(Services.HMRCMTDVAT, _) =>
        Ok(
          identifyClientVatView(
            formWithErrors.or(VatClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyVatClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyPersonalClient(Services.HMRCPIR, _) =>
        Ok(
          identifyClientIrvView(
            formWithErrors.or(IrvClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyIrvClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyPersonalClient(Services.HMRCCGTPD, _) =>
        Ok(
          identifyClientCgtView(
            formWithErrors.or(CgtClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyCgtClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyBusinessClient =>
        Ok(
          identifyClientVatView(
            formWithErrors.or(VatClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyVatClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case ConfirmClientTrust(authorisationRequest, _) =>
        Ok(
          confirmClientView(
            authorisationRequest.clientName,
            formWithErrors.or(ConfirmClientForm),
            backLinkFor(breadcrumbs).url,
            routes.AgentInvitationJourneyController.submitConfirmClient(),
            authorisationRequest.invitation.clientIdentifierType,
            authorisationRequest.invitation.clientId
          ))

      case ConfirmClientCgt(authorisationRequest, _) =>
        Ok(
          confirmClientView(
            authorisationRequest.clientName,
            formWithErrors.or(ConfirmClientForm),
            backLinkFor(breadcrumbs).url,
            routes.AgentInvitationJourneyController.submitConfirmClient(),
            authorisationRequest.invitation.clientIdentifierType,
            authorisationRequest.invitation.clientId
          ))

      case ConfirmPostcodeCgt(_, clientType, _, _, _) =>
        Ok(
          confirmPostcodeCgtView(
            clientType,
            formWithErrors.or(PostcodeForm.form),
            backLinkFor(breadcrumbs).url,
            fromFastTrack = false,
            isDeAuth = false))

      case ConfirmCountryCodeCgt(_, clientType, _, _, _) =>
        Ok(
          confirmCountryCodeCgtView(
            clientType,
            countries,
            formWithErrors.or(CountrycodeForm.form(validCountryCodes)),
            backLinkFor(breadcrumbs).url,
            fromFastTrack = false,
            isDeAuth = false))

      case ConfirmClientItsa(authorisationRequest, _) =>
        Ok(
          confirmClientView(
            authorisationRequest.clientName,
            formWithErrors.or(ConfirmClientForm),
            backLinkFor(breadcrumbs).url,
            routes.AgentInvitationJourneyController.submitConfirmClient(),
            authorisationRequest.invitation.clientIdentifierType,
            authorisationRequest.invitation.clientId
          ))

      case ConfirmClientPersonalVat(authorisationRequest, _) =>
        Ok(
          confirmClientView(
            authorisationRequest.clientName,
            formWithErrors.or(ConfirmClientForm),
            backLinkFor(breadcrumbs).url,
            routes.AgentInvitationJourneyController.submitConfirmClient(),
            authorisationRequest.invitation.clientIdentifierType,
            authorisationRequest.invitation.clientId
          ))

      case ConfirmClientBusinessVat(authorisationRequest) =>
        Ok(
          confirmClientView(
            authorisationRequest.clientName,
            formWithErrors.or(ConfirmClientForm),
            backLinkFor(breadcrumbs).url,
            routes.AgentInvitationJourneyController.submitConfirmClient(),
            authorisationRequest.invitation.clientIdentifierType,
            authorisationRequest.invitation.clientId
          ))

      case ReviewAuthorisationsPersonal(services, basket) =>
        Ok(
          reviewAuthView(
            ReviewAuthorisationsPersonalPageConfig(
              basket,
              featureFlags,
              services,
              routes.AgentInvitationJourneyController.submitReviewAuthorisations()),
            formWithErrors.or(ReviewAuthorisationsForm),
            backLinkFor(breadcrumbs).url
          ))

      case ReviewAuthorisationsTrust(services, basket) =>
        Ok(
          reviewAuthView(
            ReviewAuthorisationsTrustPageConfig(basket, featureFlags, services, routes.AgentInvitationJourneyController.submitReviewAuthorisations()),
            formWithErrors.or(ReviewAuthorisationsForm),
            backLinkFor(breadcrumbs).url
          ))

      case DeleteAuthorisationRequestPersonal(authorisationRequest, _) =>
        Ok(
          deleteView(
            DeletePageConfig(
              authorisationRequest,
              routes.AgentInvitationJourneyController.submitDeleteAuthorisation(),
              routes.AgentInvitationJourneyController.showReviewAuthorisations().url
            ),
            formWithErrors.or(DeleteAuthorisationForm)
          ))

      case DeleteAuthorisationRequestTrust(authorisationRequest, _) =>
        Ok(
          deleteView(
            DeletePageConfig(
              authorisationRequest,
              routes.AgentInvitationJourneyController.submitDeleteAuthorisation(),
              routes.AgentInvitationJourneyController.showReviewAuthorisations().url
            ),
            formWithErrors.or(DeleteAuthorisationForm)
          ))

      case InvitationSentPersonal(invitationLink, continueUrl, agencyEmail, services, isAltItsa) =>
        Ok(
          invitationSentView(
            InvitationSentPageConfig(
              invitationLink,
              None,
              continueUrl.isDefined,
              ClientType.fromEnum(personal),
              inferredExpiryDate,
              agencyEmail,
              services,
              isAltItsa
            )))

      case InvitationSentBusiness(invitationLink, continueUrl, agencyEmail, services) =>
        Ok(
          invitationSentView(
            InvitationSentPageConfig(
              invitationLink,
              None,
              continueUrl.isDefined,
              ClientType.fromEnum(business),
              inferredExpiryDate,
              agencyEmail,
              services,
              isAltItsa = false,
              services.head)))

      case KnownFactNotMatched(basket) =>
        Ok(
          notMatchedView(
            basket.nonEmpty,
            routes.AgentInvitationJourneyController.showIdentifyClient(),
            Some(routes.AgentInvitationJourneyController.showReviewAuthorisations())))

      case TrustNotFound(basket) =>
        Ok(
          notMatchedView(
            basket.nonEmpty,
            routes.AgentInvitationJourneyController.showIdentifyClient(),
            Some(routes.AgentInvitationJourneyController.showReviewAuthorisations())
          ))

      case CgtRefNotFound(cgtRef, basket) =>
        Ok(
          cgtRefNotFoundView(
            basket.nonEmpty,
            routes.AgentInvitationJourneyController.showIdentifyClient(),
            Some(routes.AgentInvitationJourneyController.showReviewAuthorisations()),
            cgtRef.value
          ))

      case CannotCreateRequest(basket) =>
        Ok(cannotCreateRequestView(CannotCreateRequestConfig(basket.nonEmpty, fromFastTrack = false, backLinkFor(breadcrumbs).url)))

      case SomeAuthorisationsFailed(_, _, _, basket) =>
        Ok(invitationCreationFailedView(SomeInvitationCreationFailedPageConfig(basket)))

      case AllAuthorisationsFailed(basket) =>
        Ok(invitationCreationFailedView(AllInvitationCreationFailedPageConfig(basket)))

      case ActiveAuthorisationExists(clientType, service, basket) =>
        Ok(
          activeAuthExistsView(
            basket.nonEmpty,
            service,
            clientType,
            fromFastTrack = false,
            routes.AgentInvitationJourneyController.showReviewAuthorisations(),
            routes.AgentInvitationJourneyController.showClientType()
          ))

      case PendingInvitationExists(_, basket) =>
        Ok(
          pendingAuthExistsView(
            PendingAuthorisationExistsPageConfig(
              basket.nonEmpty,
              backLinkFor(breadcrumbs).url,
              fromFastTrack = false,
              routes.AgentInvitationJourneyController.showReviewAuthorisations(),
              routes.AgentInvitationJourneyController.showClientType()
            )))

      case PartialAuthorisationExists(basket) =>
        Ok(
          partialAuthExistsView(
            basket.nonEmpty,
            fromFastTrack = false,
            routes.AgentInvitationJourneyController.showReviewAuthorisations(),
            routes.AgentInvitationJourneyController.showClientType()
          ))

      case ClientNotSignedUp(service, basket) => {
        val pageConfig = notSignedUpPageConfig.render(service)
        Ok(notSignedupView(service, basket.nonEmpty, false, pageConfig))
      }

      case ClientNotRegistered(basket) => {
        Ok(
          clientNotRegisteredView(
            basket.nonEmpty,
            false,
            routes.AgentInvitationJourneyController.showReviewAuthorisations(),
            routes.AgentInvitationJourneyController.showClientType()))
      }

      case AllAuthorisationsRemoved =>
        Ok(allAuthRemovedView(routes.AgentInvitationJourneyController.showClientType()))

      case AgentSuspended(suspendedService, basket) =>
        Ok(agentSuspendedView(basket, suspendedService, backLinkFor(breadcrumbs).url))

      case AlreadyCopiedAcrossItsa => Ok(alreadyCopiedAcrossView())

      case _ => throw new Exception(s"Cannot render a page for unexpected state: $state, add your state as a match case in #renderState")
    }
}

object AgentInvitationJourneyController {

  import uk.gov.hmrc.agentinvitationsfrontend.forms.CommonConfirmationForms._

  val ConfirmClientForm: Form[Confirmation] = confirmationForm("error.confirm-client.required")

  val ReviewAuthorisationsForm: Form[Confirmation] = confirmationForm("error.review-authorisation.required")

  val DeleteAuthorisationForm: Form[Confirmation] = confirmationForm("error.delete.radio")

}
