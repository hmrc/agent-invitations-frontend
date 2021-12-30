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
import play.api.{Configuration, Mode}
import play.api.data.Form
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.agentinvitationsfrontend.config.{AppConfig, CountryNamesLoader, ExternalUrls}
import uk.gov.hmrc.agentinvitationsfrontend.connectors.AgentClientAuthorisationConnector
import uk.gov.hmrc.agentinvitationsfrontend.forms._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{Business, Personal, Trust}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.support.CallOps
import uk.gov.hmrc.agentinvitationsfrontend.views.agents._
import uk.gov.hmrc.agentinvitationsfrontend.support.CallOps.localFriendlyUrl
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.{confirm_client, _}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.timed_out
import uk.gov.hmrc.agentmtdidentifiers.model.PptRef
import uk.gov.hmrc.hmrcfrontend.config.ContactFrontendConfig
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.play.fsm.JourneyController

import java.net.URI
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
  pptRefNotFoundView: pptRef_notFound,
  activeAuthExistsView: active_authorisation_exists,
  identifyClientItsaView: identify_client_itsa,
  identifyClientIrvView: identify_client_irv,
  identifyClientVatView: identify_client_vat,
  identifyClientTrustView: identify_client_trust,
  identifyClientCgtView: identify_client_cgt,
  identifyClientPptView: identify_client_ppt,
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
  legacyAuthorisationDetectedView: legacy_authorisation_detected,
  deleteView: delete,
  notSignedupView: not_signed_up,
  cannotCreateRequestView: cannot_create_request,
  invitationCreationFailedView: invitation_creation_failed,
  allAuthRemovedView: all_authorisations_removed,
  agentSuspendedView: agent_suspended,
  partialAuthExistsView: partial_auth_exists,
  clientNotRegisteredView: client_not_registered,
  clientInsolventView: client_insolvent)(
  implicit configuration: Configuration,
  implicit val contactFrontendConfig: ContactFrontendConfig,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  ec: ExecutionContext,
  val cc: MessagesControllerComponents,
  appConfig: AppConfig,
  override val actionBuilder: DefaultActionBuilder)
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
  val agentsRoot: Action[AnyContent] = Action(Redirect(routes.AgentInvitationJourneyController.showClientType()))

  val showClientType: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[SelectClientType]

  val submitClientType: Action[AnyContent] =
    actions.whenAuthorisedWithRetrievals(AsAgent).bindForm(ClientTypeForm.authorisationForm).apply(Transitions.selectedClientType)

  // TODO: Broken using DSL
  val showSelectService: Action[AnyContent] = legacy.actionShowStateWhenAuthorised(AsAgent) {
    case _: SelectService =>
  }

  val submitPersonalSelectService: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ServiceTypeForm.form)
      .applyWithRequest(implicit request =>
        Transitions.selectedPersonalService(
          featureFlags.showHmrcMtdIt,
          featureFlags.showPersonalIncome,
          featureFlags.showHmrcMtdVat,
          featureFlags.showHmrcCgt,
          featureFlags.showPlasticPackagingTax,
          featureFlags.agentSuspensionEnabled,
          getAgencySuspensionDetails
      ))

  def submitPersonalSelectSingle(service: String): Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ServiceTypeForm.selectSingleServiceForm(service, Personal))
      .applyWithRequest(implicit request =>
        Transitions.selectedPersonalService(
          featureFlags.showHmrcMtdIt,
          featureFlags.showPersonalIncome,
          featureFlags.showHmrcMtdVat,
          featureFlags.showHmrcCgt,
          featureFlags.showPlasticPackagingTax,
          featureFlags.agentSuspensionEnabled,
          getAgencySuspensionDetails
      ))

  def submitPersonalSelectSingleNew(service: String): Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ServiceTypeForm.selectSingleServiceForm(service, Personal))
      .applyWithRequest(implicit request =>
        Transitions.selectedPersonalService(
          featureFlags.showHmrcMtdIt,
          featureFlags.showPersonalIncome,
          featureFlags.showHmrcMtdVat,
          featureFlags.showHmrcCgt,
          featureFlags.showPlasticPackagingTax,
          featureFlags.agentSuspensionEnabled,
          getAgencySuspensionDetails
      ))

  val submitBusinessSelectService: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ServiceTypeForm.form)
      .applyWithRequest(
        implicit request =>
          Transitions.selectedBusinessService(
            featureFlags.showHmrcMtdVat,
            featureFlags.showPlasticPackagingTax,
            featureFlags.agentSuspensionEnabled,
            getAgencySuspensionDetails
        ))

  def submitBusinessSelectSingle(service: String): Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ServiceTypeForm.selectSingleServiceForm(service, Personal))
      .applyWithRequest(
        implicit request =>
          Transitions.selectedBusinessService(
            featureFlags.showHmrcMtdVat,
            featureFlags.showPlasticPackagingTax,
            featureFlags.agentSuspensionEnabled,
            getAgencySuspensionDetails
        ))

  def submitTrustSelectSingle(service: String): Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ServiceTypeForm.selectSingleServiceForm(service, Trust))
      .applyWithRequest(implicit request =>
        Transitions.selectedTrustService(
          featureFlags.showHmrcTrust,
          featureFlags.showHmrcCgt,
          featureFlags.showPlasticPackagingTax,
          featureFlags.agentSuspensionEnabled,
          getAgencySuspensionDetails
      ))

  // this is only for multi-select option forms
  val submitTrustSelectServiceMultiple: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ServiceTypeForm.form)
      .applyWithRequest(implicit request =>
        Transitions.selectedTrustService(
          featureFlags.showHmrcTrust,
          featureFlags.showHmrcCgt,
          featureFlags.showPlasticPackagingTax,
          featureFlags.agentSuspensionEnabled,
          getAgencySuspensionDetails
      ))

  val identifyClientRedirect: Action[AnyContent] =
    Action(Redirect(routes.AgentInvitationJourneyController.showIdentifyClient()))

  val showIdentifyClient: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[Identify].orRollback

  val showConfirmCgtPostcode: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmPostcodeCgt].orRollback

  val submitConfirmCgtPostcode: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(PostcodeForm.form)
      .applyWithRequest(implicit request => Transitions.confirmPostcodeCgt(cgtRef => acaConnector.getCgtSubscription(cgtRef)))

  val showConfirmCgtCountryCode: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmCountryCodeCgt].orRollback

  val submitConfirmCgtCountryCode: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(CountrycodeForm.form(validCountryCodes))
      .applyWithRequest(implicit request => Transitions.confirmCountryCodeCgt(cgtRef => acaConnector.getCgtSubscription(cgtRef)))

  val submitIdentifyItsaClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ItsaClientForm.form)
      .applyWithRequest(implicit request =>
        Transitions.identifiedItsaClient(checkPostcodeMatches)(hasPendingInvitationsFor)(relationshipsService.hasActiveRelationshipFor)(
          getClientNameByService)(createMultipleInvitations)(invitationsService.createAgentLink)(getAgencyEmail)(appConfig))

  val submitIdentifyVatClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(VatClientForm.form)
      .applyWithRequest(implicit request =>
        Transitions.identifiedVatClient(checkVatRegistrationDateMatches)(hasPendingInvitationsFor)(relationshipsService.hasActiveRelationshipFor)(
          getClientNameByService)(createMultipleInvitations)(invitationsService.createAgentLink)(getAgencyEmail))

  val submitIdentifyIrvClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(IrvClientForm.form)
      .applyWithRequest(
        implicit request =>
          Transitions.identifiedIrvClient(checkCitizenRecordMatches)(hasPendingInvitationsFor)(relationshipsService.hasActiveRelationshipFor)(
            hasPartialAuthorisationFor)(getClientNameByService)(createMultipleInvitations)(invitationsService.createAgentLink)(getAgencyEmail)(
            legacySaRelationshipStatusFor)(appConfig))

  val submitIdentifyTrustClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(TrustClientForm.form(urnEnabled))
      .applyWithRequest(implicit request => Transitions.identifiedTrustClient(taxId => acaConnector.getTrustName(taxId.value)))

  val submitIdentifyCgtClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(CgtClientForm.form())
      .applyWithRequest(implicit request => Transitions.identifyCgtClient(cgtRef => acaConnector.getCgtSubscription(cgtRef)))

  val submitIdentifyPptClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(PptClientForm.form)
      .applyWithRequest(implicit request => Transitions.identifyPptClient(acaConnector.checkKnownFactPPT(_), acaConnector.getPptCustomerName(_)))

  val showConfirmClient: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[Confirm].orRollback

  val submitConfirmClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ConfirmClientForm)
      .applyWithRequest(
        implicit request =>
          Transitions.clientConfirmed(featureFlags.showHmrcCgt)(createMultipleInvitations)(invitationsService.createAgentLink)(getAgencyEmail)(
            hasPendingInvitationsFor)(relationshipsService.hasActiveRelationshipFor)(hasPartialAuthorisationFor)(legacySaRelationshipStatusFor)(
            appConfig))

  // TODO review whether we only need one state/page here?
  def showReviewAuthorisations: Action[AnyContent] = legacy.actionShowStateWhenAuthorised(AsAgent) {
    case _: ReviewAuthorisations =>
  }

  val submitReviewAuthorisations: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ReviewAuthorisationsForm)
      .applyWithRequest(implicit request =>
        Transitions.authorisationsReviewed(createMultipleInvitations)(invitationsService.createAgentLink)(getAgencyEmail)(createInvitationSent))

  def showDeleteAuthorisation(itemId: String): Action[AnyContent] =
    actions.whenAuthorisedWithRetrievals(AsAgent).apply(agent => Transitions.deleteAuthorisationRequest(itemId)(agent)).display

  val submitDeleteAuthorisation: Action[AnyContent] =
    actions.whenAuthorisedWithRetrievals(AsAgent).bindForm(DeleteAuthorisationForm).apply(Transitions.confirmDeleteAuthorisationRequest)

  val showInvitationSent: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[InvitationSent].display

  val showNotMatched: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[NotFound]

  val showCannotCreateRequest: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[CannotCreateRequest]

  val showSomeAuthorisationsFailed: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[SomeAuthorisationsFailed]

  val submitSomeAuthorisationsFailed: Action[AnyContent] =
    actions.whenAuthorisedWithRetrievals(AsAgent).apply(Transitions.continueSomeResponsesFailed).redirect

  val showAllAuthorisationsFailed: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[AllAuthorisationsFailed]

  val showClientNotSignedUp: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ClientNotSignedUp]

  val showClientNotRegistered: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ClientNotRegistered]

  val showPendingAuthorisationExists: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[PendingInvitationExists]

  val showActiveAuthorisationExists: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[AuthorisationExists]

  val showAllAuthorisationsRemoved: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[AllAuthorisationsRemoved.type].display

  val showAgentSuspended: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[AgentSuspended]

  val showAlreadyCopiedAcrossItsa: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[AlreadyCopiedAcrossItsa.type]

  val showLegacyAuthorisationDetected: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[LegacyAuthorisationDetected].orRollback

  val showClientInsolvent: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ClientInsolvent]

  val submitLegacyAuthorisationDetected: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { agent =>
      LegacyAuthorisationForm.bindFromRequest
        .fold(
          { hasErrors =>
            Future successful Ok(
              legacyAuthorisationDetectedView(
                hasErrors,
                routes.AgentInvitationJourneyController.submitLegacyAuthorisationDetected(),
                routes.AgentInvitationJourneyController.showConfirmClient().url))
          }, { valid =>
            if (valid.choice)
              Future successful Redirect(externalUrls.agentMappingFrontendUrl)
                .addingToSession(toReturnFromMapping)
            else
              helpers.apply(Transitions.confirmedLegacyAuthorisation(agent), helpers.redirect)
          }
        )
    }
  }

  private def toReturnFromMapping()(implicit request: Request[AnyContent]) = {
    val sessionKeyUsedInMappingService = "OriginForMapping"
    sessionKeyUsedInMappingService -> localFriendlyUrl(env)(request.path, request.host)
  }

  private def signOutUrl(implicit request: Request[AnyContent]): Future[String] =
    journeyService.initialState
      .map { is =>
        val uri = externalUrls.agentServicesAccountUrl
        val continueUrl = CallOps
          .localFriendlyUrl(env)(uri, request.host)
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
    case _: SelectClientType         => routes.AgentInvitationJourneyController.showClientType()
    case _: SelectPersonalService    => routes.AgentInvitationJourneyController.showSelectService()
    case _: SelectBusinessService    => routes.AgentInvitationJourneyController.showSelectService()
    case _: SelectTrustService       => routes.AgentInvitationJourneyController.showSelectService()
    case _: IdentifyClient           => routes.AgentInvitationJourneyController.showIdentifyClient()
    case _: ConfirmClientItsa        => routes.AgentInvitationJourneyController.showConfirmClient()
    case _: ConfirmClientPersonalVat => routes.AgentInvitationJourneyController.showConfirmClient()
    case _: ConfirmClientBusinessVat => routes.AgentInvitationJourneyController.showConfirmClient()
    case _: ConfirmClientTrust       => routes.AgentInvitationJourneyController.showConfirmClient()
    case _: ConfirmClientCgt         => routes.AgentInvitationJourneyController.showConfirmClient()
    case _: ConfirmClientPpt         => routes.AgentInvitationJourneyController.showConfirmClient()
    case _: ConfirmPostcodeCgt       => routes.AgentInvitationJourneyController.showConfirmCgtPostcode()
    case _: ConfirmCountryCodeCgt    => routes.AgentInvitationJourneyController.showConfirmCgtCountryCode()
    case _: ReviewAuthorisations     => routes.AgentInvitationJourneyController.showReviewAuthorisations()
    case DeleteAuthorisationRequest(_, authorisationRequest, _) =>
      routes.AgentInvitationJourneyController.showDeleteAuthorisation(authorisationRequest.itemId)
    case _: InvitationSentPersonal      => routes.AgentInvitationJourneyController.showInvitationSent()
    case _: InvitationSentBusiness      => routes.AgentInvitationJourneyController.showInvitationSent()
    case _: InvitationSentTrust         => routes.AgentInvitationJourneyController.showInvitationSent()
    case _: KnownFactNotMatched         => routes.AgentInvitationJourneyController.showNotMatched()
    case _: TrustNotFound               => routes.AgentInvitationJourneyController.showNotMatched()
    case _: CgtRefNotFound              => routes.AgentInvitationJourneyController.showNotMatched()
    case _: PptRefNotFound              => routes.AgentInvitationJourneyController.showNotMatched()
    case _: CannotCreateRequest         => routes.AgentInvitationJourneyController.showCannotCreateRequest()
    case _: SomeAuthorisationsFailed    => routes.AgentInvitationJourneyController.showSomeAuthorisationsFailed()
    case _: AllAuthorisationsFailed     => routes.AgentInvitationJourneyController.showAllAuthorisationsFailed()
    case _: ClientNotSignedUp           => routes.AgentInvitationJourneyController.showClientNotSignedUp()
    case _: PendingInvitationExists     => routes.AgentInvitationJourneyController.showPendingAuthorisationExists()
    case _: ActiveAuthorisationExists   => routes.AgentInvitationJourneyController.showActiveAuthorisationExists()
    case _: PartialAuthorisationExists  => routes.AgentInvitationJourneyController.showActiveAuthorisationExists()
    case AllAuthorisationsRemoved       => routes.AgentInvitationJourneyController.showAllAuthorisationsRemoved()
    case _: AgentSuspended              => routes.AgentInvitationJourneyController.showAgentSuspended()
    case _: ClientNotRegistered         => routes.AgentInvitationJourneyController.showClientNotRegistered()
    case AlreadyCopiedAcrossItsa        => routes.AgentInvitationJourneyController.showAlreadyCopiedAcrossItsa()
    case _: LegacyAuthorisationDetected => routes.AgentInvitationJourneyController.showLegacyAuthorisationDetected()
    case _: ClientInsolvent             => routes.AgentInvitationJourneyController.showClientInsolvent()
    case _                              => throw new Exception(s"Link not found for $state")
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
          Ok(selectSingleServiceView(formWithErrors.or(ServiceTypeForm.selectSingleServiceForm(config.remainingService, Personal)), config))

      case SelectBusinessService(services, basket) =>
        val config = BusinessSelectServicePageConfig(
          basket,
          featureFlags,
          services,
          backLink = backLinkFor(breadcrumbs).url,
          reviewAuthsCall = routes.AgentInvitationJourneyController.showReviewAuthorisations()
        )

        if (config.showMultiSelect)
          Ok(selectFromServicesView(formWithErrors.or(ServiceTypeForm.form), config))
        else
          Ok(selectSingleServiceView(formWithErrors.or(ServiceTypeForm.selectSingleServiceForm(config.remainingService, Business)), config))

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
          Ok(selectSingleServiceView(formWithErrors.or(ServiceTypeForm.selectSingleServiceForm(config.remainingService, Trust)), config))
        }

      case IdentifyClient(Trust, Services.TRUST, _) =>
        Ok(
          identifyClientTrustView(
            trustClientForm = formWithErrors.or(TrustClientForm.form(urnEnabled)),
            submitFormCall = routes.AgentInvitationJourneyController.submitIdentifyTrustClient(),
            backLinkUrl = backLinkFor(breadcrumbs).url,
            isDeAuthJourney = false,
            showUrnEnabledContent = urnEnabled
          )
        )
      case IdentifyClient(Trust, Services.HMRCCGTPD, _) =>
        Ok(
          identifyClientCgtView(
            formWithErrors.or(CgtClientForm.form()),
            routes.AgentInvitationJourneyController.submitIdentifyCgtClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(Trust, Services.HMRCPPTORG, _) =>
        Ok(
          identifyClientPptView(
            formWithErrors.or(PptClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyPptClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(Personal, Services.HMRCMTDIT, _) =>
        Ok(
          identifyClientItsaView(
            formWithErrors.or(ItsaClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyItsaClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(Personal, Services.HMRCMTDVAT, _) =>
        Ok(
          identifyClientVatView(
            formWithErrors.or(VatClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyVatClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(Personal, Services.HMRCPIR, _) =>
        Ok(
          identifyClientIrvView(
            formWithErrors.or(IrvClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyIrvClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(Personal, Services.HMRCCGTPD, _) =>
        Ok(
          identifyClientCgtView(
            formWithErrors.or(CgtClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyCgtClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(Personal, Services.HMRCPPTORG, _) =>
        Ok(
          identifyClientPptView(
            formWithErrors.or(PptClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyPptClient,
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(Business, Services.HMRCMTDVAT, _) =>
        Ok(
          identifyClientVatView(
            formWithErrors.or(VatClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyVatClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(Business, Services.HMRCPPTORG, _) =>
        Ok(
          identifyClientPptView(
            formWithErrors.or(PptClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyPptClient(),
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

      case ConfirmClientPersonalVat(authorisationRequest, _, _) =>
        Ok(
          confirmClientView(
            authorisationRequest.clientName,
            formWithErrors.or(ConfirmClientForm),
            backLinkFor(breadcrumbs).url,
            routes.AgentInvitationJourneyController.submitConfirmClient(),
            authorisationRequest.invitation.clientIdentifierType,
            authorisationRequest.invitation.clientId
          ))

      case ConfirmClientBusinessVat(authorisationRequest, _, _) =>
        Ok(
          confirmClientView(
            authorisationRequest.clientName,
            formWithErrors.or(ConfirmClientForm),
            backLinkFor(breadcrumbs).url,
            routes.AgentInvitationJourneyController.submitConfirmClient(),
            authorisationRequest.invitation.clientIdentifierType,
            authorisationRequest.invitation.clientId
          ))

      case ConfirmClientPpt(authorisationRequest, basket) =>
        Ok(
          confirmClientView(
            authorisationRequest.clientName,
            formWithErrors.or(ConfirmClientForm),
            backLinkFor(breadcrumbs).url,
            routes.AgentInvitationJourneyController.submitConfirmClient(),
            authorisationRequest.invitation.clientIdentifierType,
            authorisationRequest.invitation.clientId
          ))

      case ReviewAuthorisations(Personal, services, basket) =>
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

      case ReviewAuthorisations(Business, services, basket) =>
        Ok(
          reviewAuthView(
            ReviewAuthorisationsBusinessPageConfig(
              basket,
              featureFlags,
              services,
              routes.AgentInvitationJourneyController.submitReviewAuthorisations()),
            formWithErrors.or(ReviewAuthorisationsForm),
            backLinkFor(breadcrumbs).url
          ))

      case ReviewAuthorisations(Trust, services, basket) =>
        Ok(
          reviewAuthView(
            ReviewAuthorisationsTrustPageConfig(basket, featureFlags, services, routes.AgentInvitationJourneyController.submitReviewAuthorisations()),
            formWithErrors.or(ReviewAuthorisationsForm),
            backLinkFor(breadcrumbs).url
          ))

      case DeleteAuthorisationRequest(_, authorisationRequest, _) =>
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
              ClientType.fromEnum(Personal),
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
              ClientType.fromEnum(Business),
              inferredExpiryDate,
              agencyEmail,
              services,
              isAltItsa = false,
              services.head)))

      case InvitationSentTrust(invitationLink, continueUrl, agencyEmail, services) =>
        Ok(
          invitationSentView(
            InvitationSentPageConfig(
              invitationLink,
              None,
              continueUrl.isDefined,
              ClientType.fromEnum(Trust),
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

      case PptRefNotFound(pptRef, basket) =>
        Ok(
          pptRefNotFoundView(
            basket.nonEmpty,
            routes.AgentInvitationJourneyController.showIdentifyClient(),
            Some(routes.AgentInvitationJourneyController.showReviewAuthorisations()),
            pptRef.value
          )
        )

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

      case LegacyAuthorisationDetected(_) =>
        Ok(
          legacyAuthorisationDetectedView(
            formWithErrors.or(LegacyAuthorisationForm),
            routes.AgentInvitationJourneyController.submitLegacyAuthorisationDetected(),
            backLinkFor(breadcrumbs).url
          ))

      case ClientInsolvent(basket) =>
        Ok(
          clientInsolventView(hasRequests = basket.nonEmpty, isFastTrack = false)
        )

      case _ => throw new Exception(s"Cannot render a page for unexpected state: $state, add your state as a match case in #renderState")
    }
}

object AgentInvitationJourneyController {

  import uk.gov.hmrc.agentinvitationsfrontend.forms.CommonConfirmationForms._

  val ConfirmClientForm: Form[Confirmation] = confirmationForm("error.confirm-client.required")

  val ReviewAuthorisationsForm: Form[Confirmation] = confirmationForm("error.review-authorisation.required")

  val LegacyAuthorisationForm: Form[Confirmation] = confirmationForm("error.legacy-auth-detected.required")

  val DeleteAuthorisationForm: Form[Confirmation] = confirmationForm("error.delete.radio")
}
