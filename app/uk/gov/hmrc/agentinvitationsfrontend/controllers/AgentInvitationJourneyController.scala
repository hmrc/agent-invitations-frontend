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

import play.api.Configuration
import play.api.data.Form
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.agentinvitationsfrontend.config.{AppConfig, CountryNamesLoader, ExternalUrls}
import uk.gov.hmrc.agentinvitationsfrontend.connectors.AgentClientAuthorisationConnector
import uk.gov.hmrc.agentinvitationsfrontend.forms._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.support.CallOps
import uk.gov.hmrc.agentinvitationsfrontend.support.CallOps.localFriendlyUrl
import uk.gov.hmrc.agentinvitationsfrontend.views.agents._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.timed_out
import uk.gov.hmrc.agentmtdidentifiers.model.Service
import uk.gov.hmrc.hmrcfrontend.config.ContactFrontendConfig
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.play.fsm.JourneyController

import java.time.LocalDate
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AgentInvitationJourneyController @Inject() (
  invitationsService: InvitationsService,
  relationshipsService: RelationshipsService,
  knownFactService: KnownFactService,
  acaConnector: AgentClientAuthorisationConnector,
  val authActions: AuthActions,
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
  identifyClientCbcView: identify_client_cbc,
  identifyClientPillar2View: identify_client_pillar2,
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
  clientInsolventView: client_insolvent
)(implicit
  configuration: Configuration,
  implicit val contactFrontendConfig: ContactFrontendConfig,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  ec: ExecutionContext,
  val cc: MessagesControllerComponents,
  appConfig: AppConfig,
  override val actionBuilder: DefaultActionBuilder
) extends FrontendController(cc) with JourneyController[HeaderCarrier] with I18nSupport {

  import AgentInvitationJourneyController._
  import acaConnector._
  import authActions._
  import journeyService.model._
  import uk.gov.hmrc.play.fsm.OptionalFormOps._

  override implicit def context(implicit rh: RequestHeader): HeaderCarrier = hc

  // TODO Add local date service to provide flexibility for testing
  private def inferredExpiryDate = LocalDate.now().plusDays(appConfig.invitationExpirationDuration.toDays.toInt)

  private val countries = countryNamesLoader.load
  private val validCountryCodes = countries.keys.toSet

  val AsAgent: WithAuthorised[AuthorisedAgent] = { implicit request: Request[Any] =>
    withAuthorisedAsAgent(_)
  }

  /* Here we decide how to handle HTTP request and transition the state of the journey */

  def transitions()(implicit ec: ExecutionContext, request: RequestHeader) = Transitions(
    appConfig = appConfig,
    featureFlags = featureFlags,
    getSuspensionDetails = getAgencySuspensionDetails,
    hasPendingInvitationsFor = invitationsService.hasPendingInvitationsFor,
    hasActiveRelationshipFor = relationshipsService.hasActiveRelationshipFor,
    hasPartialAuthorisationFor = invitationsService.hasPartialAuthorisationFor,
    legacySaRelationshipStatusFor = invitationsService.legacySaRelationshipStatusFor,
    hasAltItsaInvitations = invitationsService.hasPartialAuthorisationFor,
    getClientName = invitationsService.getClientNameByService,
    getAgentLink = invitationsService.createAgentLink,
    getAgencyEmail = getAgencyEmail,
    createMultipleInvitations = invitationsService.createMultipleInvitations,
    createInvitationSent = invitationsService.createInvitationSent,
    getCgtSubscription = acaConnector.getCgtSubscription,
    getCbcSubscription = acaConnector.getCbcSubscription,
    checkKnownFact = knownFactService.checkKnownFact
  )

  val agentsRoot: Action[AnyContent] = Action(Redirect(routes.AgentInvitationJourneyController.showClientType))

  val showClientType: Action[AnyContent] = if (appConfig.enableAcrfRedirects) {
    Action(Redirect(appConfig.createAuthRequestUrl))
  } else {
    actions.whenAuthorised(AsAgent).show[SelectClientType]
  }

  val submitClientType: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ClientTypeForm.authorisationForm)
      .applyWithRequest(implicit request => transitions.selectedClientType)

  // TODO: Broken using DSL
  val showSelectService: Action[AnyContent] = legacy.actionShowStateWhenAuthorised(AsAgent) { case _: SelectService =>
  }

  val submitSelectServiceMulti: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ServiceTypeForm.form)
      .applyWithRequest(implicit request => transitions.selectedServiceMulti)

  def submitSelectServiceSingle(serviceId: String, clientType: String): Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ServiceTypeForm.selectSingleServiceForm(Service.forId(serviceId), ClientType.toEnum(clientType)))
      .applyWithRequest(implicit request => transitions.selectedService)

  val showIdentifyClient: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[IdentifyClient].orRollback

  val showConfirmCgtPostcode: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmPostcodeCgt].orRollback

  val submitConfirmCgtPostcode: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(PostcodeForm.form)
      .applyWithRequest(implicit request => transitions.confirmPostcodeCgt)

  val showConfirmCgtCountryCode: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmCountryCodeCgt].orRollback

  val submitConfirmCgtCountryCode: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(CountrycodeForm.form(validCountryCodes))
      .applyWithRequest(implicit request => transitions.confirmCountryCodeCgt)

  val submitIdentifyItsaClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ItsaClientForm.form)
      .applyWithRequest(implicit request => transitions.identifiedItsaClient)

  val submitIdentifyVatClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(VatClientForm.form)
      .applyWithRequest(implicit request => transitions.identifiedVatClient)

  val submitIdentifyIrvClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(IrvClientForm.form)
      .applyWithRequest(implicit request => transitions.identifiedIrvClient)

  val submitIdentifyTrustClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(TrustClientForm.form)
      .applyWithRequest(implicit request => transitions.identifiedTrustClient)

  val submitIdentifyCgtClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(CgtClientForm.form())
      .applyWithRequest(implicit request => transitions.identifyCgtClient)

  val submitIdentifyPptClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(PptClientForm.form)
      .applyWithRequest(implicit request => transitions.identifyPptClient)

  val submitIdentifyCbcClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(CbcClientForm.form)
      .applyWithRequest(implicit request => transitions.identifyCbcClient)

  val submitIdentifyPillar2Client: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(Pillar2ClientForm.form)
      .applyWithRequest(implicit request => transitions.identifyPillar2Client)

  val showConfirmClient: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmClient].orRollback

  val submitConfirmClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ConfirmClientForm)
      .applyWithRequest(implicit request => transitions.clientConfirmed)

  // TODO review whether we only need one state/page here?
  def showReviewAuthorisations: Action[AnyContent] = legacy.actionShowStateWhenAuthorised(AsAgent) { case _: ReviewAuthorisations =>
  }

  val submitReviewAuthorisations: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ReviewAuthorisationsForm)
      .applyWithRequest(implicit request => transitions.authorisationsReviewed)

  def showDeleteAuthorisation(itemId: String): Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .applyWithRequest(implicit request => agent => transitions.deleteAuthorisationRequest(itemId)(agent))
      .display

  val submitDeleteAuthorisation: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(DeleteAuthorisationForm)
      .applyWithRequest(implicit request => transitions.confirmDeleteAuthorisationRequest)

  val showInvitationSent: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[InvitationSent].display

  val showNotMatched: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[NotFound]

  val showCannotCreateRequest: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[CannotCreateRequest]

  val showSomeAuthorisationsFailed: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[SomeAuthorisationsFailed]

  val submitSomeAuthorisationsFailed: Action[AnyContent] =
    actions.whenAuthorisedWithRetrievals(AsAgent).applyWithRequest(implicit requeest => transitions.continueSomeResponsesFailed).redirect

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
          hasErrors =>
            Future successful Ok(
              legacyAuthorisationDetectedView(
                hasErrors,
                routes.AgentInvitationJourneyController.submitLegacyAuthorisationDetected,
                routes.AgentInvitationJourneyController.showConfirmClient.url
              )
            ),
          valid =>
            if (valid.choice)
              Future successful Redirect(externalUrls.agentMappingFrontendUrl)
                .addingToSession(toReturnFromMapping)
            else
              helpers.apply(transitions.confirmedLegacyAuthorisation(agent), helpers.redirect)
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
    case _: SelectClientType      => routes.AgentInvitationJourneyController.showClientType
    case _: SelectService         => routes.AgentInvitationJourneyController.showSelectService
    case _: IdentifyClient        => routes.AgentInvitationJourneyController.showIdentifyClient
    case _: ConfirmClient         => routes.AgentInvitationJourneyController.showConfirmClient
    case _: ConfirmPostcodeCgt    => routes.AgentInvitationJourneyController.showConfirmCgtPostcode
    case _: ConfirmCountryCodeCgt => routes.AgentInvitationJourneyController.showConfirmCgtCountryCode
    case _: ReviewAuthorisations  => routes.AgentInvitationJourneyController.showReviewAuthorisations
    case DeleteAuthorisationRequest(_, authorisationRequest, _) =>
      routes.AgentInvitationJourneyController.showDeleteAuthorisation(authorisationRequest.itemId)
    case _: InvitationSent              => routes.AgentInvitationJourneyController.showInvitationSent
    case _: KnownFactNotMatched         => routes.AgentInvitationJourneyController.showNotMatched
    case _: TrustNotFound               => routes.AgentInvitationJourneyController.showNotMatched
    case _: CgtRefNotFound              => routes.AgentInvitationJourneyController.showNotMatched
    case _: PptRefNotFound              => routes.AgentInvitationJourneyController.showNotMatched
    case _: CannotCreateRequest         => routes.AgentInvitationJourneyController.showCannotCreateRequest
    case _: SomeAuthorisationsFailed    => routes.AgentInvitationJourneyController.showSomeAuthorisationsFailed
    case _: AllAuthorisationsFailed     => routes.AgentInvitationJourneyController.showAllAuthorisationsFailed
    case _: ClientNotSignedUp           => routes.AgentInvitationJourneyController.showClientNotSignedUp
    case _: PendingInvitationExists     => routes.AgentInvitationJourneyController.showPendingAuthorisationExists
    case _: ActiveAuthorisationExists   => routes.AgentInvitationJourneyController.showActiveAuthorisationExists
    case _: PartialAuthorisationExists  => routes.AgentInvitationJourneyController.showActiveAuthorisationExists
    case AllAuthorisationsRemoved       => routes.AgentInvitationJourneyController.showAllAuthorisationsRemoved
    case _: AgentSuspended              => routes.AgentInvitationJourneyController.showAgentSuspended
    case _: ClientNotRegistered         => routes.AgentInvitationJourneyController.showClientNotRegistered
    case AlreadyCopiedAcrossItsa        => routes.AgentInvitationJourneyController.showAlreadyCopiedAcrossItsa
    case _: LegacyAuthorisationDetected => routes.AgentInvitationJourneyController.showLegacyAuthorisationDetected
    case _: ClientInsolvent             => routes.AgentInvitationJourneyController.showClientInsolvent
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
            ClientTypePageConfig(backLinkForClientType, routes.AgentInvitationJourneyController.submitClientType)
          )
        )

      case SelectService(clientType, services, basket) =>
        val config = SelectServicePageConfig(
          clientType,
          basket,
          featureFlags,
          services,
          backLinkFor(breadcrumbs).url,
          routes.AgentInvitationJourneyController.showReviewAuthorisations
        )

        if (config.showMultiSelect)
          Ok(selectFromServicesView(formWithErrors.asInstanceOf[Option[Form[Service]]].or(ServiceTypeForm.form), config))
        else
          Ok(
            selectSingleServiceView(
              formWithErrors
                .asInstanceOf[Option[Form[Option[Service]]]]
                .or(ServiceTypeForm.selectSingleServiceForm(config.remainingService, clientType)),
              config
            )
          )

      case IdentifyClient(ClientType.Trust, Service.Trust, _) =>
        Ok(
          identifyClientTrustView(
            trustClientForm = formWithErrors.or(TrustClientForm.form),
            submitFormCall = routes.AgentInvitationJourneyController.submitIdentifyTrustClient,
            backLinkUrl = backLinkFor(breadcrumbs).url,
            isDeAuthJourney = false
          )
        )

      case IdentifyClient(ClientType.Personal, Service.MtdIt, _) =>
        Ok(
          identifyClientItsaView(
            formWithErrors.or(ItsaClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyItsaClient,
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(ClientType.Personal, Service.PersonalIncomeRecord, _) =>
        Ok(
          identifyClientIrvView(
            formWithErrors.or(IrvClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyIrvClient,
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(_, Service.Vat, _) =>
        Ok(
          identifyClientVatView(
            formWithErrors.or(VatClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyVatClient,
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(_, Service.CapitalGains, _) =>
        Ok(
          identifyClientCgtView(
            formWithErrors.or(CgtClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyCgtClient,
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(_, Service.Ppt, _) =>
        Ok(
          identifyClientPptView(
            formWithErrors.or(PptClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyPptClient,
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(_, Service.Cbc | Service.CbcNonUk, _) =>
        Ok(
          identifyClientCbcView(
            formWithErrors.or(CbcClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyCbcClient,
            backLinkFor(breadcrumbs).url
          )
        )
      case IdentifyClient(_, Service.Pillar2, _) =>
        Ok(
          identifyClientPillar2View(
            formWithErrors.or(Pillar2ClientForm.form),
            routes.AgentInvitationJourneyController.submitIdentifyPillar2Client,
            backLinkFor(breadcrumbs).url
          )
        )

      case ConfirmClient(authorisationRequest, _, _) =>
        Ok(
          confirmClientView(
            authorisationRequest.clientName,
            formWithErrors.or(ConfirmClientForm),
            backLinkFor(breadcrumbs).url,
            routes.AgentInvitationJourneyController.submitConfirmClient,
            authorisationRequest.invitation.clientIdentifier
          )
        )

      case ConfirmPostcodeCgt(_, clientType, _, _, _) =>
        Ok(
          confirmPostcodeCgtView(
            clientType,
            formWithErrors.or(PostcodeForm.form),
            backLinkFor(breadcrumbs).url,
            fromFastTrack = false,
            isDeAuth = false
          )
        )

      case ConfirmCountryCodeCgt(_, clientType, _, _, _) =>
        Ok(
          confirmCountryCodeCgtView(
            clientType,
            countries,
            formWithErrors.or(CountrycodeForm.form(validCountryCodes)),
            backLinkFor(breadcrumbs).url,
            fromFastTrack = false,
            isDeAuth = false
          )
        )

      case ReviewAuthorisations(clientType, services, basket) =>
        Ok(
          reviewAuthView(
            ReviewAuthorisationsPageConfig(
              clientType,
              basket,
              featureFlags,
              services,
              routes.AgentInvitationJourneyController.submitReviewAuthorisations
            ),
            formWithErrors.or(ReviewAuthorisationsForm),
            backLinkFor(breadcrumbs).url
          )
        )

      case DeleteAuthorisationRequest(_, authorisationRequest, _) =>
        Ok(
          deleteView(
            authorisationRequest,
            routes.AgentInvitationJourneyController.submitDeleteAuthorisation,
            routes.AgentInvitationJourneyController.showReviewAuthorisations.url,
            formWithErrors.or(DeleteAuthorisationForm)
          )
        )

      case InvitationSent(clientType, invitationLink, continueUrl, agencyEmail, services, isAltItsa) =>
        Ok(
          invitationSentView(
            InvitationSentPageConfig(
              invitationLink,
              None,
              continueUrl.isDefined,
              ClientType.fromEnum(clientType),
              inferredExpiryDate,
              agencyEmail,
              services,
              isAltItsa.getOrElse(false)
            )
          )
        )

      case KnownFactNotMatched(basket) =>
        Ok(
          notMatchedView(
            basket.nonEmpty,
            routes.AgentInvitationJourneyController.showIdentifyClient,
            Some(routes.AgentInvitationJourneyController.showReviewAuthorisations)
          )
        )

      case TrustNotFound(basket) =>
        Ok(
          notMatchedView(
            basket.nonEmpty,
            routes.AgentInvitationJourneyController.showIdentifyClient,
            Some(routes.AgentInvitationJourneyController.showReviewAuthorisations)
          )
        )

      case CgtRefNotFound(cgtRef, basket) =>
        Ok(
          cgtRefNotFoundView(
            basket.nonEmpty,
            routes.AgentInvitationJourneyController.showIdentifyClient,
            Some(routes.AgentInvitationJourneyController.showReviewAuthorisations),
            cgtRef.value
          )
        )

      case PptRefNotFound(pptRef, basket) =>
        Ok(
          pptRefNotFoundView(
            basket.nonEmpty,
            routes.AgentInvitationJourneyController.showIdentifyClient,
            Some(routes.AgentInvitationJourneyController.showReviewAuthorisations),
            pptRef.value
          )
        )

      case CannotCreateRequest(basket) =>
        Ok(cannotCreateRequestView(hasRequests = basket.nonEmpty, fromFastTrack = false, backLinkFor(breadcrumbs).url))

      case SomeAuthorisationsFailed(_, _, _, basket) =>
        Ok(invitationCreationFailedView(InvitationCreationFailedPageConfig(basket, isAll = false)))

      case AllAuthorisationsFailed(basket) =>
        Ok(invitationCreationFailedView(InvitationCreationFailedPageConfig(basket, isAll = true)))

      case ActiveAuthorisationExists(clientType, service, basket) =>
        Ok(
          activeAuthExistsView(
            basket.nonEmpty,
            service,
            clientType,
            fromFastTrack = false,
            routes.AgentInvitationJourneyController.showReviewAuthorisations,
            routes.AgentInvitationJourneyController.showClientType
          )
        )

      case PendingInvitationExists(_, clientName, agentLink, basket) =>
        Ok(
          pendingAuthExistsView(
            clientName,
            agentLink,
            authRequestsExist = basket.nonEmpty,
            backLinkFor(breadcrumbs).url,
            fromFastTrack = false,
            routes.AgentInvitationJourneyController.showReviewAuthorisations,
            routes.AgentInvitationJourneyController.showClientType
          )
        )

      case PartialAuthorisationExists(basket) =>
        Ok(
          partialAuthExistsView(
            basket.nonEmpty,
            fromFastTrack = false,
            routes.AgentInvitationJourneyController.showReviewAuthorisations,
            routes.AgentInvitationJourneyController.showClientType
          )
        )

      case ClientNotSignedUp(service, basket) =>
        val pageConfig = notSignedUpPageConfig.render(service)
        Ok(notSignedupView(service, basket.nonEmpty, isDeAuthJourney = false, pageConfig))

      case ClientNotRegistered(basket) =>
        Ok(
          clientNotRegisteredView(
            basket.nonEmpty,
            fromFastTrack = false,
            routes.AgentInvitationJourneyController.showReviewAuthorisations,
            routes.AgentInvitationJourneyController.showClientType
          )
        )

      case AllAuthorisationsRemoved =>
        Ok(allAuthRemovedView(routes.AgentInvitationJourneyController.showClientType))

      case AgentSuspended(suspendedService, basket) =>
        Ok(agentSuspendedView(basket, suspendedService, backLinkFor(breadcrumbs).url))

      case AlreadyCopiedAcrossItsa => Ok(alreadyCopiedAcrossView())

      case LegacyAuthorisationDetected(_) =>
        Ok(
          legacyAuthorisationDetectedView(
            formWithErrors.or(LegacyAuthorisationForm),
            routes.AgentInvitationJourneyController.submitLegacyAuthorisationDetected,
            backLinkFor(breadcrumbs).url
          )
        )

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
