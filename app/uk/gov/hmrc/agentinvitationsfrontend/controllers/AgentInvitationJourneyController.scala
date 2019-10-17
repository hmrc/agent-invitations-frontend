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
import org.joda.time.LocalDate
import play.api.data.Form
import play.api.i18n.I18nSupport
import play.api.mvc._
import play.api.{Configuration, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.config.{CountryNamesLoader, ExternalUrls}
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{AgentServicesAccountConnector, InvitationsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.forms._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.views.agents._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.play.fsm.JourneyController

import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AgentInvitationJourneyController @Inject()(
  @Named("invitation.expiryDuration") expiryDuration: String,
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  relationshipsService: RelationshipsService,
  asaConnector: AgentServicesAccountConnector,
  val authActions: AuthActions,
  override val journeyService: AgentInvitationJourneyService,
  countryNamesLoader: CountryNamesLoader)(
  implicit configuration: Configuration,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  val messagesApi: play.api.i18n.MessagesApi,
  ec: ExecutionContext)
    extends FrontendController with JourneyController[HeaderCarrier] with I18nSupport {

  import AgentInvitationJourneyController._
  import asaConnector._
  import authActions._
  import invitationsService._
  import journeyService.model.State._
  import journeyService.model.{State, Transitions}
  import uk.gov.hmrc.play.fsm.OptionalFormOps._

  override implicit def context(implicit rh: RequestHeader): HeaderCarrier = hc

  private val invitationExpiryDuration = Duration(expiryDuration.replace('_', ' '))
  private val inferredExpiryDate = LocalDate.now().plusDays(invitationExpiryDuration.toDays.toInt)

  private val countries = countryNamesLoader.load
  private val validCountryCodes = countries.keys.toSet

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
        featureFlags.showHmrcCgt))
  }

  def submitPersonalSelectSingle(service: String): Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ServiceTypeForm.selectSingleServiceForm(service, personal))(
      Transitions.selectedPersonalService(
        featureFlags.showHmrcMtdIt,
        featureFlags.showPersonalIncome,
        featureFlags.showHmrcMtdVat,
        featureFlags.showHmrcCgt))
  }

  def submitBusinessSelectService: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ServiceTypeForm.selectSingleServiceForm(HMRCMTDVAT, business))(
      Transitions.selectedBusinessService(featureFlags.showHmrcMtdVat))
  }

  def submitTrustSelectSingle(service: String): Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ServiceTypeForm.selectSingleServiceForm(service, business))(
      Transitions.selectedTrustService(featureFlags.showHmrcTrust, featureFlags.showHmrcCgt))
  }

  // this is only for multi-select option forms
  def submitTrustSelectServiceMultiple: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ServiceTypeForm.form)(
      Transitions.selectedTrustService(featureFlags.showHmrcTrust, featureFlags.showHmrcCgt))
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
    whenAuthorisedWithForm(AsAgent)(PostcodeForm.form)(Transitions.confirmPostcodeCgt(cgtRef =>
      invitationsConnector.getCgtSubscription(cgtRef)))
  }

  def showConfirmCgtCountryCode: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: ConfirmCountryCodeCgt =>
  }

  def submitConfirmCgtCountryCode: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(CountrycodeForm.form(validCountryCodes))(Transitions.confirmCountryCodeCgt(cgtRef =>
      invitationsConnector.getCgtSubscription(cgtRef)))
  }

  def submitIdentifyItsaClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ItsaClientForm.form)(
      Transitions.identifiedItsaClient(checkPostcodeMatches)(hasPendingInvitationsFor)(
        relationshipsService.hasActiveRelationshipFor)(getClientNameByService)(createMultipleInvitations)(
        createAgentLink)(getAgencyEmail)
    )
  }

  def submitIdentifyVatClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(VatClientForm.form)(
      Transitions.identifiedVatClient(checkVatRegistrationDateMatches)(hasPendingInvitationsFor)(
        relationshipsService.hasActiveRelationshipFor)(getClientNameByService)(createMultipleInvitations)(
        createAgentLink)(getAgencyEmail)
    )
  }

  def submitIdentifyIrvClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(IrvClientForm.form)(
      Transitions.identifiedIrvClient(checkCitizenRecordMatches)(hasPendingInvitationsFor)(
        relationshipsService.hasActiveRelationshipFor)(getClientNameByService)(createMultipleInvitations)(
        createAgentLink)(getAgencyEmail)
    )
  }

  def submitIdentifyTrustClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(TrustClientForm.form)(
      Transitions.identifiedTrustClient(utr => invitationsConnector.getTrustName(utr))
    )
  }

  def submitIdentifyCgtClient: Action[AnyContent] = action { implicit request =>
    journeyService.currentState.map(p => p.map(_._1)).flatMap {
      case Some(s) =>
        val clientType = s match {
          case _: IdentifyPersonalClient => Some(personal)
          case _: IdentifyTrustClient    => Some(business)
          case p =>
            Logger.warn(s"unexpected state ($p) for identifying CGT client")
            None
        }

        clientType match {
          case Some(ct) =>
            whenAuthorisedWithForm(AsAgent)(CgtClientForm.form(ct))(
              Transitions.identifyCgtClient(cgtRef => invitationsConnector.getCgtSubscription(cgtRef))
            )
          case None =>
            Future.successful(Redirect(routes.AgentInvitationJourneyController.showClientType()))
        }

      case _ =>
        Logger.warn("expecting some state here, but missing")
        Future.successful(Redirect(routes.AgentInvitationJourneyController.showClientType()))
    }
  }

  def showConfirmClient: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: ConfirmClientItsa        =>
    case _: ConfirmClientPersonalVat =>
    case _: ConfirmClientBusinessVat =>
    case _: ConfirmClientTrust       =>
    case _: ConfirmClientCgt         =>
  }

  def submitConfirmClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ConfirmClientForm)(
      Transitions.clientConfirmed(featureFlags.showHmrcCgt)(createMultipleInvitations)(createAgentLink)(getAgencyEmail)(
        hasPendingInvitationsFor)(relationshipsService.hasActiveRelationshipFor)
    )
  }

  // TODO review whether we only need one state/page here?
  def showReviewAuthorisations: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: ReviewAuthorisationsPersonal | _: ReviewAuthorisationsTrust =>
  }

  def submitReviewAuthorisations: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ReviewAuthorisationsForm)(
      Transitions.authorisationsReviewed(createMultipleInvitations)(createAgentLink)(getAgencyEmail))
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

  def showPendingAuthorisationExists: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: PendingInvitationExists =>
  }
  def showActiveAuthorisationExists: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: ActiveAuthorisationExists =>
  }

  def showAllAuthorisationsRemoved: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case AllAuthorisationsRemoved =>
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
    case _: InvitationSentPersonal    => routes.AgentInvitationJourneyController.showInvitationSent()
    case _: InvitationSentBusiness    => routes.AgentInvitationJourneyController.showInvitationSent()
    case _: KnownFactNotMatched       => routes.AgentInvitationJourneyController.showNotMatched()
    case _: TrustNotFound             => routes.AgentInvitationJourneyController.showNotMatched()
    case _: CgtRefNotFound            => routes.AgentInvitationJourneyController.showNotMatched()
    case _: CannotCreateRequest       => routes.AgentInvitationJourneyController.showCannotCreateRequest()
    case _: SomeAuthorisationsFailed  => routes.AgentInvitationJourneyController.showSomeAuthorisationsFailed()
    case _: AllAuthorisationsFailed   => routes.AgentInvitationJourneyController.showAllAuthorisationsFailed()
    case _: ClientNotSignedUp         => routes.AgentInvitationJourneyController.showClientNotSignedUp()
    case _: PendingInvitationExists   => routes.AgentInvitationJourneyController.showPendingAuthorisationExists()
    case _: ActiveAuthorisationExists => routes.AgentInvitationJourneyController.showActiveAuthorisationExists()
    case AllAuthorisationsRemoved     => routes.AgentInvitationJourneyController.showAllAuthorisationsRemoved()
    case _                            => throw new Exception(s"Link not found for $state")
  }

  /* Here we decide what to render after state transition */
  override def renderState(state: State, breadcrumbs: List[State], formWithErrors: Option[Form[_]])(
    implicit request: Request[_]): Result = state match {

    case SelectClientType(_) =>
      def backLinkForClientType(implicit request: Request[_]): String =
        breadcrumbs.headOption.fold(s"${externalUrls.agentServicesAccountUrl}/agent-services-account")(
          getCallFor(_).url)

      Ok(
        client_type(
          formWithErrors.or(ClientTypeForm.authorisationForm),
          ClientTypePageConfig(
            backLinkForClientType,
            routes.AgentInvitationJourneyController.submitClientType(),
            featureFlags.showHmrcTrust)
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
        Ok(select_from_services(formWithErrors.or(ServiceTypeForm.form), config))
      else
        Ok(
          select_single_service(
            formWithErrors.or(ServiceTypeForm.selectSingleServiceForm(config.remainingService, personal)),
            config))

    case SelectBusinessService =>
      Ok(
        select_single_service(
          formWithErrors.or(ServiceTypeForm.selectSingleServiceForm(HMRCMTDVAT, business)),
          BusinessSelectServicePageConfig(
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
        Ok(select_from_services(formWithErrors.or(ServiceTypeForm.form), config))
      } else {
        Ok(
          select_single_service(
            formWithErrors.or(ServiceTypeForm.selectSingleServiceForm(config.remainingService, business)),
            config))
      }

    case IdentifyTrustClient(Services.TRUST, _) =>
      Ok(
        identify_client_trust(
          formWithErrors.or(TrustClientForm.form),
          routes.AgentInvitationJourneyController.submitIdentifyTrustClient(),
          backLinkFor(breadcrumbs).url
        )
      )

    case IdentifyTrustClient(Services.HMRCCGTPD, _) =>
      Ok(
        identify_client_cgt(
          business,
          formWithErrors.or(CgtClientForm.form(business)),
          routes.AgentInvitationJourneyController.submitIdentifyCgtClient(),
          backLinkFor(breadcrumbs).url
        )
      )

    case IdentifyPersonalClient(Services.HMRCMTDIT, _) =>
      Ok(
        identify_client_itsa(
          formWithErrors.or(ItsaClientForm.form),
          routes.AgentInvitationJourneyController.submitIdentifyItsaClient(),
          backLinkFor(breadcrumbs).url
        )
      )

    case IdentifyPersonalClient(Services.HMRCMTDVAT, _) =>
      Ok(
        identify_client_vat(
          formWithErrors.or(VatClientForm.form),
          routes.AgentInvitationJourneyController.submitIdentifyVatClient(),
          backLinkFor(breadcrumbs).url
        )
      )

    case IdentifyPersonalClient(Services.HMRCPIR, _) =>
      Ok(
        identify_client_irv(
          formWithErrors.or(IrvClientForm.form),
          routes.AgentInvitationJourneyController.submitIdentifyIrvClient(),
          backLinkFor(breadcrumbs).url
        )
      )

    case IdentifyPersonalClient(Services.HMRCCGTPD, _) =>
      Ok(
        identify_client_cgt(
          personal,
          formWithErrors.or(CgtClientForm.form(personal)),
          routes.AgentInvitationJourneyController.submitIdentifyCgtClient(),
          backLinkFor(breadcrumbs).url
        )
      )

    case IdentifyBusinessClient =>
      Ok(
        identify_client_vat(
          formWithErrors.or(VatClientForm.form),
          routes.AgentInvitationJourneyController.submitIdentifyVatClient(),
          backLinkFor(breadcrumbs).url
        )
      )

    case ConfirmClientTrust(authorisationRequest, _) =>
      Ok(
        confirm_client(
          authorisationRequest.clientName,
          formWithErrors.or(ConfirmClientForm),
          backLinkFor(breadcrumbs).url,
          routes.AgentInvitationJourneyController.submitConfirmClient(),
          authorisationRequest.invitation.clientIdentifierType,
          authorisationRequest.invitation.clientId
        ))

    case ConfirmClientCgt(authorisationRequest, _) =>
      Ok(
        confirm_client(
          authorisationRequest.clientName,
          formWithErrors.or(ConfirmClientForm),
          backLinkFor(breadcrumbs).url,
          routes.AgentInvitationJourneyController.submitConfirmClient(),
          authorisationRequest.invitation.clientIdentifierType,
          authorisationRequest.invitation.clientId
        ))

    case ConfirmPostcodeCgt(_, clientType, _, _, _) =>
      Ok(confirm_postcode_cgt(clientType, formWithErrors.or(PostcodeForm.form), backLinkFor(breadcrumbs).url))

    case ConfirmCountryCodeCgt(_, clientType, _, _, _) =>
      Ok(
        confirm_countryCode_cgt(
          clientType,
          countries,
          formWithErrors.or(CountrycodeForm.form(validCountryCodes)),
          backLinkFor(breadcrumbs).url))

    case ConfirmClientItsa(authorisationRequest, _) =>
      Ok(
        confirm_client(
          authorisationRequest.clientName,
          formWithErrors.or(ConfirmClientForm),
          backLinkFor(breadcrumbs).url,
          routes.AgentInvitationJourneyController.submitConfirmClient(),
          authorisationRequest.invitation.clientIdentifierType,
          authorisationRequest.invitation.clientId
        ))

    case ConfirmClientPersonalVat(authorisationRequest, _) =>
      Ok(
        confirm_client(
          authorisationRequest.clientName,
          formWithErrors.or(ConfirmClientForm),
          backLinkFor(breadcrumbs).url,
          routes.AgentInvitationJourneyController.submitConfirmClient(),
          authorisationRequest.invitation.clientIdentifierType,
          authorisationRequest.invitation.clientId
        ))

    case ConfirmClientBusinessVat(authorisationRequest) =>
      Ok(
        confirm_client(
          authorisationRequest.clientName,
          formWithErrors.or(ConfirmClientForm),
          backLinkFor(breadcrumbs).url,
          routes.AgentInvitationJourneyController.submitConfirmClient(),
          authorisationRequest.invitation.clientIdentifierType,
          authorisationRequest.invitation.clientId
        ))

    case ReviewAuthorisationsPersonal(services, basket) =>
      Ok(
        review_authorisations(
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
        review_authorisations(
          ReviewAuthorisationsTrustPageConfig(
            basket,
            featureFlags,
            services,
            routes.AgentInvitationJourneyController.submitReviewAuthorisations()),
          formWithErrors.or(ReviewAuthorisationsForm),
          backLinkFor(breadcrumbs).url
        ))

    case DeleteAuthorisationRequestPersonal(authorisationRequest, _) =>
      Ok(
        delete(
          DeletePageConfig(authorisationRequest, routes.AgentInvitationJourneyController.submitDeleteAuthorisation()),
          DeleteAuthorisationForm))

    case DeleteAuthorisationRequestTrust(authorisationRequest, _) =>
      Ok(
        delete(
          DeletePageConfig(authorisationRequest, routes.AgentInvitationJourneyController.submitDeleteAuthorisation()),
          DeleteAuthorisationForm))

    case InvitationSentPersonal(invitationLink, continueUrl, agencyEmail) =>
      Ok(
        invitation_sent(
          InvitationSentPageConfig(
            invitationLink,
            None,
            continueUrl.isDefined,
            ClientType.fromEnum(personal),
            inferredExpiryDate,
            agencyEmail)))

    case InvitationSentBusiness(invitationLink, continueUrl, agencyEmail, service) =>
      Ok(
        invitation_sent(
          InvitationSentPageConfig(
            invitationLink,
            None,
            continueUrl.isDefined,
            ClientType.fromEnum(business),
            inferredExpiryDate,
            agencyEmail,
            service)))

    case KnownFactNotMatched(basket) =>
      Ok(
        not_matched(
          basket.nonEmpty,
          routes.AgentInvitationJourneyController.showIdentifyClient(),
          Some(routes.AgentInvitationJourneyController.showReviewAuthorisations())))

    case TrustNotFound(basket) =>
      Ok(
        not_matched(
          basket.nonEmpty,
          routes.AgentInvitationJourneyController.showIdentifyClient(),
          Some(routes.AgentInvitationJourneyController.showReviewAuthorisations())
        ))

    case CgtRefNotFound(cgtRef, basket) =>
      Ok(
        cgtRef_notFound(
          basket.nonEmpty,
          routes.AgentInvitationJourneyController.showIdentifyClient(),
          Some(routes.AgentInvitationJourneyController.showReviewAuthorisations()),
          cgtRef.value
        ))

    case CannotCreateRequest(basket) =>
      Ok(
        cannot_create_request(
          CannotCreateRequestConfig(basket.nonEmpty, fromFastTrack = false, backLinkFor(breadcrumbs).url)))

    case SomeAuthorisationsFailed(_, _, _, basket) =>
      Ok(invitation_creation_failed(SomeInvitationCreationFailedPageConfig(basket)))

    case AllAuthorisationsFailed(basket) =>
      Ok(invitation_creation_failed(AllInvitationCreationFailedPageConfig(basket)))

    case ActiveAuthorisationExists(_, service, basket) =>
      Ok(
        active_authorisation_exists(
          basket.nonEmpty,
          service,
          fromFastTrack = false,
          routes.AgentInvitationJourneyController.showReviewAuthorisations(),
          routes.AgentInvitationJourneyController.showClientType()
        ))

    case PendingInvitationExists(_, basket) =>
      Ok(
        pending_authorisation_exists(
          PendingAuthorisationExistsPageConfig(
            basket.nonEmpty,
            backLinkFor(breadcrumbs).url,
            fromFastTrack = false,
            routes.AgentInvitationJourneyController.showReviewAuthorisations(),
            routes.AgentInvitationJourneyController.showClientType()
          )))

    case ClientNotSignedUp(service, basket) =>
      Ok(not_signed_up(service, basket.nonEmpty))

    case AllAuthorisationsRemoved =>
      Ok(all_authorisations_removed(routes.AgentInvitationJourneyController.showClientType()))

    case _ => throw new Exception(s"Cannot render a page for unexpected state: $state")

  }
}

object AgentInvitationJourneyController {

  import uk.gov.hmrc.agentinvitationsfrontend.forms.CommonConfirmationForms._

  val ConfirmClientForm: Form[Confirmation] = confirmationForm("error.confirm-client.required")

  val ReviewAuthorisationsForm: Form[Confirmation] = confirmationForm("error.review-authorisation.required")

  val DeleteAuthorisationForm: Form[Confirmation] = confirmationForm("error.delete.radio")
}
