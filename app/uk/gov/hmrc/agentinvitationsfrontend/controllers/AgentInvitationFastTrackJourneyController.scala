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

import org.joda.time.LocalDate
import play.api.Configuration
import play.api.data.Forms.{mapping, optional, single, text}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.data.{Form, Mapping}
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.agentinvitationsfrontend.config.{AppConfig, CountryNamesLoader, ExternalUrls}
import uk.gov.hmrc.agentinvitationsfrontend.connectors.AgentClientAuthorisationConnector
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentInvitationJourneyController.ConfirmClientForm
import uk.gov.hmrc.agentinvitationsfrontend.forms._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.agents._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.track.check_details
import uk.gov.hmrc.agentmtdidentifiers.model.{CgtRef, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.play.fsm.JourneyController

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

@Singleton
class AgentInvitationFastTrackJourneyController @Inject()(
  invitationsService: InvitationsService,
  relationshipsService: RelationshipsService,
  acaConnector: AgentClientAuthorisationConnector,
  authActions: AuthActionsImpl,
  val redirectUrlActions: RedirectUrlActions,
  override val journeyService: AgentInvitationFastTrackJourneyService,
  notSignedUpPageConfig: NotSignedUpPageConfig,
  countryNamesLoader: CountryNamesLoader,
  clientTypeView: client_type,
  cgtRefNotFoundView: cgtRef_notFound,
  activeAuthExistsView: active_authorisation_exists,
  checkDetailsView: check_details,
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
  notSignedupView: not_signed_up,
  suspendedView: agent_suspended_fastrack,
  partialAuthExistsView: partial_auth_exists,
  clientNotRegisteredView: client_not_registered,
  alreadyCopiedAcrossView: already_copied_across_itsa)(
  implicit configuration: Configuration,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  ec: ExecutionContext,
  val cc: MessagesControllerComponents,
  val appConfig: AppConfig,
  override val actionBuilder: DefaultActionBuilder)
    extends FrontendController(cc) with JourneyController[HeaderCarrier] with I18nSupport {

  import AgentInvitationFastTrackJourneyController._
  import acaConnector._
  import authActions._
  import invitationsService._
  import journeyService.model.{State, Transitions}
  import redirectUrlActions._
  import relationshipsService.hasActiveRelationshipFor
  import uk.gov.hmrc.play.fsm.OptionalFormOps._

  override implicit def context(implicit rh: RequestHeader): HeaderCarrier = hc

  //TODO Add local date service to provide flexibility for testing
  def inferredExpiryDate = LocalDate.now().plusDays(appConfig.invitationExpirationDuration.toDays.toInt)

  private val countries = countryNamesLoader.load
  private val validCountryCodes = countries.keys.toSet
  private val urnEnabled = appConfig.featuresEnableTrustURNIdentifier

  val AsAgent: WithAuthorised[AuthorisedAgent] = { implicit request: Request[Any] =>
    withAuthorisedAsAgent(_)
  }

  /* Here we decide how to handle HTTP request and transition the state of the journey */

  val agentFastTrack: Action[AnyContent] =
    action { implicit request =>
      maybeRedirectUrlOrBadRequest(getRedirectUrl) { redirectUrl =>
        maybeRedirectUrlOrBadRequest(getErrorUrl) { errorUrl =>
          maybeRedirectUrlOrBadRequest(getRefererUrl) { refererUrl =>
            legacy.whenAuthorisedWithBootstrapAndForm(Transitions.prologue(errorUrl, refererUrl))(AsAgent)(agentFastTrackForm)(
              Transitions.start(featureFlags.agentSuspensionEnabled, getAgencySuspensionDetails)(redirectUrl))
          }
        }
      }
    }

  val showCheckDetails: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[CheckDetails]

  val submitCheckDetails: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(checkDetailsForm)
      .applyWithRequest(
        implicit request =>
          Transitions.checkedDetailsAllInformation(checkPostcodeMatches)(checkCitizenRecordMatches)(checkVatRegistrationDateMatches)(
            invitationsService.createInvitation)(invitationsService.createAgentLink)(getAgencyEmail)(hasPendingInvitationsFor)(
            hasActiveRelationshipFor)(hasPartialAuthorisationFor)(isAltItsa)(hasLegacyMapping)(appConfig))

  val progressToIdentifyClient: Action[AnyContent] =
    actions.whenAuthorisedWithRetrievals(AsAgent).apply(Transitions.checkedDetailsChangeInformation).redirect

  val showSuspended: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[SuspendedAgent]

  val identifyClientRedirect: Action[AnyContent] = Action(Redirect(routes.AgentInvitationFastTrackJourneyController.showIdentifyClient()))

  val showIdentifyClient: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[Identify]

  val submitIdentifyItsaClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(IdentifyItsaClientForm)
      .applyWithRequest(
        implicit request =>
          Transitions.identifiedClientItsa(checkPostcodeMatches)(checkCitizenRecordMatches)(checkVatRegistrationDateMatches)(
            invitationsService.createInvitation)(invitationsService.createAgentLink)(getAgencyEmail)(hasPendingInvitationsFor)(
            hasActiveRelationshipFor)(hasPartialAuthorisationFor)(isAltItsa)(hasLegacyMapping)(appConfig)
      )

  val submitIdentifyIrvClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(IdentifyIrvClientForm)
      .applyWithRequest(
        implicit request =>
          Transitions.identifiedClientIrv(checkPostcodeMatches)(checkCitizenRecordMatches)(checkVatRegistrationDateMatches)(
            invitationsService.createInvitation)(invitationsService.createAgentLink)(getAgencyEmail)(hasPendingInvitationsFor)(
            hasActiveRelationshipFor)(hasPartialAuthorisationFor)(isAltItsa)(hasLegacyMapping)(appConfig))

  val submitIdentifyVatClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(IdentifyVatClientForm)
      .applyWithRequest(
        implicit request =>
          Transitions.identifiedClientVat(checkPostcodeMatches)(checkCitizenRecordMatches)(checkVatRegistrationDateMatches)(
            invitationsService.createInvitation)(invitationsService.createAgentLink)(getAgencyEmail)(hasPendingInvitationsFor)(
            hasActiveRelationshipFor)(hasPartialAuthorisationFor)(isAltItsa)(hasLegacyMapping)(appConfig))

  val submitIdentifyTrustClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(TrustClientForm.form(urnEnabled))
      .applyWithRequest(implicit request => Transitions.showConfirmTrustClient(taxId => acaConnector.getTrustName(taxId.value)))

  val submitIdentifyCgtClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(CgtClientForm.form())
      .applyWithRequest(implicit request => Transitions.identifyCgtClient(cgtRef => acaConnector.getCgtSubscription(cgtRef)))

  val progressToKnownFact: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .applyWithRequest(implicit request => Transitions.checkedDetailsNoKnownFact(acaConnector.getCgtSubscription(_))) // TODO: (redirect)

  val showConfirmTrustClient: Action[AnyContent] = actions.whenAuthorised(AsAgent).showCurrentState

  val submitConfirmTrustClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(checkDetailsForm)
      .applyWithRequest(implicit request =>
        Transitions.submitConfirmTrustClient(invitationsService.createInvitation)(invitationsService.createAgentLink)(getAgencyEmail)(
          hasPendingInvitationsFor)(hasActiveRelationshipFor)(hasPartialAuthorisationFor)(isAltItsa)(hasLegacyMapping)(appConfig))

  val knownFactRedirect: Action[AnyContent] = Action(Redirect(routes.AgentInvitationFastTrackJourneyController.showKnownFact()))

  val showKnownFact: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[MissingDetail]

  val submitKnownFactItsa: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(agentFastTrackPostcodeForm)
      .applyWithRequest(
        implicit request =>
          Transitions.moreDetailsItsa(checkPostcodeMatches)(checkCitizenRecordMatches)(checkVatRegistrationDateMatches)(
            invitationsService.createInvitation)(invitationsService.createAgentLink)(getAgencyEmail)(hasPendingInvitationsFor)(
            hasActiveRelationshipFor)(hasPartialAuthorisationFor)(isAltItsa)(hasLegacyMapping)(appConfig))

  val submitKnownFactIrv: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(agentFastTrackDateOfBirthForm)
      .applyWithRequest(
        implicit request =>
          Transitions.moreDetailsIrv(checkPostcodeMatches)(checkCitizenRecordMatches)(checkVatRegistrationDateMatches)(
            invitationsService.createInvitation)(invitationsService.createAgentLink)(getAgencyEmail)(hasPendingInvitationsFor)(hasLegacyMapping)(
            hasActiveRelationshipFor)(hasPartialAuthorisationFor)(isAltItsa)(appConfig))

  val submitKnownFactVat: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(agentFastTrackVatRegDateForm)
      .applyWithRequest(
        implicit request =>
          Transitions.moreDetailsVat(checkPostcodeMatches)(checkCitizenRecordMatches)(checkVatRegistrationDateMatches)(
            invitationsService.createInvitation)(invitationsService.createAgentLink)(getAgencyEmail)(hasPendingInvitationsFor)(
            hasActiveRelationshipFor)(hasPartialAuthorisationFor)(isAltItsa)(hasLegacyMapping)(appConfig))

  val progressToClientType
    : Action[AnyContent] = actions.whenAuthorisedWithRetrievals(AsAgent)(Transitions.checkedDetailsNoClientType) //.apply(helpers.redirect)

  val showClientType: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ClientTypeState]

  val submitClientType: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ClientTypeForm.fastTrackForm)
      .applyWithRequest(
        implicit request =>
          Transitions.selectedClientType(checkPostcodeMatches)(checkCitizenRecordMatches)(checkVatRegistrationDateMatches)(
            invitationsService.createInvitation)(invitationsService.createAgentLink)(getAgencyEmail)(hasPendingInvitationsFor)(
            hasActiveRelationshipFor)(hasPartialAuthorisationFor)(isAltItsa)(hasLegacyMapping)(acaConnector.getCgtSubscription(_))(appConfig))

  val submitClientTypeCgt: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ClientTypeForm.cgtClientTypeForm)
      .applyWithRequest(
        implicit request =>
          Transitions.selectedClientType(checkPostcodeMatches)(checkCitizenRecordMatches)(checkVatRegistrationDateMatches)(
            invitationsService.createInvitation)(invitationsService.createAgentLink)(getAgencyEmail)(hasPendingInvitationsFor)(
            hasActiveRelationshipFor)(hasPartialAuthorisationFor)(isAltItsa)(hasLegacyMapping)(acaConnector.getCgtSubscription(_))(appConfig))

  val showInvitationSent: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[InvitationSent]

  val showNotMatched: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[NotMatched]

  val redirectTryAgainNotMatchedKnownFact: Action[AnyContent] =
    actions.whenAuthorisedWithRetrievals(AsAgent).apply(Transitions.tryAgainNotMatchedKnownFact) // TODO: redirect

  def showConfirmCgtPostcode: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmPostcodeCgt]

  def submitConfirmCgtPostcode: Action[AnyContent] =
    actions.whenAuthorisedWithRetrievals(AsAgent).bindForm(PostcodeForm.form).apply(Transitions.confirmPostcodeCgt)

  def showConfirmCgtCountryCode: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmCountryCodeCgt]

  def submitConfirmCgtCountryCode: Action[AnyContent] =
    actions.whenAuthorisedWithRetrievals(AsAgent).bindForm(CountrycodeForm.form(validCountryCodes))(Transitions.confirmCountryCodeCgt)

  def showConfirmClientCgt: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmClientCgt]

  def submitConfirmCgtClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(checkDetailsForm)
      .applyWithRequest(implicit request =>
        Transitions.submitConfirmClientCgt(invitationsService.createInvitation)(invitationsService.createAgentLink)(getAgencyEmail)(
          hasPendingInvitationsFor)(hasActiveRelationshipFor)(hasPartialAuthorisationFor)(isAltItsa)(hasLegacyMapping)(appConfig))

  val showClientNotSignedUp: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ClientNotSignedUp]
  val showPendingAuthorisationExists: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[PendingInvitationExists]
  val showActiveAuthorisationExists: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[AuthExists]

  val showClientNotRegistered: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ClientNotRegistered]

  val showAlreadyCopiedAcrossItsa: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[AlreadyCopiedAcrossItsa.type]

  /* Here we map states to the GET endpoints for redirecting and back linking */
  override def getCallFor(state: State)(implicit request: Request[_]): Call = state match {
    case Prologue(failureUrlOpt, refererUrlOpt) =>
      failureUrlOpt match {
        case Some(failureUrl) =>
          Call("GET", failureUrl + s"?issue=${agentFastTrackForm.bindFromRequest.errorsAsJson.as[FastTrackErrors].formErrorsMessages}")
        case None => routes.AgentInvitationFastTrackJourneyController.showClientType()
      }
    case _: SelectClientTypeVat             => routes.AgentInvitationFastTrackJourneyController.showClientType()
    case _: SelectClientTypeCgt             => routes.AgentInvitationFastTrackJourneyController.showClientType()
    case _: NoPostcode                      => routes.AgentInvitationFastTrackJourneyController.showKnownFact()
    case _: NoDob                           => routes.AgentInvitationFastTrackJourneyController.showKnownFact()
    case _: NoVatRegDate                    => routes.AgentInvitationFastTrackJourneyController.showKnownFact()
    case _: CheckDetailsCompleteItsa        => routes.AgentInvitationFastTrackJourneyController.showCheckDetails()
    case _: CheckDetailsCompleteIrv         => routes.AgentInvitationFastTrackJourneyController.showCheckDetails()
    case _: CheckDetailsCompletePersonalVat => routes.AgentInvitationFastTrackJourneyController.showCheckDetails()
    case _: CheckDetailsCompleteBusinessVat => routes.AgentInvitationFastTrackJourneyController.showCheckDetails()
    case _: CheckDetailsCompleteTrust       => routes.AgentInvitationFastTrackJourneyController.showCheckDetails()
    case _: CheckDetailsCompleteCgt         => routes.AgentInvitationFastTrackJourneyController.showCheckDetails()
    case _: CheckDetailsNoPostcode          => routes.AgentInvitationFastTrackJourneyController.showCheckDetails()
    case _: CheckDetailsNoDob               => routes.AgentInvitationFastTrackJourneyController.showCheckDetails()
    case _: CheckDetailsNoVatRegDate        => routes.AgentInvitationFastTrackJourneyController.showCheckDetails()
    case _: CheckDetailsNoClientTypeVat     => routes.AgentInvitationFastTrackJourneyController.showCheckDetails()
    case _: IdentifyPersonalClient          => routes.AgentInvitationFastTrackJourneyController.showIdentifyClient()
    case _: IdentifyBusinessClient          => routes.AgentInvitationFastTrackJourneyController.showIdentifyClient()
    case _: IdentifyTrustClient             => routes.AgentInvitationFastTrackJourneyController.showIdentifyClient()
    case _: IdentifyCgtClient               => routes.AgentInvitationFastTrackJourneyController.showIdentifyClient()
    case _: IdentifyNoClientTypeClient      => routes.AgentInvitationFastTrackJourneyController.showIdentifyClient()
    case _: ConfirmClientTrust              => routes.AgentInvitationFastTrackJourneyController.showConfirmTrustClient()
    case _: ConfirmPostcodeCgt              => routes.AgentInvitationFastTrackJourneyController.showConfirmCgtPostcode()
    case _: ConfirmCountryCodeCgt           => routes.AgentInvitationFastTrackJourneyController.showConfirmCgtCountryCode()
    case _: ConfirmClientCgt                => routes.AgentInvitationFastTrackJourneyController.showConfirmClientCgt()
    case _: InvitationSentPersonal          => routes.AgentInvitationFastTrackJourneyController.showInvitationSent()
    case _: InvitationSentBusiness          => routes.AgentInvitationFastTrackJourneyController.showInvitationSent()
    case _: KnownFactNotMatched             => routes.AgentInvitationFastTrackJourneyController.showNotMatched()
    case TryAgainWithoutFastTrack           => routes.AgentInvitationJourneyController.agentsRoot()
    case _: ClientNotSignedUp               => routes.AgentInvitationFastTrackJourneyController.showClientNotSignedUp()
    case _: PendingInvitationExists =>
      routes.AgentInvitationFastTrackJourneyController.showPendingAuthorisationExists()
    case _: ActiveAuthorisationExists =>
      routes.AgentInvitationFastTrackJourneyController.showActiveAuthorisationExists()
    case _: PartialAuthorisationExists =>
      routes.AgentInvitationFastTrackJourneyController.showActiveAuthorisationExists()
    case _: ClientNotRegistered  => routes.AgentInvitationFastTrackJourneyController.showClientNotRegistered()
    case _: TrustNotFound        => routes.AgentInvitationFastTrackJourneyController.showNotMatched()
    case _: CgtRefNotFound       => routes.AgentInvitationFastTrackJourneyController.showNotMatched()
    case _: SuspendedAgent       => routes.AgentInvitationFastTrackJourneyController.showSuspended()
    case AlreadyCopiedAcrossItsa => routes.AgentInvitationFastTrackJourneyController.showAlreadyCopiedAcrossItsa()
    case _                       => throw new Exception(s"Link not found for $state")
  }

  private def gotoCheckDetailsWithRequest(fastTrackRequest: AgentFastTrackRequest, breadcrumbs: List[State])(implicit request: Request[_]): Result = {
    val backLinkOpt: Option[String] =
      breadcrumbs.headOption match {
        case Some(Prologue(_, refererUrl)) if refererUrl.isDefined => refererUrl
        case _                                                     => None
      }

    Ok(
      checkDetailsView(
        checkDetailsForm,
        CheckDetailsPageConfig(
          fastTrackRequest,
          featureFlags,
          routes.AgentInvitationFastTrackJourneyController.progressToClientType(),
          routes.AgentInvitationFastTrackJourneyController.progressToKnownFact(),
          routes.AgentInvitationFastTrackJourneyController.progressToIdentifyClient(),
          routes.AgentInvitationFastTrackJourneyController.submitCheckDetails(),
          backLinkOpt
        )
      ))
  }

  /* Here we decide what to render after state transition */
  override def renderState(state: State, breadcrumbs: List[State], formWithErrors: Option[Form[_]])(implicit request: Request[_]): Result =
    state match {

      case s: Prologue => Redirect(getCallFor(s))

      case CheckDetailsCompleteItsa(_, ftr, _) =>
        gotoCheckDetailsWithRequest(ftr, breadcrumbs)

      case CheckDetailsCompleteIrv(_, ftr, _) =>
        gotoCheckDetailsWithRequest(ftr, breadcrumbs)

      case CheckDetailsCompletePersonalVat(_, ftr, _) =>
        gotoCheckDetailsWithRequest(ftr, breadcrumbs)

      case CheckDetailsCompleteBusinessVat(_, ftr, _) =>
        gotoCheckDetailsWithRequest(ftr, breadcrumbs)

      case CheckDetailsCompleteTrust(_, ftr, _) =>
        gotoCheckDetailsWithRequest(ftr, breadcrumbs)

      case CheckDetailsCompleteCgt(_, ftr, _) =>
        gotoCheckDetailsWithRequest(ftr, breadcrumbs)

      case CheckDetailsNoPostcode(_, ftr, _) =>
        gotoCheckDetailsWithRequest(ftr, breadcrumbs)

      case CheckDetailsNoDob(_, ftr, _) =>
        gotoCheckDetailsWithRequest(ftr, breadcrumbs)

      case CheckDetailsNoVatRegDate(_, ftr, _) =>
        gotoCheckDetailsWithRequest(ftr, breadcrumbs)

      case CheckDetailsNoClientTypeVat(_, ftr, _) =>
        gotoCheckDetailsWithRequest(ftr, breadcrumbs)

      case NoPostcode(_, ftr, _) =>
        Ok(
          knownFactsView(
            formWithErrors.or(getKnownFactFormForService(ftr.service)),
            KnownFactPageConfig(
              ftr.service,
              Services.determineServiceMessageKeyFromService(ftr.service),
              getSubmitKFFor(ftr.service),
              backLinkFor(breadcrumbs).url
            )
          ))

      case NoDob(_, ftr, _) =>
        Ok(
          knownFactsView(
            formWithErrors.or(getKnownFactFormForService(ftr.service)),
            KnownFactPageConfig(
              ftr.service,
              Services.determineServiceMessageKeyFromService(ftr.service),
              getSubmitKFFor(ftr.service),
              backLinkFor(breadcrumbs).url
            )
          ))

      case NoVatRegDate(_, ftr, _) =>
        Ok(
          knownFactsView(
            formWithErrors.or(getKnownFactFormForService(ftr.service)),
            KnownFactPageConfig(
              ftr.service,
              Services.determineServiceMessageKeyFromService(ftr.service),
              getSubmitKFFor(ftr.service),
              backLinkFor(breadcrumbs).url
            )
          ))

      case SelectClientTypeVat(_, _, _, _) =>
        Ok(
          clientTypeView(
            formWithErrors.or(ClientTypeForm.fastTrackForm),
            ClientTypePageConfig(
              backLinkFor(breadcrumbs).url,
              routes.AgentInvitationFastTrackJourneyController.submitClientType(),
              featureFlags.showHmrcTrust,
              isForVat = true
            )
          ))

      case SelectClientTypeCgt(_, _, _, _) =>
        Ok(
          clientTypeView(
            formWithErrors.or(ClientTypeForm.cgtClientTypeForm),
            ClientTypePageConfig(
              backLinkFor(breadcrumbs).url,
              routes.AgentInvitationFastTrackJourneyController.submitClientTypeCgt(),
              featureFlags.showHmrcTrust,
              isForCgt = true
            )
          ))

      case ConfirmClientTrust(_, ftr, _, trustName) =>
        Ok(
          confirmClientView(
            trustName,
            formWithErrors.or(ConfirmClientForm),
            backLinkFor(breadcrumbs).url,
            routes.AgentInvitationFastTrackJourneyController.submitConfirmTrustClient(),
            ftr.clientIdentifierType,
            ftr.clientIdentifier
          ))

      case IdentifyPersonalClient(_, ftr, _) if ftr.service == HMRCMTDIT =>
        Ok(
          identifyClientItsaView(
            formWithErrors.or(IdentifyItsaClientForm),
            routes.AgentInvitationFastTrackJourneyController.submitIdentifyItsaClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyPersonalClient(_, ftr, _) if ftr.service == HMRCMTDVAT =>
        Ok(
          identifyClientVatView(
            formWithErrors.or(IdentifyVatClientForm),
            routes.AgentInvitationFastTrackJourneyController.submitIdentifyVatClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyPersonalClient(_, ftr, _) if ftr.service == HMRCPIR =>
        Ok(
          identifyClientIrvView(
            formWithErrors.or(IdentifyIrvClientForm),
            routes.AgentInvitationFastTrackJourneyController.submitIdentifyIrvClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyBusinessClient(_, _, _) =>
        Ok(
          identifyClientVatView(
            formWithErrors.or(IdentifyVatClientForm),
            routes.AgentInvitationFastTrackJourneyController.submitIdentifyVatClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyTrustClient(_, _, _) =>
        Ok(
          identifyClientTrustView(
            formWithErrors.or(TrustClientForm.form(urnEnabled)),
            routes.AgentInvitationFastTrackJourneyController.submitIdentifyTrustClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyCgtClient(_, ftr, _) =>
        Ok(
          identifyClientCgtView(
            formWithErrors.or(CgtClientForm.form()),
            routes.AgentInvitationFastTrackJourneyController.submitIdentifyCgtClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyNoClientTypeClient(_, _, _) =>
        Ok(
          identifyClientVatView(
            formWithErrors.or(IdentifyVatClientForm),
            routes.AgentInvitationFastTrackJourneyController.submitIdentifyVatClient(),
            backLinkFor(breadcrumbs).url
          )
        )

      case ConfirmClientCgt(_, ftr, _, name) =>
        Ok(
          confirmClientView(
            name,
            formWithErrors.or(ConfirmClientForm),
            backLinkFor(breadcrumbs).url,
            routes.AgentInvitationFastTrackJourneyController.submitConfirmCgtClient(),
            ftr.clientIdentifierType,
            ftr.clientIdentifier
          ))

      case ConfirmPostcodeCgt(_, ftr, _, _, _) =>
        Ok(
          confirmPostcodeCgtView(
            ftr.clientType.getOrElse(personal),
            formWithErrors.or(PostcodeForm.form),
            backLinkFor(breadcrumbs).url,
            fromFastTrack = true,
            isDeAuth = false))

      case ConfirmCountryCodeCgt(_, ftr, _, _, _) =>
        Ok(
          confirmCountryCodeCgtView(
            ftr.clientType.getOrElse(personal),
            countries,
            formWithErrors.or(CountrycodeForm.form(validCountryCodes)),
            backLinkFor(breadcrumbs).url,
            fromFastTrack = true,
            isDeAuth = false
          ))

      case InvitationSentPersonal(invitationLink, continueUrl, agencyEmail, service, isAltItsa) =>
        Ok(
          invitationSentView(
            InvitationSentPageConfig(
              invitationLink,
              continueUrl,
              continueUrl.isDefined,
              ClientType.fromEnum(personal),
              inferredExpiryDate,
              agencyEmail,
              Set(service),
              isAltItsa
            )))

      case InvitationSentBusiness(invitationLink, continueUrl, agencyEmail, service) =>
        Ok(
          invitationSentView(
            InvitationSentPageConfig(
              invitationLink,
              continueUrl,
              continueUrl.isDefined,
              ClientType.fromEnum(business),
              inferredExpiryDate,
              agencyEmail,
              Set(service),
              isAltItsa = false,
              service,
            )))

      case KnownFactNotMatched(_, _, _) =>
        Ok(
          notMatchedView(
            hasJourneyCache = false,
            tryAgainCall = routes.AgentInvitationFastTrackJourneyController.redirectTryAgainNotMatchedKnownFact(),
            reviewAuthsCallOpt = Some(routes.AgentInvitationJourneyController.showReviewAuthorisations())
          ))

      case TrustNotFound(_, _, _) =>
        Ok(
          notMatchedView(
            hasJourneyCache = false,
            tryAgainCall = routes.AgentInvitationFastTrackJourneyController.redirectTryAgainNotMatchedKnownFact(),
            reviewAuthsCallOpt = Some(routes.AgentInvitationJourneyController.showReviewAuthorisations())
          ))

      case CgtRefNotFound(cgtRef) =>
        Ok(
          cgtRefNotFoundView(
            false,
            routes.AgentInvitationFastTrackJourneyController.showIdentifyClient(),
            Some(routes.AgentInvitationJourneyController.showReviewAuthorisations()),
            cgtRef.value
          ))

      case ActiveAuthorisationExists(agentFastTrackRequest, _) =>
        Ok(
          activeAuthExistsView(
            authRequestsExist = false,
            agentFastTrackRequest.service,
            agentFastTrackRequest.clientType.getOrElse(personal),
            fromFastTrack = true,
            routes.AgentInvitationJourneyController.showReviewAuthorisations(),
            routes.AgentInvitationFastTrackJourneyController.showClientType()
          ))

      case PendingInvitationExists(_, _) =>
        Ok(
          pendingAuthExistsView(
            PendingAuthorisationExistsPageConfig(
              authRequestsExist = false,
              backLinkFor(breadcrumbs).url,
              fromFastTrack = true,
              routes.AgentInvitationJourneyController.showReviewAuthorisations(),
              routes.AgentInvitationFastTrackJourneyController.showClientType()
            )))

      case PartialAuthorisationExists(_, _) =>
        Ok(
          partialAuthExistsView(
            authRequestsExist = false,
            fromFastTrack = true,
            routes.AgentInvitationJourneyController.showReviewAuthorisations(),
            routes.AgentInvitationFastTrackJourneyController.showClientType()
          )
        )

      case ClientNotSignedUp(fastTrackRequest, _) =>
        Ok(
          notSignedupView(
            fastTrackRequest.service,
            hasRequests = false,
            isDeAuthJourney = false,
            htmlPartial = notSignedUpPageConfig.render(fastTrackRequest.service)))

      case ClientNotRegistered(_, _) =>
        Ok(
          clientNotRegisteredView(
            authRequestsExist = false,
            fromFastTrack = true,
            routes.AgentInvitationJourneyController.showReviewAuthorisations(),
            routes.AgentInvitationFastTrackJourneyController.showClientType()
          )
        )

      case SuspendedAgent(service, continueUrl) => Ok(suspendedView(service, continueUrl))

      case AlreadyCopiedAcrossItsa => Ok(alreadyCopiedAcrossView())

      case _ => throw new Exception(s"Cannot render a page for unexpected state: $state, add your state as a match case in #renderState")
    }
}

object AgentInvitationFastTrackJourneyController {

  val validateFastTrackForm: Constraint[AgentFastTrackRequest] =
    Constraint[AgentFastTrackRequest] { formData: AgentFastTrackRequest =>
      formData match {
        case AgentFastTrackRequest(Some(ClientType.personal) | None, HMRCMTDIT, "ni", clientId, _) if Nino.isValid(clientId) =>
          Valid
        case AgentFastTrackRequest(Some(ClientType.personal) | None, HMRCPIR, "ni", clientId, _) if Nino.isValid(clientId) =>
          Valid
        case AgentFastTrackRequest(_, HMRCMTDVAT, "vrn", clientId, _) if Vrn.isValid(clientId)             => Valid
        case AgentFastTrackRequest(_, TAXABLETRUST, "utr", clientId, _) if clientId.matches(utrPattern)    => Valid
        case AgentFastTrackRequest(_, NONTAXABLETRUST, "urn", clientId, _) if clientId.matches(urnPattern) => Valid
        case AgentFastTrackRequest(_, HMRCCGTPD, "CGTPDRef", clientId, _) if CgtRef.isValid(clientId)      => Valid
        case _                                                                                             => Invalid(ValidationError("INVALID_SUBMISSION"))
      }
    }

  val agentFastTrackForm: Form[AgentFastTrackRequest] =
    Form(
      mapping(
        "clientType" -> optional(
          lowerCaseText
            .verifying("UNSUPPORTED_CLIENT_TYPE", Set("personal", "business").contains _)
            .transform(ClientType.toEnum, ClientType.fromEnum)),
        "service" -> text.verifying("UNSUPPORTED_SERVICE", service => supportedServices.contains(service)),
        "clientIdentifierType" -> text
          .verifying("UNSUPPORTED_CLIENT_ID_TYPE", clientType => supportedClientIdentifierTypes.contains(clientType)),
        "clientIdentifier" -> uppercaseNormalizedText.verifying(validateClientId),
        "knownFact"        -> optional(text)
      )({ (clientType, service, clientIdType, clientId, knownFact) =>
        AgentFastTrackRequest(ClientType.clientTypeFor(clientType, service), service, clientIdType, clientId, knownFact)
      })({ request =>
        Some((request.clientType, request.service, request.clientIdentifierType, request.clientIdentifier, request.knownFact))
      }).verifying(validateFastTrackForm))

  def confirmationForm(errorMessage: String): Form[Confirmation] =
    Form(
      mapping(
        "accepted" -> optional(normalizedText)
          .transform[String](_.getOrElse(""), s => Some(s))
          .verifying(confirmationChoice(errorMessage))
      )(choice => Confirmation(choice.toBoolean))(confirmation => Some(confirmation.choice.toString)))

  val checkDetailsForm = confirmationForm("error.confirmDetails.invalid")

  def IdentifyItsaClientForm: Form[ItsaClient] = Form(
    mapping(
      "clientIdentifier" -> uppercaseNormalizedText.verifying(validNino),
      "postcode"         -> postcodeMapping
    )(ItsaClient.apply)(ItsaClient.unapply)
  )

  def IdentifyVatClientForm: Form[VatClient] = Form(
    mapping(
      "clientIdentifier" -> normalizedText.verifying(validVrn),
      "registrationDate" -> DateFieldHelper.dateFieldsMapping("vat-registration")
    )(VatClient.apply)(VatClient.unapply)
  )

  def IdentifyTrustClientForm(urnEnabled: Boolean): Form[TrustClient] =
    Form(
      mapping(
        "taxId" -> normalizedText.verifying(validTrustTaxId(urnEnabled))
      )(x => TrustClient.apply(x, urnEnabled))(x => Some(x.taxId.value)))

  def IdentifyIrvClientForm: Form[IrvClient] = Form(
    mapping(
      "clientIdentifier" -> uppercaseNormalizedText.verifying(validNino),
      "dob"              -> DateFieldHelper.dateFieldsMapping("irv-date-of-birth")
    )(IrvClient.apply)(IrvClient.unapply)
  )

  def knownFactsForm(knownFactsMapping: Mapping[String]) =
    Form(single("knownFact" -> knownFactsMapping))

  def agentFastTrackPostcodeForm: Form[String] =
    knownFactsForm(postcodeMapping)

  def agentFastTrackDateOfBirthForm: Form[String] =
    knownFactsForm(DateFieldHelper.dateFieldsMapping("irv-date-of-birth"))

  def agentFastTrackVatRegDateForm: Form[String] =
    knownFactsForm(DateFieldHelper.dateFieldsMapping("vat-registration"))

  private def getKnownFactFormForService(service: String) =
    service match {
      case HMRCMTDIT  => agentFastTrackPostcodeForm
      case HMRCPIR    => agentFastTrackDateOfBirthForm
      case HMRCMTDVAT => agentFastTrackVatRegDateForm
      case p          => throw new Exception(s"invalid service in the cache during fast track journey: $p")
    }

  def getSubmitKFFor(service: String) =
    service match {
      case HMRCMTDIT  => routes.AgentInvitationFastTrackJourneyController.submitKnownFactItsa()
      case HMRCPIR    => routes.AgentInvitationFastTrackJourneyController.submitKnownFactIrv()
      case HMRCMTDVAT => routes.AgentInvitationFastTrackJourneyController.submitKnownFactVat()
    }

}
