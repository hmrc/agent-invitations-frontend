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
import play.api.data.Forms.{mapping, optional, single, text}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.data.{Form, Mapping}
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.agentinvitationsfrontend.config.{AppConfig, CountryNamesLoader, ExternalUrls}
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{AcrfConnector, AgentClientAuthorisationConnector}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentInvitationJourneyController.{ConfirmClientForm, LegacyAuthorisationForm}
import uk.gov.hmrc.agentinvitationsfrontend.forms._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.Personal
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.support.CallOps.localFriendlyUrl
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.agents._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.track.check_details
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.hmrcfrontend.config.ContactFrontendConfig
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.play.fsm.JourneyController

import java.time.LocalDate
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AgentInvitationFastTrackJourneyController @Inject() (
  invitationsService: InvitationsService,
  relationshipsService: RelationshipsService,
  knownFactService: KnownFactService,
  acaConnector: AgentClientAuthorisationConnector,
  authActions: AuthActions,
  val redirectUrlActions: RedirectUrlActions,
  override val journeyService: AgentInvitationFastTrackJourneyService,
  notSignedUpPageConfig: NotSignedUpPageConfig,
  countryNamesLoader: CountryNamesLoader,
  clientTypeView: client_type,
  cgtRefNotFoundView: cgtRef_notFound,
  pptRefNotFoundView: pptRef_notFound,
  activeAuthExistsView: active_authorisation_exists,
  checkDetailsView: check_details,
  knownFactsView: known_fact,
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
  confirmRegDatePptView: confirm_reg_date_ppt,
  notMatchedView: not_matched,
  pendingAuthExistsView: pending_authorisation_exists,
  invitationSentView: invitation_sent,
  notSignedupView: not_signed_up,
  suspendedView: agent_suspended_fastrack,
  partialAuthExistsView: partial_auth_exists,
  clientNotRegisteredView: client_not_registered,
  alreadyCopiedAcrossView: already_copied_across_itsa,
  legacyAuthorisationDetectedView: legacy_authorisation_detected,
  clientInsolventView: client_insolvent,
  cannotCreateRequestView: cannot_create_request,
  acrfConnector: AcrfConnector
)(implicit
  configuration: Configuration,
  implicit val contactFrontendConfig: ContactFrontendConfig,
  val externalUrls: ExternalUrls,
  ec: ExecutionContext,
  val cc: MessagesControllerComponents,
  val appConfig: AppConfig,
  override val actionBuilder: DefaultActionBuilder
) extends FrontendController(cc) with JourneyController[HeaderCarrier] with I18nSupport {

  import AgentInvitationFastTrackJourneyController._
  import acaConnector._
  import authActions._
  import journeyService.model.{State, Transitions}
  import redirectUrlActions._
  import uk.gov.hmrc.play.fsm.OptionalFormOps._

  override implicit def context(implicit rh: RequestHeader): HeaderCarrier = hc

  // TODO Add local date service to provide flexibility for testing
  def inferredExpiryDate = LocalDate.now().plusDays(appConfig.invitationExpirationDuration.toDays.toInt)

  private val countries = countryNamesLoader.load
  private val validCountryCodes = countries.keys.toSet

  val AsAgent: WithAuthorised[AuthorisedAgent] = { implicit request: Request[Any] =>
    withAuthorisedAsAgent(_)
  }

  /* Here we decide how to handle HTTP request and transition the state of the journey */

  def transitions()(implicit ec: ExecutionContext, request: RequestHeader) = Transitions(
    getSuspensionDetails = getAgencySuspensionDetails,
    hasPendingInvitationsFor = invitationsService.hasPendingInvitationsFor,
    hasActiveRelationshipFor = relationshipsService.hasActiveRelationshipFor,
    hasPartialAuthorisationFor = invitationsService.hasPartialAuthorisationFor,
    legacySaRelationshipStatusFor = invitationsService.legacySaRelationshipStatusFor,
    getClientName = invitationsService.getClientNameByService,
    getAgentLink = invitationsService.createAgentLink,
    getAgencyEmail = getAgencyEmail,
    getCgtSubscription = acaConnector.getCgtSubscription,
    getPptSubscription = acaConnector.getPptSubscription,
    getCbcSubscription = acaConnector.getCbcSubscription,
    createInvitation = invitationsService.createInvitation,
    isAltItsa = invitationsService.isAltItsa,
    checkKnownFact = knownFactService.checkKnownFact
  )

  val agentFastTrack: Action[AnyContent] =
    action { implicit request =>
      if (appConfig.enableAcrfRedirects) {
        val continueUrl = request.getQueryString("continue")
        val errorUrl = request.getQueryString("error")
        val formData = agentFastTrackForm.bindFromRequest().data.mapValues(Seq(_))
        acrfConnector.fastTrack(formData, continueUrl, errorUrl, hc).map(url => Redirect(url))
      } else {
        maybeRedirectUrlOrBadRequest(getRedirectUrl) { redirectUrl =>
          maybeRedirectUrlOrBadRequest(getErrorUrl) { errorUrl =>
            maybeRedirectUrlOrBadRequest(getRefererUrl) { refererUrl =>
              legacy.whenAuthorisedWithBootstrapAndForm(transitions.prologue(errorUrl, refererUrl))(AsAgent)(agentFastTrackForm) {
                transitions.start(redirectUrl)
              }
            }
          }
        }
      }
    }

  def showCheckDetails: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[CheckDetails].orRollback

  val submitCheckDetails: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(checkDetailsForm)
      .applyWithRequest(implicit request => transitions.checkedDetailsAllInformation(appConfig))

  def progressToIdentifyClient: Action[AnyContent] =
    actions.whenAuthorisedWithRetrievals(AsAgent).applyWithRequest(implicit request => transitions.checkedDetailsChangeInformation).redirect

  val showSuspended: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[SuspendedAgent]

  val showIdentifyClient: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[IdentifyClient].orRollback

  val submitIdentifyItsaClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ItsaClientForm.form)
      .applyWithRequest(implicit request => transitions.identifiedClientItsa(appConfig))

  val submitIdentifyIrvClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(IrvClientForm.form)
      .applyWithRequest(implicit request => transitions.identifiedClientIrv(appConfig))

  val submitIdentifyVatClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(VatClientForm.form)
      .applyWithRequest(implicit request => transitions.identifiedClientVat(appConfig))

  val submitIdentifyTrustClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(TrustClientForm.form)
      .applyWithRequest(implicit request => transitions.showConfirmTrustClient)

  val submitIdentifyCgtClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(CgtClientForm.form())
      .applyWithRequest(implicit request => transitions.identifyCgtClient)

  def submitIdentifyPptClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(PptClientForm.form)
      .applyWithRequest(implicit request => transitions.identifyPptClient(acaConnector.getPptCustomerName(_)))

  def submitIdentifyCbcClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(CbcClientForm.form)
      .applyWithRequest(implicit request => transitions.identifyCbcClient(appConfig))

  def submitIdentifyPillar2Client: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(Pillar2ClientForm.form)
      .applyWithRequest(implicit request => transitions.identifyPillar2Client(appConfig))

  val progressToKnownFact: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .applyWithRequest(implicit request => transitions.checkedDetailsNoKnownFact) // TODO: (redirect)

  val showConfirmTrustClient: Action[AnyContent] = actions.whenAuthorised(AsAgent).showCurrentState

  val submitConfirmTrustClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(checkDetailsForm)
      .applyWithRequest(implicit request => transitions.submitConfirmTrustClient(appConfig))

  val showKnownFact: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[MissingDetail].orRollback

  val submitKnownFactItsa: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(agentFastTrackPostcodeForm)
      .applyWithRequest(implicit request => transitions.moreDetailsSupplied(appConfig))

  val submitKnownFactIrv: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(agentFastTrackDateOfBirthForm)
      .applyWithRequest(implicit request => transitions.moreDetailsSupplied(appConfig))

  val submitKnownFactVat: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(agentFastTrackVatRegDateForm)
      .applyWithRequest(implicit request => transitions.moreDetailsSupplied(appConfig))

  val submitKnownFactPpt: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(agentFastTrackPptRegDateForm)
      .applyWithRequest(implicit request => transitions.moreDetailsSupplied(appConfig))

  val submitKnownFactCbc: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(agentFastTrackCbcEmailForm)
      .applyWithRequest(implicit request => transitions.moreDetailsSupplied(appConfig))

  val submitKnownFactPillar2: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(agentFastTrackPillar2RegDateForm)
      .applyWithRequest(implicit request => transitions.moreDetailsSupplied(appConfig))

  val progressToClientType: Action[AnyContent] =
    actions.whenAuthorisedWithRetrievals(AsAgent).applyWithRequest(implicit request => transitions.checkedDetailsNoClientType)

  def showClientType: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[SelectClientType].orRollback

  val submitClientType: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ClientTypeForm.fastTrackForm(ClientType.clientTypes.toSet))
      .applyWithRequest(implicit request => transitions.selectedClientType(appConfig))

  def showInvitationSent: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[InvitationSent]

  def showNotMatched: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ClientNotFound].orRollback

  def redirectTryAgainNotMatchedKnownFact: Action[AnyContent] =
    actions.whenAuthorisedWithRetrievals(AsAgent).applyWithRequest(implicit request => transitions.tryAgainNotMatchedKnownFact) // TODO: redirect

  def showConfirmCgtPostcode: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmPostcodeCgt].orRollback

  def submitConfirmCgtPostcode: Action[AnyContent] =
    actions.whenAuthorisedWithRetrievals(AsAgent).bindForm(PostcodeForm.form).applyWithRequest(implicit request => transitions.confirmPostcodeCgt)

  def showConfirmCgtCountryCode: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmCountryCodeCgt]

  def submitConfirmCgtCountryCode: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(CountrycodeForm.form(validCountryCodes))
      .applyWithRequest(implicit request => transitions.confirmCountryCodeCgt)

  def showConfirmPptRegDate: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmRegDatePpt].orRollback

  def submitConfirmPptRegDate: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(agentFastTrackPptRegDateForm)
      .applyWithRequest(implicit request => transitions.confirmRegDatePpt)

  def showConfirmClientCgt: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmClientCgt].orRollback

  def showConfirmClientPpt: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmClientPpt].orRollback

  def submitConfirmCgtClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(checkDetailsForm)
      .applyWithRequest(implicit request => transitions.submitConfirmClientCgt(appConfig))

  def submitConfirmPptClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(checkDetailsForm)
      .applyWithRequest(implicit request => transitions.submitConfirmClientPpt(appConfig))

  val showClientNotSignedUp: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ClientNotSignedUp]
  val showPendingAuthorisationExists: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[PendingInvitationExists]
  val showActiveAuthorisationExists: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[AuthExists]

  val showClientNotRegistered: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ClientNotRegistered]

  val showAlreadyCopiedAcrossItsa: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[AlreadyCopiedAcrossItsa.type]

  val showLegacyAuthorisationDetected: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[LegacyAuthorisationDetected]

  val submitLegacyAuthorisationDetected: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { agent =>
      LegacyAuthorisationForm.bindFromRequest
        .fold(
          hasErrors =>
            Future successful Ok(
              legacyAuthorisationDetectedView(
                hasErrors,
                routes.AgentInvitationFastTrackJourneyController.submitLegacyAuthorisationDetected,
                routes.AgentInvitationFastTrackJourneyController.showCheckDetails.url
              )
            ),
          valid =>
            if (valid.choice)
              Future successful Redirect(externalUrls.agentMappingFrontendUrl)
                .addingToSession(toReturnFromMapping)
            else
              helpers.apply(
                transitions.confirmedLegacyAuthorisation,
                helpers.redirect
              )
        )
    }
  }

  val showClientInsolvent: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ClientInsolventFastTrack.type]

  val showCannotCreateFastTrackRequest: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[CannotCreateFastTrackRequest.type]

  private def toReturnFromMapping()(implicit request: Request[AnyContent]) = {
    val sessionKeyUsedInMappingService = "OriginForMapping"
    sessionKeyUsedInMappingService -> localFriendlyUrl(env)(request.path, request.host)
  }
  /* Here we map states to the GET endpoints for redirecting and back linking */
  override def getCallFor(state: State)(implicit request: Request[_]): Call = state match {

    case Prologue(failureUrlOpt, refererUrlOpt) =>
      failureUrlOpt match {
        case Some(failureUrl) =>
          Call("GET", failureUrl + s"?issue=${agentFastTrackForm.bindFromRequest.errorsAsJson.as[FastTrackErrors].formErrorsMessages}")
        case None => routes.AgentInvitationFastTrackJourneyController.showClientType
      }
    case _: SelectClientType      => routes.AgentInvitationFastTrackJourneyController.showClientType
    case _: MissingDetail         => routes.AgentInvitationFastTrackJourneyController.showKnownFact
    case _: CheckDetails          => routes.AgentInvitationFastTrackJourneyController.showCheckDetails
    case _: IdentifyClient        => routes.AgentInvitationFastTrackJourneyController.showIdentifyClient
    case _: ConfirmClientTrust    => routes.AgentInvitationFastTrackJourneyController.showConfirmTrustClient
    case _: ConfirmPostcodeCgt    => routes.AgentInvitationFastTrackJourneyController.showConfirmCgtPostcode
    case _: ConfirmCountryCodeCgt => routes.AgentInvitationFastTrackJourneyController.showConfirmCgtCountryCode
    case _: ConfirmRegDatePpt     => routes.AgentInvitationFastTrackJourneyController.showConfirmPptRegDate
    case _: ConfirmClientCgt      => routes.AgentInvitationFastTrackJourneyController.showConfirmClientCgt
    case _: ConfirmClientPpt      => routes.AgentInvitationFastTrackJourneyController.showConfirmClientPpt
    case _: InvitationSent        => routes.AgentInvitationFastTrackJourneyController.showInvitationSent
    case TryAgainWithoutFastTrack => routes.AgentInvitationJourneyController.agentsRoot
    case _: ClientNotSignedUp     => routes.AgentInvitationFastTrackJourneyController.showClientNotSignedUp
    case _: PendingInvitationExists =>
      routes.AgentInvitationFastTrackJourneyController.showPendingAuthorisationExists
    case _: ActiveAuthorisationExists =>
      routes.AgentInvitationFastTrackJourneyController.showActiveAuthorisationExists
    case _: PartialAuthorisationExists =>
      routes.AgentInvitationFastTrackJourneyController.showActiveAuthorisationExists
    case _: ClientNotRegistered         => routes.AgentInvitationFastTrackJourneyController.showClientNotRegistered
    case _: ClientNotFound              => routes.AgentInvitationFastTrackJourneyController.showNotMatched
    case _: SuspendedAgent              => routes.AgentInvitationFastTrackJourneyController.showSuspended
    case AlreadyCopiedAcrossItsa        => routes.AgentInvitationFastTrackJourneyController.showAlreadyCopiedAcrossItsa
    case _: LegacyAuthorisationDetected => routes.AgentInvitationFastTrackJourneyController.showLegacyAuthorisationDetected
    case CannotCreateFastTrackRequest   => routes.AgentInvitationFastTrackJourneyController.showCannotCreateFastTrackRequest
    case ClientInsolventFastTrack       => routes.AgentInvitationFastTrackJourneyController.showClientInsolvent

    case _ => throw new Exception(s"Link not found for $state")
  }

  /* Here we decide what to render after state transition */
  override def renderState(state: State, breadcrumbs: List[State], formWithErrors: Option[Form[_]])(implicit request: Request[_]): Result =
    state match {

      case s: Prologue => Redirect(getCallFor(s))

      case CheckDetails(ftr, _) =>
        val backLinkOpt: Option[String] =
          breadcrumbs.headOption match {
            case Some(Prologue(_, refererUrl)) if refererUrl.isDefined => refererUrl
            case _                                                     => None
          }
        Ok(
          checkDetailsView(
            formWithErrors.or(checkDetailsForm),
            CheckDetailsPageConfig(
              ftr,
              routes.AgentInvitationFastTrackJourneyController.progressToClientType,
              routes.AgentInvitationFastTrackJourneyController.progressToKnownFact,
              routes.AgentInvitationFastTrackJourneyController.progressToIdentifyClient,
              routes.AgentInvitationFastTrackJourneyController.submitCheckDetails,
              backLinkOpt
            )
          )
        )

      case MissingDetail(ftr, _) =>
        Ok(
          knownFactsView(
            formWithErrors.or(getKnownFactFormForService(ftr.service)),
            ftr.service,
            getSubmitKFFor(ftr.service),
            backLinkFor(breadcrumbs).url
          )
        )

      case SelectClientType(ftr, _, _) =>
        val clientTypes = Services.supportedClientTypesFor(ftr.service)
        Ok(
          clientTypeView(
            formWithErrors.or(ClientTypeForm.fastTrackForm(clientTypes.toSet)),
            ClientTypePageConfig(
              backLinkFor(breadcrumbs).url,
              routes.AgentInvitationFastTrackJourneyController.submitClientType,
              availableClientTypes = clientTypes
            )
          )
        )

      case ConfirmClientTrust(ftr, _, trustName) =>
        Ok(
          confirmClientView(
            trustName,
            formWithErrors.or(ConfirmClientForm),
            backLinkFor(breadcrumbs).url,
            routes.AgentInvitationFastTrackJourneyController.submitConfirmTrustClient,
            ftr.clientId
          )
        )

      case IdentifyClient(ftr, _) if ftr.clientType.contains(ClientType.Personal) && ftr.service == Service.MtdIt =>
        Ok(
          identifyClientItsaView(
            formWithErrors.or(ItsaClientForm.form),
            routes.AgentInvitationFastTrackJourneyController.submitIdentifyItsaClient,
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(ftr, _) if ftr.clientType.contains(ClientType.Personal) && ftr.service == Service.Vat =>
        Ok(
          identifyClientVatView(
            formWithErrors.or(VatClientForm.form),
            routes.AgentInvitationFastTrackJourneyController.submitIdentifyVatClient,
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(ftr, _) if ftr.clientType.contains(ClientType.Personal) && ftr.service == Service.PersonalIncomeRecord =>
        Ok(
          identifyClientIrvView(
            formWithErrors.or(IrvClientForm.form),
            routes.AgentInvitationFastTrackJourneyController.submitIdentifyIrvClient,
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(ftr, _) if ftr.clientType.contains(ClientType.Business) && ftr.service == Service.Vat =>
        Ok(
          identifyClientVatView(
            formWithErrors.or(VatClientForm.form),
            routes.AgentInvitationFastTrackJourneyController.submitIdentifyVatClient,
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(ftr, _) if List(Service.Trust, Service.TrustNT).contains(ftr.service) =>
        Ok(
          identifyClientTrustView(
            formWithErrors.or(TrustClientForm.form),
            routes.AgentInvitationFastTrackJourneyController.submitIdentifyTrustClient,
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(ftr, _) if ftr.service == Service.CapitalGains =>
        Ok(
          identifyClientCgtView(
            formWithErrors.or(CgtClientForm.form()),
            routes.AgentInvitationFastTrackJourneyController.submitIdentifyCgtClient,
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(ftr, _) if ftr.service == Service.Ppt =>
        Ok(
          identifyClientPptView(
            formWithErrors.or(PptClientForm.form),
            routes.AgentInvitationFastTrackJourneyController.submitIdentifyPptClient,
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(ftr, _) if Seq(Service.Cbc, Service.CbcNonUk).contains(ftr.service) =>
        Ok(
          identifyClientCbcView(
            formWithErrors.or(CbcClientForm.form),
            routes.AgentInvitationFastTrackJourneyController.submitIdentifyCbcClient,
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(ftr, _) if ftr.service == Service.Pillar2 =>
        Ok(
          identifyClientPillar2View(
            formWithErrors.or(Pillar2ClientForm.form),
            routes.AgentInvitationFastTrackJourneyController.submitIdentifyPillar2Client,
            backLinkFor(breadcrumbs).url
          )
        )

      case IdentifyClient(ftr, _) if ftr.clientType.isEmpty && ftr.service == Service.Vat =>
        Ok(
          identifyClientVatView(
            formWithErrors.or(VatClientForm.form),
            routes.AgentInvitationFastTrackJourneyController.submitIdentifyVatClient,
            backLinkFor(breadcrumbs).url
          )
        )

      case ConfirmClientCgt(ftr, _, name) =>
        Ok(
          confirmClientView(
            name,
            formWithErrors.or(ConfirmClientForm),
            backLinkFor(breadcrumbs).url,
            routes.AgentInvitationFastTrackJourneyController.submitConfirmCgtClient,
            ftr.clientId
          )
        )

      case ConfirmPostcodeCgt(ftr, _, _, _) =>
        Ok(
          confirmPostcodeCgtView(
            ftr.clientType.getOrElse(Personal),
            formWithErrors.or(PostcodeForm.form),
            backLinkFor(breadcrumbs).url,
            fromFastTrack = true,
            isDeAuth = false
          )
        )

      case ConfirmCountryCodeCgt(ftr, _, _, _) =>
        Ok(
          confirmCountryCodeCgtView(
            ftr.clientType.getOrElse(Personal),
            countries,
            formWithErrors.or(CountrycodeForm.form(validCountryCodes)),
            backLinkFor(breadcrumbs).url,
            fromFastTrack = true,
            isDeAuth = false
          )
        )

      case ConfirmRegDatePpt(_, _, _, _) =>
        Ok(
          confirmRegDatePptView(
            formWithErrors.or(agentFastTrackPptRegDateForm),
            submitFormCall = routes.AgentInvitationFastTrackJourneyController.submitConfirmPptRegDate,
            backLinkUrl = backLinkFor(breadcrumbs).url,
            isDeAuth = false
          )
        )

      case ConfirmClientPpt(ftr, _, name) =>
        Ok(
          confirmClientView(
            name,
            formWithErrors.or(ConfirmClientForm),
            backLinkFor(breadcrumbs).url,
            routes.AgentInvitationFastTrackJourneyController.submitConfirmPptClient,
            ftr.clientId
          )
        )

      case InvitationSent(clientType, invitationLink, continueUrl, agencyEmail, service, isAltItsa) =>
        Ok(
          invitationSentView(
            InvitationSentPageConfig(
              invitationLink,
              continueUrl,
              continueUrl.isDefined,
              ClientType.fromEnum(clientType),
              inferredExpiryDate,
              agencyEmail,
              Set(service),
              isAltItsa.getOrElse(false)
            )
          )
        )

      case ClientNotFound(ftr, _) =>
        ftr.service match {
          case Service.CapitalGains =>
            Ok(
              cgtRefNotFoundView(
                false,
                routes.AgentInvitationFastTrackJourneyController.redirectTryAgainNotMatchedKnownFact,
                Some(routes.AgentInvitationJourneyController.showReviewAuthorisations),
                ftr.clientId.value
              )
            )
          case Service.Ppt =>
            Ok(
              pptRefNotFoundView(
                false,
                routes.AgentInvitationFastTrackJourneyController.redirectTryAgainNotMatchedKnownFact,
                Some(routes.AgentInvitationJourneyController.showReviewAuthorisations),
                ftr.clientId.value
              )
            )
          case _ =>
            Ok(
              notMatchedView(
                hasJourneyCache = false,
                tryAgainCall = routes.AgentInvitationFastTrackJourneyController.redirectTryAgainNotMatchedKnownFact,
                reviewAuthsCallOpt = Some(routes.AgentInvitationJourneyController.showReviewAuthorisations)
              )
            )
        }

      case ActiveAuthorisationExists(agentFastTrackRequest, _) =>
        Ok(
          activeAuthExistsView(
            authRequestsExist = false,
            agentFastTrackRequest.service,
            agentFastTrackRequest.clientType.getOrElse(Personal),
            fromFastTrack = true,
            routes.AgentInvitationJourneyController.showReviewAuthorisations,
            routes.AgentInvitationFastTrackJourneyController.showClientType
          )
        )

      case PendingInvitationExists(_, agentLink, clientName, _) =>
        Ok(
          pendingAuthExistsView(
            clientName,
            agentLink,
            authRequestsExist = false,
            backLinkFor(breadcrumbs).url,
            fromFastTrack = true,
            routes.AgentInvitationJourneyController.showReviewAuthorisations,
            routes.AgentInvitationFastTrackJourneyController.showClientType
          )
        )

      case PartialAuthorisationExists(_, _) =>
        Ok(
          partialAuthExistsView(
            authRequestsExist = false,
            fromFastTrack = true,
            routes.AgentInvitationJourneyController.showReviewAuthorisations,
            routes.AgentInvitationFastTrackJourneyController.showClientType
          )
        )

      case ClientNotSignedUp(fastTrackRequest, _) =>
        Ok(
          notSignedupView(
            fastTrackRequest.service,
            hasRequests = false,
            isDeAuthJourney = false,
            htmlPartial = notSignedUpPageConfig.render(fastTrackRequest.service)
          )
        )

      case ClientNotRegistered(_, _) =>
        Ok(
          clientNotRegisteredView(
            authRequestsExist = false,
            fromFastTrack = true,
            routes.AgentInvitationJourneyController.showReviewAuthorisations,
            routes.AgentInvitationFastTrackJourneyController.showClientType
          )
        )

      case SuspendedAgent(service, continueUrl) => Ok(suspendedView(service, continueUrl))

      case AlreadyCopiedAcrossItsa => Ok(alreadyCopiedAcrossView())

      case LegacyAuthorisationDetected(_, _, _, _) =>
        Ok(
          legacyAuthorisationDetectedView(
            formWithErrors.or(LegacyAuthorisationForm),
            routes.AgentInvitationFastTrackJourneyController.submitLegacyAuthorisationDetected,
            routes.AgentInvitationFastTrackJourneyController.showCheckDetails.url
          )
        )

      case ClientInsolventFastTrack =>
        Ok(clientInsolventView(hasRequests = false, isFastTrack = true))

      case CannotCreateFastTrackRequest =>
        Ok(cannotCreateRequestView(hasRequests = false, fromFastTrack = true, backLink = s""))

      case _ => throw new Exception(s"Cannot render a page for unexpected state: $state, add your state as a match case in #renderState")
    }
}

object AgentInvitationFastTrackJourneyController {

  val validateFastTrackForm: Constraint[AgentFastTrackRequest] =
    /* TODO [Service onboarding]
       Simplify and avoid explicit enumeration of services/id types.
       Every time we add a new service it is really annoying to have to maintain code like this */
    Constraint[AgentFastTrackRequest] { formData: AgentFastTrackRequest =>
      formData match {
        case AgentFastTrackRequest(Some(ClientType.Personal) | None, Service.MtdIt, Nino(id), _) if Nino.isValid(id) =>
          Valid
        case AgentFastTrackRequest(Some(ClientType.Personal) | None, Service.PersonalIncomeRecord, Nino(id), _) if Nino.isValid(id) =>
          Valid
        case AgentFastTrackRequest(_, Service.Vat, Vrn(id), _) if Vrn.isValid(id)                        => Valid
        case AgentFastTrackRequest(_, Service.Trust, Utr(id), _) if id.matches(utrPattern)               => Valid
        case AgentFastTrackRequest(_, Service.TrustNT, Urn(id), _) if id.matches(urnPattern)             => Valid
        case AgentFastTrackRequest(_, Service.CapitalGains, CgtRef(id), _) if CgtRef.isValid(id)         => Valid
        case AgentFastTrackRequest(_, Service.Ppt, PptRef(id), _) if PptRef.isValid(id)                  => Valid
        case AgentFastTrackRequest(_, Service.Cbc | Service.CbcNonUk, CbcId(id), _) if CbcId.isValid(id) => Valid
        case AgentFastTrackRequest(_, Service.Pillar2, PlrId(id), _) if PlrId.isValid(id)                => Valid
        case _ => Invalid(ValidationError("INVALID_SUBMISSION"))
      }
    }

  val agentFastTrackForm: Form[AgentFastTrackRequest] =
    Form(
      mapping(
        "clientType" -> optional(
          lowerCaseText
            .verifying("UNSUPPORTED_CLIENT_TYPE", Set("personal", "business", "trust").contains _)
            .transform(ClientType.toEnum, ClientType.fromEnum)
        ),
        "service" -> text
          .verifying("UNSUPPORTED_SERVICE", service => supportedServices.exists(_.id == service))
          .transform[Service](Service.forId, _.id),
        "clientIdentifierType" -> text
          .verifying(
            "UNSUPPORTED_CLIENT_ID_TYPE",
            clientType => Service.supportedServices.map(_.supportedSuppliedClientIdType.id).contains(clientType)
          ),
        "clientIdentifier" -> uppercaseNormalizedText.verifying(validateClientId),
        "knownFact"        -> optional(text)
      ) { (clientType, service, clientIdType, clientId, knownFact) =>
        AgentFastTrackRequest(
          ClientType.clientTypeFor(clientType, service),
          service,
          ClientIdType.forId(clientIdType).createUnderlying(clientId),
          knownFact
        )
      } { request =>
        Some((request.clientType, request.service, ClientIdentifier(request.clientId).typeId, request.clientId.value, request.knownFact))
      }.verifying(validateFastTrackForm)
    )

  def confirmationForm(errorMessage: String): Form[Confirmation] =
    Form(
      mapping(
        "accepted" -> optional(normalizedText)
          .transform[String](_.getOrElse(""), s => Some(s))
          .verifying(confirmationChoice(errorMessage))
      )(choice => Confirmation(choice.toBoolean))(confirmation => Some(confirmation.choice.toString))
    )

  val checkDetailsForm = confirmationForm("error.confirmDetails.invalid")

  def IdentifyTrustClientForm: Form[TrustClient] =
    Form(
      mapping(
        "taxId" -> normalizedText.verifying(validTrustTaxIdentifier())
      )(x => TrustClient.apply(x))(x => Some(x.taxId.value))
    )

  def knownFactsForm(knownFactsMapping: Mapping[String]) =
    Form(single("knownFact" -> knownFactsMapping))

  def agentFastTrackPostcodeForm: Form[String] =
    knownFactsForm(postcodeMapping)

  def agentFastTrackDateOfBirthForm: Form[String] =
    knownFactsForm(DateFieldHelper.dateFieldsMapping("irv-date-of-birth"))

  def agentFastTrackVatRegDateForm: Form[String] =
    knownFactsForm(DateFieldHelper.dateFieldsMapping("vat-registration"))

  def agentFastTrackPptRegDateForm: Form[String] =
    knownFactsForm(DateFieldHelper.dateFieldsMapping("ppt-registration"))

  def agentFastTrackCbcEmailForm: Form[String] =
    knownFactsForm(emailMapping)

  def agentFastTrackPillar2RegDateForm: Form[String] =
    knownFactsForm(DateFieldHelper.dateFieldsMapping("pillar2-registration"))

  private def getKnownFactFormForService(service: Service) =
    service match {
      case Service.MtdIt                  => agentFastTrackPostcodeForm
      case Service.PersonalIncomeRecord   => agentFastTrackDateOfBirthForm
      case Service.Vat                    => agentFastTrackVatRegDateForm
      case Service.Ppt                    => agentFastTrackPptRegDateForm
      case Service.Cbc | Service.CbcNonUk => agentFastTrackCbcEmailForm
      case Service.Pillar2                => agentFastTrackPillar2RegDateForm
      case p                              => throw new Exception(s"invalid service in the cache during fast track journey: $p")
    }

  def getSubmitKFFor(service: Service) =
    service match {
      case Service.MtdIt                  => routes.AgentInvitationFastTrackJourneyController.submitKnownFactItsa
      case Service.PersonalIncomeRecord   => routes.AgentInvitationFastTrackJourneyController.submitKnownFactIrv
      case Service.Vat                    => routes.AgentInvitationFastTrackJourneyController.submitKnownFactVat
      case Service.Ppt                    => routes.AgentInvitationFastTrackJourneyController.submitKnownFactPpt
      case Service.Cbc | Service.CbcNonUk => routes.AgentInvitationFastTrackJourneyController.submitKnownFactCbc
      case Service.Pillar2                => routes.AgentInvitationFastTrackJourneyController.submitKnownFactPillar2
    }

}
