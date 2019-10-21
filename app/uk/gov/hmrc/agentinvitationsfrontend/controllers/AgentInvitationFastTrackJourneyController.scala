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
import play.api.{Configuration, Logger}
import play.api.data.Forms.{mapping, optional, single, text}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.data.{Form, Mapping}
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.agentinvitationsfrontend.config.{CountryNamesLoader, ExternalUrls}
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{AgentServicesAccountConnector, InvitationsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentInvitationJourneyController.ConfirmClientForm
import uk.gov.hmrc.agentinvitationsfrontend.forms.{CgtClientForm, ClientTypeForm, CountrycodeForm, PostcodeForm, TrustClientForm}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.agents._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.track.check_details
import uk.gov.hmrc.agentmtdidentifiers.model.{CgtRef, Utr, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.play.fsm.JourneyController

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.Duration

@Singleton
class AgentInvitationFastTrackJourneyController @Inject()(
  @Named("invitation.expiryDuration") expiryDuration: String,
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  asaConnector: AgentServicesAccountConnector,
  relationshipsService: RelationshipsService,
  authActions: AuthActionsImpl,
  val redirectUrlActions: RedirectUrlActions,
  override val journeyService: AgentInvitationFastTrackJourneyService,
  countryNamesLoader: CountryNamesLoader)(
  implicit configuration: Configuration,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  val messagesApi: play.api.i18n.MessagesApi,
  ec: ExecutionContext)
    extends FrontendController with JourneyController[HeaderCarrier] with I18nSupport {

  import AgentInvitationFastTrackJourneyController._
  import asaConnector._
  import authActions._
  import invitationsService._
  import journeyService.model.State._
  import journeyService.model.{State, Transitions}
  import redirectUrlActions._
  import relationshipsService.hasActiveRelationshipFor
  import uk.gov.hmrc.play.fsm.OptionalFormOps._

  override implicit def context(implicit rh: RequestHeader): HeaderCarrier = hc

  //TODO Add local date service to provide flexibility for testing
  private val invitationExpiryDuration = Duration(expiryDuration.replace('_', ' '))
  private def inferredExpiryDate = LocalDate.now().plusDays(invitationExpiryDuration.toDays.toInt)

  private val countries = countryNamesLoader.load
  private val validCountryCodes = countries.keys.toSet

  val AsAgent: WithAuthorised[AuthorisedAgent] = { implicit request: Request[Any] =>
    withAuthorisedAsAgent(_)
  }

  /* Here we decide how to handle HTTP request and transition the state of the journey */

  val agentFastTrack =
    action { implicit request =>
      maybeRedirectUrlOrBadRequest(getRedirectUrl) { redirectUrl =>
        maybeRedirectUrlOrBadRequest(getErrorUrl) { errorUrl =>
          maybeRedirectUrlOrBadRequest(getRefererUrl) { refererUrl =>
            whenAuthorisedWithBootstrapAndForm(Transitions.prologue(errorUrl, refererUrl))(AsAgent)(agentFastTrackForm)(
              Transitions.start(redirectUrl))
          }
        }
      }
    }

  val showCheckDetails = actionShowStateWhenAuthorised(AsAgent) {
    case _: CheckDetailsCompleteItsa        =>
    case _: CheckDetailsCompleteIrv         =>
    case _: CheckDetailsCompletePersonalVat =>
    case _: CheckDetailsCompleteBusinessVat =>
    case _: CheckDetailsCompleteTrust       =>
    case _: CheckDetailsCompleteCgt         =>
    case _: CheckDetailsNoPostcode          =>
    case _: CheckDetailsNoDob               =>
    case _: CheckDetailsNoVatRegDate        =>
    case _: CheckDetailsNoClientTypeVat     =>
  }

  val submitCheckDetails = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(checkDetailsForm)(
      Transitions.checkedDetailsAllInformation(checkPostcodeMatches)(checkCitizenRecordMatches)(
        checkVatRegistrationDateMatches)(createInvitation)(createAgentLink)(getAgencyEmail)(hasPendingInvitationsFor)(
        hasActiveRelationshipFor))
  }

  val progressToIdentifyClient = action { implicit request =>
    whenAuthorised(AsAgent)(Transitions.checkedDetailsChangeInformation)(redirect)
  }

  val identifyClientRedirect = Action(Redirect(routes.AgentInvitationFastTrackJourneyController.showIdentifyClient()))

  val showIdentifyClient = actionShowStateWhenAuthorised(AsAgent) {
    case _: IdentifyPersonalClient     =>
    case _: IdentifyBusinessClient     =>
    case _: IdentifyTrustClient        =>
    case _: IdentifyNoClientTypeClient =>
    case _: IdentifyCgtClient          =>
  }

  val submitIdentifyItsaClient =
    action { implicit request =>
      whenAuthorisedWithForm(AsAgent)(IdentifyItsaClientForm)(
        Transitions.identifiedClientItsa(checkPostcodeMatches)(checkCitizenRecordMatches)(
          checkVatRegistrationDateMatches)(createInvitation)(createAgentLink)(getAgencyEmail)(hasPendingInvitationsFor)(
          hasActiveRelationshipFor))
    }

  val submitIdentifyIrvClient =
    action { implicit request =>
      whenAuthorisedWithForm(AsAgent)(IdentifyIrvClientForm)(
        Transitions.identifiedClientIrv(checkPostcodeMatches)(checkCitizenRecordMatches)(
          checkVatRegistrationDateMatches)(createInvitation)(createAgentLink)(getAgencyEmail)(hasPendingInvitationsFor)(
          hasActiveRelationshipFor))
    }

  val submitIdentifyVatClient =
    action { implicit request =>
      whenAuthorisedWithForm(AsAgent)(IdentifyVatClientForm)(
        Transitions.identifiedClientVat(checkPostcodeMatches)(checkCitizenRecordMatches)(
          checkVatRegistrationDateMatches)(createInvitation)(createAgentLink)(getAgencyEmail)(hasPendingInvitationsFor)(
          hasActiveRelationshipFor))
    }

  val submitIdentifyTrustClient = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(TrustClientForm.form)(Transitions.showConfirmTrustClient(utr =>
      invitationsConnector.getTrustName(utr)))
  }

  val submitIdentifyCgtClient = action { implicit request =>
    journeyService.currentState.map(p => p.map(_._1)).flatMap {
      case Some(s) =>
        val clientType = s match {
          case p: IdentifyCgtClient => p.originalFastTrackRequest.clientType
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

  val progressToKnownFact = action { implicit request =>
    whenAuthorised(AsAgent)(Transitions.checkedDetailsNoKnownFact(invitationsConnector.getCgtSubscription(_)))(redirect)
  }

  val showConfirmTrustClient = actionShowStateWhenAuthorised(AsAgent) {
    case _ =>
  }

  val submitConfirmTrustClient = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(checkDetailsForm)(
      Transitions.submitConfirmTrustClient(createInvitation)(createAgentLink)(getAgencyEmail)(hasPendingInvitationsFor)(
        hasActiveRelationshipFor))
  }

  val knownFactRedirect = Action(Redirect(routes.AgentInvitationFastTrackJourneyController.showKnownFact()))

  val showKnownFact = actionShowStateWhenAuthorised(AsAgent) {
    case _: NoPostcode | _: NoDob | _: NoVatRegDate =>
  }

  val submitKnownFactItsa =
    action { implicit request =>
      whenAuthorisedWithForm(AsAgent)(agentFastTrackPostcodeForm)(
        Transitions.moreDetailsItsa(checkPostcodeMatches)(checkCitizenRecordMatches)(checkVatRegistrationDateMatches)(
          createInvitation)(createAgentLink)(getAgencyEmail)(hasPendingInvitationsFor)(hasActiveRelationshipFor))
    }

  val submitKnownFactIrv =
    action { implicit request =>
      whenAuthorisedWithForm(AsAgent)(agentFastTrackDateOfBirthForm)(
        Transitions.moreDetailsIrv(checkPostcodeMatches)(checkCitizenRecordMatches)(checkVatRegistrationDateMatches)(
          createInvitation)(createAgentLink)(getAgencyEmail)(hasPendingInvitationsFor)(hasActiveRelationshipFor))
    }
  val submitKnownFactVat = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(agentFastTrackVatRegDateForm)(
      Transitions.moreDetailsVat(checkPostcodeMatches)(checkCitizenRecordMatches)(checkVatRegistrationDateMatches)(
        createInvitation)(createAgentLink)(getAgencyEmail)(hasPendingInvitationsFor)(hasActiveRelationshipFor))
  }

  val progressToClientType = action { implicit request =>
    whenAuthorised(AsAgent)(Transitions.checkedDetailsNoClientType)(redirect)
  }

  val showClientType = actionShowStateWhenAuthorised(AsAgent) {
    case _: SelectClientTypeVat =>
  }

  val submitClientType = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ClientTypeForm.fastTrackForm)(
      Transitions.selectedClientType(checkPostcodeMatches)(checkCitizenRecordMatches)(checkVatRegistrationDateMatches)(
        createInvitation)(createAgentLink)(getAgencyEmail)(hasPendingInvitationsFor)(hasActiveRelationshipFor)(
        invitationsConnector.getCgtSubscription(_)))
  }

  val showInvitationSent = actionShowStateWhenAuthorised(AsAgent) {
    case _: InvitationSentPersonal | _: InvitationSentBusiness =>
  }

  val showNotMatched = actionShowStateWhenAuthorised(AsAgent) {
    case _: KnownFactNotMatched =>
    case _: TrustNotFound       =>
    case _: CgtRefNotFound      =>
  }

  val redirectTryAgainNotMatchedKnownFact = action { implicit request =>
    whenAuthorised(AsAgent)(Transitions.tryAgainNotMatchedKnownFact)(redirect)
  }

  def showConfirmCgtPostcode: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: ConfirmPostcodeCgt =>
  }

  def submitConfirmCgtPostcode: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(PostcodeForm.form)(Transitions.confirmPostcodeCgt)
  }

  def showConfirmCgtCountryCode: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: ConfirmCountryCodeCgt =>
  }

  def submitConfirmCgtCountryCode: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(CountrycodeForm.form(validCountryCodes))(Transitions.confirmCountryCodeCgt)
  }

  def showConfirmClientCgt: Action[AnyContent] = actionShowStateWhenAuthorised(AsAgent) {
    case _: ConfirmClientCgt =>
  }

  def submitConfirmCgtClient: Action[AnyContent] = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(checkDetailsForm)(
      Transitions.submitConfirmClientCgt(createInvitation)(createAgentLink)(getAgencyEmail)(hasPendingInvitationsFor)(
        hasActiveRelationshipFor))
  }

  val showClientNotSignedUp = actionShowStateWhenAuthorised(AsAgent) { case _: ClientNotSignedUp                 => }
  val showPendingAuthorisationExists = actionShowStateWhenAuthorised(AsAgent) { case _: PendingInvitationExists  => }
  val showActiveAuthorisationExists = actionShowStateWhenAuthorised(AsAgent) { case _: ActiveAuthorisationExists => }

  /* Here we map states to the GET endpoints for redirecting and back linking */
  override def getCallFor(state: State)(implicit request: Request[_]): Call = state match {
    case Prologue(failureUrlOpt, refererUrlOpt) =>
      failureUrlOpt match {
        case Some(failureUrl) =>
          Call(
            "GET",
            failureUrl + s"?issue=${agentFastTrackForm.bindFromRequest.errorsAsJson.as[FastTrackErrors].formErrorsMessages}")
        case None => routes.AgentInvitationFastTrackJourneyController.showClientType()
      }
    case _: SelectClientTypeVat             => routes.AgentInvitationFastTrackJourneyController.showClientType()
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
    case _: TrustNotFound  => routes.AgentInvitationFastTrackJourneyController.showNotMatched()
    case _: CgtRefNotFound => routes.AgentInvitationFastTrackJourneyController.showNotMatched()
    case _                 => throw new Exception(s"Link not found for $state")
  }

  private def gotoCheckDetailsWithRequest(fastTrackRequest: AgentFastTrackRequest, breadcrumbs: List[State])(
    implicit request: Request[_]): Result = {
    val backLinkOpt: Option[String] =
      breadcrumbs.headOption match {
        case Some(Prologue(_, refererUrl)) if refererUrl.isDefined => refererUrl
        case _                                                     => None
      }

    Ok(
      check_details(
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
  override def renderState(state: State, breadcrumbs: List[State], formWithErrors: Option[Form[_]])(
    implicit request: Request[_]): Result = state match {

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
        known_fact(
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
        known_fact(
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
        known_fact(
          formWithErrors.or(getKnownFactFormForService(ftr.service)),
          KnownFactPageConfig(
            ftr.service,
            Services.determineServiceMessageKeyFromService(ftr.service),
            getSubmitKFFor(ftr.service),
            backLinkFor(breadcrumbs).url
          )
        ))

    case SelectClientTypeVat(_, _, _) =>
      Ok(
        client_type(
          formWithErrors.or(ClientTypeForm.fastTrackForm),
          ClientTypePageConfig(
            backLinkFor(breadcrumbs).url,
            routes.AgentInvitationFastTrackJourneyController.submitClientType(),
            featureFlags.showHmrcTrust,
            isForVat = true
          )
        ))

    case ConfirmClientTrust(_, ftr, _, trustName) =>
      Ok(
        confirm_client(
          trustName,
          formWithErrors.or(ConfirmClientForm),
          backLinkFor(breadcrumbs).url,
          routes.AgentInvitationFastTrackJourneyController.submitConfirmTrustClient(),
          ftr.clientIdentifierType,
          ftr.clientIdentifier
        ))

    case IdentifyPersonalClient(_, ftr, _) if ftr.service == HMRCMTDIT =>
      Ok(
        identify_client_itsa(
          formWithErrors.or(IdentifyItsaClientForm),
          routes.AgentInvitationFastTrackJourneyController.submitIdentifyItsaClient(),
          backLinkFor(breadcrumbs).url
        )
      )

    case IdentifyPersonalClient(_, ftr, _) if ftr.service == HMRCMTDVAT =>
      Ok(
        identify_client_vat(
          formWithErrors.or(IdentifyVatClientForm),
          routes.AgentInvitationFastTrackJourneyController.submitIdentifyVatClient(),
          backLinkFor(breadcrumbs).url
        )
      )

    case IdentifyPersonalClient(_, ftr, _) if ftr.service == HMRCPIR =>
      Ok(
        identify_client_irv(
          formWithErrors.or(IdentifyIrvClientForm),
          routes.AgentInvitationFastTrackJourneyController.submitIdentifyIrvClient(),
          backLinkFor(breadcrumbs).url
        )
      )

    case IdentifyBusinessClient(_, _, _) =>
      Ok(
        identify_client_vat(
          formWithErrors.or(IdentifyVatClientForm),
          routes.AgentInvitationFastTrackJourneyController.submitIdentifyVatClient(),
          backLinkFor(breadcrumbs).url
        )
      )

    case IdentifyTrustClient(_, _, _) =>
      Ok(
        identify_client_trust(
          formWithErrors.or(IdentifyTrustClientForm),
          routes.AgentInvitationFastTrackJourneyController.submitIdentifyTrustClient(),
          backLinkFor(breadcrumbs).url
        )
      )

    case IdentifyCgtClient(_, ftr, _) =>
      val clientType = ftr.clientType.getOrElse(personal)
      Ok(
        identify_client_cgt(
          clientType,
          formWithErrors.or(CgtClientForm.form(clientType)),
          routes.AgentInvitationFastTrackJourneyController.submitIdentifyCgtClient(),
          backLinkFor(breadcrumbs).url
        )
      )

    case IdentifyNoClientTypeClient(_, _, _) =>
      Ok(
        identify_client_vat(
          formWithErrors.or(IdentifyVatClientForm),
          routes.AgentInvitationFastTrackJourneyController.submitIdentifyVatClient(),
          backLinkFor(breadcrumbs).url
        )
      )

    case ConfirmClientCgt(_, ftr, _, name) =>
      Ok(
        confirm_client(
          name,
          formWithErrors.or(ConfirmClientForm),
          backLinkFor(breadcrumbs).url,
          routes.AgentInvitationFastTrackJourneyController.submitConfirmCgtClient(),
          ftr.clientIdentifierType,
          ftr.clientIdentifier
        ))

    case ConfirmPostcodeCgt(_, ftr, _, _, _) =>
      Ok(
        confirm_postcode_cgt(
          ftr.clientType.getOrElse(personal),
          formWithErrors.or(PostcodeForm.form),
          backLinkFor(breadcrumbs).url,
          true))

    case ConfirmCountryCodeCgt(_, ftr, _, _, _) =>
      Ok(
        confirm_countryCode_cgt(
          ftr.clientType.getOrElse(personal),
          countries,
          formWithErrors.or(CountrycodeForm.form(validCountryCodes)),
          backLinkFor(breadcrumbs).url,
          true))

    case InvitationSentPersonal(invitationLink, continueUrl, agencyEmail) =>
      Ok(
        invitation_sent(
          InvitationSentPageConfig(
            invitationLink,
            continueUrl,
            continueUrl.isDefined,
            ClientType.fromEnum(personal),
            inferredExpiryDate,
            agencyEmail)))

    case InvitationSentBusiness(invitationLink, continueUrl, agencyEmail, service) =>
      Ok(
        invitation_sent(
          InvitationSentPageConfig(
            invitationLink,
            continueUrl,
            continueUrl.isDefined,
            ClientType.fromEnum(business),
            inferredExpiryDate,
            agencyEmail,
            service)))

    case KnownFactNotMatched(_, _, _) =>
      Ok(
        not_matched(
          hasJourneyCache = false,
          tryAgainCall = routes.AgentInvitationFastTrackJourneyController.redirectTryAgainNotMatchedKnownFact(),
          reviewAuthsCallOpt = Some(routes.AgentInvitationJourneyController.showReviewAuthorisations())
        ))

    case TrustNotFound(_, _, _) =>
      Ok(
        not_matched(
          hasJourneyCache = false,
          tryAgainCall = routes.AgentInvitationFastTrackJourneyController.redirectTryAgainNotMatchedKnownFact(),
          reviewAuthsCallOpt = Some(routes.AgentInvitationJourneyController.showReviewAuthorisations())
        ))

    case CgtRefNotFound(cgtRef) =>
      Ok(
        cgtRef_notFound(
          false,
          routes.AgentInvitationFastTrackJourneyController.showIdentifyClient(),
          Some(routes.AgentInvitationJourneyController.showReviewAuthorisations()),
          cgtRef.value
        ))

    case ActiveAuthorisationExists(agentFastTrackRequest, _) =>
      Ok(
        active_authorisation_exists(
          authRequestsExist = false,
          agentFastTrackRequest.service,
          agentFastTrackRequest.clientType.getOrElse(personal),
          fromFastTrack = true,
          routes.AgentInvitationJourneyController.showReviewAuthorisations(),
          routes.AgentInvitationFastTrackJourneyController.showClientType()
        ))

    case PendingInvitationExists(_, _) =>
      Ok(
        pending_authorisation_exists(
          PendingAuthorisationExistsPageConfig(
            authRequestsExist = false,
            backLinkFor(breadcrumbs).url,
            fromFastTrack = true,
            routes.AgentInvitationJourneyController.showReviewAuthorisations(),
            routes.AgentInvitationFastTrackJourneyController.showClientType()
          )))

    case ClientNotSignedUp(fastTrackRequest, _) =>
      Ok(not_signed_up(fastTrackRequest.service, hasRequests = false))
  }
}

object AgentInvitationFastTrackJourneyController {

  val validateFastTrackForm: Constraint[AgentFastTrackRequest] =
    Constraint[AgentFastTrackRequest] { formData: AgentFastTrackRequest =>
      formData match {
        case AgentFastTrackRequest(Some(ClientType.personal) | None, HMRCMTDIT, "ni", clientId, _)
            if Nino.isValid(clientId) =>
          Valid
        case AgentFastTrackRequest(Some(ClientType.personal) | None, HMRCPIR, "ni", clientId, _)
            if Nino.isValid(clientId) =>
          Valid
        case AgentFastTrackRequest(_, HMRCMTDVAT, "vrn", clientId, _) if Vrn.isValid(clientId)        => Valid
        case AgentFastTrackRequest(_, TRUST, "utr", clientId, _) if clientId.matches(utrPattern)      => Valid
        case AgentFastTrackRequest(_, HMRCCGTPD, "CGTPDRef", clientId, _) if CgtRef.isValid(clientId) => Valid
        case _                                                                                        => Invalid(ValidationError("INVALID_SUBMISSION"))
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
        Some(
          (
            request.clientType,
            request.service,
            request.clientIdentifierType,
            request.clientIdentifier,
            request.knownFact))
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
      "clientIdentifier" -> uppercaseNormalizedText.verifying(validNino()),
      "postcode"         -> postcodeMapping
    )(ItsaClient.apply)(ItsaClient.unapply)
  )

  def IdentifyVatClientForm: Form[VatClient] = Form(
    mapping(
      "clientIdentifier" -> normalizedText.verifying(validVrn),
      "registrationDate" -> DateFieldHelper.dateFieldsMapping(validVatDateFormat)
    )(VatClient.apply)(VatClient.unapply)
  )

  def IdentifyTrustClientForm: Form[TrustClient] = Form(
    mapping(
      "utr" -> normalizedText.verifying(validUtr())
    )(x => TrustClient.apply(Utr(x)))(x => Some(x.utr.value))
  )

  def IdentifyIrvClientForm: Form[IrvClient] = Form(
    mapping(
      "clientIdentifier" -> uppercaseNormalizedText.verifying(validNino()),
      "dob"              -> dateOfBirthMapping
    )(IrvClient.apply)(IrvClient.unapply)
  )

  def knownFactsForm(knownFactsMapping: Mapping[String]) =
    Form(single("knownFact" -> knownFactsMapping))

  def agentFastTrackPostcodeForm: Form[String] =
    knownFactsForm(postcodeMapping)

  def agentFastTrackDateOfBirthForm: Form[String] =
    knownFactsForm(dateOfBirthMapping)

  def agentFastTrackVatRegDateForm: Form[String] =
    knownFactsForm(vatRegDateMapping)

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
