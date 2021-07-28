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
import play.api.data.Form
import play.api.data.Forms.{mapping, _}
import play.api.i18n.I18nSupport
import play.api.mvc._
import play.api.Configuration
import uk.gov.hmrc.agentinvitationsfrontend.config.{AppConfig, ExternalUrls}
import uk.gov.hmrc.agentinvitationsfrontend.connectors._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State.{TrustNotClaimed, _}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.{ClientInvitationJourneyService, MongoDBCachedClientInvitationJourneyService}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.support.CallOps
import uk.gov.hmrc.agentinvitationsfrontend.support.CallOps.addParamsToUrl
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators.{confirmationChoice, normalizedText}
import uk.gov.hmrc.agentinvitationsfrontend.views.clients._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.clients._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.timed_out
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.play.fsm.{JourneyController, JourneyIdSupport}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

@Singleton
class ClientInvitationJourneyController @Inject()(
  invitationsService: InvitationsService,
  invitationsConnector: AgentClientAuthorisationConnector,
  identityVerificationConnector: IdentityVerificationConnector,
  authActions: AuthActionsImpl,
  mongoDBCachedClientInvitationJourneyService: MongoDBCachedClientInvitationJourneyService,
  override val journeyService: ClientInvitationJourneyService,
  notAuthorisedAsClientView: not_authorised_as_client,
  timedOutView: timed_out,
  cannotConfirmIdentityView: cannot_confirm_identity,
  lockedOutView: locked_out,
  failedIv5xxView: failed_iv_5xx,
  sessionLostView: session_lost,
  notFoundInvitationView: not_found_invitation,
  actionNeededView: action_needed,
  requestCancelledView: request_cancelled,
  invitationExpiredView: invitation_expired,
  invitationAlreadyRespondedView: invitation_already_responded,
  warmupView: warm_up,
  createNewUserId: create_new_user_id,
  whichTaxServiceView: which_tax_service,
  signUpToTaxServiceView: sign_up_to_tax_service,
  confirmTermsMultiView: confirm_terms_multi,
  checkAnswersView: check_answers,
  confirmDeclineView: confirm_decline,
  completeView: complete,
  invitationDeclinedView: invitation_declined,
  allResponsesFailedView: all_responses_failed,
  someResponsesFailedView: some_responses_failed,
  trustNotClaimedView: trust_not_claimed,
  suspendedAgentView: suspended_agent,
  errorCannotViewRequestView: error_cannot_view_request,
  noOutstandingRequestsView: no_outstanding_requests,
  requestExpiredView: request_expired,
  agentCancelledRequestView: agent_cancelled_request,
  alreadyRespondedView: already_responded,
  cannotFindRequestView: cannot_find_request,
  ggUserIdNeededView: gg_user_id_needed,
  authorisationRequestErrorTemplateView: authorisation_request_error_template)(
  implicit configuration: Configuration,
  val externalUrls: ExternalUrls,
  val mcc: MessagesControllerComponents,
  featureFlags: FeatureFlags,
  ec: ExecutionContext,
  val appConfig: AppConfig,
  override val actionBuilder: DefaultActionBuilder)
    extends FrontendController(mcc) with JourneyController[HeaderCarrier] with JourneyIdSupport[HeaderCarrier] with I18nSupport {

  import ClientInvitationJourneyController._
  import authActions._
  import invitationsConnector._
  import invitationsService._
  import journeyService.model.{State, Transitions}
  import uk.gov.hmrc.play.fsm.OptionalFormOps._

  override implicit def context(implicit rh: RequestHeader): HeaderCarrier =
    appendJourneyId(super.hc)

  override def amendContext(headerCarrier: HeaderCarrier)(key: String, value: String): HeaderCarrier =
    headerCarrier.withExtraHeaders(key -> value)

  override def journeyId(implicit rh: RequestHeader): Option[String] = {
    val journeyIdFromSession = rh.session.get(journeyService.journeyKey)
    lazy val journeyIdFromQuery = rh.getQueryString(journeyService.journeyKey)

    journeyIdFromSession.orElse(journeyIdFromQuery)
  }

  override def withValidRequest(body: => Future[Result])(implicit rc: HeaderCarrier, request: Request[_], ec: ExecutionContext): Future[Result] =
    super.withValidRequest(body)(rc, request, ec).map(appendJourneyId)

  val AsClient: WithAuthorised[AuthorisedClient] = { implicit request: Request[Any] =>
    withAuthorisedAsAnyClient(journeyId)
  }

  type WithAuthorisedMaybe[User] = Request[_] => (Option[User] => Future[Result]) => Future[Result]

  val AsMaybeLoggedInClient: WithAuthorisedMaybe[AuthorisedClient] = { implicit request: Request[Any] =>
    withMaybeLoggedInClient
  }

  /* Here we decide how to handle HTTP request and transition the state of the journey */

  def showMissingJourneyHistory =
    legacy.actionShowState {
      case _ =>
    }

  def warmUp(clientType: String, uid: String, agentName: String): Action[AnyContent] =
    Action.async { implicit request =>
      journeyId match {
        case None =>
          // redirect to itself with new journeyId generated
          Future.successful(appendJourneyId(Results.Redirect(routes.ClientInvitationJourneyController.warmUp(clientType, uid, agentName)))(request))
        case _ =>
          helpers.apply(
            Transitions.start(clientType, uid, agentName)(getAgentReferenceRecord)(invitationsService.getAgencyName),
            helpers.display
          )
      }
    }

  val showGGUserIdNeeded: Action[AnyContent] = actions.show[GGUserIdNeeded].orApply(Transitions.transitionFromLaterState)

  val submitGGUserIdNeeded: Action[AnyContent] = actions.bindForm(confirmHasGGIdForm).apply(Transitions.submitConfirmGGUserId)

  val submitWarmUp: Action[AnyContent] = actions
    .whenAuthorisedWithRetrievals(AsMaybeLoggedInClient)
    .applyWithRequest(implicit request =>
      Transitions.submitWarmUp(featureFlags.agentSuspensionEnabled)(getAllClientInvitationDetailsForAgent, getSuspensionDetails))
    .redirect

  val submitCreateNewUserId: Action[AnyContent] = actions
    .whenAuthorisedWithRetrievals(AsClient)
    .applyWithRequest(implicit request =>
      Transitions.submitCreateNewUserId(featureFlags.agentSuspensionEnabled)(getAllClientInvitationDetailsForAgent, getSuspensionDetails))
    .redirect

  val showCreateNewUserId: Action[AnyContent] = actions.show[CreateNewUserId]

  val submitWarmUpSessionRequired: Action[AnyContent] = actions
    .whenAuthorisedWithRetrievals(AsClient)
    .applyWithRequest(implicit request =>
      Transitions.submitWarmUpSessionRequired(featureFlags.agentSuspensionEnabled)(getAllClientInvitationDetailsForAgent, getSuspensionDetails))
    .redirect

  val showWhichTaxService: Action[AnyContent] = actions.show[WhichTaxService]

  val submitWhichTaxService: Action[AnyContent] = actions
    .bindForm(whichTaxServiceForm)
    .apply(Transitions.submitWhichTaxService)

  val showSignUpToTaxService: Action[AnyContent] = actions.show[SignUpToTaxService]

  val submitSuspendedAgent: Action[AnyContent] = actions.whenAuthorisedWithRetrievals(AsClient)(Transitions.submitSuspension).redirect

  val showConsent: Action[AnyContent] = actions.whenAuthorised(AsClient).show[MultiConsent]

  val showActionNeeded: Action[AnyContent] = actions.whenAuthorised(AsClient).show[ActionNeeded]

  val showNotFoundInvitation: Action[AnyContent] = actions.whenAuthorised(AsClient).show[NotFoundInvitation.type]

  val showErrorNoOutstandingRequests: Action[AnyContent] = actions.whenAuthorised(AsClient).show[NoOutstandingRequests.type]

  val showErrorAuthorisationRequestInvalid: Action[AnyContent] = actions.whenAuthorised(AsClient).show[ErrorState]

  val showErrorAuthorisationRequestUnsuccessful: Action[AnyContent] = actions.whenAuthorised(AsClient).show[AuthErrorState]

  val showErrorCannotFindRequest: Action[AnyContent] = actions.whenAuthorised(AsClient).show[CannotFindRequest]

  val submitConsent: Action[AnyContent] =
    actions.whenAuthorisedWithRetrievals(AsClient).bindForm(confirmTermsMultiForm).apply(Transitions.submitConsents)

  // TODO: Broken using DSL
  val showConsentChange: Action[AnyContent] = legacy.actionShowStateWhenAuthorised(AsClient) {
    case _: SingleConsent =>
  }

  val submitChangeConsents: Action[AnyContent] =
    actions.whenAuthorisedWithRetrievals(AsClient).bindForm(confirmTermsMultiForm)(Transitions.submitChangeConsents)

  val showCheckAnswers: Action[AnyContent] = actions.whenAuthorised(AsClient).show[CheckAnswers]

  val submitCheckAnswers: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsClient)
      .applyWithRequest(
        implicit request => Transitions.submitCheckAnswers(acceptInvitation)(rejectInvitation)
      )
      .redirect

  def submitCheckAnswersChange(uid: String): Action[AnyContent] =
    actions.whenAuthorisedWithRetrievals(AsClient)(Transitions.submitCheckAnswersChange(uid)).redirect

  val submitWarmUpConfirmDecline: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsClient)
      .applyWithRequest(implicit request =>
        Transitions.submitWarmUpToDecline(featureFlags.agentSuspensionEnabled)(getAllClientInvitationDetailsForAgent, getSuspensionDetails))
      .redirect

  val showConfirmDecline: Action[AnyContent] = actions.whenAuthorised(AsClient).show[ConfirmDecline]

  val submitConfirmDecline: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsClient)
      .bindForm(confirmDeclineForm)
      .applyWithRequest(implicit request => Transitions.submitConfirmDecline(rejectInvitation))

  val showInvitationsAccepted: Action[AnyContent] =
    actions.whenAuthorised(AsClient).show[InvitationsAccepted].andCleanBreadcrumbs()

  val showInvitationsDeclined: Action[AnyContent] = actions.whenAuthorised(AsClient).show[InvitationsDeclined].andCleanBreadcrumbs()

  val showAllResponsesFailed: Action[AnyContent] = actions.whenAuthorised(AsClient).show[AllResponsesFailed.type]

  val showSomeResponsesFailed: Action[AnyContent] = actions.whenAuthorised(AsClient).show[SomeResponsesFailed]

  val submitSomeResponsesFailed: Action[AnyContent] =
    actions.whenAuthorisedWithRetrievals(AsClient)(Transitions.continueSomeResponsesFailed).redirect

  val showErrorCannotViewRequest: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAnyAgent {
      journeyService.currentState.flatMap {
        case Some(stateAndBreadCrumbs) =>
          stateAndBreadCrumbs._1 match {
            case WarmUp(clientType, _, _, _, _) =>
              Future successful Forbidden(errorCannotViewRequestView(ClientType.fromEnum(clientType)))
            case _ => Future successful Forbidden(notAuthorisedAsClientView())
          }
        case None => Future successful Forbidden(notAuthorisedAsClientView())
      }
    }
  }

  val signOutAndRedirect: Action[AnyContent] = Action.async { implicit request =>
    journeyId
      .fold(Future successful Redirect(externalUrls.companyAuthFrontendSignOutUrl))(
        jid =>
          journeyService.stepBack.map(_ =>
            Redirect(routes.ClientInvitationJourneyController.submitWarmUp()).withNewSession
              .addingToSession("clientInvitationJourney" -> jid)))
  }

  def incorrectlyAuthorisedAsAgent: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      Future successful Forbidden(notAuthorisedAsClientView())
    }
  }

  def handleIVTimeout(success: Option[String]): Action[AnyContent] = Action.async { implicit request =>
    val successUrl = success.getOrElse(routes.ClientInvitationJourneyController.submitWarmUp().url)
    val continueUrl = CallOps
      .localFriendlyUrl(env)(successUrl, request.host)
    Future successful Forbidden(timedOutView(s"$ggLoginUrl?continue=$continueUrl"))
  }

  private def signOutUrl(implicit request: Request[AnyContent]): Future[String] =
    journeyService.initialState map {
      case State.MissingJourneyHistory => toLocalFriendly(externalUrls.agentClientManagementUrl)
      case s: State                    => s"$ggLoginUrl?continue=${toLocalFriendly(getCallFor(s).url)}"
    }

  def signOut: Action[AnyContent] = Action.async { implicit request =>
    signOutUrl.map(url => Redirect(url).withNewSession)
  }

  def timedOut: Action[AnyContent] = Action.async { implicit request =>
    signOutUrl.map(url => Forbidden(timedOutView(url)).withNewSession)
  }

  private def toLocalFriendly(url: String)(implicit request: Request[_]): String =
    CallOps.localFriendlyUrl(env)(url, request.host)

  def lockedOut: Action[AnyContent] = Action.async { implicit request =>
    Future successful Forbidden(cannotConfirmIdentityView(titleKey = Some("locked-out.header"), html = Some(lockedOutView())))
  }

  def showCannotConfirmIdentity(journeyId: Option[String], success: Option[String]): Action[AnyContent] = Action.async { implicit request =>
    journeyId
      .fold(
        Future.successful(Forbidden(cannotConfirmIdentityView()))
      )(
        id =>
          identityVerificationConnector
            .getIVResult(id)
            .map(reason => getErrorPage(reason, success)))
  }

  private def getErrorPage(reason: Option[IVResult], success: Option[String])(implicit request: Request[_]) =
    reason.fold(Forbidden(cannotConfirmIdentityView())) {
      case TechnicalIssue =>
        Forbidden(cannotConfirmIdentityView(titleKey = Some("technical-issues.header"), html = Some(failedIv5xxView())))
      case FailedMatching | FailedDirectorCheck | FailedIV | InsufficientEvidence =>
        Forbidden(cannotConfirmIdentityView())
      case UserAborted | TimedOut => Redirect(routes.ClientInvitationJourneyController.handleIVTimeout(success))
      case LockedOut              => Redirect(routes.ClientInvitationJourneyController.lockedOut)
      case _                      => Forbidden(cannotConfirmIdentityView())
    }

  def showTrustNotClaimed: Action[AnyContent] = actions.whenAuthorised(AsClient).show[TrustNotClaimed.type]

  def showSuspendedAgent: Action[AnyContent] = actions.whenAuthorised(AsClient).show[SuspendedAgent]

  /* Here we map states to the GET endpoints for redirecting and back linking */
  override def getCallFor(state: State)(implicit request: Request[_]): Call = state match {
    case MissingJourneyHistory => routes.ClientInvitationJourneyController.showMissingJourneyHistory()
    case WarmUp(clientType, uid, _, _, normalisedAgentName) =>
      routes.ClientInvitationJourneyController.warmUp(ClientType.fromEnum(clientType), uid, normalisedAgentName)
    case _: CreateNewUserId => routes.ClientInvitationJourneyController.showCreateNewUserId()
    case _: WhichTaxService =>
      routes.ClientInvitationJourneyController.showWhichTaxService()
    case _: WarmUpSessionRequired => routes.ClientInvitationJourneyController.submitWarmUpSessionRequired()
    case _: SignUpToTaxService    => routes.ClientInvitationJourneyController.showSignUpToTaxService()
    case _: GGUserIdNeeded        => routes.ClientInvitationJourneyController.showGGUserIdNeeded()
    case NotFoundInvitation       => routes.ClientInvitationJourneyController.showNotFoundInvitation()
    case NoOutstandingRequests    => routes.ClientInvitationJourneyController.showErrorNoOutstandingRequests()
    case _: RequestExpired | _: AgentCancelledRequest | _: AlreadyRespondedToRequest =>
      routes.ClientInvitationJourneyController.showErrorAuthorisationRequestInvalid()
    case _: CannotFindRequest => routes.ClientInvitationJourneyController.showErrorCannotFindRequest()
    case _: AuthorisationRequestExpired | _: AuthorisationRequestCancelled | _: AuthorisationRequestAlreadyResponded =>
      routes.ClientInvitationJourneyController.showErrorAuthorisationRequestUnsuccessful()
    case _: ActionNeeded        => routes.ClientInvitationJourneyController.showActionNeeded()
    case _: MultiConsent        => routes.ClientInvitationJourneyController.showConsent()
    case _: SingleConsent       => routes.ClientInvitationJourneyController.showConsentChange()
    case _: CheckAnswers        => routes.ClientInvitationJourneyController.showCheckAnswers()
    case _: ConfirmDecline      => routes.ClientInvitationJourneyController.showConfirmDecline()
    case _: InvitationsAccepted => routes.ClientInvitationJourneyController.showInvitationsAccepted()
    case _: InvitationsDeclined => routes.ClientInvitationJourneyController.showInvitationsDeclined()
    case AllResponsesFailed     => routes.ClientInvitationJourneyController.showAllResponsesFailed()
    case _: SomeResponsesFailed => routes.ClientInvitationJourneyController.showSomeResponsesFailed()
    case TrustNotClaimed        => routes.ClientInvitationJourneyController.showTrustNotClaimed()
    case _: SuspendedAgent      => routes.ClientInvitationJourneyController.showSuspendedAgent()
    case _                      => throw new Exception(s"Link not found for $state")
  }

  /* Here we decide what to render after state transition */
  override def renderState(state: State, breadcrumbs: List[State], formWithErrors: Option[Form[_]])(implicit request: Request[_]): Result =
    state match {

      case MissingJourneyHistory =>
        Ok(sessionLostView())

      case WarmUp(clientType, uid, _, agentName, _) =>
        Ok(
          warmupView(
            WarmUpPageConfig(
              agentName,
              clientType,
              uid,
              routes.ClientInvitationJourneyController.submitWarmUp(),
              routes.ClientInvitationJourneyController.submitWarmUpConfirmDecline(),
              addParamsToUrl(
                appConfig.ggRegistrationFrontendExternalUrl,
                "continue" -> Some(appConfig.agentInvitationsFrontendExternalUrl + routes.ClientInvitationJourneyController.submitWarmUp().url)
              )
            )))

      case _: CreateNewUserId =>
        Ok(
          createNewUserId(
            CreateNewUserIdConfig(addParamsToUrl(
              appConfig.ggRegistrationFrontendExternalUrl,
              "continue" -> Some(appConfig.agentInvitationsFrontendExternalUrl + routes.ClientInvitationJourneyController.submitCreateNewUserId().url)
            ))))

      case _: GGUserIdNeeded =>
        Ok(
          ggUserIdNeededView(
            formWithErrors.or(confirmHasGGIdForm),
            backLinkFor(breadcrumbs),
            routes.ClientInvitationJourneyController.submitGGUserIdNeeded()))

      case _: WarmUpSessionRequired =>
        Redirect(routes.ClientInvitationJourneyController.submitWarmUpSessionRequired())

      case _: WhichTaxService =>
        Ok(
          whichTaxServiceView(
            formWithErrors.or(whichTaxServiceForm),
            backLinkFor(breadcrumbs),
            routes.ClientInvitationJourneyController.submitWhichTaxService()))

      case _: SignUpToTaxService =>
        Ok(
          signUpToTaxServiceView()
        )

      //TODO what's going on with these serviceMessageKey's -  Where are they set and what's the impact on GA?
      case ActionNeeded(clientType) =>
        val serviceMessageKey = fromSession
        Ok(actionNeededView(clientType, serviceMessageKey))

      case NotFoundInvitation =>
        val serviceMessageKey = fromSession
        Ok(notFoundInvitationView(serviceMessageKey))

      case NoOutstandingRequests =>
        val serviceMessageKey = fromSession
        Ok(noOutstandingRequestsView(serviceMessageKey))

      case RequestExpired(expiredOn) =>
        val serviceMessageKey = fromSession
        Ok(requestExpiredView(serviceMessageKey, expiredOn))

      case AgentCancelledRequest(cancelledOn) =>
        val serviceMessageKey = fromSession
        Ok(agentCancelledRequestView(serviceMessageKey, cancelledOn))

      case AlreadyRespondedToRequest(respondedOn) =>
        val serviceMessageKey = fromSession
        Ok(alreadyRespondedView(serviceMessageKey, respondedOn))

      case CannotFindRequest(clientType, agentName) =>
        val clientTypeStr = ClientType.fromEnum(clientType).toLowerCase
        Ok(cannotFindRequestView(clientTypeStr, agentName))

      case AuthorisationRequestExpired(expiredOn, clientType) =>
        val serviceMessageKey = fromSession
        val clientTypeStr = ClientType.fromEnum(clientType).toLowerCase
        Ok(authorisationRequestErrorTemplateView(serviceMessageKey, clientTypeStr, expiredOn, AuthRequestErrorCase.Expired))

      case AuthorisationRequestCancelled(cancelledOn, clientType) =>
        val serviceMessageKey = fromSession
        val clientTypeStr = ClientType.fromEnum(clientType).toLowerCase
        Ok(authorisationRequestErrorTemplateView(serviceMessageKey, clientTypeStr, cancelledOn, AuthRequestErrorCase.Cancelled))

      case AuthorisationRequestAlreadyResponded(respondedOn, clientType) =>
        val serviceMessageKey = fromSession
        val clientTypeStr = ClientType.fromEnum(clientType).toLowerCase
        Ok(authorisationRequestErrorTemplateView(serviceMessageKey, clientTypeStr, respondedOn, AuthRequestErrorCase.AlreadyResponded))

      case MultiConsent(clientType, uid, agentName, _, consents) =>
        val clientTypeStr = ClientType.fromEnum(clientType)
        Ok(
          confirmTermsMultiView(
            formWithErrors.or(confirmTermsMultiForm),
            ConfirmTermsPageConfig(
              agentName,
              clientTypeStr,
              uid,
              consents,
              submitUrl = routes.ClientInvitationJourneyController.submitConsent(),
              checkAnswersUrl = routes.ClientInvitationJourneyController.showCheckAnswers(),
              backLink =
                if (breadcrumbs.exists(_.isInstanceOf[WarmUp])) backLinkFor(breadcrumbs)
                else Call("GET", externalUrls.agentClientManagementUrl)
            )
          ))

      case SingleConsent(clientType, uid, agentName, consent, consents) =>
        Ok(
          confirmTermsMultiView(
            formWithErrors.or(confirmTermsMultiForm),
            ConfirmTermsPageConfig(
              agentName,
              ClientType.fromEnum(clientType),
              uid,
              Seq(consent),
              submitUrl = routes.ClientInvitationJourneyController.submitChangeConsents(),
              checkAnswersUrl = routes.ClientInvitationJourneyController.showCheckAnswers(),
              backLink = backLinkFor(breadcrumbs)
            ),
            changingConsent = true
          ))

      case CheckAnswers(clientType, uid, agentName, consents) =>
        Ok(
          checkAnswersView(
            CheckAnswersPageConfig(
              consents,
              agentName,
              ClientType.fromEnum(clientType),
              uid,
              submitCall = routes.ClientInvitationJourneyController.submitCheckAnswers(),
              changeCall = (serviceKey: String) => routes.ClientInvitationJourneyController.submitCheckAnswersChange(serviceKey),
              backLink = backLinkFor(breadcrumbs)
            )))

      case ConfirmDecline(clientType, uid, agentName, _, consents) =>
        Ok(
          confirmDeclineView(
            formWithErrors.or(confirmDeclineForm),
            ConfirmDeclinePageConfig(
              agentName,
              ClientType.fromEnum(clientType),
              uid,
              consents.map(_.serviceKey).distinct,
              submitUrl = routes.ClientInvitationJourneyController.submitConfirmDecline(),
              backLink = backLinkFor(breadcrumbs)
            )
          ))

      case InvitationsAccepted(agentName, consents, clientType) =>
        Ok(completeView(CompletePageConfig(agentName, consents, clientType)))

      case InvitationsDeclined(agentName, consents, clientType) =>
        Ok(invitationDeclinedView(InvitationDeclinedPageConfig(agentName, consents.map(_.serviceKey).distinct, clientType)))

      case AllResponsesFailed => Ok(allResponsesFailedView())

      case SomeResponsesFailed(agentName, failedConsents, _, clientType) =>
        Ok(someResponsesFailedView(
          SomeResponsesFailedPageConfig(failedConsents, agentName, routes.ClientInvitationJourneyController.submitSomeResponsesFailed(), clientType)))

      case TrustNotClaimed =>
        val backLink =
          if (breadcrumbs.exists(_.isInstanceOf[WarmUp])) backLinkFor(breadcrumbs)
          else Call("GET", externalUrls.agentClientManagementUrl)
        Ok(trustNotClaimedView(backLink))

      case SuspendedAgent(_, _, _, _, suspendedServices, nonSuspendedConsents) =>
        Ok(suspendedAgentView(SuspendedAgentPageConfig(suspendedServices, nonSuspendedConsents.map(_.service).toSet)))
    }

  def fromSession(implicit request: Request[_]): String =
    request.session.get("clientService").getOrElse("service_is_missing")
}

object ClientInvitationJourneyController {

  val confirmTermsMultiForm: Form[ConfirmedTerms] =
    Form[ConfirmedTerms](
      mapping(
        "confirmedTerms.itsa"    -> boolean,
        "confirmedTerms.afi"     -> boolean,
        "confirmedTerms.vat"     -> boolean,
        "confirmedTerms.trust"   -> boolean,
        "confirmedTerms.cgt"     -> boolean,
        "confirmedTerms.trustNT" -> boolean
      )(ConfirmedTerms.apply)(ConfirmedTerms.unapply))

  def confirmationForm(errorMessage: String): Form[Confirmation] =
    Form(
      mapping(
        "accepted" -> optional(normalizedText)
          .transform[String](_.getOrElse(""), s => Some(s))
          .verifying(confirmationChoice(errorMessage))
      )(choice => Confirmation(choice.toBoolean))(confirmation => Some(confirmation.choice.toString)))

  val confirmDeclineForm = confirmationForm("error.confirmDecline.invalid")

  val confirmHasGGIdForm: Form[Confirmation] = confirmationForm("error.confirm-gg-id.required")

  val whichTaxServiceForm: Form[Confirmation] = confirmationForm("???")
}
