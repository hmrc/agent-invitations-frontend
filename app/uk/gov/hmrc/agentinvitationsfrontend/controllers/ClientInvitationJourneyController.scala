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

import javax.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, _}
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc._
import play.api.{Configuration, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{IdentityVerificationConnector, InvitationsConnector, PersonalDetailsValidationConnector}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State.{TrustNotClaimed, _}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.support.CallOps
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators.{confirmationChoice, normalizedText}
import uk.gov.hmrc.agentinvitationsfrontend.views.clients._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.clients._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.play.fsm.{JourneyController, JourneyIdSupport}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

@Singleton
class ClientInvitationJourneyController @Inject()(
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  identityVerificationConnector: IdentityVerificationConnector,
  authActions: AuthActions,
  pdvConnector: PersonalDetailsValidationConnector,
  override val journeyService: ClientInvitationJourneyService)(
  implicit configuration: Configuration,
  val externalUrls: ExternalUrls,
  val messagesApi: play.api.i18n.MessagesApi,
  featureFlags: FeatureFlags,
  ec: ExecutionContext)
    extends FrontendController with JourneyController[HeaderCarrier] with JourneyIdSupport[HeaderCarrier]
    with I18nSupport {

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

  override def withValidRequest(
    body: => Future[Result])(implicit rc: HeaderCarrier, request: Request[_], ec: ExecutionContext): Future[Result] =
    super.withValidRequest(body)(rc, request, ec).map(appendJourneyId)

  val AsClient: WithAuthorised[AuthorisedClient] = { implicit request: Request[Any] =>
    withAuthorisedAsAnyClient(journeyId)
  }

  /* Here we decide how to handle HTTP request and transition the state of the journey */

  def showMissingJourneyHistory =
    actionShowState {
      case _ =>
    }

  def warmUp(clientType: String, uid: String, normalisedAgentName: String) =
    Action.async { implicit request =>
      journeyId match {
        case None =>
          // redirect to itself with new journeyId generated
          Future.successful(
            appendJourneyId(
              Results.Redirect(routes.ClientInvitationJourneyController.warmUp(clientType, uid, normalisedAgentName)))(
              request))
        case _ =>
          apply(
            Transitions.start(clientType, uid, normalisedAgentName)(getAgentReferenceRecord)(getAgencyName),
            display)
      }
    }

  val submitWarmUp = {
    action { implicit request =>
      whenAuthorised(AsClient)(Transitions.submitWarmUp(getAllClientInvitationsInfoForAgentAndStatus))(redirect)
    }
  }

  val showConsent = actionShowStateWhenAuthorised(AsClient) {
    case _: MultiConsent =>
  }

  val showNotFoundInvitation = actionShowStateWhenAuthorised(AsClient) {
    case NotFoundInvitation =>
  }

  def submitConsent = action { implicit request =>
    whenAuthorisedWithForm(AsClient)(confirmTermsMultiForm)(Transitions.submitConsents)
  }

  def showConsentChange = actionShowStateWhenAuthorised(AsClient) {
    case _: SingleConsent =>
  }

  def submitChangeConsents = action { implicit request =>
    whenAuthorisedWithForm(AsClient)(confirmTermsMultiForm)(Transitions.submitChangeConsents)
  }

  def showCheckAnswers = actionShowStateWhenAuthorised(AsClient) {
    case _: CheckAnswers =>
  }

  def submitCheckAnswers = action { implicit request =>
    whenAuthorised(AsClient)(Transitions.submitCheckAnswers(acceptInvitation)(rejectInvitation))(redirect)
  }

  def submitCheckAnswersChange(uid: String) = action { implicit request =>
    whenAuthorised(AsClient)(Transitions.submitCheckAnswersChange(uid))(redirect)
  }

  def submitWarmUpConfirmDecline = action { implicit request =>
    whenAuthorised(AsClient)(Transitions.submitWarmUpToDecline(getAllClientInvitationsInfoForAgentAndStatus))(redirect)
  }

  def showConfirmDecline = actionShowStateWhenAuthorised(AsClient) {
    case _: ConfirmDecline =>
  }

  def submitConfirmDecline = action { implicit request =>
    whenAuthorisedWithForm(AsClient)(confirmDeclineForm)(Transitions.submitConfirmDecline(rejectInvitation))
  }

  def showInvitationsAccepted = action { implicit request =>
    showStateWhenAuthorised(AsClient) {
      case _: InvitationsAccepted =>
    }.andThen {
      // clears journey history
      case Success(_) => journeyService.cleanBreadcrumbs()
    }
  }

  def showInvitationsDeclined = action { implicit request =>
    showStateWhenAuthorised(AsClient) {
      case _: InvitationsDeclined =>
    }.andThen {
      // clears journey history
      case Success(_) => journeyService.cleanBreadcrumbs()
    }
  }

  def showAllResponsesFailed = actionShowStateWhenAuthorised(AsClient) {
    case AllResponsesFailed =>
  }

  def showSomeResponsesFailed = actionShowStateWhenAuthorised(AsClient) {
    case _: SomeResponsesFailed =>
  }

  def submitSomeResponsesFailed = action { implicit request =>
    whenAuthorised(AsClient)(Transitions.continueSomeResponsesFailed)(redirect)
  }

  def incorrectlyAuthorisedAsAgent: Action[AnyContent] = Action.async { implicit request =>
    authActions.withAuthorisedAsAgent { _ =>
      Future successful Forbidden(not_authorised_as_client())
    }
  }

  def handleIVTimeout(success: Option[String]): Action[AnyContent] = Action.async { implicit request =>
    val successUrl = success.getOrElse(routes.ClientInvitationJourneyController.submitWarmUp().url)
    val continueUrl = CallOps
      .localFriendlyUrl(env, config)(successUrl, request.host)
    Future successful Forbidden(signed_out(s"$ggLoginUrl?continue=$continueUrl"))
  }

  def lockedOut: Action[AnyContent] = Action.async { implicit request =>
    Future successful Forbidden(
      cannot_confirm_identity(title = Some(Messages("locked-out.header")), html = Some(locked_out())))
  }

  def showCannotConfirmIdentity(journeyId: Option[String], success: Option[String]): Action[AnyContent] = Action.async {
    implicit request =>
      journeyId
        .fold(
          Future.successful(Forbidden(cannot_confirm_identity()))
        )(
          id =>
            identityVerificationConnector
              .getIVResult(id)
              .map(reason => getErrorPage(reason, success)))
  }

  def pdvComplete(targetUrl: Option[String] = None, validationId: Option[String] = None): Action[AnyContent] =
    Action.async { implicit request =>
      withIndividualAuth { providerId =>
        val validId = validationId.getOrElse("TODO")
        pdvConnector
          .getPdvResult(validId)
          .flatMap {
            case Some(nino) =>
              identityVerificationConnector
                .updateEntry(NinoClStoreEntry(providerId, nino, None, None, None), providerId)
                .map(_ => Redirect(targetUrl.getOrElse("TODO")))
            case None => {
              Logger.warn(
                s"no Nino returned from personal-details-validation using validationId ${validationId.getOrElse("no id found!")}")
              Future.successful(Forbidden(cannot_confirm_identity()))
            }
          }
      }
    }

  private def getErrorPage(reason: Option[IVResult], success: Option[String])(implicit request: Request[_]) =
    reason.fold(Forbidden(cannot_confirm_identity())) {
      case TechnicalIssue =>
        Forbidden(
          cannot_confirm_identity(title = Some(Messages("technical-issues.header")), html = Some(failed_iv_5xx())))
      case FailedMatching | FailedDirectorCheck | FailedIV | InsufficientEvidence =>
        Forbidden(cannot_confirm_identity())
      case UserAborted | TimedOut => Redirect(routes.ClientInvitationJourneyController.handleIVTimeout(success))
      case LockedOut              => Redirect(routes.ClientInvitationJourneyController.lockedOut)
      case _                      => Forbidden(cannot_confirm_identity())
    }

  def showTrustNotClaimed: Action[AnyContent] = actionShowStateWhenAuthorised(AsClient) {
    case TrustNotClaimed =>
  }

  /* Here we map states to the GET endpoints for redirecting and back linking */
  override def getCallFor(state: State)(implicit request: Request[_]): Call = state match {
    case MissingJourneyHistory => routes.ClientInvitationJourneyController.showMissingJourneyHistory()
    case WarmUp(clientType, uid, _, normalisedAgentName) =>
      routes.ClientInvitationJourneyController.warmUp(ClientType.fromEnum(clientType), uid, normalisedAgentName)
    case NotFoundInvitation     => routes.ClientInvitationJourneyController.showNotFoundInvitation()
    case _: MultiConsent        => routes.ClientInvitationJourneyController.showConsent()
    case _: SingleConsent       => routes.ClientInvitationJourneyController.showConsentChange()
    case _: CheckAnswers        => routes.ClientInvitationJourneyController.showCheckAnswers()
    case _: ConfirmDecline      => routes.ClientInvitationJourneyController.showConfirmDecline()
    case _: InvitationsAccepted => routes.ClientInvitationJourneyController.showInvitationsAccepted()
    case _: InvitationsDeclined => routes.ClientInvitationJourneyController.showInvitationsDeclined()
    case AllResponsesFailed     => routes.ClientInvitationJourneyController.showAllResponsesFailed()
    case _: SomeResponsesFailed => routes.ClientInvitationJourneyController.showSomeResponsesFailed()
    case TrustNotClaimed        => routes.ClientInvitationJourneyController.showTrustNotClaimed()
    case _                      => throw new Exception(s"Link not found for $state")
  }

  /* Here we decide what to render after state transition */
  override def renderState(state: State, breadcrumbs: List[State], formWithErrors: Option[Form[_]])(
    implicit request: Request[_]): Result = state match {

    case MissingJourneyHistory =>
      Ok(session_lost())

    case WarmUp(clientType, uid, agentName, _) =>
      Ok(
        warm_up(
          WarmUpPageConfig(
            agentName,
            clientType,
            uid,
            routes.ClientInvitationJourneyController.submitWarmUp(),
            routes.ClientInvitationJourneyController.submitWarmUpConfirmDecline()
          )))

    case NotFoundInvitation =>
      val serviceMessageKey = request.session.get("clientService").getOrElse("Service Is Missing")
      Ok(not_found_invitation(serviceMessageKey))

    case MultiConsent(clientType, uid, agentName, consents) =>
      val clientTypeStr = ClientType.fromEnum(clientType)
      Ok(
        confirm_terms_multi(
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
        confirm_terms_multi(
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
        check_answers(
          CheckAnswersPageConfig(
            consents,
            agentName,
            ClientType.fromEnum(clientType),
            uid,
            submitCall = routes.ClientInvitationJourneyController.submitCheckAnswers(),
            changeCall = (serviceKey: String) =>
              routes.ClientInvitationJourneyController.submitCheckAnswersChange(serviceKey),
            backLink = backLinkFor(breadcrumbs)
          )))

    case ConfirmDecline(clientType, uid, agentName, consents) =>
      Ok(
        confirm_decline(
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

    case InvitationsAccepted(agentName, consents) => Ok(complete(CompletePageConfig(agentName, consents)))

    case InvitationsDeclined(agentName, consents) =>
      Ok(invitation_declined(InvitationDeclinedPageConfig(agentName, consents.map(_.serviceKey).distinct)))

    case AllResponsesFailed => Ok(all_responses_failed())

    case SomeResponsesFailed(agentName, failedConsents, _) =>
      Ok(
        some_responses_failed(
          SomeResponsesFailedPageConfig(
            failedConsents,
            agentName,
            routes.ClientInvitationJourneyController.submitSomeResponsesFailed())))

    case TrustNotClaimed =>
      val backLink =
        if (breadcrumbs.exists(_.isInstanceOf[WarmUp])) backLinkFor(breadcrumbs)
        else Call("GET", externalUrls.agentClientManagementUrl)
      Ok(trust_not_claimed(backLink))
  }
}

object ClientInvitationJourneyController {

  val confirmTermsMultiForm: Form[ConfirmedTerms] =
    Form[ConfirmedTerms](
      mapping(
        "confirmedTerms.itsa"  -> boolean,
        "confirmedTerms.afi"   -> boolean,
        "confirmedTerms.vat"   -> boolean,
        "confirmedTerms.trust" -> boolean
      )(ConfirmedTerms.apply)(ConfirmedTerms.unapply))

  def confirmationForm(errorMessage: String): Form[Confirmation] =
    Form(
      mapping(
        "accepted" -> optional(normalizedText)
          .transform[String](_.getOrElse(""), s => Some(s))
          .verifying(confirmationChoice(errorMessage))
      )(choice => Confirmation(choice.toBoolean))(confirmation => Some(confirmation.choice.toString)))

  val confirmDeclineForm = confirmationForm("error.confirmDecline.invalid")

}
