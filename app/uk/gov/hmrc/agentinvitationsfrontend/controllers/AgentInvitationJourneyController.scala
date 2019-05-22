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
import play.api.Configuration
import play.api.data.Form
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.InvitationsConnector
import uk.gov.hmrc.agentinvitationsfrontend.forms._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.views.agents._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.play.fsm.JourneyController

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

@Singleton
class AgentInvitationJourneyController @Inject()(
  @Named("invitation.expiryDuration") expiryDuration: String,
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  relationshipsService: RelationshipsService,
  val authActions: AuthActions,
  override val journeyService: AgentInvitationJourneyService)(
  implicit configuration: Configuration,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  val messagesApi: play.api.i18n.MessagesApi,
  ec: ExecutionContext)
    extends FrontendController with JourneyController[HeaderCarrier] with I18nSupport {

  import AgentInvitationJourneyController._
  import authActions._
  import invitationsService._
  import journeyService.model.State._
  import journeyService.model.{State, Transitions}
  import uk.gov.hmrc.play.fsm.OptionalFormOps._

  override implicit def context(implicit rh: RequestHeader): HeaderCarrier = hc

  private val invitationExpiryDuration = Duration(expiryDuration.replace('_', ' '))
  private val inferredExpiryDate = LocalDate.now().plusDays(invitationExpiryDuration.toDays.toInt)

  val AsAgent: WithAuthorised[AuthorisedAgent] = { implicit request: Request[Any] =>
    withAuthorisedAsAgent(_)
  }

  /* Here we decide how to handle HTTP request and transition the state of the journey */
  val agentsRoot = Action(Redirect(routes.AgentInvitationJourneyController.showClientType()))

  val showClientType = action { implicit request =>
    whenAuthorised(AsAgent)(Transitions.showSelectClientType)(display)
  }

  val submitClientType = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ClientTypeForm.form)(Transitions.selectedClientType)
  }

  val showSelectService = actionShowStateWhenAuthorised(AsAgent) {
    case _: SelectPersonalService | SelectBusinessService =>
  }

  val submitPersonalSelectService = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ServiceTypeForm.form)(
      Transitions.selectedPersonalService(
        featureFlags.showHmrcMtdIt,
        featureFlags.showPersonalIncome,
        featureFlags.showHmrcMtdVat))
  }

  val submitBusinessSelectService = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(CommonConfirmationForms.serviceBusinessForm)(
      Transitions.selectedBusinessService(featureFlags.showHmrcMtdVat))
  }

  val showIdentifyClient = actionShowStateWhenAuthorised(AsAgent) {
    case _: IdentifyPersonalClient | IdentifyBusinessClient =>
  }

  val submitIdentifyItsaClient = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ItsaClientForm.form(featureFlags.showKfcMtdIt))(
      Transitions.identifiedItsaClient(checkPostcodeMatches)(hasPendingInvitationsFor)(
        relationshipsService.hasActiveRelationshipFor)(featureFlags.enableMtdItToConfirm)(featureFlags.showKfcMtdIt)(
        getClientNameByService)(createMultipleInvitations)(createAgentLink)
    )
  }

  val submitIdentifyVatClient = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(VatClientForm.form(featureFlags.showKfcMtdVat))(
      Transitions.identifiedVatClient(checkVatRegistrationDateMatches)(hasPendingInvitationsFor)(
        relationshipsService.hasActiveRelationshipFor)(featureFlags.enableMtdVatToConfirm)(featureFlags.showKfcMtdVat)(
        getClientNameByService)(createMultipleInvitations)(createAgentLink)
    )
  }

  val submitIdentifyIrvClient = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(IrvClientForm.form(featureFlags.showKfcPersonalIncome))(
      Transitions.identifiedIrvClient(checkCitizenRecordMatches)(hasPendingInvitationsFor)(
        relationshipsService.hasActiveRelationshipFor)(featureFlags.enableIrvToConfirm)(
        featureFlags.showKfcPersonalIncome)(getClientNameByService)(createMultipleInvitations)(createAgentLink)
    )
  }

  val showConfirmClient = actionShowStateWhenAuthorised(AsAgent) {
    case _: ConfirmClientItsa        =>
    case _: ConfirmClientPersonalVat =>
    case _: ConfirmClientBusinessVat =>
    case _: ConfirmClientIrv         =>
  }

  val submitConfirmClient = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ConfirmClientForm)(
      Transitions.clientConfirmed(createMultipleInvitations)(createAgentLink)(hasPendingInvitationsFor)(
        relationshipsService.hasActiveRelationshipFor)
    )
  }

  val showReviewAuthorisations = actionShowStateWhenAuthorised(AsAgent) {
    case _: ReviewAuthorisationsPersonal =>
  }

  val submitReviewAuthorisations = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(ReviewAuthorisationsForm)(
      Transitions.authorisationsReviewed(createMultipleInvitations)(createAgentLink))
  }

  def showDeleteAuthorisation(itemId: String) = action { implicit request =>
    whenAuthorised(AsAgent)(Transitions.deleteAuthorisationRequest(itemId))(display)
  }

  def submitDeleteAuthorisation = action { implicit request =>
    whenAuthorisedWithForm(AsAgent)(DeleteAuthorisationForm)(Transitions.confirmDeleteAuthorisationRequest)
  }

  val showInvitationSent = actionShowStateWhenAuthorised(AsAgent) {
    case _: InvitationSentPersonal | _: InvitationSentBusiness =>
  }
  val showNotMatched = actionShowStateWhenAuthorised(AsAgent) { case _: KnownFactNotMatched                      => }
  val showSomeAuthorisationsFailed = actionShowStateWhenAuthorised(AsAgent) { case _: SomeAuthorisationsFailed   => }
  val showAllAuthorisationsFailed = actionShowStateWhenAuthorised(AsAgent) { case _: AllAuthorisationsFailed     => }
  val showClientNotSignedUp = actionShowStateWhenAuthorised(AsAgent) { case _: ClientNotSignedUp                 => }
  val showPendingAuthorisationExists = actionShowStateWhenAuthorised(AsAgent) { case _: PendingInvitationExists  => }
  val showActiveAuthorisationExists = actionShowStateWhenAuthorised(AsAgent) { case _: ActiveAuthorisationExists => }
  val showAllAuthorisationsRemoved = actionShowStateWhenAuthorised(AsAgent) { case AllAuthorisationsRemoved      => }

  /* Here we map states to the GET endpoints for redirecting and back linking */
  override def getCallFor(state: State)(implicit request: Request[_]): Call = state match {
    case SelectClientType(_)             => routes.AgentInvitationJourneyController.showClientType()
    case SelectPersonalService(_, _)     => routes.AgentInvitationJourneyController.showSelectService()
    case SelectBusinessService           => routes.AgentInvitationJourneyController.showSelectService()
    case IdentifyPersonalClient(_, _)    => routes.AgentInvitationJourneyController.showIdentifyClient()
    case IdentifyBusinessClient          => routes.AgentInvitationJourneyController.showIdentifyClient()
    case ConfirmClientItsa(_, _)         => routes.AgentInvitationJourneyController.showConfirmClient()
    case ConfirmClientIrv(_, _)          => routes.AgentInvitationJourneyController.showConfirmClient()
    case ConfirmClientPersonalVat(_, _)  => routes.AgentInvitationJourneyController.showConfirmClient()
    case ConfirmClientBusinessVat(_)     => routes.AgentInvitationJourneyController.showConfirmClient()
    case ReviewAuthorisationsPersonal(_) => routes.AgentInvitationJourneyController.showReviewAuthorisations()
    case DeleteAuthorisationRequestPersonal(authorisationRequest, _) =>
      routes.AgentInvitationJourneyController.showDeleteAuthorisation(authorisationRequest.itemId)
    case InvitationSentPersonal(_, _)       => routes.AgentInvitationJourneyController.showInvitationSent()
    case InvitationSentBusiness(_, _)       => routes.AgentInvitationJourneyController.showInvitationSent()
    case KnownFactNotMatched(_)             => routes.AgentInvitationJourneyController.showNotMatched()
    case SomeAuthorisationsFailed(_)        => routes.AgentInvitationJourneyController.showSomeAuthorisationsFailed()
    case AllAuthorisationsFailed(_)         => routes.AgentInvitationJourneyController.showAllAuthorisationsFailed()
    case ClientNotSignedUp(_, _)            => routes.AgentInvitationJourneyController.showClientNotSignedUp()
    case PendingInvitationExists(_, _)      => routes.AgentInvitationJourneyController.showPendingAuthorisationExists()
    case ActiveAuthorisationExists(_, _, _) => routes.AgentInvitationJourneyController.showActiveAuthorisationExists()
    case AllAuthorisationsRemoved           => routes.AgentInvitationJourneyController.showAllAuthorisationsRemoved()
    case _                                  => throw new Exception(s"Link not found for $state")
  }

  /* Here we decide what to render after state transition */
  override def renderState(state: State, breadcrumbs: List[State], formWithErrors: Option[Form[_]])(
    implicit request: Request[_]): Result = state match {

    case SelectClientType(_) => {
      def backLinkForClientType(implicit request: Request[_]): String =
        breadcrumbs.headOption.fold(s"${externalUrls.agentServicesAccountUrl}/agent-services-account")(
          getCallFor(_).url)

      Ok(
        client_type(
          formWithErrors.or(ClientTypeForm.form),
          ClientTypePageConfig(backLinkForClientType, routes.AgentInvitationJourneyController.submitClientType())
        ))
    }

    case SelectPersonalService(services, basket) =>
      Ok(
        select_service(
          formWithErrors.or(ServiceTypeForm.form),
          SelectServicePageConfig(
            basket.nonEmpty,
            featureFlags,
            services,
            routes.AgentInvitationJourneyController.submitPersonalSelectService(),
            backLinkFor(breadcrumbs).url,
            routes.AgentInvitationJourneyController.showReviewAuthorisations()
          )
        ))

    case SelectBusinessService =>
      Ok(
        business_select_service(
          formWithErrors.or(CommonConfirmationForms.serviceBusinessForm),
          BusinessSelectServicePageConfig(
            basketFlag = false,
            routes.AgentInvitationJourneyController.submitBusinessSelectService(),
            backLinkFor(breadcrumbs).url,
            routes.AgentInvitationJourneyController.showReviewAuthorisations()
          )
        ))

    case IdentifyPersonalClient(Services.HMRCMTDIT, _) =>
      Ok(
        identify_client_itsa(
          formWithErrors.or(ItsaClientForm.form(featureFlags.showKfcMtdIt)),
          featureFlags.showKfcMtdIt,
          routes.AgentInvitationJourneyController.submitIdentifyItsaClient(),
          backLinkFor(breadcrumbs).url
        )
      )

    case IdentifyPersonalClient(Services.HMRCMTDVAT, _) =>
      Ok(
        identify_client_vat(
          formWithErrors.or(VatClientForm.form(featureFlags.showKfcMtdVat)),
          featureFlags.showKfcMtdVat,
          routes.AgentInvitationJourneyController.submitIdentifyVatClient(),
          backLinkFor(breadcrumbs).url
        )
      )

    case IdentifyPersonalClient(Services.HMRCPIR, _) =>
      Ok(
        identify_client_irv(
          formWithErrors.or(IrvClientForm.form(featureFlags.showKfcPersonalIncome)),
          featureFlags.showKfcPersonalIncome,
          routes.AgentInvitationJourneyController.submitIdentifyIrvClient(),
          backLinkFor(breadcrumbs).url
        )
      )

    case IdentifyBusinessClient =>
      Ok(
        identify_client_vat(
          formWithErrors.or(VatClientForm.form(featureFlags.showKfcMtdVat)),
          featureFlags.showKfcMtdVat,
          routes.AgentInvitationJourneyController.submitIdentifyVatClient(),
          backLinkFor(breadcrumbs).url
        )
      )

    case ConfirmClientItsa(authorisationRequest, _) =>
      Ok(
        confirm_client(
          authorisationRequest.clientName,
          formWithErrors.or(ConfirmClientForm),
          backLinkFor(breadcrumbs).url,
          routes.AgentInvitationJourneyController.submitConfirmClient()
        ))

    case ConfirmClientIrv(authorisationRequest, _) =>
      Ok(
        confirm_client(
          authorisationRequest.clientName,
          formWithErrors.or(ConfirmClientForm),
          backLinkFor(breadcrumbs).url,
          routes.AgentInvitationJourneyController.submitConfirmClient()
        ))

    case ConfirmClientPersonalVat(authorisationRequest, _) =>
      Ok(
        confirm_client(
          authorisationRequest.clientName,
          formWithErrors.or(ConfirmClientForm),
          backLinkFor(breadcrumbs).url,
          routes.AgentInvitationJourneyController.submitConfirmClient()
        ))

    case ConfirmClientBusinessVat(authorisationRequest) =>
      Ok(
        confirm_client(
          authorisationRequest.clientName,
          formWithErrors.or(ConfirmClientForm),
          backLinkFor(breadcrumbs).url,
          routes.AgentInvitationJourneyController.submitConfirmClient()
        ))

    case ReviewAuthorisationsPersonal(basket) =>
      Ok(
        review_authorisations(
          ReviewAuthorisationsPageConfig(
            basket,
            featureFlags,
            routes.AgentInvitationJourneyController.submitReviewAuthorisations()),
          formWithErrors.or(ReviewAuthorisationsForm),
          backLinkFor(breadcrumbs).url
        ))

    case DeleteAuthorisationRequestPersonal(authorisationRequest, _) =>
      Ok(
        delete(
          DeletePageConfig(authorisationRequest, routes.AgentInvitationJourneyController.submitDeleteAuthorisation()),
          DeleteAuthorisationForm))

    case InvitationSentPersonal(invitationLink, continueUrl) =>
      Ok(
        invitation_sent(
          InvitationSentPageConfig(
            invitationLink,
            None,
            continueUrl.isDefined,
            featureFlags.enableTrackRequests,
            ClientType.fromEnum(personal),
            inferredExpiryDate)))

    case InvitationSentBusiness(invitationLink, continueUrl) =>
      Ok(
        invitation_sent(
          InvitationSentPageConfig(
            invitationLink,
            None,
            continueUrl.isDefined,
            featureFlags.enableTrackRequests,
            ClientType.fromEnum(business),
            inferredExpiryDate)))

    case KnownFactNotMatched(basket) =>
      Ok(
        not_matched(
          basket.nonEmpty,
          routes.AgentInvitationJourneyController.showIdentifyClient(),
          Some(routes.AgentInvitationJourneyController.showReviewAuthorisations())))

    case SomeAuthorisationsFailed(basket) =>
      Ok(invitation_creation_failed(AllInvitationCreationFailedPageConfig(basket)))

    case AllAuthorisationsFailed(basket) =>
      Ok(invitation_creation_failed(SomeInvitationCreationFailedPageConfig(basket)))

    case ActiveAuthorisationExists(_, service, basket) =>
      Ok(
        active_authorisation_exists(
          basket.nonEmpty,
          service,
          false,
          routes.AgentInvitationJourneyController.showReviewAuthorisations(),
          routes.AgentInvitationJourneyController.showClientType()
        ))

    case PendingInvitationExists(_, basket) =>
      Ok(
        pending_authorisation_exists(
          basket.nonEmpty,
          backLinkFor(breadcrumbs).url,
          fromFastTrack = false,
          routes.AgentInvitationJourneyController.showReviewAuthorisations(),
          routes.AgentInvitationJourneyController.showClientType()
        ))

    case ClientNotSignedUp(service, basket) =>
      Ok(not_signed_up(service, basket.nonEmpty))

    case AllAuthorisationsRemoved =>
      Ok(all_authorisations_removed(routes.AgentInvitationJourneyController.showClientType()))
  }
}

object AgentInvitationJourneyController {

  import uk.gov.hmrc.agentinvitationsfrontend.forms.CommonConfirmationForms._

  val ConfirmClientForm = confirmationForm("error.confirm-client.required")

  val ReviewAuthorisationsForm = confirmationForm("error.review-authorisation.required")

  val DeleteAuthorisationForm = confirmationForm("error.delete.radio")
}
