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
import play.api.data.Forms.{mapping, optional, single, text}
import play.api.mvc.{Action, Call, Request}
import play.api.{Configuration, Environment}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.InvitationsConnector
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ValidateHelper.optionalIf
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.supportedServices
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.agents._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.auth.core.AuthConnector

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

@Singleton
class AgentInvitationJourneyController @Inject()(
  @Named("invitation.expiryDuration") expiryDuration: String,
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  relationshipsService: RelationshipsService,
  auditService: AuditService,
  val env: Environment,
  val authConnector: AuthConnector,
  val continueUrlActions: ContinueUrlActions,
  val withVerifiedPasscode: PasscodeVerification,
  override val journeyService: AgentInvitationJourneyService)(
  implicit configuration: Configuration,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  val messagesApi: play.api.i18n.MessagesApi,
  ec: ExecutionContext)
    extends JourneyController {

  import AgentInvitationJourneyController._
  import OptionalFormOps._
  import journeyService.model.States._
  import journeyService.model.{State, Transitions}
  import invitationsService.{checkCitizenRecordMatches, checkPostcodeMatches, checkVatRegistrationDateMatches, createAgentLink, createMultipleInvitations, getClientNameByService, hasPendingInvitationsFor}

  private val invitationExpiryDuration = Duration(expiryDuration.replace('_', ' '))
  private val inferredExpiryDate = LocalDate.now().plusDays(invitationExpiryDuration.toDays.toInt)

  val AsAgent: WithAuthorised[AuthorisedAgent] = { implicit request: Request[Any] =>
    withAuthorisedAsAgent(_)
  }

  /* Here we decide how to handle HTTP request and transition the state of the journey */
  val agentsRoot = Action(Redirect(routes.AgentInvitationJourneyController.showClientType()))

  val showClientType = action { implicit request =>
    authorised(AsAgent)(Transitions.showSelectClientType)(display)
  }

  val submitClientType = action { implicit request =>
    authorisedWithForm(AsAgent)(SelectClientTypeForm)(Transitions.selectedClientType)
  }

  val showSelectService = authorisedShowCurrentStateWhen(AsAgent) {
    case _: SelectPersonalService | SelectBusinessService =>
  }

  val submitPersonalSelectService = action { implicit request =>
    authorisedWithForm(AsAgent)(SelectPersonalServiceForm)(
      Transitions.selectedPersonalService(
        featureFlags.showHmrcMtdIt,
        featureFlags.showPersonalIncome,
        featureFlags.showHmrcMtdVat))
  }

  val submitBusinessSelectService = action { implicit request =>
    authorisedWithForm(AsAgent)(SelectBusinessServiceForm)(
      Transitions.selectedBusinessService(featureFlags.showHmrcMtdVat))
  }

  val showIdentifyClient = authorisedShowCurrentStateWhen(AsAgent) {
    case _: IdentifyPersonalClient | IdentifyBusinessClient =>
  }

  val submitIdentifyItsaClient = action { implicit request =>
    authorisedWithForm(AsAgent)(IdentifyItsaClientForm(featureFlags.showKfcMtdIt))(
      Transitions.identifiedItsaClient(checkPostcodeMatches)(hasPendingInvitationsFor)(
        relationshipsService.hasActiveRelationshipFor)(featureFlags.enableMtdItToConfirm)(featureFlags.showKfcMtdIt)(
        getClientNameByService)(createMultipleInvitations)(createAgentLink)
    )
  }

  val submitIdentifyVatClient = action { implicit request =>
    authorisedWithForm(AsAgent)(IdentifyVatClientForm(featureFlags.showKfcMtdVat))(
      Transitions.identifiedVatClient(checkVatRegistrationDateMatches)(hasPendingInvitationsFor)(
        relationshipsService.hasActiveRelationshipFor)(featureFlags.enableMtdVatToConfirm)(featureFlags.showKfcMtdVat)(
        getClientNameByService)(createMultipleInvitations)(createAgentLink)
    )
  }

  val submitIdentifyIrvClient = action { implicit request =>
    authorisedWithForm(AsAgent)(IdentifyIrvClientForm(featureFlags.showKfcPersonalIncome))(
      Transitions.identifiedIrvClient(checkCitizenRecordMatches)(hasPendingInvitationsFor)(
        relationshipsService.hasActiveRelationshipFor)(featureFlags.enableIrvToConfirm)(
        featureFlags.showKfcPersonalIncome)(getClientNameByService)(createMultipleInvitations)(createAgentLink)
    )
  }

  val showConfirmClient = authorisedShowCurrentStateWhen(AsAgent) {
    case _: ConfirmClientItsa        =>
    case _: ConfirmClientPersonalVat =>
    case _: ConfirmClientBusinessVat =>
    case _: ConfirmClientIrv         =>
  }

  val submitConfirmClient = action { implicit request =>
    authorisedWithForm(AsAgent)(ConfirmClientForm)(
      Transitions.clientConfirmed(createMultipleInvitations)(createAgentLink)(hasPendingInvitationsFor)(
        relationshipsService.hasActiveRelationshipFor)
    )
  }

  val showReviewAuthorisations = authorisedShowCurrentStateWhen(AsAgent) {
    case _: ReviewAuthorisationsPersonal =>
  }

  val submitReviewAuthorisations = action { implicit request =>
    authorisedWithForm(AsAgent)(ReviewAuthorisationsForm)(
      Transitions.authorisationsReviewed(createMultipleInvitations)(createAgentLink))
  }

  def showDeleteAuthorisation(itemId: String) = action { implicit request =>
    authorised(AsAgent)(Transitions.deleteAuthorisationRequest(itemId))(display)
  }

  def submitDeleteAuthorisation = action { implicit request =>
    authorisedWithForm(AsAgent)(DeleteAuthorisationForm)(Transitions.confirmDeleteAuthorisationRequest)
  }

  val showInvitationSent = authorisedShowCurrentStateWhen(AsAgent) {
    case _: InvitationSentPersonal | _: InvitationSentBusiness =>
  }
  val showNotMatched = authorisedShowCurrentStateWhen(AsAgent) { case _: KnownFactNotMatched                      => }
  val showSomeAuthorisationsFailed = authorisedShowCurrentStateWhen(AsAgent) { case _: SomeAuthorisationsFailed   => }
  val showAllAuthorisationsFailed = authorisedShowCurrentStateWhen(AsAgent) { case _: AllAuthorisationsFailed     => }
  val showClientNotSignedUp = authorisedShowCurrentStateWhen(AsAgent) { case _: ClientNotSignedUp                 => }
  val showPendingAuthorisationExists = authorisedShowCurrentStateWhen(AsAgent) { case _: PendingInvitationExists  => }
  val showActiveAuthorisationExists = authorisedShowCurrentStateWhen(AsAgent) { case _: ActiveAuthorisationExists => }
  val showAllAuthorisationsRemoved = authorisedShowCurrentStateWhen(AsAgent) { case AllAuthorisationsRemoved      => }

  /* Here we map states to the GET endpoints for redirecting and back linking */
  override def getCallFor(state: State): Call = state match {
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

  private def backLinkFor(breadcrumbs: List[State]): String =
    breadcrumbs.headOption.map(getCallFor).getOrElse(routes.AgentInvitationJourneyController.showClientType()).url

  /* Here we decide what to render after state transition */
  override def renderState(state: State, breadcrumbs: List[State], formWithErrors: Option[Form[_]]): Route = {
    implicit request =>
      state match {
        case SelectClientType(_) =>
          Ok(client_type(
            formWithErrors.or(SelectClientTypeForm),
            ClientTypePageConfig(backLinkFor(breadcrumbs), routes.AgentInvitationJourneyController.submitClientType())))

        case SelectPersonalService(services, basket) =>
          Ok(
            select_service(
              formWithErrors.or(SelectPersonalServiceForm),
              SelectServicePageConfig(
                basket.nonEmpty,
                featureFlags,
                services,
                routes.AgentInvitationJourneyController.submitPersonalSelectService(),
                backLinkFor(breadcrumbs),
                routes.AgentInvitationJourneyController.showReviewAuthorisations()
              )
            ))

        case SelectBusinessService =>
          Ok(
            business_select_service(
              formWithErrors.or(SelectBusinessServiceForm),
              BusinessSelectServicePageConfig(
                basketFlag = false,
                routes.AgentInvitationJourneyController.submitBusinessSelectService(),
                backLinkFor(breadcrumbs),
                routes.AgentInvitationJourneyController.showReviewAuthorisations()
              )
            ))

        case IdentifyPersonalClient(Services.HMRCMTDIT, _) =>
          Ok(
            identify_client_itsa(
              formWithErrors.or(IdentifyItsaClientForm(featureFlags.showKfcMtdIt)),
              featureFlags.showKfcMtdIt,
              routes.AgentInvitationJourneyController.submitIdentifyItsaClient(),
              backLinkFor(breadcrumbs)
            )
          )

        case IdentifyPersonalClient(Services.HMRCMTDVAT, _) =>
          Ok(
            identify_client_vat(
              formWithErrors.or(IdentifyVatClientForm(featureFlags.showKfcMtdVat)),
              featureFlags.showKfcMtdVat,
              routes.AgentInvitationJourneyController.submitIdentifyVatClient(),
              backLinkFor(breadcrumbs)
            )
          )

        case IdentifyPersonalClient(Services.HMRCPIR, _) =>
          Ok(
            identify_client_irv(
              formWithErrors.or(IdentifyIrvClientForm(featureFlags.showKfcPersonalIncome)),
              featureFlags.showKfcPersonalIncome,
              routes.AgentInvitationJourneyController.submitIdentifyIrvClient(),
              backLinkFor(breadcrumbs)
            )
          )

        case IdentifyBusinessClient =>
          Ok(
            identify_client_vat(
              formWithErrors.or(IdentifyVatClientForm(featureFlags.showKfcMtdVat)),
              featureFlags.showKfcMtdVat,
              routes.AgentInvitationJourneyController.submitIdentifyVatClient(),
              backLinkFor(breadcrumbs)
            )
          )

        case ConfirmClientItsa(authorisationRequest, _) =>
          Ok(
            confirm_client(
              authorisationRequest.clientName,
              formWithErrors.or(ConfirmClientForm),
              backLinkFor(breadcrumbs),
              routes.AgentInvitationJourneyController.submitConfirmClient()
            ))

        case ConfirmClientIrv(authorisationRequest, _) =>
          Ok(
            confirm_client(
              authorisationRequest.clientName,
              formWithErrors.or(ConfirmClientForm),
              backLinkFor(breadcrumbs),
              routes.AgentInvitationJourneyController.submitConfirmClient()
            ))

        case ConfirmClientPersonalVat(authorisationRequest, _) =>
          Ok(
            confirm_client(
              authorisationRequest.clientName,
              formWithErrors.or(ConfirmClientForm),
              backLinkFor(breadcrumbs),
              routes.AgentInvitationJourneyController.submitConfirmClient()
            ))

        case ConfirmClientBusinessVat(authorisationRequest) =>
          Ok(
            confirm_client(
              authorisationRequest.clientName,
              formWithErrors.or(ConfirmClientForm),
              backLinkFor(breadcrumbs),
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
              backLinkFor(breadcrumbs)
            ))

        case DeleteAuthorisationRequestPersonal(authorisationRequest, _) =>
          Ok(delete(DeletePageConfig(authorisationRequest), DeleteAuthorisationForm))

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
              routes.AgentInvitationJourneyController.showReviewAuthorisations()))

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
              backLinkFor(breadcrumbs),
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
}

object AgentInvitationJourneyController {

  val SelectClientTypeForm: Form[ClientType] = Form(
    single(
      "clientType" -> lowerCaseText.verifying("client.type.invalid", Set("personal", "business").contains _)
    ).transform(ClientType.toEnum, ClientType.fromEnum)
  )

  val SelectPersonalServiceForm: Form[String] = Form(
    single(
      "serviceType" -> text.verifying("service.type.invalid", supportedServices.contains _)
    )
  )

  def confirmationForm(errorMessage: String): Form[Confirmation] =
    Form(
      mapping(
        "accepted" -> optional(normalizedText)
          .transform[String](_.getOrElse(""), s => Some(s))
          .verifying(confirmationChoice(errorMessage))
      )(choice => Confirmation(choice.toBoolean))(confirmation => Some(confirmation.choice.toString)))

  val SelectBusinessServiceForm = confirmationForm("error.business-service.required")

  def IdentifyItsaClientForm(showKfcMtdIt: Boolean): Form[ItsaClient] = Form(
    mapping(
      "clientIdentifier" -> normalizedText.verifying(validNino()),
      "postcode"         -> postcodeMapping(showKfcMtdIt)
    )(ItsaClient.apply)(ItsaClient.unapply)
  )

  def IdentifyVatClientForm(showKfcMtdVat: Boolean): Form[VatClient] = Form(
    mapping(
      "clientIdentifier" -> normalizedText.verifying(validVrn),
      "registrationDate" -> optionalIf(showKfcMtdVat, DateFieldHelper.dateFieldsMapping(validVatDateFormat))
    )(VatClient.apply)(VatClient.unapply)
  )

  def IdentifyIrvClientForm(showKfcPersonalIncome: Boolean): Form[IrvClient] = Form(
    mapping(
      "clientIdentifier" -> normalizedText.verifying(validNino()),
      "dob"              -> dateOfBirthMapping(showKfcPersonalIncome)
    )(IrvClient.apply)(IrvClient.unapply)
  )

  val ConfirmClientForm = confirmationForm("error.confirm-client.required")

  val ReviewAuthorisationsForm = confirmationForm("error.review-authorisation.required")

  val DeleteAuthorisationForm = confirmationForm("error.delete.radio")
}
