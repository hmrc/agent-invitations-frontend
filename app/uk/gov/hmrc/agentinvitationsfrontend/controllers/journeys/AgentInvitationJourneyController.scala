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

package uk.gov.hmrc.agentinvitationsfrontend.controllers.journeys

import javax.inject.{Inject, Named, Singleton}
import org.joda.time.LocalDate
import play.api.data.Form
import play.api.data.Forms.{mapping, optional, single, text}
import play.api.mvc.Call
import play.api.{Configuration, Environment}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.InvitationsConnector
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ValidateHelper.optionalIf
import uk.gov.hmrc.agentinvitationsfrontend.controllers._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.supportedServices
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.agents._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.http.HeaderCarrier

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
  import journeyService.model.States._
  import journeyService.model.{State, Transitions}
  import OptionalFormOps._

  private val invitationExpiryDuration = Duration(expiryDuration.replace('_', ' '))
  private val inferredExpiryDate = LocalDate.now().plusDays(invitationExpiryDuration.toDays.toInt)

  /* Here we decide how to handle HTTP re=quest and transition the state of the journey */
  val agentsRoot = simpleAction(Transitions.startJourney)(redirect)

  val showClientType = authorisedAgentAction(Transitions.showSelectClientType)(display)
  val submitClientType = authorisedAgentActionWithForm(SelectClientTypeForm)(Transitions.selectedClientType)

  val showSelectService = authorisedAgentCurrentStateAction(display)

  val submitPersonalSelectService =
    authorisedAgentActionWithForm(SelectPersonalServiceForm)(Transitions.selectedPersonalService)
  val submitBusinessSelectService =
    authorisedAgentActionWithForm(SelectBusinessServiceForm)(Transitions.selectedBusinessService)

  val showIdentifyClient = authorisedAgentCurrentStateAction(display)

  val submitIdentifyItsaClient =
    authorisedAgentActionWithFormWithHC(IdentifyItsaClientForm(featureFlags.showKfcMtdIt)) {
      implicit hc: HeaderCarrier =>
        Transitions.identifiedItsaClient(invitationsService.checkPostcodeMatches)(
          invitationsService.hasPendingInvitationsFor)(relationshipsService.hasActiveRelationshipFor)(
          invitationsService.getClientNameByService)
    }
  val submitIdentifyVatClient = authorisedAgentActionWithFormWithHC(IdentifyVatClientForm(featureFlags.showKfcMtdVat)) {
    implicit hc: HeaderCarrier =>
      Transitions.identifiedVatClient(invitationsService.checkVatRegistrationDateMatches)(
        invitationsService.getClientNameByService)
  }
  val submitIdentifyIrvClient =
    authorisedAgentActionWithFormWithHC(IdentifyIrvClientForm(featureFlags.showKfcPersonalIncome)) { implicit hc =>
      Transitions.identifiedIrvClient(invitationsService.checkCitizenRecordMatches)(
        invitationsService.getClientNameByService)
    }

  val showConfirmClient = authorisedAgentCurrentStateAction(display)

  val submitConfirmClient = authorisedAgentActionWithForm(ConfirmClientForm)(Transitions.clientConfirmed)

  val showReviewAuthorisations = authorisedAgentCurrentStateAction(display)

  val authorisationsReviewed = authorisedAgentActionWithFormWithHCWithRequest(ReviewAuthorisationsForm) {
    implicit hc => implicit request =>
      Transitions.authorisationsReviewed(invitationsService.createMultipleInvitations)(
        invitationsService.createAgentLink)
  }

  val showInvitationSent = authorisedAgentCurrentStateAction(display)
  val showNotMatched = authorisedAgentCurrentStateAction(display)
  val showSomeAuthorisationsFailed = authorisedAgentCurrentStateAction(display)
  val showAllAuthorisationsFailed = authorisedAgentCurrentStateAction(display)

  /* Here we map states to the GET endpoints for redirecting and back linking */
  override def getCallFor(state: State): Call = state match {
    case SelectClientType(_)             => routes.AgentInvitationJourneyController.showClientType()
    case SelectPersonalService(_, _)     => routes.AgentInvitationJourneyController.showSelectService()
    case SelectBusinessService(_)        => routes.AgentInvitationJourneyController.showSelectService()
    case IdentifyPersonalClient(_, _)    => routes.AgentInvitationJourneyController.showIdentifyClient()
    case IdentifyBusinessClient(_)       => routes.AgentInvitationJourneyController.showIdentifyClient()
    case ConfirmClientItsa(_, _)         => routes.AgentInvitationJourneyController.showConfirmClient()
    case ConfirmClientIrv(_, _)          => routes.AgentInvitationJourneyController.showConfirmClient()
    case ConfirmClientPersonalVat(_, _)  => routes.AgentInvitationJourneyController.showConfirmClient()
    case ConfirmClientBusinessVat(_, _)  => routes.AgentInvitationJourneyController.showConfirmClient()
    case ReviewAuthorisationsPersonal(_) => routes.AgentInvitationJourneyController.showReviewAuthorisations()
    case ReviewAuthorisationsBusiness(_) => routes.AgentInvitationJourneyController.showReviewAuthorisations()
    case InvitationSentPersonal(_, _)    => routes.AgentInvitationJourneyController.showInvitationSent()
    case InvitationSentBusiness(_, _)    => routes.AgentInvitationJourneyController.showInvitationSent()
    case KnownFactNotMatched(_)          => routes.AgentInvitationJourneyController.showNotMatched()
    case _                               => throw new Exception(s"Link not found for $state")
  }

  private def backLinkFor(breadcrumbs: List[State]): Option[String] =
    breadcrumbs.headOption.map(getCallFor).map(_.url)

  /* Here we decide how to render or where to redirect after state transition */
  override def renderState(state: State, breadcrumbs: List[State], formWithErrors: Option[Form[_]]): Route = {
    implicit request =>
      state match {
        case SelectClientType(_) =>
          Ok(client_type(formWithErrors.or(SelectClientTypeForm), ClientTypePageConfig(backLinkFor(breadcrumbs))))

        case SelectPersonalService(services, basket) =>
          Ok(
            select_service(
              formWithErrors.or(SelectPersonalServiceForm),
              SelectServicePageConfig(basket.nonEmpty, featureFlags, services)))

        case SelectBusinessService(basket) =>
          Ok(
            business_select_service(
              formWithErrors.or(SelectBusinessServiceForm),
              BusinessSelectServicePageConfig(
                basket.nonEmpty,
                routes.AgentInvitationJourneyController.submitBusinessSelectService(),
                backLinkFor(breadcrumbs))
            ))

        case IdentifyPersonalClient(Services.HMRCMTDIT, _) =>
          Ok(
            identify_client_itsa(
              formWithErrors.or(IdentifyItsaClientForm(featureFlags.showKfcMtdIt)),
              featureFlags.showKfcMtdIt,
              routes.AgentInvitationJourneyController.submitIdentifyItsaClient(),
              backLinkFor(breadcrumbs).getOrElse(routes.AgentInvitationJourneyController.showClientType().url)
            ))

        case IdentifyPersonalClient(Services.HMRCMTDVAT, _) =>
          Ok(
            identify_client_vat(
              formWithErrors.or(IdentifyVatClientForm(featureFlags.showKfcMtdVat)),
              featureFlags.showKfcMtdVat,
              routes.AgentInvitationJourneyController.submitIdentifyVatClient(),
              backLinkFor(breadcrumbs).getOrElse(routes.AgentInvitationJourneyController.showClientType().url)
            ))

        case IdentifyPersonalClient(Services.HMRCPIR, _) =>
          Ok(
            identify_client_irv(
              formWithErrors.or(IdentifyIrvClientForm(featureFlags.showKfcPersonalIncome)),
              featureFlags.showKfcPersonalIncome,
              routes.AgentInvitationJourneyController.submitIdentifyIrvClient(),
              backLinkFor(breadcrumbs).getOrElse(routes.AgentInvitationJourneyController.showClientType().url)
            ))

        case IdentifyBusinessClient(_) =>
          Ok(
            identify_client_vat(
              formWithErrors.or(IdentifyVatClientForm(featureFlags.showKfcMtdVat)),
              featureFlags.showKfcMtdVat,
              routes.AgentInvitationJourneyController.submitIdentifyVatClient(),
              backLinkFor(breadcrumbs).getOrElse(routes.AgentInvitationJourneyController.showClientType().url)
            ))

        case ConfirmClientItsa(clientName, _) =>
          Ok(
            confirm_client(
              clientName,
              formWithErrors.or(ConfirmClientForm),
              backLinkFor(breadcrumbs).getOrElse(routes.AgentInvitationJourneyController.showClientType().url)))

        case ConfirmClientIrv(clientName, _) =>
          Ok(
            confirm_client(
              clientName,
              formWithErrors.or(ConfirmClientForm),
              backLinkFor(breadcrumbs).getOrElse(routes.AgentInvitationJourneyController.showClientType().url)))

        case ConfirmClientPersonalVat(clientName, _) =>
          Ok(
            confirm_client(
              clientName,
              formWithErrors.or(ConfirmClientForm),
              backLinkFor(breadcrumbs).getOrElse(routes.AgentInvitationJourneyController.showClientType().url)))

        case ConfirmClientBusinessVat(clientName, _) =>
          Ok(
            confirm_client(
              clientName,
              formWithErrors.or(ConfirmClientForm),
              backLinkFor(breadcrumbs).getOrElse(routes.AgentInvitationJourneyController.showClientType().url)))

        case ReviewAuthorisationsPersonal(basket) =>
          Ok(
            review_authorisations(
              ReviewAuthorisationsPageConfig(basket, featureFlags),
              formWithErrors.or(ReviewAuthorisationsForm),
              backLinkFor(breadcrumbs).getOrElse(routes.AgentInvitationJourneyController.showClientType().url)
            ))

        case ReviewAuthorisationsBusiness(basket) =>
          Ok(
            review_authorisations(
              ReviewAuthorisationsPageConfig(basket, featureFlags),
              formWithErrors.or(ReviewAuthorisationsForm),
              backLinkFor(breadcrumbs).getOrElse(routes.AgentInvitationJourneyController.showClientType().url)
            ))

        case InvitationSentPersonal(invitationLink, continueUrl) =>
          Ok(
            invitation_sent(
              InvitationSentPageConfig(
                invitationLink,
                continueUrl.isDefined,
                featureFlags.enableTrackRequests,
                ClientType.fromEnum(personal),
                inferredExpiryDate)))

        case InvitationSentBusiness(invitationLink, continueUrl) =>
          Ok(
            invitation_sent(
              InvitationSentPageConfig(
                invitationLink,
                continueUrl.isDefined,
                featureFlags.enableTrackRequests,
                ClientType.fromEnum(business),
                inferredExpiryDate)))

        case KnownFactNotMatched(basket) =>
          Ok(not_matched(basket.nonEmpty))

        case SomeAuthorisationsFailed(basket) =>
          Ok(invitation_creation_failed(AllInvitationCreationFailedPageConfig(basket)))

        case AllAuthorisationsFailed(basket) =>
          Ok(invitation_creation_failed(SomeInvitationCreationFailedPageConfig(basket)))

        case ActiveRelationshipExists(_, _) =>
          NotImplemented

        case PendingInvitationExists(_, _) =>
          NotImplemented
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
}
