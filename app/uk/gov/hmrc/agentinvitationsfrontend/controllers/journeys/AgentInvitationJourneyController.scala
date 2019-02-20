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
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.supportedServices
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.{BusinessSelectServicePageConfig, ClientTypePageConfig, SelectServicePageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.auth.core.AuthConnector

import scala.concurrent.ExecutionContext

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
  import journeyService.model.{Error, Errors, State, Transitions}

  /* Here we decide how to handle HTTP request and transition the state of the journey */
  val agentsRoot = simpleAction(Transitions.startJourney)
  val showClientType = authorisedAgentAction(Transitions.showSelectClientType)
  val submitClientType =
    authorisedAgentActionWithForm(SelectClientTypeForm)(Transitions.selectedClientType)(
      SelectClientTypeFormValidationFailed)
  val showSelectService = authorisedAgentAction(Transitions.showSelectService)
  val submitPersonalSelectService = authorisedAgentActionWithForm(SelectPersonalServiceForm)(
    Transitions.selectedPersonalService)(SelectPersonalServiceFormValidationFailed)
  val submitBusinessSelectService = authorisedAgentActionWithForm(SelectBusinessServiceForm)(
    Transitions.selectedBusinessService)(SelectBusinessServiceFormValidationFailed)
  val showIdentifyClient = authorisedAgentAction(Transitions.showIdentifyClient)
  val submitIdentifyItsaClient = authorisedAgentActionWithForm(IdentifyItsaClientForm(featureFlags.showKfcMtdIt))(
    Transitions.identifiedItsaClient)(IdentifyItsaClientFormValidationFailed)
  val submitIdentifyVatClient = authorisedAgentActionWithForm(IdentifyVatClientForm(featureFlags.showKfcMtdVat))(
    Transitions.identifiedVatClient)(IdentifyVatClientFormValidationFailed)
  val submitIdentifyIrvClient =
    authorisedAgentActionWithForm(IdentifyIrvClientForm(featureFlags.showKfcPersonalIncome))(
      Transitions.identifyIrvClient)(IdentifyIrvClientFormValidationFailed)

  /* Here we handle errors thrown during state transition */
  override def handleError(error: Error): Route = { implicit request =>
    error match {
      case Errors.TransitionNotAllowed(origin, breadcrumbs, _) =>
        renderState(origin, breadcrumbs)(request) // renders current state back
      case Errors.GenericError(ex) => throw ex //delegates to the global ErrorHandler
    }
  }

  /* Here we decide how to render or where to redirect after state transition */
  override def renderState(state: State, breadcrumbs: List[State]): Route = { implicit request =>
    state match {
      case Start => Redirect(routes.AgentInvitationJourneyController.showClientType())
      case SelectClientType =>
        Ok(client_type(SelectClientTypeForm, ClientTypePageConfig(backLinkFor(breadcrumbs))))
      case ClientTypeSelected(ClientType.personal) =>
        Redirect(routes.AgentInvitationJourneyController.showSelectService())
      case ClientTypeSelected(ClientType.business) =>
        Redirect(routes.AgentInvitationJourneyController.showSelectService())
      case SelectPersonalService(basket, services) =>
        Ok(select_service(SelectPersonalServiceForm, SelectServicePageConfig(basket.nonEmpty, featureFlags, services)))
      case SelectBusinessService(basket) =>
        Ok(
          business_select_service(
            SelectBusinessServiceForm,
            BusinessSelectServicePageConfig(
              basket.nonEmpty,
              routes.AgentInvitationJourneyController.submitBusinessSelectService(),
              backLinkFor(breadcrumbs))
          ))
      case PersonalServiceSelected(_, _) =>
        Redirect(routes.AgentInvitationJourneyController.showIdentifyClient())
      case BusinessServiceSelected(_) =>
        Redirect(routes.AgentInvitationJourneyController.showIdentifyClient())
      case IdentifyClient(Services.HMRCMTDIT, _) =>
        Ok(
          identify_client_itsa(
            IdentifyItsaClientForm(featureFlags.showKfcMtdIt),
            featureFlags.showKfcMtdIt,
            routes.AgentInvitationJourneyController.submitIdentifyItsaClient(),
            backLinkFor(breadcrumbs).getOrElse(routes.AgentInvitationJourneyController.showClientType().url)
          ))
      case IdentifyClient(Services.HMRCMTDVAT, _) =>
        Ok(
          identify_client_vat(
            IdentifyVatClientForm(featureFlags.showKfcMtdVat),
            featureFlags.showKfcMtdVat,
            routes.AgentInvitationJourneyController.submitIdentifyVatClient(),
            backLinkFor(breadcrumbs).getOrElse(routes.AgentInvitationJourneyController.showClientType().url)
          ))
      case IdentifyClient(Services.HMRCPIR, _) =>
        Ok(
          identify_client_irv(
            IdentifyIrvClientForm(featureFlags.showKfcPersonalIncome),
            featureFlags.showKfcPersonalIncome,
            routes.AgentInvitationJourneyController.submitIdentifyIrvClient(),
            backLinkFor(breadcrumbs).getOrElse(routes.AgentInvitationJourneyController.showClientType().url)
          ))
      case ItsaIdentifiedClient(Services.HMRCMTDIT, clientId, postcode, _) => ???
      case VatIdentifiedClient(Services.HMRCMTDVAT, clientId, regDate, _)  => ???
      case IrvIdentifiedClient(Services.HMRCPIR, clientId, dob, _)         => ???
    }
  }

  /* Here we handle form validation errors */
  override def handleFormValidationError(error: FormValidationError, breadcrumbs: List[State]): Route = {
    implicit request =>
      error match {
        case SelectClientTypeFormValidationFailed(form) =>
          Ok(client_type(form, ClientTypePageConfig(backLinkFor(breadcrumbs))))
      }
  }

  private def getLinkTo(state: State): Call = state match {
    case Start                         => routes.AgentInvitationJourneyController.agentsRoot()
    case SelectClientType              => routes.AgentInvitationJourneyController.showClientType()
    case SelectPersonalService(_, _)   => routes.AgentInvitationJourneyController.showSelectService()
    case SelectBusinessService(_)      => routes.AgentInvitationJourneyController.showSelectService()
    case PersonalServiceSelected(_, _) => routes.AgentInvitationJourneyController.showSelectService()
    case BusinessServiceSelected(_)    => routes.AgentInvitationJourneyController.showSelectService()
    case IdentifyClient(_, _)          => routes.AgentInvitationJourneyController.showIdentifyClient()
    case _                             => throw new Exception(s"Link not found for $state")
  }

  private def backLinkFor(breadcrumbs: List[State]): Option[String] =
    breadcrumbs.headOption.map(getLinkTo).map(_.url)

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

  val SelectBusinessServiceForm: Form[Confirmation] =
    Form(
      mapping(
        "accepted" -> optional(normalizedText)
          .transform[String](_.getOrElse(""), s => Some(s))
          .verifying(confirmationChoice("error.business-service.required"))
      )(choice => Confirmation(choice.toBoolean))(confirmation => Some(confirmation.choice.toString)))

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

  case class SelectClientTypeFormValidationFailed(form: Form[ClientType]) extends FormValidationError
  case class SelectPersonalServiceFormValidationFailed(form: Form[String]) extends FormValidationError
  case class SelectBusinessServiceFormValidationFailed(form: Form[Confirmation]) extends FormValidationError
  case class IdentifyItsaClientFormValidationFailed(form: Form[ItsaClient]) extends FormValidationError
  case class IdentifyVatClientFormValidationFailed(form: Form[VatClient]) extends FormValidationError
  case class IdentifyIrvClientFormValidationFailed(form: Form[IrvClient]) extends FormValidationError

}
