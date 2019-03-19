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
import play.api.data.Forms.{mapping, optional, single, text}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.data.{Form, Mapping}
import play.api.i18n.I18nSupport
import play.api.mvc.{Call, Request, Result}
import play.api.{Configuration, Environment}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.InvitationsConnector
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ValidateHelper.optionalIf
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.business
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.agents._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentmtdidentifiers.model.Vrn
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

@Singleton
class AgentInvitationFastTrackJourneyController @Inject()(
  @Named("invitation.expiryDuration") expiryDuration: String,
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  relationshipsService: RelationshipsService,
  auditService: AuditService,
  val env: Environment,
  val authConnector: AuthConnector,
  val continueUrlActions: ContinueUrlActions,
  val withVerifiedPasscode: PasscodeVerification,
  override val journeyService: AgentInvitationFastTrackJourneyService)(
  implicit configuration: Configuration,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  val messagesApi: play.api.i18n.MessagesApi,
  ec: ExecutionContext)
    extends FrontendController with JourneyController with I18nSupport with AuthActions {

  import AgentInvitationFastTrackJourneyController._
  import OptionalFormOps._
  import journeyService.model.States._
  import journeyService.model.{State, Transitions}

  import invitationsService.{checkCitizenRecordMatches, checkPostcodeMatches, checkVatRegistrationDateMatches, createAgentLink, createInvitation, hasPendingInvitationsFor}
  import relationshipsService.hasActiveRelationshipFor

  private val invitationExpiryDuration = Duration(expiryDuration.replace('_', ' '))
  private val inferredExpiryDate = LocalDate.now().plusDays(invitationExpiryDuration.toDays.toInt)

  val AsAgent: WithAuthorised[AuthorisedAgent] = { implicit request: Request[Any] =>
    withAuthorisedAsAgent(_)
  }

  /* Here we decide how to handle HTTP request and transition the state of the journey */

  val agentFastTrack =
    action { implicit request =>
      authorisedWithBootstrapAndForm(Transitions.prologue(continueUrlActions.getErrorUrl.map(_.url)))(AsAgent)(
        agentFastTrackForm)(Transitions.start(featureFlags)(continueUrlActions.getErrorUrl.map(_.url)))
    }

  val showCheckDetails = authorisedShowCurrentStateWhen(AsAgent) {
    case _: CheckDetailsCompleteItsa        =>
    case _: CheckDetailsCompleteIrv         =>
    case _: CheckDetailsCompletePersonalVat =>
    case _: CheckDetailsCompleteBusinessVat =>
    case _: CheckDetailsNoPostcode          =>
    case _: CheckDetailsNoDob               =>
    case _: CheckDetailsNoVatRegDate        =>
    case _: CheckDetailsNoClientTypeVat     =>
  }

  val submitCheckDetails = action { implicit request =>
    authorisedWithForm(AsAgent)(checkDetailsForm)(
      Transitions.checkedDetailsAllInformation(checkPostcodeMatches)(checkCitizenRecordMatches)(
        checkVatRegistrationDateMatches)(createInvitation)(createAgentLink)(hasPendingInvitationsFor)(
        hasActiveRelationshipFor)(featureFlags))
  }

  val progressToIdentifyClient = action { implicit request =>
    authorised(AsAgent)(Transitions.checkedDetailsChangeInformation)(redirect)
  }

  val showIdentifyClient = authorisedShowCurrentStateWhen(AsAgent) {
    case _: IdentifyPersonalClient =>
    case _: IdentifyBusinessClient =>
  }

  val submitIdentifyItsaClient =
    action { implicit request =>
      authorisedWithForm(AsAgent)(IdentifyItsaClientForm(featureFlags.showKfcMtdIt))(
        Transitions.identifiedClientItsa(checkPostcodeMatches)(checkCitizenRecordMatches)(
          checkVatRegistrationDateMatches)(createInvitation)(createAgentLink)(hasPendingInvitationsFor)(
          hasActiveRelationshipFor)(featureFlags))
    }

  val submitIdentifyIrvClient =
    action { implicit request =>
      authorisedWithForm(AsAgent)(IdentifyIrvClientForm(featureFlags.showKfcPersonalIncome))(
        Transitions.identifiedClientIrv(checkPostcodeMatches)(checkCitizenRecordMatches)(
          checkVatRegistrationDateMatches)(createInvitation)(createAgentLink)(hasPendingInvitationsFor)(
          hasActiveRelationshipFor)(featureFlags))
    }
  val submitIdentifyVatClient =
    action { implicit request =>
      authorisedWithForm(AsAgent)(IdentifyVatClientForm(featureFlags.showKfcMtdVat))(
        Transitions.identifiedClientVat(checkPostcodeMatches)(checkCitizenRecordMatches)(
          checkVatRegistrationDateMatches)(createInvitation)(createAgentLink)(hasPendingInvitationsFor)(
          hasActiveRelationshipFor)(featureFlags))
    }

  val progressToKnownFact = action { implicit request =>
    authorised(AsAgent)(Transitions.checkedDetailsNoKnownFact)(redirect)
  }

  val showKnownFact = authorisedShowCurrentStateWhen(AsAgent) {
    case _: NoPostcode | _: NoDob | _: NoVatRegDate =>
  }

  val submitKnownFactItsa =
    action { implicit request =>
      authorisedWithForm(AsAgent)(agentFastTrackPostcodeForm(featureFlags.showKfcMtdIt))(
        Transitions.moreDetailsItsa(checkPostcodeMatches)(checkCitizenRecordMatches)(checkVatRegistrationDateMatches)(
          createInvitation)(createAgentLink)(hasPendingInvitationsFor)(hasActiveRelationshipFor)(featureFlags))
    }
  val submitKnownFactIrv =
    action { implicit request =>
      authorisedWithForm(AsAgent)(agentFastTrackDateOfBirthForm(featureFlags.showKfcPersonalIncome))(
        Transitions.moreDetailsIrv(checkPostcodeMatches)(checkCitizenRecordMatches)(checkVatRegistrationDateMatches)(
          createInvitation)(createAgentLink)(hasPendingInvitationsFor)(hasActiveRelationshipFor)(featureFlags))
    }
  val submitKnownFactVat = action { implicit request =>
    authorisedWithForm(AsAgent)(agentFastTrackVatRegDateForm(featureFlags))(
      Transitions.moreDetailsVat(checkPostcodeMatches)(checkCitizenRecordMatches)(checkVatRegistrationDateMatches)(
        createInvitation)(createAgentLink)(hasPendingInvitationsFor)(hasActiveRelationshipFor)(featureFlags))
  }

  val progressToClientType = action { implicit request =>
    authorised(AsAgent)(Transitions.checkedDetailsNoClientType)(redirect)
  }

  val showClientType = authorisedShowCurrentStateWhen(AsAgent) {
    case _: SelectClientTypeVat =>
  }

  val submitClientType = action { implicit request =>
    authorisedWithForm(AsAgent)(SelectClientTypeForm)(
      Transitions.selectedClientType(checkPostcodeMatches)(checkCitizenRecordMatches)(checkVatRegistrationDateMatches)(
        createInvitation)(createAgentLink)(hasPendingInvitationsFor)(hasActiveRelationshipFor)(featureFlags))
  }

  val showInvitationSent = authorisedShowCurrentStateWhen(AsAgent) {
    case _: InvitationSentPersonal | _: InvitationSentBusiness =>
  }

  val showNotMatched = authorisedShowCurrentStateWhen(AsAgent) { case _: KnownFactNotMatched                      => }
  val showClientNotSignedUp = authorisedShowCurrentStateWhen(AsAgent) { case _: ClientNotSignedUp                 => }
  val showPendingAuthorisationExists = authorisedShowCurrentStateWhen(AsAgent) { case _: PendingInvitationExists  => }
  val showActiveAuthorisationExists = authorisedShowCurrentStateWhen(AsAgent) { case _: ActiveAuthorisationExists => }

  /* Here we map states to the GET endpoints for redirecting and back linking */
  override def getCallFor(state: State): Call = state match {
    case Prologue(_)                           => routes.AgentInvitationFastTrackJourneyController.showClientType()
    case SelectClientTypeVat(_, _)             => routes.AgentInvitationFastTrackJourneyController.showClientType()
    case NoPostcode(_, _)                      => routes.AgentInvitationFastTrackJourneyController.showKnownFact()
    case NoDob(_, _)                           => routes.AgentInvitationFastTrackJourneyController.showKnownFact()
    case NoVatRegDate(_, _)                    => routes.AgentInvitationFastTrackJourneyController.showKnownFact()
    case CheckDetailsCompleteItsa(_, _)        => routes.AgentInvitationFastTrackJourneyController.showCheckDetails()
    case CheckDetailsCompleteIrv(_, _)         => routes.AgentInvitationFastTrackJourneyController.showCheckDetails()
    case CheckDetailsCompletePersonalVat(_, _) => routes.AgentInvitationFastTrackJourneyController.showCheckDetails()
    case CheckDetailsCompleteBusinessVat(_, _) => routes.AgentInvitationFastTrackJourneyController.showCheckDetails()
    case CheckDetailsNoPostcode(_, _)          => routes.AgentInvitationFastTrackJourneyController.showCheckDetails()
    case CheckDetailsNoDob(_, _)               => routes.AgentInvitationFastTrackJourneyController.showCheckDetails()
    case CheckDetailsNoVatRegDate(_, _)        => routes.AgentInvitationFastTrackJourneyController.showCheckDetails()
    case CheckDetailsNoClientTypeVat(_, _)     => routes.AgentInvitationFastTrackJourneyController.showCheckDetails()
    case IdentifyPersonalClient(_, _)          => routes.AgentInvitationFastTrackJourneyController.showIdentifyClient()
    case IdentifyBusinessClient(_, _)          => routes.AgentInvitationFastTrackJourneyController.showIdentifyClient()
    case InvitationSentPersonal(_, _)          => routes.AgentInvitationFastTrackJourneyController.showInvitationSent()
    case InvitationSentBusiness(_, _)          => routes.AgentInvitationFastTrackJourneyController.showInvitationSent()
    case KnownFactNotMatched(_, _)             => routes.AgentInvitationFastTrackJourneyController.showNotMatched()
    case ClientNotSignedUp(_, _)               => routes.AgentInvitationFastTrackJourneyController.showClientNotSignedUp()
    case PendingInvitationExists(_, _) =>
      routes.AgentInvitationFastTrackJourneyController.showPendingAuthorisationExists()
    case ActiveAuthorisationExists(_, _) =>
      routes.AgentInvitationFastTrackJourneyController.showActiveAuthorisationExists()
    case _ => throw new Exception(s"Link not found for $state")
  }

  private def backLinkFor(breadcrumbs: List[State]): String =
    breadcrumbs.headOption.map(getCallFor).getOrElse(routes.AgentInvitationJourneyController.showClientType()).url

  private def gotoCheckDetailsWithRequest(fastTrackRequest: AgentFastTrackRequest)(
    implicit request: Request[_]): Result =
    Ok(
      check_details(
        checkDetailsForm,
        CheckDetailsPageConfig(
          fastTrackRequest,
          featureFlags,
          routes.AgentInvitationFastTrackJourneyController.progressToClientType(),
          routes.AgentInvitationFastTrackJourneyController.progressToKnownFact(),
          routes.AgentInvitationFastTrackJourneyController.progressToIdentifyClient(),
          routes.AgentInvitationFastTrackJourneyController.submitCheckDetails()
        )
      ))

  /* Here we decide what to render after state transition */
  override def renderState(state: State, breadcrumbs: List[State], formWithErrors: Option[Form[_]]): Route = {
    implicit request =>
      state match {
        case Prologue(failureUrl) =>
          failureUrl match {
            case Some(url) =>
              Redirect(url + s"?issue=${formWithErrors.get.errorsAsJson.as[FastTrackErrors].formErrorsMessages}")
            case None => throw new Exception("no error url found")
          }

        case CheckDetailsCompleteItsa(fastTrackRequest, _) => gotoCheckDetailsWithRequest(fastTrackRequest)

        case CheckDetailsCompleteIrv(fastTrackRequest, _) => gotoCheckDetailsWithRequest(fastTrackRequest)

        case CheckDetailsCompletePersonalVat(fastTrackRequest, _) => gotoCheckDetailsWithRequest(fastTrackRequest)

        case CheckDetailsCompleteBusinessVat(fastTrackRequest, _) => gotoCheckDetailsWithRequest(fastTrackRequest)

        case CheckDetailsNoPostcode(fastTrackRequest, _) => gotoCheckDetailsWithRequest(fastTrackRequest)

        case CheckDetailsNoDob(fastTrackRequest, _) => gotoCheckDetailsWithRequest(fastTrackRequest)

        case CheckDetailsNoVatRegDate(fastTrackRequest, _) => gotoCheckDetailsWithRequest(fastTrackRequest)

        case CheckDetailsNoClientTypeVat(fastTrackRequest, _) => gotoCheckDetailsWithRequest(fastTrackRequest)

        case NoPostcode(fastTrackRequest, _) =>
          Ok(
            known_fact(
              getKnownFactFormForService(fastTrackRequest.service, featureFlags),
              KnownFactPageConfig(
                fastTrackRequest.service,
                Services.determineServiceMessageKeyFromService(fastTrackRequest.service),
                getSubmitKFFor(fastTrackRequest.service),
                backLinkFor(breadcrumbs)
              )
            ))

        case NoDob(fastTrackRequest, _) =>
          Ok(
            known_fact(
              getKnownFactFormForService(fastTrackRequest.service, featureFlags),
              KnownFactPageConfig(
                fastTrackRequest.service,
                Services.determineServiceMessageKeyFromService(fastTrackRequest.service),
                getSubmitKFFor(fastTrackRequest.service),
                backLinkFor(breadcrumbs)
              )
            ))

        case NoVatRegDate(fastTrackRequest, _) =>
          Ok(
            known_fact(
              getKnownFactFormForService(fastTrackRequest.service, featureFlags),
              KnownFactPageConfig(
                fastTrackRequest.service,
                Services.determineServiceMessageKeyFromService(fastTrackRequest.service),
                getSubmitKFFor(fastTrackRequest.service),
                backLinkFor(breadcrumbs)
              )
            ))

        case SelectClientTypeVat(_, _) =>
          Ok(
            client_type(
              formWithErrors.or(SelectClientTypeForm),
              ClientTypePageConfig(
                backLinkFor(breadcrumbs),
                routes.AgentInvitationFastTrackJourneyController.submitClientType())
            ))

        case IdentifyPersonalClient(ftRequest, _) if ftRequest.service == HMRCMTDIT =>
          Ok(
            identify_client_itsa(
              formWithErrors.or(IdentifyItsaClientForm(featureFlags.showKfcMtdIt)),
              featureFlags.showKfcMtdIt,
              routes.AgentInvitationFastTrackJourneyController.submitIdentifyItsaClient(),
              backLinkFor(breadcrumbs)
            )
          )

        case IdentifyPersonalClient(ftRequest, _) if ftRequest.service == HMRCMTDVAT =>
          Ok(
            identify_client_vat(
              formWithErrors.or(IdentifyVatClientForm(featureFlags.showKfcMtdVat)),
              featureFlags.showKfcMtdVat,
              routes.AgentInvitationFastTrackJourneyController.submitIdentifyVatClient(),
              backLinkFor(breadcrumbs)
            )
          )

        case IdentifyPersonalClient(ftRequest, _) if ftRequest.service == HMRCPIR =>
          Ok(
            identify_client_irv(
              formWithErrors.or(IdentifyIrvClientForm(featureFlags.showKfcPersonalIncome)),
              featureFlags.showKfcPersonalIncome,
              routes.AgentInvitationFastTrackJourneyController.submitIdentifyIrvClient(),
              backLinkFor(breadcrumbs)
            )
          )

        case IdentifyBusinessClient(_, _) =>
          Ok(
            identify_client_vat(
              formWithErrors.or(IdentifyVatClientForm(featureFlags.showKfcMtdVat)),
              featureFlags.showKfcMtdVat,
              routes.AgentInvitationFastTrackJourneyController.submitIdentifyVatClient(),
              backLinkFor(breadcrumbs)
            )
          )

        case InvitationSentPersonal(invitationLink, continueUrl) =>
          Ok(
            invitation_sent(
              InvitationSentPageConfig(
                invitationLink,
                None,
                continueUrl.isDefined,
                featureFlags.enableTrackRequests,
                ClientType.fromEnum(business),
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

        case KnownFactNotMatched(_, _) =>
          Ok(
            not_matched(
              hasJourneyCache = false,
              routes.AgentInvitationFastTrackJourneyController.showIdentifyClient(),
              routes.AgentInvitationJourneyController.showReviewAuthorisations()
            ))

        case ActiveAuthorisationExists(_, _) =>
          Ok(
            active_authorisation_exists(
              authRequestsExist = false,
              "",
              fromFastTrack = true,
              routes.AgentInvitationJourneyController.showReviewAuthorisations(),
              routes.AgentInvitationFastTrackJourneyController.showClientType()
            ))

        case PendingInvitationExists(_, _) =>
          Ok(
            pending_authorisation_exists(
              authRequestsExist = false,
              backLinkFor(breadcrumbs),
              fromFastTrack = true,
              routes.AgentInvitationJourneyController.showReviewAuthorisations(),
              routes.AgentInvitationFastTrackJourneyController.showClientType()
            ))

        case ClientNotSignedUp(fastTrackRequest, _) =>
          Ok(
            not_signed_up(
              Services.determineServiceMessageKeyFromService(fastTrackRequest.service),
              hasRequests = false))
      }
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
        case AgentFastTrackRequest(_, HMRCMTDVAT, "vrn", clientId, _) if Vrn.isValid(clientId) => Valid
        case _                                                                                 => Invalid(ValidationError("INVALID_SUBMISSION"))
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
          .verifying("UNSUPPORTED_CLIENT_ID_TYPE", clientType => supportedTypes.contains(clientType)),
        "clientIdentifier" -> normalizedText.verifying(validateClientId),
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

  val SelectClientTypeForm: Form[ClientType] = Form(
    single(
      "clientType" -> lowerCaseText.verifying("client.type.invalid", Set("personal", "business").contains _)
    ).transform(ClientType.toEnum, ClientType.fromEnum)
  )

  def confirmationForm(errorMessage: String): Form[Confirmation] =
    Form(
      mapping(
        "accepted" -> optional(normalizedText)
          .transform[String](_.getOrElse(""), s => Some(s))
          .verifying(confirmationChoice(errorMessage))
      )(choice => Confirmation(choice.toBoolean))(confirmation => Some(confirmation.choice.toString)))

  val checkDetailsForm = confirmationForm("error.confirmDetails.invalid")

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

  def knownFactsForm(knownFactsMapping: Mapping[Option[String]]) =
    Form(single("knownFact" -> knownFactsMapping))

  def agentFastTrackPostcodeForm(showKfcMtdIt: Boolean): Form[Option[String]] =
    knownFactsForm(postcodeMapping(showKfcMtdIt))

  def agentFastTrackDateOfBirthForm(showKfcPersonalIncome: Boolean): Form[Option[String]] =
    knownFactsForm(dateOfBirthMapping(showKfcPersonalIncome))

  def agentFastTrackVatRegDateForm(featureFlags: FeatureFlags): Form[Option[String]] =
    knownFactsForm(vatRegDateMapping(featureFlags))

  private def getKnownFactFormForService(service: String, featureFlags: FeatureFlags) =
    service match {
      case HMRCMTDIT  => agentFastTrackPostcodeForm(featureFlags.showKfcMtdIt)
      case HMRCPIR    => agentFastTrackDateOfBirthForm(featureFlags.showKfcPersonalIncome)
      case HMRCMTDVAT => agentFastTrackVatRegDateForm(featureFlags)
      case p          => throw new Exception(s"invalid service in the cache during fast track journey: $p")
    }

  def getSubmitIdentifyClientFor(service: String) =
    service match {
      case HMRCMTDIT  => routes.AgentInvitationFastTrackJourneyController.submitIdentifyItsaClient()
      case HMRCPIR    => routes.AgentInvitationFastTrackJourneyController.submitIdentifyIrvClient()
      case HMRCMTDVAT => routes.AgentInvitationFastTrackJourneyController.submitIdentifyVatClient()
    }

  def getSubmitKFFor(service: String) =
    service match {
      case HMRCMTDIT  => routes.AgentInvitationFastTrackJourneyController.submitKnownFactItsa()
      case HMRCPIR    => routes.AgentInvitationFastTrackJourneyController.submitKnownFactIrv()
      case HMRCMTDVAT => routes.AgentInvitationFastTrackJourneyController.submitKnownFactVat()
    }

}
