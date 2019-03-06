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
import play.api.data.{Form, Mapping}
import play.api.data.Forms.{mapping, optional, single, text}
import play.api.mvc.Call
import play.api.{Configuration, Environment}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.InvitationsConnector
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsFastTrackInvitationController.{clientTypeFor, knownFactsForm, validateFastTrackForm}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ValidateHelper.optionalIf
import uk.gov.hmrc.agentinvitationsfrontend.controllers._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.business
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.agents._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.auth.core.AuthConnector

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
    extends JourneyController {

  import AgentInvitationFastTrackJourneyController._
  import OptionalFormOps._
  import journeyService.model.States._
  import journeyService.model.{State, Transitions}

  private val invitationExpiryDuration = Duration(expiryDuration.replace('_', ' '))
  private val inferredExpiryDate = LocalDate.now().plusDays(invitationExpiryDuration.toDays.toInt)

  /* Here we decide how to handle HTTP request and transition the state of the journey */

  val agentFastTrack = authorisedAgentActionWithForm(agentFastTrackForm)(Transitions.agentFastTrack)

  val showKnownFact = authorisedAgentActionRenderStateWhen { case _: KnownFactNotMatched => }
  val submitKnownFact = authorisedAgentActionWithForm(getKnownFactFormForService){
    (Transitions.moreDetails)(invitationsService.checkPostcodeMatches)(invitationsService.checkCitizenRecordMatches)(invitationsService.checkVatRegistrationDateMatches)(invitationsService.createInvitation)(invitationsService.createAgentLink)(
      invitationsService.hasPendingInvitationsFor)(relationshipsService.hasActiveRelationshipFor)
  }

  val showCheckDetails = authorisedAgentActionRenderStateWhen { case _: CheckDetails => }
  val submitCheckDetails =

  val showNotMatched = authorisedAgentActionRenderStateWhen { case _: KnownFactNotMatched                      => }
  val showClientNotSignedUp = authorisedAgentActionRenderStateWhen { case _: ClientNotSignedUp                 => }
  val showPendingAuthorisationExists = authorisedAgentActionRenderStateWhen { case _: PendingInvitationExists  => }
  val showActiveAuthorisationExists = authorisedAgentActionRenderStateWhen { case _: ActiveAuthorisationExists => }

  /* Here we map states to the GET endpoints for redirecting and back linking */
  override def getCallFor(state: State): Call = state match {
    case SelectClientType(_)          => routes.AgentInvitationJourneyController.showClientType()
    case IdentifyPersonalClient(_)    => routes.AgentInvitationJourneyController.showIdentifyClient()
    case IdentifyBusinessClient       => routes.AgentInvitationJourneyController.showIdentifyClient()
    case InvitationSentPersonal(_, _) => routes.AgentInvitationJourneyController.showInvitationSent()
    case InvitationSentBusiness(_, _) => routes.AgentInvitationJourneyController.showInvitationSent()
    case KnownFactNotMatched(_)       => routes.AgentInvitationJourneyController.showNotMatched()
    case ClientNotSignedUp(_)         => routes.AgentInvitationJourneyController.showClientNotSignedUp()
    case PendingInvitationExists(_)   => routes.AgentInvitationJourneyController.showPendingAuthorisationExists()
    case ActiveAuthorisationExists(_) => routes.AgentInvitationJourneyController.showActiveAuthorisationExists()
    case _                            => throw new Exception(s"Link not found for $state")
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

        case IdentifyPersonalClient(_) =>
          Ok(
            identify_client_itsa(
              formWithErrors.or(IdentifyItsaClientForm(featureFlags.showKfcMtdIt)),
              featureFlags.showKfcMtdIt,
              routes.AgentInvitationJourneyController.submitIdentifyItsaClient(),
              backLinkFor(breadcrumbs)
            )
          )

        case IdentifyPersonalClient(_) =>
          Ok(
            identify_client_vat(
              formWithErrors.or(IdentifyVatClientForm(featureFlags.showKfcMtdVat)),
              featureFlags.showKfcMtdVat,
              routes.AgentInvitationJourneyController.submitIdentifyVatClient(),
              backLinkFor(breadcrumbs)
            )
          )

        case IdentifyPersonalClient(_) =>
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
              false,
              routes.AgentInvitationJourneyController.showIdentifyClient(),
              routes.AgentInvitationJourneyController.showReviewAuthorisations()))

        case ActiveAuthorisationExists(_) =>
          Ok(
            active_authorisation_exists(
              false,
              "",
              false,
              routes.AgentInvitationJourneyController.showReviewAuthorisations(),
              routes.AgentInvitationJourneyController.showClientType()
            ))

        case PendingInvitationExists(_) =>
          Ok(
            pending_authorisation_exists(
              false,
              backLinkFor(breadcrumbs),
              fromFastTrack = false,
              routes.AgentInvitationJourneyController.showReviewAuthorisations(),
              routes.AgentInvitationJourneyController.showClientType()
            ))

        case ClientNotSignedUp(_) =>
          Ok(not_signed_up(""))
      }
  }
}

object AgentInvitationFastTrackJourneyController {

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
        AgentFastTrackRequest(clientTypeFor(clientType, service), service, clientIdType, clientId, knownFact)
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

}
