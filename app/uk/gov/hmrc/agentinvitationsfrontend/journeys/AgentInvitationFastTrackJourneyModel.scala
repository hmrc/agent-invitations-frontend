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

package uk.gov.hmrc.agentinvitationsfrontend.journeys

import org.joda.time.LocalDate
import play.api.mvc.Request
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{ContinueUrlActions, FeatureFlags}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.States.CheckDetails
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.Transitions.CheckDOBMatches
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object AgentInvitationFastTrackJourneyModel extends JourneyModel {

  sealed trait State

  val root: State = States.Prologue(None)

  /* State should contain only minimal set of data required to proceed */
  object States {
    case class Prologue(failureUrl: Option[String]) extends State
    case class CheckDetails(fastTrackRequest: AgentFastTrackRequest, continueURL: Option[String]) extends State
    case class MoreDetails(fastTrackRequest: AgentFastTrackRequest, continueURL: Option[String]) extends State
    case class SelectClientType(fastTrackRequest: AgentFastTrackRequest, continueURL: Option[String]) extends State
    case class IdentifyPersonalClient(fastTrackRequest: AgentFastTrackRequest, continueURL: Option[String])
        extends State
    case class IdentifyBusinessClient(fastTrackRequest: AgentFastTrackRequest, continueURL: Option[String])
        extends State
    case class PendingInvitationExists(fastTrackRequest: AgentFastTrackRequest, continueURL: Option[String])
        extends State
    case class ActiveAuthorisationExists(fastTrackRequest: AgentFastTrackRequest) extends State
    case class KnownFactNotMatched(fastTrackRequest: AgentFastTrackRequest) extends State
    case class InvitationSentPersonal(invitationLink: String, continueUrl: Option[String]) extends State
    case class InvitationSentBusiness(invitationLink: String, continueUrl: Option[String]) extends State
    case class ClientNotSignedUp(fastTrackRequest: AgentFastTrackRequest) extends State
  }

  object Transitions {
    import Extractors._
    import States._

    type HasPendingInvitations = (Arn, String, String) => Future[Boolean]
    type HasActiveRelationship = (Arn, String, String) => Future[Boolean]
    type GetClientName = (String, String) => Future[Option[String]]
    type CheckPostcodeMatches = (Nino, String) => Future[Option[Boolean]]
    type CheckRegDateMatches = (Vrn, LocalDate) => Future[Option[Int]]
    type GetAgentLink = (Arn, Option[ClientType]) => Future[String]
    type CreateInvitation =
      (Arn, Invitation) => Future[InvitationId]

    def prologue(failureUrl: Option[String]) = Transition {
      case _ => goto(Prologue(failureUrl))
    }

    def start(continueUrl: Option[String])(agent: AuthorisedAgent)(
      fastTrackRequest: AgentFastTrackRequest)(implicit request: Request[Any], hc: HeaderCarrier) = Transition {
      case _ => {
        fastTrackRequest.service match {
          case HMRCMTDIT | HMRCPIR =>
            goto(CheckDetails(fastTrackRequest.copy(clientType = Some(personal)), continueUrl))
          case HMRCMTDVAT =>
            goto(CheckDetails(fastTrackRequest.copy(clientType = Some(personal)), continueUrl))
        }
      }
    }

    private def checkIfPendingOrActiveAndGoto(
      fastTrackRequest: AgentFastTrackRequest,
      arn: Arn,
      invitation: Invitation,
      continueUrl: Option[String])(
      hasPendingInvitationsFor: HasPendingInvitations,
      hasActiveRelationshipFor: HasActiveRelationship,
      createInvitation: CreateInvitation,
      getAgentLink: GetAgentLink): Future[State] =
      for {
        hasPendingInvitations <- hasPendingInvitationsFor(
                                  arn,
                                  fastTrackRequest.clientIdentifier,
                                  fastTrackRequest.service)
        result <- if (hasPendingInvitations) {
                   goto(PendingInvitationExists(fastTrackRequest, continueUrl))
                 } else {
                   hasActiveRelationshipFor(arn, fastTrackRequest.clientIdentifier, fastTrackRequest.service).flatMap {
                     case true => goto(ActiveAuthorisationExists(fastTrackRequest))
                     case false =>
                       for {
                         _              <- createInvitation(arn, invitation)
                         invitationLink <- getAgentLink(arn, fastTrackRequest.clientType)
                         result <- fastTrackRequest.clientType match {
                                    case Some(ClientType.personal) =>
                                      goto(InvitationSentPersonal(invitationLink, continueUrl))
                                    case Some(ClientType.business) =>
                                      goto(InvitationSentBusiness(invitationLink, continueUrl))
                                  }
                       } yield result
                   }
                 }
      } yield result

    def checkedDetails(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      featureFlags: FeatureFlags)(agent: AuthorisedAgent)(confirmation: Confirmation) = Transition {
      case checkDetails @ CheckDetailsItsaWithClientTypeAndKnownFact() if featureFlags.showKfcMtdIt =>
        val ftRequest = checkDetails.fastTrackRequest
        if (confirmation.choice) {
          checkPostcodeMatches(Nino(checkDetails.fastTrackRequest.clientIdentifier), ftRequest.knownFact.getOrElse(""))
            .flatMap {
              case Some(true) =>
                checkIfPendingOrActiveAndGoto(
                  ftRequest,
                  agent.arn,
                  ItsaInvitation(Nino(ftRequest.clientIdentifier), ftRequest.knownFact.map(Postcode.apply)),
                  checkDetails.continueURL
                )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink)
              case Some(false) => goto(KnownFactNotMatched(ftRequest))
              case None        => goto(ClientNotSignedUp(ftRequest))
            }
        } else goto(IdentifyPersonalClient(ftRequest, checkDetails.continueURL))

      case checkDetails @ CheckDetailsItsaWithClientType() if !featureFlags.showKfcMtdIt =>
        val ftRequest = checkDetails.fastTrackRequest
        if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(
            ftRequest,
            agent.arn,
            ItsaInvitation(Nino(ftRequest.clientIdentifier), ftRequest.knownFact.map(Postcode.apply)),
            checkDetails.continueURL
          )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink)
        } else goto(IdentifyPersonalClient(ftRequest, checkDetails.continueURL))

      case checkDetails @ CheckDetailsIrvWithClientTypeAndKnownFact() if featureFlags.showKfcPersonalIncome =>
        val ftRequest = checkDetails.fastTrackRequest
        if (confirmation.choice) {
          checkDobMatches(Nino(ftRequest.clientIdentifier), LocalDate.parse(ftRequest.knownFact.getOrElse("")))
            .flatMap {
              case Some(true) =>
                checkIfPendingOrActiveAndGoto(
                  ftRequest,
                  agent.arn,
                  PirInvitation(Nino(ftRequest.clientIdentifier), ftRequest.knownFact.map(DOB.apply)),
                  checkDetails.continueURL
                )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink)
              case Some(false) => goto(KnownFactNotMatched(ftRequest))
              case None        => goto(ClientNotSignedUp(ftRequest))
            }
        } else goto(IdentifyPersonalClient(ftRequest, checkDetails.continueURL))

      case checkDetails @ CheckDetailsIrvWithClientType() if !featureFlags.showKfcPersonalIncome =>
        val ftRequest = checkDetails.fastTrackRequest
        if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(
            ftRequest,
            agent.arn,
            PirInvitation(Nino(ftRequest.clientIdentifier), ftRequest.knownFact.map(DOB.apply)),
            checkDetails.continueURL
          )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink)
        } else goto(IdentifyPersonalClient(ftRequest, checkDetails.continueURL))

      case checkDetails @ CheckDetailsPersonalVatWithClientTypeAndKnownFact() if featureFlags.showKfcMtdVat =>
        val ftRequest = checkDetails.fastTrackRequest
        if (confirmation.choice) {
          checkRegDateMatches(Vrn(ftRequest.clientIdentifier), LocalDate.parse(ftRequest.knownFact.getOrElse("")))
            .flatMap {
              case Some(204) =>
                checkIfPendingOrActiveAndGoto(
                  ftRequest,
                  agent.arn,
                  VatInvitation(
                    Some(personal),
                    Vrn(ftRequest.clientIdentifier),
                    ftRequest.knownFact.map(VatRegDate.apply)),
                  checkDetails.continueURL
                )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink)
              case Some(_) => goto(KnownFactNotMatched(ftRequest))
              case None    => goto(ClientNotSignedUp(ftRequest))
            }
        } else goto(IdentifyPersonalClient(ftRequest, checkDetails.continueURL))

      case checkDetails @ CheckDetailsPersonalVatWithClientType() if !featureFlags.showKfcMtdVat =>
        val ftRequest = checkDetails.fastTrackRequest
        if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(
            ftRequest,
            agent.arn,
            VatInvitation(Some(personal), Vrn(ftRequest.clientIdentifier), ftRequest.knownFact.map(VatRegDate.apply)),
            checkDetails.continueURL
          )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink)
        } else goto(IdentifyPersonalClient(ftRequest, checkDetails.continueURL))

      case checkDetails @ CheckDetailsBusinessVatWithClientTypeAndKnownFact() if featureFlags.showKfcMtdVat =>
        val ftRequest = checkDetails.fastTrackRequest
        if (confirmation.choice) {
          checkRegDateMatches(Vrn(ftRequest.clientIdentifier), LocalDate.parse(ftRequest.knownFact.getOrElse("")))
            .flatMap {
              case Some(204) =>
                checkIfPendingOrActiveAndGoto(
                  ftRequest,
                  agent.arn,
                  VatInvitation(
                    Some(ClientType.business),
                    Vrn(ftRequest.clientIdentifier),
                    ftRequest.knownFact.map(VatRegDate.apply)),
                  checkDetails.continueURL
                )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink)
              case Some(_) => goto(KnownFactNotMatched(ftRequest))
              case None    => goto(ClientNotSignedUp(ftRequest))
            }
        } else goto(IdentifyBusinessClient(ftRequest, checkDetails.continueURL))

      case checkDetails @ CheckDetailsBusinessVatWithClientType() if !featureFlags.showKfcMtdVat =>
        val ftRequest = checkDetails.fastTrackRequest
        if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(
            ftRequest,
            agent.arn,
            VatInvitation(
              Some(ClientType.business),
              Vrn(ftRequest.clientIdentifier),
              ftRequest.knownFact.map(VatRegDate.apply)),
            checkDetails.continueURL
          )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink)
        } else goto(IdentifyBusinessClient(ftRequest, checkDetails.continueURL))

      case checkDetails @ CheckDetailsNoKnownFact() =>
        goto(MoreDetails(checkDetails.fastTrackRequest, checkDetails.continueURL))

      case checkDetails @ CheckDetailsNoClientType() =>
        goto(SelectClientType(checkDetails.fastTrackRequest, checkDetails.continueURL))
    }

    def identifiedClientItsa(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      featureFlags: FeatureFlags)(agent: AuthorisedAgent)(itsaClient: ItsaClient) = Transition {
      case IdentifyPersonalClient(ftRequest, continueURL) if featureFlags.showKfcMtdIt =>
        val newState = CheckDetails(
          ftRequest.copy(clientIdentifier = itsaClient.clientIdentifier, knownFact = itsaClient.postcode),
          continueURL)
        checkedDetails(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(
          hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(Confirmation(true)).apply(newState)
    }

    def identifiedClientIrv(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      featureFlags: FeatureFlags)(agent: AuthorisedAgent)(irvClient: IrvClient) = Transition {
      case IdentifyPersonalClient(ftRequest, continueURL) if featureFlags.showKfcMtdIt =>
        val newState =
          CheckDetails(
            ftRequest.copy(clientIdentifier = irvClient.clientIdentifier, knownFact = irvClient.dob),
            continueURL)
        checkedDetails(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(
          hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(Confirmation(true)).apply(newState)
    }

    def identifiedClientVat(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      featureFlags: FeatureFlags)(agent: AuthorisedAgent)(vatClient: VatClient) = Transition {
      case IdentifyPersonalClient(ftRequest, continueURL) =>
        val newState = CheckDetails(
          ftRequest.copy(clientIdentifier = vatClient.clientIdentifier, knownFact = vatClient.registrationDate),
          continueURL)
        checkedDetails(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(
          hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(Confirmation(true)).apply(newState)
      case IdentifyBusinessClient(ftRequest, continueURL) =>
        val newState = CheckDetails(
          ftRequest.copy(clientIdentifier = vatClient.clientIdentifier, knownFact = vatClient.registrationDate),
          continueURL)
        checkedDetails(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(
          hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(Confirmation(true)).apply(newState)
    }

    def moreDetailsItsa(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      featureFlags: FeatureFlags)(agent: AuthorisedAgent)(suppliedKnownFact: Option[String]) = Transition {
      case MoreDetails(ftRequest, continueUrl) =>
        val newState = CheckDetails(ftRequest.copy(knownFact = suppliedKnownFact), continueUrl)
        checkedDetails(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(
          hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(Confirmation(true)).apply(newState)
    }

    def moreDetailsIrv(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      featureFlags: FeatureFlags)(agent: AuthorisedAgent)(suppliedKnownFact: Option[String]) = Transition {
      case MoreDetails(ftRequest, continueUrl) =>
        val newState = CheckDetails(ftRequest.copy(knownFact = suppliedKnownFact), continueUrl)
        checkedDetails(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(
          hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(Confirmation(true)).apply(newState)
    }

    def moreDetailsVat(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      featureFlags: FeatureFlags)(agent: AuthorisedAgent)(suppliedKnownFact: Option[String]) = Transition {
      case MoreDetails(ftRequest, continueUrl) =>
        val newState = CheckDetails(ftRequest.copy(knownFact = suppliedKnownFact), continueUrl)
        checkedDetails(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(
          hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(Confirmation(true)).apply(newState)
    }

    def selectedClientType(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      featureFlags: FeatureFlags)(agent: AuthorisedAgent)(suppliedClientType: ClientType) = Transition {
      case SelectClientType(ftRequest, continueUrl) =>
        val newState = CheckDetails(ftRequest.copy(clientType = Some(suppliedClientType)), continueUrl)
        checkedDetails(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(
          hasPendingInvitations)(hasActiveRelationship)(featureFlags)(agent)(Confirmation(true)).apply(newState)
    }
  }

  object Extractors {
    object CheckDetailsItsaWithClientTypeAndKnownFact {
      def unapply(checkDetails: CheckDetails): Boolean =
        checkDetails.fastTrackRequest.service == HMRCMTDIT &&
          checkDetails.fastTrackRequest.clientType.nonEmpty &&
          checkDetails.fastTrackRequest.knownFact.nonEmpty
    }

    object CheckDetailsItsaWithClientType {
      def unapply(checkDetails: CheckDetails): Boolean =
        checkDetails.fastTrackRequest.service == HMRCMTDIT &&
          checkDetails.fastTrackRequest.clientType.nonEmpty
    }

    object CheckDetailsIrvWithClientTypeAndKnownFact {
      def unapply(checkDetails: CheckDetails) =
        checkDetails.fastTrackRequest.service == HMRCPIR &&
          checkDetails.fastTrackRequest.clientType.nonEmpty &&
          checkDetails.fastTrackRequest.knownFact.nonEmpty
    }

    object CheckDetailsIrvWithClientType {
      def unapply(checkDetails: CheckDetails) =
        checkDetails.fastTrackRequest.service == HMRCPIR &&
          checkDetails.fastTrackRequest.clientType.nonEmpty
    }

    object CheckDetailsPersonalVatWithClientTypeAndKnownFact {
      def unapply(checkDetails: CheckDetails) =
        checkDetails.fastTrackRequest.service == HMRCMTDVAT &&
          checkDetails.fastTrackRequest.clientType.contains(personal) &&
          checkDetails.fastTrackRequest.knownFact.nonEmpty
    }

    object CheckDetailsPersonalVatWithClientType {
      def unapply(checkDetails: CheckDetails) =
        checkDetails.fastTrackRequest.service == HMRCMTDVAT &&
          checkDetails.fastTrackRequest.clientType.contains(personal)
    }

    object CheckDetailsBusinessVatWithClientTypeAndKnownFact {
      def unapply(checkDetails: CheckDetails) =
        checkDetails.fastTrackRequest.service == HMRCMTDVAT &&
          checkDetails.fastTrackRequest.clientType.contains(business) &&
          checkDetails.fastTrackRequest.knownFact.nonEmpty
    }

    object CheckDetailsBusinessVatWithClientType {
      def unapply(checkDetails: CheckDetails) =
        checkDetails.fastTrackRequest.service == HMRCMTDVAT &&
          checkDetails.fastTrackRequest.clientType.contains(business)
    }

    object CheckDetailsNoKnownFact {
      def unapply(checkDetails: CheckDetails) =
        checkDetails.fastTrackRequest.clientType.nonEmpty &&
          checkDetails.fastTrackRequest.knownFact.isEmpty
    }

    object CheckDetailsNoClientType {
      def unapply(checkDetails: CheckDetails) =
        checkDetails.fastTrackRequest.clientType.isEmpty &&
          checkDetails.fastTrackRequest.knownFact.nonEmpty
    }
  }
}
