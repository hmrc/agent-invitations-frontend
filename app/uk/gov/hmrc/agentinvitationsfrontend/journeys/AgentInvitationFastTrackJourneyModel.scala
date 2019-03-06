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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.Transitions.{CheckDOBMatches, CheckPostcodeMatches, CheckRegDateMatches}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.personal
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, Vrn}
import uk.gov.hmrc.domain.Nino

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object AgentInvitationFastTrackJourneyModel extends JourneyModel {

  sealed trait State
  sealed trait Error

  val root: State = ???

  /* State should contain only minimal set of data required to proceed */
  object States {
    case class CheckDetails(request: AgentFastTrackRequest) extends State
    case class MoreDetails(request: AgentFastTrackRequest) extends State
    case class SelectClientType(request: AgentFastTrackRequest) extends State
    case class IdentifyPersonalClient(service: String) extends State
    case object IdentifyBusinessClient extends State
    case class PendingInvitationExists(request: AgentFastTrackRequest) extends State
    case class ActiveAuthorisationExists(request: AgentFastTrackRequest) extends State
    case class KnownFactNotMatched(request: AgentFastTrackRequest) extends State
    case class InvitationSentPersonal(invitationLink: String, continueUrl: Option[String]) extends State
    case class InvitationSentBusiness(invitationLink: String, continueUrl: Option[String]) extends State
    case class ClientNotSignedUp(request: AgentFastTrackRequest) extends State
  }

  object Transitions {
    import States._

    type HasPendingInvitations = (Arn, String, String) => Future[Boolean]
    type HasActiveRelationship = (Arn, String, String) => Future[Boolean]
    type GetClientName = (String, String) => Future[Option[String]]
    type CheckPostcodeMatches = (Nino, String) => Future[Option[Boolean]]
    type CheckRegDateMatches = (Vrn, LocalDate) => Future[Option[Int]]
    type GetAgentLink = (Arn, Option[ClientType]) => Future[String]
    type CreateInvitation =
      (Arn, Invitation) => Future[InvitationId]

    def agentFastTrack(agent: AuthorisedAgent)(fastTrackRequest: AgentFastTrackRequest) = Transition {
      case _ => {
        fastTrackRequest.service match {
          case HMRCMTDIT | HMRCPIR => goto(CheckDetails(fastTrackRequest.copy(clientType = Some(personal))))
          case HMRCMTDVAT          => goto(CheckDetails(fastTrackRequest))
        }
      }
    }

    private def checkIfPendingOrActiveAndGoto(
      fastTrackRequest: AgentFastTrackRequest,
      arn: Arn,
      invitation: Invitation)(
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
                   goto(PendingInvitationExists(fastTrackRequest))
                 } else {
                   hasActiveRelationshipFor(arn, fastTrackRequest.clientIdentifier, fastTrackRequest.service).flatMap {
                     case true => goto(ActiveAuthorisationExists(fastTrackRequest))
                     case false =>
                       for {
                         _              <- createInvitation(arn, invitation)
                         invitationLink <- getAgentLink(arn, fastTrackRequest.clientType)
                         result <- fastTrackRequest.clientType match {
                                    case Some(ClientType.personal) => goto(InvitationSentPersonal(invitationLink, None))
                                    case Some(ClientType.business) => goto(InvitationSentBusiness(invitationLink, None))
                                  }
                       } yield result
                   }
                 }
      } yield result

    def checkedDetails(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      agent: AuthorisedAgent)(confirmation: Confirmation) = Transition {
      case CheckDetails(ftRequest)
          if ftRequest.service == HMRCMTDIT && ftRequest.clientType.nonEmpty && ftRequest.knownFact.nonEmpty =>
        if (confirmation.choice) {
          checkPostcodeMatches(Nino(ftRequest.clientIdentifier), ftRequest.knownFact.getOrElse("")).flatMap {
            case Some(true) =>
              checkIfPendingOrActiveAndGoto(
                ftRequest,
                agent.arn,
                ItsaInvitation(Nino(ftRequest.clientIdentifier), ftRequest.knownFact.map(Postcode.apply))
              )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink)
            case Some(false) => goto(KnownFactNotMatched(ftRequest))
            case None        => goto(ClientNotSignedUp(ftRequest))
          }
        } else {
          goto(IdentifyPersonalClient(ftRequest.service))
        }

      case CheckDetails(ftRequest)
          if ftRequest.service == HMRCPIR && ftRequest.clientType.nonEmpty && ftRequest.knownFact.nonEmpty =>
        if (confirmation.choice) {
          checkDobMatches(Nino(ftRequest.clientIdentifier), LocalDate.parse(ftRequest.knownFact.getOrElse("")))
            .flatMap {
              case Some(true) =>
                checkIfPendingOrActiveAndGoto(
                  ftRequest,
                  agent.arn,
                  PirInvitation(Nino(ftRequest.clientIdentifier), ftRequest.knownFact.map(DOB.apply))
                )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink)
              case Some(false) => goto(KnownFactNotMatched(ftRequest))
              case None        => goto(ClientNotSignedUp(ftRequest))
            }
        } else {
          goto(IdentifyPersonalClient(ftRequest.service))
        }

      case CheckDetails(ftRequest)
          if ftRequest.service == HMRCMTDVAT && ftRequest.clientType
            .contains(ClientType.personal) && ftRequest.knownFact.nonEmpty =>
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
                    ftRequest.knownFact.map(VatRegDate.apply))
                )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink)
              case Some(_) => goto(KnownFactNotMatched(ftRequest))
              case None    => goto(ClientNotSignedUp(ftRequest))
            }
        } else {
          goto(IdentifyPersonalClient(ftRequest.service))
        }

      case CheckDetails(ftRequest)
          if ftRequest.service == HMRCMTDVAT && ftRequest.clientType
            .contains(ClientType.business) && ftRequest.knownFact.nonEmpty =>
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
                    ftRequest.knownFact.map(VatRegDate.apply))
                )(hasPendingInvitations, hasActiveRelationship, createInvitation, getAgentLink)
              case Some(_) => goto(KnownFactNotMatched(ftRequest))
              case None    => goto(ClientNotSignedUp(ftRequest))
            }
        } else {
          goto(IdentifyPersonalClient(ftRequest.service))
        }

      case CheckDetails(ftRequest) if ftRequest.clientType.nonEmpty && ftRequest.knownFact.isEmpty =>
        goto(MoreDetails(ftRequest))

      case CheckDetails(ftRequest) if ftRequest.clientType.isEmpty && ftRequest.knownFact.nonEmpty =>
        goto(SelectClientType(ftRequest))
    }

    def moreDetailsItsa(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      agent: AuthorisedAgent)(suppliedKnownFact: Option[String]) = Transition {
      case MoreDetails(ftRequest) =>
        val newState = CheckDetails(ftRequest.copy(knownFact = suppliedKnownFact))
        checkedDetails(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(
          hasPendingInvitations)(hasActiveRelationship)(agent)(Confirmation(true)).apply(newState)
    }

    def moreDetailsIrv(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      agent: AuthorisedAgent)(suppliedKnownFact: Option[String]) = Transition {
      case MoreDetails(ftRequest) =>
        val newState = CheckDetails(ftRequest.copy(knownFact = suppliedKnownFact))
        checkedDetails(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(
          hasPendingInvitations)(hasActiveRelationship)(agent)(Confirmation(true)).apply(newState)
    }

    def moreDetailsVat(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      agent: AuthorisedAgent)(suppliedKnownFact: Option[String]) = Transition {
      case MoreDetails(ftRequest) =>
        val newState = CheckDetails(ftRequest.copy(knownFact = suppliedKnownFact))
        checkedDetails(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(
          hasPendingInvitations)(hasActiveRelationship)(agent)(Confirmation(true)).apply(newState)
    }

    def selectedClientType(checkPostcodeMatches: CheckPostcodeMatches)(checkDobMatches: CheckDOBMatches)(
      checkRegDateMatches: CheckRegDateMatches)(createInvitation: CreateInvitation)(getAgentLink: GetAgentLink)(
      hasPendingInvitations: HasPendingInvitations)(hasActiveRelationship: HasActiveRelationship)(
      agent: AuthorisedAgent)(suppliedClientType: ClientType) = Transition {
      case SelectClientType(ftRequest) =>
        val newState = CheckDetails(ftRequest.copy(clientType = Some(suppliedClientType)))
        checkedDetails(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(
          hasPendingInvitations)(hasActiveRelationship)(agent)(Confirmation(true)).apply(newState)
    }
  }
}
