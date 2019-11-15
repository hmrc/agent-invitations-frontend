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

import play.api.Logger
import uk.gov.hmrc.agentinvitationsfrontend.connectors.SuspensionResponse
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models.{ClientType, _}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId}
import uk.gov.hmrc.auth.core.AffinityGroup.Individual
import uk.gov.hmrc.play.fsm.JourneyModel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ClientInvitationJourneyModel extends JourneyModel {

  sealed trait State
  sealed trait IsError

  val root: State = State.MissingJourneyHistory

  /* State should contain only minimal set of data required to proceed */
  object State {
    case object MissingJourneyHistory extends State

    case class WarmUp(clientType: ClientType, uid: String, arn: Arn, agentName: String, normalisedAgentName: String)
        extends State

    case object NotFoundInvitation extends State with IsError

    case class MultiConsent(clientType: ClientType, uid: String, agentName: String, consents: Seq[ClientConsent])
        extends State

    case class SingleConsent(
      clientType: ClientType,
      uid: String,
      agentName: String,
      consent: ClientConsent,
      consents: Seq[ClientConsent])
        extends State

    case class CheckAnswers(clientType: ClientType, uid: String, agentName: String, consents: Seq[ClientConsent])
        extends State

    case class InvitationsAccepted(agentName: String, consents: Seq[ClientConsent], clientType: ClientType)
        extends State

    case class InvitationsDeclined(agentName: String, consents: Seq[ClientConsent], clientType: ClientType)
        extends State

    case object AllResponsesFailed extends State

    case class SomeResponsesFailed(
      agentName: String,
      failedConsents: Seq[ClientConsent],
      successfulConsents: Seq[ClientConsent],
      clientType: ClientType)
        extends State

    case class ConfirmDecline(clientType: ClientType, uid: String, agentName: String, consents: Seq[ClientConsent])
        extends State

    case object TrustNotClaimed extends State

    case class SuspendedAgent(suspendedServices: Set[String]) extends State
  }

  object Transitions {

    import State._

    type GetAgentReferenceRecord = String => Future[Option[AgentReferenceRecord]]
    type GetAgencyName = Arn => Future[String]
    type GetPendingInvitationIdsAndExpiryDates = (String, InvitationStatus) => Future[Seq[InvitationIdAndExpiryDate]]
    type AcceptInvitation = InvitationId => String => Future[Boolean]
    type RejectInvitation = InvitationId => String => Future[Boolean]
    type GetSuspensionStatus = Arn => Future[SuspensionResponse]

    def start(clientTypeStr: String, uid: String, normalisedAgentName: String)(
      getAgentReferenceRecord: GetAgentReferenceRecord)(getAgencyName: GetAgencyName) =
      Transition {
        case _ =>
          for {
            record <- getAgentReferenceRecord(uid)
            result <- record match {
                       case Some(r) if r.normalisedAgentNames.contains(normalisedAgentName) =>
                         val clientType = ClientType.toEnum(clientTypeStr)
                         getAgencyName(r.arn).flatMap { name =>
                           goto(WarmUp(clientType, uid, r.arn, name, normalisedAgentName))
                         }
                       case _ => goto(NotFoundInvitation)
                     }
          } yield result
      }

    private def getConsents(getPendingInvitationIdsAndExpiryDates: GetPendingInvitationIdsAndExpiryDates)(
      agencyName: String,
      uid: String): Future[Seq[ClientConsent]] =
      for {
        invitations <- getPendingInvitationIdsAndExpiryDates(uid, Pending)
        consents = invitations.map(
          invitation =>
            ClientConsent(
              invitation.invitationId,
              invitation.expiryDate,
              determineServiceMessageKey(invitation.invitationId),
              consent = false))
      } yield consents

    def submitWarmUp(agentSuspensionEnabled: Boolean)(
      getPendingInvitationIdsAndExpiryDates: GetPendingInvitationIdsAndExpiryDates,
      getSuspensionStatus: GetSuspensionStatus)(client: AuthorisedClient) =
      transitionFromWarmup(agentSuspensionEnabled, idealTargetState = MultiConsent.apply)(
        getPendingInvitationIdsAndExpiryDates,
        getSuspensionStatus)(client)

    def submitWarmUpToDecline(agentSuspensionEnabled: Boolean)(
      getPendingInvitationIdsAndExpiryDates: GetPendingInvitationIdsAndExpiryDates,
      getSuspensionStatus: GetSuspensionStatus)(client: AuthorisedClient) =
      transitionFromWarmup(agentSuspensionEnabled, idealTargetState = ConfirmDecline.apply)(
        getPendingInvitationIdsAndExpiryDates,
        getSuspensionStatus)(client)

    private def transitionFromWarmup(
      agentSuspensionEnabled: Boolean,
      idealTargetState: (ClientType, String, String, Seq[ClientConsent]) => State)(
      getPendingInvitationIdsAndExpiryDates: GetPendingInvitationIdsAndExpiryDates,
      getSuspensionStatus: GetSuspensionStatus)(client: AuthorisedClient) =
      Transition {
        case WarmUp(clientType, uid, arn, agentName, _) =>
          getConsents(getPendingInvitationIdsAndExpiryDates)(agentName, uid).flatMap { consents =>
            val containsTrust = consents.exists(_.serviceKey == determineServiceMessageKeyFromService(TRUST))
            val butNoTrustEnrolment = !client.enrolments.enrolments.exists(_.key == TRUST)
            if (containsTrust && butNoTrustEnrolment) {
              Logger.warn("client doesn't have the expected HMRC-TERS-ORG enrolment to accept/reject an invitation")
              goto(TrustNotClaimed)
            } else {
              consents match {
                case _ if consents.nonEmpty && agentSuspensionEnabled =>
                  getSuspensionStatus(arn).flatMap { suspendedServices =>
                    val consentServices: Set[String] =
                      consents.map(consent => consent.service).toSet
                    if (suspendedServices.isAllSuspended(consentServices)) goto(SuspendedAgent(consentServices))
                    else {
                      val nonSuspendedConsents =
                        consents.filter(consent => !suspendedServices.isSuspended(consent.service))
                      goto(idealTargetState(clientType, uid, agentName, nonSuspendedConsents))
                    }
                  }
                case _ if consents.nonEmpty => goto(idealTargetState(clientType, uid, agentName, consents))
                case _                      => goto(NotFoundInvitation)
              }
            }
          }
      }

    def submitConfirmDecline(rejectInvitation: RejectInvitation)(client: AuthorisedClient)(confirmation: Confirmation) =
      Transition {
        case ConfirmDecline(clientType, uid, agentName, consents) =>
          if (confirmation.choice) {
            val newConsentsF =
              Future.sequence {
                consents.map(consent =>
                  rejectInvitation(consent.invitationId)(agentName).map(processed =>
                    consent.copy(processed = processed)))
              }
            for {
              newConsents <- newConsentsF
              result      <- getRedirectLinkAfterProcessConsents(consents, newConsents, agentName, clientType)

            } yield result
          } else goto(MultiConsent(clientType, uid, agentName, consents))
      }

    def determineNewConsents(oldConsents: Seq[ClientConsent], formTerms: ConfirmedTerms): Seq[ClientConsent] =
      oldConsents.map { oldConsent =>
        oldConsent.serviceKey match {
          case "itsa"  => oldConsent.copy(consent = formTerms.itsaConsent)
          case "afi"   => oldConsent.copy(consent = formTerms.afiConsent)
          case "vat"   => oldConsent.copy(consent = formTerms.vatConsent)
          case "trust" => oldConsent.copy(consent = formTerms.trustConsent)
          case "cgt"   => oldConsent.copy(consent = formTerms.cgtConsent)
          case _       => throw new IllegalStateException("the service key was not supported")
        }
      }

    def submitConsents(client: AuthorisedClient)(confirmedTerms: ConfirmedTerms) = Transition {
      case MultiConsent(clientType, uid, agentName, consents) =>
        val newConsents = determineNewConsents(consents, confirmedTerms)
        goto(CheckAnswers(clientType, uid, agentName, newConsents))
    }

    def determineChangedConsents(
      changedConsent: ClientConsent,
      oldConsents: Seq[ClientConsent],
      formTerms: ConfirmedTerms): Seq[ClientConsent] = {
      val newConsent = changedConsent.serviceKey match {
        case "itsa"  => changedConsent.copy(consent = formTerms.itsaConsent)
        case "afi"   => changedConsent.copy(consent = formTerms.afiConsent)
        case "vat"   => changedConsent.copy(consent = formTerms.vatConsent)
        case "trust" => changedConsent.copy(consent = formTerms.trustConsent)
        case "cgt"   => changedConsent.copy(consent = formTerms.cgtConsent)
        case _       => throw new IllegalStateException("the service key was not supported")
      }
      oldConsents.map(c => if (c.serviceKey == changedConsent.serviceKey) c.copy(consent = newConsent.consent) else c)
    }

    def submitChangeConsents(client: AuthorisedClient)(confirmedTerms: ConfirmedTerms) = Transition {
      case SingleConsent(clientType, uid, agentName, consent, consents) =>
        val newConsents = determineChangedConsents(consent, consents, confirmedTerms)
        goto(CheckAnswers(clientType, uid, agentName, newConsents))
    }

    private def processConsents(acceptInvitation: AcceptInvitation)(rejectInvitation: RejectInvitation)(
      consents: Seq[ClientConsent])(agentName: String): Future[Seq[ClientConsent]] =
      for {
        result <- Future.traverse(consents) {
                   case chosenConsent @ ClientConsent(invitationId, _, _, consent, _) =>
                     if (consent) {
                       acceptInvitation(invitationId)(agentName)
                         .map(acceptSuccess => chosenConsent.copy(processed = acceptSuccess))
                     } else {
                       rejectInvitation(invitationId)(agentName)
                         .map(processed => chosenConsent.copy(processed = processed))
                     }
                 }
      } yield result

    def submitCheckAnswers(acceptInvitation: AcceptInvitation)(rejectInvitation: RejectInvitation)(
      client: AuthorisedClient) = Transition {
      case CheckAnswers(clientType, _, agentName, consents) =>
        for {
          newConsents <- processConsents(acceptInvitation)(rejectInvitation)(consents)(agentName)
          result      <- getRedirectLinkAfterProcessConsents(consents, newConsents, agentName, clientType)
        } yield result
    }

    private def getRedirectLinkAfterProcessConsents(
      consents: Seq[ClientConsent],
      newConsents: Seq[ClientConsent],
      agentName: String,
      clientType: ClientType) =
      if (ClientConsent.allFailed(newConsents))
        goto(AllResponsesFailed)
      else if (ClientConsent.someFailed(newConsents))
        goto(
          SomeResponsesFailed(
            agentName,
            newConsents.filter(_.processed == false),
            newConsents.filter(_.processed == true),
            clientType))
      else if (ClientConsent.allAcceptedProcessed(newConsents))
        goto(InvitationsAccepted(agentName, consents, clientType))
      else if (ClientConsent.allDeclinedProcessed(newConsents))
        goto(InvitationsDeclined(agentName, consents, clientType))
      else goto(InvitationsAccepted(agentName, consents, clientType))

    def continueSomeResponsesFailed(client: AuthorisedClient) = Transition {
      case SomeResponsesFailed(agentName, _, successfulConsents, _) =>
        goto(
          InvitationsAccepted(
            agentName,
            successfulConsents,
            if (client.affinityGroup == Individual) personal else business))
    }

    def submitCheckAnswersChange(serviceMessageKeyToChange: String)(client: AuthorisedClient) = Transition {
      case CheckAnswers(clientType, uid, agentName, consents) =>
        val chosenConsent: Option[ClientConsent] = consents.find(_.serviceKey == serviceMessageKeyToChange)
        chosenConsent match {
          case Some(consent) => goto(SingleConsent(clientType, uid, agentName, consent, consents))
          case None          => throw new IllegalStateException("the key for this consent was not found")
        }
    }
  }
}
