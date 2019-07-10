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

import uk.gov.hmrc.agentinvitationsfrontend.models.{ClientType, _}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId}
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
    case class WarmUp(clientType: ClientType, uid: String, agentName: String, normalisedAgentName: String) extends State
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
    case class IncorrectClientType(clientType: ClientType) extends State with IsError
    case class CheckAnswers(clientType: ClientType, uid: String, agentName: String, consents: Seq[ClientConsent])
        extends State
    case class InvitationsAccepted(agentName: String, consents: Seq[ClientConsent]) extends State
    case class InvitationsDeclined(agentName: String, consents: Seq[ClientConsent]) extends State
    case object AllResponsesFailed extends State
    case class SomeResponsesFailed(
      agentName: String,
      failedConsents: Seq[ClientConsent],
      successfulConsents: Seq[ClientConsent])
        extends State
    case class ConfirmDecline(clientType: ClientType, uid: String, agentName: String, consents: Seq[ClientConsent])
        extends State
  }

  object Transitions {

    import State._

    type GetAgentReferenceRecord = String => Future[Option[AgentReferenceRecord]]
    type GetAgencyName = Arn => Future[String]
    type GetPendingInvitationIdsAndExpiryDates = (String, InvitationStatus) => Future[Seq[InvitationIdAndExpiryDate]]
    type AcceptInvitation = InvitationId => Future[Boolean]
    type RejectInvitation = InvitationId => Future[Boolean]

    private def clientTypeMatchesGroup(affinityGroup: String, clientType: ClientType): Boolean =
      (affinityGroup, clientType) match {
        case ("Individual", ClientType.personal)   => true
        case ("Organisation", ClientType.business) => true
        case _                                     => false
      }

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
                           goto(WarmUp(clientType, uid, name, normalisedAgentName))
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
              Services.determineServiceMessageKey(invitation.invitationId),
              consent = false))
      } yield consents

    def submitWarmUp(getPendingInvitationIdsAndExpiryDates: GetPendingInvitationIdsAndExpiryDates)(
      client: AuthorisedClient) =
      transitionFromWarmup(idealTargetState = MultiConsent.apply)(getPendingInvitationIdsAndExpiryDates)(client)

    def submitWarmUpToDecline(getPendingInvitationIdsAndExpiryDates: GetPendingInvitationIdsAndExpiryDates)(
      client: AuthorisedClient) =
      transitionFromWarmup(idealTargetState = ConfirmDecline.apply)(getPendingInvitationIdsAndExpiryDates)(client)

    private def transitionFromWarmup(idealTargetState: (ClientType, String, String, Seq[ClientConsent]) => State)(
      getPendingInvitationIdsAndExpiryDates: GetPendingInvitationIdsAndExpiryDates)(client: AuthorisedClient) =
      Transition {
        case WarmUp(clientType, uid, agentName, _) =>
          if (!clientTypeMatchesGroup(client.affinityGroup, clientType))
            goto(IncorrectClientType(clientType))
          else
            getConsents(getPendingInvitationIdsAndExpiryDates)(agentName, uid).flatMap {
              case consents if consents.nonEmpty => goto(idealTargetState(clientType, uid, agentName, consents))
              case _                             => goto(NotFoundInvitation)
            }
      }

    def submitConfirmDecline(rejectInvitation: RejectInvitation)(client: AuthorisedClient)(confirmation: Confirmation) =
      Transition {
        case ConfirmDecline(clientType, uid, agentName, consents) =>
          if (confirmation.choice) {
            consents.map(consent => rejectInvitation(consent.invitationId))
            goto(InvitationsDeclined(agentName, consents))
          } else goto(MultiConsent(clientType, uid, agentName, consents))
      }

    def determineNewConsents(oldConsents: Seq[ClientConsent], formTerms: ConfirmedTerms): Seq[ClientConsent] =
      oldConsents.map { oldConsent =>
        oldConsent.serviceKey match {
          case "itsa"  => oldConsent.copy(consent = formTerms.itsaConsent)
          case "afi"   => oldConsent.copy(consent = formTerms.afiConsent)
          case "vat"   => oldConsent.copy(consent = formTerms.vatConsent)
          case "trust" => oldConsent.copy(consent = formTerms.trustConsent)
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
      consents: Seq[ClientConsent]): Future[Seq[ClientConsent]] =
      for {
        result <- Future.traverse(consents) {
                   case chosenConsent @ ClientConsent(invitationId, _, _, consent, _) =>
                     if (consent) {
                       acceptInvitation(invitationId)
                         .map(acceptSuccess => chosenConsent.copy(processed = acceptSuccess))
                     } else {
                       rejectInvitation(invitationId)
                         .map(_ => chosenConsent.copy(processed = true))
                     }
                 }
      } yield result

    def submitCheckAnswers(acceptInvitation: AcceptInvitation)(rejectInvitation: RejectInvitation)(
      client: AuthorisedClient) = Transition {
      case CheckAnswers(clientType, uid, agentName, consents) =>
        for {
          newConsents <- processConsents(acceptInvitation)(rejectInvitation)(consents)
          result <- if (ClientConsent.allDeclinedProcessed(newConsents))
                     goto(InvitationsDeclined(agentName, consents))
                   else if (ClientConsent.allAcceptanceFailed(newConsents)) goto(AllResponsesFailed)
                   else if (ClientConsent.someAcceptanceFailed(newConsents))
                     goto(
                       SomeResponsesFailed(
                         agentName,
                         newConsents.filter(_.processed == false),
                         newConsents.filter(_.processed == true)))
                   else goto(InvitationsAccepted(agentName, consents))
        } yield result
    }

    def continueSomeResponsesFailed(client: AuthorisedClient) = Transition {
      case SomeResponsesFailed(agentName, _, successfulConsents) =>
        goto(InvitationsAccepted(agentName, successfulConsents))
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
