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

import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.fsm.JourneyModel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ClientInvitationJourneyModel extends JourneyModel {

  import Services._

  sealed trait State

  val root: State = State.Start()

  /* State should contain only minimal set of data required to proceed */
  object State {
    case class Start() extends State
    case class WarmUp(clientType: ClientType, uid: String, agentName: String) extends State
    case object NotFoundInvitation extends State
    case class MultiConsent(clientType: ClientType, uid: String, agentName: String, consents: Seq[ClientConsent])
        extends State
    case class SingleConsent(
      clientType: ClientType,
      uid: String,
      agentName: String,
      consent: ClientConsent,
      consents: Seq[ClientConsent])
        extends State
    case class IncorrectClientType(clientType: ClientType) extends State
    case class CheckAnswers(clientType: ClientType, uid: String, agentName: String, consents: Seq[ClientConsent])
        extends State
    case class InvitationsAccepted(agentName: String, consents: Seq[ClientConsent]) extends State
    case class InvitationsDeclined(clientType: ClientType, uid: String, agentName: String, consents: Seq[ClientConsent])
        extends State
    case object AllResponsesFailed extends State
    case class SomeResponsesFailed(agentName: String, consents: Seq[ClientConsent]) extends State
  }

  object Transitions {

    type GetAgentReferenceRecord = String => Future[Option[AgentReferenceRecord]]
    type GetAgencyName = Arn => Future[String]
    type GetPendingInvitationIdsAndExpiryDates = (String, InvitationStatus) => Future[Seq[InvitationIdAndExpiryDate]]
    type AcceptInvitation = InvitationId => Future[Boolean]
    type RejectInvitation = InvitationId => Future[Boolean]

    private def clientTypeMatchesGroup(affinityGroup: String, clientType: String): Boolean =
      (affinityGroup, clientType) match {
        case ("Individual", "personal")   => true
        case ("Organisation", "business") => true
        case _                            => false
      }

    def start(clientType: String, uid: String, normalisedAgentName: String)(
      getAgentReferenceRecord: GetAgentReferenceRecord)(getAgencyName: GetAgencyName)(client: AuthorisedClient) =
      Transition {
        case _ =>
          if (clientTypeMatchesGroup(client.affinity, clientType)) {
            for {
              record <- getAgentReferenceRecord(uid)
              result <- record match {
                         case Some(r) if r.normalisedAgentNames.contains(normalisedAgentName) =>
                           getAgencyName(r.arn).flatMap { name =>
                             goto(WarmUp(ClientType.toEnum(clientType), uid, name))
                           }
                         case None => goto(NotFoundInvitation)
                       }
            } yield result
          } else goto(IncorrectClientType(ClientType.toEnum(clientType)))
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
      client: AuthorisedClient) = Transition {
      case WarmUp(clientType, uid, agentName) =>
        getConsents(getPendingInvitationIdsAndExpiryDates)(agentName, uid).flatMap {
          case consents if consents.nonEmpty => goto(MultiConsent(clientType, agentName, uid, consents))
          case _                             => goto(NotFoundInvitation)
        }
    }

    def determineNewConsents(oldConsents: Seq[ClientConsent], formTerms: ConfirmedTerms): Seq[ClientConsent] =
      oldConsents.map { oldConsent =>
        oldConsent.serviceKey match {
          case "itsa" => oldConsent.copy(consent = formTerms.itsaConsent)
          case "afi"  => oldConsent.copy(consent = formTerms.afiConsent)
          case "vat"  => oldConsent.copy(consent = formTerms.vatConsent)
          case _      => throw new IllegalStateException("the service key was not supported")
        }
      }

    def submitConsents(client: AuthorisedClient)(confirmedTerms: ConfirmedTerms) = Transition {
      case MultiConsent(clientType, uid, agentName, consents) =>
        val newConsents = determineNewConsents(consents, confirmedTerms)
        goto(CheckAnswers(clientType, uid, agentName, newConsents))
    }

    def submitChangeConsents(client: AuthorisedClient) = Transition {
      case SingleConsent(clientType, uid, agentName, newConsent, consents) =>
        val newConsents =
          consents.map(c => if (c.serviceKey == newConsent.serviceKey) c.copy(consent = newConsent.consent) else c)
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
                     goto(InvitationsDeclined(clientType, uid, agentName, consents))
                   else if (ClientConsent.allAcceptanceFailed(newConsents)) goto(AllResponsesFailed)
                   else if (ClientConsent.someAcceptanceFailed(newConsents))
                     goto(SomeResponsesFailed(agentName, consents))
                   else goto(InvitationsAccepted(agentName, consents))
        } yield result
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
