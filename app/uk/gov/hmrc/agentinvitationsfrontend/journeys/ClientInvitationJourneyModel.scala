/*
 * Copyright 2021 HM Revenue & Customs
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

import com.github.nscala_time.time.Imports.DateTimeFormat
import org.joda.time.DateTime
import play.api.Logging
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models.{ClientType, _}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId}
import uk.gov.hmrc.auth.core.AffinityGroup.Individual
import uk.gov.hmrc.auth.core.{Enrolment, Enrolments}
import uk.gov.hmrc.play.fsm.JourneyModel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ClientInvitationJourneyModel extends JourneyModel with Logging {

  sealed trait State
  sealed trait IsError

  val root: State = State.MissingJourneyHistory

  /* State should contain only minimal set of data required to proceed */
  object State {
    case object MissingJourneyHistory extends State

    case class WarmUp(clientType: ClientType, uid: String, arn: Arn, agentName: String, normalisedAgentName: String) extends State

    case class ActionNeeded(clientType: ClientType) extends State with IsError

    case object NotFoundInvitation extends State with IsError

    case object NoOutstandingRequests extends State with IsError

    case class RequestExpired(expiredOn: String) extends State with IsError

    case class AlreadyRespondedToRequest(respondedOn: String) extends State with IsError

    case class AgentCancelledRequest(cancelledOn: String) extends State with IsError

    case class CannotFindRequest(clientType: ClientType, agencyName: String) extends State with IsError

    case class AuthorisationRequestExpired(
      expiredOn: String,
      clientType: ClientType
    ) extends State with IsError

    case class AuthorisationRequestCancelled(cancelledOn: String, clientType: ClientType) extends State with IsError

    case class AuthorisationRequestAlreadyResponded(respondedOn: String, clientType: ClientType) extends State with IsError

    case class MultiConsent(clientType: ClientType, uid: String, agentName: String, consents: Seq[ClientConsent]) extends State

    case class SingleConsent(clientType: ClientType, uid: String, agentName: String, consent: ClientConsent, consents: Seq[ClientConsent])
        extends State

    case class CheckAnswers(clientType: ClientType, uid: String, agentName: String, consents: Seq[ClientConsent]) extends State

    case class InvitationsAccepted(agentName: String, consents: Seq[ClientConsent], clientType: ClientType) extends State

    case class InvitationsDeclined(agentName: String, consents: Seq[ClientConsent], clientType: ClientType) extends State

    case object AllResponsesFailed extends State

    case class SomeResponsesFailed(
      agentName: String,
      failedConsents: Seq[ClientConsent],
      successfulConsents: Seq[ClientConsent],
      clientType: ClientType)
        extends State

    case class ConfirmDecline(clientType: ClientType, uid: String, agentName: String, consents: Seq[ClientConsent]) extends State

    case object TrustNotClaimed extends State

    case class SuspendedAgent(
      clientType: ClientType,
      uid: String,
      agentName: String,
      suspendedRegimes: Set[String],
      nonSuspendedConsents: Seq[ClientConsent])
        extends State
  }

  object Transitions {

    import State._

    type GetAgentReferenceRecord = String => Future[Option[AgentReferenceRecord]]
    type GetAgencyName = Arn => Future[String]
    type GetInvitationDetails = String => Future[Seq[InvitationDetails]]
    type AcceptInvitation = InvitationId => String => Future[Boolean]
    type RejectInvitation = InvitationId => String => Future[Boolean]
    type GetSuspensionDetails = Arn => Future[SuspensionDetails]

    def start(clientTypeStr: String, uid: String, agentName: String)(getAgentReferenceRecord: GetAgentReferenceRecord)(getAgencyName: GetAgencyName) =
      Transition {
        case _ =>
          for {
            record <- getAgentReferenceRecord(uid)
            clientType = ClientType.toEnum(clientTypeStr)
            result <- record match {
                       case Some(r) if r.normalisedAgentNames.map(_.trim).contains(agentName.trim) =>
                         getAgencyName(r.arn).flatMap { name =>
                           goto(WarmUp(clientType, uid, r.arn, name, agentName.trim))
                         }
                       case Some(r) =>
                         val names = r.normalisedAgentNames.mkString(",")
                         logger.warn(s"Agency name not matching for uid: $uid, record has [$names], but url provided: [$agentName]")
                         goto(NotFoundInvitation)
                       case _ =>
                         logger.warn(s"No agent reference record found matching uid: $uid")
                         goto(NotFoundInvitation)
                     }
          } yield result
      }

    private def getConsents(pendingInvitationDetails: Seq[InvitationDetails])(agencyName: String, uid: String): Seq[ClientConsent] =
      pendingInvitationDetails.map(invitation =>
        ClientConsent(invitation.invitationId, invitation.expiryDate, determineServiceMessageKey(invitation.invitationId), consent = false))

    def submitWarmUp(agentSuspensionEnabled: Boolean)(
      getPendingInvitationIdsAndExpiryDates: GetInvitationDetails,
      getSuspensionStatus: GetSuspensionDetails)(client: AuthorisedClient) =
      transitionFromWarmup(agentSuspensionEnabled, idealTargetState = MultiConsent.apply)(getPendingInvitationIdsAndExpiryDates, getSuspensionStatus)(
        client)

    def submitWarmUpToDecline(agentSuspensionEnabled: Boolean)(
      getPendingInvitationIdsAndExpiryDates: GetInvitationDetails,
      getSuspensionStatus: GetSuspensionDetails)(client: AuthorisedClient) =
      transitionFromWarmup(agentSuspensionEnabled, idealTargetState = ConfirmDecline.apply)(
        getPendingInvitationIdsAndExpiryDates,
        getSuspensionStatus)(client)

    private def transitionFromWarmup(agentSuspensionEnabled: Boolean, idealTargetState: (ClientType, String, String, Seq[ClientConsent]) => State)(
      getInvitationDetails: GetInvitationDetails,
      getSuspensionDetails: GetSuspensionDetails)(client: AuthorisedClient) =
      Transition {
        case WarmUp(clientType, uid, arn, agentName, _) => {
          client.enrolmentCoverage match {
            case NoSupportedMTDEnrolments => {
              logger.warn(s"client had no supported MTD enrolments; client enrolments: ${tempEnrolLog(client.enrolments)}")
              goto(CannotFindRequest(clientType, agentName))
            }
            case maybeAll @ (AllSupportedMTDEnrolments | SomeSupportedMTDEnrolments) =>
              getInvitationDetails(uid).flatMap { invitationDetails =>
                if (invitationDetails.isEmpty) {
                  logger.warn(
                    s"no authorisation requests returned for uid: $uid. client had ${maybeAll.str}; client enrolments: [${tempEnrolLog(client.enrolments)}]")
                  if (maybeAll == SomeSupportedMTDEnrolments) goto(CannotFindRequest(clientType, agentName))
                  else goto(NoOutstandingRequests)
                } else {
                  getConsents(invitationDetails.filter(_.status == Pending))(agentName, uid) match {
                    case Nil => determineStateForNonPending(invitationDetails, maybeAll, clientType)
                    case consents =>
                      val containsTrust = consents.exists(_.serviceKey == determineServiceMessageKeyFromService(TRUST))
                      //not sure about the application here of TRUSTNT?
                      val containsTrustNT = consents.exists(_.serviceKey == determineServiceMessageKeyFromService(TRUSTNT))
                      val butNoTrustEnrolment = !client.enrolments.enrolments.exists(_.key == TRUST)
                      val butNoTrustNtEnrolment = !client.enrolments.enrolments.exists(_.key == TRUSTNT)
                      if (containsTrust && butNoTrustEnrolment) {
                        logger.warn("client doesn't have the expected HMRC-TERS-ORG enrolment to accept/reject an invitation")
                        goto(TrustNotClaimed)
                      } else if (containsTrustNT && butNoTrustNtEnrolment) {
                        logger.warn("client doesn't have the expected HMRC-TERSNT-ORG enrolment to accept/reject an invitation")
                        goto(TrustNotClaimed)
                      } else {
                        consents match {
                          case _ if consents.nonEmpty && agentSuspensionEnabled =>
                            getSuspensionDetails(arn).flatMap { suspensionDetails =>
                              val consentServices: Set[String] =
                                consents.map(consent => consent.service).toSet
                              val nonSuspendedConsents =
                                consents.filter(consent => !suspensionDetails.isRegimeSuspended(consent.service))
                              if (suspensionDetails.isAgentSuspended(consentServices))
                                goto(
                                  SuspendedAgent(
                                    clientType,
                                    uid,
                                    agentName,
                                    suspensionDetails.getSuspendedRegimes(consentServices),
                                    nonSuspendedConsents))
                              else {
                                goto(idealTargetState(clientType, uid, agentName, nonSuspendedConsents))
                              }
                            }
                          case _ if consents.nonEmpty => goto(idealTargetState(clientType, uid, agentName, consents))
                          case _ =>
                            logger.warn(s"No pending invitations are found for uid: $uid")
                            goto(NotFoundInvitation)
                        }
                      }
                  }
                }
              }
          }
        }
      }

    private def determineStateForNonPending(
      invitationDetails: Seq[InvitationDetails],
      enrolmentCoverage: EnrolmentCoverage,
      clientType: ClientType): Future[State] = {
      val state = gotoState(_: State, _: State)(enrolmentCoverage)
      invitationDetails
        .sortBy(_.mostRecentEvent())
        .reverse
        .headOption
        .fold(
          goto(NotFoundInvitation)
        )(i => {
          val eventDate = dateString(i.mostRecentEvent().time)
          i.status match {
            case Expired =>
              state(RequestExpired(eventDate), AuthorisationRequestExpired(eventDate, clientType))
            case Cancelled =>
              state(AgentCancelledRequest(eventDate), AuthorisationRequestCancelled(eventDate, clientType))
            case Accepted | Rejected =>
              state(AlreadyRespondedToRequest(eventDate), AuthorisationRequestAlreadyResponded(eventDate, clientType))
            case e => throw new RuntimeException(s"transition exception unexpected status $e")
          }
        })
    }

    private def dateString(date: DateTime): String = {
      val fmt = DateTimeFormat.forPattern("d/M/yyy")
      date.toString(fmt)
    }

    private def gotoState(targetState: State, fallbackState: State)(enrolmentCoverage: EnrolmentCoverage): Future[State] =
      if (enrolmentCoverage == AllSupportedMTDEnrolments) goto(targetState)
      else {
        goto(fallbackState)
      }

    private def tempEnrolLog(enrolments: Enrolments): String =
      enrolments.enrolments
        .map(
          x =>
            Enrolment(
              x.key,
              x.identifiers
                .map(id => id.copy(id.key, if (Option(id.value).exists(_.trim.nonEmpty)) "NOT_EMPTY" else "EMPTY")),
              x.state))
        .mkString(",")

    def submitSuspension(client: AuthorisedClient) = Transition {
      case SuspendedAgent(clientType, uid, agentName, _, nonSuspendedConsents) =>
        goto(MultiConsent(clientType, uid, agentName, nonSuspendedConsents))
    }

    def submitConfirmDecline(rejectInvitation: RejectInvitation)(client: AuthorisedClient)(confirmation: Confirmation) =
      Transition {
        case ConfirmDecline(clientType, uid, agentName, consents) =>
          if (confirmation.choice) {
            val newConsentsF =
              Future.sequence {
                consents.map(consent => rejectInvitation(consent.invitationId)(agentName).map(processed => consent.copy(processed = processed)))
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
          case "itsa"    => oldConsent.copy(consent = formTerms.itsaConsent)
          case "afi"     => oldConsent.copy(consent = formTerms.afiConsent)
          case "vat"     => oldConsent.copy(consent = formTerms.vatConsent)
          case "trust"   => oldConsent.copy(consent = formTerms.trustConsent)
          case "trustNT" => oldConsent.copy(consent = formTerms.trustNTConsent)
          case "cgt"     => oldConsent.copy(consent = formTerms.cgtConsent)
          case _         => throw new IllegalStateException("the service key was not supported")
        }
      }

    def submitConsents(client: AuthorisedClient)(confirmedTerms: ConfirmedTerms) = Transition {
      case MultiConsent(clientType, uid, agentName, consents) =>
        val newConsents = determineNewConsents(consents, confirmedTerms)
        goto(CheckAnswers(clientType, uid, agentName, newConsents))
    }

    def determineChangedConsents(changedConsent: ClientConsent, oldConsents: Seq[ClientConsent], formTerms: ConfirmedTerms): Seq[ClientConsent] = {
      val newConsent = changedConsent.serviceKey match {
        case "itsa"    => changedConsent.copy(consent = formTerms.itsaConsent)
        case "afi"     => changedConsent.copy(consent = formTerms.afiConsent)
        case "vat"     => changedConsent.copy(consent = formTerms.vatConsent)
        case "trust"   => changedConsent.copy(consent = formTerms.trustConsent)
        case "trustNT" => changedConsent.copy(consent = formTerms.trustNTConsent)
        case "cgt"     => changedConsent.copy(consent = formTerms.cgtConsent)
        case _         => throw new IllegalStateException("the service key was not supported")
      }
      oldConsents.map(c => if (c.serviceKey == changedConsent.serviceKey) c.copy(consent = newConsent.consent) else c)
    }

    def submitChangeConsents(client: AuthorisedClient)(confirmedTerms: ConfirmedTerms) = Transition {
      case SingleConsent(clientType, uid, agentName, consent, consents) =>
        val newConsents = determineChangedConsents(consent, consents, confirmedTerms)
        goto(CheckAnswers(clientType, uid, agentName, newConsents))
    }

    private def processConsents(acceptInvitation: AcceptInvitation)(rejectInvitation: RejectInvitation)(consents: Seq[ClientConsent])(
      agentName: String): Future[Seq[ClientConsent]] =
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

    def submitCheckAnswers(acceptInvitation: AcceptInvitation)(rejectInvitation: RejectInvitation)(client: AuthorisedClient) = Transition {
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
        goto(SomeResponsesFailed(agentName, newConsents.filter(_.processed == false), newConsents.filter(_.processed == true), clientType))
      else if (ClientConsent.allAcceptedProcessed(newConsents))
        goto(InvitationsAccepted(agentName, consents, clientType))
      else if (ClientConsent.allDeclinedProcessed(newConsents))
        goto(InvitationsDeclined(agentName, consents, clientType))
      else goto(InvitationsAccepted(agentName, consents, clientType))

    def continueSomeResponsesFailed(client: AuthorisedClient) = Transition {
      case SomeResponsesFailed(agentName, _, successfulConsents, _) =>
        goto(InvitationsAccepted(agentName, successfulConsents, if (client.affinityGroup == Individual) personal else business))
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
