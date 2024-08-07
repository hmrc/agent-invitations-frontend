/*
 * Copyright 2023 HM Revenue & Customs
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

import play.api.Logging
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, Service, SuspensionDetails}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{Business, Personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.determineService
import uk.gov.hmrc.agentinvitationsfrontend.models.{ClientType, _}
import uk.gov.hmrc.auth.core.AffinityGroup.Individual
import uk.gov.hmrc.auth.core.{Enrolment, Enrolments}
import uk.gov.hmrc.play.fsm.JourneyModel

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.concurrent.{ExecutionContext, Future}

object ClientInvitationJourneyModel extends JourneyModel with Logging {

  sealed trait State
  sealed trait IsError

  val root: State = State.MissingJourneyHistory

  /* State should contain only minimal set of data required to proceed */
  object State {
    case object MissingJourneyHistory extends State

    case class WarmUp(clientType: ClientType, uid: String, arn: Arn, agentName: String, normalisedAgentName: String) extends State

    case class CreateNewUserId(clientType: ClientType, uid: String, arn: Arn, agentName: String) extends State

    case class WarmUpSessionRequired(clientType: ClientType, uid: String, arn: Arn, agentName: String) extends State

    case class GGUserIdNeeded(clientType: ClientType, uid: String, arn: Arn, agentName: String) extends State

    case class WhichTaxService(clientType: ClientType, uid: String, arn: Arn, agentName: String) extends State

    case class SubmitWhichTaxService(clientType: ClientType, uid: String, arn: Arn, agentName: String) extends State

    case class SignUpToTaxService(uid: String) extends State

    case class ActionNeeded(clientType: ClientType) extends State with IsError

    case object NotFoundInvitation extends State with IsError

    case object NoOutstandingRequests extends State with IsError

    trait ErrorState extends State

    case class RequestExpired(expiredOn: String) extends ErrorState with IsError
    case class AlreadyRespondedToRequest(respondedOn: String) extends ErrorState with IsError
    case class AgentCancelledRequest(cancelledOn: String) extends ErrorState with IsError

    case class CannotFindRequest(clientType: ClientType, agencyName: String) extends State with IsError

    trait AuthErrorState extends State

    case class AuthorisationRequestExpired(
      expiredOn: String,
      clientType: ClientType
    ) extends AuthErrorState with IsError

    case class AuthorisationRequestCancelled(cancelledOn: String, clientType: ClientType) extends AuthErrorState with IsError

    case class AuthorisationRequestAlreadyResponded(respondedOn: String, clientType: ClientType) extends AuthErrorState with IsError

    case class MultiConsent(clientType: ClientType, uid: String, agentName: String, arn: Arn, consents: Seq[ClientConsent]) extends State

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
      clientType: ClientType
    ) extends State

    case class ConfirmDecline(clientType: ClientType, uid: String, agentName: String, arn: Arn, consents: Seq[ClientConsent]) extends State

    case object TrustNotClaimed extends State

    case class SuspendedAgent(
      clientType: ClientType,
      uid: String,
      agentName: String,
      arn: Arn,
      suspendedRegimes: Set[String],
      nonSuspendedConsents: Seq[ClientConsent]
    ) extends State
  }

  object Transitions {

    import State._

    type GetAgentReferenceRecord = String => Future[Option[AgentReferenceRecord]]
    type GetAgencyName = Arn => Future[String]
    type GetInvitationDetails = String => Future[Seq[InvitationDetails]]
    type RespondToInvitation = (InvitationId, String, Boolean /* reject/accept */ ) => Future[Boolean]
    type GetSuspensionDetails = Arn => Future[SuspensionDetails]

    def start(clientTypeStr: String, uid: String, agentName: String)(getAgentReferenceRecord: GetAgentReferenceRecord)(getAgencyName: GetAgencyName)(
      implicit ec: ExecutionContext
    ) =
      Transition { case _ =>
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

    def submitConfirmGGUserId(confirmation: Confirmation) = Transition { case GGUserIdNeeded(clientType, uid, arn, name) =>
      if (confirmation.choice) goto(WarmUpSessionRequired(clientType, uid, arn, name))
      else goto(WhichTaxService(clientType, uid, arn, name))
    }

    def transitionFromMultiConsent = Transition {
      case MultiConsent(clientType, uid, agentName, arn, _)       => goto(GGUserIdNeeded(clientType, uid, arn, agentName))
      case WarmUpSessionRequired(clientType, uid, arn, agentName) => goto(GGUserIdNeeded(clientType, uid, arn, agentName))
      case WhichTaxService(clientType, uid, arn, agentName)       => goto(GGUserIdNeeded(clientType, uid, arn, agentName))
    }

    private def getConsents(pendingInvitationDetails: Seq[InvitationDetails])(agencyName: String, uid: String): Seq[ClientConsent] =
      pendingInvitationDetails.map(invitation =>
        ClientConsent(
          invitation.invitationId,
          invitation.expiryDate,
          determineService(invitation.invitationId),
          consent = false,
          isAltItsa = invitation.isAltItsa
        )
      )

    def submitWarmUp(getPendingInvitationIdsAndExpiryDates: GetInvitationDetails, getSuspensionStatus: GetSuspensionDetails)(
      client: Option[AuthorisedClient]
    )(implicit ec: ExecutionContext) =
      transitionFromWarmup(idealTargetState = MultiConsent.apply)(getPendingInvitationIdsAndExpiryDates, getSuspensionStatus)(client)

    def submitCreateNewUserId(getPendingInvitationIdsAndExpiryDates: GetInvitationDetails, getSuspensionStatus: GetSuspensionDetails)(
      client: AuthorisedClient
    )(implicit ec: ExecutionContext) =
      transitionFromCreateNewUserId(idealTargetState = MultiConsent.apply)(getPendingInvitationIdsAndExpiryDates, getSuspensionStatus)(client)

    def submitWarmUpSessionRequired(getPendingInvitationIdsAndExpiryDates: GetInvitationDetails, getSuspensionStatus: GetSuspensionDetails)(
      client: AuthorisedClient
    )(implicit ec: ExecutionContext) =
      transitionFromWarmUpWithSession(idealTargetState = MultiConsent.apply)(getPendingInvitationIdsAndExpiryDates, getSuspensionStatus)(client)

    def submitWarmUpToDecline(getPendingInvitationIdsAndExpiryDates: GetInvitationDetails, getSuspensionStatus: GetSuspensionDetails)(
      client: AuthorisedClient
    )(implicit ec: ExecutionContext) =
      transitionFromWarmup(idealTargetState = ConfirmDecline.apply)(getPendingInvitationIdsAndExpiryDates, getSuspensionStatus)(Some(client))

    private def transitionFromWarmUpWithSession(
      idealTargetState: (ClientType, String, String, Arn, Seq[ClientConsent]) => State
    )(getInvitationDetails: GetInvitationDetails, getSuspensionDetails: GetSuspensionDetails)(client: AuthorisedClient)(implicit
      ec: ExecutionContext
    ) =
      Transition { case WarmUpSessionRequired(clientType, uid, arn, agentName) =>
        transitionFromWarmUpFunction(
          client,
          clientType,
          agentName,
          getInvitationDetails,
          getSuspensionDetails,
          uid,
          arn,
          idealTargetState
        )
      }

    private def transitionFromWarmup(
      idealTargetState: (ClientType, String, String, Arn, Seq[ClientConsent]) => State
    )(getInvitationDetails: GetInvitationDetails, getSuspensionDetails: GetSuspensionDetails)(
      client: Option[AuthorisedClient]
    )(implicit ec: ExecutionContext) =
      Transition {
        case WarmUp(clientType, uid, arn, agentName, _) if client.isDefined =>
          transitionFromWarmUpFunction(
            client.get,
            clientType,
            agentName,
            getInvitationDetails,
            getSuspensionDetails,
            uid,
            arn,
            idealTargetState
          )
        case WarmUp(clientType, uid, arn, agentName, _) if clientType == ClientType.Personal =>
          goto(GGUserIdNeeded(clientType, uid, arn, agentName))
        case WarmUp(clientType, uid, arn, agentName, _) =>
          goto(WarmUpSessionRequired(clientType, uid, arn, agentName))
      }

    private def transitionFromWarmUpFunction(
      client: AuthorisedClient,
      clientType: ClientType,
      agentName: String,
      getInvitationDetails: GetInvitationDetails,
      getSuspensionDetails: GetSuspensionDetails,
      uid: String,
      arn: Arn,
      idealTargetState: (ClientType, String, String, Arn, Seq[ClientConsent]) => State
    )(implicit ec: ExecutionContext): Future[State] =
      client.enrolmentCoverage match {
        case NoSupportedMTDEnrolments =>
          logger.warn(s"client had no supported MTD enrolments")
          goto(CannotFindRequest(clientType, agentName))
        case maybeAll @ (AllSupportedMTDEnrolments | SomeSupportedMTDEnrolments) =>
          getInvitationDetails(uid).flatMap { invitationDetails =>
            if (invitationDetails.isEmpty) {
              logger.warn(s"no authorisation requests returned for uid: $uid. client had ${maybeAll.str}")
              if (maybeAll == SomeSupportedMTDEnrolments) goto(CannotFindRequest(clientType, agentName))
              else goto(NoOutstandingRequests)
            } else {
              getConsents(invitationDetails.filter(_.status == Pending))(agentName, uid) match {
                case Nil => determineStateForNonPending(invitationDetails, maybeAll, clientType)
                case consents =>
                  val containsTrust = consents.exists(_.service == Service.Trust)
                  val containsTrustNT = consents.exists(_.service == Service.TrustNT)
                  val butNoTrustEnrolment = !client.enrolments.enrolments.exists(_.key == Service.Trust.id)
                  val butNoTrustNtEnrolment = !client.enrolments.enrolments.exists(_.key == Service.TrustNT.id)
                  if (containsTrust && butNoTrustEnrolment) {
                    logger.warn("client doesn't have the expected HMRC-TERS-ORG enrolment to accept/reject an invitation")
                    goto(TrustNotClaimed)
                  } else if (containsTrustNT && butNoTrustNtEnrolment) {
                    logger.warn("client doesn't have the expected HMRC-TERSNT-ORG enrolment to accept/reject an invitation")
                    goto(TrustNotClaimed)
                  } else {
                    consents match {
                      case _ if consents.nonEmpty =>
                        getSuspensionDetails(arn).flatMap { suspensionDetails =>
                          val consentServices: Set[Service] =
                            consents.map(consent => consent.service).toSet
                          val nonSuspendedConsents =
                            consents.filter(consent => !suspensionDetails.isRegimeSuspended(consent.service))
                          if (suspensionDetails.isAnyRegimeSuspendedForServices(consentServices.map(_.id)))
                            goto(
                              SuspendedAgent(
                                clientType,
                                uid,
                                agentName,
                                arn,
                                suspensionDetails.suspendedRegimesForServices(consentServices.map(_.id)),
                                nonSuspendedConsents
                              )
                            )
                          else {
                            goto(idealTargetState(clientType, uid, agentName, arn, nonSuspendedConsents))
                          }
                        }
                      case _ =>
                        logger.warn(s"No pending invitations are found for uid: $uid")
                        goto(NotFoundInvitation)
                    }
                  }
              }
            }
          }
      }

    private def transitionFromCreateNewUserId(
      idealTargetState: (ClientType, String, String, Arn, Seq[ClientConsent]) => State
    )(getInvitationDetails: GetInvitationDetails, getSuspensionDetails: GetSuspensionDetails)(client: AuthorisedClient)(implicit
      ec: ExecutionContext
    ) =
      Transition { case CreateNewUserId(clientType, uid, arn, agentName) =>
        transitionFromWarmUpFunction(client, clientType, agentName, getInvitationDetails, getSuspensionDetails, uid, arn, idealTargetState)
      }

    private def determineStateForNonPending(
      invitationDetails: Seq[InvitationDetails],
      enrolmentCoverage: EnrolmentCoverage,
      clientType: ClientType
    ): Future[State] = {
      val state = gotoState(_: State, _: State)(enrolmentCoverage)
      invitationDetails
        .sortBy(_.mostRecentEvent())
        .reverse
        .headOption
        .fold(
          goto(NotFoundInvitation)
        ) { i =>
          val eventDate = dateString(i.mostRecentEvent().time)
          i.status match {
            case Expired =>
              state(RequestExpired(eventDate), AuthorisationRequestExpired(eventDate, clientType))
            case Cancelled =>
              state(AgentCancelledRequest(eventDate), AuthorisationRequestCancelled(eventDate, clientType))
            case Accepted | Rejected | Deauthorised | Partialauth =>
              state(AlreadyRespondedToRequest(eventDate), AuthorisationRequestAlreadyResponded(eventDate, clientType))
            case e => throw new RuntimeException(s"transition exception unexpected status $e")
          }
        }
    }

    private def dateString(date: LocalDateTime): String = {
      val fmt = DateTimeFormatter.ofPattern("d/M/yyy")
      date.format(fmt)
    }

    private def gotoState(targetState: State, fallbackState: State)(enrolmentCoverage: EnrolmentCoverage): Future[State] =
      if (enrolmentCoverage == AllSupportedMTDEnrolments) goto(targetState)
      else {
        goto(fallbackState)
      }

    private def tempEnrolLog(enrolments: Enrolments): String =
      enrolments.enrolments
        .map(x =>
          Enrolment(
            x.key,
            x.identifiers
              .map(id => id.copy(id.key, if (Option(id.value).exists(_.trim.nonEmpty)) "NOT_EMPTY" else "EMPTY")),
            x.state
          )
        )
        .mkString(",")

    def submitSuspension(client: AuthorisedClient) = Transition { case SuspendedAgent(clientType, uid, agentName, arn, _, nonSuspendedConsents) =>
      goto(MultiConsent(clientType, uid, agentName, arn, nonSuspendedConsents))
    }

    def submitConfirmDecline(respondToInvitation: RespondToInvitation)(client: AuthorisedClient)(confirmation: Confirmation)(implicit
      ec: ExecutionContext
    ) =
      Transition { case ConfirmDecline(clientType, uid, agentName, arn, consents) =>
        if (confirmation.choice) {
          val newConsentsF =
            Future.sequence {
              consents.map(consent =>
                respondToInvitation(consent.invitationId, agentName, false /* reject */ ).map(processed => consent.copy(processed = processed))
              )
            }
          for {
            newConsents <- newConsentsF
            result      <- getRedirectLinkAfterProcessConsents(consents, newConsents, agentName, clientType)

          } yield result
        } else goto(MultiConsent(clientType, uid, agentName, arn, consents))
      }

    def determineNewConsents(oldConsents: Seq[ClientConsent], formTerms: ConfirmedTerms): Seq[ClientConsent] =
      oldConsents.map { oldConsent =>
        oldConsent.copy(consent = formTerms.get(oldConsent.service))
      }

    def submitConsents(client: AuthorisedClient)(confirmedTerms: ConfirmedTerms) = Transition {
      case MultiConsent(clientType, uid, agentName, _, consents) =>
        val newConsents = determineNewConsents(consents, confirmedTerms)
        goto(CheckAnswers(clientType, uid, agentName, newConsents))
    }

    def determineChangedConsents(changedConsent: ClientConsent, oldConsents: Seq[ClientConsent], formTerms: ConfirmedTerms): Seq[ClientConsent] = {
      val newConsent = changedConsent.copy(consent = formTerms.get(changedConsent.service))
      oldConsents.map(c => if (c.service == changedConsent.service) c.copy(consent = newConsent.consent) else c)
    }

    def submitChangeConsents(client: AuthorisedClient)(confirmedTerms: ConfirmedTerms) = Transition {
      case SingleConsent(clientType, uid, agentName, consent, consents) =>
        val newConsents = determineChangedConsents(consent, consents, confirmedTerms)
        goto(CheckAnswers(clientType, uid, agentName, newConsents))
    }

    private def processConsents(
      respondToInvitation: RespondToInvitation
    )(consents: Seq[ClientConsent])(agentName: String)(implicit ec: ExecutionContext): Future[Seq[ClientConsent]] =
      for {
        result <- Future.traverse(consents) { case chosenConsent @ ClientConsent(invitationId, _, _, consent, _, _) =>
                    respondToInvitation(invitationId, agentName, consent /* reject/accept */ )
                      .map(processed => chosenConsent.copy(processed = processed))
                  }
      } yield result

    def submitCheckAnswers(respondToInvitation: RespondToInvitation)(client: AuthorisedClient)(implicit ec: ExecutionContext) = Transition {
      case CheckAnswers(clientType, _, agentName, consents) =>
        for {
          newConsents <- processConsents(respondToInvitation)(consents)(agentName)
          result      <- getRedirectLinkAfterProcessConsents(consents, newConsents, agentName, clientType)
        } yield result
    }

    private def getRedirectLinkAfterProcessConsents(
      consents: Seq[ClientConsent],
      newConsents: Seq[ClientConsent],
      agentName: String,
      clientType: ClientType
    ) =
      if (ClientConsent.allFailed(newConsents))
        goto(AllResponsesFailed)
      else if (ClientConsent.someFailed(newConsents))
        goto(SomeResponsesFailed(agentName, newConsents.filter(_.processed == false), newConsents.filter(_.processed == true), clientType))
      else if (ClientConsent.allAcceptedProcessed(newConsents))
        goto(InvitationsAccepted(agentName, consents, clientType))
      else if (ClientConsent.allDeclinedProcessed(newConsents))
        goto(InvitationsDeclined(agentName, consents, clientType))
      else goto(InvitationsAccepted(agentName, consents, clientType))

    def continueSomeResponsesFailed(client: AuthorisedClient) = Transition { case SomeResponsesFailed(agentName, _, successfulConsents, _) =>
      goto(
        InvitationsAccepted(agentName, successfulConsents, if (client.affinityGroup == Individual) Personal else Business)
      ) // TODO ACCOMODATE TRUST
    }

    def submitCheckAnswersChange(serviceToChange: Service)(client: AuthorisedClient) = Transition {
      case CheckAnswers(clientType, uid, agentName, consents) =>
        val chosenConsent: Option[ClientConsent] = consents.find(_.service == serviceToChange)
        chosenConsent match {
          case Some(consent) => goto(SingleConsent(clientType, uid, agentName, consent, consents))
          case None          => throw new IllegalStateException("the key for this consent was not found")
        }
    }

    def submitWhichTaxService(confirmation: Confirmation) =
      Transition { case WhichTaxService(clientType, uid, arn, name) =>
        if (confirmation.choice) {
          goto(CreateNewUserId(clientType, uid, arn, name))
        } else {
          goto(SignUpToTaxService(uid))
        }
      }
  }
}
