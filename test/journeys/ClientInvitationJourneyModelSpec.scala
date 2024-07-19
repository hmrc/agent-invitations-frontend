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

package journeys

import play.api.test.Helpers._
import support.UnitSpec
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.{State, Transition}
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, Service, SuspensionDetails}
import uk.gov.hmrc.auth.core.AffinityGroup.Organisation
import uk.gov.hmrc.auth.core.{AffinityGroup, Enrolment, Enrolments}
import uk.gov.hmrc.http.HeaderCarrier

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ClientInvitationJourneyModelSpec extends UnitSpec with StateMatchers[State] {

  import ClientType._

  implicit val hc: HeaderCarrier = HeaderCarrier()

  case class given(initialState: State) extends ClientInvitationJourneyService with TestStorage[(State, List[State])] {
    await(save((initialState, Nil)))

    def when(transition: Transition): (State, List[State]) =
      await(super.apply(transition))
  }

  private val now = LocalDateTime.now()

  private def dateString(date: LocalDateTime): String = {
    val fmt = DateTimeFormatter.ofPattern("d/M/yyy")
    date.format(fmt)
  }

  private val authorisedIndividualClient = AuthorisedClient(AffinityGroup.Individual, Enrolments(Set(Enrolment("HMRC-MTD-IT"))))
  private val authorisedIndividualClientWithAllSupportedEnrolments =
    AuthorisedClient(
      AffinityGroup.Individual,
      Enrolments(
        Set(
          Enrolment("HMRC-MTD-IT"),
          Enrolment("HMRC-CGT-PD"),
          Enrolment("HMRC-PPT-ORG"),
          Enrolment("HMRC-MTD-VAT"),
          Enrolment("HMRC-NI")
        )
      )
    )
  private val authorisedBusinessClient = AuthorisedClient(Organisation, Enrolments(Set(Enrolment("HMRC-MTD-VAT"))))
  private val authorisedBusinessClientWithBusinessOnly =
    AuthorisedClient(
      Organisation,
      Enrolments(Set(Enrolment("HMRC-MTD-VAT"), Enrolment("HMRC-PPT-ORG"), Enrolment("HMRC-CBC-ORG"), Enrolment("HMRC-PILLAR2-ORG")))
    )
  private val authorisedTrustOrEstateClient = AuthorisedClient(Organisation, Enrolments(Set(Enrolment("HMRC-CGT-PD"))))
  private val authorisedTrustNTClient = AuthorisedClient(Organisation, Enrolments(Set(Enrolment("HMRC-TERSNT-ORG"))))
  private val authorisedIndividualClientWithoutRelevantEnrolments =
    AuthorisedClient(AffinityGroup.Individual, Enrolments(Set(Enrolment("some-key"))))
  private val authorisedOrganisationClientWithoutRelevantEnrolments =
    AuthorisedClient(AffinityGroup.Organisation, Enrolments(Set(Enrolment("some-key"))))

  private val arn = Arn("TARN0000001")
  private val uid = "uid123"
  val invitationIdItsa = InvitationId("A1BEOZEO7MNO6")
  val invitationIdIrv = InvitationId("B1BEOZEO7MNO6")
  val invitationIdVat = InvitationId("C1BEOZEO7MNO6")
  val invitationIdTrust = InvitationId("D1BEOZEO7MNO6")
  val invitationIdTrustNT = InvitationId("F1BEOZO7MN06")
  val invitationIdCgt = InvitationId("E1BEOZEO7MNO6")
  val invitationIdPpt = InvitationId("E1BEOZEO7MNO6")
  val invitationIdCbc = InvitationId("HF99K6PXSBHTG")
  val invitationIdCbcNonUk = InvitationId("IF99K6PXSBHTG")
  val invitationIdPillar2 = InvitationId("KF99K6PXSBHTG")
  val expiryDate = LocalDate.parse("2010-01-01")
  val invitationStatus = Pending

  val normalisedAgentName = "agent-name"
  val agentName = "Agent Name"

  "ClientInvitationJourneyModel" when {
    "at any state" should {
      "transition to WarmUp with agency name" when {
        def getAgentReferenceRecord(uid: String) =
          Future(Some(AgentReferenceRecord("uid123", arn, Seq(normalisedAgentName, s"$normalisedAgentName-2"))))
        def getAgencyName(arn: Arn) = Future(agentName)

        "the affinity group does match the client type" in {
          given(MissingJourneyHistory) when start("personal", uid, normalisedAgentName)(getAgentReferenceRecord)(getAgencyName) should
            thenGo(WarmUp(Personal, uid, arn, agentName, normalisedAgentName))
        }

        "the affinity group does not match the client type" in {
          given(MissingJourneyHistory) when start("personal", uid, normalisedAgentName)(getAgentReferenceRecord)(getAgencyName) should
            thenGo(WarmUp(Personal, uid, arn, agentName, normalisedAgentName))
        }
      }
      "transition to NotFoundInvitation" when {
        "there is no agent reference record" in {
          def getAgentReferenceRecord(uid: String) = Future(None)
          def getAgencyName(arn: Arn) = Future(agentName)

          given(MissingJourneyHistory) when
            start("personal", "uid", normalisedAgentName)(getAgentReferenceRecord)(getAgencyName) should
            thenGo(NotFoundInvitation)
        }

        "the agency name in the agent reference record is different to the normalised agent name in the invitation link" in {
          def getAgentReferenceRecord(uid: String) =
            Future(Some(AgentReferenceRecord("uid123", arn, Seq(s"$normalisedAgentName-2"))))
          def getAgencyName(arn: Arn) = Future(agentName)

          given(MissingJourneyHistory) when
            start("personal", "uid", normalisedAgentName)(getAgentReferenceRecord)(getAgencyName) should
            thenGo(NotFoundInvitation)
        }
      }
    }
    "at WarmUp state" when {
      "submitting intent to continue" should {

        "transition to Consent when the invitation is found" in {
          def getInvitationDetails(uid: String) =
            Future(
              Seq(InvitationDetails(invitationIdItsa, expiryDate, invitationStatus, false, List(StatusChangeEvent(LocalDateTime.now(), Pending))))
            )
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

          given(WarmUp(Personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUp(getInvitationDetails, getNotSuspended)(Some(authorisedIndividualClient)) should
            thenGo(MultiConsent(Personal, uid, agentName, arn, Seq(ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = false))))
        }
        "transition to GGUserIdNeeded when the ClientType is personal and there is no active session" in {
          def getInvitationDetails(uid: String) =
            Future(
              Seq(InvitationDetails(invitationIdItsa, expiryDate, invitationStatus, false, List(StatusChangeEvent(LocalDateTime.now(), Pending))))
            )
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))
          given(WarmUp(Personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUp(getInvitationDetails, getNotSuspended)(None) should
            thenGo(GGUserIdNeeded(Personal, uid, arn, agentName))
        }

        "transition to WarmUpSessionRequired when the ClientType is business and there is no active session" in {
          def getInvitationDetails(uid: String) =
            Future(
              Seq(InvitationDetails(invitationIdItsa, expiryDate, invitationStatus, false, List(StatusChangeEvent(LocalDateTime.now(), Pending))))
            )
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

          given(WarmUp(Business, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUp(getInvitationDetails, getNotSuspended)(None) should
            thenGo(WarmUpSessionRequired(Business, uid, arn, agentName))
        }

        "transition to AuthorisationRequestAlreadyResponded when the invitation has a status of Accepted and " +
          "the client has some but not all supported MTD enrolments" in {
            def getInvitationDetails(uid: String) =
              Future(Seq(InvitationDetails(invitationIdItsa, expiryDate, Accepted, false, List(StatusChangeEvent(now, Accepted)))))
            def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

            given(WarmUp(Personal, uid, arn, agentName, normalisedAgentName)) when
              submitWarmUpToDecline(getInvitationDetails, getNotSuspended)(authorisedIndividualClient) should
              thenGo(AuthorisationRequestAlreadyResponded(dateString(now), Personal))
          }

        "transition to AlreadyRespondedToRequest when the invitation has a status of Accepted and " +
          "the client has ALL supported MTD enrolments" in {
            def getInvitationDetails(uid: String) =
              Future(Seq(InvitationDetails(invitationIdItsa, expiryDate, Accepted, false, List(StatusChangeEvent(now, Accepted)))))
            def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

            given(WarmUp(Personal, uid, arn, agentName, normalisedAgentName)) when
              submitWarmUpToDecline(getInvitationDetails, getNotSuspended)(authorisedIndividualClientWithAllSupportedEnrolments) should
              thenGo(AlreadyRespondedToRequest(dateString(now)))
          }

        "transition to AuthorisationRequestCancelled when the invitation has a status of Cancelled and " +
          "the client has some but not all supported MTD enrolments" in {
            def getInvitationDetails(uid: String) =
              Future(Seq(InvitationDetails(invitationIdItsa, expiryDate, Cancelled, false, List(StatusChangeEvent(LocalDateTime.now(), Cancelled)))))
            def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

            given(WarmUp(Personal, uid, arn, agentName, normalisedAgentName)) when
              submitWarmUpToDecline(getInvitationDetails, getNotSuspended)(authorisedIndividualClient) should
              thenGo(AuthorisationRequestCancelled(dateString(now), Personal))
          }

        "transition to AgentCancelledRequest when the invitation has a status of Cancelled and " +
          "the client has ALL supported MTD enrolments" in {
            def getInvitationDetails(uid: String) =
              Future(Seq(InvitationDetails(invitationIdItsa, expiryDate, Cancelled, false, List(StatusChangeEvent(now, Cancelled)))))
            def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

            given(WarmUp(Personal, uid, arn, agentName, normalisedAgentName)) when
              submitWarmUpToDecline(getInvitationDetails, getNotSuspended)(authorisedIndividualClientWithAllSupportedEnrolments) should
              thenGo(AgentCancelledRequest(dateString(now)))
          }

        "transition to AuthorisationRequestExpired when the invitation has a status of Expired and " +
          "the client has some but not all supported MTD enrolments" in {
            def getInvitationDetails(uid: String) =
              Future(Seq(InvitationDetails(invitationIdItsa, expiryDate, Expired, false, List(StatusChangeEvent(LocalDateTime.now(), Expired)))))
            def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

            given(WarmUp(Personal, uid, arn, agentName, normalisedAgentName)) when
              submitWarmUpToDecline(getInvitationDetails, getNotSuspended)(authorisedIndividualClient) should
              thenGo(AuthorisationRequestExpired(dateString(now), Personal))
          }

        "transition to RequestExpired when the invitation has a status of Expired and " +
          "the client has ALL supported MTD enrolments" in {
            def getInvitationDetails(uid: String) =
              Future(Seq(InvitationDetails(invitationIdItsa, expiryDate, Expired, false, List(StatusChangeEvent(now, Expired)))))
            def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

            given(WarmUp(Personal, uid, arn, agentName, normalisedAgentName)) when
              submitWarmUpToDecline(getInvitationDetails, getNotSuspended)(authorisedIndividualClientWithAllSupportedEnrolments) should
              thenGo(RequestExpired(dateString(now)))
          }

        "transition to AuthorisationRequestExpired when the status of the invitations is mixed with most recent Expired and " +
          "the client has some but not all supported MTD enrolments" in {
            def getInvitationDetails(uid: String) =
              Future(
                Seq(
                  InvitationDetails(invitationIdItsa, expiryDate, Expired, false, List(StatusChangeEvent(LocalDateTime.now(), Expired))),
                  InvitationDetails(
                    invitationIdItsa,
                    expiryDate,
                    Accepted,
                    false,
                    List(StatusChangeEvent(LocalDateTime.now().minusDays(1), Accepted))
                  )
                )
              )
            def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

            given(WarmUp(Personal, uid, arn, agentName, normalisedAgentName)) when
              submitWarmUpToDecline(getInvitationDetails, getNotSuspended)(authorisedIndividualClient) should
              thenGo(AuthorisationRequestExpired(dateString(now), Personal))
          }

        "transition to AlreadyRespondedToRequest when there are several authorisation requests and " +
          "the latest status change event is Accepted (or Declined) and the client has ALL supported MTD enrolments" in {
            def getInvitationDetails(uid: String) =
              Future(
                Seq(
                  InvitationDetails(invitationIdItsa, expiryDate, Cancelled, false, List(StatusChangeEvent(now.minusDays(1), Cancelled))),
                  InvitationDetails(invitationIdItsa, expiryDate, Expired, false, List(StatusChangeEvent(now.minusDays(5), Expired))),
                  InvitationDetails(invitationIdItsa, expiryDate, Cancelled, false, List(StatusChangeEvent(now.minusDays(2), Cancelled))),
                  InvitationDetails(invitationIdItsa, expiryDate, Accepted, false, List(StatusChangeEvent(now, Accepted)))
                )
              )
            def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

            given(WarmUp(Personal, uid, arn, agentName, normalisedAgentName)) when
              submitWarmUpToDecline(getInvitationDetails, getNotSuspended)(authorisedIndividualClientWithAllSupportedEnrolments) should
              thenGo(AlreadyRespondedToRequest(dateString(now)))
          }

        "transition to RequestExpired when there are several authorisation requests and " +
          "the latest status change event is Expired and the client has ALL supported MTD enrolments" in {
            def getInvitationDetails(uid: String) =
              Future(
                Seq(
                  InvitationDetails(
                    invitationIdItsa,
                    expiryDate,
                    Cancelled,
                    isRelationshipEnded = false,
                    List(StatusChangeEvent(now.minusDays(1), Cancelled))
                  ),
                  InvitationDetails(
                    invitationIdVat,
                    expiryDate,
                    Cancelled,
                    isRelationshipEnded = false,
                    List(StatusChangeEvent(now.minusDays(2), Cancelled))
                  ),
                  InvitationDetails(invitationIdIrv, expiryDate, Expired, isRelationshipEnded = false, List(StatusChangeEvent(now, Expired)))
                )
              )
            def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

            given(WarmUp(Personal, uid, arn, agentName, normalisedAgentName)) when
              submitWarmUpToDecline(getInvitationDetails, getNotSuspended)(authorisedIndividualClientWithAllSupportedEnrolments) should
              thenGo(RequestExpired(dateString(now)))
          }

        "transition to TrustNotClaimed when the invitation contains trust but the client doesn't have HMRC-TERS-ORG enrolment" in {
          def getInvitationDetails(uid: String) =
            Future(
              Seq(InvitationDetails(invitationIdTrust, expiryDate, invitationStatus, false, List(StatusChangeEvent(LocalDateTime.now(), Pending))))
            )
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

          given(WarmUp(Business, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUp(getInvitationDetails, getNotSuspended)(Some(authorisedTrustOrEstateClient)) should
            thenGo(TrustNotClaimed)
        }
        "transition to Consent when the invitation is to manage a trust and the client has the HMRC-TERSNT-ORG enrolment" in {
          def getInvitationDetails(uid: String) =
            Future(
              Seq(InvitationDetails(invitationIdTrustNT, expiryDate, invitationStatus, false, List(StatusChangeEvent(LocalDateTime.now(), Pending))))
            )
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

          given(WarmUp(Trust, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUp(getInvitationDetails, getNotSuspended)(Some(authorisedTrustNTClient)) should
            thenGo(MultiConsent(Trust, uid, agentName, arn, Seq(ClientConsent(invitationIdTrustNT, expiryDate, Service.TrustNT, consent = false))))
        }
        "transition to CannotFindRequest when the client has AffinityGroup Individual and no relevant enrolments" in {
          def getInvitationDetails(uid: String) =
            Future(Seq.empty[InvitationDetails])
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))
          given(WarmUp(Personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUp(getInvitationDetails, getNotSuspended)(Some(authorisedIndividualClientWithoutRelevantEnrolments)) should
            thenGo(CannotFindRequest(Personal, agentName))
        }

        "transition to CannotFindRequest when the client has AffinityGroup Organisation and no relevant enrolments" in {
          def getInvitationDetails(uid: String) =
            Future(Seq.empty[InvitationDetails])
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))
          given(WarmUp(Business, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUp(getInvitationDetails, getNotSuspended)(Some(authorisedOrganisationClientWithoutRelevantEnrolments)) should
            thenGo(CannotFindRequest(Business, agentName))
        }
        "transition to CannotFindRequest when the client has AffinityGroup Individual and some " +
          "relevant enrolments and there are no authorisation requests 'old' or current" in {
            def getInvitationDetails(uid: String) =
              Future(Seq.empty[InvitationDetails])
            def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))
            given(WarmUp(Personal, uid, arn, agentName, normalisedAgentName)) when
              submitWarmUp(getInvitationDetails, getNotSuspended)(Some(authorisedIndividualClient)) should
              thenGo(CannotFindRequest(Personal, agentName))
          }

        "transition to NoOutstandingRequests when the client has AffinityGroup Individual and all " +
          "relevant enrolments and there are no authorisation requests 'old' or current" in {
            def getInvitationDetails(uid: String) =
              Future(Seq.empty[InvitationDetails])
            def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))
            given(WarmUp(Personal, uid, arn, agentName, normalisedAgentName)) when
              submitWarmUp(getInvitationDetails, getNotSuspended)(Some(authorisedIndividualClientWithAllSupportedEnrolments)) should
              thenGo(NoOutstandingRequests)
          }

        "transition to NoOutstandingRequests when the client has AffinityGroup Organisation and all " +
          "relevant enrolments and there are no authorisation requests 'old' or current" in {
            def getInvitationDetails(uid: String) =
              Future(Seq.empty[InvitationDetails])
            def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))
            given(WarmUp(Business, uid, arn, agentName, normalisedAgentName)) when
              submitWarmUp(getInvitationDetails, getNotSuspended)(Some(authorisedBusinessClientWithBusinessOnly)) should
              thenGo(NoOutstandingRequests)
          }

        "transition to NoOutstandingRequests when the client has AffinityGroup Organisation and all " +
          "relevant enrolments  - including HMRC-NI - and there are no authorisation requests 'old' or current" in {
            def getInvitationDetails(uid: String) =
              Future(Seq.empty[InvitationDetails])
            def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))
            given(WarmUp(Business, uid, arn, agentName, normalisedAgentName)) when
              submitWarmUp(getInvitationDetails, getNotSuspended)(Some(authorisedIndividualClientWithAllSupportedEnrolments)) should
              thenGo(NoOutstandingRequests)
          }

        "transition to SuspendedAgent when agent is suspended for one or more of consent services" in {
          def getInvitationDetails(uid: String) =
            Future(
              Seq(InvitationDetails(invitationIdItsa, expiryDate, invitationStatus, false, List(StatusChangeEvent(LocalDateTime.now(), Pending))))
            )
          def getSuspendedForItsa(arn: Arn) = Future(SuspensionDetails(suspensionStatus = true, Some(Set("ITSA"))))

          given(WarmUp(Personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUp(getInvitationDetails, getSuspendedForItsa)(Some(authorisedIndividualClient)) should
            thenGo(SuspendedAgent(Personal, uid, agentName, arn, Set("ITSA"), Seq()))
        }
      }
      "submitting from suspension" should {
        "transition to MultiConsent with the nonsuspended consents" in {
          given(
            SuspendedAgent(
              Personal,
              uid,
              agentName,
              arn,
              Set("HMRC-MTD-VAT"),
              Seq(ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = false))
            )
          ) when submitSuspension(authorisedIndividualClient) should
            thenGo(MultiConsent(Personal, uid, agentName, arn, Seq(ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = false))))
        }
      }
      "submitting intent to decline" should {
        "transition to ConfirmDecline when the invitation is found" in {
          def getInvitationDetails(uid: String) =
            Future(
              Seq(InvitationDetails(invitationIdItsa, expiryDate, invitationStatus, false, List(StatusChangeEvent(LocalDateTime.now(), Pending))))
            )
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

          given(WarmUp(Personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(getInvitationDetails, getNotSuspended)(authorisedIndividualClient) should
            thenGo(ConfirmDecline(Personal, uid, agentName, arn, Seq(ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = false))))
        }

        "transition to AuthorisationRequestAlreadyResponded when the invitation status is Rejected" in {
          def getInvitationDetails(uid: String) =
            Future(Seq(InvitationDetails(invitationIdItsa, expiryDate, Rejected, false, List(StatusChangeEvent(LocalDateTime.now(), Rejected)))))
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

          given(WarmUp(Personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(getInvitationDetails, getNotSuspended)(authorisedIndividualClient) should
            thenGo(AuthorisationRequestAlreadyResponded(dateString(LocalDateTime.now()), Personal))
        }
      }
    }

    "at GGUserIdNeeded" should {
      "transition to WarmUpSessionRequired when Yes selected" in {
        given(
          GGUserIdNeeded(
            Personal,
            "uid",
            arn,
            "agentName"
          )
        ) when
          submitConfirmGGUserId(Confirmation(true)) should
          thenGo(WarmUpSessionRequired(Personal, "uid", arn, "agentName"))
      }

      "transition to WhichTaxService when No selected" in {
        given(
          GGUserIdNeeded(
            Personal,
            "uid",
            arn,
            "agentName"
          )
        ) when
          submitConfirmGGUserId(Confirmation(false)) should
          thenGo(WhichTaxService(Personal, "uid", arn, "agentName"))
      }
    }

    "at MultiConsent" should {
      "transition to CheckAnswers when all consents are given" in {
        given(
          MultiConsent(
            Personal,
            "uid",
            "agent name",
            arn,
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = false),
              ClientConsent(invitationIdIrv, expiryDate, Service.PersonalIncomeRecord, consent = false),
              ClientConsent(invitationIdVat, expiryDate, Service.Vat, consent = false),
              ClientConsent(invitationIdTrust, expiryDate, Service.Trust, consent = false),
              ClientConsent(invitationIdTrustNT, expiryDate, Service.TrustNT, consent = false),
              ClientConsent(invitationIdTrust, expiryDate, Service.CapitalGains, consent = false),
              ClientConsent(invitationIdCbc, expiryDate, Service.Cbc, consent = false),
              ClientConsent(invitationIdPillar2, expiryDate, Service.Pillar2, consent = false)
            )
          )
        ) when
          submitConsents(authorisedIndividualClient)(
            ConfirmedTerms.forServices(
              Service.MtdIt,
              Service.PersonalIncomeRecord,
              Service.Vat,
              Service.Trust,
              Service.TrustNT,
              Service.CapitalGains,
              Service.Ppt,
              Service.Cbc,
              Service.Pillar2
            )
          ) should
          thenGo(
            CheckAnswers(
              Personal,
              "uid",
              "agent name",
              Seq(
                ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = true),
                ClientConsent(invitationIdIrv, expiryDate, Service.PersonalIncomeRecord, consent = true),
                ClientConsent(invitationIdVat, expiryDate, Service.Vat, consent = true),
                ClientConsent(invitationIdTrust, expiryDate, Service.Trust, consent = true),
                ClientConsent(invitationIdTrustNT, expiryDate, Service.TrustNT, consent = true),
                ClientConsent(invitationIdTrust, expiryDate, Service.CapitalGains, consent = true),
                ClientConsent(invitationIdCbc, expiryDate, Service.Cbc, consent = true),
                ClientConsent(invitationIdPillar2, expiryDate, Service.Pillar2, consent = true)
              )
            )
          )
      }
    }
    "at SingleConsent" should {
      "transition to CheckAnswers with changed consent" in {
        given(
          SingleConsent(
            Personal,
            uid,
            "agent name",
            ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = false),
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = false),
              ClientConsent(invitationIdIrv, expiryDate, Service.PersonalIncomeRecord, consent = true),
              ClientConsent(invitationIdVat, expiryDate, Service.Vat, consent = true)
            )
          )
        ) when submitChangeConsents(authorisedIndividualClient)(
          ConfirmedTerms.forServices(Service.MtdIt, Service.Trust, Service.TrustNT, Service.CapitalGains)
        ) should thenGo(
          CheckAnswers(
            Personal,
            uid,
            "agent name",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = true),
              ClientConsent(invitationIdIrv, expiryDate, Service.PersonalIncomeRecord, consent = true),
              ClientConsent(invitationIdVat, expiryDate, Service.Vat, consent = true)
            )
          )
        )
      }
      Seq[(ClientType, Service, InvitationId)](
        (Personal, Service.MtdIt, invitationIdItsa),
        (Personal, Service.PersonalIncomeRecord, invitationIdIrv),
        (Personal, Service.Vat, invitationIdVat),
        (Trust, Service.Trust, invitationIdTrust),
        (Trust, Service.TrustNT, invitationIdTrustNT),
        (Personal, Service.CapitalGains, invitationIdCgt),
        (Business, Service.Ppt, invitationIdPpt),
        (Business, Service.Cbc, invitationIdCbc),
        (Business, Service.CbcNonUk, invitationIdCbcNonUk),
        (Business, Service.Pillar2, invitationIdPillar2)
      ).foreach { case (clientType, service, invitationId) =>
        val authorisedClient = clientType match {
          case Personal => authorisedIndividualClient
          case Business => authorisedBusinessClient
          case Trust    => authorisedTrustOrEstateClient
        }
        s"transition to CheckAnswers with changed $service consent" in {
          given(
            SingleConsent(
              clientType,
              uid,
              "agent name",
              ClientConsent(invitationId, expiryDate, service, consent = false),
              Seq(ClientConsent(invitationId, expiryDate, service, consent = false))
            )
          ) when submitChangeConsents(authorisedClient)(ConfirmedTerms.forServices(service)) should thenGo(
            CheckAnswers(clientType, uid, "agent name", Seq(ClientConsent(invitationId, expiryDate, service, consent = true)))
          )
        }
      }
    }
    "at CheckAnswers" should {
      "transition to InvitationsAccepted if all invitations are successfully accepted" in {
        def respondToInvitation(invitationId: InvitationId, agencyName: String, accepted: Boolean) = Future(true)

        given(
          CheckAnswers(
            Personal,
            uid,
            "agent name",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = true),
              ClientConsent(invitationIdIrv, expiryDate, Service.PersonalIncomeRecord, consent = true),
              ClientConsent(invitationIdVat, expiryDate, Service.Vat, consent = true)
            )
          )
        ) when submitCheckAnswers(respondToInvitation)(authorisedIndividualClient) should thenGo(
          InvitationsAccepted(
            "agent name",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = true),
              ClientConsent(invitationIdIrv, expiryDate, Service.PersonalIncomeRecord, consent = true),
              ClientConsent(invitationIdVat, expiryDate, Service.Vat, consent = true)
            ),
            Personal
          )
        )
      }
      "transition to InvitationsDeclined if all invitations are successfully declined" in {
        def respondToInvitation(invitationId: InvitationId, agencyName: String, accepted: Boolean) = Future(true)

        given(
          CheckAnswers(
            Personal,
            uid,
            "agent name",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = false),
              ClientConsent(invitationIdIrv, expiryDate, Service.PersonalIncomeRecord, consent = false),
              ClientConsent(invitationIdVat, expiryDate, Service.Vat, consent = false),
              ClientConsent(invitationIdTrust, expiryDate, Service.Trust, consent = false)
            )
          )
        ) when submitCheckAnswers(respondToInvitation)(authorisedIndividualClient) should thenGo(
          InvitationsDeclined(
            "agent name",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = false),
              ClientConsent(invitationIdIrv, expiryDate, Service.PersonalIncomeRecord, consent = false),
              ClientConsent(invitationIdVat, expiryDate, Service.Vat, consent = false),
              ClientConsent(invitationIdTrust, expiryDate, Service.Trust, consent = false)
            ),
            Personal
          )
        )
      }
      "transition to SomeResponsesFailed if some of the invitation acceptances fail" in {
        def respondToInvitation(invitationId: InvitationId, agencyName: String, accepted: Boolean) =
          if (invitationId == invitationIdItsa) Future(false) else Future(true)

        given(
          CheckAnswers(
            Personal,
            uid,
            "agent name",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = true),
              ClientConsent(invitationIdIrv, expiryDate, Service.PersonalIncomeRecord, consent = true),
              ClientConsent(invitationIdVat, expiryDate, Service.Vat, consent = true)
            )
          )
        ) when submitCheckAnswers(respondToInvitation)(authorisedIndividualClient) should thenGo(
          SomeResponsesFailed(
            "agent name",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = true)
            ),
            Seq(
              ClientConsent(invitationIdIrv, expiryDate, Service.PersonalIncomeRecord, consent = true, processed = true),
              ClientConsent(invitationIdVat, expiryDate, Service.Vat, consent = true, processed = true)
            ),
            Personal
          )
        )
      }
      "transition to AllResponsesFailed if all of the invitation acceptances fail" in {
        def respondToInvitation(invitationId: InvitationId, agencyName: String, accepted: Boolean) = Future(false)

        given(
          CheckAnswers(
            Personal,
            uid,
            "agent name",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = true),
              ClientConsent(invitationIdIrv, expiryDate, Service.PersonalIncomeRecord, consent = true),
              ClientConsent(invitationIdVat, expiryDate, Service.Vat, consent = true)
            )
          )
        ) when submitCheckAnswers(respondToInvitation)(authorisedIndividualClient) should thenGo(AllResponsesFailed)
      }
    }

    "at state CheckAnswers" should {
      "transition to SingleConsent " in {
        given(CheckAnswers(Personal, "uid", "agent name", Seq(ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = true)))) when
          submitCheckAnswersChange(Service.MtdIt)(authorisedIndividualClient) should thenGo(
            SingleConsent(
              Personal,
              "uid",
              "agent name",
              ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = true),
              Seq(ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = true))
            )
          )
      }
    }
    "at state ConfirmDecline" should {

      "transition to InvitationsDeclined if selected YES" in {
        def respondToInvitation(invitationId: InvitationId, agencyName: String, accepted: Boolean) = Future(true)

        given(
          ConfirmDecline(
            Personal,
            uid,
            "agent pearson",
            arn,
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = false),
              ClientConsent(invitationIdIrv, expiryDate, Service.PersonalIncomeRecord, consent = false),
              ClientConsent(invitationIdVat, expiryDate, Service.Vat, consent = false)
            )
          )
        ) when
          submitConfirmDecline(respondToInvitation)(authorisedIndividualClient)(Confirmation(true)) should thenGo(
            InvitationsDeclined(
              "agent pearson",
              Seq(
                ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = false),
                ClientConsent(invitationIdIrv, expiryDate, Service.PersonalIncomeRecord, consent = false),
                ClientConsent(invitationIdVat, expiryDate, Service.Vat, consent = false)
              ),
              Personal
            )
          )
      }

      "transition to MultiConsent if selected NO" in {
        def respondToInvitation(invitationId: InvitationId, agencyName: String, accepted: Boolean) = Future(true)

        given(
          ConfirmDecline(
            Personal,
            uid,
            "agent pearson",
            arn,
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = false),
              ClientConsent(invitationIdIrv, expiryDate, Service.PersonalIncomeRecord, consent = false),
              ClientConsent(invitationIdVat, expiryDate, Service.Vat, consent = false)
            )
          )
        ) when
          submitConfirmDecline(respondToInvitation)(authorisedIndividualClient)(Confirmation(false)) should thenGo(
            MultiConsent(
              Personal,
              uid,
              "agent pearson",
              arn,
              Seq(
                ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = false),
                ClientConsent(invitationIdIrv, expiryDate, Service.PersonalIncomeRecord, consent = false),
                ClientConsent(invitationIdVat, expiryDate, Service.Vat, consent = false)
              )
            )
          )
      }
    }

    "at state SomeResponsesFailed" should {
      "transition to InvitationsAccepted with successfully processed consents" in {
        given(
          SomeResponsesFailed(
            "Mr agent",
            Seq(ClientConsent(invitationIdItsa, expiryDate, Service.MtdIt, consent = true)),
            Seq(ClientConsent(invitationIdIrv, expiryDate, Service.PersonalIncomeRecord, consent = true, processed = true)),
            Personal
          )
        ) when continueSomeResponsesFailed(authorisedIndividualClient) should thenGo(
          InvitationsAccepted(
            "Mr agent",
            Seq(ClientConsent(InvitationId("B1BEOZEO7MNO6"), expiryDate, Service.PersonalIncomeRecord, consent = true, processed = true)),
            Personal
          )
        )
      }
    }
  }
}
