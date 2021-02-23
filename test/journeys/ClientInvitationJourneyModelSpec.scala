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

package journeys

import com.github.nscala_time.time.Imports.DateTimeFormat
import org.joda.time.{DateTime, LocalDate}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.{State, Transition}
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models.{ConfirmedTerms, _}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId}
import uk.gov.hmrc.auth.core.AffinityGroup.Organisation
import uk.gov.hmrc.auth.core.{AffinityGroup, Enrolment, Enrolments}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

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

  private val now = DateTime.now()

  private def dateString(date: DateTime): String = {
    val fmt = DateTimeFormat.forPattern("d/M/yyy")
    date.toString(fmt)
  }

  val authorisedIndividualClient = AuthorisedClient(AffinityGroup.Individual, Enrolments(Set(Enrolment("HMRC-MTD-IT"))))
  val authorisedIndividualClientWithAllSupportedEnrolments =
    AuthorisedClient(
      AffinityGroup.Individual,
      Enrolments(
        Set(
          Enrolment("HMRC-MTD-IT"),
          Enrolment("HMRC-CGT-PD"),
          Enrolment("HMRC-MTD-VAT"),
          Enrolment("HMRC-NI")
        )
      )
    )
  val authorisedBusinessClient = AuthorisedClient(Organisation, Enrolments(Set(Enrolment("HMRC-MTD-VAT"))))
  val authorisedTrustOrEstateClient = AuthorisedClient(Organisation, Enrolments(Set(Enrolment("HMRC-CGT-PD"))))
  val authorisedIndividualClientWithoutRelevantEnrolments =
    AuthorisedClient(AffinityGroup.Individual, Enrolments(Set(Enrolment("some-key"))))
  val authorisedOrganisationClientWithoutRelevantEnrolments =
    AuthorisedClient(AffinityGroup.Organisation, Enrolments(Set(Enrolment("some-key"))))
  val availableServices = Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)

  val nino = "AB123456A"
  val arn = Arn("TARN0000001")
  val postCode = Some("BN114AW")
  val vrn = "123456"
  val vatRegDate = Some("2010-10-10")
  val dob = Some("1990-10-10")
  val uid = "uid123"
  val invitationIdItsa = InvitationId("A1BEOZEO7MNO6")
  val invitationIdIrv = InvitationId("B1BEOZEO7MNO6")
  val invitationIdVat = InvitationId("C1BEOZEO7MNO6")
  val invitationIdTrust = InvitationId("D1BEOZEO7MNO6")
  val invitationIdTrustNT = InvitationId("F1BEOZO7MN06")
  val invitationIdCgt = InvitationId("E1BEOZEO7MNO6")
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
            thenGo(WarmUp(personal, uid, arn, agentName, normalisedAgentName))
        }

        "the affinity group does not match the client type" in {
          given(MissingJourneyHistory) when start("personal", uid, normalisedAgentName)(getAgentReferenceRecord)(getAgencyName) should
            thenGo(WarmUp(personal, uid, arn, agentName, normalisedAgentName))
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
            Future(Seq(InvitationDetails(invitationIdItsa, expiryDate, invitationStatus, false, List(StatusChangeEvent(DateTime.now(), Pending)))))
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUp(agentSuspensionEnabled = true)(getInvitationDetails, getNotSuspended)(authorisedIndividualClient) should
            thenGo(MultiConsent(personal, uid, agentName, Seq(ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false))))
        }

        "transition to AuthorisationRequestAlreadyResponded when the invitation has a status of Accepted and " +
          "the client has some but not all supported MTD enrolments" in {
          def getInvitationDetails(uid: String) =
            Future(Seq(InvitationDetails(invitationIdItsa, expiryDate, Accepted, false, List(StatusChangeEvent(now, Accepted)))))
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(agentSuspensionEnabled = true)(getInvitationDetails, getNotSuspended)(authorisedIndividualClient) should
            thenGo(AuthorisationRequestAlreadyResponded(dateString(now), personal))
        }

        "transition to AlreadyRespondedToRequest when the invitation has a status of Accepted and " +
          "the client has ALL supported MTD enrolments" in {
          def getInvitationDetails(uid: String) =
            Future(Seq(InvitationDetails(invitationIdItsa, expiryDate, Accepted, false, List(StatusChangeEvent(now, Accepted)))))
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(agentSuspensionEnabled = true)(getInvitationDetails, getNotSuspended)(
              authorisedIndividualClientWithAllSupportedEnrolments) should
            thenGo(AlreadyRespondedToRequest(dateString(now)))
        }

        "transition to AuthorisationRequestCancelled when the invitation has a status of Cancelled and " +
          "the client has some but not all supported MTD enrolments" in {
          def getInvitationDetails(uid: String) =
            Future(Seq(InvitationDetails(invitationIdItsa, expiryDate, Cancelled, false, List(StatusChangeEvent(DateTime.now(), Cancelled)))))
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(agentSuspensionEnabled = true)(getInvitationDetails, getNotSuspended)(authorisedIndividualClient) should
            thenGo(AuthorisationRequestCancelled(dateString(now), personal))
        }

        "transition to AgentCancelledRequest when the invitation has a status of Cancelled and " +
          "the client has ALL supported MTD enrolments" in {
          def getInvitationDetails(uid: String) =
            Future(Seq(InvitationDetails(invitationIdItsa, expiryDate, Cancelled, false, List(StatusChangeEvent(now, Cancelled)))))
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(agentSuspensionEnabled = true)(getInvitationDetails, getNotSuspended)(
              authorisedIndividualClientWithAllSupportedEnrolments) should
            thenGo(AgentCancelledRequest(dateString(now)))
        }

        "transition to AuthorisationRequestExpired when the invitation has a status of Expired and " +
          "the client has some but not all supported MTD enrolments" in {
          def getInvitationDetails(uid: String) =
            Future(Seq(InvitationDetails(invitationIdItsa, expiryDate, Expired, false, List(StatusChangeEvent(DateTime.now(), Expired)))))
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(agentSuspensionEnabled = true)(getInvitationDetails, getNotSuspended)(authorisedIndividualClient) should
            thenGo(AuthorisationRequestExpired(dateString(now), personal))
        }

        "transition to RequestExpired when the invitation has a status of Expired and " +
          "the client has ALL supported MTD enrolments" in {
          def getInvitationDetails(uid: String) =
            Future(Seq(InvitationDetails(invitationIdItsa, expiryDate, Expired, false, List(StatusChangeEvent(now, Expired)))))
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(agentSuspensionEnabled = true)(getInvitationDetails, getNotSuspended)(
              authorisedIndividualClientWithAllSupportedEnrolments) should
            thenGo(RequestExpired(dateString(now)))
        }

        "transition to AuthorisationRequestExpired when the status of the invitations is mixed with most recent Expired and " +
          "the client has some but not all supported MTD enrolments" in {
          def getInvitationDetails(uid: String) =
            Future(
              Seq(
                InvitationDetails(invitationIdItsa, expiryDate, Expired, false, List(StatusChangeEvent(DateTime.now(), Expired))),
                InvitationDetails(invitationIdItsa, expiryDate, Accepted, false, List(StatusChangeEvent(DateTime.now().minusDays(1), Accepted)))
              ))
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(agentSuspensionEnabled = true)(getInvitationDetails, getNotSuspended)(authorisedIndividualClient) should
            thenGo(AuthorisationRequestExpired(dateString(now), personal))
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
              ))
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(agentSuspensionEnabled = true)(getInvitationDetails, getNotSuspended)(
              authorisedIndividualClientWithAllSupportedEnrolments) should
            thenGo(AlreadyRespondedToRequest(dateString(now)))
        }

        "transition to RequestExpired when there are several authorisation requests and " +
          "the latest status change event is Expired and the client has ALL supported MTD enrolments" in {
          def getInvitationDetails(uid: String) =
            Future(
              Seq(
                InvitationDetails(invitationIdItsa, expiryDate, Cancelled, false, List(StatusChangeEvent(now.minusDays(1), Cancelled))),
                InvitationDetails(invitationIdVat, expiryDate, Cancelled, false, List(StatusChangeEvent(now.minusDays(2), Cancelled))),
                InvitationDetails(invitationIdIrv, expiryDate, Expired, false, List(StatusChangeEvent(now, Expired)))
              ))
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(agentSuspensionEnabled = true)(getInvitationDetails, getNotSuspended)(
              authorisedIndividualClientWithAllSupportedEnrolments) should
            thenGo(RequestExpired(dateString(now)))
        }

        "transition to TrustNotClaimed when the invitation contains trust but the client doesn't have HMRC-TERS-ORG enrolment" in {
          def getInvitationDetails(uid: String) =
            Future(Seq(InvitationDetails(invitationIdTrust, expiryDate, invitationStatus, false, List(StatusChangeEvent(DateTime.now(), Pending)))))
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

          given(WarmUp(business, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUp(agentSuspensionEnabled = true)(getInvitationDetails, getNotSuspended)(authorisedTrustOrEstateClient) should
            thenGo(TrustNotClaimed)
        }

        "transition to CannotFindRequest when the client has AffinityGroup Individual and no relevant enrolments" in {
          def getInvitationDetails(uid: String) =
            Future(Seq.empty[InvitationDetails])
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))
          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUp(agentSuspensionEnabled = true)(getInvitationDetails, getNotSuspended)(authorisedIndividualClientWithoutRelevantEnrolments) should
            thenGo(CannotFindRequest(personal, agentName))
        }

        "transition to CannotFindRequest when the client has AffinityGroup Organisation and no relevant enrolments" in {
          def getInvitationDetails(uid: String) =
            Future(Seq.empty[InvitationDetails])
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))
          given(WarmUp(business, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUp(agentSuspensionEnabled = true)(getInvitationDetails, getNotSuspended)(authorisedOrganisationClientWithoutRelevantEnrolments) should
            thenGo(CannotFindRequest(business, agentName))
        }

        "transition to CannotFindRequest when the client has AffinityGroup Individual and some " +
          "relevant enrolments and there are no authorisation requests 'old' or current" in {
          def getInvitationDetails(uid: String) =
            Future(Seq.empty[InvitationDetails])
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))
          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUp(agentSuspensionEnabled = true)(getInvitationDetails, getNotSuspended)(authorisedIndividualClient) should
            thenGo(CannotFindRequest(personal, agentName))
        }

        "transition to NoOutstandingRequests when the client has AffinityGroup Individual and all " +
          "relevant enrolments and there are no authorisation requests 'old' or current" in {
          def getInvitationDetails(uid: String) =
            Future(Seq.empty[InvitationDetails])
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))
          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUp(agentSuspensionEnabled = true)(getInvitationDetails, getNotSuspended)(authorisedIndividualClientWithAllSupportedEnrolments) should
            thenGo(NoOutstandingRequests)
        }

        "transition to SuspendedAgent when agent is suspended for one or more of consent services" in {
          def getInvitationDetails(uid: String) =
            Future(Seq(InvitationDetails(invitationIdItsa, expiryDate, invitationStatus, false, List(StatusChangeEvent(DateTime.now(), Pending)))))
          def getSuspendedForItsa(arn: Arn) = Future(SuspensionDetails(suspensionStatus = true, Some(Set("ITSA"))))

          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUp(agentSuspensionEnabled = true)(getInvitationDetails, getSuspendedForItsa)(authorisedIndividualClient) should
            thenGo(SuspendedAgent(personal, uid, agentName, Set("ITSA"), Seq()))
        }
      }
      "submitting from suspension" should {
        "transition to MultiConsent with the nonsuspended consents" in {
          given(SuspendedAgent(personal, uid, agentName, Set(HMRCMTDVAT), Seq(ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false)))) when submitSuspension(
            authorisedIndividualClient) should
            thenGo(MultiConsent(personal, uid, agentName, Seq(ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false))))
        }
      }
      "submitting intent to decline" should {
        "transition to ConfirmDecline when the invitation is found" in {
          def getInvitationDetails(uid: String) =
            Future(Seq(InvitationDetails(invitationIdItsa, expiryDate, invitationStatus, false, List(StatusChangeEvent(DateTime.now(), Pending)))))
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(agentSuspensionEnabled = true)(getInvitationDetails, getNotSuspended)(authorisedIndividualClient) should
            thenGo(ConfirmDecline(personal, uid, agentName, Seq(ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false))))
        }

        "transition to AuthorisationRequestAlreadyResponded when the invitation status is Rejected" in {
          def getInvitationDetails(uid: String) =
            Future(Seq(InvitationDetails(invitationIdItsa, expiryDate, Rejected, false, List(StatusChangeEvent(DateTime.now(), Rejected)))))
          def getNotSuspended(arn: Arn) = Future(SuspensionDetails(suspensionStatus = false, None))

          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(agentSuspensionEnabled = true)(getInvitationDetails, getNotSuspended)(authorisedIndividualClient) should
            thenGo(AuthorisationRequestAlreadyResponded(dateString(DateTime.now()), personal))
        }
      }
    }

    "at MultiConsent" should {
      "transition to CheckAnswers when all consents are given" in {
        given(
          MultiConsent(
            personal,
            "uid",
            "agent name",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false),
              ClientConsent(invitationIdIrv, expiryDate, "afi", consent = false),
              ClientConsent(invitationIdVat, expiryDate, "vat", consent = false),
              ClientConsent(invitationIdTrust, expiryDate, "trust", consent = false),
              ClientConsent(invitationIdTrustNT, expiryDate, "trustNT", consent = false),
              ClientConsent(invitationIdTrust, expiryDate, "cgt", consent = false)
            )
          )) when
          submitConsents(authorisedIndividualClient)(
            ConfirmedTerms(itsaConsent = true, afiConsent = true, vatConsent = true, trustConsent = true, trustNTConsent = true, cgtConsent = true)) should
          thenGo(
            CheckAnswers(
              personal,
              "uid",
              "agent name",
              Seq(
                ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = true),
                ClientConsent(invitationIdIrv, expiryDate, "afi", consent = true),
                ClientConsent(invitationIdVat, expiryDate, "vat", consent = true),
                ClientConsent(invitationIdTrust, expiryDate, "trust", consent = true),
                ClientConsent(invitationIdTrustNT, expiryDate, "trustNT", consent = false),
                ClientConsent(invitationIdTrust, expiryDate, "cgt", consent = true)
              )
            )
          )
      }
      "throw an exception when the message key is not supported in the request" in {
        intercept[IllegalStateException] {
          given(
            MultiConsent(
              personal,
              "uid",
              "agent name",
              Seq(
                ClientConsent(invitationIdItsa, expiryDate, "foo", consent = false)
              )
            )) when
            submitConsents(authorisedIndividualClient)(
              ConfirmedTerms(itsaConsent = true, afiConsent = true, vatConsent = true, trustConsent = true, trustNTConsent = true, cgtConsent = true))
        }.getMessage shouldBe "the service key was not supported"
      }
    }
    "at SingleConsent" should {
      "transition to CheckAnswers with changed itsa consent" in {
        given(
          SingleConsent(
            personal,
            uid,
            "agent name",
            ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false),
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false),
              ClientConsent(invitationIdIrv, expiryDate, "afi", consent = true),
              ClientConsent(invitationIdVat, expiryDate, "vat", consent = true)
            )
          )) when submitChangeConsents(authorisedIndividualClient)(
          ConfirmedTerms(itsaConsent = true, afiConsent = false, vatConsent = false, trustConsent = true, trustNTConsent = true, cgtConsent = false)) should thenGo(
          CheckAnswers(
            personal,
            uid,
            "agent name",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = true),
              ClientConsent(invitationIdIrv, expiryDate, "afi", consent = true),
              ClientConsent(invitationIdVat, expiryDate, "vat", consent = true)
            )
          )
        )
      }
      "transition to CheckAnswers with changed irv consent" in {
        given(
          SingleConsent(
            personal,
            uid,
            "agent name",
            ClientConsent(invitationIdIrv, expiryDate, "afi", consent = false),
            Seq(ClientConsent(invitationIdIrv, expiryDate, "afi", consent = false))
          )) when submitChangeConsents(authorisedIndividualClient)(
          ConfirmedTerms(itsaConsent = false, afiConsent = true, vatConsent = false, trustConsent = false, trustNTConsent = false, cgtConsent = false)) should thenGo(
          CheckAnswers(personal, uid, "agent name", Seq(ClientConsent(invitationIdIrv, expiryDate, "afi", consent = true)))
        )
      }
      "transition to CheckAnswers with changed vat consent" in {
        given(
          SingleConsent(
            personal,
            uid,
            "agent name",
            ClientConsent(invitationIdVat, expiryDate, "vat", consent = false),
            Seq(ClientConsent(invitationIdVat, expiryDate, "vat", consent = false))
          )) when submitChangeConsents(authorisedIndividualClient)(
          ConfirmedTerms(itsaConsent = false, afiConsent = false, vatConsent = true, trustConsent = false, trustNTConsent = false, cgtConsent = false)) should thenGo(
          CheckAnswers(personal, uid, "agent name", Seq(ClientConsent(invitationIdVat, expiryDate, "vat", consent = true)))
        )
      }
      "transition to CheckAnswers with changed trust consent" in {
        given(
          SingleConsent(
            personal,
            uid,
            "agent name",
            ClientConsent(invitationIdTrust, expiryDate, "trust", consent = false),
            Seq(ClientConsent(invitationIdTrust, expiryDate, "trust", consent = false))
          )) when submitChangeConsents(authorisedIndividualClient)(
          ConfirmedTerms(itsaConsent = false, afiConsent = false, vatConsent = false, trustConsent = true, trustNTConsent = true, cgtConsent = false)) should thenGo(
          CheckAnswers(personal, uid, "agent name", Seq(ClientConsent(invitationIdTrust, expiryDate, "trust", consent = true)))
        )
      }

      "transition to CheckAnswers with changed cgt consent" in {
        given(
          SingleConsent(
            personal,
            uid,
            "agent name",
            ClientConsent(invitationIdCgt, expiryDate, "cgt", consent = false),
            Seq(ClientConsent(invitationIdCgt, expiryDate, "cgt", consent = false))
          )) when submitChangeConsents(authorisedIndividualClient)(
          ConfirmedTerms(itsaConsent = false, afiConsent = false, vatConsent = false, trustConsent = false, trustNTConsent = false, cgtConsent = true)) should thenGo(
          CheckAnswers(personal, uid, "agent name", Seq(ClientConsent(invitationIdCgt, expiryDate, "cgt", consent = true)))
        )
      }
    }
    "at CheckAnswers" should {
      "transition to InvitationsAccepted if all invitations are successfully accepted" in {
        def acceptInvitation(invitationId: InvitationId)(agencyName: String) = Future(true)
        def rejectInvitation(invitationId: InvitationId)(agencyName: String) = Future(true)

        given(
          CheckAnswers(
            personal,
            uid,
            "agent name",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = true),
              ClientConsent(invitationIdIrv, expiryDate, "afi", consent = true),
              ClientConsent(invitationIdVat, expiryDate, "vat", consent = true)
            )
          )) when submitCheckAnswers(acceptInvitation)(rejectInvitation)(authorisedIndividualClient) should thenGo(
          InvitationsAccepted(
            "agent name",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = true),
              ClientConsent(invitationIdIrv, expiryDate, "afi", consent = true),
              ClientConsent(invitationIdVat, expiryDate, "vat", consent = true)
            ),
            personal
          ))
      }
      "transition to InvitationsDeclined if all invitations are successfully declined" in {
        def acceptInvitation(invitationId: InvitationId)(agencyName: String) = Future(true)
        def rejectInvitation(invitationId: InvitationId)(agencyName: String) = Future(true)

        given(
          CheckAnswers(
            personal,
            uid,
            "agent name",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false),
              ClientConsent(invitationIdIrv, expiryDate, "afi", consent = false),
              ClientConsent(invitationIdVat, expiryDate, "vat", consent = false),
              ClientConsent(invitationIdTrust, expiryDate, "trust", consent = false)
            )
          )) when submitCheckAnswers(acceptInvitation)(rejectInvitation)(authorisedIndividualClient) should thenGo(
          InvitationsDeclined(
            "agent name",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false),
              ClientConsent(invitationIdIrv, expiryDate, "afi", consent = false),
              ClientConsent(invitationIdVat, expiryDate, "vat", consent = false),
              ClientConsent(invitationIdTrust, expiryDate, "trust", consent = false)
            ),
            personal
          ))
      }
      "transition to SomeResponsesFailed if some of the invitation acceptances fail" in {
        def acceptInvitation(invitationId: InvitationId)(agencyName: String) =
          if (invitationId == invitationIdItsa) Future(false) else Future(true)
        def rejectInvitation(invitationId: InvitationId)(agencyName: String) = Future(true)

        given(
          CheckAnswers(
            personal,
            uid,
            "agent name",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = true),
              ClientConsent(invitationIdIrv, expiryDate, "afi", consent = true),
              ClientConsent(invitationIdVat, expiryDate, "vat", consent = true)
            )
          )) when submitCheckAnswers(acceptInvitation)(rejectInvitation)(authorisedIndividualClient) should thenGo(
          SomeResponsesFailed(
            "agent name",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = true)
            ),
            Seq(
              ClientConsent(invitationIdIrv, expiryDate, "afi", consent = true, processed = true),
              ClientConsent(invitationIdVat, expiryDate, "vat", consent = true, processed = true)
            ),
            personal
          ))
      }
      "transition to AllResponsesFailed if all of the invitation acceptances fail" in {
        def acceptInvitation(invitationId: InvitationId)(agencyName: String) = Future(false)
        def rejectInvitation(invitationId: InvitationId)(agencyName: String) = Future(true)

        given(
          CheckAnswers(
            personal,
            uid,
            "agent name",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = true),
              ClientConsent(invitationIdIrv, expiryDate, "afi", consent = true),
              ClientConsent(invitationIdVat, expiryDate, "vat", consent = true)
            )
          )) when submitCheckAnswers(acceptInvitation)(rejectInvitation)(authorisedIndividualClient) should thenGo(AllResponsesFailed)
      }
    }

    "at state CheckAnswers" should {
      "transition to SingleConsent " in {
        given(CheckAnswers(personal, "uid", "agent name", Seq(ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = true)))) when
          submitCheckAnswersChange("itsa")(authorisedIndividualClient) should thenGo(
          SingleConsent(
            personal,
            "uid",
            "agent name",
            ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = true),
            Seq(ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = true))
          ))
      }
      "throw an exception when the key for the consent is not found" in {
        intercept[IllegalStateException] {
          given(CheckAnswers(personal, "uid", "agent name", Seq(ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = true)))) when
            submitCheckAnswersChange("foo")(authorisedIndividualClient)
        }.getMessage shouldBe "the key for this consent was not found"
      }
    }
    "at state ConfirmDecline" should {

      "transition to InvitationsDeclined if selected YES" in {
        def rejectInvitation(invitationId: InvitationId)(agencyName: String) = Future(true)

        given(
          ConfirmDecline(
            personal,
            uid,
            "agent pearson",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false),
              ClientConsent(invitationIdIrv, expiryDate, "afi", consent = false),
              ClientConsent(invitationIdVat, expiryDate, "vat", consent = false)
            )
          )) when
          submitConfirmDecline(rejectInvitation)(authorisedIndividualClient)(Confirmation(true)) should thenGo(
          InvitationsDeclined(
            "agent pearson",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false),
              ClientConsent(invitationIdIrv, expiryDate, "afi", consent = false),
              ClientConsent(invitationIdVat, expiryDate, "vat", consent = false)
            ),
            personal
          )
        )
      }

      "transition to MultiConsent if selected NO" in {
        def rejectInvitation(invitationId: InvitationId)(agencyName: String) = Future(true)

        given(
          ConfirmDecline(
            personal,
            uid,
            "agent pearson",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false),
              ClientConsent(invitationIdIrv, expiryDate, "afi", consent = false),
              ClientConsent(invitationIdVat, expiryDate, "vat", consent = false)
            )
          )) when
          submitConfirmDecline(rejectInvitation)(authorisedIndividualClient)(Confirmation(false)) should thenGo(
          MultiConsent(
            personal,
            uid,
            "agent pearson",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false),
              ClientConsent(invitationIdIrv, expiryDate, "afi", consent = false),
              ClientConsent(invitationIdVat, expiryDate, "vat", consent = false)
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
            Seq(ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = true)),
            Seq(ClientConsent(invitationIdIrv, expiryDate, "afi", consent = true, processed = true)),
            personal
          )) when continueSomeResponsesFailed(authorisedIndividualClient) should thenGo(
          InvitationsAccepted(
            "Mr agent",
            Seq(ClientConsent(InvitationId("B1BEOZEO7MNO6"), expiryDate, "afi", consent = true, processed = true)),
            personal))
      }
    }
  }
}
