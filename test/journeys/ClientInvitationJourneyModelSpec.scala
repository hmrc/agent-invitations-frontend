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

package journeys

import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.connectors.SuspendedServices
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.{State, Transition}
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCCGTPD, HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
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

  val authorisedIndividualClient = AuthorisedClient(AffinityGroup.Individual, Enrolments(Set(Enrolment("some-key"))))
  val authorisedBusinessClient = AuthorisedClient(Organisation, Enrolments(Set(Enrolment("some-key"))))
  val availableServices = Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)
  val featureFlags = FeatureFlags()

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
  val invitationIdCgt = InvitationId("E1BEOZEO7MNO6")
  val expiryDate = LocalDate.parse("2010-01-01")

  val normalisedAgentName = "agent-name"
  val agentName = "Agent Name"

  "ClientInvitationJourneyModel" when {
    "at any state" should {
      "transition to WarmUp with agency name" when {
        def getAgentReferenceRecord(uid: String) =
          Future(Some(AgentReferenceRecord("uid123", arn, Seq(normalisedAgentName, s"$normalisedAgentName-2"))))
        def getAgencyName(arn: Arn) = Future(agentName)

        "the affinity group does match the client type" in {
          given(MissingJourneyHistory) when start("personal", uid, normalisedAgentName)(getAgentReferenceRecord)(
            getAgencyName) should
            thenGo(WarmUp(personal, uid, arn, agentName, normalisedAgentName))
        }

        "the affinity group does not match the client type" in {
          given(MissingJourneyHistory) when start("personal", uid, normalisedAgentName)(getAgentReferenceRecord)(
            getAgencyName) should
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
          def getPendingInvitationIdsAndExpiryDates(uid: String, status: InvitationStatus) =
            Future(Seq(InvitationIdAndExpiryDate(invitationIdItsa, expiryDate)))
          def getNotSuspended(arn: Arn) = Future(SuspendedServices(Set.empty))

          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUp(agentSuspensionEnabled = true)(getPendingInvitationIdsAndExpiryDates, getNotSuspended)(
              authorisedIndividualClient) should
            thenGo(
              MultiConsent(
                personal,
                uid,
                agentName,
                Seq(ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false))))
        }

        "transition to NotFoundInvitation when the invitation is not found" in {
          def getPendingInvitationIdsAndExpiryDates(uid: String, status: InvitationStatus) = Future(Seq.empty)
          def getNotSuspended(arn: Arn) = Future(SuspendedServices(Set.empty))

          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(agentSuspensionEnabled = true)(
              getPendingInvitationIdsAndExpiryDates,
              getNotSuspended)(authorisedIndividualClient) should
            thenGo(NotFoundInvitation)
        }

        "transition to TrustNotClaimed when the invitation contains trust but the client doesn't have HMRC-TERS-ORG enrolment" in {
          def getPendingInvitationIdsAndExpiryDates(uid: String, status: InvitationStatus) =
            Future(Seq(InvitationIdAndExpiryDate(invitationIdTrust, expiryDate)))
          def getNotSuspended(arn: Arn) = Future(SuspendedServices(Set.empty))

          given(WarmUp(business, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUp(agentSuspensionEnabled = true)(getPendingInvitationIdsAndExpiryDates, getNotSuspended)(
              authorisedBusinessClient) should
            thenGo(TrustNotClaimed)
        }

        "transition to SuspendedAgent when agent is suspended for one or more of consent services" in {
          def getPendingInvitationIdsAndExpiryDates(uid: String, status: InvitationStatus) =
            Future(Seq(InvitationIdAndExpiryDate(invitationIdItsa, expiryDate)))
          def getSuspendedForItsa(arn: Arn) = Future(SuspendedServices(Set("HMRC-MTD-IT")))

          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUp(agentSuspensionEnabled = true)(getPendingInvitationIdsAndExpiryDates, getSuspendedForItsa)(
              authorisedIndividualClient) should
            thenGo(SuspendedAgent(Set("itsa")))
        }
      }
      "submitting intent to decline" should {
        "transition to ConfirmDecline when the invitation is found" in {
          def getPendingInvitationIdsAndExpiryDates(uid: String, status: InvitationStatus) =
            Future(Seq(InvitationIdAndExpiryDate(invitationIdItsa, expiryDate)))
          def getNotSuspended(arn: Arn) = Future(SuspendedServices(Set.empty))

          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(agentSuspensionEnabled = true)(
              getPendingInvitationIdsAndExpiryDates,
              getNotSuspended)(authorisedIndividualClient) should
            thenGo(
              ConfirmDecline(
                personal,
                uid,
                agentName,
                Seq(ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false))))
        }

        "transition to NotFoundInvitation when the invitation is not found" in {
          def getPendingInvitationIdsAndExpiryDates(uid: String, status: InvitationStatus) = Future(Seq.empty)
          def getNotSuspended(arn: Arn) = Future(SuspendedServices(Set.empty))

          given(WarmUp(personal, uid, arn, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(agentSuspensionEnabled = true)(
              getPendingInvitationIdsAndExpiryDates,
              getNotSuspended)(authorisedIndividualClient) should
            thenGo(NotFoundInvitation)
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
              ClientConsent(invitationIdTrust, expiryDate, "cgt", consent = false)
            )
          )) when
          submitConsents(authorisedIndividualClient)(
            ConfirmedTerms(
              itsaConsent = true,
              afiConsent = true,
              vatConsent = true,
              trustConsent = true,
              cgtConsent = true)) should
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
              ConfirmedTerms(
                itsaConsent = true,
                afiConsent = true,
                vatConsent = true,
                trustConsent = true,
                cgtConsent = true))
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
          ConfirmedTerms(
            itsaConsent = true,
            afiConsent = false,
            vatConsent = false,
            trustConsent = true,
            cgtConsent = false)) should thenGo(
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
          ConfirmedTerms(
            itsaConsent = false,
            afiConsent = true,
            vatConsent = false,
            trustConsent = false,
            cgtConsent = false)) should thenGo(
          CheckAnswers(
            personal,
            uid,
            "agent name",
            Seq(ClientConsent(invitationIdIrv, expiryDate, "afi", consent = true)))
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
          ConfirmedTerms(
            itsaConsent = false,
            afiConsent = false,
            vatConsent = true,
            trustConsent = false,
            cgtConsent = false)) should thenGo(
          CheckAnswers(
            personal,
            uid,
            "agent name",
            Seq(ClientConsent(invitationIdVat, expiryDate, "vat", consent = true)))
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
          ConfirmedTerms(
            itsaConsent = false,
            afiConsent = false,
            vatConsent = false,
            trustConsent = true,
            cgtConsent = false)) should thenGo(
          CheckAnswers(
            personal,
            uid,
            "agent name",
            Seq(ClientConsent(invitationIdTrust, expiryDate, "trust", consent = true)))
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
          ConfirmedTerms(
            itsaConsent = false,
            afiConsent = false,
            vatConsent = false,
            trustConsent = false,
            cgtConsent = true)) should thenGo(
          CheckAnswers(
            personal,
            uid,
            "agent name",
            Seq(ClientConsent(invitationIdCgt, expiryDate, "cgt", consent = true)))
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
          )) when submitCheckAnswers(acceptInvitation)(rejectInvitation)(authorisedIndividualClient) should thenGo(
          AllResponsesFailed)
      }
    }

    "at state CheckAnswers" should {
      "transition to SingleConsent " in {
        given(
          CheckAnswers(
            personal,
            "uid",
            "agent name",
            Seq(ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = true)))) when
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
          given(
            CheckAnswers(
              personal,
              "uid",
              "agent name",
              Seq(ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = true)))) when
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
