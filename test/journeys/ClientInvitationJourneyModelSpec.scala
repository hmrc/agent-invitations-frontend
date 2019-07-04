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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.Transition
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.root
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models.{ConfirmedTerms, _}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId}
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

  val authorisedIndividualClient = AuthorisedClient("Individual", Seq(("personal", "clientId")))
  val authorisedBusinessClient = AuthorisedClient("Organisation", Seq(("business", "clientId")))
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
            thenGo(WarmUp(personal, uid, agentName, normalisedAgentName))
        }

        "the affinity group does not match the client type" in {
          given(MissingJourneyHistory) when start("personal", uid, normalisedAgentName)(getAgentReferenceRecord)(
            getAgencyName) should
            thenGo(WarmUp(personal, uid, agentName, normalisedAgentName))
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

          given(WarmUp(personal, uid, agentName, normalisedAgentName)) when
            submitWarmUp(getPendingInvitationIdsAndExpiryDates)(authorisedIndividualClient) should
            thenGo(
              MultiConsent(
                personal,
                uid,
                agentName,
                Seq(ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false))))
        }

        "transition to NotFoundInvitation when the invitation is not found" in {
          def getPendingInvitationIdsAndExpiryDates(uid: String, status: InvitationStatus) = Future(Seq.empty)

          given(WarmUp(personal, uid, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(getPendingInvitationIdsAndExpiryDates)(authorisedIndividualClient) should
            thenGo(NotFoundInvitation)
        }

        "transition to IncorrectClientType when the affinity group does not match the client type" in {
          def getPendingInvitationIdsAndExpiryDates(uid: String, status: InvitationStatus) =
            Future.failed[Seq[InvitationIdAndExpiryDate]](new Throwable("should not be called"))

          given(WarmUp(personal, uid, agentName, normalisedAgentName)) when
            submitWarmUp(getPendingInvitationIdsAndExpiryDates)(authorisedBusinessClient) should
            thenGo(IncorrectClientType(personal))
        }
      }
      "submitting intent to decline" should {
        "transition to ConfirmDecline when the invitation is found" in {
          def getPendingInvitationIdsAndExpiryDates(uid: String, status: InvitationStatus) =
            Future(Seq(InvitationIdAndExpiryDate(invitationIdItsa, expiryDate)))

          given(WarmUp(personal, uid, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(getPendingInvitationIdsAndExpiryDates)(authorisedIndividualClient) should
            thenGo(
              ConfirmDecline(
                personal,
                uid,
                agentName,
                Seq(ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false))))
        }

        "transition to NotFoundInvitation when the invitation is not found" in {
          def getPendingInvitationIdsAndExpiryDates(uid: String, status: InvitationStatus) = Future(Seq.empty)

          given(WarmUp(personal, uid, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(getPendingInvitationIdsAndExpiryDates)(authorisedIndividualClient) should
            thenGo(NotFoundInvitation)
        }

        "transition to IncorrectClientType when the affinity group does not match the client type" in {
          def getPendingInvitationIdsAndExpiryDates(uid: String, status: InvitationStatus) =
            Future.failed[Seq[InvitationIdAndExpiryDate]](new Throwable("should not be called"))

          given(WarmUp(personal, uid, agentName, normalisedAgentName)) when
            submitWarmUpToDecline(getPendingInvitationIdsAndExpiryDates)(authorisedBusinessClient) should
            thenGo(IncorrectClientType(personal))
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
              ClientConsent(invitationIdVat, expiryDate, "vat", consent = false)
            )
          )) when
          submitConsents(authorisedIndividualClient)(
            ConfirmedTerms(itsaConsent = true, afiConsent = true, vatConsent = true, trustConsent = true)) should
          thenGo(
            CheckAnswers(
              personal,
              "uid",
              "agent name",
              Seq(
                ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = true),
                ClientConsent(invitationIdIrv, expiryDate, "afi", consent = true),
                ClientConsent(invitationIdVat, expiryDate, "vat", consent = true)
              )
            )
          )
      }
    }
    "at SingleConsent" should {
      "transition to CheckAnswers with changed consent" in {
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
          )) when submitChangeConsents(authorisedIndividualClient)(ConfirmedTerms(
          itsaConsent = true,
          afiConsent = false,
          vatConsent = false,
          trustConsent = true)) should thenGo(
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
    }
    "at CheckAnswers" should {
      "transition to InvitationsAccepted if all invitations are successfully accepted" in {
        def acceptInvitation(invitationId: InvitationId) = Future(true)
        def rejectInvitation(invitationId: InvitationId) = Future(true)

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
            )
          ))
      }
      "transition to InvitationsDeclined if all invitations are successfully declined" in {
        def acceptInvitation(invitationId: InvitationId) = Future(true)
        def rejectInvitation(invitationId: InvitationId) = Future(true)

        given(
          CheckAnswers(
            personal,
            uid,
            "agent name",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false),
              ClientConsent(invitationIdIrv, expiryDate, "afi", consent = false),
              ClientConsent(invitationIdVat, expiryDate, "vat", consent = false)
            )
          )) when submitCheckAnswers(acceptInvitation)(rejectInvitation)(authorisedIndividualClient) should thenGo(
          InvitationsDeclined(
            "agent name",
            Seq(
              ClientConsent(invitationIdItsa, expiryDate, "itsa", consent = false),
              ClientConsent(invitationIdIrv, expiryDate, "afi", consent = false),
              ClientConsent(invitationIdVat, expiryDate, "vat", consent = false)
            )
          ))
      }
      "transition to SomeResponsesFailed if some of the invitation acceptances fail" in {
        def acceptInvitation(invitationId: InvitationId) =
          if (invitationId == invitationIdItsa) Future(false) else Future(true)
        def rejectInvitation(invitationId: InvitationId) = Future(true)

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
            )
          ))
      }
      "transition to AllResponsesFailed if all of the invitation acceptances fail" in {
        def acceptInvitation(invitationId: InvitationId) = Future(false)
        def rejectInvitation(invitationId: InvitationId) = Future(true)

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
    "at state ConfirmDecline" should {

      "transition to InvitationsDeclined if selected YES" in {
        def rejectInvitation(invitationId: InvitationId) = Future(true)

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
            )
          )
        )
      }

      "transition to MultiConsent if selected NO" in {
        def rejectInvitation(invitationId: InvitationId) = Future(true)

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
            Seq(ClientConsent(invitationIdIrv, expiryDate, "afi", consent = true, processed = true))
          )) when continueSomeResponsesFailed(authorisedIndividualClient) should thenGo(
          InvitationsAccepted(
            "Mr agent",
            Seq(ClientConsent(InvitationId("B1BEOZEO7MNO6"), expiryDate, "afi", consent = true, processed = true))))
      }
    }
  }
}
