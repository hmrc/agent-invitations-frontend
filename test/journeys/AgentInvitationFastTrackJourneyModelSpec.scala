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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.States._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.{State, Transition}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AgentInvitationFastTrackJourneyModelSpec extends UnitSpec with StateMatchers[State] {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  case class given(initialState: State)
      extends AgentInvitationFastTrackJourneyService with TestStorage[(State, List[State])] {
    await(save((initialState, Nil)))

    def when(transition: Transition): (State, List[State]) =
      await(super.apply(transition))
  }

  val authorisedAgent = AuthorisedAgent(Arn("TARN0000001"), isWhitelisted = true)
  val availableServices = Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)
  val featureFlags = FeatureFlags()

  val nino = "AB123456A"
  val postCode = Some("BN114AW")
  val vrn = "123456"
  val vatRegDate = Some("2010-10-10")
  val dob = Some("1990-10-10")

  "AgentInvitationFastTrackJourneyService" when {
    "at state CheckDetails" should {
      def clientName(service: String, clientId: String) = Future(Some("Piglet"))
      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def createInvitation(arn: Arn, invitation: Invitation): Future[InvitationId] =
        Future(InvitationId("ABBTAKTMFKWU8"))
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(true))
      def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(204))
      def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(true))

      "transition to InvitationSentPersonal if all fields are present, no pending or active invitations and known facts match for itsa" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None))
      }
      "transition to InvitationSentPersonal if all fields are present, no pending or active invitations and known facts match for itsa if feature flag is off" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags.copy(showKfcMtdIt = false))(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None))
      }
      "transition to InvitationSentPersonal if all fields are present, no pending or active invitations and known facts match for irv" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None))
      }
      "transition to InvitationSentPersonal if all fields are present, no pending or active invitations and known facts match for irv if feature flag is off" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags.copy(showKfcPersonalIncome = false))(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None))
      }
      "transition to InvitationSentPersonal if all fields are present, no pending or active invitations and known facts match for personal vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ni", nino, vatRegDate)

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None))
      }
      "transition to InvitationSentPersonal if all fields are present, no pending or active invitations and known facts match for personal vat if feature flag is off" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ni", nino, vatRegDate)

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags.copy(showKfcMtdVat = false))(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None))
      }
      "transition to InvitationSentBusiness if all fields are present, no pending or active invitations and known facts match for business vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "ni", nino, vatRegDate)

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentBusiness("invitation/link", None))
      }
      "transition to InvitationSentBusiness if all fields are present, no pending or active invitations and known facts match for business vat if feature flag is off" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "ni", nino, vatRegDate)

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags.copy(showKfcMtdVat = false))(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentBusiness("invitation/link", None))
      }
      "transition to IdentifyPersonalClient when the form is false for itsa" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient when the form is false for irv" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient when the form is false for personal vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ni", nino, vatRegDate)

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient when the form is false for business vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "ni", nino, vatRegDate)

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyBusinessClient(fastTrackRequest, None))
      }
      "transition to knownFactNotMatched when the clientIdentifier and known fact do not match for itsa" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)
        def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(false))

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(KnownFactNotMatched(fastTrackRequest))
      }
      "transition to knownFactNotMatched when the clientIdentifier and known fact do not match for irv" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)
        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(false))

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(KnownFactNotMatched(fastTrackRequest))
      }
      "transition to knownFactNotMatched when the clientIdentifier and known fact do not match for vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ni", nino, vatRegDate)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(400))

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(KnownFactNotMatched(fastTrackRequest))
      }
      "transition to ClientNotSignedUp when the client is not enrolled for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)
        def checkPostcodeMatches(nino: Nino, postcode: String) = Future(None)

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientNotSignedUp(fastTrackRequest))
      }
      "transition to ClientNotSignedUp when the client is not enrolled for afi service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)
        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(None)

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientNotSignedUp(fastTrackRequest))
      }
      "transition to ClientNotSignedUp when the client is not enrolled for vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ni", nino, vatRegDate)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(None)

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientNotSignedUp(fastTrackRequest))
      }
      "transition to PendingInvitationExists when there is already a pending invitation for this request" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)
        def hasPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(true)

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(PendingInvitationExists(fastTrackRequest, None))
      }
      "transition to ActiveAuthorisationExists when there is already an active relationship between agent and client for this service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)
        def hasActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(true)

        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasActiveRelationship)(
          featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(ActiveAuthorisationExists(fastTrackRequest))
      }
      "transition to MoreDetails when there is no known fact in the request" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, None)
        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(MoreDetails(fastTrackRequest, None))
      }
      "transition to SelectClientType when there is no client type in the request" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCMTDIT, "ni", nino, postCode)
        given(CheckDetails(fastTrackRequest, None)) when checkedDetails(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(SelectClientType(fastTrackRequest, None))
      }
    }
    "at IdentifyPersonalClient" should {
      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def createInvitation(arn: Arn, invitation: Invitation): Future[InvitationId] =
        Future(InvitationId("ABBTAKTMFKWU8"))
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(true))
      def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(204))
      def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(true))
      "transition to InvitationSent for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)

        given(IdentifyPersonalClient(fastTrackRequest, None)) when identifiedClientItsa(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(ItsaClient("AB123456C", Some("BN32TM"))) should
          thenGo(InvitationSentPersonal("invitation/link", None))
      }
      "transition to InvitationSent for irv service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)

        given(IdentifyPersonalClient(fastTrackRequest, None)) when identifiedClientIrv(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(IrvClient("AB123456C", Some("1990-10-10"))) should
          thenGo(InvitationSentPersonal("invitation/link", None))
      }
      "transition to InvitationSent for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, vatRegDate)

        given(IdentifyPersonalClient(fastTrackRequest, None)) when identifiedClientVat(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(VatClient("1234567", Some("2010-10-10"))) should
          thenGo(InvitationSentPersonal("invitation/link", None))
      }
      "transition to InvitationSent for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "vrn", vrn, vatRegDate)

        given(IdentifyBusinessClient(fastTrackRequest, None)) when identifiedClientVat(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(VatClient("1234567", Some("2010-10-10"))) should
          thenGo(InvitationSentBusiness("invitation/link", None))
      }
    }
    "at MoreDetails" should {
      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def createInvitation(arn: Arn, invitation: Invitation): Future[InvitationId] =
        Future(InvitationId("ABBTAKTMFKWU8"))
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(true))
      def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(204))
      def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(true))
      "transition to InvitationSent for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)

        given(MoreDetails(fastTrackRequest, None)) when moreDetailsItsa(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Some("BN114AW")) should
          thenGo(InvitationSentPersonal("invitation/link", None))
      }
      "transition to InvitationSent for irv service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)

        given(MoreDetails(fastTrackRequest, None)) when moreDetailsItsa(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Some("1991-10-10")) should
          thenGo(InvitationSentPersonal("invitation/link", None))
      }
      "transition to InvitationSent for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, vatRegDate)

        given(MoreDetails(fastTrackRequest, None)) when moreDetailsVat(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Some("2011-10-10")) should
          thenGo(InvitationSentPersonal("invitation/link", None))
      }
      "transition to InvitationSent for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "vrn", vrn, vatRegDate)

        given(MoreDetails(fastTrackRequest, None)) when moreDetailsVat(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(Some("2011-10-10")) should
          thenGo(InvitationSentBusiness("invitation/link", None))
      }
    }
    "at SelectClientType" should {
      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def createInvitation(arn: Arn, invitation: Invitation): Future[InvitationId] =
        Future(InvitationId("ABBTAKTMFKWU8"))
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(true))
      def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(204))
      def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(true))
      "transition to InvitationSent for vat service when there is a known fact present" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCMTDVAT, "vrn", vrn, vatRegDate)

        given(SelectClientType(fastTrackRequest, None)) when selectedClientType(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(personal) should
          thenGo(InvitationSentPersonal("invitation/link", None))
      }
      "transition to MoreDetails for vat service when there is no known fact" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCMTDVAT, "vrn", vrn, None)

        given(SelectClientType(fastTrackRequest, None)) when selectedClientType(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(hasNoPendingInvitation)(hasNoActiveRelationship)(
          featureFlags)(authorisedAgent)(personal) should
          thenGo(MoreDetails(fastTrackRequest.copy(clientType = Some(personal)), None))
      }
    }
  }
}
