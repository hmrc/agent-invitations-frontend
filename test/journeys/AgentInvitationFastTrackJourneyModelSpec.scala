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
import play.api.test.FakeRequest
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.State._
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
    "at state Prologue" should {
      "transition to CheckDetailsCompleteItsa when all required fields are present for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCMTDIT, "ni", nino, postCode)

        given(Prologue(None)) when start(featureFlags)(None)(authorisedAgent)(fastTrackRequest)(
          FakeRequest(),
          HeaderCarrier()) should
          thenGo(CheckDetailsCompleteItsa(fastTrackRequest.copy(clientType = Some(personal)), None))
      }
      "transition to CheckDetailsNoPostcode when the postcode is missing for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCMTDIT, "ni", nino, None)

        given(Prologue(None)) when start(featureFlags)(None)(authorisedAgent)(fastTrackRequest)(
          FakeRequest(),
          HeaderCarrier()) should
          thenGo(CheckDetailsNoPostcode(fastTrackRequest.copy(clientType = Some(personal)), None))
      }
      "transition to CheckDetailsCompleteIrv when all required fields are present for irv service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCPIR, "ni", nino, dob)

        given(Prologue(None)) when start(featureFlags)(None)(authorisedAgent)(fastTrackRequest)(
          FakeRequest(),
          HeaderCarrier()) should
          thenGo(CheckDetailsCompleteIrv(fastTrackRequest.copy(clientType = Some(personal)), None))
      }
      "transition to CheckDetailsNoDob when there is no dob for irv service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCPIR, "ni", nino, None)

        given(Prologue(None)) when start(featureFlags)(None)(authorisedAgent)(fastTrackRequest)(
          FakeRequest(),
          HeaderCarrier()) should
          thenGo(CheckDetailsNoDob(fastTrackRequest.copy(clientType = Some(personal)), None))
      }
      "transition to CheckDetailsCompleteVat when all required fields are present for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ni", nino, vatRegDate)

        given(Prologue(None)) when start(featureFlags)(None)(authorisedAgent)(fastTrackRequest)(
          FakeRequest(),
          HeaderCarrier()) should
          thenGo(CheckDetailsCompletePersonalVat(fastTrackRequest, None))
      }
      "transition to CheckDetailsNoVatRegDate when there is no vat reg date for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ni", nino, None)

        given(Prologue(None)) when start(featureFlags)(None)(authorisedAgent)(fastTrackRequest)(
          FakeRequest(),
          HeaderCarrier()) should
          thenGo(CheckDetailsNoVatRegDate(fastTrackRequest, None))
      }
      "transition to CheckDetailsCompleteVat when all required fields are present for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "ni", nino, vatRegDate)

        given(Prologue(None)) when start(featureFlags)(None)(authorisedAgent)(fastTrackRequest)(
          FakeRequest(),
          HeaderCarrier()) should
          thenGo(CheckDetailsCompleteBusinessVat(fastTrackRequest, None))
      }
      "transition to CheckDetailsNoVatRegDate when there is no vat reg date for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "ni", nino, None)

        given(Prologue(None)) when start(featureFlags)(None)(authorisedAgent)(fastTrackRequest)(
          FakeRequest(),
          HeaderCarrier()) should
          thenGo(CheckDetailsNoVatRegDate(fastTrackRequest, None))
      }
      "transition to CheckDetailsNoClientTypeVat when there is no client type for vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCMTDVAT, "ni", nino, None)

        given(Prologue(None)) when start(featureFlags)(None)(authorisedAgent)(fastTrackRequest)(
          FakeRequest(),
          HeaderCarrier()) should
          thenGo(CheckDetailsNoClientTypeVat(fastTrackRequest, None))
      }
    }

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
      def getAgencyEmail() = Future(Some("abc@xyz.com"))

      "transition to InvitationSentPersonal if all fields are present, no pending or active invitations and known facts match for itsa" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)

        given(CheckDetailsCompleteItsa(fastTrackRequest, None)) when checkedDetailsAllInformation(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSentPersonal if all fields are present, no pending or active invitations and known facts match for itsa if feature flag is off" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)

        given(CheckDetailsCompleteItsa(fastTrackRequest, None)) when checkedDetailsAllInformation(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags.copy(showKfcMtdIt = false))(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSentPersonal if all fields are present, no pending or active invitations and known facts match for irv" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)

        given(CheckDetailsCompleteIrv(fastTrackRequest, None)) when checkedDetailsAllInformation(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSentPersonal if all fields are present, no pending or active invitations and known facts match for irv if feature flag is off" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)

        given(CheckDetailsCompleteIrv(fastTrackRequest, None)) when checkedDetailsAllInformation(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags.copy(showKfcPersonalIncome = false))(authorisedAgent)(
          Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSentPersonal if all fields are present, no pending or active invitations and known facts match for personal vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ni", nino, vatRegDate)

        given(CheckDetailsCompletePersonalVat(fastTrackRequest, None)) when checkedDetailsAllInformation(
          checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSentPersonal if all fields are present, no pending or active invitations and known facts match for personal vat if feature flag is off" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ni", nino, vatRegDate)

        given(CheckDetailsCompletePersonalVat(fastTrackRequest, None)) when checkedDetailsAllInformation(
          checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(featureFlags.copy(showKfcMtdVat = false))(authorisedAgent)(
          Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSentBusiness if all fields are present, no pending or active invitations and known facts match for business vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "ni", nino, vatRegDate)

        given(CheckDetailsCompleteBusinessVat(fastTrackRequest, None)) when checkedDetailsAllInformation(
          checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentBusiness("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSentBusiness if all fields are present, no pending or active invitations and known facts match for business vat if feature flag is off" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "ni", nino, vatRegDate)

        given(CheckDetailsCompleteBusinessVat(fastTrackRequest, None)) when checkedDetailsAllInformation(
          checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(featureFlags.copy(showKfcMtdVat = false))(authorisedAgent)(
          Confirmation(true)) should
          thenGo(InvitationSentBusiness("invitation/link", None, "abc@xyz.com"))
      }
      "transition to IdentifyPersonalClient when the form is false for itsa" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)

        given(CheckDetailsCompleteItsa(fastTrackRequest, None)) when checkedDetailsAllInformation(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient when the form is false for irv" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)

        given(CheckDetailsCompleteIrv(fastTrackRequest, None)) when checkedDetailsAllInformation(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient when the form is false for personal vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ni", nino, vatRegDate)

        given(CheckDetailsCompletePersonalVat(fastTrackRequest, None)) when checkedDetailsAllInformation(
          checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(featureFlags)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient when the form is false for business vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "ni", nino, vatRegDate)

        given(CheckDetailsCompleteBusinessVat(fastTrackRequest, None)) when checkedDetailsAllInformation(
          checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(featureFlags)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyBusinessClient(fastTrackRequest, None))
      }
      "transition to knownFactNotMatched when the clientIdentifier and known fact do not match for itsa" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)
        def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(false))

        given(CheckDetailsCompleteItsa(fastTrackRequest, None)) when checkedDetailsAllInformation(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(KnownFactNotMatched(fastTrackRequest, None))
      }
      "transition to knownFactNotMatched when the clientIdentifier and known fact do not match for irv" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)
        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(false))

        given(CheckDetailsCompleteIrv(fastTrackRequest, None)) when checkedDetailsAllInformation(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(KnownFactNotMatched(fastTrackRequest, None))
      }
      "transition to knownFactNotMatched when the clientIdentifier and known fact do not match for vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ni", nino, vatRegDate)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(400))

        given(CheckDetailsCompletePersonalVat(fastTrackRequest, None)) when checkedDetailsAllInformation(
          checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(KnownFactNotMatched(fastTrackRequest, None))
      }
      "transition to ClientNotSignedUp when the client is not enrolled for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)
        def checkPostcodeMatches(nino: Nino, postcode: String) = Future(None)

        given(CheckDetailsCompleteItsa(fastTrackRequest, None)) when checkedDetailsAllInformation(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientNotSignedUp(fastTrackRequest, None))
      }
      "transition to ClientNotSignedUp when the client is not enrolled for afi service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)
        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(None)

        given(CheckDetailsCompleteIrv(fastTrackRequest, None)) when checkedDetailsAllInformation(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientNotSignedUp(fastTrackRequest, None))
      }
      "transition to ClientNotSignedUp when the client is not enrolled for vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ni", nino, vatRegDate)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(None)

        given(CheckDetailsCompletePersonalVat(fastTrackRequest, None)) when checkedDetailsAllInformation(
          checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientNotSignedUp(fastTrackRequest, None))
      }
      "transition to PendingInvitationExists when there is already a pending invitation for this request" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)
        def hasPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(true)

        given(CheckDetailsCompleteItsa(fastTrackRequest, None)) when checkedDetailsAllInformation(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(PendingInvitationExists(fastTrackRequest, None))
      }
      "transition to ActiveAuthorisationExists when there is already an active relationship between agent and client for this service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)
        def hasActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(true)

        given(CheckDetailsCompleteItsa(fastTrackRequest, None)) when checkedDetailsAllInformation(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasActiveRelationship)(featureFlags)(authorisedAgent)(Confirmation(true)) should
          thenGo(ActiveAuthorisationExists(fastTrackRequest, None))
      }
      "transition to NoPostcode when there is no known fact in the request" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, None)
        given(CheckDetailsNoPostcode(fastTrackRequest, None)) when checkedDetailsNoKnownFact(authorisedAgent) should
          thenGo(NoPostcode(fastTrackRequest, None))
      }
      "transition to SelectClientType when there is no client type in the request" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCMTDIT, "ni", nino, postCode)
        given(CheckDetailsNoClientTypeVat(fastTrackRequest, None)) when checkedDetailsNoClientType(authorisedAgent) should
          thenGo(SelectClientTypeVat(fastTrackRequest, None))
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
      def getAgencyEmail() = Future(Some("abc@xyz.com"))
      "transition to InvitationSent for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)

        given(IdentifyPersonalClient(fastTrackRequest, None)) when identifiedClientItsa(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(ItsaClient("AB123456C", Some("BN32TM"))) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSent for irv service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)

        given(IdentifyPersonalClient(fastTrackRequest, None)) when identifiedClientIrv(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(IrvClient("AB123456C", Some("1990-10-10"))) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSent for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, vatRegDate)

        given(IdentifyPersonalClient(fastTrackRequest, None)) when identifiedClientVat(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(VatClient("1234567", Some("2010-10-10"))) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSent for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "vrn", vrn, vatRegDate)

        given(IdentifyBusinessClient(fastTrackRequest, None)) when identifiedClientVat(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(VatClient("1234567", Some("2010-10-10"))) should
          thenGo(InvitationSentBusiness("invitation/link", None, "abc@xyz.com"))
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
      def getAgencyEmail() = Future(Some("abc@xyz.com"))
      "transition to InvitationSent for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)

        given(NoPostcode(fastTrackRequest, None)) when moreDetailsItsa(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(Some("BN114AW")) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSent for irv service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)

        given(NoDob(fastTrackRequest, None)) when moreDetailsIrv(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(Some("1991-10-10")) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSent for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, vatRegDate)

        given(NoVatRegDate(fastTrackRequest, None)) when moreDetailsVat(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(Some("2011-10-10")) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSent for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "vrn", vrn, vatRegDate)

        given(NoVatRegDate(fastTrackRequest, None)) when moreDetailsVat(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(Some("2011-10-10")) should
          thenGo(InvitationSentBusiness("invitation/link", None, "abc@xyz.com"))
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
      def getAgencyEmail() = Future(Some("abc@xyz.com"))

      "transition to InvitationSent for vat service when there is a known fact present" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCMTDVAT, "vrn", vrn, vatRegDate)

        given(SelectClientTypeVat(fastTrackRequest, None)) when selectedClientType(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(personal) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to MoreDetails for vat service when there is no known fact" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCMTDVAT, "vrn", vrn, None)

        given(SelectClientTypeVat(fastTrackRequest, None)) when selectedClientType(checkPostcodeMatches)(
          checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(featureFlags)(authorisedAgent)(personal) should
          thenGo(NoVatRegDate(fastTrackRequest.copy(clientType = Some(personal)), None))
      }
    }
  }
}
