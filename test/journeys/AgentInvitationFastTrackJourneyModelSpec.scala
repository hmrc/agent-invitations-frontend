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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.{State, Transition}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR, TRUST}
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentFastTrackRequest, _}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, Utr, Vrn}
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
  val nino = "AB123456A"
  val postCode = Some("BN114AW")
  val vrn = "123456"
  val vatRegDate = Some("2010-10-10")
  val dob = Some("1990-10-10")
  val utr = Utr("1977030537")

  "AgentInvitationFastTrackJourneyService" when {
    "at state Prologue" should {
      "transition to CheckDetailsCompleteItsa when all required fields are present for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCMTDIT, "ni", nino, postCode)

        given(Prologue(None, None)) when start(None)(authorisedAgent)(fastTrackRequest)(FakeRequest(), HeaderCarrier()) should
          thenGo(
            CheckDetailsCompleteItsa(
              originalFastTrackRequest = fastTrackRequest.copy(clientType = Some(personal)),
              fastTrackRequest = fastTrackRequest.copy(clientType = Some(personal)),
              None
            ))
      }
      "transition to CheckDetailsNoPostcode when the postcode is missing for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCMTDIT, "ni", nino, None)

        given(Prologue(None, None)) when start(None)(authorisedAgent)(fastTrackRequest)(FakeRequest(), HeaderCarrier()) should
          thenGo(
            CheckDetailsNoPostcode(
              originalFastTrackRequest = fastTrackRequest.copy(clientType = Some(personal)),
              fastTrackRequest = fastTrackRequest.copy(clientType = Some(personal)),
              None
            ))
      }
      "transition to CheckDetailsCompleteIrv when all required fields are present for irv service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCPIR, "ni", nino, dob)

        given(Prologue(None, None)) when start(None)(authorisedAgent)(fastTrackRequest)(FakeRequest(), HeaderCarrier()) should
          thenGo(
            CheckDetailsCompleteIrv(
              originalFastTrackRequest = fastTrackRequest.copy(clientType = Some(personal)),
              fastTrackRequest = fastTrackRequest.copy(clientType = Some(personal)),
              None
            ))
      }
      "transition to CheckDetailsNoDob when there is no dob for irv service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCPIR, "ni", nino, None)

        given(Prologue(None, None)) when start(None)(authorisedAgent)(fastTrackRequest)(FakeRequest(), HeaderCarrier()) should
          thenGo(
            CheckDetailsNoDob(
              originalFastTrackRequest = fastTrackRequest.copy(clientType = Some(personal)),
              fastTrackRequest = fastTrackRequest.copy(clientType = Some(personal)),
              None
            ))
      }
      "transition to CheckDetailsCompleteVat when all required fields are present for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ni", nino, vatRegDate)

        given(Prologue(None, None)) when start(None)(authorisedAgent)(fastTrackRequest)(FakeRequest(), HeaderCarrier()) should
          thenGo(
            CheckDetailsCompletePersonalVat(
              originalFastTrackRequest = fastTrackRequest,
              fastTrackRequest = fastTrackRequest,
              None
            ))
      }
      "transition to CheckDetailsNoVatRegDate when there is no vat reg date for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ni", nino, None)

        given(Prologue(None, None)) when start(None)(authorisedAgent)(fastTrackRequest)(FakeRequest(), HeaderCarrier()) should
          thenGo(
            CheckDetailsNoVatRegDate(
              originalFastTrackRequest = fastTrackRequest,
              fastTrackRequest = fastTrackRequest,
              None
            ))
      }
      "transition to CheckDetailsCompleteVat when all required fields are present for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "ni", nino, vatRegDate)

        given(Prologue(None, None)) when start(None)(authorisedAgent)(fastTrackRequest)(FakeRequest(), HeaderCarrier()) should
          thenGo(
            CheckDetailsCompleteBusinessVat(
              originalFastTrackRequest = fastTrackRequest,
              fastTrackRequest = fastTrackRequest,
              None
            ))
      }
      "transition to CheckDetailsNoVatRegDate when there is no vat reg date for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "ni", nino, None)

        given(Prologue(None, None)) when start(None)(authorisedAgent)(fastTrackRequest)(FakeRequest(), HeaderCarrier()) should
          thenGo(
            CheckDetailsNoVatRegDate(
              originalFastTrackRequest = fastTrackRequest,
              fastTrackRequest = fastTrackRequest,
              None
            ))
      }
      "transition to CheckDetailsNoClientTypeVat when there is no client type for vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCMTDVAT, "ni", nino, None)

        given(Prologue(None, None)) when start(None)(authorisedAgent)(fastTrackRequest)(FakeRequest(), HeaderCarrier()) should
          thenGo(
            CheckDetailsNoClientTypeVat(
              originalFastTrackRequest = fastTrackRequest,
              fastTrackRequest = fastTrackRequest,
              None
            ))
      }

      "transition to CheckDetailsCompleteTrust when there are all the required fields are present for a Trust service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(ClientType.business), TRUST, "utr", utr.value, None)

        given(Prologue(None, None)) when start(None)(authorisedAgent)(fastTrackRequest)(FakeRequest(), HeaderCarrier()) should
          thenGo(
            CheckDetailsCompleteTrust(
              originalFastTrackRequest = fastTrackRequest,
              fastTrackRequest = fastTrackRequest,
              None
            ))
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
      def getAgencyEmail() = Future("abc@xyz.com")

      "transition to InvitationSentPersonal if all fields are present, no pending or active invitations and known facts match for itsa" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)

        given(CheckDetailsCompleteItsa(fastTrackRequest, fastTrackRequest, None)) when checkedDetailsAllInformation(
          checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to IdentifyPersonalClient for ITSA when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)

        given(CheckDetailsCompleteItsa(fastTrackRequest, fastTrackRequest, None)) when checkedDetailsChangeInformation(
          authorisedAgent) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, fastTrackRequest, None))
      }
      "transition to InvitationSentPersonal if all fields are present, no pending or active invitations and known facts match for irv" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)

        given(CheckDetailsCompleteIrv(fastTrackRequest, fastTrackRequest, None)) when checkedDetailsAllInformation(
          checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to IdentifyPersonalClient for IRV when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)

        given(CheckDetailsCompleteIrv(fastTrackRequest, fastTrackRequest, None)) when checkedDetailsChangeInformation(
          authorisedAgent) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, fastTrackRequest, None))
      }
      "transition to InvitationSentPersonal if all fields are present, no pending or active invitations and known facts match for personal vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ni", nino, vatRegDate)

        given(CheckDetailsCompletePersonalVat(fastTrackRequest, fastTrackRequest, None)) when checkedDetailsAllInformation(
          checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to IdentifyPersonalClient for Personal VAT when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, vatRegDate)

        given(CheckDetailsCompletePersonalVat(fastTrackRequest, fastTrackRequest, None)) when checkedDetailsChangeInformation(
          authorisedAgent) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, fastTrackRequest, None))
      }
      "transition to InvitationSentBusiness if all fields are present, no pending or active invitations and known facts match for business vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "ni", nino, vatRegDate)

        given(CheckDetailsCompleteBusinessVat(fastTrackRequest, fastTrackRequest, None)) when checkedDetailsAllInformation(
          checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentBusiness("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSentBusiness if all fields are present, no pending or active invitations and known facts match for Trust" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), TRUST, "utr", utr.value, None)

        given(CheckDetailsCompleteTrust(fastTrackRequest, fastTrackRequest, None)) when checkedDetailsAllInformation(
          checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentBusiness("invitation/link", None, "abc@xyz.com", TRUST))
      }
      "transition to IdentifyBusinessClient for Business VAT when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "vrn", vrn, vatRegDate)

        given(CheckDetailsCompleteBusinessVat(fastTrackRequest, fastTrackRequest, None)) when checkedDetailsChangeInformation(
          authorisedAgent) should
          thenGo(IdentifyBusinessClient(fastTrackRequest, fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient when the form is false for itsa" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)

        given(CheckDetailsCompleteItsa(originalFastTrackRequest, fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
            Confirmation(false)) should
          thenGo(IdentifyPersonalClient(originalFastTrackRequest, fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient when the form is false for irv" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)

        given(CheckDetailsCompleteIrv(originalFastTrackRequest, fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
            Confirmation(false)) should
          thenGo(IdentifyPersonalClient(originalFastTrackRequest, fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient when the form is false for personal vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ni", nino, vatRegDate)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)

        given(CheckDetailsCompletePersonalVat(originalFastTrackRequest, fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
            Confirmation(false)) should
          thenGo(IdentifyPersonalClient(originalFastTrackRequest, fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient when the form is false for business vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "ni", nino, vatRegDate)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)

        given(CheckDetailsCompleteBusinessVat(originalFastTrackRequest, fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
            Confirmation(false)) should
          thenGo(IdentifyBusinessClient(originalFastTrackRequest, fastTrackRequest, None))
      }
      "transition to knownFactNotMatched when the clientIdentifier and known fact do not match for itsa" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)
        def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(false))

        given(CheckDetailsCompleteItsa(originalFastTrackRequest, fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
            Confirmation(true)) should
          thenGo(KnownFactNotMatched(originalFastTrackRequest, fastTrackRequest, None))
      }
      "transition to knownFactNotMatched when the clientIdentifier and known fact do not match for irv" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)
        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(false))

        given(CheckDetailsCompleteIrv(originalFastTrackRequest, fastTrackRequest, None)) when checkedDetailsAllInformation(
          checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(Confirmation(true)) should
          thenGo(KnownFactNotMatched(originalFastTrackRequest, fastTrackRequest, None))
      }
      "transition to knownFactNotMatched when the clientIdentifier and known fact do not match for personal vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "ni", nino, vatRegDate)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(400))

        given(CheckDetailsCompletePersonalVat(originalFastTrackRequest, fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
            Confirmation(true)) should
          thenGo(KnownFactNotMatched(originalFastTrackRequest, fastTrackRequest, None))
      }
      "transition to knownFactNotMatched when the clientIdentifier and known fact do not match for business vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "ni", nino, vatRegDate)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(400))

        given(CheckDetailsCompletePersonalVat(originalFastTrackRequest, fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
            Confirmation(true)) should
          thenGo(KnownFactNotMatched(originalFastTrackRequest, fastTrackRequest, None))
      }
      "transition to ClientNotSignedUp when the client is not enrolled for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)
        def checkPostcodeMatches(nino: Nino, postcode: String) = Future(None)

        given(CheckDetailsCompleteItsa(aFastTrackRequestWithDiffParams(fastTrackRequest), fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
            Confirmation(true)) should
          thenGo(ClientNotSignedUp(fastTrackRequest, None))
      }
      "transition to ClientNotSignedUp when the client is not enrolled for afi service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)
        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(None)

        given(CheckDetailsCompleteIrv(aFastTrackRequestWithDiffParams(fastTrackRequest), fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
            Confirmation(true)) should
          thenGo(ClientNotSignedUp(fastTrackRequest, None))
      }
      "transition to ClientNotSignedUp when the client is not enrolled for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, vatRegDate)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(None)

        given(CheckDetailsCompletePersonalVat(
          aFastTrackRequestWithDiffParams(fastTrackRequest),
          fastTrackRequest,
          None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
            Confirmation(true)) should
          thenGo(ClientNotSignedUp(fastTrackRequest, None))
      }
      "transition to ClientNotSignedUp when the client is not enrolled for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "vrn", vrn, vatRegDate)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(None)

        given(CheckDetailsCompletePersonalVat(
          aFastTrackRequestWithDiffParams(fastTrackRequest),
          fastTrackRequest,
          None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
            Confirmation(true)) should
          thenGo(ClientNotSignedUp(fastTrackRequest, None))
      }
      "transition to PendingInvitationExists when there is already a pending invitation for this request" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)
        def hasPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(true)

        given(CheckDetailsCompleteItsa(aFastTrackRequestWithDiffParams(fastTrackRequest), fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
            Confirmation(true)) should
          thenGo(PendingInvitationExists(fastTrackRequest, None))
      }
      "transition to ActiveAuthorisationExists when there is already an active relationship between agent and client for this service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)
        def hasActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(true)

        given(CheckDetailsCompleteItsa(aFastTrackRequestWithDiffParams(fastTrackRequest), fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasActiveRelationship)(authorisedAgent)(
            Confirmation(true)) should
          thenGo(ActiveAuthorisationExists(fastTrackRequest, None))
      }
      "transition to NoPostcode when there is no known fact in the request" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, None)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)

        given(CheckDetailsNoPostcode(originalFastTrackRequest, fastTrackRequest, None)) when
          checkedDetailsNoKnownFact(authorisedAgent) should
          thenGo(NoPostcode(originalFastTrackRequest, fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient for ITSA with no postcode when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, None)

        given(CheckDetailsNoPostcode(fastTrackRequest, fastTrackRequest, None)) when checkedDetailsChangeInformation(
          authorisedAgent) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient for IRV with no dob when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, None)

        given(CheckDetailsNoDob(fastTrackRequest, fastTrackRequest, None)) when checkedDetailsChangeInformation(
          authorisedAgent) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient for VAT with no vat reg date when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, None)

        given(CheckDetailsNoVatRegDate(fastTrackRequest, fastTrackRequest, None)) when checkedDetailsChangeInformation(
          authorisedAgent) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, fastTrackRequest, None))
      }
      "transition to IdentifyNoClientTypeClient for VAT with no client type when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCMTDVAT, "vrn", vrn, None)

        given(CheckDetailsNoClientTypeVat(fastTrackRequest, fastTrackRequest, None)) when checkedDetailsChangeInformation(
          authorisedAgent) should
          thenGo(IdentifyNoClientTypeClient(fastTrackRequest, fastTrackRequest, None))
      }
      "transition to SelectClientType when there is no client type in the request" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCMTDIT, "ni", nino, postCode)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)
        given(CheckDetailsNoClientTypeVat(originalFastTrackRequest, fastTrackRequest, None)) when
          checkedDetailsNoClientType(authorisedAgent) should
          thenGo(SelectClientTypeVat(originalFastTrackRequest, fastTrackRequest, None))
      }
    }

    "at IdentifyClient" should {
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
      def getAgencyEmail() = Future("abc@xyz.com")
      "transition to InvitationSent for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)

        given(IdentifyPersonalClient(aFastTrackRequestWithDiffParams(fastTrackRequest), fastTrackRequest, None)) when
          identifiedClientItsa(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
            ItsaClient("AB123456C", "BN32TM")) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSent for irv service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)

        given(IdentifyPersonalClient(aFastTrackRequestWithDiffParams(fastTrackRequest), fastTrackRequest, None)) when
          identifiedClientIrv(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
            IrvClient("AB123456C", "1990-10-10")) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSent for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, vatRegDate)

        given(IdentifyPersonalClient(aFastTrackRequestWithDiffParams(fastTrackRequest), fastTrackRequest, None)) when
          identifiedClientVat(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
            VatClient("1234567", "2010-10-10")) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSent for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "vrn", vrn, vatRegDate)

        given(IdentifyBusinessClient(aFastTrackRequestWithDiffParams(fastTrackRequest), fastTrackRequest, None)) when
          identifiedClientVat(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
            VatClient("1234567", "2010-10-10")) should
          thenGo(InvitationSentBusiness("invitation/link", None, "abc@xyz.com"))
      }
      "transition to client type for no client type vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCMTDVAT, "vrn", vrn, vatRegDate)
        val newVrn = "1234567"
        val newVatRegDate = "2010-10-10"

        given(IdentifyNoClientTypeClient(aFastTrackRequestWithDiffParams(fastTrackRequest), fastTrackRequest, None)) when
          identifiedClientVat(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(
            getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
            VatClient(newVrn, newVatRegDate)) should
          thenGo(
            SelectClientTypeVat(
              aFastTrackRequestWithDiffParams(fastTrackRequest),
              fastTrackRequest.copy(clientIdentifier = newVrn, knownFact = Some(newVatRegDate)),
              None))
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
      def getAgencyEmail() = Future("abc@xyz.com")
      "transition to InvitationSent for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, postCode)

        given(NoPostcode(aFastTrackRequestWithDiffParams(fastTrackRequest), fastTrackRequest, None)) when
          moreDetailsItsa(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(
            getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)("BN114AW") should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSent for irv service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, dob)

        given(NoDob(aFastTrackRequestWithDiffParams(fastTrackRequest), fastTrackRequest, None)) when
          moreDetailsIrv(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(
            getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)("1991-10-10") should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSent for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, vatRegDate)

        given(NoVatRegDate(aFastTrackRequestWithDiffParams(fastTrackRequest), fastTrackRequest, None)) when
          moreDetailsVat(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(
            getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)("2011-10-10") should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSent for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "vrn", vrn, vatRegDate)

        given(NoVatRegDate(aFastTrackRequestWithDiffParams(fastTrackRequest), fastTrackRequest, None)) when
          moreDetailsVat(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(
            getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)("2011-10-10") should
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
      def getAgencyEmail() = Future("abc@xyz.com")

      "transition to InvitationSent for vat service when there is a known fact present" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCMTDVAT, "vrn", vrn, vatRegDate)

        given(SelectClientTypeVat(aFastTrackRequestWithDiffParams(fastTrackRequest), fastTrackRequest, None)) when
          selectedClientType(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(
            getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)("personal") should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "transition to MoreDetails for vat service when there is no known fact" in {
        val fastTrackRequest = AgentFastTrackRequest(None, HMRCMTDVAT, "vrn", vrn, None)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)

        given(SelectClientTypeVat(originalFastTrackRequest, fastTrackRequest, None)) when
          selectedClientType(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(createInvitation)(getAgentLink)(
            getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)("personal") should
          thenGo(NoVatRegDate(originalFastTrackRequest, fastTrackRequest.copy(clientType = Some(personal)), None))
      }
    }

    "at KnownFactNotMatched, calling tryAgainNotMatchedKnownFact" when {
      "fast track request is for MTD-VAT and client type is missing should go to SelectClientTypeVat" in {
        val originalFtr = AgentFastTrackRequest(
          clientType = Some(personal),
          service = HMRCMTDVAT,
          "vrn",
          vrn,
          knownFact = Some("2001-01-01")
        )

        val ftr = AgentFastTrackRequest(
          clientType = None,
          service = HMRCMTDVAT,
          "vrn",
          vrn,
          knownFact = None
        )

        given(
          KnownFactNotMatched(
            originalFastTrackRequest = originalFtr,
            fastTrackRequest = ftr,
            continueUrl = None
          )) when
          tryAgainNotMatchedKnownFact(authorisedAgent) should
          thenGo(
            SelectClientTypeVat(
              originalFastTrackRequest = originalFtr,
              fastTrackRequest = ftr,
              continueUrl = None
            ))
      }

      "fast track request is for MTD-VAT with client type should go to NoVatRegDate" in {
        val originalFtr = AgentFastTrackRequest(
          clientType = Some(personal),
          service = HMRCMTDVAT,
          "vrn",
          vrn,
          knownFact = Some("2001-01-01")
        )

        val ftr = AgentFastTrackRequest(
          clientType = Some(personal),
          service = HMRCMTDVAT,
          "vrn",
          vrn,
          knownFact = None
        )

        given(
          KnownFactNotMatched(
            originalFastTrackRequest = originalFtr,
            fastTrackRequest = ftr,
            continueUrl = None
          )) when
          tryAgainNotMatchedKnownFact(authorisedAgent) should
          thenGo(
            NoVatRegDate(
              originalFastTrackRequest = originalFtr,
              fastTrackRequest = ftr,
              continueUrl = None
            ))
      }

      "fat track request is for MTD-IT service (and clientType was personal)" in {
        val originalFtr = AgentFastTrackRequest(
          clientType = Some(personal),
          service = HMRCMTDIT,
          "ni",
          nino,
          knownFact = Some("AA11AA")
        )

        val ftr = AgentFastTrackRequest(
          clientType = Some(personal),
          service = HMRCMTDIT,
          "ni",
          nino,
          knownFact = None
        )

        given(
          KnownFactNotMatched(
            originalFastTrackRequest = originalFtr,
            fastTrackRequest = ftr,
            continueUrl = None
          )) when
          tryAgainNotMatchedKnownFact(authorisedAgent) should
          thenGo(
            NoPostcode(
              originalFastTrackRequest = originalFtr,
              fastTrackRequest = ftr,
              continueUrl = None
            ))
      }

      "original request was for IRV service (and clientType was personal)" in {
        val originalFtr = AgentFastTrackRequest(
          clientType = Some(personal),
          service = HMRCPIR,
          "ni",
          nino,
          knownFact = Some("1990-09-09")
        )

        val ftr = AgentFastTrackRequest(
          clientType = Some(personal),
          service = HMRCPIR,
          "ni",
          nino,
          knownFact = None
        )

        given(
          KnownFactNotMatched(
            originalFastTrackRequest = originalFtr,
            fastTrackRequest = ftr,
            continueUrl = None
          )) when
          tryAgainNotMatchedKnownFact(authorisedAgent) should
          thenGo(
            NoDob(
              originalFastTrackRequest = originalFtr,
              fastTrackRequest = ftr,
              continueUrl = None
            ))
      }

      "original request was for a Trust service" when {
        val completedTrustFastTrack = AgentFastTrackRequest(
          clientType = Some(ClientType.business),
          service = TRUST,
          "utr",
          utr.value,
          knownFact = None
        )

        "trust not found for a given utr, should transition to IdentifyTrustClient " in {
          val TrustNotFoundState = TrustNotFound(
            originalFastTrackRequest = completedTrustFastTrack,
            fastTrackRequest = completedTrustFastTrack,
            continueUrl = None
          )

          given(TrustNotFoundState) when
            tryAgainNotMatchedKnownFact(authorisedAgent) should
            thenGo(IdentifyTrustClient(completedTrustFastTrack, completedTrustFastTrack, None))
        }
      }
    }
  }

  def aFastTrackRequestWithDiffParams(toThisFastTrackRequest: AgentFastTrackRequest) =
    toThisFastTrackRequest.clientType match {
      case Some(_) => toThisFastTrackRequest.copy(clientType = None)
      case None    => toThisFastTrackRequest.copy(clientType = Some(personal))
    }
}
