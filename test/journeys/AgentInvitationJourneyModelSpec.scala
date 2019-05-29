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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.{Basket, State, Transition, start}
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AgentInvitationJourneyModelSpec extends UnitSpec with StateMatchers[State] {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  case class given(initialState: State) extends AgentInvitationJourneyService with TestStorage[(State, List[State])] {
    await(save((initialState, Nil)))

    def when(transition: Transition): (State, List[State]) =
      await(super.apply(transition))
  }

  val emptyBasket: Basket = Set.empty
  val authorisedAgent = AuthorisedAgent(Arn("TARN0000001"), isWhitelisted = true)
  val availableServices = Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)

  val nino = "AB123456A"
  val postCode = Some("BN114AW")
  val vrn = "123456"
  val vatRegDate = Some("2010-10-10")
  val dob = Some("1990-10-10")

  "AgentInvitationJourneyService" when {
    "at state SelectClientType" should {
      "transition to SelectClientType" in {
        given(SelectClientType(emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }
      "transition to SelectPersonalService" in {
        given(SelectClientType(emptyBasket)) when selectedClientType(authorisedAgent)(ClientType.personal) should thenGo(
          SelectPersonalService(availableServices, emptyBasket))
      }
      "transition to SelectBusinessService" in {
        given(SelectClientType(emptyBasket)) when selectedClientType(authorisedAgent)(ClientType.business) should thenGo(
          SelectBusinessService)
      }
    }

    "at state SelectPersonalService" should {
      "transition to SelectClientType" in {
        given(SelectPersonalService(availableServices, emptyBasket)) when start should thenGo(
          SelectClientType(emptyBasket))
      }
      "transition to IdentifyPersonalClient for ITSA service" in {
        await(
          given(SelectPersonalService(availableServices, emptyBasket)) when selectedPersonalService(true, true, true)(
            authorisedAgent)(HMRCMTDIT)) should thenGo(IdentifyPersonalClient(HMRCMTDIT, emptyBasket))
      }
      "transition to IdentifyPersonalClient for PIR service" in {
        await(
          given(SelectPersonalService(availableServices, emptyBasket)) when selectedPersonalService(true, true, true)(
            authorisedAgent)(HMRCPIR)) should thenGo(IdentifyPersonalClient(HMRCPIR, emptyBasket))
      }
      "transition to IdentifyPersonalClient for VAT service" in {
        await(
          given(SelectPersonalService(availableServices, emptyBasket)) when selectedPersonalService(true, true, true)(
            authorisedAgent)(HMRCMTDVAT)) should thenGo(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket))
      }
      "transition to SelectPersonalService" in {
        await(
          given(SelectPersonalService(availableServices, emptyBasket)) when selectedPersonalService(true, true, true)(
            authorisedAgent)("foo")) should thenGo(SelectPersonalService(availableServices, emptyBasket))
      }
      "throw an exception when the show itsa feature flag is off" in {
        intercept[Exception] {
          await(
            given(SelectPersonalService(availableServices, emptyBasket)) when selectedPersonalService(
              false,
              true,
              true)(authorisedAgent)(HMRCMTDIT))
        }.getMessage shouldBe "Service: HMRC-MTD-IT feature flag is switched off"
      }
      "throw an exception when the show pir feature flag is off" in {
        intercept[Exception] {
          await(
            given(SelectPersonalService(availableServices, emptyBasket)) when selectedPersonalService(
              true,
              false,
              true)(authorisedAgent)(HMRCPIR))
        }.getMessage shouldBe "Service: PERSONAL-INCOME-RECORD feature flag is switched off"
      }
      "throw an exception when the show vat feature flag is off" in {
        intercept[Exception] {
          await(
            given(SelectPersonalService(availableServices, emptyBasket)) when selectedPersonalService(
              true,
              true,
              false)(authorisedAgent)(HMRCMTDVAT))
        }.getMessage shouldBe "Service: HMRC-MTD-VAT feature flag is switched off"
      }
    }

    "at state SelectBusinessService" should {
      "transition to SelectClientType" in {
        given(SelectBusinessService) when start should thenGo(SelectClientType(emptyBasket))
      }
      "after selectedBusinessService(true)(true) transition to IdentifyBusinessClient" in {
        given(SelectBusinessService) when selectedBusinessService(true)(authorisedAgent)(Confirmation(true)) should thenGo(
          IdentifyBusinessClient)
      }
      "after selectedBusinessService(true)(false) transition to SelectClientType" in {
        given(SelectBusinessService) when selectedBusinessService(true)(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType(emptyBasket))
      }
      "throw an exception when the show vat feature flag is off" in {
        intercept[Exception] {
          given(SelectBusinessService) when selectedBusinessService(false)(authorisedAgent)(Confirmation(true))
        }.getMessage shouldBe "Service: HMRC-MTD-VAT feature flag is switched off"
      }
    }

    "at state IdentifyPersonalClient" should {

      def clientName(service: String, clientId: String) = Future(Some("Piglet"))
      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def createMultipleInvitations(
        arn: Arn,
        clientType: Option[ClientType],
        requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] = Future(emptyBasket)
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      def getAgencyEmail() = Future(Some("abc@xyz.com"))

      "transition to SelectClientType" in {
        given(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }
      "transition to ConfirmClientItsa" in {
        def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(true))
        given(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)) when identifiedItsaClient(checkPostcodeMatches)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(true)(true)(clientName)(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(authorisedAgent)(ItsaClient("AB123456A", Some("BN114AW"))) should matchPattern {
          case (
              ConfirmClientItsa(
                AuthorisationRequest(
                  "Piglet",
                  ItsaInvitation(Nino("AB123456A"), Some(Postcode("BN114AW")), personal, HMRCMTDIT, "ni"),
                  AuthorisationRequest.NEW,
                  _),
                `emptyBasket`),
              _) =>
        }
      }
      "transition to ReviewAuthorisationsPersonal when the redirectToConfirmFlag is off for itsa" in {
        def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(true))
        given(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)) when identifiedItsaClient(checkPostcodeMatches)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(false)(true)(clientName)(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(authorisedAgent)(ItsaClient("AB123456A", Some("BN114AW"))) should
          thenMatch { case ReviewAuthorisationsPersonal(basket) if basket.nonEmpty => }
      }
      "transition to KnownFactsNotMatched when the nino and postcode do not match" in {
        def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(false))

        given(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)) when identifiedItsaClient(checkPostcodeMatches)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(true)(true)(clientName)(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(authorisedAgent)(ItsaClient("AB123456A", Some("BN114AW"))) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }
      "transition to PendingInvitationExists for itsa service when redirect flag is off" in {
        def hasPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(true)
        def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(true))
        given(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)) when identifiedItsaClient(checkPostcodeMatches)(
          hasPendingInvitation)(hasNoActiveRelationship)(false)(true)(clientName)(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(authorisedAgent)(ItsaClient("AB123456A", Some("BN114AW"))) should thenGo(
          PendingInvitationExists(personal, emptyBasket))
      }
      "transition to ConfirmClientItsa for itsa service when redirect flag is off" in {
        def hasActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(true)
        def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(true))
        given(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)) when identifiedItsaClient(checkPostcodeMatches)(
          hasNoPendingInvitation)(hasActiveRelationship)(false)(true)(clientName)(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(authorisedAgent)(ItsaClient("AB123456A", Some("BN114AW"))) should thenGo(
          ActiveAuthorisationExists(personal, HMRCMTDIT, emptyBasket))
      }
      "transition to ConfirmClientPersonalVat" in {
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(204))
        given(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket)) when identifiedVatClient(checkRegDateMatches)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(true)(true)(clientName)(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(authorisedAgent)(VatClient("123456", Some("2010-10-10"))) should
          matchPattern {
            case (
                ConfirmClientPersonalVat(
                  AuthorisationRequest(
                    "Piglet",
                    VatInvitation(Some(personal), Vrn("123456"), Some(VatRegDate("2010-10-10")), HMRCMTDVAT, "vrn"),
                    AuthorisationRequest.NEW,
                    _),
                  `emptyBasket`),
                _) =>
          }
      }
      "transition to KnownFactNotMatched when the vrn and regDate don't match" in {
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(403))
        given(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket)) when identifiedVatClient(checkRegDateMatches)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(false)(true)(clientName)(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(authorisedAgent)(VatClient("123456", Some("2010-10-10"))) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }
      "transition to PendingInvitationExists for vat service when redirect flag is off" in {
        def hasPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(true)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(204))
        given(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket)) when identifiedVatClient(checkRegDateMatches)(
          hasPendingInvitation)(hasNoActiveRelationship)(false)(true)(clientName)(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(authorisedAgent)(VatClient("123456", Some("2010-10-10"))) should thenGo(
          PendingInvitationExists(personal, emptyBasket))
      }
      "transition to ActiveRelationshipExists for vat service when redirect flag is off" in {
        def hasActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(true)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(204))
        given(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket)) when identifiedVatClient(checkRegDateMatches)(
          hasNoPendingInvitation)(hasActiveRelationship)(false)(true)(clientName)(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(authorisedAgent)(VatClient("123456", Some("2010-10-10"))) should thenGo(
          ActiveAuthorisationExists(personal, HMRCMTDVAT, emptyBasket))
      }
      "transition to ConfirmClientIrv" in {
        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(true))
        given(IdentifyPersonalClient(HMRCPIR, emptyBasket)) when identifiedIrvClient(checkDobMatches)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(true)(true)(clientName)(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(authorisedAgent)(IrvClient("AB123456A", Some("1990-10-10"))) should matchPattern {
          case (
              ConfirmClientIrv(
                AuthorisationRequest(
                  "Piglet",
                  PirInvitation(Nino("AB123456A"), Some(DOB("1990-10-10")), personal, HMRCPIR, "ni"),
                  AuthorisationRequest.NEW,
                  _),
                `emptyBasket`),
              _) =>
        }
      }
      "transition to ReviewAuthorisations when the confirmToRedirect flag is off for IRV" in {
        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(true))
        given(IdentifyPersonalClient(HMRCPIR, emptyBasket)) when identifiedIrvClient(checkDobMatches)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(false)(true)(clientName)(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(authorisedAgent)(IrvClient("AB123456A", Some("1990-10-10"))) should
          thenMatch { case ReviewAuthorisationsPersonal(basket) if basket.nonEmpty => }
      }
      "transition to KnownFactNotMatched when the nino and dob don't match" in {
        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(false))
        given(IdentifyPersonalClient(HMRCPIR, emptyBasket)) when identifiedIrvClient(checkDobMatches)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(true)(true)(clientName)(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(authorisedAgent)(IrvClient("AB123456A", Some("1990-10-10"))) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }
      "transition to PendingInvitationExists for irv service when redirect flag is off" in {
        def hasPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(true)
        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(true))
        given(IdentifyPersonalClient(HMRCPIR, emptyBasket)) when identifiedIrvClient(checkDobMatches)(
          hasPendingInvitation)(hasNoActiveRelationship)(false)(true)(clientName)(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(authorisedAgent)(IrvClient("AB123456A", Some("1990-10-10"))) should thenGo(
          PendingInvitationExists(personal, emptyBasket))
      }
      "transition to ActiveRelationshipExists for irv service when redirect flag is off" in {
        def hasActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(true)
        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(true))
        given(IdentifyPersonalClient(HMRCPIR, emptyBasket)) when identifiedIrvClient(checkDobMatches)(
          hasNoPendingInvitation)(hasActiveRelationship)(false)(true)(clientName)(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(authorisedAgent)(IrvClient("AB123456A", Some("1990-10-10"))) should thenGo(
          ActiveAuthorisationExists(personal, HMRCPIR, emptyBasket))
      }
      "transition to KnownFactNotMatched when client not found" in {
        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(None)
        given(IdentifyPersonalClient(HMRCPIR, emptyBasket)) when identifiedIrvClient(checkDobMatches)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(true)(true)(clientName)(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(authorisedAgent)(IrvClient("AB123456A", Some("1990-10-10"))) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }

    }

    "at state IdentifyBusinessClient" should {
      def clientName(service: String, clientId: String) = Future(Some("Piglet"))
      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def createMultipleInvitations(
        arn: Arn,
        clientType: Option[ClientType],
        requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] = Future(emptyBasket)
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      def getAgencyEmail() = Future(Some("abc@xyz.com"))

      "transition to SelectClientType" in {
        given(IdentifyBusinessClient) when start should thenGo(SelectClientType(emptyBasket))
      }
      "transition to ConfirmClientBusinessVat" in {
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(204))
        given(IdentifyBusinessClient) when identifiedVatClient(checkRegDateMatches)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(true)(true)(clientName)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(
          authorisedAgent)(VatClient("123456", Some("2010-10-10"))) should matchPattern {
          case (
              ConfirmClientBusinessVat(
                AuthorisationRequest(
                  "Piglet",
                  VatInvitation(Some(business), Vrn("123456"), Some(VatRegDate("2010-10-10")), HMRCMTDVAT, "vrn"),
                  AuthorisationRequest.NEW,
                  _)),
              _) =>
        }
      }
      "transition to InvitationSent when confirmToRedirect flag is off for VAT" in {
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(204))
        given(IdentifyBusinessClient) when identifiedVatClient(checkRegDateMatches)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(false)(true)(clientName)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(
          authorisedAgent)(VatClient("123456", Some("2010-10-10"))) should
          thenGo(InvitationSentBusiness("invitation/link", None, "abc@xyz.com"))
      }
      "transition to KnownFactNotMatched client" in {
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(404))
        given(IdentifyBusinessClient) when identifiedVatClient(checkRegDateMatches)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(true)(true)(clientName)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(
          authorisedAgent)(VatClient("123456", Some("2010-10-10"))) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }
      "transition to PendingInvitationExists" in {
        def hasPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(true)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(204))
        given(IdentifyBusinessClient) when identifiedVatClient(checkRegDateMatches)(hasPendingInvitation)(
          hasNoActiveRelationship)(true)(true)(clientName)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(
          authorisedAgent)(VatClient("123456", Some("2010-10-10"))) should {
          matchPattern {
            case (
                ConfirmClientBusinessVat(
                  AuthorisationRequest(
                    "Piglet",
                    VatInvitation(Some(business), Vrn("123456"), Some(VatRegDate("2010-10-10")), HMRCMTDVAT, "vrn"),
                    AuthorisationRequest.NEW,
                    _)),
                _) =>
          }
        }
      }
      "transition to ActiveRelationshipExists" in {
        def hasActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(true)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(204))
        given(IdentifyBusinessClient) when identifiedVatClient(checkRegDateMatches)(hasNoPendingInvitation)(
          hasActiveRelationship)(true)(true)(clientName)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(
          authorisedAgent)(VatClient("123456", Some("2010-10-10"))) should
          matchPattern {
            case (
                ConfirmClientBusinessVat(
                  AuthorisationRequest(
                    "Piglet",
                    VatInvitation(Some(business), Vrn("123456"), Some(VatRegDate("2010-10-10")), HMRCMTDVAT, "vrn"),
                    AuthorisationRequest.NEW,
                    _)),
                _) =>
          }
      }
      "transition to ClientNotSignedUp" in {
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(None)
        given(IdentifyBusinessClient) when identifiedVatClient(checkRegDateMatches)(hasNoPendingInvitation)(
          hasNoActiveRelationship)(true)(true)(clientName)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(
          authorisedAgent)(VatClient("123456", Some("2010-10-10"))) should
          thenGo(ClientNotSignedUp(HMRCMTDVAT, emptyBasket))
      }
    }

    "at ConfirmClientItsa" should {
      val authorisationRequest =
        AuthorisationRequest("Piglet", ItsaInvitation(Nino("AB123456A"), Some(Postcode("BN114AW"))))
      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def createMultipleInvitations(
        arn: Arn,
        clientType: Option[ClientType],
        requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] = Future(emptyBasket)
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      def getAgencyEmail() = Future(Some("abc@xyz.com"))
      "transition to SelectClientType" in {
        given(ConfirmClientItsa(authorisationRequest, emptyBasket)) when start should
          thenGo(SelectClientType(emptyBasket))
      }
      "transition to ReviewAuthorisationsPersonal" in {
        given(ConfirmClientItsa(authorisationRequest, emptyBasket)) when clientConfirmed(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
          Confirmation(true)) should
          thenMatch { case ReviewAuthorisationsPersonal(basket) if basket.nonEmpty => }
      }
      "transition to SelectPersonalService" in {
        given(ConfirmClientItsa(authorisationRequest, emptyBasket)) when clientConfirmed(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
          Confirmation(false)) should
          thenGo(IdentifyPersonalClient(HMRCMTDIT, emptyBasket))
      }
    }

    "at ConfirmClientIrv" should {
      val authorisationRequest =
        AuthorisationRequest("Piglet", PirInvitation(Nino("AB123456A"), Some(DOB("1990-10-10"))))
      def createMultipleInvitations(
        arn: Arn,
        clientType: Option[ClientType],
        requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] = Future(emptyBasket)
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def getAgencyEmail() = Future(Some("abc@xyz.com"))
      "transition to SelectClientType" in {
        given(ConfirmClientIrv(authorisationRequest, emptyBasket)) when start should thenGo(
          SelectClientType(emptyBasket))
      }
      "transition to ReviewAuthorisationsPersonal" in {
        given(ConfirmClientIrv(authorisationRequest, emptyBasket)) when clientConfirmed(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
          Confirmation(true)) should
          thenMatch { case ReviewAuthorisationsPersonal(basket) if basket.nonEmpty => }
      }
      "transition to SelectPersonalService" in {
        given(ConfirmClientIrv(authorisationRequest, emptyBasket)) when clientConfirmed(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
          Confirmation(false)) should
          thenGo(IdentifyPersonalClient(HMRCPIR, emptyBasket))
      }
    }

    "at ConfirmClientPersonalVat" should {
      def createMultipleInvitations(
        arn: Arn,
        clientType: Option[ClientType],
        requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] = Future(emptyBasket)
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def getAgencyEmail() = Future(Some("abc@xyz.com"))
      "transition to Start" in {
        given(ConfirmClientPersonalVat(
          AuthorisationRequest("Piglet", VatInvitation(Some(personal), Vrn("123456"), Some(VatRegDate("2010-10-10")))),
          emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }
      "transition to ClientConfirmedPersonal" in {
        given(ConfirmClientPersonalVat(
          AuthorisationRequest("Piglet", VatInvitation(Some(personal), Vrn("123456"), Some(VatRegDate("2010-10-10")))),
          emptyBasket)) when clientConfirmed(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(Confirmation(true)) should
          thenMatch { case ReviewAuthorisationsPersonal(basket) if basket.nonEmpty => }
      }
      "transition to PersonalServiceSelected" in {
        given(ConfirmClientPersonalVat(
          AuthorisationRequest("Piglet", VatInvitation(Some(personal), Vrn("123456"), Some(VatRegDate("2010-10-10")))),
          emptyBasket)) when clientConfirmed(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket))
      }
    }

    "at ConfirmClientBusinessVat" should {
      val authorisationRequest =
        AuthorisationRequest("Piglet", VatInvitation(Some(business), Vrn("123456"), Some(VatRegDate("2010-10-10"))))
      def createMultipleInvitations(
        arn: Arn,
        clientType: Option[ClientType],
        requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] = Future(emptyBasket)
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def getAgencyEmail() = Future(Some("abc@xyz.com"))
      "after start transition to Start" in {
        given(ConfirmClientBusinessVat(authorisationRequest)) when start should thenGo(SelectClientType(emptyBasket))
      }
      "after clientConfirmed(true) transition to InvitationSentBusiness" in {
        given(ConfirmClientBusinessVat(authorisationRequest)) when clientConfirmed(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
          Confirmation(true)) should
          thenGo(InvitationSentBusiness("invitation/link", None, "abc@xyz.com"))
      }
      "after clientConfirmed(false) transition to IdentifyBusinessClient" in {
        given(ConfirmClientBusinessVat(authorisationRequest)) when clientConfirmed(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(authorisedAgent)(
          Confirmation(false)) should
          thenGo(IdentifyBusinessClient)
      }
    }

    "at ReviewAuthorisationsPersonal" should {
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      def getAgencyEmail() = Future(Some("abc@xyz.com"))
      "after start transition to Start" in {
        given(ReviewAuthorisationsPersonal(emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }
      "after authorisationsReviewed(true) transition to SelectPersonalService" in {
        def createMultipleInvitations(
          arn: Arn,
          clientType: Option[ClientType],
          requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] = Future(emptyBasket)

        given(ReviewAuthorisationsPersonal(emptyBasket)) when authorisationsReviewed(createMultipleInvitations)(
          getAgentLink)(getAgencyEmail)(authorisedAgent)(Confirmation(true)) should
          thenGo(SelectPersonalService(availableServices, emptyBasket)) //FIXME check basket has invitation added
      }
      "after authorisationsReviewed(false) transition to InvitationSentPersonal" in {
        val authorisationRequestNew = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", Some("BN114AW")),
          AuthorisationRequest.NEW,
          "ABC123")
        val authorisationRequestCreated = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", Some("BN114AW")),
          AuthorisationRequest.CREATED,
          "ABC123")

        def createMultipleInvitations(arn: Arn, clientType: Option[ClientType], requests: Set[AuthorisationRequest]) =
          Future(Set(authorisationRequestCreated))

        given(ReviewAuthorisationsPersonal(Set(authorisationRequestNew))) when authorisationsReviewed(
          createMultipleInvitations)(getAgentLink)(getAgencyEmail)(authorisedAgent)(Confirmation(false)) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
      "after authorisationsReviewed(false) when all fail transition to AuthorisationsReviewedAllFailed" in {
        val authorisationRequestNew = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", Some("BN114AW")),
          AuthorisationRequest.NEW,
          "ABC123")
        val authorisationRequestFailed = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", Some("BN114AW")),
          AuthorisationRequest.FAILED,
          "ABC123")

        def createMultipleInvitations(arn: Arn, clientType: Option[ClientType], requests: Set[AuthorisationRequest]) =
          Future(Set(authorisationRequestFailed))

        given(ReviewAuthorisationsPersonal(Set(authorisationRequestNew))) when authorisationsReviewed(
          createMultipleInvitations)(getAgentLink)(getAgencyEmail)(authorisedAgent)(Confirmation(false)) should
          thenGo(AllAuthorisationsFailed(Set(authorisationRequestNew)))
      }
      "after authorisationsReviewed(false) when some fail transition to AuthorisationReviewedSomeFailed" in {
        val authorisationRequestNew1 = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", Some("BN114AW")),
          AuthorisationRequest.NEW,
          "ABC123")
        val authorisationRequestNew2 = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456B", Some("BN114AT")),
          AuthorisationRequest.NEW,
          "ABC124")
        val authorisationRequestSuccess1 = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", Some("BN114AW")),
          AuthorisationRequest.CREATED,
          "ABC123")
        val authorisationRequestFailed2 = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456B", Some("BN114AT")),
          AuthorisationRequest.FAILED,
          "ABC124")

        def createMultipleInvitations(arn: Arn, clientType: Option[ClientType], requests: Set[AuthorisationRequest]) =
          Future(Set(authorisationRequestSuccess1, authorisationRequestFailed2))

        given(ReviewAuthorisationsPersonal(Set(authorisationRequestNew1, authorisationRequestNew2))) when authorisationsReviewed(
          createMultipleInvitations)(getAgentLink)(getAgencyEmail)(authorisedAgent)(Confirmation(false)) should
          thenGo(SomeAuthorisationsFailed(Set(authorisationRequestNew1, authorisationRequestNew2)))
      }
      "after deleteAuthorisationRequest with a valid itemId transition to DeleteAuthorisationRequestPersonal" in {
        val authorisationRequest = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", Some("BN114AW")),
          AuthorisationRequest.NEW,
          "ABC123")
        given(ReviewAuthorisationsPersonal(Set(authorisationRequest))) when deleteAuthorisationRequest("ABC123")(
          authorisedAgent) should thenGo(
          DeleteAuthorisationRequestPersonal(authorisationRequest, Set(authorisationRequest)))
      }
      "throw an Exception when there is no corresponding itemId in the basket" in {
        val authorisationRequest = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", Some("BN114AW")),
          AuthorisationRequest.NEW,
          "ABC123")
        intercept[Exception] {
          given(ReviewAuthorisationsPersonal(Set(authorisationRequest))) when deleteAuthorisationRequest("XXX")(
            authorisedAgent)
        }.getMessage shouldBe "No Item to delete"
      }
    }
    "at DeleteAuthorisationRequestPersonal" should {
      "after start transition to Start" in {
        val authorisationRequest = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", Some("BN114AW")),
          AuthorisationRequest.NEW,
          "ABC123")
        given(DeleteAuthorisationRequestPersonal(authorisationRequest, emptyBasket)) when start should thenGo(
          SelectClientType(emptyBasket))
      }
      "after confirmDeleteAuthorisationRequest(true) should transition to ReviewAuthorisationPersonal with one request removed form the basket" in {
        val authorisationRequest1 = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", Some("BN114AW")),
          AuthorisationRequest.NEW,
          "ABC123")
        val authorisationRequest2 = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456B", Some("BN114AT")),
          AuthorisationRequest.NEW,
          "ABC124")
        given(DeleteAuthorisationRequestPersonal(
          authorisationRequest1,
          Set(authorisationRequest1, authorisationRequest2))) when confirmDeleteAuthorisationRequest(authorisedAgent)(
          Confirmation(true)) should
          thenGo(ReviewAuthorisationsPersonal(Set(authorisationRequest2)))
      }
      "after confirmDeleteAuthorisationRequest(true) should transition to AllAuthorisationsRemoved when there is nothing left in the basket" in {
        val authorisationRequest = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", Some("BN114AW")),
          AuthorisationRequest.NEW,
          "ABC123")
        given(DeleteAuthorisationRequestPersonal(authorisationRequest, Set(authorisationRequest))) when confirmDeleteAuthorisationRequest(
          authorisedAgent)(Confirmation(true)) should
          thenGo(AllAuthorisationsRemoved)
      }
      "after confirmDeleteAuthorisationRequest(false) should transition to ReviewAuthorisationPersonal with basket in tact" in {
        val authorisationRequest = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", Some("BN114AW")),
          AuthorisationRequest.NEW,
          "ABC123")
        given(DeleteAuthorisationRequestPersonal(authorisationRequest, Set(authorisationRequest))) when confirmDeleteAuthorisationRequest(
          authorisedAgent)(Confirmation(false)) should
          thenGo(ReviewAuthorisationsPersonal(Set(authorisationRequest)))
      }
    }
  }
}
