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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.{Basket, State, Transition}
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCCGTPD, HMRCMTDIT, HMRCMTDVAT, HMRCPIR, TRUST}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, CgtRef, Utr, Vrn}
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

  val emptyBasket: Basket           = Set.empty
  val authorisedAgent               = AuthorisedAgent(Arn("TARN0000001"), isWhitelisted = true)
  val authorisedAgentNotWhitelisted = AuthorisedAgent(Arn("TARN0000001"), isWhitelisted = false)
  private val availableServices      = Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT, HMRCCGTPD)
  private val availableTrustServices = Set(TRUST, HMRCCGTPD)
  private val nonWhitelistedServices = Set(HMRCMTDIT, HMRCMTDVAT, HMRCCGTPD)

  val nino       = "AB123456A"
  val postCode   = Some("BN114AW")
  val vrn        = "123456"
  val vatRegDate = Some("2010-10-10")
  val dob        = Some("1990-10-10")

  def getAgencyEmail: GetAgencyEmail = () => Future("abc@xyz.com")

  def getCgtRefName: GetCgtRefName = CgtRef => Future("myCgtRef")

  "AgentInvitationJourneyService" when {

    // TODO add test for selectedTrustServiceMultiple transition

    "at state SelectClientType" should {

      "transition to SelectClientType" in {

        given(SelectClientType(emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }

      "transition to SelectPersonalService" in {

        given(SelectClientType(emptyBasket)) when
        selectedClientType(authorisedAgent)("personal") should
        thenGo(SelectPersonalService(availableServices, emptyBasket))
      }

      "transition to SelectPersonalService with fewer services when agent is not whitelisted" in {

        given(SelectClientType(emptyBasket)) when
        selectedClientType(authorisedAgentNotWhitelisted)("personal") should
        thenGo(SelectPersonalService(nonWhitelistedServices, emptyBasket))
      }

      "transition to SelectBusinessService" in {

        given(SelectClientType(emptyBasket)) when
        selectedClientType(authorisedAgent)("business") should
        thenGo(SelectBusinessService)
      }
    }

    "at state SelectPersonalService" should {

      "transition to SelectClientType" in {

        given(SelectPersonalService(availableServices, emptyBasket)) when
        start should
        thenGo(SelectClientType(emptyBasket))
      }

      "transition to IdentifyPersonalClient for ITSA service" in {

        await(
          given(SelectPersonalService(availableServices, emptyBasket)) when
          selectedPersonalService(
            showItsaFlag = true,
            showPirFlag = true,
            showVatFlag = true,
            showCgtFlag = true)(authorisedAgent)(HMRCMTDIT)) should
        thenGo(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)
               )
      }

      "transition to IdentifyPersonalClient for PIR service" in {

        await(
          given(SelectPersonalService(availableServices, emptyBasket)) when
          selectedPersonalService(
            showItsaFlag = true,
            showPirFlag = true,
            showVatFlag = true,
            showCgtFlag = true)(authorisedAgent)(HMRCPIR)) should
        thenGo(IdentifyPersonalClient(HMRCPIR, emptyBasket)
               )
      }

      "transition to IdentifyPersonalClient for VAT service" in {

        await(
          given(SelectPersonalService(availableServices, emptyBasket)) when
          selectedPersonalService(
            showItsaFlag = true,
            showPirFlag = true,
            showVatFlag = true,
            showCgtFlag = true
            )(authorisedAgent)(HMRCMTDVAT)) should
        thenGo(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket)
               )
      }

      "transition to IdentifyPersonalClient for CGT service" in {

        await(
          given(SelectPersonalService(availableServices, emptyBasket)) when
          selectedPersonalService(
            showItsaFlag = true,
            showPirFlag = true,
            showVatFlag = true,
            showCgtFlag = true
          )(authorisedAgent)(HMRCCGTPD)) should
        thenGo(IdentifyPersonalClient(HMRCCGTPD, emptyBasket)
        )
      }

      "transition to SelectPersonalService" in {

        await(
          given(SelectPersonalService(availableServices, emptyBasket)) when
          selectedPersonalService(
            showItsaFlag = true,
            showPirFlag = true,
            showVatFlag = true,
            showCgtFlag = true
            )(authorisedAgent)("foo")) should
        thenGo(SelectPersonalService(availableServices, emptyBasket)
               )
      }

      "throw an exception when the show itsa feature flag is off" in {

        intercept[Exception] {
          await(
            given(SelectPersonalService(availableServices, emptyBasket)) when
            selectedPersonalService(
              showItsaFlag = false,
              showPirFlag = true,
              showVatFlag = true,
              showCgtFlag = true
              )(authorisedAgent)(HMRCMTDIT)
            )
        }.getMessage shouldBe "Service: HMRC-MTD-IT feature flag is switched off"
      }

      "throw an exception when the show pir feature flag is off" in {

        intercept[Exception] {
          await(
            given(SelectPersonalService(availableServices, emptyBasket)) when
            selectedPersonalService(
              showItsaFlag = true,
              showPirFlag = false,
              showVatFlag = true,
              showCgtFlag = true
              )(authorisedAgent)(HMRCPIR)
            )
        }.getMessage shouldBe "Service: PERSONAL-INCOME-RECORD feature flag is switched off"
      }

      "throw an exception when the show vat feature flag is off" in {

        intercept[Exception] {
          await(
            given(SelectPersonalService(availableServices, emptyBasket)) when
            selectedPersonalService(
              showItsaFlag = true,
              showPirFlag = true,
              showVatFlag = false,
              showCgtFlag = true
              )(authorisedAgent)(HMRCMTDVAT)
            )
        }.getMessage shouldBe "Service: HMRC-MTD-VAT feature flag is switched off"
      }

      "throw an exception when the show cgt feature flag is off" in {

        intercept[Exception] {
          await(
            given(SelectPersonalService(availableServices, emptyBasket)) when
            selectedPersonalService(
              showItsaFlag = true,
              showPirFlag = true,
              showVatFlag = true,
              showCgtFlag = false
            )(authorisedAgent)(HMRCCGTPD)
          )
        }.getMessage shouldBe "Service: HMRC-CGT-PD feature flag is switched off"
      }


    }

    "at state SelectBusinessService" should {

      "transition to SelectClientType" in {

        given(SelectBusinessService) when start should thenGo(SelectClientType(emptyBasket))
      }

      "after selectedBusinessService(true)(true) transition to IdentifyBusinessClient" in {

        given(SelectBusinessService) when
        selectedBusinessService(showVatFlag = true)(authorisedAgent)(Confirmation(true)) should
        thenGo(IdentifyBusinessClient)
      }

      "after selectedBusinessService(true)(false) transition to SelectClientType" in {

        given(SelectBusinessService) when
        selectedBusinessService(showVatFlag = true)(authorisedAgent)(Confirmation(false)) should
        thenGo(SelectClientType(emptyBasket))
      }

      "throw an exception when the show vat feature flag is off" in {

        intercept[Exception] {
          given(SelectBusinessService) when
          selectedBusinessService(showVatFlag = false)(authorisedAgent)(Confirmation(true))
        }.getMessage shouldBe "Service: HMRC-MTD-VAT feature flag is switched off"
      }
    }

    "at state SelectTrustService" should {

      "transition to SelectClientType" in {

        given(SelectTrustService(availableTrustServices, emptyBasket)) when
        start should
        thenGo(SelectClientType(emptyBasket))
      }

      "after selectedTrustService(true)(true) transition to IdentifyTrustClient" in {

        given(SelectTrustService(availableTrustServices, emptyBasket)) when
        selectedTrustServiceSingle(showTrustsFlag = true)(authorisedAgent)(Confirmation(true)) should
        thenGo(IdentifyTrustClient(TRUST, emptyBasket))
      }

      "after selectedTrustService(true)(false) transition to SelectClientType" in {

        given(SelectTrustService(availableTrustServices, emptyBasket)) when
        selectedTrustServiceSingle(showTrustsFlag = true)(authorisedAgent)(Confirmation(false)) should
        thenGo(SelectClientType(emptyBasket))
      }

      "throw an exception when the show trust feature flag is off" in {

        intercept[Exception] {
          given(SelectTrustService(availableTrustServices, emptyBasket)) when
          selectedTrustServiceSingle(showTrustsFlag = false)(authorisedAgent)(Confirmation(true))
        }.getMessage shouldBe "Service: HMRC-TERS-ORG feature flag is switched off"
      }
    }

    "at state IdentifyPersonalClient" should {

      def clientName(service: String, clientId: String) = Future(Some("Piglet"))

      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(false)

      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(false)

      def createMultipleInvitations(
                                     arn: Arn,
                                     requests: Set[AuthorisationRequest]
                                   ): Future[Set[AuthorisationRequest]] = Future(emptyBasket)

      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

      "transition to SelectClientType" in {

        given(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }

      "transition to ConfirmClientItsa" in {

        def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(true))

        given(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)) when
        identifiedItsaClient(
          checkPostcodeMatches)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          clientName)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          ItsaClient("AB123456A", "BN114AW")) should
        matchPattern {
          case (
            ConfirmClientItsa(
            AuthorisationRequest(
            "Piglet",
            ItsaInvitation(Nino("AB123456A"), Postcode("BN114AW"), _, HMRCMTDIT, "ni"),
            AuthorisationRequest.NEW,
            _),
            `emptyBasket`),
            _) =>
        }
      }

      "transition to ConfirmClientPersonalCgt" in {

        given(IdentifyPersonalClient(HMRCCGTPD, emptyBasket)) when
        identifiedCgtClient(getCgtRefName)(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
        matchPattern {
          case (
            ConfirmClientPersonalCgt(
            AuthorisationRequest(
            "myCgtRef",
            CgtInvitation(CgtRef("myCgtRef"), Some(`personal`), HMRCCGTPD, "CGTPDRef"),
            AuthorisationRequest.NEW,
            _),
            `emptyBasket`),
            _) =>
        }

      }

      "transition to KnownFactsNotMatched when the nino and postcode do not match" in {

        def checkPostcodeMatches(nino: Nino, postcode: String) =
          Future(Some(false))

        given(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)) when
        identifiedItsaClient(
          checkPostcodeMatches)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          clientName)(createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(ItsaClient("AB123456A", "BN114AW")) should
        thenGo(KnownFactNotMatched(emptyBasket))
      }

      "transition to ConfirmClientPersonalVat" in {

        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(204))

        given(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket)) when
        identifiedVatClient(
          checkRegDateMatches)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          clientName)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          VatClient("123456", "2010-10-10")) should
        matchPattern {
          case (
            ConfirmClientPersonalVat(
            AuthorisationRequest(
            "Piglet",
            VatInvitation(Some(_), Vrn("123456"), VatRegDate("2010-10-10"), HMRCMTDVAT, "vrn"),
            AuthorisationRequest.NEW,
            _),
            `emptyBasket`),
            _) =>
        }
      }

      "transition to KnownFactNotMatched when the vrn and regDate don't match" in {

        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(403))

        given(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket)) when
        identifiedVatClient(
          checkRegDateMatches)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          clientName)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          VatClient("123456", "2010-10-10")) should
        thenGo(KnownFactNotMatched(emptyBasket))
      }

      "transition to CannotCreateRequest when a migration is in process" in {

        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(423))

        given(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket)) when
        identifiedVatClient(
          checkRegDateMatches)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          clientName)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          VatClient("123456", "2010-10-10")) should
        thenGo(CannotCreateRequest(emptyBasket))
      }

      "transition to ClientNotSignedUp when the client is not signed up for the service" in {

        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(None)

        given(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket)) when
        identifiedVatClient(
          checkRegDateMatches)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          clientName)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          VatClient("123456", "2010-10-10")) should
        thenGo(ClientNotSignedUp(HMRCMTDVAT, emptyBasket))
      }

      "transition to KnownFactNotMatched when the nino and dob don't match" in {

        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(false))

        given(IdentifyPersonalClient(HMRCPIR, emptyBasket)) when
        identifiedIrvClient(
          checkDobMatches)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          clientName)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          IrvClient("AB123456A", "1990-10-10")) should
        thenGo(KnownFactNotMatched(emptyBasket))
      }

      "transition to KnownFactNotMatched when client not found" in {

        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(None)

        given(IdentifyPersonalClient(HMRCPIR, emptyBasket)) when
        identifiedIrvClient(
          checkDobMatches)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          clientName)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          IrvClient("AB123456A", "1990-10-10")) should
        thenGo(KnownFactNotMatched(emptyBasket))
      }
    }

    "at state IdentifyTrustClient" should {

      "transition to ConfirmClientTrustCgt when cgt client is identified" in {
        given(IdentifyTrustClient(HMRCCGTPD, emptyBasket)) when
        identifiedCgtClient(getCgtRefName)(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
        matchPattern {
          case (
            ConfirmClientTrustCgt(
            AuthorisationRequest(
            "myCgtRef",
            CgtInvitation(CgtRef("myCgtRef"), Some(`business`), HMRCCGTPD, "CGTPDRef"),
            AuthorisationRequest.NEW,
            _),
            `emptyBasket`),
            _) =>
        }
      }

    }

    "at state IdentifyBusinessClient" should {

      def clientName(service: String, clientId: String) = Future(Some("Piglet"))

      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(false)

      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(false)

      def createMultipleInvitations(arn: Arn,
                                    requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] = Future(emptyBasket)

      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

      "transition to SelectClientType" in {

        given(IdentifyBusinessClient) when start should thenGo(SelectClientType(emptyBasket))
      }

      "transition to ConfirmClientBusinessVat" in {

        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(204))

        given(IdentifyBusinessClient) when
        identifiedVatClient(
          checkRegDateMatches)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          clientName)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          VatClient("123456", "2010-10-10")) should
        matchPattern {
          case (
            ConfirmClientBusinessVat(
            AuthorisationRequest(
            "Piglet",
            VatInvitation(Some(_), Vrn("123456"), VatRegDate("2010-10-10"), HMRCMTDVAT, "vrn"),
            AuthorisationRequest.NEW,
            _)),
            _) =>
        }
      }

      "transition to KnownFactNotMatched client" in {

        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) =
          Future(Some(404))

        given(IdentifyBusinessClient) when
        identifiedVatClient(
          checkRegDateMatches)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          clientName)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          VatClient("123456", "2010-10-10")) should
        thenGo(KnownFactNotMatched(emptyBasket))
      }

      "transition to CannotCreateRequest" in {

        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) =
          Future(Some(423))

        given(IdentifyBusinessClient) when
        identifiedVatClient(
          checkRegDateMatches)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          clientName)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          VatClient("123456", "2010-10-10")) should
        thenGo(CannotCreateRequest(emptyBasket))
      }

      "transition to ActiveRelationshipExists" in {

        def hasActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(true)

        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(204))

        given(IdentifyBusinessClient) when
        identifiedVatClient(
          checkRegDateMatches)(
          hasNoPendingInvitation)(
          hasActiveRelationship)(
          clientName)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          VatClient("123456", "2010-10-10")) should
        matchPattern {
          case (ConfirmClientBusinessVat(
          AuthorisationRequest("Piglet",
                               VatInvitation(Some(_), Vrn("123456"), VatRegDate("2010-10-10"), HMRCMTDVAT, "vrn"),
                               AuthorisationRequest.NEW, _)), _) =>
        }
      }

      "transition to ClientNotSignedUp" in {

        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(None)

        given(IdentifyBusinessClient) when
        identifiedVatClient(
          checkRegDateMatches)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          clientName)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          VatClient("123456", "2010-10-10")) should
        thenGo(ClientNotSignedUp(HMRCMTDVAT, emptyBasket))
      }
    }

    "at state ConfirmClientItsa" should {

      val authorisationRequest = AuthorisationRequest("Piglet", ItsaInvitation(Nino("AB123456A"), Postcode("BN114AW")))

      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(false)

      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(false)

      def createMultipleInvitations(arn: Arn,
                                    requests: Set[AuthorisationRequest]
                                   ): Future[Set[AuthorisationRequest]] = Future(emptyBasket)

      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

      "transition to SelectClientType" in {

        given(ConfirmClientItsa(authorisationRequest, emptyBasket)) when
        start should
        thenGo(SelectClientType(emptyBasket))
      }

      "transition to ReviewAuthorisationsPersonal" in {

        given(ConfirmClientItsa(authorisationRequest, emptyBasket)) when
        clientConfirmed(
          showCgtFlag = false)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          authorisedAgent)(
          Confirmation(true)) should
        thenMatch {
          case ReviewAuthorisationsPersonal(_, basket) if basket.nonEmpty =>
        }
      }

      "transition to SelectPersonalService" in {

        given(ConfirmClientItsa(authorisationRequest, emptyBasket)) when
        clientConfirmed(
          showCgtFlag = false)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          authorisedAgent)(
          Confirmation(false)) should
        thenGo(IdentifyPersonalClient(HMRCMTDIT, emptyBasket))
      }

      "transition to PendingInvitationExists when the invitation is already in the basket" in {

        given(
          ConfirmClientItsa(
            authorisationRequest,
            Set(
              AuthorisationRequest(
                "client name",
                Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW")
                )
              )
            )
          ) when
        clientConfirmed(
          showCgtFlag = false)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          authorisedAgent)(
          Confirmation(true)) should
        thenMatch {
          case PendingInvitationExists(_, basket) if basket.nonEmpty =>
        }
      }
    }

    "at state ConfirmClientPersonalVat" should {

      def createMultipleInvitations(arn: Arn,
                                    requests: Set[AuthorisationRequest]
                                   ): Future[Set[AuthorisationRequest]] = Future(emptyBasket)

      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(false)

      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(false)

      "transition to Start" in {

        given(
          ConfirmClientPersonalVat(
            AuthorisationRequest(
              "Piglet",
              VatInvitation(
                Some(personal),
                Vrn("123456"),
                VatRegDate("2010-10-10")
                )
              ),
            emptyBasket
            )
          ) when
        start should
        thenGo(SelectClientType(emptyBasket))
      }

      "transition to ClientConfirmedPersonal" in {

        given(
          ConfirmClientPersonalVat(
            AuthorisationRequest(
              "Piglet",
              VatInvitation(
                Some(personal),
                Vrn("123456"),
                VatRegDate("2010-10-10")
                )
              ),
            emptyBasket
            )
          ) when
        clientConfirmed(
          showCgtFlag = false)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          authorisedAgent)(Confirmation(true)) should
        thenMatch {
          case ReviewAuthorisationsPersonal(_, basket) if basket.nonEmpty =>
        }
      }

      "transition to PersonalServiceSelected" in {

        given(
          ConfirmClientPersonalVat(
            AuthorisationRequest(
              "Piglet",
              VatInvitation(
                Some(personal),
                Vrn("123456"),
                VatRegDate("2010-10-10")
                )
              ),
            emptyBasket
            )
          ) when
        clientConfirmed(
          showCgtFlag = false)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          authorisedAgent)(Confirmation(false)) should
        thenGo(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket))
      }
    }

    "at state ConfirmClientBusinessVat" should {

      val authorisationRequest =
        AuthorisationRequest(
          "Piglet",
          VatInvitation(Some(business), Vrn("123456"), VatRegDate("2010-10-10"))
          )

      def createMultipleInvitations(arn: Arn,
                                    requests: Set[AuthorisationRequest]
                                   ): Future[Set[AuthorisationRequest]] = Future(emptyBasket)

      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(false)

      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(false)

      "after start transition to Start" in {

        given(ConfirmClientBusinessVat(authorisationRequest)) when start should thenGo(
          SelectClientType(emptyBasket)
          )
      }

      "after clientConfirmed(true) transition to InvitationSentBusiness" in {

        given(ConfirmClientBusinessVat(authorisationRequest)) when
        clientConfirmed(
          showCgtFlag = false)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          authorisedAgent)(
          Confirmation(true)) should
        thenGo(InvitationSentBusiness("invitation/link", None, "abc@xyz.com"))
      }

      "after clientConfirmed(false) transition to IdentifyBusinessClient" in {

        given(ConfirmClientBusinessVat(authorisationRequest)) when
        clientConfirmed(
          showCgtFlag = false)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          authorisedAgent)(
          Confirmation(false)) should
        thenGo(IdentifyBusinessClient)
      }

      "transition to PendingInvitationExists when there is already a pending invitation" in {

        def hasPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(true)

        given(ConfirmClientBusinessVat(authorisationRequest)) when
        clientConfirmed(
          showCgtFlag = false)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          hasPendingInvitation)(
          hasNoActiveRelationship)(
          authorisedAgent)(
          Confirmation(true)) should
        thenGo(PendingInvitationExists(business, emptyBasket))
      }

      "transition to ActiveAuthorisationEXists when an active relationship already exists" in {

        def hasActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(true)

        given(ConfirmClientBusinessVat(authorisationRequest)) when clientConfirmed(
          showCgtFlag = false
          )(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(
          hasNoPendingInvitation
          )(hasActiveRelationship)(authorisedAgent)(Confirmation(true)) should
        thenGo(ActiveAuthorisationExists(business, HMRCMTDVAT, emptyBasket))
      }
    }

    "at state ConfirmClientPersonalCgt" should {

      "transition to IdentifyPersonalClient" in {

        val authorisationRequest = AuthorisationRequest("Roo",
          CgtInvitation(CgtRef("myCgtRef"), Some(personal))
        )

        def createMultipleInvitations(arn: Arn,
                                      requests: Set[AuthorisationRequest]
                                     ): Future[Set[AuthorisationRequest]] = Future(emptyBasket)

        def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

        def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(false)

        def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(false)

        given(ConfirmClientPersonalCgt(authorisationRequest, emptyBasket)) when
        clientConfirmed(
          showCgtFlag = true)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          authorisedAgent)(
          Confirmation(false)) should
        thenGo(IdentifyPersonalClient(HMRCCGTPD, emptyBasket))

      }

    }

    "at state ConfirmClientTrustCgt" should {

      "transition to IdentifyTrustClient" in {

        val authorisationRequest = AuthorisationRequest("Roo",
          CgtInvitation(CgtRef("myCgtRef"), Some(business))
        )

        def createMultipleInvitations(arn: Arn,
                                      requests: Set[AuthorisationRequest]
                                     ): Future[Set[AuthorisationRequest]] = Future(emptyBasket)

        def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

        def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(false)

        def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(false)

        given(ConfirmClientTrustCgt(authorisationRequest, emptyBasket)) when
        clientConfirmed(
          showCgtFlag = true)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          authorisedAgent)(
          Confirmation(false)) should
        thenGo(IdentifyTrustClient(HMRCCGTPD, emptyBasket))

      }

    }

    "at state ConfirmClientTrust" should {

      val utr = Utr("4937455253")

      val authorisationRequest = AuthorisationRequest("Piglet", TrustInvitation(utr))

      def createMultipleInvitations(arn: Arn,
                                    requests: Set[AuthorisationRequest]
                                   ): Future[Set[AuthorisationRequest]] = Future(emptyBasket)

      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(false)

      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(false)

      "transition to IdentifyTrustClient if NO is selected" in {

        given(ConfirmClientTrust(authorisationRequest, emptyBasket)) when
        clientConfirmed(
          showCgtFlag = false)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          authorisedAgent)(
          Confirmation(false)) should
        thenGo(IdentifyTrustClient(TRUST, emptyBasket))
      }

      "transition to InvitationSentBusiness" in {

        given(ConfirmClientTrust(authorisationRequest, emptyBasket)) when
        clientConfirmed(
          showCgtFlag = false)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          authorisedAgent)(
          Confirmation(true)) should
        thenGo(
          InvitationSentBusiness(
            "invitation/link",
            None,
            "abc@xyz.com",
            "HMRC-TERS-ORG"
            ))
      }

      "transition to PendingInvitationExists when a pending invitation exists for the service" in {

        def hasPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(true)

        given(ConfirmClientTrust(authorisationRequest, emptyBasket)) when
        clientConfirmed(
          showCgtFlag = false)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          hasPendingInvitation)(
          hasNoActiveRelationship)(
          authorisedAgent)(
          Confirmation(true)) should
        thenGo(PendingInvitationExists(business, emptyBasket))
      }

      "transition to ActiveAuthorisationExists when a pending invitation exists for the service" in {

        def hasActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(true)

        given(ConfirmClientTrust(authorisationRequest, emptyBasket)) when
        clientConfirmed(
          showCgtFlag = false)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          hasNoPendingInvitation)(
          hasActiveRelationship)(
          authorisedAgent)(
          Confirmation(true)) should
        thenGo(ActiveAuthorisationExists(business, Services.TRUST, emptyBasket))
      }
    }

    "at state ReviewAuthorisationsTrust" should {

      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

      "after start transition to Start" in {

        given(ReviewAuthorisationsTrust(availableTrustServices, emptyBasket)) when
        start should
        thenGo(SelectClientType(emptyBasket))
      }

      "after authorisationsReviewed(true) transition to SelectTrustService" in {

        def createMultipleInvitations(arn: Arn,
                                      requests: Set[AuthorisationRequest]
                                     ): Future[Set[AuthorisationRequest]] = Future(emptyBasket)

        given(ReviewAuthorisationsTrust(availableTrustServices, emptyBasket)) when
        authorisationsReviewed(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          Confirmation(true)) should
        thenGo(SelectTrustService(availableTrustServices, emptyBasket))
      }

      "after authorisationsReviewed(false) transition to InvitationSentTrust" in {

        val authorisationRequestNew = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.NEW,
          "ABC123")

        val authorisationRequestCreated = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.CREATED,
          "ABC123")

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]) = Future(Set(authorisationRequestCreated))

        given(
          ReviewAuthorisationsTrust(
            availableTrustServices,
            Set(authorisationRequestNew)
          )
        ) when
        authorisationsReviewed(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          Confirmation(false)) should
        thenGo(InvitationSentBusiness("invitation/link", None, "abc@xyz.com"))
      }

      "after authorisationsReviewed(false) when all fail transition to AuthorisationsReviewedAllFailed" in {

        val authorisationRequestNew = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.NEW,
          "ABC123")

        val authorisationRequestFailed = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.FAILED,
          "ABC123")

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]) = Future(Set(authorisationRequestFailed))

        given(
          ReviewAuthorisationsTrust(
            availableTrustServices,
            Set(authorisationRequestNew)
          )
        ) when
        authorisationsReviewed(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          Confirmation(false)) should
        thenGo(AllAuthorisationsFailed(Set(authorisationRequestFailed)))
      }

      "after authorisationsReviewed(false) when some fail transition to AuthorisationReviewedSomeFailed" in {

        val authorisationRequestNew1 = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.NEW,
          "ABC123")

        val authorisationRequestNew2 = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456B", "BN114AT"),
          AuthorisationRequest.NEW,
          "ABC124")

        val authorisationRequestSuccess1 = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.CREATED,
          "ABC123")

        val authorisationRequestFailed2 = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456B", "BN114AT"),
          AuthorisationRequest.FAILED,
          "ABC124")

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]) =
          Future(Set(authorisationRequestSuccess1, authorisationRequestFailed2))

        given(
          ReviewAuthorisationsTrust(
            availableServices,
            Set(authorisationRequestNew1, authorisationRequestNew2)
          )
        ) when
        authorisationsReviewed(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          Confirmation(false)) should
        thenGo(
          SomeAuthorisationsFailed(
            "invitation/link",
            None,
            "abc@xyz.com",
            Set(authorisationRequestSuccess1, authorisationRequestFailed2)
          )
        )
      }

      "after deleteAuthorisationRequest with a valid itemId transition to DeleteAuthorisationRequestPersonal" in {

        val authorisationRequest = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.NEW,
          "ABC123")

        given(
          ReviewAuthorisationsTrust(
            availableTrustServices,
            Set(authorisationRequest)
          )
        ) when
        deleteAuthorisationRequest("ABC123")(authorisedAgent) should
        thenGo(
          DeleteAuthorisationRequestPersonal(
            authorisationRequest,
            Set(authorisationRequest)
          ))
      }

      "throw an Exception when there is no corresponding itemId in the basket" in {

        val authorisationRequest = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), TRUST, "AB123456A", "BN114AW"),
          AuthorisationRequest.NEW,
          "ABC123")

        intercept[Exception] {
          given(
            ReviewAuthorisationsTrust(
              availableTrustServices,
              Set(authorisationRequest)
            )
          ) when deleteAuthorisationRequest("XXX")(authorisedAgent)
        }.getMessage shouldBe "No Item to delete"
      }

    }

    "at state ReviewAuthorisationsPersonal" should {

      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

      "after start transition to Start" in {

        given(ReviewAuthorisationsPersonal(availableServices, emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }

      "after authorisationsReviewed(true) transition to SelectPersonalService" in {

        def createMultipleInvitations(arn: Arn,
                                      requests: Set[AuthorisationRequest]
                                     ): Future[Set[AuthorisationRequest]] = Future(emptyBasket)

        given(ReviewAuthorisationsPersonal(availableServices, emptyBasket)) when
        authorisationsReviewed(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          Confirmation(true)) should
        thenGo(SelectPersonalService(availableServices, emptyBasket)) //FIXME check basket has invitation added
      }

      "after authorisationsReviewed(false) transition to InvitationSentPersonal" in {

        val authorisationRequestNew = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.NEW,
          "ABC123")

        val authorisationRequestCreated = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.CREATED,
          "ABC123")

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]) = Future(Set(authorisationRequestCreated))

        given(
          ReviewAuthorisationsPersonal(
            availableServices,
            Set(authorisationRequestNew)
            )
          ) when
        authorisationsReviewed(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          Confirmation(false)) should
        thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }

      "after authorisationsReviewed(false) when all fail transition to AuthorisationsReviewedAllFailed" in {

        val authorisationRequestNew = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.NEW,
          "ABC123")

        val authorisationRequestFailed = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.FAILED,
          "ABC123")

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]) = Future(Set(authorisationRequestFailed))

        given(
          ReviewAuthorisationsPersonal(
            availableServices,
            Set(authorisationRequestNew)
            )
          ) when
        authorisationsReviewed(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          Confirmation(false)) should
        thenGo(AllAuthorisationsFailed(Set(authorisationRequestFailed)))
      }

      "after authorisationsReviewed(false) when some fail transition to AuthorisationReviewedSomeFailed" in {

        val authorisationRequestNew1 = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.NEW,
          "ABC123")

        val authorisationRequestNew2 = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456B", "BN114AT"),
          AuthorisationRequest.NEW,
          "ABC124")

        val authorisationRequestSuccess1 = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.CREATED,
          "ABC123")

        val authorisationRequestFailed2 = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456B", "BN114AT"),
          AuthorisationRequest.FAILED,
          "ABC124")

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]) =
          Future(Set(authorisationRequestSuccess1, authorisationRequestFailed2))

        given(
          ReviewAuthorisationsPersonal(
            availableServices,
            Set(authorisationRequestNew1, authorisationRequestNew2)
            )
          ) when
        authorisationsReviewed(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          Confirmation(false)) should
        thenGo(
          SomeAuthorisationsFailed(
            "invitation/link",
            None,
            "abc@xyz.com",
            Set(authorisationRequestSuccess1, authorisationRequestFailed2)
            )
          )
      }

      "after deleteAuthorisationRequest with a valid itemId transition to DeleteAuthorisationRequestPersonal" in {

        val authorisationRequest = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.NEW,
          "ABC123")

        given(
          ReviewAuthorisationsPersonal(
            availableServices,
            Set(authorisationRequest)
            )
          ) when
        deleteAuthorisationRequest("ABC123")(authorisedAgent) should
        thenGo(
          DeleteAuthorisationRequestPersonal(
            authorisationRequest,
            Set(authorisationRequest)
            ))
      }

      "throw an Exception when there is no corresponding itemId in the basket" in {

        val authorisationRequest = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.NEW,
          "ABC123")

        intercept[Exception] {
          given(
            ReviewAuthorisationsPersonal(
              availableServices,
              Set(authorisationRequest)
              )
            ) when deleteAuthorisationRequest("XXX")(authorisedAgent)
        }.getMessage shouldBe "No Item to delete"
      }
    }

    "at state DeleteAuthorisationRequestPersonal" should {

      "after start transition to Start" in {

        val authorisationRequest = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.NEW,
          "ABC123"
          )

        given(
          DeleteAuthorisationRequestPersonal(authorisationRequest, emptyBasket)
          ) when
        start should
        thenGo(SelectClientType(emptyBasket))
      }

      "after confirmDeleteAuthorisationRequest(true) should transition to ReviewAuthorisationPersonal with one request removed form the basket" in
      {

        val authorisationRequest1 = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.NEW,
          "ABC123")

        val authorisationRequest2 = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456B", "BN114AT"),
          AuthorisationRequest.NEW,
          "ABC124")

        given(
          DeleteAuthorisationRequestPersonal(
            authorisationRequest1,
            Set(authorisationRequest1, authorisationRequest2)
            )
          ) when
        confirmDeleteAuthorisationRequest(authorisedAgent)(Confirmation(true)) should
        thenGo(
          ReviewAuthorisationsPersonal(
            availableServices,
            Set(authorisationRequest2)
            ))
      }

      "after confirmDeleteAuthorisationRequest(true) should transition to AllAuthorisationsRemoved when there is nothing left in the basket" in
      {

        val authorisationRequest = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.NEW,
          "ABC123")

        given(
          DeleteAuthorisationRequestPersonal(
            authorisationRequest,
            Set(authorisationRequest))
          ) when
        confirmDeleteAuthorisationRequest(
          authorisedAgent)(
          Confirmation(true)) should
        thenGo(AllAuthorisationsRemoved)
      }

      "after confirmDeleteAuthorisationRequest(false) should transition to ReviewAuthorisationPersonal with basket in tact" in {

        val authorisationRequest = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.NEW,
          "ABC123")

        given(
          DeleteAuthorisationRequestPersonal(
            authorisationRequest,
            Set(authorisationRequest))
          ) when
        confirmDeleteAuthorisationRequest(
          authorisedAgent)(
          Confirmation(false)) should
        thenGo(
          ReviewAuthorisationsPersonal(
            availableServices,
            Set(authorisationRequest)
            ))
      }
    }

    "at state SomeAuthorisationFailed" should {

      "go to InvitationSentPersonal state" in {

        given(
          SomeAuthorisationsFailed(
            "invitation/link",
            None,
            "abc@xyz.com",
            Set.empty
            )
          ) when continueSomeResponsesFailed(authorisedAgent) should
        thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
    }
  }
}
