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

import org.joda.time.LocalDate
import play.api.http.Status
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.{AgentInvitationJourneyModel, _}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, CgtRef, Utr, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.collection.immutable.Set
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
  val authorisedAgentNotWhitelisted = AuthorisedAgent(Arn("TARN0000001"), isWhitelisted = false)
  private val availableServices = Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT, HMRCCGTPD)
  private val availableTrustServices = Set(ANYTRUST, HMRCCGTPD)
  private val nonWhitelistedServices = Set(HMRCMTDIT, HMRCMTDVAT, HMRCCGTPD)

  def makeBasket(services: Set[String]) = services.map {
    case `HMRCCGTPD` =>
      AuthorisationRequest("client", CgtInvitation(CgtRef("X"), Some(business)), AuthorisationRequest.NEW, "item-cgt")
    case `HMRCMTDVAT` =>
      AuthorisationRequest("client", VatInvitation(Some(personal), Vrn(vrn)), AuthorisationRequest.NEW, "item-vat")
    case `HMRCMTDIT` =>
      AuthorisationRequest("client", ItsaInvitation(Nino(nino)), AuthorisationRequest.NEW, "item-itsa")
  }

  val nino = "AB123456A"
  val postCode = Some("BN114AW")
  val vrn = "123456"
  val vatRegDate = Some("2010-10-10")
  val dob = Some("1990-10-10")
  val TRUSTNT = "HMRC-TERSNT-ORG"
  val TRUST = "HMRC-TERS-ORG"

  val tpd = TypeOfPersonDetails("Individual", Left(IndividualName("firstName", "lastName")))

  def cgtAddressDetails(countryCode: String = "GB") =
    CgtAddressDetails("line1", Some("line2"), Some("line2"), Some("line2"), countryCode, Some("BN13 1FN"))

  def cgtSubscription(countryCode: String = "GB") =
    CgtSubscription("CGT", SubscriptionDetails(tpd, cgtAddressDetails(countryCode)))

  def getAgencyEmail: GetAgencyEmail = () => Future("abc@xyz.com")

  def getCgtSubscription(countryCode: String = "GB"): GetCgtSubscription =
    CgtRef => Future(Some(cgtSubscription(countryCode)))

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

    // *************************************************
    //               SelectPersonalService
    // *************************************************

    "at state SelectPersonalService" should {

      def notSuspended() = SuspensionDetails(suspensionStatus = false, None)

      def selectedService(
        showItsaFlag: Boolean = true,
        showPirFlag: Boolean = true,
        showVatFlag: Boolean = true,
        showCgtFlag: Boolean = true,
        agentSuspensionEnabled: Boolean = true): String => AgentInvitationJourneyModel.Transition =
        selectedPersonalService(showItsaFlag, showPirFlag, showVatFlag, showCgtFlag, agentSuspensionEnabled, notSuspended)(authorisedAgent)

      "transition to SelectClientType" in {

        given(SelectPersonalService(availableServices, emptyBasket)) when
          start should
          thenGo(SelectClientType(emptyBasket))
      }

      "transition to IdentifyPersonalClient for ITSA service" in {

        given(SelectPersonalService(availableServices, emptyBasket)) when
          selectedService()(HMRCMTDIT) should
          thenGo(IdentifyPersonalClient(HMRCMTDIT, emptyBasket))
      }

      "transition to IdentifyPersonalClient for PIR service" in {

        given(SelectPersonalService(availableServices, emptyBasket)) when
          selectedService()(HMRCPIR) should
          thenGo(IdentifyPersonalClient(HMRCPIR, emptyBasket))
      }

      "transition to IdentifyPersonalClient for VAT service" in {

        given(SelectPersonalService(availableServices, emptyBasket)) when
          selectedService()(HMRCMTDVAT) should
          thenGo(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket))
      }

      "transition to IdentifyPersonalClient for CGT service" in {

        given(SelectPersonalService(availableServices, emptyBasket)) when
          selectedService()(HMRCCGTPD) should
          thenGo(IdentifyPersonalClient(HMRCCGTPD, emptyBasket))
      }

      "transition to ReviewPersonalService when last service selected and user does not confirm" in {

        given(SelectPersonalService(Set(HMRCPIR), makeBasket(Set(HMRCMTDIT, HMRCMTDVAT, HMRCCGTPD)))) when
          selectedService()("") should
          thenGo(ReviewAuthorisationsPersonal(Set(HMRCPIR), makeBasket(Set(HMRCMTDIT, HMRCMTDVAT, HMRCCGTPD))))
      }

      "transition to SelectPersonalService" in {

        given(SelectPersonalService(availableServices, emptyBasket)) when
          selectedService()("foo") should
          thenGo(SelectPersonalService(availableServices, emptyBasket))
      }

      "transition to AgentSuspended when agent is suspended for the selected service" in {
        def suspendedForItsa() = SuspensionDetails(suspensionStatus = true, Some(Set("ITSA")))

        def selectedService(
          showItsaFlag: Boolean = true,
          showPirFlag: Boolean = true,
          showVatFlag: Boolean = true,
          showCgtFlag: Boolean = true,
          agentSuspensionEnabled: Boolean = true): String => AgentInvitationJourneyModel.Transition =
          selectedPersonalService(showItsaFlag, showPirFlag, showVatFlag, showCgtFlag, agentSuspensionEnabled, suspendedForItsa)(authorisedAgent)

        given(SelectPersonalService(availableServices, emptyBasket)) when selectedService()(HMRCMTDIT) should thenGo(
          AgentSuspended(HMRCMTDIT, emptyBasket))
      }

      "transition to IdentifyPersonalClient when agent is suspended for a service not selected" in {
        def suspendedForItsa() = SuspensionDetails(suspensionStatus = true, Some(Set("ITSA")))

        def selectedService(
          showItsaFlag: Boolean = true,
          showPirFlag: Boolean = true,
          showVatFlag: Boolean = true,
          showCgtFlag: Boolean = true,
          agentSuspensionEnabled: Boolean = true): String => AgentInvitationJourneyModel.Transition =
          selectedPersonalService(showItsaFlag, showPirFlag, showVatFlag, showCgtFlag, agentSuspensionEnabled, suspendedForItsa)(authorisedAgent)

        given(SelectPersonalService(availableServices, emptyBasket)) when selectedService()(HMRCMTDVAT) should thenGo(
          IdentifyPersonalClient(HMRCMTDVAT, emptyBasket))
      }

      "throw an exception when the show itsa feature flag is off" in {

        intercept[Exception] {
          given(SelectPersonalService(availableServices, emptyBasket)) when
            selectedService(showItsaFlag = false)(HMRCMTDIT)
        }.getMessage shouldBe "Service: HMRC-MTD-IT feature flag is switched off"
      }

      "throw an exception when the show pir feature flag is off" in {

        intercept[Exception] {
          given(SelectPersonalService(availableServices, emptyBasket)) when
            selectedService(showPirFlag = false)(HMRCPIR)
        }.getMessage shouldBe "Service: PERSONAL-INCOME-RECORD feature flag is switched off"
      }

      "throw an exception when the show vat feature flag is off" in {

        intercept[Exception] {
          given(SelectPersonalService(availableServices, emptyBasket)) when
            selectedService(showVatFlag = false)(HMRCMTDVAT)
        }.getMessage shouldBe "Service: HMRC-MTD-VAT feature flag is switched off"
      }

      "throw an exception when the show cgt feature flag is off" in {

        intercept[Exception] {
          given(SelectPersonalService(availableServices, emptyBasket)) when
            selectedService(showCgtFlag = false)(HMRCCGTPD)
        }.getMessage shouldBe "Service: HMRC-CGT-PD feature flag is switched off"
      }

    }

    // *************************************************
    //               SelectBusinessService
    // *************************************************

    "at state SelectBusinessService" should {
      def notSuspended() = SuspensionDetails(suspensionStatus = false, None)

      "transition to SelectClientType" in {

        given(SelectBusinessService) when start should thenGo(SelectClientType(emptyBasket))
      }

      "after selectedBusinessService(true)(true) transition to IdentifyBusinessClient" in {

        given(SelectBusinessService) when
          selectedBusinessService(showVatFlag = true, agentSuspensionEnabled = true, notSuspended)(authorisedAgent)(HMRCMTDVAT) should
          thenGo(IdentifyBusinessClient)
      }

      "after selectedBusinessService(true)(false) transition to SelectClientType" in {

        given(SelectBusinessService) when
          selectedBusinessService(showVatFlag = true, agentSuspensionEnabled = true, notSuspended)(authorisedAgent)("") should
          thenGo(SelectClientType(emptyBasket))
      }

      "transition to AgentSuspended if agent is suspended for the chosen service" in {
        def suspendedForVat() = SuspensionDetails(suspensionStatus = true, Some(Set("VATC")))

        given(SelectBusinessService) when
          selectedBusinessService(showVatFlag = true, agentSuspensionEnabled = true, suspendedForVat)(authorisedAgent)(HMRCMTDVAT) should
          thenGo(AgentSuspended(HMRCMTDVAT, emptyBasket))
      }

      "transition to IdentifyBusinessClient if agent is suspended for a different service" in {
        def suspendedForItsa() = SuspensionDetails(suspensionStatus = true, Some(Set("ITSA")))

        given(SelectBusinessService) when
          selectedBusinessService(showVatFlag = true, agentSuspensionEnabled = true, suspendedForItsa)(authorisedAgent)(HMRCMTDVAT) should
          thenGo(IdentifyBusinessClient)
      }

      "throw an exception when the show vat feature flag is off" in {

        intercept[Exception] {
          given(SelectBusinessService) when
            selectedBusinessService(showVatFlag = false, agentSuspensionEnabled = true, notSuspended)(authorisedAgent)(HMRCMTDVAT)
        }.getMessage shouldBe "Service: HMRC-MTD-VAT feature flag is switched off"
      }
    }

    // *************************************************
    //               SelectTrustService
    // *************************************************

    "at state SelectTrustService" should {
      def notSuspended() = SuspensionDetails(false, None)

      "transition to SelectClientType" in {

        given(SelectTrustService(availableTrustServices, emptyBasket)) when
          start should
          thenGo(SelectClientType(emptyBasket))
      }

      "after selectedTrustService(false)(true)(true) transition to IdentifyTrustClient" in {

        given(SelectTrustService(availableTrustServices, emptyBasket)) when
          selectedTrustService(true, true, true, notSuspended)(agent = authorisedAgent)(TRUST) should
          thenGo(IdentifyTrustClient(ANYTRUST, emptyBasket))
      }

      "after selectedTrustService(false)(true)(false) transition to SelectClientType" in {

        given(SelectTrustService(availableTrustServices, emptyBasket)) when
          selectedTrustService(true, true, true, notSuspended)(agent = authorisedAgent)("") should
          thenGo(SelectClientType(emptyBasket))
      }

      "after selectedTrustService(true)(true)(false) transition to SelectClientType" in {

        given(SelectTrustService(availableTrustServices, emptyBasket)) when
          selectedTrustService(true, true, true, notSuspended)(agent = authorisedAgent)("") should
          thenGo(SelectClientType(emptyBasket))
      }

      "after selectedTrustService(true)(true)(false) with non-empty basket transition to ReviewAuthorisationsTrust" in {
        val basket = makeBasket(Set(HMRCCGTPD))
        given(SelectTrustService(availableTrustServices, basket)) when
          selectedTrustService(true, true, true, notSuspended)(agent = authorisedAgent)("") should
          thenGo(ReviewAuthorisationsTrust(availableTrustServices, basket))
      }

      "transition to AgentSuspended if the agent is suspended for the selected service" in {
        def suspendedForTrust() = SuspensionDetails(suspensionStatus = true, Some(Set("TRS")))

        given(SelectTrustService(availableTrustServices, emptyBasket)) when
          selectedTrustService(true, true, true, suspendedForTrust)(agent = authorisedAgent)(ANYTRUST) should
          thenGo(AgentSuspended(ANYTRUST, emptyBasket))
      }
    }

    // *************************************************
    //               IdentifyPersonalClient
    // *************************************************

    "at state IdentifyPersonalClient" should {

      def clientName(service: String, clientId: String) = Future(Some("Piglet"))

      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)

      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)

      def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] =
        Future(emptyBasket)

      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

      // format: off
      def itsaClientIdentified(postcodeCheck: CheckPostcodeMatches) =
        identifiedItsaClient(
          postcodeCheck)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          clientName)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          ItsaClient("AB123456A", "BN114AW"))

      def vatClientIdentified(checkRegDateMatches: CheckRegDateMatches) =
        identifiedVatClient(
          checkRegDateMatches)(
          hasNoPendingInvitation)(
          hasNoActiveRelationship)(
          clientName)(
          createMultipleInvitations)(
          getAgentLink)(
          getAgencyEmail)(
          authorisedAgent)(
          VatClient("123456", "2010-10-10"))

      // format: on

      "transition to SelectClientType" in {

        given(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)) when
          start should
          thenGo(SelectClientType(emptyBasket))
      }

      "transition to ConfirmClientItsa" in {

        given(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)) when
          itsaClientIdentified(postcodeCheck = (_, _) => Future(Some(true))) should
          matchPattern {
            case (
                ConfirmClientItsa(
                  AuthorisationRequest("Piglet", ItsaInvitation(Nino("AB123456A"), _, HMRCMTDIT, "ni"), AuthorisationRequest.NEW, _),
                  `emptyBasket`),
                _) =>
          }
      }

      "transition to KnownFactsNotMatched when the nino and postcode do not match" in {

        given(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)) when
          itsaClientIdentified(postcodeCheck = (_, _) => Future(Some(false))) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }

      "transition to ConfirmPostcodeCgt for personal cgt clients" in {

        given(IdentifyPersonalClient(HMRCCGTPD, emptyBasket)) when
          identifyCgtClient(getCgtSubscription())(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
          matchPattern {
            case (ConfirmPostcodeCgt(CgtRef("myCgtRef"), personal, `emptyBasket`, _, _), _) =>
          }
      }

      "transition to ConfirmPostcodeCgt for trust cgt clients" in {

        given(IdentifyTrustClient(HMRCCGTPD, emptyBasket)) when
          identifyCgtClient(getCgtSubscription())(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
          matchPattern {
            case (ConfirmPostcodeCgt(CgtRef("myCgtRef"), business, `emptyBasket`, _, _), _) =>
          }
      }

      "transition to CgtRefNotFound" in {
        given(IdentifyPersonalClient(HMRCCGTPD, emptyBasket)) when
          identifyCgtClient(cgtRef => Future.successful(None))(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
          matchPattern {
            case (CgtRefNotFound(CgtRef("myCgtRef"), _), _) =>
          }
      }

      "transition to ConfirmClientPersonalVat" in {

        given(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket)) when
          vatClientIdentified(checkRegDateMatches = (_, _) => Future(Some(Status.NO_CONTENT))) should
          matchPattern {
            case (
                ConfirmClientPersonalVat(
                  AuthorisationRequest("Piglet", VatInvitation(Some(_), Vrn("123456"), HMRCMTDVAT, "vrn"), AuthorisationRequest.NEW, _),
                  `emptyBasket`),
                _) =>
          }
      }

      "transition to KnownFactNotMatched when the vrn and regDate don't match" in {

        given(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket)) when
          vatClientIdentified(checkRegDateMatches = (_, _) => Future(Some(Status.FORBIDDEN))) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }

      "transition to CannotCreateRequest when a migration is in process" in {

        given(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket)) when
          vatClientIdentified(checkRegDateMatches = (_, _) => Future(Some(Status.LOCKED))) should
          thenGo(CannotCreateRequest(emptyBasket))
      }

      "transition to ClientNotSignedUp when the client is not signed up for the service" in {

        given(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket)) when
          vatClientIdentified(checkRegDateMatches = (_, _) => Future(None)) should
          thenGo(ClientNotSignedUp(HMRCMTDVAT, emptyBasket))
      }

      "transition to KnownFactNotMatched when the nino and dob don't match" in {

        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(false))

        given(IdentifyPersonalClient(HMRCPIR, emptyBasket)) when
          identifiedIrvClient(checkDobMatches)(hasNoPendingInvitation)(hasNoActiveRelationship)(clientName)(createMultipleInvitations)(getAgentLink)(
            getAgencyEmail)(authorisedAgent)(IrvClient("AB123456A", "1990-10-10")) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }

      "transition to KnownFactNotMatched when client not found" in {

        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(None)

        given(IdentifyPersonalClient(HMRCPIR, emptyBasket)) when
          identifiedIrvClient(checkDobMatches)(hasNoPendingInvitation)(hasNoActiveRelationship)(clientName)(createMultipleInvitations)(getAgentLink)(
            getAgencyEmail)(authorisedAgent)(IrvClient("AB123456A", "1990-10-10")) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }
    }

    // *************************************************
    //               IdentifyTrustClient
    // *************************************************

    "at state IdentifyTrustClient" should {

      "transition to ConfirmPostcodeCgt when cgt client is identified for a personal" in {
        given(IdentifyTrustClient(HMRCCGTPD, emptyBasket)) when
          identifyCgtClient(getCgtSubscription())(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
          matchPattern {
            case (ConfirmPostcodeCgt(CgtRef("myCgtRef"), personal, emptyBasket, _, _), _) =>
          }
      }

      "transition to ConfirmPostcodeCgt when cgt client is identified for a trust" in {
        given(IdentifyTrustClient(HMRCCGTPD, emptyBasket)) when
          identifyCgtClient(getCgtSubscription())(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
          matchPattern {
            case (ConfirmPostcodeCgt(CgtRef("myCgtRef"), business, emptyBasket, _, _), _) =>
          }
      }

      "transition to CgtRefNotFound" in {
        given(IdentifyPersonalClient(HMRCCGTPD, emptyBasket)) when
          identifyCgtClient(cgtRef => Future.successful(None))(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
          matchPattern {
            case (CgtRefNotFound(CgtRef("myCgtRef"), _), _) =>
          }
      }
    }

    "at state ConfirmPostcodeCgt" should {

      "transition to ConfirmClientCgt when the postcode is matched for a UK personal client" in {
        given(ConfirmPostcodeCgt(CgtRef("cgtRef"), personal, emptyBasket, Some("BN13 1FN"), "firstName lastName")) when
          confirmPostcodeCgt(getCgtSubscription())(authorisedAgent)(Postcode("BN13 1FN")) should
          matchPattern {
            case (ConfirmClientCgt(AuthorisationRequest("firstName lastName", _, _, _), _), _) =>
          }
      }

      "transition to ConfirmClientCgt when the postcode is matched for a UK business/trustee client" in {
        given(ConfirmPostcodeCgt(CgtRef("cgtRef"), business, emptyBasket, Some("BN13 1FN"), "firstName lastName")) when
          confirmPostcodeCgt(getCgtSubscription())(authorisedAgent)(Postcode("BN13 1FN")) should
          matchPattern {
            case (ConfirmClientCgt(AuthorisationRequest("firstName lastName", _, _, _), _), _) =>
          }
      }

      "transition to KnownFactsNotMatched when the postcode is not matched for a UK client" in {
        given(ConfirmPostcodeCgt(CgtRef("cgtRef"), personal, emptyBasket, Some("BN13 1FN"), "firstName lastName")) when
          confirmPostcodeCgt(getCgtSubscription())(authorisedAgent)(Postcode("BN13 1ZZ")) should
          matchPattern {
            case (KnownFactNotMatched(_), _) =>
          }
      }
    }

    "at state ConfirmCountryCodeCgt" should {

      "transition to ConfirmClientCgt when the countryCode is matched for a personal client" in {
        given(ConfirmCountryCodeCgt(CgtRef("cgtRef"), personal, emptyBasket, "IN", "firstName lastName")) when
          confirmCountryCodeCgt(getCgtSubscription("IN"))(authorisedAgent)(CountryCode("IN")) should
          matchPattern {
            case (ConfirmClientCgt(AuthorisationRequest("firstName lastName", _, _, _), _), _) =>
          }
      }

      "transition to ConfirmClientTrustCgt when the countryCode is matched for a business client" in {
        given(ConfirmCountryCodeCgt(CgtRef("cgtRef"), business, emptyBasket, "IN", "firstName lastName")) when
          confirmCountryCodeCgt(getCgtSubscription("IN"))(authorisedAgent)(CountryCode("IN")) should
          matchPattern {
            case (ConfirmClientCgt(AuthorisationRequest("firstName lastName", _, _, _), _), _) =>
          }
      }

      "transition to KnownFactsNotMatched when the countryCode is not matched for a non UK client" in {
        given(ConfirmCountryCodeCgt(CgtRef("cgtRef"), personal, emptyBasket, "IN", "firstName lastName")) when
          confirmCountryCodeCgt(getCgtSubscription("IN"))(authorisedAgent)(CountryCode("FR")) should
          matchPattern {
            case (KnownFactNotMatched(_), _) =>
          }
      }
    }

    "at state IdentifyBusinessClient" should {

      def clientName(service: String, clientId: String) = Future(Some("Piglet"))

      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)

      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)

      def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] =
        Future(emptyBasket)

      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

      "transition to SelectClientType" in {

        given(IdentifyBusinessClient) when start should thenGo(SelectClientType(emptyBasket))
      }

      "transition to ConfirmClientBusinessVat" in {

        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(204))

        given(IdentifyBusinessClient) when
          identifiedVatClient(checkRegDateMatches)(hasNoPendingInvitation)(hasNoActiveRelationship)(clientName)(createMultipleInvitations)(
            getAgentLink)(getAgencyEmail)(authorisedAgent)(VatClient("123456", "2010-10-10")) should
          matchPattern {
            case (
                ConfirmClientBusinessVat(
                  AuthorisationRequest("Piglet", VatInvitation(Some(_), Vrn("123456"), HMRCMTDVAT, "vrn"), AuthorisationRequest.NEW, _)),
                _) =>
          }
      }

      "transition to KnownFactNotMatched client" in {

        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) =
          Future(Some(404))

        given(IdentifyBusinessClient) when
          identifiedVatClient(checkRegDateMatches)(hasNoPendingInvitation)(hasNoActiveRelationship)(clientName)(createMultipleInvitations)(
            getAgentLink)(getAgencyEmail)(authorisedAgent)(VatClient("123456", "2010-10-10")) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }

      "transition to CannotCreateRequest" in {

        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) =
          Future(Some(423))

        given(IdentifyBusinessClient) when
          identifiedVatClient(checkRegDateMatches)(hasNoPendingInvitation)(hasNoActiveRelationship)(clientName)(createMultipleInvitations)(
            getAgentLink)(getAgencyEmail)(authorisedAgent)(VatClient("123456", "2010-10-10")) should
          thenGo(CannotCreateRequest(emptyBasket))
      }

      "transition to ActiveRelationshipExists" in {

        def hasActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(true)

        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(204))

        given(IdentifyBusinessClient) when
          identifiedVatClient(checkRegDateMatches)(hasNoPendingInvitation)(hasActiveRelationship)(clientName)(createMultipleInvitations)(
            getAgentLink)(getAgencyEmail)(authorisedAgent)(VatClient("123456", "2010-10-10")) should
          matchPattern {
            case (
                ConfirmClientBusinessVat(
                  AuthorisationRequest("Piglet", VatInvitation(Some(_), Vrn("123456"), HMRCMTDVAT, "vrn"), AuthorisationRequest.NEW, _)),
                _) =>
          }
      }

      "transition to ClientNotSignedUp" in {

        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(None)

        given(IdentifyBusinessClient) when
          identifiedVatClient(checkRegDateMatches)(hasNoPendingInvitation)(hasNoActiveRelationship)(clientName)(createMultipleInvitations)(
            getAgentLink)(getAgencyEmail)(authorisedAgent)(VatClient("123456", "2010-10-10")) should
          thenGo(ClientNotSignedUp(HMRCMTDVAT, emptyBasket))
      }
    }

    "at state ConfirmClientItsa" should {

      val authorisationRequest = AuthorisationRequest("Piglet", ItsaInvitation(Nino("AB123456A")))

      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)

      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)

      def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] =
        Future(emptyBasket)

      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

      "transition to SelectClientType" in {

        given(ConfirmClientItsa(authorisationRequest, emptyBasket)) when
          start should
          thenGo(SelectClientType(emptyBasket))
      }

      "transition to ReviewAuthorisationsPersonal" in {

        given(ConfirmClientItsa(authorisationRequest, emptyBasket)) when
          clientConfirmed(showCgtFlag = false)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
            hasNoActiveRelationship)(authorisedAgent)(Confirmation(true)) should
          thenMatch {
            case ReviewAuthorisationsPersonal(_, basket) if basket.nonEmpty =>
          }
      }

      "transition to SelectPersonalService" in {

        given(ConfirmClientItsa(authorisationRequest, emptyBasket)) when
          clientConfirmed(showCgtFlag = false)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
            hasNoActiveRelationship)(authorisedAgent)(Confirmation(false)) should
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
          clientConfirmed(showCgtFlag = false)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
            hasNoActiveRelationship)(authorisedAgent)(Confirmation(true)) should
          thenMatch {
            case PendingInvitationExists(_, basket) if basket.nonEmpty =>
          }
      }
    }

    "at state ConfirmClientPersonalVat" should {

      def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] =
        Future(emptyBasket)

      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)

      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)

      "transition to Start" in {

        given(
          ConfirmClientPersonalVat(
            AuthorisationRequest(
              "Piglet",
              VatInvitation(
                Some(personal),
                Vrn("123456")
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
                Vrn("123456")
              )
            ),
            emptyBasket
          )
        ) when
          clientConfirmed(showCgtFlag = false)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
            hasNoActiveRelationship)(authorisedAgent)(Confirmation(true)) should
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
                Vrn("123456")
              )
            ),
            emptyBasket
          )
        ) when
          clientConfirmed(showCgtFlag = false)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
            hasNoActiveRelationship)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket))
      }
    }

    "at state ConfirmClientBusinessVat" should {

      val authorisationRequest =
        AuthorisationRequest(
          "Piglet",
          VatInvitation(Some(business), Vrn("123456"))
        )

      def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] =
        Future(emptyBasket)

      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)

      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)

      "after start transition to Start" in {

        given(ConfirmClientBusinessVat(authorisationRequest)) when start should thenGo(
          SelectClientType(emptyBasket)
        )
      }

      "after clientConfirmed(true) transition to InvitationSentBusiness" in {

        given(ConfirmClientBusinessVat(authorisationRequest)) when
          clientConfirmed(showCgtFlag = false)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
            hasNoActiveRelationship)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentBusiness("invitation/link", None, "abc@xyz.com", Set(authorisationRequest.invitation.service)))
      }

      "after clientConfirmed(false) transition to IdentifyBusinessClient" in {

        given(ConfirmClientBusinessVat(authorisationRequest)) when
          clientConfirmed(showCgtFlag = false)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
            hasNoActiveRelationship)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyBusinessClient)
      }

      "transition to PendingInvitationExists when there is already a pending invitation" in {

        def hasPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(true)

        given(ConfirmClientBusinessVat(authorisationRequest)) when
          clientConfirmed(showCgtFlag = false)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(hasPendingInvitation)(
            hasNoActiveRelationship)(authorisedAgent)(Confirmation(true)) should
          thenGo(PendingInvitationExists(business, emptyBasket))
      }

      "transition to ActiveAuthorisationEXists when an active relationship already exists" in {

        def hasActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(true)

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

        val authorisationRequest =
          AuthorisationRequest("Roo", CgtInvitation(CgtRef("myCgtRef"), Some(personal)))

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] = Future(emptyBasket)

        def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

        def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(false)

        def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(false)

        given(ConfirmClientCgt(authorisationRequest, emptyBasket)) when
          clientConfirmed(showCgtFlag = true)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
            hasNoActiveRelationship)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyPersonalClient(HMRCCGTPD, emptyBasket))

      }

    }

    "at state ConfirmClientTrustCgt" should {

      "transition to IdentifyTrustClient" in {

        val authorisationRequest =
          AuthorisationRequest("Roo", CgtInvitation(CgtRef("myCgtRef"), Some(business)))

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] = Future(emptyBasket)

        def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

        def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(false)

        def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(false)

        given(ConfirmClientCgt(authorisationRequest, emptyBasket)) when
          clientConfirmed(showCgtFlag = true)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
            hasNoActiveRelationship)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyTrustClient(HMRCCGTPD, emptyBasket))

      }

    }

    "at state ConfirmClientTrust" should {

      val utr = Utr("4937455253")

      val authorisationRequest = AuthorisationRequest("Piglet", TrustInvitation(utr))

      def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] =
        Future(emptyBasket)

      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)

      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)

      "transition to IdentifyTrustClient if NO is selected" in {

        given(ConfirmClientTrust(authorisationRequest, emptyBasket)) when
          clientConfirmed(showCgtFlag = false)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
            hasNoActiveRelationship)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyTrustClient(ANYTRUST, emptyBasket))
      }

      "transition to InvitationSentBusiness with taxable trust" in {

        given(ConfirmClientTrust(authorisationRequest, emptyBasket)) when
          clientConfirmed(showCgtFlag = false)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
            hasNoActiveRelationship)(authorisedAgent)(Confirmation(true)) should
          thenGo(
            InvitationSentBusiness(
              "invitation/link",
              None,
              "abc@xyz.com",
              Set("HMRC-TERS-ORG")
            ))
      }

      "transition to PendingInvitationExists when a pending invitation exists for the service" in {

        def hasPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(true)

        given(ConfirmClientTrust(authorisationRequest, emptyBasket)) when
          clientConfirmed(showCgtFlag = false)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(hasPendingInvitation)(
            hasNoActiveRelationship)(authorisedAgent)(Confirmation(true)) should
          thenGo(PendingInvitationExists(business, emptyBasket))
      }

      "transition to ActiveAuthorisationExists when a pending invitation exists for the service" in {

        def hasActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(true)

        given(ConfirmClientTrust(authorisationRequest, emptyBasket)) when
          clientConfirmed(showCgtFlag = false)(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(hasNoPendingInvitation)(
            hasActiveRelationship)(authorisedAgent)(Confirmation(true)) should
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

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] = Future(emptyBasket)

        given(ReviewAuthorisationsTrust(availableTrustServices, emptyBasket)) when
          authorisationsReviewed(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(authorisedAgent)(Confirmation(true)) should
          thenGo(SelectTrustService(availableTrustServices, emptyBasket))
      }

      "after authorisationsReviewed(false) transition to InvitationSentTrust" in {

        val authorisationRequestNew =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        val authorisationRequestCreated =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.CREATED, "ABC123")

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]) =
          Future(Set(authorisationRequestCreated))

        given(
          ReviewAuthorisationsTrust(
            availableTrustServices,
            Set(authorisationRequestNew)
          )
        ) when
          authorisationsReviewed(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(authorisedAgent)(Confirmation(false)) should
          thenGo(InvitationSentBusiness("invitation/link", None, "abc@xyz.com", Set(HMRCMTDIT)))
      }

      "after authorisationsReviewed(false) when all fail transition to AuthorisationsReviewedAllFailed" in {

        val authorisationRequestNew =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        val authorisationRequestFailed =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.FAILED, "ABC123")

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]) =
          Future(Set(authorisationRequestFailed))

        given(
          ReviewAuthorisationsTrust(
            availableTrustServices,
            Set(authorisationRequestNew)
          )
        ) when
          authorisationsReviewed(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(authorisedAgent)(Confirmation(false)) should
          thenGo(AllAuthorisationsFailed(Set(authorisationRequestFailed)))
      }

      "after authorisationsReviewed(false) when some fail transition to AuthorisationReviewedSomeFailed" in {

        val authorisationRequestNew1 =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        val authorisationRequestNew2 =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456B", "BN114AT"), AuthorisationRequest.NEW, "ABC124")

        val authorisationRequestSuccess1 =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.CREATED, "ABC123")

        val authorisationRequestFailed2 =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456B", "BN114AT"), AuthorisationRequest.FAILED, "ABC124")

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]) =
          Future(Set(authorisationRequestSuccess1, authorisationRequestFailed2))

        given(
          ReviewAuthorisationsTrust(
            availableServices,
            Set(authorisationRequestNew1, authorisationRequestNew2)
          )
        ) when
          authorisationsReviewed(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(authorisedAgent)(Confirmation(false)) should
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

        val authorisationRequest =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

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

        val authorisationRequest =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), TRUST, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

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

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] = Future(emptyBasket)

        given(ReviewAuthorisationsPersonal(availableServices, emptyBasket)) when
          authorisationsReviewed(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(authorisedAgent)(Confirmation(true)) should
          thenGo(SelectPersonalService(availableServices, emptyBasket)) //FIXME check basket has invitation added
      }

      "after authorisationsReviewed(false) transition to InvitationSentPersonal" in {

        val authorisationRequestNew =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        val authorisationRequestCreated =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.CREATED, "ABC123")

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]) =
          Future(Set(authorisationRequestCreated))

        given(
          ReviewAuthorisationsPersonal(
            availableServices,
            Set(authorisationRequestNew)
          )
        ) when
          authorisationsReviewed(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(authorisedAgent)(Confirmation(false)) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Set(HMRCMTDIT)))
      }

      "after authorisationsReviewed(false) when all fail transition to AuthorisationsReviewedAllFailed" in {

        val authorisationRequestNew =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        val authorisationRequestFailed =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.FAILED, "ABC123")

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]) =
          Future(Set(authorisationRequestFailed))

        given(
          ReviewAuthorisationsPersonal(
            availableServices,
            Set(authorisationRequestNew)
          )
        ) when
          authorisationsReviewed(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(authorisedAgent)(Confirmation(false)) should
          thenGo(AllAuthorisationsFailed(Set(authorisationRequestFailed)))
      }

      "after authorisationsReviewed(false) when some fail transition to AuthorisationReviewedSomeFailed" in {

        val authorisationRequestNew1 =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        val authorisationRequestNew2 =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456B", "BN114AT"), AuthorisationRequest.NEW, "ABC124")

        val authorisationRequestSuccess1 =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.CREATED, "ABC123")

        val authorisationRequestFailed2 =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456B", "BN114AT"), AuthorisationRequest.FAILED, "ABC124")

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]) =
          Future(Set(authorisationRequestSuccess1, authorisationRequestFailed2))

        given(
          ReviewAuthorisationsPersonal(
            availableServices,
            Set(authorisationRequestNew1, authorisationRequestNew2)
          )
        ) when
          authorisationsReviewed(createMultipleInvitations)(getAgentLink)(getAgencyEmail)(authorisedAgent)(Confirmation(false)) should
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

        val authorisationRequest =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

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

        val authorisationRequest =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

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

      "after confirmDeleteAuthorisationRequest(true) should transition to ReviewAuthorisationPersonal with one request removed form the basket" in {

        val authorisationRequest1 =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        val authorisationRequest2 =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456B", "BN114AT"), AuthorisationRequest.NEW, "ABC124")

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

      "after confirmDeleteAuthorisationRequest(true) should transition to AllAuthorisationsRemoved when there is nothing left in the basket" in {

        val authorisationRequest =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        given(
          DeleteAuthorisationRequestPersonal(authorisationRequest, Set(authorisationRequest))
        ) when
          confirmDeleteAuthorisationRequest(authorisedAgent)(Confirmation(true)) should
          thenGo(AllAuthorisationsRemoved)
      }

      "after confirmDeleteAuthorisationRequest(false) should transition to ReviewAuthorisationPersonal with basket in tact" in {

        val authorisationRequest =
          AuthorisationRequest("Mr Client", Invitation(Some(personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        given(
          DeleteAuthorisationRequestPersonal(authorisationRequest, Set(authorisationRequest))
        ) when
          confirmDeleteAuthorisationRequest(authorisedAgent)(Confirmation(false)) should
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
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Set.empty))
      }
    }
  }
}
