/*
 * Copyright 2022 HM Revenue & Customs
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
import org.mockito.Mockito.{mock, when}
import play.api.test.Helpers._
import support.UnitSpec
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.TransitionEffects._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel._
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models.VatKnownFactCheckResult._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, CgtRef, PptRef, SuspensionDetails, Utr, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

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
  val authorisedAgent = AuthorisedAgent(Arn("TARN0000001"))
  val authorisedAgentNotAllowlisted = AuthorisedAgent(Arn("TARN0000001"))
  private val availableServices = Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT, HMRCCGTPD, HMRCPPTORG)
  private val availableBusinessServices = Set(HMRCMTDVAT, HMRCPPTORG)
  private val availableTrustServices = Set(TAXABLETRUST, HMRCCGTPD, HMRCPPTORG)
  private val nonAllowlistedServices = Set(HMRCMTDIT, HMRCMTDVAT, HMRCCGTPD, HMRCPPTORG)
  private val mockAppConfig = mock(classOf[AppConfig])

  def makeBasket(services: Set[String]) = services.map {
    case `HMRCCGTPD` =>
      AuthorisationRequest("client", CgtInvitation(CgtRef("X"), Some(Business)), AuthorisationRequest.NEW, "item-cgt")
    case `HMRCMTDVAT` =>
      AuthorisationRequest("client", VatInvitation(Some(Personal), Vrn(vrn)), AuthorisationRequest.NEW, "item-vat")
    case `HMRCMTDIT` =>
      AuthorisationRequest("client", ItsaInvitation(Nino(nino)), AuthorisationRequest.NEW, "item-itsa")
    case `HMRCPPTORG` =>
      AuthorisationRequest("client", PptInvitation(PptRef(pptRef), Some(Personal)), AuthorisationRequest.NEW, "item-ppt")
  }

  val nino = "AB123456A"
  val postCode = Some("BN114AW")
  val vrn = "123456"
  val vatRegDate = Some("2010-10-10")
  val dob = Some("1990-10-10")
  val pptRef = "XAPPT000012345"
  val pptRegDate = new LocalDate(2021, 1, 1)
  val pptRegDateStr = pptRegDate.toString("yyyy-MM-dd")

  val tpd = TypeOfPersonDetails("Individual", Left(IndividualName("firstName", "lastName")))

  def cgtAddressDetails(countryCode: String = "GB") =
    CgtAddressDetails("line1", Some("line2"), Some("line2"), Some("line2"), countryCode, Some("BN13 1FN"))

  def cgtSubscription(countryCode: String = "GB") =
    CgtSubscription("CGT", SubscriptionDetails(tpd, cgtAddressDetails(countryCode)))

  def getClientName: GetClientName = (_: String, _: String) => Future(Some("Piglet"))

  def getAgencyEmail: GetAgencyEmail = () => Future("abc@xyz.com")

  def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")

  def createInvitationSentMock: CreateInvitationSent = (_: String, _: String, _: Arn, _: Basket) => Future(???)

  def createMultipleInvitationsEmpty(arn: Arn, requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] = Future(emptyBasket)

  def legacySaRelationshipStatusMapped(arn: Arn, clientId: String): Future[LegacySaRelationshipResult] =
    Future.successful(LegacySaRelationshipFoundAndMapped)

  def legacySaRelationshipStatusNotMapped(arn: Arn, clientId: String): Future[LegacySaRelationshipResult] =
    Future.successful(LegacySaRelationshipFoundNotMapped)

  def legacySaRelationshipStatusNotFound(arn: Arn, clientId: String): Future[LegacySaRelationshipResult] =
    Future.successful(LegacySaRelationshipNotFound)

  def getCgtSubscription(countryCode: String = "GB"): GetCgtSubscription =
    CgtRef => Future(Some(cgtSubscription(countryCode)))

  def getPptSubscription(registrationDate: LocalDate = pptRegDate): GetPptSubscription =
    PptRef => Future(Some(PptSubscription("PPT Client", registrationDate, None)))

  def notSuspended() = Future.successful(SuspensionDetails(suspensionStatus = false, None))

  def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
    Future.successful(false)

  def hasPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
    Future.successful(true)

  def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
    Future.successful(false)

  def hasActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
    Future.successful(true)

  def hasNoPartialAuthorisation(arn: Arn, clientId: String): Future[Boolean] =
    Future successful (false)

  // This is the default behaviour. Modify as needed in individual tests
  val transitions = Transitions(
    getSuspensionDetails = notSuspended,
    hasPendingInvitationsFor = hasNoPendingInvitation,
    hasActiveRelationshipFor = hasNoActiveRelationship,
    hasPartialAuthorisationFor = hasNoPartialAuthorisation,
    legacySaRelationshipStatusFor = legacySaRelationshipStatusNotFound,
    hasAltItsaInvitations = hasNoPartialAuthorisation,
    checkDobMatches = (_, _) => Future.successful(None),
    checkPostcodeMatches = (_, _) => Future.successful(None),
    checkRegDateMatches = (_, _) => Future.successful(VatDetailsNotFound),
    getClientName = getClientName,
    getAgentLink = getAgentLink,
    getAgencyEmail = getAgencyEmail,
    createMultipleInvitations = createMultipleInvitationsEmpty,
    createInvitationSent = createInvitationSentMock
  )

  "AgentInvitationJourneyService" when {

    // TODO add test for selectedTrustServiceMultiple transition

    "at state SelectClientType" should {

      "transition to SelectClientType" in {

        given(SelectClientType(emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }

      "transition to SelectPersonalService" in {

        given(SelectClientType(emptyBasket)) when
          transitions.selectedClientType(authorisedAgent)("personal") should
          thenGo(SelectService(Personal, availableServices, emptyBasket))
      }

      "transition to SelectBusinessService" in {

        given(SelectClientType(emptyBasket)) when
          transitions.selectedClientType(authorisedAgent)("business") should
          thenGo(SelectService(Business, availableBusinessServices, emptyBasket))
      }
    }

    // *************************************************
    //               SelectPersonalService
    // *************************************************

    "at state SelectPersonalService" should {

      def selectedService(
        showItsaFlag: Boolean = true,
        showPirFlag: Boolean = true,
        showVatFlag: Boolean = true,
        showCgtFlag: Boolean = true,
        showPptFlag: Boolean = true,
        agentSuspensionEnabled: Boolean = true): String => AgentInvitationJourneyModel.Transition =
        transitions.selectedPersonalService(showItsaFlag, showPirFlag, showVatFlag, showCgtFlag, showPptFlag, agentSuspensionEnabled)(authorisedAgent)

      "transition to SelectClientType" in {

        given(SelectService(Personal, availableServices, emptyBasket)) when
          start should
          thenGo(SelectClientType(emptyBasket))
      }

      "transition to IdentifyPersonalClient for ITSA service" in {

        given(SelectService(Personal, availableServices, emptyBasket)) when
          selectedService()(HMRCMTDIT) should
          thenGo(IdentifyClient(Personal, HMRCMTDIT, emptyBasket))
      }

      "transition to IdentifyPersonalClient for PIR service" in {

        given(SelectService(Personal, availableServices, emptyBasket)) when
          selectedService()(HMRCPIR) should
          thenGo(IdentifyClient(Personal, HMRCPIR, emptyBasket))
      }

      "transition to IdentifyPersonalClient for VAT service" in {

        given(SelectService(Personal, availableServices, emptyBasket)) when
          selectedService()(HMRCMTDVAT) should
          thenGo(IdentifyClient(Personal, HMRCMTDVAT, emptyBasket))
      }

      "transition to IdentifyPersonalClient for CGT service" in {

        given(SelectService(Personal, availableServices, emptyBasket)) when
          selectedService()(HMRCCGTPD) should
          thenGo(IdentifyClient(Personal, HMRCCGTPD, emptyBasket))
      }

      "transition to IdentifyPersonalClient for PPT service" in {

        given(SelectService(Personal, availableServices, emptyBasket)) when
          selectedService()(HMRCPPTORG) should
          thenGo(IdentifyClient(Personal, HMRCPPTORG, emptyBasket))
      }

      "transition to ReviewPersonalService when last service selected and user does not confirm" in {

        given(SelectService(Personal, Set(HMRCPIR), makeBasket(Set(HMRCMTDIT, HMRCMTDVAT, HMRCCGTPD)))) when
          selectedService()("") should
          thenGo(ReviewAuthorisations(Personal, Set(HMRCPIR), makeBasket(Set(HMRCMTDIT, HMRCMTDVAT, HMRCCGTPD))))
      }

      "transition to SelectPersonalService" in {

        given(SelectService(Personal, availableServices, emptyBasket)) when
          selectedService()("foo") should
          thenGo(SelectService(Personal, availableServices, emptyBasket))
      }

      "transition to AgentSuspended when agent is suspended for the selected service" in {
        def suspendedForItsa() = Future.successful(SuspensionDetails(suspensionStatus = true, Some(Set("ITSA"))))

        def selectedService(
          showItsaFlag: Boolean = true,
          showPirFlag: Boolean = true,
          showVatFlag: Boolean = true,
          showCgtFlag: Boolean = true,
          showPptFlag: Boolean = true,
          agentSuspensionEnabled: Boolean = true): String => AgentInvitationJourneyModel.Transition =
          transitions
            .copy(getSuspensionDetails = suspendedForItsa)
            .selectedPersonalService(showItsaFlag, showPirFlag, showVatFlag, showCgtFlag, showPptFlag, agentSuspensionEnabled)(authorisedAgent)

        given(SelectService(Personal, availableServices, emptyBasket)) when selectedService()(HMRCMTDIT) should thenGo(
          AgentSuspended(HMRCMTDIT, emptyBasket))
      }

      "transition to IdentifyPersonalClient when agent is suspended for a service not selected" in {
        def suspendedForItsa() = Future.successful(SuspensionDetails(suspensionStatus = true, Some(Set("ITSA"))))

        def selectedService(
          showItsaFlag: Boolean = true,
          showPirFlag: Boolean = true,
          showVatFlag: Boolean = true,
          showCgtFlag: Boolean = true,
          showPptFlag: Boolean = true,
          agentSuspensionEnabled: Boolean = true): String => AgentInvitationJourneyModel.Transition =
          transitions
            .copy(getSuspensionDetails = suspendedForItsa)
            .selectedPersonalService(showItsaFlag, showPirFlag, showVatFlag, showCgtFlag, showPptFlag, agentSuspensionEnabled)(authorisedAgent)

        given(SelectService(Personal, availableServices, emptyBasket)) when selectedService()(HMRCMTDVAT) should thenGo(
          IdentifyClient(Personal, HMRCMTDVAT, emptyBasket))
      }

      "throw an exception when the show itsa feature flag is off" in {

        intercept[Exception] {
          given(SelectService(Personal, availableServices, emptyBasket)) when
            selectedService(showItsaFlag = false)(HMRCMTDIT)
        }.getMessage shouldBe "Service: HMRC-MTD-IT feature flag is switched off"
      }

      "throw an exception when the show pir feature flag is off" in {

        intercept[Exception] {
          given(SelectService(Personal, availableServices, emptyBasket)) when
            selectedService(showPirFlag = false)(HMRCPIR)
        }.getMessage shouldBe "Service: PERSONAL-INCOME-RECORD feature flag is switched off"
      }

      "throw an exception when the show vat feature flag is off" in {

        intercept[Exception] {
          given(SelectService(Personal, availableServices, emptyBasket)) when
            selectedService(showVatFlag = false)(HMRCMTDVAT)
        }.getMessage shouldBe "Service: HMRC-MTD-VAT feature flag is switched off"
      }

      "throw an exception when the show cgt feature flag is off" in {

        intercept[Exception] {
          given(SelectService(Personal, availableServices, emptyBasket)) when
            selectedService(showCgtFlag = false)(HMRCCGTPD)
        }.getMessage shouldBe "Service: HMRC-CGT-PD feature flag is switched off"
      }

      "throw an exception when the show ppt feature flag is off" in {

        intercept[Exception] {
          given(SelectService(Personal, availableServices, emptyBasket)) when
            selectedService(showPptFlag = false)(HMRCPPTORG)
        }.getMessage shouldBe "Service: HMRC-PPT-ORG feature flag is switched off"
      }
    }

    // *************************************************
    //               SelectBusinessService
    // *************************************************

    "at state SelectBusinessService" should {

      "transition to SelectClientType" in {

        given(SelectService(Business, availableServices, emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }

      "after selectedBusinessService(true)(true) transition to IdentifyBusinessClient" in {

        given(SelectService(Business, availableBusinessServices, emptyBasket)) when
          transitions.selectedBusinessService(showVatFlag = true, showPptFlag = true, agentSuspensionEnabled = true)(authorisedAgent)(HMRCMTDVAT) should
          thenGo(IdentifyClient(Business, HMRCMTDVAT, emptyBasket))
      }

      "transition to AgentSuspended if agent is suspended for the chosen service" in {
        def suspendedForVat() = Future.successful(SuspensionDetails(suspensionStatus = true, Some(Set("VATC"))))

        given(SelectService(Business, availableBusinessServices, emptyBasket)) when
          transitions
            .copy(getSuspensionDetails = suspendedForVat)
            .selectedBusinessService(showVatFlag = true, showPptFlag = true, agentSuspensionEnabled = true)(authorisedAgent)(HMRCMTDVAT) should
          thenGo(AgentSuspended(HMRCMTDVAT, emptyBasket))
      }

      "transition to IdentifyBusinessClient if agent is suspended for a different service" in {
        def suspendedForItsa() = Future.successful(SuspensionDetails(suspensionStatus = true, Some(Set("ITSA"))))

        given(SelectService(Business, availableServices, emptyBasket)) when
          transitions
            .copy(getSuspensionDetails = suspendedForItsa)
            .selectedBusinessService(showVatFlag = true, showPptFlag = true, agentSuspensionEnabled = true)(authorisedAgent)(HMRCMTDVAT) should
          thenGo(IdentifyClient(Business, HMRCMTDVAT, emptyBasket))
      }

      "throw an exception when the show vat feature flag is off" in {
        intercept[Exception] {
          given(SelectService(Business, availableServices, emptyBasket)) when
            transitions.selectedBusinessService(showVatFlag = false, showPptFlag = true, agentSuspensionEnabled = true)(authorisedAgent)(HMRCMTDVAT)
        }.getMessage shouldBe "Service: HMRC-MTD-VAT feature flag is switched off"
      }
    }

    // *************************************************
    //               SelectTrustService
    // *************************************************

    "at state SelectTrustService" should {

      "transition to SelectClientType" in {

        given(SelectService(Trust, availableTrustServices, emptyBasket)) when
          start should
          thenGo(SelectClientType(emptyBasket))
      }

      "after selectedTrustService(false)(true)(true) transition to IdentifyTrustClient" in {

        given(SelectService(Trust, availableTrustServices, emptyBasket)) when
          transitions.selectedTrustService(true, true, true, true)(agent = authorisedAgent)(TAXABLETRUST) should
          thenGo(IdentifyClient(Trust, TAXABLETRUST, emptyBasket))
      }

      "after selectedTrustService(false)(true)(false) transition to SelectClientType" in {

        given(SelectService(Trust, availableTrustServices, emptyBasket)) when
          transitions.selectedTrustService(true, true, true, true)(agent = authorisedAgent)("") should
          thenGo(SelectClientType(emptyBasket))
      }

      "after selectedTrustService(true)(true)(false) transition to SelectClientType" in {

        given(SelectService(Trust, availableTrustServices, emptyBasket)) when
          transitions.selectedTrustService(true, true, true, true)(agent = authorisedAgent)("") should
          thenGo(SelectClientType(emptyBasket))
      }

      "after selectedTrustService(true)(true)(false) with non-empty basket transition to ReviewAuthorisationsTrust" in {
        val basket = makeBasket(Set(HMRCCGTPD))
        given(SelectService(Trust, availableTrustServices, basket)) when
          transitions.selectedTrustService(true, true, true, true)(agent = authorisedAgent)("") should
          thenGo(ReviewAuthorisations(Trust, availableTrustServices, basket))
      }

      "transition to AgentSuspended if the agent is suspended for the selected service" in {
        def suspendedForTrust() = Future.successful(SuspensionDetails(suspensionStatus = true, Some(Set("TRS"))))

        given(SelectService(Trust, availableTrustServices, emptyBasket)) when
          transitions
            .copy(getSuspensionDetails = suspendedForTrust)
            .selectedTrustService(true, true, true, true)(agent = authorisedAgent)(TAXABLETRUST) should
          thenGo(AgentSuspended(TAXABLETRUST, emptyBasket))
      }
    }

    // *************************************************
    //               IdentifyPersonalClient
    // *************************************************

    "at state IdentifyPersonalClient" should {

      when(mockAppConfig.featuresAltItsa).thenReturn(true)

      // format: off
      def itsaClientIdentified(postcodeCheckResult: Option[Boolean]): Transition =
        transitions.copy(checkPostcodeMatches = (_, _) => Future.successful(postcodeCheckResult)).identifiedItsaClient(mockAppConfig)(
          authorisedAgent)(
          ItsaClient("AB123456A", "BN114AW"))

      def vatClientIdentified(regDateCheckResult: VatKnownFactCheckResult): Transition =
        transitions.copy(checkRegDateMatches = (_, _) => Future.successful(regDateCheckResult)).identifiedVatClient(
          authorisedAgent)(
          VatClient("123456", "2010-10-10"))

      // format: on

      "transition to SelectClientType" in {

        given(IdentifyClient(Personal, HMRCMTDIT, emptyBasket)) when
          start should
          thenGo(SelectClientType(emptyBasket))
      }

      "transition to ConfirmClientItsa" in {

        given(IdentifyClient(Personal, HMRCMTDIT, emptyBasket)) when
          itsaClientIdentified(Some(true)) should
          matchPattern {
            case (
                ConfirmClientItsa(
                  AuthorisationRequest("Piglet", ItsaInvitation(Nino("AB123456A"), _, HMRCMTDIT, "ni"), AuthorisationRequest.NEW, _),
                  `emptyBasket`),
                _) =>
          }
      }

      "transition to KnownFactsNotMatched when the nino and postcode do not match" in {

        given(IdentifyClient(Personal, HMRCMTDIT, emptyBasket)) when
          itsaClientIdentified(Some(false)) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }

      "transition to ClientNotRegistered when service is HMRC-MTD-IT and the client has no SAUTR on CiD record" in {

        given(IdentifyClient(Personal, HMRCMTDIT, emptyBasket)) when
          itsaClientIdentified(None) should
          thenGo(ClientNotRegistered(emptyBasket))
      }

      "transition to ConfirmPostcodeCgt for personal cgt clients" in {

        given(IdentifyClient(Personal, HMRCCGTPD, emptyBasket)) when
          transitions.identifyCgtClient(getCgtSubscription())(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
          matchPattern {
            case (ConfirmPostcodeCgt(CgtRef("myCgtRef"), personal, `emptyBasket`, _, _), _) =>
          }
      }

      "transition to ConfirmPostcodeCgt for trust cgt clients" in {

        given(IdentifyClient(Trust, HMRCCGTPD, emptyBasket)) when
          transitions.identifyCgtClient(getCgtSubscription())(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
          matchPattern {
            case (ConfirmPostcodeCgt(CgtRef("myCgtRef"), business, `emptyBasket`, _, _), _) =>
          }
      }

      "transition to CgtRefNotFound" in {
        given(IdentifyClient(Personal, HMRCCGTPD, emptyBasket)) when
          transitions.identifyCgtClient(cgtRef => Future.successful(None))(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
          matchPattern {
            case (CgtRefNotFound(CgtRef("myCgtRef"), _), _) =>
          }
      }

      "transition to ConfirmClientPpt for personal PPT clients" in {
        given(IdentifyClient(Personal, HMRCPPTORG, emptyBasket)) when
          transitions.identifyPptClient(pptClient => Future.successful(true), pptRef => Future.successful(Some("PPT Client")))(authorisedAgent)(
            PptClient(PptRef(pptRef), pptRegDateStr)) should
          matchPattern {
            case (
                ConfirmClientPpt(AuthorisationRequest("PPT Client", PptInvitation(PptRef(pptRef), Some(Personal), _, _), _, _), `emptyBasket`),
                _) =>
          }
      }
      "transition to ConfirmClientPpt for business PPT clients" in {
        given(IdentifyClient(Business, HMRCPPTORG, emptyBasket)) when
          transitions.identifyPptClient(pptClient => Future.successful(true), pptRef => Future.successful(Some("PPT Client")))(authorisedAgent)(
            PptClient(PptRef(pptRef), pptRegDateStr)) should
          matchPattern {
            case (
                ConfirmClientPpt(AuthorisationRequest("PPT Client", PptInvitation(PptRef(pptRef), Some(Business), _, _), _, _), `emptyBasket`),
                _) =>
          }
      }
      "transition to ConfirmClientPpt for trust PPT clients" in {
        given(IdentifyClient(Trust, HMRCPPTORG, emptyBasket)) when
          transitions.identifyPptClient(pptClient => Future.successful(true), pptRef => Future.successful(Some("PPT Client")))(authorisedAgent)(
            PptClient(PptRef(pptRef), pptRegDateStr)) should
          matchPattern {
            case (ConfirmClientPpt(AuthorisationRequest("PPT Client", PptInvitation(PptRef(pptRef), Some(Trust), _, _), _, _), `emptyBasket`), _) =>
          }
      }
      "transition to PptRefNotFound when known fact check fails" in {
        given(IdentifyClient(Personal, HMRCPPTORG, emptyBasket)) when
          transitions.identifyPptClient(pptClient => Future.successful(false), pptRef => Future.successful(Some("PPT Client")))(authorisedAgent)(
            PptClient(PptRef(pptRef), pptRegDateStr)) should
          matchPattern {
            case (PptRefNotFound(PptRef(pptRef), _), _) =>
          }
      }

      "transition to PptRefNotFound when PPT client cannot be looked up" in {
        given(IdentifyClient(Personal, HMRCPPTORG, emptyBasket)) when
          transitions.identifyPptClient(pptClient => Future.successful(true), pptRef => Future.successful(None))(authorisedAgent)(
            PptClient(PptRef(pptRef), pptRegDateStr)) should
          matchPattern {
            case (PptRefNotFound(PptRef(pptRef), _), _) =>
          }
      }

      "transition to ConfirmClientPersonalVat" in {

        given(IdentifyClient(Personal, HMRCMTDVAT, emptyBasket)) when
          vatClientIdentified(VatKnownFactCheckOk) should
          matchPattern {
            case (
                ConfirmClientPersonalVat(
                  AuthorisationRequest("Piglet", VatInvitation(Some(_), Vrn("123456"), HMRCMTDVAT, "vrn"), AuthorisationRequest.NEW, _),
                  `emptyBasket`,
                  false),
                _) =>
          }
      }

      "transition to ConfirmClientPersonalVat when client insolvent" in {

        given(IdentifyClient(Personal, HMRCMTDVAT, emptyBasket)) when
          vatClientIdentified(VatRecordClientInsolvent) should
          matchPattern {
            case (
                ConfirmClientPersonalVat(
                  AuthorisationRequest("Piglet", VatInvitation(Some(_), Vrn("123456"), HMRCMTDVAT, "vrn"), AuthorisationRequest.NEW, _),
                  `emptyBasket`,
                  true),
                _) =>
          }
      }

      "transition to KnownFactNotMatched when the vrn and regDate don't match" in {

        given(IdentifyClient(Personal, HMRCMTDVAT, emptyBasket)) when
          vatClientIdentified(VatKnownFactNotMatched) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }

      "transition to CannotCreateRequest when a migration is in process" in {

        given(IdentifyClient(Personal, HMRCMTDVAT, emptyBasket)) when
          vatClientIdentified(VatRecordMigrationInProgress) should
          thenGo(CannotCreateRequest(emptyBasket))
      }

      "transition to ClientNotSignedUp when the client is not signed up for the service" in {

        given(IdentifyClient(Personal, HMRCMTDVAT, emptyBasket)) when
          vatClientIdentified(VatDetailsNotFound) should
          thenGo(ClientNotSignedUp(HMRCMTDVAT, emptyBasket))
      }

      "transition to KnownFactNotMatched when the nino and dob don't match" in {

        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(false))

        given(IdentifyClient(Personal, HMRCPIR, emptyBasket)) when
          transitions
            .copy(checkDobMatches = checkDobMatches)
            .identifiedIrvClient(mockAppConfig)(authorisedAgent)(IrvClient("AB123456A", "1990-10-10")) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }

      "transition to KnownFactNotMatched when client not found" in {

        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(None)

        given(IdentifyClient(Personal, HMRCPIR, emptyBasket)) when
          transitions
            .copy(checkDobMatches = checkDobMatches)
            .identifiedIrvClient(mockAppConfig)(authorisedAgent)(IrvClient("AB123456A", "1990-10-10")) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }
    }

    // *************************************************
    //               IdentifyTrustClient
    // *************************************************

    "at state IdentifyTrustClient" should {

      "transition to ConfirmPostcodeCgt when cgt client is identified for a personal" in {
        given(IdentifyClient(Trust, HMRCCGTPD, emptyBasket)) when
          transitions.identifyCgtClient(getCgtSubscription())(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
          matchPattern {
            case (ConfirmPostcodeCgt(CgtRef("myCgtRef"), personal, emptyBasket, _, _), _) =>
          }
      }

      "transition to ConfirmPostcodeCgt when cgt client is identified for a trust" in {
        given(IdentifyClient(Trust, HMRCCGTPD, emptyBasket)) when
          transitions.identifyCgtClient(getCgtSubscription())(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
          matchPattern {
            case (ConfirmPostcodeCgt(CgtRef("myCgtRef"), business, emptyBasket, _, _), _) =>
          }
      }

      "transition to CgtRefNotFound" in {
        given(IdentifyClient(Personal, HMRCCGTPD, emptyBasket)) when
          transitions.identifyCgtClient(cgtRef => Future.successful(None))(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
          matchPattern {
            case (CgtRefNotFound(CgtRef("myCgtRef"), _), _) =>
          }
      }
    }

    "at state ConfirmPostcodeCgt" should {

      "transition to ConfirmClientCgt when the postcode is matched for a UK personal client" in {
        given(ConfirmPostcodeCgt(CgtRef("cgtRef"), Personal, emptyBasket, Some("BN13 1FN"), "firstName lastName")) when
          transitions.confirmPostcodeCgt(getCgtSubscription())(authorisedAgent)(Postcode("BN13 1FN")) should
          matchPattern {
            case (ConfirmClientCgt(AuthorisationRequest("firstName lastName", _, _, _), _), _) =>
          }
      }

      "transition to ConfirmClientCgt when the postcode is matched for a UK business/trustee client" in {
        given(ConfirmPostcodeCgt(CgtRef("cgtRef"), Business, emptyBasket, Some("BN13 1FN"), "firstName lastName")) when
          transitions.confirmPostcodeCgt(getCgtSubscription())(authorisedAgent)(Postcode("BN13 1FN")) should
          matchPattern {
            case (ConfirmClientCgt(AuthorisationRequest("firstName lastName", _, _, _), _), _) =>
          }
      }

      "transition to KnownFactsNotMatched when the postcode is not matched for a UK client" in {
        given(ConfirmPostcodeCgt(CgtRef("cgtRef"), Personal, emptyBasket, Some("BN13 1FN"), "firstName lastName")) when
          transitions.confirmPostcodeCgt(getCgtSubscription())(authorisedAgent)(Postcode("BN13 1ZZ")) should
          matchPattern {
            case (KnownFactNotMatched(_), _) =>
          }
      }
    }

    "at state ConfirmCountryCodeCgt" should {

      "transition to ConfirmClientCgt when the countryCode is matched for a personal client" in {
        given(ConfirmCountryCodeCgt(CgtRef("cgtRef"), Personal, emptyBasket, "IN", "firstName lastName")) when
          transitions.confirmCountryCodeCgt(getCgtSubscription("IN"))(authorisedAgent)(CountryCode("IN")) should
          matchPattern {
            case (ConfirmClientCgt(AuthorisationRequest("firstName lastName", _, _, _), _), _) =>
          }
      }

      "transition to ConfirmClientTrustCgt when the countryCode is matched for a business client" in {
        given(ConfirmCountryCodeCgt(CgtRef("cgtRef"), Business, emptyBasket, "IN", "firstName lastName")) when
          transitions.confirmCountryCodeCgt(getCgtSubscription("IN"))(authorisedAgent)(CountryCode("IN")) should
          matchPattern {
            case (ConfirmClientCgt(AuthorisationRequest("firstName lastName", _, _, _), _), _) =>
          }
      }

      "transition to KnownFactsNotMatched when the countryCode is not matched for a non UK client" in {
        given(ConfirmCountryCodeCgt(CgtRef("cgtRef"), Personal, emptyBasket, "IN", "firstName lastName")) when
          transitions.confirmCountryCodeCgt(getCgtSubscription("IN"))(authorisedAgent)(CountryCode("FR")) should
          matchPattern {
            case (KnownFactNotMatched(_), _) =>
          }
      }
    }

    "at state IdentifyBusinessClient" should {

      "transition to SelectClientType" in {

        given(IdentifyClient(Business, HMRCMTDVAT, emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }

      "transition to ConfirmClientBusinessVat" in {

        given(IdentifyClient(Business, HMRCMTDVAT, emptyBasket)) when
          transitions
            .copy(checkRegDateMatches = (_, _) => Future(VatKnownFactCheckOk))
            .identifiedVatClient(authorisedAgent)(VatClient("123456", "2010-10-10")) should
          matchPattern {
            case (
                ConfirmClientBusinessVat(
                  AuthorisationRequest("Piglet", VatInvitation(Some(_), Vrn("123456"), HMRCMTDVAT, "vrn"), AuthorisationRequest.NEW, _),
                  _,
                  false),
                _) =>
          }
      }

      "transition to KnownFactNotMatched client" in {

        given(IdentifyClient(Business, HMRCMTDVAT, emptyBasket)) when
          transitions
            .copy(checkRegDateMatches = (_, _) => Future(VatKnownFactNotMatched))
            .identifiedVatClient(authorisedAgent)(VatClient("123456", "2010-10-10")) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }

      "transition to CannotCreateRequest" in {

        given(IdentifyClient(Business, HMRCMTDVAT, emptyBasket)) when
          transitions
            .copy(checkRegDateMatches = (_, _) => Future(VatRecordMigrationInProgress))
            .identifiedVatClient(authorisedAgent)(VatClient("123456", "2010-10-10")) should
          thenGo(CannotCreateRequest(emptyBasket))
      }

      "transition to ActiveRelationshipExists" in {

        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(VatKnownFactCheckOk)

        given(IdentifyClient(Business, HMRCMTDVAT, emptyBasket)) when
          transitions
            .copy(checkRegDateMatches = checkRegDateMatches, hasActiveRelationshipFor = hasActiveRelationship)
            .identifiedVatClient(authorisedAgent)(VatClient("123456", "2010-10-10")) should
          matchPattern {
            case (
                ConfirmClientBusinessVat(
                  AuthorisationRequest("Piglet", VatInvitation(Some(_), Vrn("123456"), HMRCMTDVAT, "vrn"), AuthorisationRequest.NEW, _),
                  _,
                  false),
                _) =>
          }
      }

      "transition to ClientNotSignedUp" in {

        given(IdentifyClient(Business, HMRCMTDVAT, emptyBasket)) when
          transitions
            .copy(checkRegDateMatches = (_, _) => Future(VatDetailsNotFound))
            .identifiedVatClient(authorisedAgent)(VatClient("123456", "2010-10-10")) should
          thenGo(ClientNotSignedUp(HMRCMTDVAT, emptyBasket))
      }
    }

    "at state ConfirmClientItsa" should {

      val authorisationRequest = AuthorisationRequest("Piglet", ItsaInvitation(Nino("AB123456A")))

      def hasPartialAuthorisation(arn: Arn, clientId: String): Future[Boolean] =
        Future successful (true)

      "transition to SelectClientType" in {

        given(ConfirmClientItsa(authorisationRequest, emptyBasket)) when
          start should
          thenGo(SelectClientType(emptyBasket))
      }

      "transition to ReviewAuthorisationsPersonal" in {

        given(ConfirmClientItsa(authorisationRequest, emptyBasket)) when
          transitions.clientConfirmed(showCgtFlag = false)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenMatch {
            case ReviewAuthorisations(Personal, _, basket) if basket.nonEmpty =>
          }
      }

      "transition to SelectPersonalService" in {

        given(ConfirmClientItsa(authorisationRequest, emptyBasket)) when
          transitions.clientConfirmed(showCgtFlag = false)(mockAppConfig)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyClient(Personal, HMRCMTDIT, emptyBasket))
      }

      "transition to PendingInvitationExists when the invitation is already in the basket" in {

        given(
          ConfirmClientItsa(
            authorisationRequest,
            Set(
              AuthorisationRequest(
                "client name",
                Invitation(Some(Personal), HMRCMTDIT, "AB123456A", "BN114AW")
              )
            )
          )
        ) when
          transitions.clientConfirmed(showCgtFlag = false)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenMatch {
            case PendingInvitationExists(_, "Piglet", "invitation/link", basket) if basket.nonEmpty =>
          }
      }

      "transition to PartialAuthorisationExists when there is a Partial Authorisation" in {

        given(ConfirmClientItsa(authorisationRequest, emptyBasket)) when
          transitions
            .copy(hasPartialAuthorisationFor = hasPartialAuthorisation)
            .clientConfirmed(showCgtFlag = false)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenMatch {
            case PartialAuthorisationExists(_) =>
          }
      }

      "transition to AlreadyCopiedAcrossItsa when there is a Legacy Mapping" in {
        when(mockAppConfig.featuresAltItsa).thenReturn(true)
        given(ConfirmClientItsa(authorisationRequest, emptyBasket)) when
          transitions
            .copy(legacySaRelationshipStatusFor = legacySaRelationshipStatusMapped)
            .clientConfirmed(showCgtFlag = false)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenMatch {
            case AlreadyCopiedAcrossItsa =>
          }
      }

      "transition to LegacyAuthorisationDetected when there is an other Legacy Mapping" in {
        when(mockAppConfig.featuresAltItsa).thenReturn(true)
        given(ConfirmClientItsa(authorisationRequest, emptyBasket)) when
          transitions
            .copy(legacySaRelationshipStatusFor = legacySaRelationshipStatusNotMapped)
            .clientConfirmed(showCgtFlag = false)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenMatch {
            case LegacyAuthorisationDetected(_) =>
          }
      }
    }

    "at state ConfirmClientPersonalVat" should {

      def legacySaRelationshipStatusNotFound(arn: Arn, clientId: String): Future[LegacySaRelationshipResult] =
        Future.successful(LegacySaRelationshipNotFound)

      "transition to Start" in {

        given(
          ConfirmClientPersonalVat(
            AuthorisationRequest(
              "Piglet",
              VatInvitation(
                Some(Personal),
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
                Some(Personal),
                Vrn("123456")
              )
            ),
            emptyBasket
          )
        ) when
          transitions.clientConfirmed(showCgtFlag = false)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenMatch {
            case ReviewAuthorisations(Personal, _, basket) if basket.nonEmpty =>
          }
      }

      "transition to ClientInsolvent" in {

        given(
          ConfirmClientPersonalVat(
            AuthorisationRequest(
              "Piglet",
              VatInvitation(
                Some(Personal),
                Vrn("123456")
              )
            ),
            emptyBasket,
            clientInsolvent = true
          )
        ) when
          transitions.clientConfirmed(showCgtFlag = false)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenMatch {
            case ClientInsolvent(_) =>
          }
      }

      "transition to PersonalServiceSelected" in {

        given(
          ConfirmClientPersonalVat(
            AuthorisationRequest(
              "Piglet",
              VatInvitation(
                Some(Personal),
                Vrn("123456")
              )
            ),
            emptyBasket
          )
        ) when
          transitions.clientConfirmed(showCgtFlag = false)(mockAppConfig)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyClient(Personal, HMRCMTDVAT, emptyBasket))
      }
    }

    "at state ConfirmClientBusinessVat" should {

      val authorisationRequest =
        AuthorisationRequest(
          "Piglet",
          VatInvitation(Some(Business), Vrn("123456"))
        )

      "after start transition to Start" in {

        given(ConfirmClientBusinessVat(authorisationRequest, emptyBasket)) when start should thenGo(
          SelectClientType(emptyBasket)
        )
      }

      "after clientConfirmed(true) transition to ReviewAuthorisations" in {

        given(ConfirmClientBusinessVat(authorisationRequest, emptyBasket)) when
          transitions.clientConfirmed(showCgtFlag = false)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ReviewAuthorisations(Business, availableBusinessServices, Set(authorisationRequest)))
      }

      "after clientConfirmed(false) transition to IdentifyBusinessClient" in {

        given(ConfirmClientBusinessVat(authorisationRequest, emptyBasket)) when
          transitions.clientConfirmed(showCgtFlag = false)(mockAppConfig)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyClient(Business, HMRCMTDVAT, emptyBasket))
      }

      "transition to PendingInvitationExists when there is already a pending invitation" in {

        def hasPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(true)

        given(ConfirmClientBusinessVat(authorisationRequest, emptyBasket)) when
          transitions
            .copy(hasPendingInvitationsFor = hasPendingInvitation)
            .clientConfirmed(showCgtFlag = false)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(PendingInvitationExists(Business, "Piglet", "invitation/link", emptyBasket))
      }

      "transition to ActiveAuthorisationExists when an active relationship already exists" in {

        given(ConfirmClientBusinessVat(authorisationRequest, emptyBasket)) when transitions
          .copy(hasActiveRelationshipFor = hasActiveRelationship)
          .clientConfirmed(
            showCgtFlag = false
          )(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ActiveAuthorisationExists(Business, HMRCMTDVAT, emptyBasket))
      }
    }

    "at state ConfirmClientPersonalCgt" should {

      "transition to IdentifyPersonalClient" in {

        val authorisationRequest =
          AuthorisationRequest("Roo", CgtInvitation(CgtRef("myCgtRef"), Some(Personal)))

        given(ConfirmClientCgt(authorisationRequest, emptyBasket)) when
          transitions.clientConfirmed(showCgtFlag = true)(mockAppConfig)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyClient(Personal, HMRCCGTPD, emptyBasket))
      }

    }

    "at state ConfirmClientTrustCgt" should {

      "transition to IdentifyTrustClient" in {

        val authorisationRequest =
          AuthorisationRequest("Roo", CgtInvitation(CgtRef("myCgtRef"), Some(Trust)))

        def legacySaRelationshipStatusNotFound(arn: Arn, clientId: String): Future[LegacySaRelationshipResult] =
          Future.successful(LegacySaRelationshipNotFound)

        given(ConfirmClientCgt(authorisationRequest, emptyBasket)) when
          transitions.clientConfirmed(showCgtFlag = true)(mockAppConfig)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyClient(Trust, HMRCCGTPD, emptyBasket))
      }

    }

    "at state ConfirmClientTrust" should {

      val utr = Utr("4937455253")

      val authorisationRequest = AuthorisationRequest("Piglet", TrustInvitation(utr))

      "transition to IdentifyTrustClient if NO is selected" in {

        given(ConfirmClientTrust(authorisationRequest, emptyBasket)) when
          transitions.clientConfirmed(showCgtFlag = false)(mockAppConfig)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyClient(Trust, TAXABLETRUST, emptyBasket))
      }

      "transition to InvitationSentBusiness with taxable trust" in {

        given(ConfirmClientTrust(authorisationRequest, emptyBasket)) when
          transitions.clientConfirmed(showCgtFlag = false)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(
            InvitationSentTrust(
              "invitation/link",
              None,
              "abc@xyz.com",
              Set("HMRC-TERS-ORG")
            ))
      }

      "transition to PendingInvitationExists when a pending invitation exists for the service" in {

        given(ConfirmClientTrust(authorisationRequest, emptyBasket)) when
          transitions
            .copy(hasPendingInvitationsFor = hasPendingInvitation)
            .clientConfirmed(showCgtFlag = false)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(PendingInvitationExists(Trust, "Piglet", "invitation/link", emptyBasket))
      }

      "transition to ActiveAuthorisationExists when a pending invitation exists for the service" in {

        def hasActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(true)

        given(ConfirmClientTrust(authorisationRequest, emptyBasket)) when
          transitions
            .copy(hasActiveRelationshipFor = hasActiveRelationship)
            .clientConfirmed(showCgtFlag = false)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ActiveAuthorisationExists(Trust, Services.TAXABLETRUST, emptyBasket))
      }
    }

    "at state ReviewAuthorisationsTrust" should {

      "after start transition to Start" in {

        given(ReviewAuthorisations(Trust, availableTrustServices, emptyBasket)) when
          start should
          thenGo(SelectClientType(emptyBasket))
      }

      "after authorisationsReviewed(true) transition to SelectTrustService" in {

        given(ReviewAuthorisations(Trust, availableTrustServices, emptyBasket)) when
          transitions.authorisationsReviewed(authorisedAgent)(Confirmation(true)) should
          thenGo(SelectService(Trust, availableTrustServices, emptyBasket))
      }

      "after authorisationsReviewed(false) transition to InvitationSentTrust" in {

        val authorisationRequestNew =
          AuthorisationRequest("Mr Client", Invitation(Some(Trust), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        val authorisationRequestCreated = authorisationRequestNew.copy(state = AuthorisationRequest.CREATED)

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]) =
          Future(Set(authorisationRequestCreated))

        given(
          ReviewAuthorisations(Trust, availableTrustServices, Set(authorisationRequestNew))
        ) when
          transitions.copy(createMultipleInvitations = createMultipleInvitations).authorisationsReviewed(authorisedAgent)(Confirmation(false)) should
          thenGo(InvitationSentTrust("invitation/link", None, "abc@xyz.com", Set(HMRCMTDIT)))
      }

      "after authorisationsReviewed(false) when all fail transition to AuthorisationsReviewedAllFailed" in {

        val authorisationRequestNew =
          AuthorisationRequest("Mr Client", Invitation(Some(Trust), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        val authorisationRequestFailed = authorisationRequestNew.copy(state = AuthorisationRequest.FAILED)

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]) =
          Future(Set(authorisationRequestFailed))

        given(
          ReviewAuthorisations(Trust, availableTrustServices, Set(authorisationRequestNew))
        ) when
          transitions.copy(createMultipleInvitations = createMultipleInvitations).authorisationsReviewed(authorisedAgent)(Confirmation(false)) should
          thenGo(AllAuthorisationsFailed(Set(authorisationRequestFailed)))
      }

      "after authorisationsReviewed(false) when some fail transition to AuthorisationReviewedSomeFailed" in {

        val authorisationRequestNew1 =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), HMRCMTDIT, "AB123456B", "BN114AT"), AuthorisationRequest.NEW, "ABC123")

        val authorisationRequestNew2 =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), HMRCMTDIT, "AB123456B", "BN114AT"), AuthorisationRequest.NEW, "ABC124")

        val authorisationRequestSuccess1 =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.CREATED, "ABC123")

        val authorisationRequestFailed2 =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), HMRCMTDIT, "AB123456B", "BN114AT"), AuthorisationRequest.FAILED, "ABC124")

        given(
          ReviewAuthorisations(Trust, availableServices, Set(authorisationRequestNew1, authorisationRequestNew2))
        ) when
          transitions
            .copy(createMultipleInvitations = (_, _) => Future(Set(authorisationRequestSuccess1, authorisationRequestFailed2)))
            .authorisationsReviewed(authorisedAgent)(Confirmation(false)) should
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
          AuthorisationRequest("Mr Client", Invitation(Some(Trust), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        given(
          ReviewAuthorisations(Trust, availableTrustServices, Set(authorisationRequest))
        ) when
          transitions.deleteAuthorisationRequest("ABC123")(authorisedAgent) should
          thenGo(DeleteAuthorisationRequest(Trust, authorisationRequest, Set(authorisationRequest)))
      }

      "throw an Exception when there is no corresponding itemId in the basket" in {

        val authorisationRequest =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), TAXABLETRUST, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        intercept[Exception] {
          given(
            ReviewAuthorisations(Trust, availableTrustServices, Set(authorisationRequest))
          ) when transitions.deleteAuthorisationRequest("XXX")(authorisedAgent)
        }.getMessage shouldBe "No Item to delete"
      }

    }

    "at state ReviewAuthorisationsPersonal" should {

      "after start transition to Start" in {

        given(ReviewAuthorisations(Personal, availableServices, emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }

      "after authorisationsReviewed(true) transition to SelectPersonalService" in {

        given(ReviewAuthorisations(Personal, availableServices, emptyBasket)) when
          transitions.authorisationsReviewed(authorisedAgent)(Confirmation(true)) should
          thenGo(SelectService(Personal, availableServices, emptyBasket)) //FIXME check basket has invitation added
      }

      "after authorisationsReviewed(false) transition to InvitationSentPersonal" in {

        val authorisationRequestNew =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        val authorisationRequestCreated = authorisationRequestNew.copy(state = AuthorisationRequest.CREATED)

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]) =
          Future(Set(authorisationRequestCreated))

        def createInvitationSent: CreateInvitationSent =
          (_: String, _: String, _: Arn, _: Basket) =>
            Future.successful(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Set(HMRCMTDIT), isAltItsa = false))

        given(
          ReviewAuthorisations(Personal, availableServices, Set(authorisationRequestNew))
        ) when
          transitions
            .copy(createMultipleInvitations = createMultipleInvitations, createInvitationSent = createInvitationSent)
            .authorisationsReviewed(authorisedAgent)(Confirmation(false)) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Set(HMRCMTDIT), isAltItsa = false))
      }

      "after authorisationsReviewed(false) transition to InvitationSentPersonal (alt itsa)" in {

        val authorisationRequestNew =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        val authorisationRequestCreated = authorisationRequestNew.copy(state = AuthorisationRequest.CREATED)

        def createInvitationSent: CreateInvitationSent =
          (_: String, _: String, _: Arn, _: Basket) =>
            Future.successful(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Set(HMRCMTDIT), isAltItsa = true))

        given(
          ReviewAuthorisations(Personal, availableServices, Set(authorisationRequestNew))
        ) when
          transitions
            .copy(createMultipleInvitations = (_, _) => Future(Set(authorisationRequestCreated)), createInvitationSent = createInvitationSent)
            .authorisationsReviewed(authorisedAgent)(Confirmation(false)) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Set(HMRCMTDIT), isAltItsa = true))
      }

      "after authorisationsReviewed(false) when all fail transition to AuthorisationsReviewedAllFailed" in {

        val authorisationRequestNew =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        val authorisationRequestFailed = authorisationRequestNew.copy(state = AuthorisationRequest.FAILED)

        def createInvitationSent: CreateInvitationSent =
          (_: String, _: String, _: Arn, _: Basket) =>
            Future.successful(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Set(HMRCMTDIT), isAltItsa = false))

        given(
          ReviewAuthorisations(Personal, availableServices, Set(authorisationRequestNew))
        ) when
          transitions
            .copy(createMultipleInvitations = (_, _) => Future(Set(authorisationRequestFailed)), createInvitationSent = createInvitationSent)
            .authorisationsReviewed(authorisedAgent)(Confirmation(false)) should
          thenGo(AllAuthorisationsFailed(Set(authorisationRequestFailed)))
      }

      "after authorisationsReviewed(false) when some fail transition to AuthorisationReviewedSomeFailed" in {

        val authorisationRequestNew1 =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), HMRCMTDIT, "AB123456B", "BN114AT"), AuthorisationRequest.NEW, "ABC123")
        val authorisationRequestNew2 = authorisationRequestNew1.copy(itemId = "ABC124")
        val authorisationRequestSuccess1 = authorisationRequestNew1.copy(state = AuthorisationRequest.CREATED)
        val authorisationRequestFailed2 = authorisationRequestNew2.copy(state = AuthorisationRequest.FAILED)

        def createInvitationSent: CreateInvitationSent =
          (_: String, _: String, _: Arn, _: Basket) =>
            Future.successful(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Set(HMRCMTDIT), isAltItsa = false))

        given(
          ReviewAuthorisations(Personal, availableServices, Set(authorisationRequestNew1, authorisationRequestNew2))
        ) when
          transitions
            .copy(
              createMultipleInvitations = (_, _) => Future(Set(authorisationRequestSuccess1, authorisationRequestFailed2)),
              createInvitationSent = createInvitationSent)
            .authorisationsReviewed(authorisedAgent)(Confirmation(false)) should
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
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        given(
          ReviewAuthorisations(Personal, availableServices, Set(authorisationRequest))
        ) when
          transitions.deleteAuthorisationRequest("ABC123")(authorisedAgent) should
          thenGo(DeleteAuthorisationRequest(Personal, authorisationRequest, Set(authorisationRequest)))
      }

      "throw an Exception when there is no corresponding itemId in the basket" in {

        val authorisationRequest =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        intercept[Exception] {
          given(
            ReviewAuthorisations(Personal, availableServices, Set(authorisationRequest))
          ) when transitions.deleteAuthorisationRequest("XXX")(authorisedAgent)
        }.getMessage shouldBe "No Item to delete"
      }
    }

    "at state DeleteAuthorisationRequestPersonal" should {

      "after start transition to Start" in {

        val authorisationRequest = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(Personal), HMRCMTDIT, "AB123456A", "BN114AW"),
          AuthorisationRequest.NEW,
          "ABC123"
        )

        given(
          DeleteAuthorisationRequest(Personal, authorisationRequest, emptyBasket)
        ) when
          start should
          thenGo(SelectClientType(emptyBasket))
      }

      "after confirmDeleteAuthorisationRequest(true) should transition to ReviewAuthorisationPersonal with one request removed form the basket" in {

        val authorisationRequest1 =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        val authorisationRequest2 =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), HMRCMTDIT, "AB123456B", "BN114AT"), AuthorisationRequest.NEW, "ABC124")

        given(
          DeleteAuthorisationRequest(Personal, authorisationRequest1, Set(authorisationRequest1, authorisationRequest2))
        ) when
          transitions.confirmDeleteAuthorisationRequest(authorisedAgent)(Confirmation(true)) should
          thenGo(ReviewAuthorisations(Personal, availableServices, Set(authorisationRequest2)))
      }

      "after confirmDeleteAuthorisationRequest(true) should transition to AllAuthorisationsRemoved when there is nothing left in the basket" in {

        val authorisationRequest =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        given(
          DeleteAuthorisationRequest(Personal, authorisationRequest, Set(authorisationRequest))
        ) when
          transitions.confirmDeleteAuthorisationRequest(authorisedAgent)(Confirmation(true)) should
          thenGo(AllAuthorisationsRemoved)
      }

      "after confirmDeleteAuthorisationRequest(false) should transition to ReviewAuthorisationPersonal with basket in tact" in {

        val authorisationRequest =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), HMRCMTDIT, "AB123456A", "BN114AW"), AuthorisationRequest.NEW, "ABC123")

        given(
          DeleteAuthorisationRequest(Personal, authorisationRequest, Set(authorisationRequest))
        ) when
          transitions.confirmDeleteAuthorisationRequest(authorisedAgent)(Confirmation(false)) should
          thenGo(ReviewAuthorisations(Personal, availableServices, Set(authorisationRequest)))
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
        ) when transitions.continueSomeResponsesFailed(authorisedAgent) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Set.empty, isAltItsa = false))
      }
    }
  }
}
