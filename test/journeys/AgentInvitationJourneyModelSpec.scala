/*
 * Copyright 2023 HM Revenue & Customs
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

import java.time.LocalDate
import org.mockito.Mockito.{mock, when}
import play.api.test.Helpers._
import support.{TestFeatureFlags, UnitSpec}
import support.TestFeatureFlags.{allDisabled, allEnabled}
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.TransitionEffects._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel._
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType._
import uk.gov.hmrc.agentinvitationsfrontend.models.VatKnownFactCheckResult._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, CgtRef, PptRef, Service, SuspensionDetails, Urn, Utr, Vrn}
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
  val authorisedAgent = AuthorisedAgent(Arn("TARN0000001"), isAllowlisted = true)
  val authorisedAgentNotAllowlisted = AuthorisedAgent(Arn("TARN0000001"), isAllowlisted = false)
  private val availableServices: Set[Service] = Set(Service.PersonalIncomeRecord, Service.MtdIt, Service.Vat, Service.CapitalGains, Service.Ppt)
  private val availableBusinessServices: Set[Service] = Set(Service.Vat, Service.Ppt)
  private val availableTrustServices: Set[Service] = Set(Service.Trust, Service.CapitalGains, Service.Ppt)
  private val nonAllowlistedServices: Set[Service] = Set(Service.MtdIt, Service.Vat, Service.CapitalGains, Service.Ppt)
  private val mockAppConfig = mock(classOf[AppConfig])

  def makeBasket(services: Set[Service]) = services.map {
    case Service.CapitalGains =>
      AuthorisationRequest("client", Invitation(Some(ClientType.Business), Service.CapitalGains, CgtRef("X")), AuthorisationRequest.NEW, "item-cgt")
    case Service.Vat =>
      AuthorisationRequest("client", Invitation(Some(ClientType.Personal), Service.Vat, Vrn(vrn)), AuthorisationRequest.NEW, "item-vat")
    case Service.MtdIt =>
      AuthorisationRequest("client", Invitation(Some(ClientType.Personal), Service.MtdIt, Nino(nino)), AuthorisationRequest.NEW, "item-itsa")
    case Service.Ppt =>
      AuthorisationRequest("client", Invitation(Some(ClientType.Personal), Service.Ppt, PptRef(pptRef)), AuthorisationRequest.NEW, "item-ppt")
  }

  val nino = "AB123456A"
  val postCode = Some("BN114AW")
  val vrn = "123456"
  val vatRegDate = Some("2010-10-10")
  val dob = Some("1990-10-10")
  val pptRef = "XAPPT000012345"
  val pptRegDate = LocalDate.parse("2021-01-01")
  val pptRegDateStr = pptRegDate.toString
  val utr = Utr("1977030537")
  val urn = Urn("XXTRUST10010010")

  val tpd = TypeOfPersonDetails("Individual", Left(IndividualName("firstName", "lastName")))

  def cgtAddressDetails(countryCode: String = "GB") =
    CgtAddressDetails("line1", Some("line2"), Some("line2"), Some("line2"), countryCode, Some("BN13 1FN"))

  def cgtSubscription(countryCode: String = "GB") =
    CgtSubscription("CGT", SubscriptionDetails(tpd, cgtAddressDetails(countryCode)))

  def getClientName: GetClientName = (_: String, _: Service) => Future(Some("Piglet"))

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

  def hasNoPendingInvitation(arn: Arn, clientId: String, service: Service): Future[Boolean] =
    Future.successful(false)

  def hasPendingInvitation(arn: Arn, clientId: String, service: Service): Future[Boolean] =
    Future.successful(true)

  def hasNoActiveRelationship(arn: Arn, clientId: String, service: Service): Future[Boolean] =
    Future.successful(false)

  def hasActiveRelationship(arn: Arn, clientId: String, service: Service): Future[Boolean] =
    Future.successful(true)

  def hasNoPartialAuthorisation(arn: Arn, clientId: String): Future[Boolean] =
    Future successful (false)

  def pptKnownFactCheckPasses: CheckPptKnownFact = (_, _) => Future.successful(true)
  def pptKnownFactCheckFails: CheckPptKnownFact = (_, _) => Future.successful(false)

  // This is the default behaviour. Modify as needed in individual tests
  val transitions = Transitions(
    mockAppConfig,
    allEnabled,
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
    createInvitationSent = createInvitationSentMock,
    getCgtSubscription = _ => Future(None),
    getTrustName = _ => Future(TrustResponse(Right(TrustName("a trust")))),
    getPptCustomerName = _ => Future.successful(Some("PPT Client")),
    checkPptKnownFact = pptKnownFactCheckFails
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

      "transition to SelectPersonalService with all services when agent is allowlisted" in {

        given(SelectClientType(emptyBasket)) when
          transitions.selectedClientType(authorisedAgent)("personal") should
          thenMatch {
            case SelectService(ClientType.Personal, services, _) if services.contains(Service.PersonalIncomeRecord) =>
          }
      }

      "transition to SelectPersonalService with fewer services when agent is not allowlisted" in {

        given(SelectClientType(emptyBasket)) when
          transitions.selectedClientType(authorisedAgentNotAllowlisted)("personal") should
          thenMatch {
            case SelectService(ClientType.Personal, services, _) if !services.contains(Service.PersonalIncomeRecord) =>
          }
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
        agentSuspensionEnabled: Boolean = true): Option[Service] => AgentInvitationJourneyModel.Transition = {
        val featureFlags = TestFeatureFlags.allDisabled.copy(
          showHmrcMtdIt = showItsaFlag,
          showPersonalIncome = showPirFlag,
          showHmrcMtdVat = showVatFlag,
          showHmrcCgt = showCgtFlag,
          showPlasticPackagingTax = showPptFlag,
          agentSuspensionEnabled = agentSuspensionEnabled
        )
        transitions.copy(featureFlags = featureFlags).selectedService(authorisedAgent)
      }

      "transition to SelectClientType" in {

        given(SelectService(Personal, availableServices, emptyBasket)) when
          start should
          thenGo(SelectClientType(emptyBasket))
      }

      "transition to IdentifyPersonalClient for ITSA service" in {

        given(SelectService(Personal, availableServices, emptyBasket)) when
          selectedService()(Some(Service.MtdIt)) should
          thenGo(IdentifyClient(Personal, Service.MtdIt, emptyBasket))
      }

      "transition to IdentifyPersonalClient for PIR service" in {

        given(SelectService(Personal, availableServices, emptyBasket)) when
          selectedService()(Some(Service.PersonalIncomeRecord)) should
          thenGo(IdentifyClient(Personal, Service.PersonalIncomeRecord, emptyBasket))
      }

      "transition to IdentifyPersonalClient for VAT service" in {

        given(SelectService(Personal, availableServices, emptyBasket)) when
          selectedService()(Some(Service.Vat)) should
          thenGo(IdentifyClient(Personal, Service.Vat, emptyBasket))
      }

      "transition to IdentifyPersonalClient for CGT service" in {

        given(SelectService(Personal, availableServices, emptyBasket)) when
          selectedService()(Some(Service.CapitalGains)) should
          thenGo(IdentifyClient(Personal, Service.CapitalGains, emptyBasket))
      }

      "transition to IdentifyPersonalClient for PPT service" in {

        given(SelectService(Personal, availableServices, emptyBasket)) when
          selectedService()(Some(Service.Ppt)) should
          thenGo(IdentifyClient(Personal, Service.Ppt, emptyBasket))
      }

      "transition to ReviewPersonalService when last service selected and user does not confirm" in {

        given(SelectService(Personal, Set(Service.PersonalIncomeRecord), makeBasket(Set(Service.MtdIt, Service.Vat, Service.CapitalGains)))) when
          selectedService()(None) should
          thenGo(ReviewAuthorisations(Personal, Set(Service.PersonalIncomeRecord), makeBasket(Set(Service.MtdIt, Service.Vat, Service.CapitalGains))))
      }

      "transition to AgentSuspended when agent is suspended for the selected service" in {
        def suspendedForItsa() = Future.successful(SuspensionDetails(suspensionStatus = true, Some(Set("ITSA"))))

        given(SelectService(Personal, availableServices, emptyBasket)) when
          transitions
            .copy(getSuspensionDetails = suspendedForItsa)
            .selectedService(authorisedAgent)(Some(Service.MtdIt)) should thenGo(AgentSuspended(Service.MtdIt, emptyBasket))
      }

      "transition to IdentifyPersonalClient when agent is suspended for a service not selected" in {
        def suspendedForItsa() = Future.successful(SuspensionDetails(suspensionStatus = true, Some(Set("ITSA"))))

        given(SelectService(Personal, availableServices, emptyBasket)) when
          transitions
            .copy(getSuspensionDetails = suspendedForItsa)
            .selectedService(authorisedAgent)(Some(Service.Vat)) should thenGo(IdentifyClient(Personal, Service.Vat, emptyBasket))
      }

      "throw an exception when the show itsa feature flag is off" in {

        intercept[Exception] {
          given(SelectService(Personal, availableServices, emptyBasket)) when
            selectedService(showItsaFlag = false)(Some(Service.MtdIt))
        }.getMessage shouldBe "Service: HMRC-MTD-IT feature flag is switched off"
      }

      "throw an exception when the show pir feature flag is off" in {

        intercept[Exception] {
          given(SelectService(Personal, availableServices, emptyBasket)) when
            selectedService(showPirFlag = false)(Some(Service.PersonalIncomeRecord))
        }.getMessage shouldBe "Service: PERSONAL-INCOME-RECORD feature flag is switched off"
      }

      "throw an exception when the show vat feature flag is off" in {

        intercept[Exception] {
          given(SelectService(Personal, availableServices, emptyBasket)) when
            selectedService(showVatFlag = false)(Some(Service.Vat))
        }.getMessage shouldBe "Service: HMRC-MTD-VAT feature flag is switched off"
      }

      "throw an exception when the show cgt feature flag is off" in {

        intercept[Exception] {
          given(SelectService(Personal, availableServices, emptyBasket)) when
            selectedService(showCgtFlag = false)(Some(Service.CapitalGains))
        }.getMessage shouldBe "Service: HMRC-CGT-PD feature flag is switched off"
      }

      "throw an exception when the show ppt feature flag is off" in {

        intercept[Exception] {
          given(SelectService(Personal, availableServices, emptyBasket)) when
            selectedService(showPptFlag = false)(Some(Service.Ppt))
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

      "after selectedService(true)(true) transition to IdentifyBusinessClient" in {

        given(SelectService(Business, availableBusinessServices, emptyBasket)) when
          transitions.selectedService(authorisedAgent)(Some(Service.Vat)) should
          thenGo(IdentifyClient(Business, Service.Vat, emptyBasket))
      }

      "transition to AgentSuspended if agent is suspended for the chosen service" in {
        def suspendedForVat() = Future.successful(SuspensionDetails(suspensionStatus = true, Some(Set("VATC"))))

        given(SelectService(Business, availableBusinessServices, emptyBasket)) when
          transitions
            .copy(getSuspensionDetails = suspendedForVat)
            .selectedService(authorisedAgent)(Some(Service.Vat)) should
          thenGo(AgentSuspended(Service.Vat, emptyBasket))
      }

      "transition to IdentifyBusinessClient if agent is suspended for a different service" in {
        def suspendedForItsa() = Future.successful(SuspensionDetails(suspensionStatus = true, Some(Set("ITSA"))))

        given(SelectService(Business, availableServices, emptyBasket)) when
          transitions
            .copy(getSuspensionDetails = suspendedForItsa)
            .selectedService(authorisedAgent)(Some(Service.Vat)) should
          thenGo(IdentifyClient(Business, Service.Vat, emptyBasket))
      }

      "throw an exception when the show vat feature flag is off" in {
        intercept[Exception] {
          given(SelectService(Business, availableServices, emptyBasket)) when
            transitions
              .copy(featureFlags = allEnabled.copy(showHmrcMtdVat = false))
              .selectedService(authorisedAgent)(Some(Service.Vat))
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

      "after selectedService(false)(true)(true) transition to IdentifyTrustClient" in {

        given(SelectService(Trust, availableTrustServices, emptyBasket)) when
          transitions.selectedService(agent = authorisedAgent)(Some(Service.Trust)) should
          thenGo(IdentifyClient(Trust, Service.Trust, emptyBasket))
      }

      "after selectedService(false)(true)(false) transition to SelectClientType" in {

        given(SelectService(Trust, availableTrustServices, emptyBasket)) when
          transitions.selectedService(agent = authorisedAgent)(None) should
          thenGo(SelectClientType(emptyBasket))
      }

      "after selectedService(true)(true)(false) transition to SelectClientType" in {

        given(SelectService(Trust, availableTrustServices, emptyBasket)) when
          transitions.selectedService(agent = authorisedAgent)(None) should
          thenGo(SelectClientType(emptyBasket))
      }

      "after selectedService(true)(true)(false) with non-empty basket transition to ReviewAuthorisationsTrust" in {
        val basket = makeBasket(Set(Service.CapitalGains))
        given(SelectService(Trust, availableTrustServices, basket)) when
          transitions.selectedService(agent = authorisedAgent)(None) should
          thenGo(ReviewAuthorisations(Trust, availableTrustServices, basket))
      }

      "transition to AgentSuspended if the agent is suspended for the selected service" in {
        def suspendedForTrust() = Future.successful(SuspensionDetails(suspensionStatus = true, Some(Set("TRS"))))

        given(SelectService(Trust, availableTrustServices, emptyBasket)) when
          transitions
            .copy(getSuspensionDetails = suspendedForTrust)
            .selectedService(agent = authorisedAgent)(Some(Service.Trust)) should
          thenGo(AgentSuspended(Service.Trust, emptyBasket))
      }
    }

    // *************************************************
    //               IdentifyPersonalClient
    // *************************************************

    "at state IdentifyPersonalClient" should {

      when(mockAppConfig.featuresAltItsa).thenReturn(true)

      // format: off
      def itsaClientIdentified(postcodeCheckResult: Option[Boolean]): Transition =
        transitions.copy(checkPostcodeMatches = (_, _) => Future.successful(postcodeCheckResult)).identifiedItsaClient(
          authorisedAgent)(
          ItsaClient("AB123456A", "BN114AW"))

      def vatClientIdentified(regDateCheckResult: VatKnownFactCheckResult): Transition =
        transitions.copy(checkRegDateMatches = (_, _) => Future.successful(regDateCheckResult)).identifiedVatClient(
          authorisedAgent)(
          VatClient("123456", "2010-10-10"))

      // format: on

      "transition to SelectClientType" in {

        given(IdentifyClient(Personal, Service.MtdIt, emptyBasket)) when
          start should
          thenGo(SelectClientType(emptyBasket))
      }

      "transition to ConfirmClientItsa" in {

        given(IdentifyClient(Personal, Service.MtdIt, emptyBasket)) when
          itsaClientIdentified(Some(true)) should
          matchPattern {
            case (
                ConfirmClient(
                  AuthorisationRequest(
                    "Piglet",
                    Invitation(Some(ClientType.Personal), Service.MtdIt, Nino("AB123456A")),
                    AuthorisationRequest.NEW,
                    _),
                  `emptyBasket`,
                  _),
                _) =>
          }
      }

      "transition to KnownFactsNotMatched when the nino and postcode do not match" in {

        given(IdentifyClient(Personal, Service.MtdIt, emptyBasket)) when
          itsaClientIdentified(Some(false)) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }

      "transition to ClientNotRegistered when service is HMRC-MTD-IT and the client has no SAUTR on CiD record" in {

        given(IdentifyClient(Personal, Service.MtdIt, emptyBasket)) when
          itsaClientIdentified(None) should
          thenGo(ClientNotRegistered(emptyBasket))
      }

      "transition to ConfirmPostcodeCgt for personal cgt clients" in {

        given(IdentifyClient(Personal, Service.CapitalGains, emptyBasket)) when
          transitions.copy(getCgtSubscription = getCgtSubscription()).identifyCgtClient(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
          matchPattern {
            case (ConfirmPostcodeCgt(CgtRef("myCgtRef"), personal, `emptyBasket`, _, _), _) =>
          }
      }

      "transition to ConfirmPostcodeCgt for trust cgt clients" in {

        given(IdentifyClient(Trust, Service.CapitalGains, emptyBasket)) when
          transitions.copy(getCgtSubscription = getCgtSubscription()).identifyCgtClient(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
          matchPattern {
            case (ConfirmPostcodeCgt(CgtRef("myCgtRef"), business, `emptyBasket`, _, _), _) =>
          }
      }

      "transition to CgtRefNotFound" in {
        given(IdentifyClient(Personal, Service.CapitalGains, emptyBasket)) when
          transitions.identifyCgtClient(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
          matchPattern {
            case (CgtRefNotFound(CgtRef("myCgtRef"), _), _) =>
          }
      }

      "transition to ConfirmClientPpt for personal PPT clients" in {
        given(IdentifyClient(Personal, Service.Ppt, emptyBasket)) when
          transitions
            .copy(checkPptKnownFact = pptKnownFactCheckPasses)
            .identifyPptClient(authorisedAgent)(PptClient(PptRef(pptRef), pptRegDateStr)) should
          matchPattern {
            case (
                ConfirmClient(
                  AuthorisationRequest("PPT Client", Invitation(Some(ClientType.Personal), Service.Ppt, PptRef(pptRef)), _, _),
                  `emptyBasket`,
                  _),
                _) =>
          }
      }
      "transition to ConfirmClientPpt for business PPT clients" in {
        given(IdentifyClient(Business, Service.Ppt, emptyBasket)) when
          transitions
            .copy(checkPptKnownFact = pptKnownFactCheckPasses)
            .identifyPptClient(authorisedAgent)(PptClient(PptRef(pptRef), pptRegDateStr)) should
          matchPattern {
            case (
                ConfirmClient(AuthorisationRequest("PPT Client", Invitation(Some(Business), Service.Ppt, PptRef(pptRef)), _, _), `emptyBasket`, _),
                _) =>
          }
      }
      "transition to ConfirmClientPpt for trust PPT clients" in {
        given(IdentifyClient(Trust, Service.Ppt, emptyBasket)) when
          transitions
            .copy(checkPptKnownFact = pptKnownFactCheckPasses)
            .identifyPptClient(authorisedAgent)(PptClient(PptRef(pptRef), pptRegDateStr)) should
          matchPattern {
            case (
                ConfirmClient(AuthorisationRequest("PPT Client", Invitation(Some(Trust), Service.Ppt, PptRef(pptRef)), _, _), `emptyBasket`, _),
                _) =>
          }
      }
      "transition to PptRefNotFound when known fact check fails" in {
        given(IdentifyClient(Personal, Service.Ppt, emptyBasket)) when
          transitions
            .copy(checkPptKnownFact = pptKnownFactCheckFails)
            .identifyPptClient(authorisedAgent)(PptClient(PptRef(pptRef), pptRegDateStr)) should
          matchPattern {
            case (PptRefNotFound(PptRef(pptRef), _), _) =>
          }
      }

      "transition to PptRefNotFound when PPT client cannot be looked up" in {
        given(IdentifyClient(Personal, Service.Ppt, emptyBasket)) when
          transitions
            .copy(checkPptKnownFact = pptKnownFactCheckPasses, getPptCustomerName = _ => Future.successful(None))
            .identifyPptClient(authorisedAgent)(PptClient(PptRef(pptRef), pptRegDateStr)) should
          matchPattern {
            case (PptRefNotFound(PptRef(pptRef), _), _) =>
          }
      }

      "transition to ConfirmClientPersonalVat" in {

        given(IdentifyClient(Personal, Service.Vat, emptyBasket)) when
          vatClientIdentified(VatKnownFactCheckOk) should
          matchPattern {
            case (
                ConfirmClient(
                  AuthorisationRequest("Piglet", Invitation(Some(ClientType.Personal), Service.Vat, Vrn("123456")), AuthorisationRequest.NEW, _),
                  `emptyBasket`,
                  Some(false)),
                _) =>
          }
      }

      "transition to ConfirmClientPersonalVat when client insolvent" in {

        given(IdentifyClient(Personal, Service.Vat, emptyBasket)) when
          vatClientIdentified(VatRecordClientInsolvent) should
          matchPattern {
            case (
                ConfirmClient(
                  AuthorisationRequest("Piglet", Invitation(Some(ClientType.Personal), Service.Vat, Vrn("123456")), AuthorisationRequest.NEW, _),
                  `emptyBasket`,
                  Some(true)),
                _) =>
          }
      }

      "transition to KnownFactNotMatched when the vrn and regDate don't match" in {

        given(IdentifyClient(Personal, Service.Vat, emptyBasket)) when
          vatClientIdentified(VatKnownFactNotMatched) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }

      "transition to CannotCreateRequest when a migration is in process" in {

        given(IdentifyClient(Personal, Service.Vat, emptyBasket)) when
          vatClientIdentified(VatRecordMigrationInProgress) should
          thenGo(CannotCreateRequest(emptyBasket))
      }

      "transition to ClientNotSignedUp when the client is not signed up for the service" in {

        given(IdentifyClient(Personal, Service.Vat, emptyBasket)) when
          vatClientIdentified(VatDetailsNotFound) should
          thenGo(ClientNotSignedUp(Service.Vat, emptyBasket))
      }

      "transition to KnownFactNotMatched when the nino and dob don't match" in {

        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(false))

        given(IdentifyClient(Personal, Service.PersonalIncomeRecord, emptyBasket)) when
          transitions
            .copy(checkDobMatches = checkDobMatches)
            .identifiedIrvClient(authorisedAgent)(IrvClient("AB123456A", "1990-10-10")) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }

      "transition to KnownFactNotMatched when client not found" in {

        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(None)

        given(IdentifyClient(Personal, Service.PersonalIncomeRecord, emptyBasket)) when
          transitions
            .copy(checkDobMatches = checkDobMatches)
            .identifiedIrvClient(authorisedAgent)(IrvClient("AB123456A", "1990-10-10")) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }
    }

    // *************************************************
    //               IdentifyTrustClient
    // *************************************************

    "at state IdentifyTrustClient" should {

      "transition to ConfirmClient when trust client is identified with a UTR" in {
        given(IdentifyClient(Trust, Service.Trust, emptyBasket)) when
          transitions.identifiedTrustClient(authorisedAgent)(TrustClient(utr)) should
          matchPattern {
            case (cc: ConfirmClient, _) if cc.service == Service.Trust =>
          }
      }

      "transition to ConfirmClient (and change service to non-taxable trust) when trust client is identified with a URN" in {
        given(IdentifyClient(Trust, Service.Trust, emptyBasket)) when
          transitions.identifiedTrustClient(authorisedAgent)(TrustClient(urn)) should
          matchPattern {
            case (cc: ConfirmClient, _) if cc.service == Service.TrustNT =>
          }
      }

      "transition to ConfirmPostcodeCgt when cgt client is identified for a personal" in {
        given(IdentifyClient(Trust, Service.CapitalGains, emptyBasket)) when
          transitions.copy(getCgtSubscription = getCgtSubscription()).identifyCgtClient(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
          matchPattern {
            case (ConfirmPostcodeCgt(CgtRef("myCgtRef"), personal, emptyBasket, _, _), _) =>
          }
      }

      "transition to ConfirmPostcodeCgt when cgt client is identified for a trust" in {
        given(IdentifyClient(Trust, Service.CapitalGains, emptyBasket)) when
          transitions.copy(getCgtSubscription = getCgtSubscription()).identifyCgtClient(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
          matchPattern {
            case (ConfirmPostcodeCgt(CgtRef("myCgtRef"), business, emptyBasket, _, _), _) =>
          }
      }

      "transition to CgtRefNotFound" in {
        given(IdentifyClient(Personal, Service.CapitalGains, emptyBasket)) when
          transitions.identifyCgtClient(authorisedAgent)(CgtClient(CgtRef("myCgtRef"))) should
          matchPattern {
            case (CgtRefNotFound(CgtRef("myCgtRef"), _), _) =>
          }
      }
    }

    "at state ConfirmPostcodeCgt" should {

      "transition to ConfirmClientCgt when the postcode is matched for a UK personal client" in {
        given(ConfirmPostcodeCgt(CgtRef("cgtRef"), Personal, emptyBasket, Some("BN13 1FN"), "firstName lastName")) when
          transitions.copy(getCgtSubscription = getCgtSubscription()).confirmPostcodeCgt(authorisedAgent)(Postcode("BN13 1FN")) should
          matchPattern {
            case (cc @ ConfirmClient(AuthorisationRequest("firstName lastName", _, _, _), _, _), _) if cc.service == Service.CapitalGains =>
          }
      }

      "transition to ConfirmClientCgt when the postcode is matched for a UK business/trustee client" in {
        given(ConfirmPostcodeCgt(CgtRef("cgtRef"), Business, emptyBasket, Some("BN13 1FN"), "firstName lastName")) when
          transitions.copy(getCgtSubscription = getCgtSubscription()).confirmPostcodeCgt(authorisedAgent)(Postcode("BN13 1FN")) should
          matchPattern {
            case (cc @ ConfirmClient(AuthorisationRequest("firstName lastName", _, _, _), _, _), _) if cc.service == Service.CapitalGains =>
          }
      }

      "transition to KnownFactsNotMatched when the postcode is not matched for a UK client" in {
        given(ConfirmPostcodeCgt(CgtRef("cgtRef"), Personal, emptyBasket, Some("BN13 1FN"), "firstName lastName")) when
          transitions.copy(getCgtSubscription = getCgtSubscription()).confirmPostcodeCgt(authorisedAgent)(Postcode("BN13 1ZZ")) should
          matchPattern {
            case (KnownFactNotMatched(_), _) =>
          }
      }
    }

    "at state ConfirmCountryCodeCgt" should {

      "transition to ConfirmClientCgt when the countryCode is matched for a personal client" in {
        given(ConfirmCountryCodeCgt(CgtRef("cgtRef"), Personal, emptyBasket, "IN", "firstName lastName")) when
          transitions.copy(getCgtSubscription = getCgtSubscription("IN")).confirmCountryCodeCgt(authorisedAgent)(CountryCode("IN")) should
          matchPattern {
            case (cc @ ConfirmClient(AuthorisationRequest("firstName lastName", _, _, _), _, _), _) if cc.service == Service.CapitalGains =>
          }
      }

      "transition to ConfirmClientCgt when the countryCode is matched for a business client" in {
        given(ConfirmCountryCodeCgt(CgtRef("cgtRef"), Business, emptyBasket, "IN", "firstName lastName")) when
          transitions.copy(getCgtSubscription = getCgtSubscription("IN")).confirmCountryCodeCgt(authorisedAgent)(CountryCode("IN")) should
          matchPattern {
            case (cc @ ConfirmClient(AuthorisationRequest("firstName lastName", _, _, _), _, _), _) if cc.service == Service.CapitalGains =>
          }
      }

      "transition to KnownFactsNotMatched when the countryCode is not matched for a non UK client" in {
        given(ConfirmCountryCodeCgt(CgtRef("cgtRef"), Personal, emptyBasket, "IN", "firstName lastName")) when
          transitions.copy(getCgtSubscription = getCgtSubscription("IN")).confirmCountryCodeCgt(authorisedAgent)(CountryCode("FR")) should
          matchPattern {
            case (KnownFactNotMatched(_), _) =>
          }
      }
    }

    "at state IdentifyBusinessClient" should {

      "transition to SelectClientType" in {

        given(IdentifyClient(Business, Service.Vat, emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }

      "transition to ConfirmClient" in {

        given(IdentifyClient(Business, Service.Vat, emptyBasket)) when
          transitions
            .copy(checkRegDateMatches = (_, _) => Future(VatKnownFactCheckOk))
            .identifiedVatClient(authorisedAgent)(VatClient("123456", "2010-10-10")) should
          matchPattern {
            case (
                ConfirmClient(
                  AuthorisationRequest("Piglet", Invitation(Some(ClientType.Business), Service.Vat, Vrn("123456")), AuthorisationRequest.NEW, _),
                  _,
                  Some(false)),
                _) =>
          }
      }

      "transition to KnownFactNotMatched client" in {

        given(IdentifyClient(Business, Service.Vat, emptyBasket)) when
          transitions
            .copy(checkRegDateMatches = (_, _) => Future(VatKnownFactNotMatched))
            .identifiedVatClient(authorisedAgent)(VatClient("123456", "2010-10-10")) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }

      "transition to CannotCreateRequest" in {

        given(IdentifyClient(Business, Service.Vat, emptyBasket)) when
          transitions
            .copy(checkRegDateMatches = (_, _) => Future(VatRecordMigrationInProgress))
            .identifiedVatClient(authorisedAgent)(VatClient("123456", "2010-10-10")) should
          thenGo(CannotCreateRequest(emptyBasket))
      }

      "transition to ActiveRelationshipExists" in {

        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(VatKnownFactCheckOk)

        given(IdentifyClient(Business, Service.Vat, emptyBasket)) when
          transitions
            .copy(checkRegDateMatches = checkRegDateMatches, hasActiveRelationshipFor = hasActiveRelationship)
            .identifiedVatClient(authorisedAgent)(VatClient("123456", "2010-10-10")) should
          matchPattern {
            case (
                ConfirmClient(
                  AuthorisationRequest("Piglet", Invitation(Some(ClientType.Business), Service.Vat, Vrn("123456")), AuthorisationRequest.NEW, _),
                  _,
                  Some(false)),
                _) =>
          }
      }

      "transition to ClientNotSignedUp" in {

        given(IdentifyClient(Business, Service.Vat, emptyBasket)) when
          transitions
            .copy(checkRegDateMatches = (_, _) => Future(VatDetailsNotFound))
            .identifiedVatClient(authorisedAgent)(VatClient("123456", "2010-10-10")) should
          thenGo(ClientNotSignedUp(Service.Vat, emptyBasket))
      }
    }

    "at state ConfirmClientItsa" should {

      val authorisationRequest = AuthorisationRequest("Piglet", Invitation(Some(ClientType.Personal), Service.MtdIt, Nino("AB123456A")))

      def hasPartialAuthorisation(arn: Arn, clientId: String): Future[Boolean] =
        Future successful (true)

      "transition to SelectClientType" in {

        given(ConfirmClient(authorisationRequest, emptyBasket)) when
          start should
          thenGo(SelectClientType(emptyBasket))
      }

      "transition to ReviewAuthorisationsPersonal" in {

        given(ConfirmClient(authorisationRequest, emptyBasket)) when
          transitions.copy(featureFlags = allEnabled.copy(showHmrcCgt = false)).clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenMatch {
            case ReviewAuthorisations(Personal, _, basket) if basket.nonEmpty =>
          }
      }

      "transition to SelectPersonalService" in {

        given(ConfirmClient(authorisationRequest, emptyBasket)) when
          transitions.copy(featureFlags = allEnabled.copy(showHmrcCgt = false)).clientConfirmed(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyClient(Personal, Service.MtdIt, emptyBasket))
      }

      "transition to PendingInvitationExists when the invitation is already in the basket" in {

        given(
          ConfirmClient(
            authorisationRequest,
            Set(authorisationRequest)
          )
        ) when
          transitions.copy(featureFlags = allEnabled.copy(showHmrcCgt = false)).clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenMatch {
            case PendingInvitationExists(_, "Piglet", "invitation/link", basket) if basket.nonEmpty =>
          }
      }

      "transition to PartialAuthorisationExists when there is a Partial Authorisation" in {

        given(ConfirmClient(authorisationRequest, emptyBasket)) when
          transitions
            .copy(featureFlags = allEnabled.copy(showHmrcCgt = false))
            .copy(hasPartialAuthorisationFor = hasPartialAuthorisation)
            .clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenMatch {
            case PartialAuthorisationExists(_) =>
          }
      }

      "transition to AlreadyCopiedAcrossItsa when there is a Legacy Mapping" in {
        when(mockAppConfig.featuresAltItsa).thenReturn(true)
        given(ConfirmClient(authorisationRequest, emptyBasket)) when
          transitions
            .copy(featureFlags = allEnabled.copy(showHmrcCgt = false))
            .copy(legacySaRelationshipStatusFor = legacySaRelationshipStatusMapped)
            .clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenMatch {
            case AlreadyCopiedAcrossItsa =>
          }
      }

      "transition to LegacyAuthorisationDetected when there is an other Legacy Mapping" in {
        when(mockAppConfig.featuresAltItsa).thenReturn(true)
        given(ConfirmClient(authorisationRequest, emptyBasket)) when
          transitions
            .copy(featureFlags = allEnabled.copy(showHmrcCgt = false))
            .copy(legacySaRelationshipStatusFor = legacySaRelationshipStatusNotMapped)
            .clientConfirmed(authorisedAgent)(Confirmation(true)) should
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
          ConfirmClient(
            AuthorisationRequest(
              "Piglet",
              Invitation(
                Some(Personal),
                Service.Vat,
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
          ConfirmClient(
            AuthorisationRequest(
              "Piglet",
              Invitation(
                Some(Personal),
                Service.Vat,
                Vrn("123456")
              )
            ),
            emptyBasket
          )
        ) when
          transitions.copy(featureFlags = allEnabled.copy(showHmrcCgt = false)).clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenMatch {
            case ReviewAuthorisations(Personal, _, basket) if basket.nonEmpty =>
          }
      }

      "transition to ClientInsolvent" in {

        given(
          ConfirmClient(
            AuthorisationRequest(
              "Piglet",
              Invitation(
                Some(Personal),
                Service.Vat,
                Vrn("123456")
              )
            ),
            emptyBasket,
            clientInsolvent = Some(true)
          )
        ) when
          transitions.copy(featureFlags = allEnabled.copy(showHmrcCgt = false)).clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenMatch {
            case ClientInsolvent(_) =>
          }
      }

      "transition to PersonalServiceSelected" in {

        given(
          ConfirmClient(
            AuthorisationRequest(
              "Piglet",
              Invitation(
                Some(Personal),
                Service.Vat,
                Vrn("123456")
              )
            ),
            emptyBasket
          )
        ) when
          transitions.copy(featureFlags = allEnabled.copy(showHmrcCgt = false)).clientConfirmed(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyClient(Personal, Service.Vat, emptyBasket))
      }
    }

    "at state ConfirmClientBusinessVat" should {

      val authorisationRequest =
        AuthorisationRequest(
          "Piglet",
          Invitation(Some(Business), Service.Vat, Vrn("123456"))
        )

      "after start transition to Start" in {

        given(ConfirmClient(authorisationRequest, emptyBasket)) when start should thenGo(
          SelectClientType(emptyBasket)
        )
      }

      "after clientConfirmed(true) transition to ReviewAuthorisations" in {

        given(ConfirmClient(authorisationRequest, emptyBasket)) when
          transitions.copy(featureFlags = allEnabled.copy(showHmrcCgt = false)).clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenGo(ReviewAuthorisations(Business, availableBusinessServices, Set(authorisationRequest)))
      }

      "after clientConfirmed(false) transition to IdentifyBusinessClient" in {

        given(ConfirmClient(authorisationRequest, emptyBasket)) when
          transitions.copy(featureFlags = allEnabled.copy(showHmrcCgt = false)).clientConfirmed(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyClient(Business, Service.Vat, emptyBasket))
      }

      "transition to PendingInvitationExists when there is already a pending invitation" in {

        def hasPendingInvitation(arn: Arn, clientId: String, service: Service): Future[Boolean] = Future.successful(true)

        given(ConfirmClient(authorisationRequest, emptyBasket)) when
          transitions
            .copy(featureFlags = allEnabled.copy(showHmrcCgt = false))
            .copy(hasPendingInvitationsFor = hasPendingInvitation)
            .clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenGo(PendingInvitationExists(Business, "Piglet", "invitation/link", emptyBasket))
      }

      "transition to ActiveAuthorisationExists when an active relationship already exists" in {

        given(ConfirmClient(authorisationRequest, emptyBasket)) when transitions
          .copy(featureFlags = allEnabled.copy(showHmrcCgt = false))
          .copy(hasActiveRelationshipFor = hasActiveRelationship)
          .clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenGo(ActiveAuthorisationExists(Business, Service.Vat, emptyBasket))
      }
    }

    "at state ConfirmClientPersonalCgt" should {

      "transition to IdentifyPersonalClient" in {

        val authorisationRequest =
          AuthorisationRequest("Roo", Invitation(Some(Personal), Service.CapitalGains, CgtRef("myCgtRef")))

        given(ConfirmClient(authorisationRequest, emptyBasket)) when
          transitions.clientConfirmed(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyClient(Personal, Service.CapitalGains, emptyBasket))
      }

    }

    "at state ConfirmClientCgt" should {

      "transition to IdentifyTrustClient" in {

        val authorisationRequest =
          AuthorisationRequest("Roo", Invitation(Some(Trust), Service.CapitalGains, CgtRef("myCgtRef")))

        given(ConfirmClient(authorisationRequest, emptyBasket)) when
          transitions.clientConfirmed(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyClient(Trust, Service.CapitalGains, emptyBasket))
      }

    }

    "at state ConfirmClient" should {

      val utr = Utr("4937455253")

      val authorisationRequest = AuthorisationRequest("Piglet", Invitation(Some(ClientType.Trust), Service.Trust, utr))

      "transition to IdentifyTrustClient if NO is selected" in {

        given(ConfirmClient(authorisationRequest, emptyBasket)) when
          transitions.copy(featureFlags = allEnabled.copy(showHmrcCgt = false)).clientConfirmed(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyClient(Trust, Service.Trust, emptyBasket))
      }

      "transition to InvitationSentBusiness with taxable trust" in {

        given(ConfirmClient(authorisationRequest, emptyBasket)) when
          transitions.copy(featureFlags = allEnabled.copy(showHmrcCgt = false)).clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSent(ClientType.Trust, "invitation/link", None, "abc@xyz.com", Set(Service.Trust)))
      }

      "transition to PendingInvitationExists when a pending invitation exists for the service" in {

        given(ConfirmClient(authorisationRequest, emptyBasket)) when
          transitions
            .copy(featureFlags = allEnabled.copy(showHmrcCgt = false))
            .copy(hasPendingInvitationsFor = hasPendingInvitation)
            .clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenGo(PendingInvitationExists(Trust, "Piglet", "invitation/link", emptyBasket))
      }

      "transition to ActiveAuthorisationExists when a pending invitation exists for the service" in {

        def hasActiveRelationship(arn: Arn, clientId: String, service: Service): Future[Boolean] =
          Future.successful(true)

        given(ConfirmClient(authorisationRequest, emptyBasket)) when
          transitions
            .copy(featureFlags = allEnabled.copy(showHmrcCgt = false))
            .copy(hasActiveRelationshipFor = hasActiveRelationship)
            .clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenGo(ActiveAuthorisationExists(Trust, Service.Trust, emptyBasket))
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
          AuthorisationRequest("Mr Client", Invitation(Some(Trust), Service.MtdIt, Nino("AB123456A")), AuthorisationRequest.NEW, "ABC123")

        val authorisationRequestCreated = authorisationRequestNew.copy(state = AuthorisationRequest.CREATED)

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]) =
          Future(Set(authorisationRequestCreated))

        given(
          ReviewAuthorisations(Trust, availableTrustServices, Set(authorisationRequestNew))
        ) when
          transitions.copy(createMultipleInvitations = createMultipleInvitations).authorisationsReviewed(authorisedAgent)(Confirmation(false)) should
          thenGo(InvitationSent(ClientType.Trust, "invitation/link", None, "abc@xyz.com", Set(Service.MtdIt)))
      }

      "after authorisationsReviewed(false) when all fail transition to AuthorisationsReviewedAllFailed" in {

        val authorisationRequestNew =
          AuthorisationRequest("Mr Client", Invitation(Some(Trust), Service.MtdIt, Nino("AB123456A")), AuthorisationRequest.NEW, "ABC123")

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
          AuthorisationRequest(
            "Mr Client",
            Invitation(Some(ClientType.Personal), Service.MtdIt, Nino("AB123456B")),
            AuthorisationRequest.NEW,
            "ABC123")

        val authorisationRequestNew2 =
          AuthorisationRequest(
            "Mr Client",
            Invitation(Some(ClientType.Personal), Service.MtdIt, Nino("AB123456B")),
            AuthorisationRequest.NEW,
            "ABC124")

        val authorisationRequestSuccess1 =
          AuthorisationRequest(
            "Mr Client",
            Invitation(Some(ClientType.Personal), Service.MtdIt, Nino("AB123456A")),
            AuthorisationRequest.CREATED,
            "ABC123")

        val authorisationRequestFailed2 =
          AuthorisationRequest(
            "Mr Client",
            Invitation(Some(ClientType.Personal), Service.MtdIt, Nino("AB123456B")),
            AuthorisationRequest.FAILED,
            "ABC124")

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
          AuthorisationRequest("Mr Client", Invitation(Some(Trust), Service.MtdIt, Nino("AB123456A")), AuthorisationRequest.NEW, "ABC123")

        given(
          ReviewAuthorisations(Trust, availableTrustServices, Set(authorisationRequest))
        ) when
          transitions.deleteAuthorisationRequest("ABC123")(authorisedAgent) should
          thenGo(DeleteAuthorisationRequest(Trust, authorisationRequest, Set(authorisationRequest)))
      }

      "throw an Exception when there is no corresponding itemId in the basket" in {

        val authorisationRequest =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), Service.Trust, Utr("1234567890")), AuthorisationRequest.NEW, "ABC123")

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
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), Service.MtdIt, Nino("AB123456A")), AuthorisationRequest.NEW, "ABC123")

        val authorisationRequestCreated = authorisationRequestNew.copy(state = AuthorisationRequest.CREATED)

        def createMultipleInvitations(arn: Arn, requests: Set[AuthorisationRequest]) =
          Future(Set(authorisationRequestCreated))

        def createInvitationSent: CreateInvitationSent =
          (_: String, _: String, _: Arn, _: Basket) =>
            Future.successful(
              InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Set(Service.MtdIt), isAltItsa = Some(false)))

        given(
          ReviewAuthorisations(Personal, availableServices, Set(authorisationRequestNew))
        ) when
          transitions
            .copy(createMultipleInvitations = createMultipleInvitations, createInvitationSent = createInvitationSent)
            .authorisationsReviewed(authorisedAgent)(Confirmation(false)) should
          thenGo(InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Set(Service.MtdIt), isAltItsa = Some(false)))
      }

      "after authorisationsReviewed(false) transition to InvitationSentPersonal (alt itsa)" in {

        val authorisationRequestNew =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), Service.MtdIt, Nino("AB123456A")), AuthorisationRequest.NEW, "ABC123")

        val authorisationRequestCreated = authorisationRequestNew.copy(state = AuthorisationRequest.CREATED)

        def createInvitationSent: CreateInvitationSent =
          (_: String, _: String, _: Arn, _: Basket) =>
            Future.successful(InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Set(Service.MtdIt), isAltItsa = Some(true)))

        given(
          ReviewAuthorisations(Personal, availableServices, Set(authorisationRequestNew))
        ) when
          transitions
            .copy(createMultipleInvitations = (_, _) => Future(Set(authorisationRequestCreated)), createInvitationSent = createInvitationSent)
            .authorisationsReviewed(authorisedAgent)(Confirmation(false)) should
          thenGo(InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Set(Service.MtdIt), isAltItsa = Some(true)))
      }

      "after authorisationsReviewed(false) when all fail transition to AuthorisationsReviewedAllFailed" in {

        val authorisationRequestNew =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), Service.MtdIt, Nino("AB123456A")), AuthorisationRequest.NEW, "ABC123")

        val authorisationRequestFailed = authorisationRequestNew.copy(state = AuthorisationRequest.FAILED)

        def createInvitationSent: CreateInvitationSent =
          (_: String, _: String, _: Arn, _: Basket) =>
            Future.successful(
              InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Set(Service.MtdIt), isAltItsa = Some(false)))

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
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), Service.MtdIt, Nino("AB123456B")), AuthorisationRequest.NEW, "ABC123")
        val authorisationRequestNew2 = authorisationRequestNew1.copy(itemId = "ABC124")
        val authorisationRequestSuccess1 = authorisationRequestNew1.copy(state = AuthorisationRequest.CREATED)
        val authorisationRequestFailed2 = authorisationRequestNew2.copy(state = AuthorisationRequest.FAILED)

        def createInvitationSent: CreateInvitationSent =
          (_: String, _: String, _: Arn, _: Basket) =>
            Future.successful(
              InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Set(Service.MtdIt), isAltItsa = Some(false)))

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
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), Service.MtdIt, Nino("AB123456A")), AuthorisationRequest.NEW, "ABC123")

        given(
          ReviewAuthorisations(Personal, availableServices, Set(authorisationRequest))
        ) when
          transitions.deleteAuthorisationRequest("ABC123")(authorisedAgent) should
          thenGo(DeleteAuthorisationRequest(Personal, authorisationRequest, Set(authorisationRequest)))
      }

      "throw an Exception when there is no corresponding itemId in the basket" in {

        val authorisationRequest =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), Service.MtdIt, Nino("AB123456A")), AuthorisationRequest.NEW, "ABC123")

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
          Invitation(Some(Personal), Service.MtdIt, Nino("AB123456A")),
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
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), Service.MtdIt, Nino("AB123456A")), AuthorisationRequest.NEW, "ABC123")

        val authorisationRequest2 =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), Service.MtdIt, Nino("AB123456B")), AuthorisationRequest.NEW, "ABC124")

        given(
          DeleteAuthorisationRequest(Personal, authorisationRequest1, Set(authorisationRequest1, authorisationRequest2))
        ) when
          transitions.confirmDeleteAuthorisationRequest(authorisedAgent)(Confirmation(true)) should
          thenGo(ReviewAuthorisations(Personal, availableServices, Set(authorisationRequest2)))
      }

      "after confirmDeleteAuthorisationRequest(true) should transition to AllAuthorisationsRemoved when there is nothing left in the basket" in {

        val authorisationRequest =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), Service.MtdIt, Nino("AB123456A")), AuthorisationRequest.NEW, "ABC123")

        given(
          DeleteAuthorisationRequest(Personal, authorisationRequest, Set(authorisationRequest))
        ) when
          transitions.confirmDeleteAuthorisationRequest(authorisedAgent)(Confirmation(true)) should
          thenGo(AllAuthorisationsRemoved)
      }

      "after confirmDeleteAuthorisationRequest(false) should transition to ReviewAuthorisationPersonal with basket in tact" in {

        val authorisationRequest =
          AuthorisationRequest("Mr Client", Invitation(Some(Personal), Service.MtdIt, Nino("AB123456A")), AuthorisationRequest.NEW, "ABC123")

        given(
          DeleteAuthorisationRequest(Personal, authorisationRequest, Set(authorisationRequest))
        ) when
          transitions.confirmDeleteAuthorisationRequest(authorisedAgent)(Confirmation(false)) should
          thenGo(ReviewAuthorisations(Personal, availableServices, Set(authorisationRequest)))
      }
    }

    "at state SomeAuthorisationFailed" should {

      "go to InvitationSent state" in {

        given(
          SomeAuthorisationsFailed(
            "invitation/link",
            None,
            "abc@xyz.com",
            Set.empty
          )
        ) when transitions.continueSomeResponsesFailed(authorisedAgent) should
          thenGo(InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Set.empty, isAltItsa = None))
      }
    }
  }
}
