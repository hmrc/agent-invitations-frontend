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
import org.scalatest.BeforeAndAfter
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.{start => _, _}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{Business, Personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentFastTrackRequest, _}
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import support.UnitSpec
import play.api.test.Helpers._
import uk.gov.hmrc.agentmtdidentifiers.model.SuspensionDetails
import uk.gov.hmrc.agentinvitationsfrontend.models.VatKnownFactCheckResult.{VatDetailsNotFound, VatKnownFactCheckOk, VatKnownFactNotMatched, VatRecordClientInsolvent}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AgentInvitationFastTrackJourneyModelSpec extends UnitSpec with StateMatchers[State] with BeforeAndAfter {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  case class given(initialState: State) extends AgentInvitationFastTrackJourneyService with TestStorage[(State, List[State])] {
    await(save((initialState, Nil)))

    def when(transition: Transition): (State, List[State]) =
      await(super.apply(transition))
  }

  val authorisedAgent = AuthorisedAgent(Arn("TARN0000001"))
  val availableServices: Set[Service] = Set(Service.PersonalIncomeRecord, Service.MtdIt, Service.Vat, Service.Ppt)
  val nino = "AB123456A"
  val postCode = Some("BN114AW")
  val vrn = "123456"
  val vatRegDate = Some("2010-10-10")
  val dob = Some("1990-10-10")
  val utr = Utr("1977030537")
  val urn = Urn("XXTRUST10010010")
  val pptRef = PptRef("XAPPT000012345")
  val cgtRef = CgtRef("XMCGTP123456789")

  val tpd = TypeOfPersonDetails("Individual", Left(IndividualName("firstName", "lastName")))

  def cgtAddressDetails(countryCode: String = "GB") =
    CgtAddressDetails("line1", Some("line2"), Some("line2"), Some("line2"), countryCode, Some("BN13 1FN"))

  def cgtSubscription(countryCode: String = "GB") =
    CgtSubscription("CGT", SubscriptionDetails(tpd, cgtAddressDetails(countryCode)))

  def pptSubscription(regDate: LocalDate) =
    PptSubscription("PPT", regDate, None)

  def hasNoLegacyMapping(arn: Arn, clientId: String): Future[Boolean] =
    Future.successful(false)

  def isNotAltItsa(arn: Arn, clientId: String): Future[Boolean] = Future.successful(false)

  def isAltItsa(arn: Arn, clientId: String): Future[Boolean] = Future.successful(true)

  def legacySaRelationshipStatusMapped(arn: Arn, clientId: String): Future[LegacySaRelationshipResult] =
    Future.successful(LegacySaRelationshipFoundAndMapped)

  def legacySaRelationshipStatusNotMapped(arn: Arn, clientId: String): Future[LegacySaRelationshipResult] =
    Future.successful(LegacySaRelationshipFoundNotMapped)

  def legacySaRelationshipStatusNotFound(arn: Arn, clientId: String): Future[LegacySaRelationshipResult] =
    Future.successful(LegacySaRelationshipNotFound)

  def getCgtSubscription(countryCode: String = "GB"): GetCgtSubscription =
    CgtRef => Future(Some(cgtSubscription(countryCode)))

  def getPptSubscription(regDate: LocalDate = new LocalDate(2021, 1, 1)): GetPptSubscription =
    PptRef => Future(Some(pptSubscription(regDate)))

  val mockAppConfig = mock(classOf[AppConfig])

  before {
    when(mockAppConfig.featuresAltItsa).thenReturn(true)
  }

  "AgentInvitationFastTrackJourneyService" when {

    val notSuspended: GetSuspensionDetails = () => Future.successful(SuspensionDetails(false, None))

    "at state Prologue" should {
      "transition to CheckDetailsCompleteItsa when all required fields are present for itsa service" in {

        val fastTrackRequest = AgentFastTrackRequest(None, Service.MtdIt, "ni", nino, postCode)

        given(Prologue(None, None)) when start(true, notSuspended)(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetailsComplete(
              fastTrackRequest = fastTrackRequest.copy(clientType = Some(Personal)),
              None
            ))
      }
      "transition to CheckDetailsNoPostcode when the postcode is missing for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.MtdIt, "ni", nino, None)

        given(Prologue(None, None)) when start(true, notSuspended)(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetailsNoPostcode(
              fastTrackRequest = fastTrackRequest.copy(clientType = Some(Personal)),
              None
            ))
      }
      "transition to CheckDetailsCompleteIrv when all required fields are present for irv service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.PersonalIncomeRecord, "ni", nino, dob)

        given(Prologue(None, None)) when start(true, notSuspended)(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetailsComplete(
              fastTrackRequest = fastTrackRequest.copy(clientType = Some(Personal)),
              None
            ))
      }
      "transition to CheckDetailsNoDob when there is no dob for irv service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.PersonalIncomeRecord, "ni", nino, None)

        given(Prologue(None, None)) when start(true, notSuspended)(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetailsNoDob(
              fastTrackRequest = fastTrackRequest.copy(clientType = Some(Personal)),
              None
            ))
      }

      "transition to AgentSuspended when there are all the required fields are present for irv service but the agent has been suspended" +
        "for this service" in {
        val fastTrackRequest =
          AgentFastTrackRequest(None, Service.MtdIt, "ni", nino, None)

        def suspendedForIT() = Future.successful(SuspensionDetails(true, Some(Set("ITSA"))))

        given(Prologue(None, None)) when start(true, suspendedForIT)(Some("continue/url"))(authorisedAgent)(fastTrackRequest) should
          thenGo(
            SuspendedAgent(Service.MtdIt, Some("continue/url"))
          )
      }

      "transition to CheckDetailsCompleteVat when all required fields are present for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, "ni", nino, vatRegDate)

        given(Prologue(None, None)) when start(true, notSuspended)(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetailsComplete(
              fastTrackRequest = fastTrackRequest,
              None
            ))
      }
      "transition to CheckDetailsNoVatRegDate when there is no vat reg date for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, "ni", nino, None)

        given(Prologue(None, None)) when start(true, notSuspended)(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetailsNoVatRegDate(
              fastTrackRequest = fastTrackRequest,
              None
            ))
      }
      "transition to CheckDetailsCompleteVat when all required fields are present for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Vat, "ni", nino, vatRegDate)

        given(Prologue(None, None)) when start(true, notSuspended)(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetailsComplete(
              fastTrackRequest = fastTrackRequest,
              None
            ))
      }
      "transition to CheckDetailsNoVatRegDate when there is no vat reg date for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Vat, "ni", nino, None)

        given(Prologue(None, None)) when start(true, notSuspended)(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetailsNoVatRegDate(
              fastTrackRequest = fastTrackRequest,
              None
            ))
      }
      "transition to CheckDetailsNoClientTypeVat when there is no client type for vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.Vat, "ni", nino, None)

        given(Prologue(None, None)) when start(true, notSuspended)(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetailsNoClientTypeVat(
              fastTrackRequest = fastTrackRequest,
              None
            ))
      }

      "transition to AgentSuspended when there are all the required fields are present for vat service but the agent has been suspended" +
        "for this service" in {
        val fastTrackRequest =
          AgentFastTrackRequest(None, Service.Vat, "ni", nino, None)

        def suspendedForVat() = Future.successful(SuspensionDetails(true, Some(Set("VATC"))))

        given(Prologue(None, None)) when start(true, suspendedForVat)(Some("continue/url"))(authorisedAgent)(fastTrackRequest) should
          thenGo(
            SuspendedAgent(Service.Vat, Some("continue/url"))
          )
      }

      "transition to CheckDetailsCompleteTrust when there are all the required fields are present for a Trust service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(ClientType.Trust), Service.Trust, "utr", utr.value, None)

        given(Prologue(None, None)) when start(true, notSuspended)(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetailsComplete(
              fastTrackRequest = fastTrackRequest,
              None
            ))
      }

      "transition to CheckDetailsCompleteTrust when there are all the required fields are present for a TrustNT service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(ClientType.Trust), Service.TrustNT, "urn", urn.value, None)

        given(Prologue(None, None)) when start(true, notSuspended)(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetailsComplete(
              fastTrackRequest = fastTrackRequest,
              None
            ))
      }

      "transition to CheckDetailsCompleteCgt when there are all the required fields are present for a CGT service" in {
        val fastTrackRequest =
          AgentFastTrackRequest(Some(ClientType.Business), Service.CapitalGains, "CGTPDRef", cgtRef.value, None)

        given(Prologue(None, None)) when start(true, notSuspended)(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetailsComplete(
              fastTrackRequest = fastTrackRequest,
              None
            ))
      }

      "transition to CheckDetailsCompletePpt when there are all the required fields are present for a PPT service" in {
        val fastTrackRequest =
          AgentFastTrackRequest(Some(ClientType.Business), Service.Ppt, "EtmpRegistrationNumber", pptRef.value, None)

        given(Prologue(None, None)) when start(true, notSuspended)(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetailsComplete(
              fastTrackRequest = fastTrackRequest,
              None
            ))
      }

      "transition to AgentSuspended when there are all the required fields are present for Trust service but the agent has been suspended" +
        "for this service" in {
        val fastTrackRequest =
          AgentFastTrackRequest(Some(ClientType.Business), Service.CapitalGains, "CGTPDRef", cgtRef.value, None)

        def suspendedForTrust() = Future.successful(SuspensionDetails(true, Some(Set("CGT"))))

        given(Prologue(None, None)) when start(true, suspendedForTrust)(Some("continue/url"))(authorisedAgent)(fastTrackRequest) should
          thenGo(
            SuspendedAgent(Service.CapitalGains, Some("continue/url"))
          )
      }

      "transition to AgentSuspended when there are all the required fields are present for PPT service but the agent has been suspended" +
        "for this service" in {
        val fastTrackRequest =
          AgentFastTrackRequest(Some(ClientType.Business), Service.Ppt, "EtmpRegistrationNumber", pptRef.value, None)

        def suspendedForTrust() = Future.successful(SuspensionDetails(true, Some(Set("PPT"))))

        given(Prologue(None, None)) when start(true, suspendedForTrust)(Some("continue/url"))(authorisedAgent)(fastTrackRequest) should
          thenGo(
            SuspendedAgent(Service.Ppt, Some("continue/url"))
          )
      }
    }

    "at CheckDetailsCompleteCgt" should {
      "transition to ConfirmPostcodeCgt for CGT if client is UK based" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, "CGTPDRef", cgtRef.value, None)

        given(CheckDetailsComplete(fastTrackRequest, None)) when Transitions
          .checkedDetailsNoKnownFact(getCgtSubscription("GB"), getPptSubscription(new LocalDate(2021, 1, 1)))(authorisedAgent) should
          thenGo(ConfirmPostcodeCgt(fastTrackRequest, None, Some("BN13 1FN"), "firstName lastName"))
      }

      "transition to ConfirmCountryCodeCgt for CGT if client is not UK based" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, "CGTPDRef", cgtRef.value, None)

        given(CheckDetailsComplete(fastTrackRequest, None)) when checkedDetailsNoKnownFact(
          getCgtSubscription("FR"),
          pptRef => Future.successful(None))(authorisedAgent) should
          thenGo(ConfirmCountryCodeCgt(fastTrackRequest, None, "FR", "firstName lastName"))
      }

      "transition to CgtRefNotFound if there is no cgt subscription found" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, "CGTPDRef", cgtRef.value, None)

        given(CheckDetailsComplete(fastTrackRequest, None)) when checkedDetailsNoKnownFact(
          cgtRef => Future.successful(None),
          pptRef => Future.successful(None))(authorisedAgent) should
          thenGo(CgtRefNotFound(fastTrackRequest, None))
      }
    }

    "at IdentifyCgtClient" should {

      def getCgtSubscription(countryCode: String = "GB"): GetCgtSubscription =
        CgtRef => Future(Some(cgtSubscription(countryCode)))

      "transition to ConfirmPostcodeCgt" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, "CGTPDRef", cgtRef.value, None)

        given(IdentifyCgtClient(fastTrackRequest, None)) when identifyCgtClient(getCgtSubscription())(authorisedAgent)(CgtClient(cgtRef)) should
          thenGo(ConfirmPostcodeCgt(fastTrackRequest, None, Some("BN13 1FN"), "firstName lastName"))
      }
    }

    "at SelectClientTypeCgt" should {

      def getCgtSubscription(countryCode: String = "GB"): GetCgtSubscription =
        CgtRef => Future(Some(cgtSubscription(countryCode)))

      def hasNoPendingInvitation(arn: Arn, clientId: String, service: Service): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: Service): Future[Boolean] =
        Future.successful(false)
      def hasNoPartialAuthorisation(arn: Arn, clientId: String): Future[Boolean] =
        Future successful (false)

      def createInvitation(arn: Arn, invitation: Invitation): Future[InvitationId] =
        Future(InvitationId("ABBTAKTMFKWU8"))
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      def getClientName(clientId: String, service: Service) = Future(Some("firstName lastName"))
      def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(true))
      def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(VatKnownFactCheckOk)
      def checkPptKnownFact(pptRef: PptRef, regDate: LocalDate) = Future(true)
      def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(true))
      def getAgencyEmail() = Future("abc@xyz.com")

      "transition to ConfirmPostcodeCgt when CGT client is a UK based client" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, "CGTPDRef", cgtRef.value, None)

        given(SelectClientTypeCgt(fastTrackRequest, None)) when
          selectedClientType(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(createInvitation)(getAgentLink)(
            getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(hasNoPartialAuthorisation)(isNotAltItsa)(
            legacySaRelationshipStatusNotFound)(getCgtSubscription(), getPptSubscription())(mockAppConfig)(authorisedAgent)("personal") should
          thenGo(ConfirmPostcodeCgt(fastTrackRequest, None, Some("BN13 1FN"), "firstName lastName"))
      }

      "transition to ConfirmCountryCodeCgt when CGT client is a non UK based client" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, "CGTPDRef", cgtRef.value, None)

        given(SelectClientTypeCgt(fastTrackRequest, None)) when
          selectedClientType(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(createInvitation)(getAgentLink)(
            getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(hasNoPartialAuthorisation)(isNotAltItsa)(
            legacySaRelationshipStatusNotFound)(getCgtSubscription("FR"), getPptSubscription())(mockAppConfig)(authorisedAgent)("personal") should
          thenGo(ConfirmCountryCodeCgt(fastTrackRequest, None, "FR", "firstName lastName"))
      }

      "transition to IdentifyCgtClient when changing is true" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, "CGTPDRef", cgtRef.value, None)

        given(SelectClientTypeCgt(fastTrackRequest, None, isChanging = true)) when
          selectedClientType(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(createInvitation)(getAgentLink)(
            getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(hasNoPartialAuthorisation)(isNotAltItsa)(
            legacySaRelationshipStatusNotFound)(getCgtSubscription("FR"), getPptSubscription())(mockAppConfig)(authorisedAgent)("personal") should
          thenGo(IdentifyCgtClient(fastTrackRequest, None))
      }
    }

    "at ConfirmPostcodeCgt" should {
      "transition to ConfirmClientCgt when postcodes are matched for a UK client" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, "CGTPDRef", cgtRef.value, None)

        given(ConfirmPostcodeCgt(fastTrackRequest, None, Some("BN13 1FN"), "some-cgt-name")) when
          confirmPostcodeCgt(authorisedAgent)(Postcode("BN13 1FN")) should
          thenGo(ConfirmClientCgt(fastTrackRequest.copy(knownFact = Some("BN13 1FN")), None, "some-cgt-name"))
      }

      "transition to KnownFactNotMatched when postcodes are not matched for a UK client" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, "CGTPDRef", cgtRef.value, None)

        given(ConfirmPostcodeCgt(fastTrackRequest, None, Some("BN13 1FN"), "some-cgt-name")) when
          confirmPostcodeCgt(authorisedAgent)(Postcode("BN13 1XX")) should
          thenGo(KnownFactNotMatched(fastTrackRequest, None))
      }
    }

    "at ConfirmCountryCodeCgt" should {
      "transition to ConfirmClientCgt when country codes are matched" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, "CGTPDRef", cgtRef.value, None)

        given(ConfirmCountryCodeCgt(fastTrackRequest, None, "GB", "some-cgt-name")) when
          confirmCountryCodeCgt(authorisedAgent)(CountryCode("GB")) should
          thenGo(ConfirmClientCgt(fastTrackRequest.copy(knownFact = Some("GB")), None, "some-cgt-name"))
      }

      "transition to KnownFactNotMatched when country codes are not matched" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, "CGTPDRef", cgtRef.value, None)

        given(ConfirmCountryCodeCgt(fastTrackRequest, None, "GB", "some-cgt-name")) when
          confirmCountryCodeCgt(authorisedAgent)(CountryCode("IN")) should
          thenGo(KnownFactNotMatched(fastTrackRequest, None))
      }
    }

    "at ConfirmClientCgt" should {

      def hasNoPendingInvitation(arn: Arn, clientId: String, service: Service): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: Service): Future[Boolean] =
        Future.successful(false)
      def hasNoPartialAuthorisation(arn: Arn, clientId: String): Future[Boolean] =
        Future.successful(false)
      def createInvitation(arn: Arn, invitation: Invitation): Future[InvitationId] =
        Future(InvitationId("ABBTAKTMFKWU8"))
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      def getClientName(clientId: String, service: Service) = Future(Some("firstName lastName"))
      def getAgencyEmail() = Future("abc@xyz.com")

      "transition to InvitationSentPersonal" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, "CGTPDRef", cgtRef.value, None)

        given(ConfirmClientCgt(fastTrackRequest, None, "some-cgt-name")) when
          submitConfirmClientCgt(createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Service.CapitalGains, isAltItsa = false))
      }

      "transition to IdentifyCgtClient when the form is false for CGT" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, "ni", nino, dob)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)

        given(ConfirmClientCgt(fastTrackRequest, None, "some-cgt-name")) when
          submitConfirmClientCgt(createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyCgtClient(fastTrackRequest, None))
      }
    }

    "at CheckDetailsCompletePpt" should {
      "transition to ConfirmRegDatePpt" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Ppt, "EtmpRegistrationNumber", pptRef.value, None)
        val regDate = new LocalDate(2021, 1, 1)
        given(CheckDetailsComplete(fastTrackRequest, None)) when Transitions
          .checkedDetailsNoKnownFact(getCgtSubscription(), getPptSubscription(regDate))(authorisedAgent) should
          thenGo(ConfirmRegDatePpt(fastTrackRequest, None, regDate, "PPT"))
      }

      "transition to PptRefNotFound if there is no ppt subscription found" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Ppt, "EtmpRegistrationNumber", pptRef.value, None)

        given(CheckDetailsComplete(fastTrackRequest, None)) when checkedDetailsNoKnownFact(
          pptRef => Future.successful(None),
          pptRef => Future.successful(None))(authorisedAgent) should
          thenGo(PptRefNotFound(fastTrackRequest, None))
      }
    }

    "at SelectClientTypePpt" should {
      val regDate = new LocalDate(2021, 1, 1)

      def getPptSubscription(regDate: LocalDate = regDate): GetPptSubscription =
        PptRef => Future(Some(pptSubscription(regDate)))

      def hasNoPendingInvitation(arn: Arn, clientId: String, service: Service): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: Service): Future[Boolean] =
        Future.successful(false)
      def hasNoPartialAuthorisation(arn: Arn, clientId: String): Future[Boolean] =
        Future successful (false)

      def createInvitation(arn: Arn, invitation: Invitation): Future[InvitationId] =
        Future(InvitationId("ABBTAKTMFKWU8"))
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      def getClientName(clientId: String, service: Service) = Future(Some("firstName lastName"))
      def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(true))
      def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(VatKnownFactCheckOk)
      def checkPptKnownFact(pptRef: PptRef, regDate: LocalDate) = Future(true)
      def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(true))
      def getAgencyEmail() = Future("abc@xyz.com")

      "transition to ConfirmRegDatePpt" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Ppt, "EtmpRegistrationNumber", pptRef.value, None)

        given(SelectClientTypePpt(fastTrackRequest, None)) when
          selectedClientType(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(createInvitation)(getAgentLink)(
            getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(hasNoPartialAuthorisation)(isNotAltItsa)(
            legacySaRelationshipStatusNotFound)(getCgtSubscription(), getPptSubscription())(mockAppConfig)(authorisedAgent)("personal") should
          thenGo(ConfirmRegDatePpt(fastTrackRequest, None, regDate, "PPT"))
      }

      "transition to IdentifyPptClient when changing is true" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Ppt, "EtmpRegistrationNumber", pptRef.value, None)

        given(SelectClientTypePpt(fastTrackRequest, None, isChanging = true)) when
          selectedClientType(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(createInvitation)(getAgentLink)(
            getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(hasNoPartialAuthorisation)(isNotAltItsa)(
            legacySaRelationshipStatusNotFound)(getCgtSubscription("FR"), getPptSubscription())(mockAppConfig)(authorisedAgent)("personal") should
          thenGo(IdentifyPptClient(fastTrackRequest, None))
      }
    }

    "at ConfirmRegDatePpt" should {
      val regDate = new LocalDate(2021, 1, 1)

      "transition to ConfirmClientPpt when country codes are matched" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Ppt, "EtmpRegistrationNumber", pptRef.value, None)

        given(ConfirmRegDatePpt(fastTrackRequest, None, regDate, "some-ppt-name")) when
          confirmRegDatePpt(authorisedAgent)(regDate.toString("yyyy-MM-dd")) should
          thenGo(ConfirmClientPpt(fastTrackRequest, None, "some-ppt-name"))
      }

      "transition to KnownFactNotMatched when country codes are not matched" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Ppt, "EtmpRegistrationNumber", pptRef.value, None)

        given(ConfirmRegDatePpt(fastTrackRequest, None, regDate, "some-ppt-name")) when
          confirmRegDatePpt(authorisedAgent)(regDate.plusDays(1).toString("yyyy-MM-dd")) should
          thenGo(KnownFactNotMatched(fastTrackRequest, None))
      }
    }

    "at ConfirmClientPpt" should {

      def hasNoPendingInvitation(arn: Arn, clientId: String, service: Service): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: Service): Future[Boolean] =
        Future.successful(false)
      def hasNoPartialAuthorisation(arn: Arn, clientId: String): Future[Boolean] =
        Future.successful(false)
      def createInvitation(arn: Arn, invitation: Invitation): Future[InvitationId] =
        Future(InvitationId("ABBTAKTMFKWU8"))
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      def getClientName(clientId: String, service: Service) = Future(Some("firstName lastName"))
      def getAgencyEmail() = Future("abc@xyz.com")

      "transition to InvitationSentPersonal" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Ppt, "EtmpRegistrationNumber", pptRef.value, None)

        given(ConfirmClientPpt(fastTrackRequest, None, "some-ppt-name")) when
          submitConfirmClientPpt(createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Service.Ppt, isAltItsa = false))
      }

      "transition to IdentifyPptClient when the form is false for PPT" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, "ni", nino, dob)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)

        given(ConfirmClientPpt(fastTrackRequest, None, "some-ppt-name")) when
          submitConfirmClientPpt(createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyPptClient(fastTrackRequest, None))
      }
    }

    "at state CheckDetails" should {
      def hasNoPendingInvitation(arn: Arn, clientId: String, service: Service): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: Service): Future[Boolean] =
        Future.successful(false)
      def hasNoPartialAuthorisation(arn: Arn, clientId: String): Future[Boolean] =
        Future.successful(false)
      def createInvitation(arn: Arn, invitation: Invitation): Future[InvitationId] =
        Future(InvitationId("ABBTAKTMFKWU8"))
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      def getClientName(clientId: String, service: Service) = Future(Some("firstName lastName"))
      def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(true))
      def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(VatKnownFactCheckOk)
      def checkPptKnownFact(pptRef: PptRef, regDate: LocalDate) = Future(true)
      def getCgtSubscription(cgtRef: CgtRef) =
        Future(
          Some(
            CgtSubscription(
              "cgt",
              SubscriptionDetails(
                TypeOfPersonDetails("individual", Left(IndividualName("first", "last"))),
                CgtAddressDetails("19", None, None, None, "GB", None)))))
      def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(true))
      def getAgencyEmail() = Future("abc@xyz.com")

      "transition to InvitationSentPersonal if all fields are present, no pending or active invitations and known facts match for itsa" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, "ni", nino, postCode)

        given(CheckDetailsComplete(fastTrackRequest, None)) when checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(
          mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Service.MtdIt, isAltItsa = false))
      }
      "transition to IdentifyPersonalClient for ITSA when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, "ni", nino, postCode)

        given(CheckDetailsComplete(fastTrackRequest, None)) when checkedDetailsChangeInformation(authorisedAgent) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, None))
      }
      "transition to InvitationSentPersonal if all fields are present, no pending or active invitations and known facts match for irv" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, "ni", nino, dob)

        given(CheckDetailsComplete(fastTrackRequest, None)) when checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(
          mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Service.PersonalIncomeRecord, isAltItsa = false))
      }
      "transition to IdentifyPersonalClient for IRV when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, "ni", nino, dob)

        given(CheckDetailsComplete(fastTrackRequest, None)) when checkedDetailsChangeInformation(authorisedAgent) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, None))
      }
      "transition to InvitationSentPersonal if all fields are present, no pending or active invitations and known facts match for personal vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, "ni", nino, vatRegDate)

        given(CheckDetailsComplete(fastTrackRequest, None)) when checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(
          mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Service.Vat, isAltItsa = false))
      }
      "transition to IdentifyPersonalClient for Personal VAT when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, "vrn", vrn, vatRegDate)

        given(CheckDetailsComplete(fastTrackRequest, None)) when checkedDetailsChangeInformation(authorisedAgent) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, None))
      }
      "transition to InvitationSentBusiness if all fields are present, no pending or active invitations and known facts match for business vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Vat, "ni", nino, vatRegDate)

        given(CheckDetailsComplete(fastTrackRequest, None)) when checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(
          mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentBusiness("invitation/link", None, "abc@xyz.com"))
      }
      "transition to InvitationSentBusiness if all fields are present, no pending or active invitations and known facts match for Trust" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Trust, "utr", utr.value, None)

        given(CheckDetailsComplete(fastTrackRequest, None)) when checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(
          mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSentBusiness("invitation/link", None, "abc@xyz.com", Service.Trust))
      }
      "transition to IdentifyBusinessClient for Business VAT when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Vat, "vrn", vrn, vatRegDate)

        given(CheckDetailsComplete(fastTrackRequest, None)) when checkedDetailsChangeInformation(authorisedAgent) should
          thenGo(IdentifyBusinessClient(fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient when the form is false for itsa" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, "ni", nino, postCode)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)

        given(CheckDetailsComplete(fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient when the form is false for irv" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, "ni", nino, dob)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)

        given(CheckDetailsComplete(fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient when the form is false for personal vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, "ni", nino, vatRegDate)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)

        given(CheckDetailsComplete(fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient when the form is false for business vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Vat, "ni", nino, vatRegDate)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)

        given(CheckDetailsComplete(fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyBusinessClient(fastTrackRequest, None))
      }
      "transition to knownFactNotMatched when the clientIdentifier and known fact do not match for itsa" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, "ni", nino, postCode)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)
        def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(false))

        given(CheckDetailsComplete(fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(KnownFactNotMatched(fastTrackRequest, None))
      }
      "transition to knownFactNotMatched when the clientIdentifier and known fact do not match for irv" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, "ni", nino, dob)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)
        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(false))

        given(CheckDetailsComplete(fastTrackRequest, None)) when checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(
          checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(
          mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(KnownFactNotMatched(fastTrackRequest, None))
      }
      "transition to knownFactNotMatched when the clientIdentifier and known fact do not match for personal vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, "ni", nino, vatRegDate)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(VatKnownFactNotMatched)

        given(CheckDetailsComplete(fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(KnownFactNotMatched(fastTrackRequest, None))
      }

      "transition to ClientInsolvent when the clientIdentifier and known fact match for personal vat but client is insolvent" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, "ni", nino, vatRegDate)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(VatRecordClientInsolvent)

        given(CheckDetailsComplete(fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientInsolventFastTrack)
      }
      "transition to knownFactNotMatched when the clientIdentifier and known fact do not match for business vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Vat, "ni", nino, vatRegDate)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(VatKnownFactNotMatched)

        given(CheckDetailsComplete(fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(KnownFactNotMatched(fastTrackRequest, None))
      }
      "transition to ClientNotSignedUp when features alt-itsa false and the client is not enrolled for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, "ni", nino, postCode)
        def checkPostcodeMatches(nino: Nino, postcode: String) = Future(None)
        val mockAppConfig = mock(classOf[AppConfig])
        when(mockAppConfig.featuresAltItsa).thenReturn(false)

        given(CheckDetailsComplete(fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientNotSignedUp(fastTrackRequest, None))
      }

      "transition to ClientNotRegistered when features alt-itsa true and the client is not enrolled for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, "ni", nino, postCode)
        def checkPostcodeMatches(nino: Nino, postcode: String) = Future(None)
        val mockAppConfig = mock(classOf[AppConfig])
        when(mockAppConfig.featuresAltItsa).thenReturn(true)

        given(CheckDetailsComplete(fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientNotRegistered(fastTrackRequest, None))
      }

      "transition to knownFactNotMatched when the client is not enrolled for afi service - as afi clients don't need any enrolment" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, "ni", nino, dob)
        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(None)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)

        given(CheckDetailsComplete(fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(KnownFactNotMatched(fastTrackRequest, None))
      }
      "transition to ClientNotSignedUp when the client is not enrolled for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, "vrn", vrn, vatRegDate)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(VatDetailsNotFound)

        given(CheckDetailsComplete(fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientNotSignedUp(fastTrackRequest, None))
      }
      "transition to ClientNotSignedUp when the client is not enrolled for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Vat, "vrn", vrn, vatRegDate)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(VatDetailsNotFound)

        given(CheckDetailsComplete(fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientNotSignedUp(fastTrackRequest, None))
      }
      "transition to PendingInvitationExists when there is already a pending invitation for this request" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, "ni", nino, postCode)
        def hasPendingInvitation(arn: Arn, clientId: String, service: Service): Future[Boolean] =
          Future.successful(true)

        given(CheckDetailsComplete(fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasPendingInvitation)(hasNoActiveRelationship)(hasNoPartialAuthorisation)(
            isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(PendingInvitationExists(fastTrackRequest, "invitation/link", "firstName lastName", None))
      }

      "transition to ActiveAuthorisationExists when there is already an active relationship between agent and client for this service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, "ni", nino, postCode)
        def hasActiveRelationship(arn: Arn, clientId: String, service: Service): Future[Boolean] =
          Future.successful(true)

        given(CheckDetailsComplete(fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasActiveRelationship)(hasNoPartialAuthorisation)(
            isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ActiveAuthorisationExists(fastTrackRequest, None))
      }

      "transition to PartialAuthorisationExists when there is already a PartialAuth request between agent and client for ITSA" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, "ni", nino, postCode)
        def hasPartialAuthorisation(arn: Arn, clientId: String): Future[Boolean] =
          Future.successful(true)

        given(CheckDetailsComplete(fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(hasPartialAuthorisation)(
            isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(PartialAuthorisationExists(fastTrackRequest, None))
      }

      "transition to AlreadyCopiedAcross when there is a legacy mapping between agent and client for ITSA" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, "ni", nino, postCode)

        when(mockAppConfig.featuresAltItsa).thenReturn(true)

        given(CheckDetailsComplete(fastTrackRequest, None)) when
          checkedDetailsAllInformation(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription)(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusMapped)(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(AlreadyCopiedAcrossItsa)
      }

      "transition to NoPostcode when there is no known fact in the request" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, "ni", nino, None)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)

        given(CheckDetailsNoPostcode(fastTrackRequest, None)) when
          checkedDetailsNoKnownFact(getCgtSubscription, getPptSubscription())(authorisedAgent) should
          thenGo(NoPostcode(fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient for ITSA with no postcode when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, "ni", nino, None)

        given(CheckDetailsNoPostcode(fastTrackRequest, None)) when checkedDetailsChangeInformation(authorisedAgent) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient for IRV with no dob when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, "ni", nino, None)

        given(CheckDetailsNoDob(fastTrackRequest, None)) when checkedDetailsChangeInformation(authorisedAgent) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, None))
      }
      "transition to IdentifyPersonalClient for VAT with no vat reg date when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, "vrn", vrn, None)

        given(CheckDetailsNoVatRegDate(fastTrackRequest, None)) when checkedDetailsChangeInformation(authorisedAgent) should
          thenGo(IdentifyPersonalClient(fastTrackRequest, None))
      }
      "transition to SelectClientTypeVat for VAT with no client type when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.Vat, "vrn", vrn, None)

        given(CheckDetailsNoClientTypeVat(fastTrackRequest, None)) when checkedDetailsChangeInformation(authorisedAgent) should
          thenGo(SelectClientTypeVat(fastTrackRequest, None, isChanging = true))
      }
      "transition to SelectClientType when there is no client type in the request" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.MtdIt, "ni", nino, postCode)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)
        given(CheckDetailsNoClientTypeVat(fastTrackRequest, None)) when
          checkedDetailsNoClientType(authorisedAgent) should
          thenGo(SelectClientTypeVat(fastTrackRequest, None))
      }

      "transition to IdentifyCgtClient for CGT when client is UK based" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, "cgt", cgtRef.value, None)

        given(CheckDetailsComplete(fastTrackRequest, None)) when checkedDetailsChangeInformation(authorisedAgent) should
          thenGo(IdentifyCgtClient(fastTrackRequest, None))
      }
    }

    "at IdentifyClient" should {
      def hasNoPendingInvitation(arn: Arn, clientId: String, service: Service): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: Service): Future[Boolean] =
        Future.successful(false)
      def hasNoPartialAuthorisation(arn: Arn, clientId: String): Future[Boolean] =
        Future successful false

      def createInvitation(arn: Arn, invitation: Invitation): Future[InvitationId] =
        Future(InvitationId("ABBTAKTMFKWU8"))
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      def getClientName(clientId: String, service: Service) = Future(Some("firstName lastName"))
      def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(true))
      def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(VatKnownFactCheckOk)
      def checkPptKnownFact(pptRef: PptRef, regDate: LocalDate) = Future(true)
      def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(true))
      def getAgencyEmail() = Future("abc@xyz.com")
      "transition to InvitationSent for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, "ni", nino, postCode)

        given(IdentifyPersonalClient(fastTrackRequest, None)) when
          identifiedClientItsa(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription("GB"))(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(
            ItsaClient("AB123456C", "BN32TM")) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Service.MtdIt, isAltItsa = false))
      }
      "transition to InvitationSent for itsa service when alt itsa authorisation" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, "ni", nino, postCode)

        given(IdentifyPersonalClient(fastTrackRequest, None)) when
          identifiedClientItsa(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription("GB"))(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(isAltItsa)(
            hasNoPartialAuthorisation)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(ItsaClient("AB123456C", "BN32TM")) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Service.MtdIt, isAltItsa = true))
      }

      "transition to InvitationSent for irv service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, "ni", nino, dob)

        given(IdentifyPersonalClient(fastTrackRequest, None)) when
          identifiedClientIrv(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription("GB"))(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(
            IrvClient("AB123456C", "1990-10-10")) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Service.PersonalIncomeRecord, isAltItsa = false))
      }
      "transition to InvitationSent for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, "vrn", vrn, vatRegDate)

        given(IdentifyPersonalClient(fastTrackRequest, None)) when
          identifiedClientVat(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription("GB"))(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(
            VatClient("1234567", "2010-10-10")) should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Service.Vat, isAltItsa = false))
      }
      "transition to InvitationSent for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Vat, "vrn", vrn, vatRegDate)

        given(IdentifyBusinessClient(fastTrackRequest, None)) when
          identifiedClientVat(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription("GB"))(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(
            VatClient("1234567", "2010-10-10")) should
          thenGo(InvitationSentBusiness("invitation/link", None, "abc@xyz.com"))
      }
      "transition to client type for no client type vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.Vat, "vrn", vrn, vatRegDate)
        val newVrn = "1234567"
        val newVatRegDate = "2010-10-10"

        given(IdentifyNoClientTypeClient(fastTrackRequest, None)) when
          identifiedClientVat(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription("GB"))(
            createInvitation)(getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(
            hasNoPartialAuthorisation)(isNotAltItsa)(legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)(
            VatClient(newVrn, newVatRegDate)) should
          thenGo(SelectClientTypeVat(fastTrackRequest.copy(clientIdentifier = newVrn, knownFact = Some(newVatRegDate)), None))
      }
    }
    "at MoreDetails" should {
      def hasNoPendingInvitation(arn: Arn, clientId: String, service: Service): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: Service): Future[Boolean] =
        Future.successful(false)
      def hasNoPartialAuthorisation(arn: Arn, clientId: String): Future[Boolean] =
        Future successful false

      def createInvitation(arn: Arn, invitation: Invitation): Future[InvitationId] =
        Future(InvitationId("ABBTAKTMFKWU8"))
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      def getClientName(clientId: String, service: Service) = Future(Some("firstName lastName"))
      def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(true))
      def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(VatKnownFactCheckOk)
      def checkPptKnownFact(pptRef: PptRef, regDate: LocalDate) = Future(true)
      def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(true))
      def getAgencyEmail() = Future("abc@xyz.com")
      "transition to InvitationSent for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, "ni", nino, postCode)

        given(NoPostcode(fastTrackRequest, None)) when
          moreDetailsItsa(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription("GB"))(createInvitation)(
            getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(hasNoPartialAuthorisation)(isNotAltItsa)(
            legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)("BN114AW") should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Service.MtdIt, isAltItsa = false))
      }
      "transition to InvitationSent for irv service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, "ni", nino, dob)

        given(NoDob(fastTrackRequest, None)) when
          moreDetailsIrv(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription("GB"))(createInvitation)(
            getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(hasNoPartialAuthorisation)(isNotAltItsa)(
            legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)("1991-10-10") should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Service.PersonalIncomeRecord, isAltItsa = false))
      }
      "transition to InvitationSent for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, "vrn", vrn, vatRegDate)

        given(NoVatRegDate(fastTrackRequest, None)) when
          moreDetailsVat(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription("GB"))(createInvitation)(
            getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(hasNoPartialAuthorisation)(isNotAltItsa)(
            legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)("2011-10-10") should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Service.Vat, isAltItsa = false))
      }
      "transition to InvitationSent for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Vat, "vrn", vrn, vatRegDate)

        given(NoVatRegDate(fastTrackRequest, None)) when
          moreDetailsVat(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(getCgtSubscription("GB"))(createInvitation)(
            getAgentLink)(getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(hasNoPartialAuthorisation)(isNotAltItsa)(
            legacySaRelationshipStatusNotFound)(mockAppConfig)(authorisedAgent)("2011-10-10") should
          thenGo(InvitationSentBusiness("invitation/link", None, "abc@xyz.com"))
      }
    }
    "at SelectClientType" should {
      def hasNoPendingInvitation(arn: Arn, clientId: String, service: Service): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: Service): Future[Boolean] =
        Future.successful(false)
      def hasNoPartialAuthorisation(arn: Arn, clientId: String): Future[Boolean] =
        Future successful false
      def createInvitation(arn: Arn, invitation: Invitation): Future[InvitationId] =
        Future(InvitationId("ABBTAKTMFKWU8"))
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      def getClientName(clientId: String, service: Service) = Future(Some("firstName lastName"))
      def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(true))
      def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(VatKnownFactCheckOk)
      def checkPptKnownFact(pptRef: PptRef, regDate: LocalDate) = Future(true)
      def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(true))
      def getAgencyEmail() = Future("abc@xyz.com")

      "transition to InvitationSent for vat service when there is a known fact present" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.Vat, "vrn", vrn, vatRegDate)

        given(SelectClientTypeVat(fastTrackRequest, None)) when
          selectedClientType(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(createInvitation)(getAgentLink)(
            getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(hasNoPartialAuthorisation)(isNotAltItsa)(
            legacySaRelationshipStatusNotFound)(getCgtSubscription(), getPptSubscription())(mockAppConfig)(authorisedAgent)("personal") should
          thenGo(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Service.Vat, isAltItsa = false))
      }
      "transition to MoreDetails for vat service when there is no known fact" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.Vat, "vrn", vrn, None)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)

        given(SelectClientTypeVat(fastTrackRequest, None)) when
          selectedClientType(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(createInvitation)(getAgentLink)(
            getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(hasNoPartialAuthorisation)(isNotAltItsa)(
            legacySaRelationshipStatusNotFound)(getCgtSubscription(), getPptSubscription())(mockAppConfig)(authorisedAgent)("personal") should
          thenGo(NoVatRegDate(fastTrackRequest.copy(clientType = Some(Personal)), None))
      }

      "transition to IdentifyClientVat when changing answers" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.Vat, "vrn", vrn, None)
        val originalFastTrackRequest = aFastTrackRequestWithDiffParams(fastTrackRequest)

        given(SelectClientTypeVat(fastTrackRequest, None, isChanging = true)) when
          selectedClientType(checkPostcodeMatches)(checkDobMatches)(checkRegDateMatches)(checkPptKnownFact)(createInvitation)(getAgentLink)(
            getClientName)(getAgencyEmail)(hasNoPendingInvitation)(hasNoActiveRelationship)(hasNoPartialAuthorisation)(isNotAltItsa)(
            legacySaRelationshipStatusNotFound)(getCgtSubscription(), getPptSubscription())(mockAppConfig)(authorisedAgent)("personal") should
          thenGo(IdentifyPersonalClient(fastTrackRequest.copy(clientType = Some(Personal)), None))
      }
    }

    "at KnownFactNotMatched, calling tryAgainNotMatchedKnownFact" when {
      "fast track request is for MTD-VAT and client type is missing should go to SelectClientTypeVat" in {
        val originalFtr = AgentFastTrackRequest(
          clientType = Some(Personal),
          service = Service.Vat,
          "vrn",
          vrn,
          knownFact = Some("2001-01-01")
        )

        val ftr = AgentFastTrackRequest(
          clientType = None,
          service = Service.Vat,
          "vrn",
          vrn,
          knownFact = None
        )

        given(
          KnownFactNotMatched(
            fastTrackRequest = ftr,
            continueUrl = None
          )) when
          tryAgainNotMatchedKnownFact(authorisedAgent) should
          thenGo(
            SelectClientTypeVat(
              fastTrackRequest = ftr,
              continueUrl = None
            ))
      }

      "fast track request is for MTD-VAT with client type should go to IdentifyPersonalClient" in {
        val originalFtr = AgentFastTrackRequest(
          clientType = Some(Personal),
          service = Service.Vat,
          "vrn",
          vrn,
          knownFact = Some("2001-01-01")
        )

        val ftr = AgentFastTrackRequest(
          clientType = Some(Personal),
          service = Service.Vat,
          "vrn",
          vrn,
          knownFact = None
        )

        given(
          KnownFactNotMatched(
            fastTrackRequest = ftr,
            continueUrl = None
          )) when
          tryAgainNotMatchedKnownFact(authorisedAgent) should
          thenGo(
            IdentifyPersonalClient(
              fastTrackRequest = ftr,
              continueUrl = None
            ))
      }

      "fast track request is for MTD-IT service (and clientType was personal)" in {
        val ftr = AgentFastTrackRequest(
          clientType = Some(Personal),
          service = Service.MtdIt,
          "ni",
          nino,
          knownFact = None
        )

        given(
          KnownFactNotMatched(
            fastTrackRequest = ftr,
            continueUrl = None
          )) when
          tryAgainNotMatchedKnownFact(authorisedAgent) should
          thenGo(
            IdentifyPersonalClient(
              fastTrackRequest = ftr,
              continueUrl = None
            ))
      }

      "original request was for IRV service (and clientType was personal)" in {
        val originalFtr = AgentFastTrackRequest(
          clientType = Some(Personal),
          service = Service.PersonalIncomeRecord,
          "ni",
          nino,
          knownFact = Some("1990-09-09")
        )

        val ftr = AgentFastTrackRequest(
          clientType = Some(Personal),
          service = Service.PersonalIncomeRecord,
          "ni",
          nino,
          knownFact = None
        )

        given(
          KnownFactNotMatched(
            fastTrackRequest = ftr,
            continueUrl = None
          )) when
          tryAgainNotMatchedKnownFact(authorisedAgent) should
          thenGo(
            IdentifyPersonalClient(
              fastTrackRequest = ftr,
              continueUrl = None
            ))
      }

      "original request was for a Trust service" when {
        val completedTrustFastTrack = AgentFastTrackRequest(
          clientType = Some(ClientType.Business),
          service = Service.Trust,
          "utr",
          utr.value,
          knownFact = None
        )

        "trust not found for a given utr, should transition to IdentifyTrustClient " in {
          val TrustNotFoundState = TrustNotFound(
            fastTrackRequest = completedTrustFastTrack,
            continueUrl = None
          )

          given(TrustNotFoundState) when
            tryAgainNotMatchedKnownFact(authorisedAgent) should
            thenGo(IdentifyTrustClient(completedTrustFastTrack, None))
        }
      }
    }
  }

  def aFastTrackRequestWithDiffParams(toThisFastTrackRequest: AgentFastTrackRequest) =
    toThisFastTrackRequest.clientType match {
      case Some(_) => toThisFastTrackRequest.copy(clientType = None)
      case None    => toThisFastTrackRequest.copy(clientType = Some(Personal))
    }
}
