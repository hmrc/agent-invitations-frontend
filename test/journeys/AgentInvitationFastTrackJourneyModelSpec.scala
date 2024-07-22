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

import org.mockito.Mockito.{mock, when}
import org.scalatest.BeforeAndAfter
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.{start => _, _}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.TransitionEffects._
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{Business, Personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentFastTrackRequest, _}
import uk.gov.hmrc.agentinvitationsfrontend.models.KnownFactResult._
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.http.HeaderCarrier
import support.UnitSpec
import play.api.test.Helpers._
import uk.gov.hmrc.agentmtdidentifiers.model.SuspensionDetails

import java.time.LocalDate
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
  val availableServices: Set[Service] = Set(Service.PersonalIncomeRecord, Service.MtdIt, Service.Vat, Service.Ppt, Service.Cbc, Service.Pillar2)
  val nino = Nino("AB123456A")
  val postCode = "BN114AW"
  val vrn = Vrn("123456")
  val cgtPostcode = "BN13 1FN"
  val vatRegDate = "2010-10-10"
  val pptRegDate = "2022-10-10"
  val dob = "1990-10-10"
  val cbcEmail = "contact@business.com"
  val utr = Utr("1977030537")
  val urn = Urn("XXTRUST10010010")
  val pptRef = PptRef("XAPPT000012345")
  val cgtRef = CgtRef("XMCGTP123456789")
  val cbcId = CbcId("XACBC0123456789")

  val tpd = TypeOfPersonDetails("Individual", Left(IndividualName("firstName", "lastName")))

  def cgtAddressDetails(countryCode: String = "GB") =
    CgtAddressDetails("line1", Some("line2"), Some("line2"), Some("line2"), countryCode, Some(cgtPostcode))

  def cgtSubscription(countryCode: String = "GB") =
    CgtSubscription("CGT", SubscriptionDetails(tpd, cgtAddressDetails(countryCode)))

  def pptSubscription(regDate: LocalDate) =
    PptSubscription("PPT", regDate, None)

  val notSuspended: GetSuspensionDetails = () => Future.successful(SuspensionDetails(false, None))

  def hasNoLegacyMapping(arn: Arn, clientId: String): Future[Boolean] =
    Future.successful(false)

  def hasNoPendingInvitation(arn: Arn, clientId: String, service: Service): Future[Boolean] =
    Future.successful(false)

  def hasNoActiveRelationship(arn: Arn, clientId: String, service: Service): Future[Boolean] =
    Future.successful(false)

  def hasNoPartialAuthorisation(arn: Arn, clientId: String): Future[Boolean] =
    Future successful false

  def isNotAltItsa(arn: Arn, clientId: String): Future[Boolean] = Future.successful(false)

  def isAltItsa(arn: Arn, clientId: String): Future[Boolean] = Future.successful(true)

  def legacySaRelationshipStatusMapped(arn: Arn, clientId: String): Future[LegacySaRelationshipResult] =
    Future.successful(LegacySaRelationshipFoundAndMapped)

  def legacySaRelationshipStatusNotMapped(arn: Arn, clientId: String): Future[LegacySaRelationshipResult] =
    Future.successful(LegacySaRelationshipFoundNotMapped)

  def legacySaRelationshipStatusNotFound(arn: Arn, clientId: String): Future[LegacySaRelationshipResult] =
    Future.successful(LegacySaRelationshipNotFound)

  def noCgtRefFound: GetCgtSubscription = cgtRef => Future(None)

  def noPptRefFound: GetPptSubscription = pptRef => Future(None)

  def getCgtSubscription(countryCode: String = "GB"): GetCgtSubscription =
    CgtRef => Future(Some(cgtSubscription(countryCode)))

  def getPptSubscription(regDate: LocalDate = LocalDate.parse("2021-01-01")): GetPptSubscription =
    PptRef => Future(Some(pptSubscription(regDate)))

  def getClientName(clientId: String, service: Service): Future[Option[String]] = Future(Some("firstName lastName"))

  def getAgentLink(arn: Arn, clientType: Option[ClientType]): Future[String] = Future("invitation/link")

  def getAgencyEmail(): Future[String] = Future("abc@xyz.com")

  def knownFactCheckReturns(result: KnownFactResult): CheckKnownFact = _ => Future.successful(result)
  def knownFactCheckPasses = knownFactCheckReturns(Pass)
  def knownFactCheckDoesntMatch = knownFactCheckReturns(Fail(NotMatched))
  def knownFactCheckClientNotFound = knownFactCheckReturns(Fail(NotFound))

  def createInvitation(arn: Arn, invitation: Invitation): Future[InvitationId] =
    Future(InvitationId("ABBTAKTMFKWU8"))

  // This is the default behaviour. Modify as needed in individual tests
  val transitions = Transitions(
    getSuspensionDetails = notSuspended,
    hasPendingInvitationsFor = hasNoPendingInvitation,
    hasActiveRelationshipFor = hasNoActiveRelationship,
    hasPartialAuthorisationFor = hasNoPartialAuthorisation,
    legacySaRelationshipStatusFor = legacySaRelationshipStatusNotFound,
    getClientName = getClientName,
    getAgentLink = getAgentLink,
    getAgencyEmail = getAgencyEmail,
    getCgtSubscription = noCgtRefFound,
    getPptSubscription = noPptRefFound,
    getCbcSubscription = _ => Future(None),
    createInvitation = createInvitation,
    isAltItsa = isNotAltItsa,
    checkKnownFact = knownFactCheckClientNotFound
  )

  val transitionsWithKnownFactsPassing = transitions.copy(
    checkKnownFact = knownFactCheckPasses,
    getCgtSubscription = _ => Future(Some(cgtSubscription("GB"))),
    getCbcSubscription = _ => Future(Some(SimpleCbcSubscription(Some("Trader Ltd"), Seq.empty, true)))
  )

  val mockAppConfig = mock(classOf[AppConfig])

  before {
    when(mockAppConfig.featuresAltItsa).thenReturn(true)
  }

  "AgentInvitationFastTrackJourneyService" when {

    "at state Prologue" should {
      "transition to CheckDetails when all required fields are present for itsa service" in {

        val fastTrackRequest = AgentFastTrackRequest(None, Service.MtdIt, nino, Some(postCode))

        given(Prologue(None, None)) when transitions.start(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetails(
              fastTrackRequest = fastTrackRequest.copy(clientType = Some(Personal)),
              None
            )
          )
      }
      "transition to CheckDetailsNoPostcode when the postcode is missing for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.MtdIt, nino, None)

        given(Prologue(None, None)) when transitions.start(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetails(
              fastTrackRequest = fastTrackRequest.copy(clientType = Some(Personal)),
              None
            )
          )
      }
      "transition to CheckDetails when all required fields are present for irv service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.PersonalIncomeRecord, nino, Some(dob))

        given(Prologue(None, None)) when transitions.start(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetails(
              fastTrackRequest = fastTrackRequest.copy(clientType = Some(Personal)),
              None
            )
          )
      }
      "transition to CheckDetailsNoDob when there is no dob for irv service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.PersonalIncomeRecord, nino, None)

        given(Prologue(None, None)) when transitions.start(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetails(
              fastTrackRequest = fastTrackRequest.copy(clientType = Some(Personal)),
              None
            )
          )
      }

      "transition to AgentSuspended when there are all the required fields are present for irv service but the agent has been suspended" +
        "for this service" in {
          val fastTrackRequest =
            AgentFastTrackRequest(None, Service.MtdIt, nino, None)

          def suspendedForIT() = Future.successful(SuspensionDetails(suspensionStatus = true, Some(Set("ITSA"))))

          given(Prologue(None, None)) when transitions
            .copy(getSuspensionDetails = suspendedForIT)
            .start(Some("continue/url"))(authorisedAgent)(fastTrackRequest) should
            thenGo(
              SuspendedAgent(Service.MtdIt, Some("continue/url"))
            )
        }

      "transition to CheckDetails when all required fields are present for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, nino, Some(vatRegDate))

        given(Prologue(None, None)) when transitions.start(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetails(
              fastTrackRequest = fastTrackRequest,
              None
            )
          )
      }
      "transition to CheckDetailsNoVatRegDate when there is no vat reg date for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, nino, None)

        given(Prologue(None, None)) when transitions.start(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetails(
              fastTrackRequest = fastTrackRequest,
              None
            )
          )
      }
      "transition to CheckDetails when all required fields are present for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Vat, nino, Some(vatRegDate))

        given(Prologue(None, None)) when transitions.start(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetails(
              fastTrackRequest = fastTrackRequest,
              None
            )
          )
      }
      "transition to CheckDetailsNoVatRegDate when there is no vat reg date for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Vat, nino, None)

        given(Prologue(None, None)) when transitions.start(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetails(
              fastTrackRequest = fastTrackRequest,
              None
            )
          )
      }
      "transition to CheckDetailsNoClientTypeVat when there is no client type for vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.Vat, nino, None)

        given(Prologue(None, None)) when transitions.start(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetails(
              fastTrackRequest = fastTrackRequest,
              None
            )
          )
      }

      "transition to AgentSuspended when there are all the required fields are present for vat service but the agent has been suspended" +
        "for this service" in {
          val fastTrackRequest =
            AgentFastTrackRequest(None, Service.Vat, nino, None)

          def suspendedForVat() = Future.successful(SuspensionDetails(suspensionStatus = true, Some(Set("VATC"))))

          given(Prologue(None, None)) when transitions
            .copy(getSuspensionDetails = suspendedForVat)
            .start(Some("continue/url"))(authorisedAgent)(fastTrackRequest) should
            thenGo(
              SuspendedAgent(Service.Vat, Some("continue/url"))
            )
        }

      "transition to CheckDetails when there are all the required fields are present for a Trust service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(ClientType.Trust), Service.Trust, utr, None)

        given(Prologue(None, None)) when transitions.start(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetails(
              fastTrackRequest = fastTrackRequest,
              None
            )
          )
      }

      "transition to CheckDetails when there are all the required fields are present for a TrustNT service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(ClientType.Trust), Service.TrustNT, urn, None)

        given(Prologue(None, None)) when transitions.start(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetails(
              fastTrackRequest = fastTrackRequest,
              None
            )
          )
      }

      "transition to CheckDetails when there are all the required fields are present for a CGT service" in {
        val fastTrackRequest =
          AgentFastTrackRequest(Some(ClientType.Business), Service.CapitalGains, cgtRef, None)

        given(Prologue(None, None)) when transitions.start(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetails(
              fastTrackRequest = fastTrackRequest,
              None
            )
          )
      }

      "transition to CheckDetails when there are all the required fields are present for a PPT service" in {
        val fastTrackRequest =
          AgentFastTrackRequest(Some(ClientType.Business), Service.Ppt, pptRef, None)

        given(Prologue(None, None)) when transitions.start(None)(authorisedAgent)(fastTrackRequest) should
          thenGo(
            CheckDetails(
              fastTrackRequest = fastTrackRequest,
              None
            )
          )
      }

      "transition to AgentSuspended when there are all the required fields are present for Trust service but the agent has been suspended" +
        "for this service" in {
          val fastTrackRequest =
            AgentFastTrackRequest(Some(ClientType.Business), Service.CapitalGains, cgtRef, None)

          def suspendedForTrust() = Future.successful(SuspensionDetails(suspensionStatus = true, Some(Set("CGT"))))

          given(Prologue(None, None)) when transitions
            .copy(getSuspensionDetails = suspendedForTrust)
            .start(Some("continue/url"))(authorisedAgent)(fastTrackRequest) should
            thenGo(
              SuspendedAgent(Service.CapitalGains, Some("continue/url"))
            )
        }

      "transition to AgentSuspended when there are all the required fields are present for PPT service but the agent has been suspended" +
        "for this service" in {
          val fastTrackRequest =
            AgentFastTrackRequest(Some(ClientType.Business), Service.Ppt, pptRef, None)

          def suspendedForTrust() = Future.successful(SuspensionDetails(suspensionStatus = true, Some(Set("PPT"))))

          given(Prologue(None, None)) when transitions
            .copy(getSuspensionDetails = suspendedForTrust)
            .start(Some("continue/url"))(authorisedAgent)(fastTrackRequest) should
            thenGo(
              SuspendedAgent(Service.Ppt, Some("continue/url"))
            )
        }
    }

    "at CheckDetails" should {
      "transition to transitions.confirmPostcodeCgt for CGT if client is UK based" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, cgtRef, None)

        given(CheckDetails(fastTrackRequest, None)) when transitions
          .copy(getCgtSubscription = getCgtSubscription("GB"))
          .checkedDetailsNoKnownFact(authorisedAgent) should
          thenGo(ConfirmPostcodeCgt(fastTrackRequest, None, Some(cgtPostcode), "firstName lastName"))
      }

      "transition to transitions.confirmCountryCodeCgt for CGT if client is not UK based" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, cgtRef, None)

        given(CheckDetails(fastTrackRequest, None)) when transitions
          .copy(getCgtSubscription = getCgtSubscription("FR"))
          .checkedDetailsNoKnownFact(authorisedAgent) should
          thenGo(ConfirmCountryCodeCgt(fastTrackRequest, None, "FR", "firstName lastName"))
      }

      "transition to ClientNotFound if there is no cgt subscription found" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, cgtRef, None)

        given(CheckDetails(fastTrackRequest, None)) when transitions.checkedDetailsNoKnownFact(authorisedAgent) should
          thenGo(ClientNotFound(fastTrackRequest, None))
      }
    }

    "at IdentifyCgtClient" should {

      "transition to transitions.confirmPostcodeCgt" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, cgtRef, None)

        given(IdentifyClient(fastTrackRequest, None)) when transitions
          .copy(getCgtSubscription = getCgtSubscription())
          .identifyCgtClient(authorisedAgent)(cgtRef) should
          thenGo(ConfirmPostcodeCgt(fastTrackRequest, None, Some(cgtPostcode), "firstName lastName"))
      }
    }

    "at SelectClientTypeCgt" should {

      "transition to transitions.confirmPostcodeCgt when CGT client is a UK based client" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, cgtRef, None)

        given(SelectClientType(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing
            .copy(getCgtSubscription = getCgtSubscription())
            .selectedClientType(mockAppConfig)(authorisedAgent)("personal") should
          thenGo(ConfirmPostcodeCgt(fastTrackRequest, None, Some(cgtPostcode), "firstName lastName"))
      }

      "transition to transitions.confirmCountryCodeCgt when CGT client is a non UK based client" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, cgtRef, None)

        given(SelectClientType(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing
            .copy(getCgtSubscription = getCgtSubscription("FR"))
            .selectedClientType(mockAppConfig)(authorisedAgent)("personal") should
          thenGo(ConfirmCountryCodeCgt(fastTrackRequest, None, "FR", "firstName lastName"))
      }

      "transition to IdentifyCgtClient when changing is true" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, cgtRef, None)

        given(SelectClientType(fastTrackRequest, None, isChanging = true)) when
          transitionsWithKnownFactsPassing
            .copy(getCgtSubscription = getCgtSubscription("FR"))
            .selectedClientType(mockAppConfig)(authorisedAgent)("personal") should
          thenGo(IdentifyClient(fastTrackRequest, None))
      }
    }

    "at transitions.confirmPostcodeCgt" should {
      "transition to ConfirmClientCgt when postcodes are matched for a UK client" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, cgtRef, None)

        given(ConfirmPostcodeCgt(fastTrackRequest, None, Some(cgtPostcode), "some-cgt-name")) when
          transitions.confirmPostcodeCgt(authorisedAgent)(Postcode(cgtPostcode)) should
          thenGo(ConfirmClientCgt(fastTrackRequest.copy(knownFact = Some(cgtPostcode)), None, "some-cgt-name"))
      }

      "transition to ClientNotFound when postcodes are not matched for a UK client" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, cgtRef, None)

        given(ConfirmPostcodeCgt(fastTrackRequest, None, Some(cgtPostcode), "some-cgt-name")) when
          transitions.confirmPostcodeCgt(authorisedAgent)(Postcode("BN13 1XX")) should
          thenGo(ClientNotFound(fastTrackRequest, None))
      }
    }

    "at transitions.confirmCountryCodeCgt" should {
      "transition to ConfirmClientCgt when country codes are matched" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, cgtRef, None)

        given(ConfirmCountryCodeCgt(fastTrackRequest, None, "GB", "some-cgt-name")) when
          transitions.confirmCountryCodeCgt(authorisedAgent)(CountryCode("GB")) should
          thenGo(ConfirmClientCgt(fastTrackRequest.copy(knownFact = Some("GB")), None, "some-cgt-name"))
      }

      "transition to ClientNotFound when country codes are not matched" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, cgtRef, None)

        given(ConfirmCountryCodeCgt(fastTrackRequest, None, "GB", "some-cgt-name")) when
          transitions.confirmCountryCodeCgt(authorisedAgent)(CountryCode("IN")) should
          thenGo(ClientNotFound(fastTrackRequest, None))
      }
    }

    "at ConfirmClientCgt" should {

      "transition to InvitationSentPersonal" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, cgtRef, None)

        given(ConfirmClientCgt(fastTrackRequest, None, "some-cgt-name")) when
          transitions.submitConfirmClientCgt(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Service.CapitalGains, isAltItsa = Some(false)))
      }

      "transition to IdentifyCgtClient when the form is false for CGT" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, nino, Some(dob))

        given(ConfirmClientCgt(fastTrackRequest, None, "some-cgt-name")) when
          transitions.submitConfirmClientCgt(mockAppConfig)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyClient(fastTrackRequest, None))
      }
    }

    "at CheckDetails for PPT" should {
      "transition to ConfirmRegDatePpt" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Ppt, pptRef, None)
        val regDate = LocalDate.parse("2021-01-01")
        given(CheckDetails(fastTrackRequest, None)) when transitions
          .copy(getPptSubscription = getPptSubscription(regDate))
          .checkedDetailsNoKnownFact(authorisedAgent) should
          thenGo(ConfirmRegDatePpt(fastTrackRequest, None, regDate, "PPT"))
      }

      "transition to ClientNotFound if there is no ppt subscription found" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Ppt, pptRef, None)

        given(CheckDetails(fastTrackRequest, None)) when transitions.checkedDetailsNoKnownFact(authorisedAgent) should
          thenGo(ClientNotFound(fastTrackRequest, None))
      }
    }

    "at SelectClientTypePpt" should {
      val regDate = LocalDate.parse("2021-01-01")

      def getPptSubscription(regDate: LocalDate = regDate): GetPptSubscription =
        PptRef => Future(Some(pptSubscription(regDate)))

      "transition to ConfirmRegDatePpt" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Ppt, pptRef, None)

        given(SelectClientType(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing
            .copy(getPptSubscription = getPptSubscription())
            .selectedClientType(mockAppConfig)(authorisedAgent)("personal") should
          thenGo(ConfirmRegDatePpt(fastTrackRequest, None, regDate, "PPT"))
      }

      "transition to IdentifyClient when changing is true" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Ppt, pptRef, None)

        given(SelectClientType(fastTrackRequest, None, isChanging = true)) when
          transitionsWithKnownFactsPassing
            .copy(getPptSubscription = getPptSubscription())
            .selectedClientType(mockAppConfig)(authorisedAgent)("personal") should
          thenGo(IdentifyClient(fastTrackRequest, None))
      }
    }

    "at ConfirmRegDatePpt" should {
      val regDate = LocalDate.parse("2021-01-01")

      "transition to ConfirmClientPpt when country codes are matched" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Ppt, pptRef, None)

        given(ConfirmRegDatePpt(fastTrackRequest, None, regDate, "some-ppt-name")) when
          transitions.confirmRegDatePpt(authorisedAgent)(regDate.toString) should
          thenGo(ConfirmClientPpt(fastTrackRequest, None, "some-ppt-name"))
      }

      "transition to ClientNotFound when country codes are not matched" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Ppt, pptRef, None)

        given(ConfirmRegDatePpt(fastTrackRequest, None, regDate, "some-ppt-name")) when
          transitions.confirmRegDatePpt(authorisedAgent)(regDate.plusDays(1).toString) should
          thenGo(ClientNotFound(fastTrackRequest, None))
      }
    }

    "at ConfirmClientPpt" should {

      "transition to InvitationSent" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Ppt, pptRef, None)

        given(ConfirmClientPpt(fastTrackRequest, None, "some-ppt-name")) when
          transitions.submitConfirmClientPpt(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Service.Ppt, isAltItsa = Some(false)))
      }

      "transition to IdentifyClient when the form is false for PPT" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, nino, Some(dob))

        given(ConfirmClientPpt(fastTrackRequest, None, "some-ppt-name")) when
          transitions.submitConfirmClientPpt(mockAppConfig)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyClient(fastTrackRequest, None))
      }
    }

    "at state CheckDetails" should {

      "transition to InvitationSent if all fields are present, no pending or active invitations and known facts match for itsa" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, Some(postCode))

        given(CheckDetails(fastTrackRequest, None)) when transitionsWithKnownFactsPassing.checkedDetailsAllInformation(mockAppConfig)(
          authorisedAgent
        )(Confirmation(true)) should
          thenGo(InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Service.MtdIt, isAltItsa = Some(false)))
      }
      "transition to IdentifyClient for ITSA when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, Some(postCode))

        given(CheckDetails(fastTrackRequest, None)) when transitions.checkedDetailsChangeInformation(authorisedAgent) should
          thenGo(IdentifyClient(fastTrackRequest, None))
      }
      "transition to InvitationSent if all fields are present, no pending or active invitations and known facts match for irv" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, nino, Some(dob))

        given(CheckDetails(fastTrackRequest, None)) when transitionsWithKnownFactsPassing.checkedDetailsAllInformation(mockAppConfig)(
          authorisedAgent
        )(Confirmation(true)) should
          thenGo(InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Service.PersonalIncomeRecord, isAltItsa = Some(false)))
      }
      "transition to IdentifyClient for IRV when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, nino, Some(dob))

        given(CheckDetails(fastTrackRequest, None)) when transitions.checkedDetailsChangeInformation(authorisedAgent) should
          thenGo(IdentifyClient(fastTrackRequest, None))
      }
      "transition to InvitationSent if all fields are present, no pending or active invitations and known facts match for personal vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, vrn, Some(vatRegDate))

        given(CheckDetails(fastTrackRequest, None)) when transitionsWithKnownFactsPassing.checkedDetailsAllInformation(mockAppConfig)(
          authorisedAgent
        )(Confirmation(true)) should
          thenGo(InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Service.Vat, isAltItsa = Some(false)))
      }
      "transition to IdentifyClient for Personal VAT when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, vrn, Some(vatRegDate))

        given(CheckDetails(fastTrackRequest, None)) when transitions.checkedDetailsChangeInformation(authorisedAgent) should
          thenGo(IdentifyClient(fastTrackRequest, None))
      }
      "transition to InvitationSentBusiness if all fields are present, no pending or active invitations and known facts match for business vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Vat, vrn, Some(vatRegDate))

        given(CheckDetails(fastTrackRequest, None)) when transitionsWithKnownFactsPassing.checkedDetailsAllInformation(mockAppConfig)(
          authorisedAgent
        )(Confirmation(true)) should
          thenGo(InvitationSent(ClientType.Business, "invitation/link", None, "abc@xyz.com", Service.Vat))
      }
      "transition to InvitationSentBusiness if all fields are present, no pending or active invitations and known facts match for Trust" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Trust, utr, None)

        given(CheckDetails(fastTrackRequest, None)) when transitionsWithKnownFactsPassing.checkedDetailsAllInformation(mockAppConfig)(
          authorisedAgent
        )(Confirmation(true)) should
          thenGo(InvitationSent(ClientType.Business, "invitation/link", None, "abc@xyz.com", Service.Trust))
      }
      "transition to IdentifyBusinessClient for Business VAT when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Vat, vrn, Some(vatRegDate))

        given(CheckDetails(fastTrackRequest, None)) when transitions.checkedDetailsChangeInformation(authorisedAgent) should
          thenGo(IdentifyClient(fastTrackRequest, None))
      }
      "transition to IdentifyClient when the form is false for itsa" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, Some(postCode))

        given(CheckDetails(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing.checkedDetailsAllInformation(mockAppConfig)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyClient(fastTrackRequest, None))
      }
      "transition to IdentifyClient when the form is false for irv" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, nino, Some(dob))

        given(CheckDetails(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing.checkedDetailsAllInformation(mockAppConfig)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyClient(fastTrackRequest, None))
      }
      "transition to IdentifyClient when the form is false for personal vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, nino, Some(vatRegDate))

        given(CheckDetails(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing.checkedDetailsAllInformation(mockAppConfig)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyClient(fastTrackRequest, None))
      }
      "transition to IdentifyClient when the form is false for business vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Vat, nino, Some(vatRegDate))

        given(CheckDetails(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing.checkedDetailsAllInformation(mockAppConfig)(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyClient(fastTrackRequest, None))
      }
      "transition to ClientNotFound when the clientIdentifier and known fact do not match for itsa" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, Some(postCode))

        given(CheckDetails(fastTrackRequest, None)) when
          transitions
            .copy(checkKnownFact = knownFactCheckDoesntMatch)
            .checkedDetailsAllInformation(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientNotFound(fastTrackRequest, None))
      }
      "transition to ClientNotFound when the clientIdentifier and known fact do not match for irv" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, nino, Some(dob))

        given(CheckDetails(fastTrackRequest, None)) when transitions
          .copy(checkKnownFact = knownFactCheckDoesntMatch)
          .checkedDetailsAllInformation(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientNotFound(fastTrackRequest, None))
      }
      "transition to ClientNotFound when the clientIdentifier and known fact do not match for personal vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, nino, Some(vatRegDate))

        given(CheckDetails(fastTrackRequest, None)) when
          transitions
            .copy(checkKnownFact = knownFactCheckDoesntMatch)
            .checkedDetailsAllInformation(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientNotFound(fastTrackRequest, None))
      }

      "transition to ClientInsolvent when the clientIdentifier and known fact match for personal vat but client is insolvent" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, nino, Some(vatRegDate))

        given(CheckDetails(fastTrackRequest, None)) when
          transitions
            .copy(checkKnownFact = knownFactCheckReturns(Fail(VatClientInsolvent)))
            .checkedDetailsAllInformation(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientInsolventFastTrack)
      }
      "transition to ClientNotFound when the clientIdentifier and known fact do not match for business vat" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Vat, nino, Some(vatRegDate))

        given(CheckDetails(fastTrackRequest, None)) when
          transitions
            .copy(checkKnownFact = knownFactCheckDoesntMatch)
            .checkedDetailsAllInformation(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientNotFound(fastTrackRequest, None))
      }
      "transition to ClientNotSignedUp when features alt-itsa false and the client is not enrolled for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, Some(postCode))
        val mockAppConfig = mock(classOf[AppConfig])
        when(mockAppConfig.featuresAltItsa).thenReturn(false)

        given(CheckDetails(fastTrackRequest, None)) when
          transitions
            .copy(checkKnownFact = knownFactCheckClientNotFound)
            .checkedDetailsAllInformation(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientNotSignedUp(fastTrackRequest, None))
      }

      "transition to ClientNotRegistered when features alt-itsa true and the client is not enrolled for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, Some(postCode))
        val mockAppConfig = mock(classOf[AppConfig])
        when(mockAppConfig.featuresAltItsa).thenReturn(true)

        given(CheckDetails(fastTrackRequest, None)) when
          transitions
            .copy(checkKnownFact = knownFactCheckClientNotFound)
            .checkedDetailsAllInformation(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientNotRegistered(fastTrackRequest, None))
      }

      "transition to ClientNotFound when the client is not enrolled for afi service - as afi clients don't need any enrolment" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, nino, Some(dob))

        given(CheckDetails(fastTrackRequest, None)) when
          transitions
            .copy(checkKnownFact = knownFactCheckClientNotFound)
            .checkedDetailsAllInformation(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientNotFound(fastTrackRequest, None))
      }
      "transition to ClientNotSignedUp when the client is not enrolled for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, vrn, Some(vatRegDate))

        given(CheckDetails(fastTrackRequest, None)) when
          transitions
            .copy(checkKnownFact = knownFactCheckClientNotFound)
            .checkedDetailsAllInformation(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientNotSignedUp(fastTrackRequest, None))
      }
      "transition to ClientNotSignedUp when the client is not enrolled for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Vat, vrn, Some(vatRegDate))

        given(CheckDetails(fastTrackRequest, None)) when
          transitions
            .copy(checkKnownFact = knownFactCheckClientNotFound)
            .checkedDetailsAllInformation(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ClientNotSignedUp(fastTrackRequest, None))
      }
      "transition to PendingInvitationExists when there is already a pending invitation for this request" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, Some(postCode))
        def hasPendingInvitation(arn: Arn, clientId: String, service: Service): Future[Boolean] =
          Future.successful(true)

        given(CheckDetails(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing
            .copy(hasPendingInvitationsFor = hasPendingInvitation)
            .checkedDetailsAllInformation(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(PendingInvitationExists(fastTrackRequest, "invitation/link", "firstName lastName", None))
      }

      "transition to ActiveAuthorisationExists when there is already an active relationship between agent and client for this service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, Some(postCode))
        def hasActiveRelationship(arn: Arn, clientId: String, service: Service): Future[Boolean] =
          Future.successful(true)

        given(CheckDetails(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing
            .copy(hasActiveRelationshipFor = hasActiveRelationship)
            .checkedDetailsAllInformation(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(ActiveAuthorisationExists(fastTrackRequest, None))
      }

      "transition to PartialAuthorisationExists when there is already a PartialAuth request between agent and client for ITSA" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, Some(postCode))
        def hasPartialAuthorisation(arn: Arn, clientId: String): Future[Boolean] =
          Future.successful(true)

        given(CheckDetails(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing
            .copy(hasPartialAuthorisationFor = hasPartialAuthorisation)
            .checkedDetailsAllInformation(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(PartialAuthorisationExists(fastTrackRequest, None))
      }

      "transition to AlreadyCopiedAcross when there is a legacy mapping between agent and client for ITSA" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, Some(postCode))

        when(mockAppConfig.featuresAltItsa).thenReturn(true)

        given(CheckDetails(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing
            .copy(legacySaRelationshipStatusFor = legacySaRelationshipStatusMapped)
            .checkedDetailsAllInformation(mockAppConfig)(authorisedAgent)(Confirmation(true)) should
          thenGo(AlreadyCopiedAcrossItsa)
      }

      "transition to MissingDetail when there is no known fact in the request" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, None)

        given(CheckDetails(fastTrackRequest, None)) when
          transitions.checkedDetailsNoKnownFact(authorisedAgent) should
          thenGo(MissingDetail(fastTrackRequest, None))
      }
      "transition to IdentifyClient for ITSA with no postcode when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, None)

        given(CheckDetails(fastTrackRequest, None)) when transitions.checkedDetailsChangeInformation(authorisedAgent) should
          thenGo(IdentifyClient(fastTrackRequest, None))
      }
      "transition to IdentifyClient for IRV with no dob when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, nino, None)

        given(CheckDetails(fastTrackRequest, None)) when transitions.checkedDetailsChangeInformation(authorisedAgent) should
          thenGo(IdentifyClient(fastTrackRequest, None))
      }
      "transition to IdentifyClient for VAT with no vat reg date when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, vrn, None)

        given(CheckDetails(fastTrackRequest, None)) when transitions.checkedDetailsChangeInformation(authorisedAgent) should
          thenGo(IdentifyClient(fastTrackRequest, None))
      }
      "transition to SelectClientTypeVat for VAT with no client type when changing information" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.Vat, vrn, None)

        given(CheckDetails(fastTrackRequest, None)) when transitions.checkedDetailsChangeInformation(authorisedAgent) should
          thenGo(SelectClientType(fastTrackRequest, None, isChanging = true))
      }
      "transition to SelectClientType when there is no client type in the request" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.MtdIt, nino, Some(postCode))

        given(CheckDetails(fastTrackRequest, None)) when
          transitions.checkedDetailsNoClientType(authorisedAgent) should
          thenGo(SelectClientType(fastTrackRequest, None))
      }

      "transition to IdentifyCgtClient for CGT when client is UK based" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, cgtRef, None)

        given(CheckDetails(fastTrackRequest, None)) when transitions.checkedDetailsChangeInformation(authorisedAgent) should
          thenGo(IdentifyClient(fastTrackRequest, None))
      }
    }

    "at IdentifyClient" should {

      "transition to InvitationSent for itsa service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, Some(postCode))

        given(IdentifyClient(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing.identifiedClientItsa(mockAppConfig)(authorisedAgent)(ItsaClient(Nino("AB123456C"), "BN32TM")) should
          thenGo(InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Service.MtdIt, isAltItsa = Some(false)))
      }
      "transition to InvitationSent for itsa service when alt itsa authorisation" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, Some(postCode))

        given(IdentifyClient(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing
            .copy(isAltItsa = isAltItsa)
            .identifiedClientItsa(mockAppConfig)(authorisedAgent)(ItsaClient(Nino("AB123456C"), "BN32TM")) should
          thenGo(InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Service.MtdIt, isAltItsa = Some(true)))
      }

      "transition to InvitationSent for irv service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, nino, Some(dob))

        given(IdentifyClient(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing.identifiedClientIrv(mockAppConfig)(authorisedAgent)(IrvClient(Nino("AB123456C"), "1990-10-10")) should
          thenGo(InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Service.PersonalIncomeRecord, isAltItsa = Some(false)))
      }
      "transition to InvitationSent for personal vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, vrn, Some(vatRegDate))

        given(IdentifyClient(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing.identifiedClientVat(mockAppConfig)(authorisedAgent)(VatClient(Vrn("1234567"), "2010-10-10")) should
          thenGo(InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Service.Vat, isAltItsa = Some(false)))
      }
      "transition to InvitationSent for business vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Vat, vrn, Some(vatRegDate))

        given(IdentifyClient(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing.identifiedClientVat(mockAppConfig)(authorisedAgent)(VatClient(Vrn("1234567"), "2010-10-10")) should
          thenGo(InvitationSent(ClientType.Business, "invitation/link", None, "abc@xyz.com", Service.Vat))
      }
      "transition to client type for no client type vat service" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.Vat, vrn, Some(vatRegDate))
        val newVrn = Vrn("1234567")
        val newVatRegDate = "2010-10-10"

        given(IdentifyClient(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing.identifiedClientVat(mockAppConfig)(authorisedAgent)(VatClient(newVrn, newVatRegDate)) should
          thenGo(SelectClientType(fastTrackRequest.copy(clientId = newVrn, knownFact = Some(newVatRegDate)), None))
      }
      "transition to InvitationSent for business CBC service" in {
        val fastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Cbc, cbcId, Some(cbcEmail))

        given(IdentifyClient(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing.identifyCbcClient(mockAppConfig)(authorisedAgent)(CbcClient(cbcId, cbcEmail)) should
          thenGo(InvitationSent(ClientType.Business, "invitation/link", None, "abc@xyz.com", Service.Cbc))
      }

    }
    "at MoreDetails" should {
      val scenarios = Seq[(ClientType, Service, TaxIdentifier, String)](
        (Personal, Service.MtdIt, nino, postCode),
        (Personal, Service.PersonalIncomeRecord, nino, dob),
        (Personal, Service.Vat, vrn, vatRegDate),
        (Business, Service.Vat, vrn, vatRegDate),
        (Business, Service.CapitalGains, cgtRef, cgtPostcode),
        (Business, Service.Ppt, pptRef, pptRegDate),
        (Business, Service.Cbc, cbcId, cbcEmail)
      )

      scenarios.foreach { case (clientType, service, identifier, knownFact) =>
        s"transition to InvitationSent for $service ($clientType) service" in {
          val fastTrackRequest = AgentFastTrackRequest(Some(clientType), service, identifier, Some(knownFact))

          given(MissingDetail(fastTrackRequest, None)) when transitionsWithKnownFactsPassing.moreDetailsSupplied(mockAppConfig)(authorisedAgent)(
            knownFact
          ) should thenMatch {
            case InvitationSent(clientType, "invitation/link", None, "abc@xyz.com", service, None | Some(false) /* isAltItsa */ ) =>
          }
        }
      }
    }
    "at SelectClientType" should {

      "transition to InvitationSent for vat service when there is a known fact present" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.Vat, vrn, Some(vatRegDate))

        given(SelectClientType(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing.selectedClientType(mockAppConfig)(authorisedAgent)("personal") should
          thenGo(InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Service.Vat, isAltItsa = Some(false)))
      }
      "transition to MissingDetail for vat service when there is no known fact" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.Vat, vrn, None)

        given(SelectClientType(fastTrackRequest, None)) when
          transitionsWithKnownFactsPassing.selectedClientType(mockAppConfig)(authorisedAgent)("personal") should
          thenGo(MissingDetail(fastTrackRequest.copy(clientType = Some(Personal)), None))
      }

      "transition to IdentifyClientVat when changing answers" in {
        val fastTrackRequest = AgentFastTrackRequest(None, Service.Vat, vrn, None)

        given(SelectClientType(fastTrackRequest, None, isChanging = true)) when
          transitionsWithKnownFactsPassing.selectedClientType(mockAppConfig)(authorisedAgent)("personal") should
          thenGo(IdentifyClient(fastTrackRequest.copy(clientType = Some(Personal)), None))
      }
    }

    "at ClientNotFound, calling tryAgainNotMatchedKnownFact" when {
      "fast track request is for MTD-VAT and client type is missing should go to SelectClientTypeVat" in {
        val ftr = AgentFastTrackRequest(
          clientType = None,
          service = Service.Vat,
          vrn,
          knownFact = None
        )

        given(
          ClientNotFound(
            fastTrackRequest = ftr,
            continueUrl = None
          )
        ) when
          transitions.tryAgainNotMatchedKnownFact(authorisedAgent) should
          thenGo(SelectClientType(fastTrackRequest = ftr, continueUrl = None))
      }

      "fast track request is for MTD-VAT with client type should go to IdentifyClient" in {
        val ftr = AgentFastTrackRequest(
          clientType = Some(Personal),
          service = Service.Vat,
          vrn,
          knownFact = None
        )

        given(
          ClientNotFound(
            fastTrackRequest = ftr,
            continueUrl = None
          )
        ) when
          transitions.tryAgainNotMatchedKnownFact(authorisedAgent) should
          thenGo(
            IdentifyClient(
              fastTrackRequest = ftr,
              continueUrl = None
            )
          )
      }

      "fast track request is for MTD-IT service (and clientType was personal)" in {
        val ftr = AgentFastTrackRequest(
          clientType = Some(Personal),
          service = Service.MtdIt,
          nino,
          knownFact = None
        )

        given(
          ClientNotFound(
            fastTrackRequest = ftr,
            continueUrl = None
          )
        ) when
          transitions.tryAgainNotMatchedKnownFact(authorisedAgent) should
          thenGo(
            IdentifyClient(
              fastTrackRequest = ftr,
              continueUrl = None
            )
          )
      }

      "original request was for IRV service (and clientType was personal)" in {
        val ftr = AgentFastTrackRequest(
          clientType = Some(Personal),
          service = Service.PersonalIncomeRecord,
          nino,
          knownFact = None
        )

        given(
          ClientNotFound(
            fastTrackRequest = ftr,
            continueUrl = None
          )
        ) when
          transitions.tryAgainNotMatchedKnownFact(authorisedAgent) should
          thenGo(
            IdentifyClient(
              fastTrackRequest = ftr,
              continueUrl = None
            )
          )
      }

      "original request was for a Trust service" when {
        val completedTrustFastTrack = AgentFastTrackRequest(
          clientType = Some(ClientType.Business),
          service = Service.Trust,
          utr,
          knownFact = None
        )

        "trust not found for a given utr, should transition to IdentifyClient" in {
          val TrustNotFoundState = ClientNotFound(
            fastTrackRequest = completedTrustFastTrack,
            continueUrl = None
          )

          given(TrustNotFoundState) when
            transitions.tryAgainNotMatchedKnownFact(authorisedAgent) should
            thenGo(IdentifyClient(completedTrustFastTrack, None))
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
