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
import play.api.test.Helpers._
import support.UnitSpec
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.TransitionEffects._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel._
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models.VatKnownFactCheckResult._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AgentLedDeauthJourneyModelSpec extends UnitSpec with StateMatchers[State] {

  import Services._

  implicit val hc: HeaderCarrier = HeaderCarrier()

  case class given(initialState: State) extends AgentLedDeauthJourneyService with TestStorage[(State, List[State])] {
    await(save((initialState, Nil)))

    def when(transition: Transition): (State, List[State]) =
      await(super.apply(transition))
  }

  val authorisedAgent: AuthorisedAgent = AuthorisedAgent(Arn("TARN0000001"))
  val availableServices: Set[Service] = Set(Service.PersonalIncomeRecord, Service.MtdIt, Service.Vat, Service.CapitalGains, Service.Ppt)
  val allowlistedServices: Set[Service] = Set(Service.MtdIt, Service.Vat, Service.CapitalGains, Service.Ppt)
  val nino = "AB123456A"
  val postCode = "BN114AW"
  val vrn = "123456"
  val vatRegDate = "2010-10-10"
  val dob = "1990-10-10"

  val utr = Utr("1977030537")
  val trustResponse = TrustResponse(Right(TrustName("some-trust")))
  val TrustNotFoundResponse = TrustResponse(Left(InvalidTrust("RESOURCE_NOT_FOUND", "blah")))
  val cgtRef = CgtRef("XMCGTP123456789")

  val tpd = TypeOfPersonDetails("Individual", Left(IndividualName("firstName", "lastName")))

  def cgtAddressDetails(countryCode: String = "GB") =
    CgtAddressDetails("line1", Some("line2"), Some("line2"), Some("line2"), countryCode, Some("BN13 1FN"))

  def cgtSubscription(countryCode: String = "GB") =
    CgtSubscription("CGT", SubscriptionDetails(tpd, cgtAddressDetails(countryCode)))

  def getAgencyEmail: GetAgencyEmail = () => Future("abc@xyz.com")

  def getCgtSubscription(countryCode: String = "GB"): GetCgtSubscription = { cgtRef: CgtRef =>
    Future.successful(Some(cgtSubscription(countryCode)))
  }

  "AgentLedDeauthJourneyModel" when {
    "at state ClientType" should {
      "transition to SelectServicePersonal when personal is selected" in {
        given(SelectClientType) when selectedClientType(authorisedAgent)("personal") should thenGo(
          SelectService(ClientType.Personal, availableServices))
      }
      "transition to SelectServiceBusiness when business is selected" in {
        given(SelectClientType) when selectedClientType(authorisedAgent)("business") should thenGo(
          SelectService(ClientType.Business, enabledServices = Set(Service.Vat, Service.Ppt)))
      }
    }
    "at state SelectServicePersonal" should {
      "transition to IdentifyClientPersonal when service is ITSA and feature flag is on" in {
        given(SelectService(ClientType.Personal, availableServices)) when chosenPersonalService(
          showItsaFlag = true,
          showPirFlag = true,
          showVatFlag = true,
          showCgtFlag = true,
          showPptFlag = true
        )(authorisedAgent)(Service.MtdIt) should thenGo(
          IdentifyClientPersonal(Service.MtdIt)
        )
      }
      "throw an exception when service is ITSA and the show itsa flag is switched off" in {
        intercept[Exception] {
          given(SelectService(ClientType.Personal, availableServices)) when chosenPersonalService(
            showItsaFlag = false,
            showPirFlag = true,
            showVatFlag = true,
            showCgtFlag = true,
            showPptFlag = true
          )(authorisedAgent)(Service.MtdIt)
        }.getMessage shouldBe "Service: HMRC-MTD-IT feature flag is switched off"
      }
      "transition to IdentifyClientPersonal when service is PIR and feature flag is on" in {
        given(SelectService(ClientType.Personal, availableServices)) when chosenPersonalService(
          showItsaFlag = true,
          showPirFlag = true,
          showVatFlag = true,
          showCgtFlag = true,
          showPptFlag = true
        )(authorisedAgent)(Service.PersonalIncomeRecord) should thenGo(
          IdentifyClientPersonal(Service.PersonalIncomeRecord)
        )
      }
      "throw an exception when service is IRV and the show irv flag is switched off" in {
        intercept[Exception] {
          given(SelectService(ClientType.Personal, availableServices)) when chosenPersonalService(
            showItsaFlag = true,
            showPirFlag = false,
            showVatFlag = true,
            showCgtFlag = true,
            showPptFlag = true
          )(authorisedAgent)(Service.PersonalIncomeRecord)
        }.getMessage shouldBe "Service: PERSONAL-INCOME-RECORD feature flag is switched off"
      }
      "transition to IdentifyClientPersonal when service is VAT and feature flag is on" in {
        given(SelectService(ClientType.Personal, availableServices)) when chosenPersonalService(
          showItsaFlag = true,
          showPirFlag = true,
          showVatFlag = true,
          showCgtFlag = true,
          showPptFlag = true
        )(authorisedAgent)(Service.Vat) should thenGo(
          IdentifyClientPersonal(Service.Vat)
        )
      }
      "throw an exception when service is VAT and the show vat flag is switched off" in {
        intercept[Exception] {
          given(SelectService(ClientType.Personal, availableServices)) when chosenPersonalService(
            showItsaFlag = true,
            showPirFlag = true,
            showVatFlag = false,
            showCgtFlag = true,
            showPptFlag = true
          )(authorisedAgent)(Service.Vat)
        }.getMessage shouldBe "Service: HMRC-MTD-VAT feature flag is switched off"
      }

      "transition to IdentifyClientPersonal when service is CGT and feature flag is on" in {
        given(SelectService(ClientType.Personal, availableServices)) when chosenPersonalService(
          showItsaFlag = true,
          showPirFlag = true,
          showVatFlag = true,
          showCgtFlag = true,
          showPptFlag = true
        )(authorisedAgent)(Service.CapitalGains) should thenGo(
          IdentifyClientPersonal(Service.CapitalGains)
        )
      }
      "throw an exception when service is CGT and the show cgt flag is switched off" in {
        intercept[Exception] {
          given(SelectService(ClientType.Personal, availableServices)) when chosenPersonalService(
            showItsaFlag = true,
            showPirFlag = true,
            showVatFlag = false,
            showCgtFlag = false,
            showPptFlag = false
          )(authorisedAgent)(Service.CapitalGains)
        }.getMessage shouldBe "Service: HMRC-CGT-PD feature flag is switched off"
      }
      "throw an exception when service is PPT and the show ppt flag is switched off" in {
        intercept[Exception] {
          given(SelectService(ClientType.Personal, availableServices)) when chosenPersonalService(
            showItsaFlag = true,
            showPirFlag = true,
            showVatFlag = false,
            showCgtFlag = false,
            showPptFlag = false
          )(authorisedAgent)(Service.Ppt)
        }.getMessage shouldBe "Service: HMRC-PPT-ORG feature flag is switched off"
      }
    }
    "at state SelectServiceBusiness" should {
      "transition to IdentifyClientBusiness when YES is selected and feature flag is on" in {
        given(SelectService(ClientType.Business, enabledServices = Set(Service.Vat, Service.Ppt))) when
          chosenBusinessService(showVatFlag = true, showPptFlag = true)(authorisedAgent)(Some(Service.Vat)) should
          thenGo(IdentifyClientBusiness(Service.Vat))
      }
      "transition to ClientType when NO is selected and feature flag is on" in {
        given(SelectService(ClientType.Business, enabledServices = Set(Service.Vat, Service.Ppt))) when
          chosenBusinessService(showVatFlag = true, showPptFlag = true)(authorisedAgent)(None) should
          thenGo(SelectClientType)
      }
      "throw an exception when YES is selected but the show vat flag is switched off" in {
        intercept[Exception] {
          given(SelectService(ClientType.Business, enabledServices = Set(Service.Vat, Service.Ppt))) when
            chosenBusinessService(showVatFlag = false, showPptFlag = true)(authorisedAgent)(Some(Service.Vat))
        }.getMessage shouldBe "Service: HMRC-MTD-VAT feature flag is switched off"
      }
    }

    "at state SelectServiceTrust" should {
      "transition to IdentifyClientTrust for TRUST and when feature flag is on" in {
        given(SelectService(ClientType.Trust, Set(Service.Trust, Service.CapitalGains))) when chosenTrustService(
          showTrustFlag = true,
          showCgtFlag = true,
          showPptFlag = true)(authorisedAgent)(Service.Trust) should thenGo(IdentifyClientTrust)
      }

      "transition to IdentifyClientCgt when YES is selected and feature flag is on" in {
        given(SelectService(ClientType.Trust, Set(Service.Trust, Service.CapitalGains))) when chosenTrustService(
          showTrustFlag = true,
          showCgtFlag = true,
          showPptFlag = true)(authorisedAgent)(Service.CapitalGains) should thenGo(IdentifyClientCgt)
      }

      "throw an exception when YES is selected but the show trust flag is switched off" in {
        intercept[Exception] {
          given(SelectService(ClientType.Trust, Set(Service.Trust, Service.CapitalGains))) when chosenTrustService(
            showTrustFlag = false,
            showCgtFlag = true,
            showPptFlag = true)(authorisedAgent)(Service.Trust)
        }.getMessage shouldBe "Service: HMRC-TERS-ORG feature flag is switched off"
      }
    }

    "at state IdentifyClientPersonal" should {
      def getClientName(clientId: String, service: Service) = Future(Some("John Smith"))
      def hasActiveRelationships(arn: Arn, clientId: String, service: Service) = Future(true)
      def hasPartialAuthorisation(arn: Arn, clientId: String) = Future(false)
      val itsaClient = ItsaClient(nino, postCode)
      val irvClient = IrvClient(nino, dob)
      val vatClient = VatClient(vrn, vatRegDate)
      val cgtClient = CgtClient(cgtRef)

      "transition to ConfirmClientItsa when postcode matches" in {
        def postcodeMatches(nino: Nino, postcode: String): Future[Some[Boolean]] = Future(Some(true))

        given(IdentifyClientPersonal(Service.MtdIt)) when submitIdentifyClientItsa(postcodeMatches, getClientName, hasActiveRelationships)(
          authorisedAgent)(itsaClient) should thenGo(ConfirmClientItsa(Some("John Smith"), Nino(nino)))
      }
      "transition to KnownFactNotMatched when postcode does not match" in {
        def postcodeDoesNotMatch(nino: Nino, postcode: String): Future[Some[Boolean]] = Future(Some(false))

        given(IdentifyClientPersonal(Service.MtdIt)) when submitIdentifyClientItsa(postcodeDoesNotMatch, getClientName, hasActiveRelationships)(
          authorisedAgent)(itsaClient) should thenGo(KnownFactNotMatched)
      }
      "transition to NotSignedUp when client is not enrolled for itsa" in {
        def clientNotSignedUp(nino: Nino, postcode: String): Future[Option[Boolean]] = Future(None)

        given(IdentifyClientPersonal(Service.MtdIt)) when submitIdentifyClientItsa(clientNotSignedUp, getClientName, hasActiveRelationships)(
          authorisedAgent)(itsaClient) should thenGo(NotSignedUp(Service.MtdIt))
      }
      "throw an Exception when the client has no postcode" in {
        def postcodeNotMatches(nino: Nino, postcode: String): Future[Some[Boolean]] = Future(Some(false))

        intercept[Exception] {
          given(IdentifyClientPersonal(Service.MtdIt)) when submitIdentifyClientItsa(postcodeNotMatches, getClientName, hasActiveRelationships)(
            authorisedAgent)(ItsaClient(nino, ""))
        }.getMessage shouldBe "Postcode expected but none found"
      }
      "transition to ConfirmCancel when dob matches" in {
        def dobMatches(nino: Nino, localDate: LocalDate): Future[Some[Boolean]] = Future(Some(true))

        given(IdentifyClientPersonal(Service.PersonalIncomeRecord)) when submitIdentifyClientIrv(
          dobMatches,
          getClientName,
          hasActiveRelationships,
          hasPartialAuthorisation)(authorisedAgent)(irvClient) should thenGo(ConfirmCancel(Service.PersonalIncomeRecord, Some("John Smith"), nino))
      }
      "transition to KnownFactNotMatched when dob does not match" in {
        def dobDoesNotMatch(nino: Nino, localDate: LocalDate): Future[Some[Boolean]] = Future(Some(false))

        given(IdentifyClientPersonal(Service.PersonalIncomeRecord)) when submitIdentifyClientIrv(
          dobDoesNotMatch,
          getClientName,
          hasActiveRelationships,
          hasPartialAuthorisation)(authorisedAgent)(irvClient) should thenGo(KnownFactNotMatched)
      }
      "transition to NotSignedUp when client endpoint returns None" in {
        def clientNotSignedUp(nino: Nino, localDate: LocalDate): Future[Option[Boolean]] = Future(None)

        given(IdentifyClientPersonal(Service.PersonalIncomeRecord)) when submitIdentifyClientIrv(
          clientNotSignedUp,
          getClientName,
          hasActiveRelationships,
          hasPartialAuthorisation)(authorisedAgent)(irvClient) should thenGo(NotSignedUp(Service.PersonalIncomeRecord))
      }
      "throw an Exception when the client has no dob" in {
        def dobNotMatches(nino: Nino, localDate: LocalDate): Future[Some[Boolean]] = Future(Some(false))

        intercept[Exception] {
          given(IdentifyClientPersonal(Service.PersonalIncomeRecord)) when submitIdentifyClientIrv(
            dobNotMatches,
            getClientName,
            hasActiveRelationships,
            hasPartialAuthorisation)(authorisedAgent)(IrvClient(nino, ""))
        }.getMessage shouldBe "Date of birth expected but none found"
      }
      "transition to ConfirmClientVat when vat reg date matches" in {
        def vatRegDateMatches(vrn: Vrn, vatRegDate: LocalDate): Future[VatKnownFactCheckResult] = Future(VatKnownFactCheckOk)

        given(IdentifyClientPersonal(Service.Vat)) when submitIdentifyClientVat(vatRegDateMatches, getClientName, hasActiveRelationships)(
          authorisedAgent)(vatClient) should thenGo(ConfirmClientPersonalVat(Some("John Smith"), Vrn(vrn)))
      }
      "transition to KnownFactNotMatched when vat reg date does not match" in {
        def vatRegDateDoesNotMatch(vrn: Vrn, vatRegDate: LocalDate): Future[VatKnownFactCheckResult] = Future(VatKnownFactNotMatched)

        given(IdentifyClientPersonal(Service.Vat)) when submitIdentifyClientVat(vatRegDateDoesNotMatch, getClientName, hasActiveRelationships)(
          authorisedAgent)(vatClient) should thenGo(KnownFactNotMatched)
      }

      "transition to ConfirmClientPersonalVat when vat reg date matches but client is insolvent" in {
        def vatClientInsolvent(vrn: Vrn, vatRegDate: LocalDate): Future[VatKnownFactCheckResult] = Future(VatRecordClientInsolvent)

        given(IdentifyClientPersonal(Service.Vat)) when submitIdentifyClientVat(vatClientInsolvent, getClientName, hasActiveRelationships)(
          authorisedAgent)(vatClient) should thenGo(ConfirmClientPersonalVat(Some("John Smith"), Vrn("123456")))
      }

      "transition to Not signed up when vat record is being migrated" in {
        def vatRecordMigration(vrn: Vrn, vatRegDate: LocalDate): Future[VatKnownFactCheckResult] = Future(VatRecordMigrationInProgress)

        given(IdentifyClientPersonal(Service.Vat)) when submitIdentifyClientVat(vatRecordMigration, getClientName, hasActiveRelationships)(
          authorisedAgent)(vatClient) should thenGo(NotSignedUp(Service.Vat))
      }

      "transition to NotSignedUp when client is not enrolled for VAT" in {
        def clientNotSignedUp(vrn: Vrn, vatRegDate: LocalDate): Future[VatKnownFactCheckResult] = Future(VatDetailsNotFound)

        given(IdentifyClientPersonal(Service.Vat)) when submitIdentifyClientVat(clientNotSignedUp, getClientName, hasActiveRelationships)(
          authorisedAgent)(vatClient) should thenGo(NotSignedUp(Service.Vat))
      }
      "throw an Exception when the client has no vat reg date" in {
        def vatRegDateDoesNotMatch(vrn: Vrn, vatRegDate: LocalDate): Future[VatKnownFactCheckResult] = Future(VatDetailsNotFound)

        intercept[Exception] {
          given(IdentifyClientPersonal(Service.Vat)) when submitIdentifyClientVat(vatRegDateDoesNotMatch, getClientName, hasActiveRelationships)(
            authorisedAgent)(VatClient(vrn, ""))
        }.getMessage shouldBe "Vat registration date expected but none found"
      }

      "transition to ConfirmPostcodeCgt for cgt for UK based clients" in {
        given(IdentifyClientPersonal(Service.CapitalGains)) when submitIdentifyClientCgt(getCgtSubscription())(authorisedAgent)(cgtClient) should thenGo(
          ConfirmPostcodeCgt(cgtRef, Some("BN13 1FN"), "firstName lastName"))
      }

      "transition to ConfirmCountryCodeCgt for cgt for non-UK based clients" in {
        given(IdentifyClientPersonal(Service.CapitalGains)) when submitIdentifyClientCgt(getCgtSubscription("FR"))(authorisedAgent)(cgtClient) should thenGo(
          ConfirmCountryCodeCgt(cgtRef, "FR", "firstName lastName"))
      }
    }

    "at state IdentifyClientBusiness" should {
      def getClientName(clientId: String, service: Service) = Future(Some("John Smith"))
      def hasActiveRelationships(arn: Arn, clientId: String, service: Service) = Future(true)
      val vatClient = VatClient(vrn, vatRegDate)
      "transition to ConfirmClientVat when known fact matches" in {
        def vatRegDateMatches(vrn: Vrn, vatRegDate: LocalDate): Future[VatKnownFactCheckResult] = Future(VatKnownFactCheckOk)

        given(IdentifyClientBusiness(Service.Vat)) when submitIdentifyClientVat(vatRegDateMatches, getClientName, hasActiveRelationships)(
          authorisedAgent)(vatClient) should thenGo(ConfirmClientBusiness(Some("John Smith"), Vrn(vrn)))
      }
      "transition to KnownFactNotMatched when known fact does not match" in {
        def vatRegDateDoesNotMatch(vrn: Vrn, vatRegDate: LocalDate): Future[VatKnownFactCheckResult] = Future(VatKnownFactNotMatched)

        given(IdentifyClientBusiness(Service.Vat)) when submitIdentifyClientVat(vatRegDateDoesNotMatch, getClientName, hasActiveRelationships)(
          authorisedAgent)(vatClient) should thenGo(KnownFactNotMatched)
      }

      "transition to NotSignedUp when client is not enrolled" in {
        def clientNotSignedUp(vrn: Vrn, vatRegDate: LocalDate): Future[VatKnownFactCheckResult] = Future(VatDetailsNotFound)

        given(IdentifyClientBusiness(Service.Vat)) when submitIdentifyClientVat(clientNotSignedUp, getClientName, hasActiveRelationships)(
          authorisedAgent)(vatClient) should thenGo(NotSignedUp(Service.Vat))
      }
    }

    "at state IdentifyClientTrust" should {
      val trustClient = TrustClient(utr)
      "transition to ConfirmClientTrust when a trust is found for a given utr" in {
        def getTrustName(trustTaxIdentifier: TrustTaxIdentifier): Future[TrustResponse] = Future(trustResponse)

        given(IdentifyClientTrust) when submitIdentifyClientTrust(getTrustName)(authorisedAgent)(trustClient) should thenGo(
          ConfirmClientTrust("some-trust", utr))
      }
      "transition to TrustNotFound when a trust is not found for a given utr" in {
        def getTrustName(trustTaxIdentifier: TrustTaxIdentifier): Future[TrustResponse] = Future(TrustNotFoundResponse)
        given(IdentifyClientTrust) when submitIdentifyClientTrust(getTrustName)(authorisedAgent)(trustClient) should thenGo(TrustNotFound)
      }
    }

    "at state ConfirmPostcodeCgt" should {
      "transition to ConfirmClientCgt for cgt" in {
        given(ConfirmPostcodeCgt(cgtRef, Some("BN13 1FN"), "firstName lastName")) when confirmPostcodeCgt(getCgtSubscription())(authorisedAgent)(
          Postcode("BN13 1FN")) should thenGo(ConfirmClientCgt(cgtRef, "firstName lastName"))
      }

      "transition to KnownFactNotMatched if postcodes do not match" in {
        given(ConfirmPostcodeCgt(cgtRef, Some("BN13 1FN"), "firstName lastName")) when confirmPostcodeCgt(getCgtSubscription())(authorisedAgent)(
          Postcode("AAA")) should thenGo(KnownFactNotMatched)
      }
    }

    "at state ConfirmCountryCodeCgt" should {
      "transition to ConfirmClientCgt for cgt" in {
        given(ConfirmCountryCodeCgt(cgtRef, "FR", "firstName lastName")) when confirmCountryCodeCgt(getCgtSubscription())(authorisedAgent)(
          CountryCode("FR")) should thenGo(ConfirmClientCgt(cgtRef, "firstName lastName"))
      }

      "transition to KnownFactNotMatched if country codes do not match" in {
        given(ConfirmCountryCodeCgt(cgtRef, "FR", "firstName lastName")) when confirmCountryCodeCgt(getCgtSubscription())(authorisedAgent)(
          CountryCode("FRX")) should thenGo(KnownFactNotMatched)
      }
    }

    "at state ConfirmClientItsa" should {
      "transition to ConfirmCancel when YES is selected" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: Service) = Future(true)
        def hasPartialAuthorisation(arn: Arn, clientId: String) = Future(false)

        given(ConfirmClientItsa(Some("Lucy Rose"), Nino(nino))) when
          clientConfirmed(hasActiveRelationships)(hasPartialAuthorisation)(authorisedAgent)(Confirmation(true)) should thenGo(
          ConfirmCancel(Service.MtdIt, Some("Lucy Rose"), nino)
        )
      }

      "transition to ConfirmCancel when YES is selected for alt-itsa" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: Service) = Future(false)
        def hasPartialAuthorisation(arn: Arn, clientId: String) = Future(true)

        given(ConfirmClientItsa(Some("Lucy Rose"), Nino(nino))) when
          clientConfirmed(hasActiveRelationships)(hasPartialAuthorisation)(authorisedAgent)(Confirmation(true)) should thenGo(
          ConfirmCancel(Service.MtdIt, Some("Lucy Rose"), nino, true)
        )
      }
      "transition to root when NO is selected" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: Service) = Future(true)
        def hasPartialAuthorisation(arn: Arn, clientId: String) = Future(false)

        given(ConfirmClientItsa(Some("Lucy Rose"), Nino(nino))) when
          clientConfirmed(hasActiveRelationships)(hasPartialAuthorisation)(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType
        )
      }
      "transition to root when NO is selected for alt-itsa" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: Service) = Future(false)
        def hasPartialAuthorisation(arn: Arn, clientId: String) = Future(true)

        given(ConfirmClientItsa(Some("Lucy Rose"), Nino(nino))) when
          clientConfirmed(hasActiveRelationships)(hasPartialAuthorisation)(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType
        )
      }
      "transition to NotAuthorised when there are no active relationships or partial auth requests" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: Service) = Future(false)
        def hasPartialAuthorisation(arn: Arn, clientId: String) = Future(false)

        given(ConfirmClientItsa(Some("Lucy Rose"), Nino(nino))) when
          clientConfirmed(hasActiveRelationships)(hasPartialAuthorisation)(authorisedAgent)(Confirmation(true)) should thenGo(
          NotAuthorised(Service.MtdIt)
        )
      }
    }
    "at state ConfirmClientIrv" should {
      "transition to ConfirmCancel when YES is selected" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: Service) = Future(true)
        def hasPartialAuthorisation(arn: Arn, clientId: String) = Future(false)

        given(ConfirmClientIrv(Some("Lucy Rose"), Nino(nino))) when
          clientConfirmed(hasActiveRelationships)(hasPartialAuthorisation)(authorisedAgent)(Confirmation(true)) should thenGo(
          ConfirmCancel(Service.PersonalIncomeRecord, Some("Lucy Rose"), nino)
        )
      }
      "transition to root when NO is selected" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: Service) = Future(true)
        def hasPartialAuthorisation(arn: Arn, clientId: String) = Future(false)

        given(ConfirmClientIrv(Some("Lucy Rose"), Nino(nino))) when
          clientConfirmed(hasActiveRelationships)(hasPartialAuthorisation)(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType
        )
      }
      "transition to NotAuthorised when there are no active relationships" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: Service) = Future(false)
        def hasPartialAuthorisation(arn: Arn, clientId: String) = Future(false)

        given(ConfirmClientIrv(Some("Lucy Rose"), Nino(nino))) when
          clientConfirmed(hasActiveRelationships)(hasPartialAuthorisation)(authorisedAgent)(Confirmation(true)) should thenGo(
          NotAuthorised(Service.PersonalIncomeRecord)
        )
      }
    }
    "at state ConfirmClientPersonalVat" should {
      "transition to ConfirmCancel when YES is selected" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: Service) = Future(true)
        def hasPartialAuthorisation(arn: Arn, clientId: String) = Future(false)

        given(ConfirmClientPersonalVat(Some("Lucy Rose"), Vrn(vrn))) when
          clientConfirmed(hasActiveRelationships)(hasPartialAuthorisation)(authorisedAgent)(Confirmation(true)) should thenGo(
          ConfirmCancel(Service.Vat, Some("Lucy Rose"), vrn)
        )
      }
      "transition to root when NO is selected" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: Service) = Future(true)
        def hasPartialAuthorisation(arn: Arn, clientId: String) = Future(false)

        given(ConfirmClientPersonalVat(Some("Lucy Rose"), Vrn(vrn))) when
          clientConfirmed(hasActiveRelationships)(hasPartialAuthorisation)(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType
        )
      }
      "transition to NotAuthorised when there are no active relationships" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: Service) = Future(false)
        def hasPartialAuthorisation(arn: Arn, clientId: String) = Future(false)

        given(ConfirmClientPersonalVat(Some("Lucy Rose"), Vrn(vrn))) when
          clientConfirmed(hasActiveRelationships)(hasPartialAuthorisation)(authorisedAgent)(Confirmation(true)) should thenGo(
          NotAuthorised(Service.Vat)
        )
      }
    }
    "at state ConfirmClientBusiness" should {
      "transition to ConfirmCancel when YES is selected" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: Service) = Future(true)
        def hasPartialAuthorisation(arn: Arn, clientId: String) = Future(false)

        given(ConfirmClientBusiness(Some("Lucy Rose"), Vrn(vrn))) when
          clientConfirmed(hasActiveRelationships)(hasPartialAuthorisation)(authorisedAgent)(Confirmation(true)) should thenGo(
          ConfirmCancel(Service.Vat, Some("Lucy Rose"), vrn)
        )
      }
      "transition to root when NO is selected" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: Service) = Future(true)
        def hasPartialAuthorisation(arn: Arn, clientId: String) = Future(false)

        given(ConfirmClientBusiness(Some("Lucy Rose"), Vrn(vrn))) when
          clientConfirmed(hasActiveRelationships)(hasPartialAuthorisation)(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType
        )
      }
      "transition to NotAuthorised when there are no active relationships" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: Service) = Future(false)
        def hasPartialAuthorisation(arn: Arn, clientId: String) = Future(false)

        given(ConfirmClientBusiness(Some("Lucy Rose"), Vrn(vrn))) when
          clientConfirmed(hasActiveRelationships)(hasPartialAuthorisation)(authorisedAgent)(Confirmation(true)) should thenGo(
          NotAuthorised(Service.Vat)
        )
      }
    }

    "at state ConfirmClientTrust" should {
      "transition to ConfirmCancel when YES is selected" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: Service) = Future(true)
        def hasPartialAuthorisation(arn: Arn, clientId: String) = Future(false)

        given(ConfirmClientTrust("some-trust", utr)) when
          clientConfirmed(hasActiveRelationships)(hasPartialAuthorisation)(authorisedAgent)(Confirmation(true)) should thenGo(
          ConfirmCancel(Service.Trust, Some("some-trust"), utr.value)
        )
      }
      "transition to root when NO is selected" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: Service) = Future(true)
        def hasPartialAuthorisation(arn: Arn, clientId: String) = Future(false)

        given(ConfirmClientTrust("some-trust", utr)) when
          clientConfirmed(hasActiveRelationships)(hasPartialAuthorisation)(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType
        )
      }
      "transition to NotAuthorised when there are no active relationships" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: Service) = Future(false)
        def hasPartialAuthorisation(arn: Arn, clientId: String) = Future(false)

        given(ConfirmClientTrust("some-trust", utr)) when
          clientConfirmed(hasActiveRelationships)(hasPartialAuthorisation)(authorisedAgent)(Confirmation(true)) should thenGo(
          NotAuthorised(Service.Trust)
        )
      }
    }

    "at state ConfirmCancel" should {
      "transition to AuthorisationCancelled when YES is selected" in {
        def deleteRelationship(service: Service, arn: Arn, clientId: String) = Future(Some(true))
        def getAgencyName(arn: Arn) = Future("Popeye")
        def setRelationshipEnded(arn: Arn, client: String, service: Service): Future[Option[Boolean]] = Future(Some(true))

        given(ConfirmCancel(Service.MtdIt, Some("Holly Herndon"), nino)) when cancelConfirmed(
          deleteRelationship,
          getAgencyName,
          setRelationshipEnded)(authorisedAgent)(Confirmation(true)) should thenGo(
          AuthorisationCancelled(Service.MtdIt, Some("Holly Herndon"), "Popeye")
        )
      }

      "transition to AuthorisationCancelled when YES is selected for alt-itsa" in {
        def deleteRelationship(service: Service, arn: Arn, clientId: String) = Future(Some(true))
        def getAgencyName(arn: Arn) = Future("Popeye")
        def setRelationshipEnded(arn: Arn, client: String, service: Service): Future[Option[Boolean]] = Future(Some(true))

        given(ConfirmCancel(Service.MtdIt, Some("Holly Herndon"), nino, true)) when cancelConfirmed(
          deleteRelationship,
          getAgencyName,
          setRelationshipEnded)(authorisedAgent)(Confirmation(true)) should thenGo(
          AuthorisationCancelled(Service.MtdIt, Some("Holly Herndon"), "Popeye")
        )
      }

      "transition to AuthorisationCancelled when YES is selected and service is Trust" in {
        def deleteRelationship(service: Service, arn: Arn, clientId: String) = Future(Some(true))
        def getAgencyName(arn: Arn) = Future("Popeye")
        def setRelationshipEnded(arn: Arn, client: String, service: Service): Future[Option[Boolean]] = Future(Some(true))

        given(ConfirmCancel(Service.Trust, Some("some-trust"), utr.value)) when cancelConfirmed(
          deleteRelationship,
          getAgencyName,
          setRelationshipEnded)(authorisedAgent)(Confirmation(true)) should thenGo(
          AuthorisationCancelled(Service.Trust, Some("some-trust"), "Popeye")
        )
      }

      "transition to select client type when NO is selected" in {
        def deleteRelationship(service: Service, arn: Arn, clientId: String) = Future(Some(true))
        def getAgencyName(arn: Arn) = Future("Popeye")
        def setRelationshipEnded(arn: Arn, client: String, service: Service): Future[Option[Boolean]] = Future(Some(true))

        given(ConfirmCancel(Service.MtdIt, Some("Holly Herndon"), nino)) when cancelConfirmed(
          deleteRelationship,
          getAgencyName,
          setRelationshipEnded)(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType
        )
      }

      "transition to select client type when NO is selected for alt-itsa" in {
        def deleteRelationship(service: Service, arn: Arn, clientId: String) = Future(Some(true))
        def getAgencyName(arn: Arn) = Future("Popeye")
        def setRelationshipEnded(arn: Arn, client: String, service: Service): Future[Option[Boolean]] = Future(Some(true))

        given(ConfirmCancel(Service.MtdIt, Some("Holly Herndon"), nino, true)) when cancelConfirmed(
          deleteRelationship,
          getAgencyName,
          setRelationshipEnded)(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType
        )
      }
      "transition to ResponseFailed when the relationship termination fails" in {
        def deleteRelationship(service: Service, arn: Arn, clientId: String) = Future(Some(false))
        def getAgencyName(arn: Arn) = Future("Popeye")
        def setRelationshipEnded(arn: Arn, client: String, service: Service): Future[Option[Boolean]] = Future(Some(true))

        given(ConfirmCancel(Service.MtdIt, Some("Holly Herndon"), nino)) when cancelConfirmed(
          deleteRelationship,
          getAgencyName,
          setRelationshipEnded)(authorisedAgent)(Confirmation(true)) should thenGo(
          ResponseFailed(Service.MtdIt, Some("Holly Herndon"), nino)
        )
      }
    }
  }
}
