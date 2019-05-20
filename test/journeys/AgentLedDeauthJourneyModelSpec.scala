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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel._
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

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

  val authorisedAgent = AuthorisedAgent(Arn("TARN0000001"), isWhitelisted = true)
  val nonWhitelistedAgent = AuthorisedAgent(Arn("TARN0000001"), isWhitelisted = false)
  val availableServices = Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)
  val whitelistedServices = Set(HMRCMTDIT, HMRCMTDVAT)
  val nino = "AB123456A"
  val postCode = Some("BN114AW")
  val vrn = "123456"
  val vatRegDate = Some("2010-10-10")
  val dob = Some("1990-10-10")

  "AgentLedDeauthJourneyModel" when {
    "at root" should {
      "transition to SelectClientType if agent led deauth flag is on" in {
        given(root) when showSelectClientType(showAgentLedDeauth = true)(authorisedAgent) should thenGo(
          SelectClientType
        )
      }
      "fail and throw an exception if agent led deauth flag is off" in {
        intercept[Exception] {
          given(root) when showSelectClientType(showAgentLedDeauth = false)(authorisedAgent)
        }.getMessage shouldBe "Agent led de authorisation feature is disabled."
      }
    }
    "at state ClientType" should {
      "transition to SelectServicePersonal when personal is selected" in {
        given(SelectClientType) when chosenClientType(authorisedAgent)(ClientType.personal) should thenGo(
          SelectServicePersonal(availableServices))
      }
      "transition to SelectServiceBusiness when business is selected" in {
        given(SelectClientType) when chosenClientType(authorisedAgent)(ClientType.business) should thenGo(
          SelectServiceBusiness)
      }
      "transition to SelectServicePersonal with only whitelisted services" in {
        given(SelectClientType) when chosenClientType(nonWhitelistedAgent)(ClientType.personal) should thenGo(
          SelectServicePersonal(whitelistedServices))
      }
    }
    "at state SelectServicePersonal" should {
      "transition to IdentifyClientPersonal when service is ITSA and feature flag is on" in {
        given(SelectServicePersonal(availableServices)) when chosenPersonalService(
          showItsaFlag = true,
          showPirFlag = true,
          showVatFlag = true)(authorisedAgent)(HMRCMTDIT) should thenGo(
          IdentifyClientPersonal(HMRCMTDIT)
        )
      }
      "throw an exception when service is ITSA if the show itsa flag is switched off" in {
        intercept[Exception] {
          given(SelectServicePersonal(availableServices)) when chosenPersonalService(
            showItsaFlag = false,
            showPirFlag = true,
            showVatFlag = true)(authorisedAgent)(HMRCMTDIT)
        }.getMessage shouldBe "Service: HMRC-MTD-IT feature flag is switched off"
      }
      "transition to IdentifyClientPersonal when service is PIR and feature flag is on" in {
        given(SelectServicePersonal(availableServices)) when chosenPersonalService(
          showItsaFlag = true,
          showPirFlag = true,
          showVatFlag = true)(authorisedAgent)(HMRCPIR) should thenGo(
          IdentifyClientPersonal(HMRCPIR)
        )
      }
      "throw an exception when service is IRV if the show irv flag is switched off" in {
        intercept[Exception] {
          given(SelectServicePersonal(availableServices)) when chosenPersonalService(
            showItsaFlag = true,
            showPirFlag = false,
            showVatFlag = true)(authorisedAgent)(HMRCPIR)
        }.getMessage shouldBe "Service: PERSONAL-INCOME-RECORD feature flag is switched off"
      }
      "transition to IdentifyClientPersonal when service is VAT and feature flag is on" in {
        given(SelectServicePersonal(availableServices)) when chosenPersonalService(
          showItsaFlag = true,
          showPirFlag = true,
          showVatFlag = true)(authorisedAgent)(HMRCMTDVAT) should thenGo(
          IdentifyClientPersonal(HMRCMTDVAT)
        )
      }
      "throw an exception when service is VAT if the show vat flag is switched off" in {
        intercept[Exception] {
          given(SelectServicePersonal(availableServices)) when chosenPersonalService(
            showItsaFlag = true,
            showPirFlag = true,
            showVatFlag = false)(authorisedAgent)(HMRCMTDVAT)
        }.getMessage shouldBe "Service: HMRC-MTD-VAT feature flag is switched off"
      }
    }
    "at state SelectServiceBusiness" should {
      "transition to IdentifyClientBusiness when YES is selected and feature flag is on" in {
        given(SelectServiceBusiness) when chosenBusinessService(showVatFlag = true)(authorisedAgent)(Confirmation(true)) should thenGo(
          IdentifyClientBusiness
        )
      }
      "transition to ClientType when NO is selected and feature flag is on" in {
        given(SelectServiceBusiness) when chosenBusinessService(showVatFlag = true)(authorisedAgent)(
          Confirmation(false)) should thenGo(
          SelectClientType
        )
      }
      "throw an exception when YES is selected but the show vat flag is switched off" in {
        intercept[Exception] {
          given(SelectServiceBusiness) when chosenBusinessService(showVatFlag = false)(authorisedAgent)(
            Confirmation(true))
        }.getMessage shouldBe "Service: HMRC-MTD-VAT feature flag is switched off"
      }
    }

    def getClientName(clientId: String, service: String) = Future(Some("John Smith"))
    def hasActiveRelationships(arn: Arn, clientId: String, service: String) = Future(true)

    "at state IdentifyClientPersonal" should {
      val itsaClient = ItsaClient(nino, postCode)
      val itsaClientNoPostcode = ItsaClient(nino, None)
      val irvClient = IrvClient(nino, dob)
      val irvClientNoDob = IrvClient(nino, None)
      val vatClient = VatClient(vrn, vatRegDate)
      val vatClientNoVatRegDate = VatClient(vrn, None)

      "transition to ConfirmClientItsa when postcode matches and flags are on" in {
        def postcodeMatches(nino: Nino, postcode: String): Future[Some[Boolean]] = Future(Some(true))

        given(IdentifyClientPersonal(HMRCMTDIT)) when submitIdentifyClientItsa(
          postcodeMatches,
          getClientName,
          hasActiveRelationships)(showKFCItsa = true, redirectToConfirmItsa = true)(authorisedAgent)(itsaClient) should thenGo(
          ConfirmClientItsa(Some("John Smith"), Nino(nino), Postcode(postCode.getOrElse(""))))
      }
      "transition to KnownFactNotMatched when postcode does not match and flags are on" in {
        def postcodeDoesNotMatch(nino: Nino, postcode: String): Future[Some[Boolean]] = Future(Some(false))

        given(IdentifyClientPersonal(HMRCMTDIT)) when submitIdentifyClientItsa(
          postcodeDoesNotMatch,
          getClientName,
          hasActiveRelationships)(showKFCItsa = true, redirectToConfirmItsa = true)(authorisedAgent)(itsaClient) should thenGo(
          KnownFactNotMatched)
      }
      "transition to NotSignedUp when client is not enrolled for itsa and flags are on" in {
        def clientNotSignedUp(nino: Nino, postcode: String): Future[Option[Boolean]] = Future(None)

        given(IdentifyClientPersonal(HMRCMTDIT)) when submitIdentifyClientItsa(
          clientNotSignedUp,
          getClientName,
          hasActiveRelationships)(showKFCItsa = true, redirectToConfirmItsa = true)(authorisedAgent)(itsaClient) should thenGo(
          NotSignedUp(HMRCMTDIT))
      }
      "transition to ConfirmClientItsa with no postcode when KF flag is off" in {
        def postcodeDoesNotMatch(nino: Nino, postcode: String): Future[Some[Boolean]] = Future(Some(false))

        given(IdentifyClientPersonal(HMRCMTDIT)) when submitIdentifyClientItsa(
          postcodeDoesNotMatch,
          getClientName,
          hasActiveRelationships)(showKFCItsa = false, redirectToConfirmItsa = true)(authorisedAgent)(itsaClient) should thenGo(
          ConfirmClientItsa(Some("John Smith"), Nino(nino), Postcode(postCode.getOrElse(""))))
      }
      "transition to ConfirmCancel when redirect to confirm flag is off" in {
        def postcodeMatches(nino: Nino, postcode: String): Future[Some[Boolean]] = Future(Some(true))

        given(IdentifyClientPersonal(HMRCMTDIT)) when submitIdentifyClientItsa(
          postcodeMatches,
          getClientName,
          hasActiveRelationships)(showKFCItsa = false, redirectToConfirmItsa = false)(authorisedAgent)(itsaClient) should thenGo(
          ConfirmCancel(HMRCMTDIT, Some("John Smith"), nino))
      }
      "throw an Exception when the client has no postcode and the KF flag is on" in {
        def postcodeNotMatches(nino: Nino, postcode: String): Future[Some[Boolean]] = Future(Some(false))

        intercept[Exception] {
          given(IdentifyClientPersonal(HMRCMTDIT)) when submitIdentifyClientItsa(
            postcodeNotMatches,
            getClientName,
            hasActiveRelationships)(showKFCItsa = true, redirectToConfirmItsa = true)(authorisedAgent)(
            itsaClientNoPostcode)
        }.getMessage shouldBe "Postcode expected but none found"
      }
      "transition to ConfirmClientIrv when dob matches and flags are on" in {
        def dobMatches(nino: Nino, localDate: LocalDate): Future[Some[Boolean]] = Future(Some(true))

        given(IdentifyClientPersonal(HMRCPIR)) when submitIdentifyClientIrv(
          dobMatches,
          getClientName,
          hasActiveRelationships)(showKFCIrv = true, redirectToConfirmIrv = true)(authorisedAgent)(irvClient) should thenGo(
          ConfirmClientIrv(Some("John Smith"), Nino(nino), DOB(dob.getOrElse(""))))
      }
      "transition to KnownFactNotMatched when dob does not match and flags are on" in {
        def dobDoesNotMatch(nino: Nino, localDate: LocalDate): Future[Some[Boolean]] = Future(Some(false))

        given(IdentifyClientPersonal(HMRCPIR)) when submitIdentifyClientIrv(
          dobDoesNotMatch,
          getClientName,
          hasActiveRelationships)(showKFCIrv = true, redirectToConfirmIrv = true)(authorisedAgent)(irvClient) should thenGo(
          KnownFactNotMatched)
      }
      "transition to NotSignedUp when client endpoint returns None and flags are on" in {
        def clientNotSignedUp(nino: Nino, localDate: LocalDate): Future[Option[Boolean]] = Future(None)

        given(IdentifyClientPersonal(HMRCPIR)) when submitIdentifyClientIrv(
          clientNotSignedUp,
          getClientName,
          hasActiveRelationships)(showKFCIrv = true, redirectToConfirmIrv = true)(authorisedAgent)(irvClient) should thenGo(
          NotSignedUp(HMRCPIR))
      }
      "transition to ConfirmClientIrv with no dob when KF flag is off" in {
        def dobNotMatches(nino: Nino, localDate: LocalDate): Future[Some[Boolean]] = Future(Some(false))

        given(IdentifyClientPersonal(HMRCPIR)) when submitIdentifyClientIrv(
          dobNotMatches,
          getClientName,
          hasActiveRelationships)(showKFCIrv = false, redirectToConfirmIrv = true)(authorisedAgent)(irvClient) should thenGo(
          ConfirmClientIrv(Some("John Smith"), Nino(nino), DOB(dob.getOrElse(""))))
      }
      "transition to ConfirmCancel when redirect to confirm irv flag is off" in {
        def dobNotMatches(nino: Nino, localDate: LocalDate): Future[Some[Boolean]] = Future(Some(false))

        given(IdentifyClientPersonal(HMRCPIR)) when submitIdentifyClientIrv(
          dobNotMatches,
          getClientName,
          hasActiveRelationships)(showKFCIrv = false, redirectToConfirmIrv = false)(authorisedAgent)(irvClient) should thenGo(
          ConfirmCancel(HMRCPIR, Some("John Smith"), nino))
      }
      "throw an Exception when the client has no dob and the KF flag is on" in {
        def dobNotMatches(nino: Nino, localDate: LocalDate): Future[Some[Boolean]] = Future(Some(false))

        intercept[Exception] {
          given(IdentifyClientPersonal(HMRCPIR)) when submitIdentifyClientIrv(
            dobNotMatches,
            getClientName,
            hasActiveRelationships)(showKFCIrv = true, redirectToConfirmIrv = true)(authorisedAgent)(irvClientNoDob)
        }.getMessage shouldBe "Date of birth expected but none found"
      }
      "transition to ConfirmClientVat when vat reg date matches and flags are on" in {
        def vatRegDateMatches(vrn: Vrn, vatRegDate: LocalDate): Future[Some[Int]] = Future(Some(204))

        given(IdentifyClientPersonal(HMRCMTDVAT)) when submitIdentifyClientVat(
          vatRegDateMatches,
          getClientName,
          hasActiveRelationships)(showKFCVat = true, redirectToConfirmVat = true)(authorisedAgent)(vatClient) should thenGo(
          ConfirmClientPersonalVat(Some("John Smith"), Vrn(vrn), VatRegDate(vatRegDate.getOrElse(""))))
      }
      "transition to KnownFactNotMatched when vat reg date does not match and flags are on" in {
        def vatRegDateDoesNotMatch(vrn: Vrn, vatRegDate: LocalDate): Future[Some[Int]] = Future(Some(403))

        given(IdentifyClientPersonal(HMRCMTDVAT)) when submitIdentifyClientVat(
          vatRegDateDoesNotMatch,
          getClientName,
          hasActiveRelationships)(showKFCVat = true, redirectToConfirmVat = true)(authorisedAgent)(vatClient) should thenGo(
          KnownFactNotMatched)
      }
      "transition to CannotCreateRequest when there is a data migration in progress and flags are on" in {
        def cannotCreateRequest(vrn: Vrn, vatRegDate: LocalDate): Future[Some[Int]] = Future(Some(423))

        given(IdentifyClientPersonal(HMRCMTDVAT)) when submitIdentifyClientVat(
          cannotCreateRequest,
          getClientName,
          hasActiveRelationships)(showKFCVat = true, redirectToConfirmVat = true)(authorisedAgent)(vatClient) should thenGo(
          CannotCreateRequest)
      }
      "transition to NotSignedUp when client is not enrolled for VAT and flags are on" in {
        def clientNotSignedUp(vrn: Vrn, vatRegDate: LocalDate): Future[Option[Int]] = Future(None)

        given(IdentifyClientPersonal(HMRCMTDVAT)) when submitIdentifyClientVat(
          clientNotSignedUp,
          getClientName,
          hasActiveRelationships)(showKFCVat = true, redirectToConfirmVat = true)(authorisedAgent)(vatClient) should thenGo(
          NotSignedUp(HMRCMTDVAT))
      }
      "transition to ConfirmClientPersonalVat with no vatRegDate when KF flag is off" in {
        def vatRegDateDoesNotMatch(vrn: Vrn, vatRegDate: LocalDate): Future[Some[Int]] = Future(Some(403))

        given(IdentifyClientPersonal(HMRCMTDVAT)) when submitIdentifyClientVat(
          vatRegDateDoesNotMatch,
          getClientName,
          hasActiveRelationships)(showKFCVat = false, redirectToConfirmVat = true)(authorisedAgent)(vatClient) should thenGo(
          ConfirmClientPersonalVat(Some("John Smith"), Vrn(vrn), VatRegDate(vatRegDate.getOrElse(""))))
      }
      "transition to ConfirmCancel when redirect to confirm vat flag is off" in {
        def vatRegDateDoesNotMatch(vrn: Vrn, vatRegDate: LocalDate): Future[Some[Int]] = Future(Some(403))

        given(IdentifyClientPersonal(HMRCMTDVAT)) when submitIdentifyClientVat(
          vatRegDateDoesNotMatch,
          getClientName,
          hasActiveRelationships)(showKFCVat = false, redirectToConfirmVat = false)(authorisedAgent)(vatClient) should thenGo(
          ConfirmCancel(HMRCMTDVAT, Some("John Smith"), vrn))
      }
      "throw an Exception when the client has no vat reg date and the KF flag is on" in {
        def vatRegDateDoesNotMatch(vrn: Vrn, vatRegDate: LocalDate): Future[Some[Int]] = Future(Some(403))

        intercept[Exception] {
          given(IdentifyClientPersonal(HMRCMTDVAT)) when submitIdentifyClientVat(
            vatRegDateDoesNotMatch,
            getClientName,
            hasActiveRelationships)(showKFCVat = true, redirectToConfirmVat = true)(authorisedAgent)(
            vatClientNoVatRegDate)
        }.getMessage shouldBe "Vat registration date expected but none found"
      }
    }

    "at state IdentifyClientBusiness" should {
      val vatClient = VatClient(vrn, vatRegDate)
      "transition to ConfirmClientVat when known fact matches and flags are on" in {
        def vatRegDateMatches(vrn: Vrn, vatRegDate: LocalDate): Future[Some[Int]] = Future(Some(204))

        given(IdentifyClientBusiness) when submitIdentifyClientVat(
          vatRegDateMatches,
          getClientName,
          hasActiveRelationships)(showKFCVat = true, redirectToConfirmVat = true)(authorisedAgent)(vatClient) should thenGo(
          ConfirmClientBusiness(Some("John Smith"), Vrn(vrn), VatRegDate(vatRegDate.getOrElse(""))))
      }
      "transition to KnownFactNotMatched when known fact does not match and flags are on" in {
        def vatRegDateDoesNotMatch(vrn: Vrn, vatRegDate: LocalDate): Future[Some[Int]] = Future(Some(403))

        given(IdentifyClientBusiness) when submitIdentifyClientVat(
          vatRegDateDoesNotMatch,
          getClientName,
          hasActiveRelationships)(showKFCVat = true, redirectToConfirmVat = true)(authorisedAgent)(vatClient) should thenGo(
          KnownFactNotMatched)
      }
      "transition to CannotCreateRequest when there is a data migration in progress and flags are on" in {
        def cannotCreateRequest(vrn: Vrn, vatRegDate: LocalDate): Future[Some[Int]] = Future(Some(423))

        given(IdentifyClientBusiness) when submitIdentifyClientVat(
          cannotCreateRequest,
          getClientName,
          hasActiveRelationships)(showKFCVat = true, redirectToConfirmVat = true)(authorisedAgent)(vatClient) should thenGo(
          CannotCreateRequest)
      }
      "transition to NotSignedUp when client is not enrolled and flags are on" in {
        def clientNotSignedUp(vrn: Vrn, vatRegDate: LocalDate): Future[Option[Int]] = Future(None)

        given(IdentifyClientBusiness) when submitIdentifyClientVat(
          clientNotSignedUp,
          getClientName,
          hasActiveRelationships)(showKFCVat = true, redirectToConfirmVat = true)(authorisedAgent)(vatClient) should thenGo(
          NotSignedUp(HMRCMTDVAT))
      }
      "transition to ConfirmClientBusiness with no vatRegDate when KF flag is off" in {
        def vatRegDateDoesNotMatch(vrn: Vrn, vatRegDate: LocalDate): Future[Some[Int]] = Future(Some(403))

        given(IdentifyClientBusiness) when submitIdentifyClientVat(
          vatRegDateDoesNotMatch,
          getClientName,
          hasActiveRelationships)(showKFCVat = false, redirectToConfirmVat = true)(authorisedAgent)(vatClient) should thenGo(
          ConfirmClientBusiness(Some("John Smith"), Vrn(vrn), VatRegDate(vatRegDate.getOrElse(""))))
      }
      "transition to ConfirmCancel when redirect to confirm vat flag is off" in {
        def vatRegDateDoesNotMatch(vrn: Vrn, vatRegDate: LocalDate): Future[Some[Int]] = Future(Some(403))

        given(IdentifyClientBusiness) when submitIdentifyClientVat(
          vatRegDateDoesNotMatch,
          getClientName,
          hasActiveRelationships)(showKFCVat = false, redirectToConfirmVat = false)(authorisedAgent)(vatClient) should thenGo(
          ConfirmCancel(HMRCMTDVAT, Some("John Smith"), vrn))
      }
    }
    "at state ConfirmClientItsa" should {
      "transition to ConfirmCancel when YES is selected" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: String) = Future(true)

        given(ConfirmClientItsa(Some("Lucy Rose"), Nino(nino), Postcode(postCode.getOrElse("")))) when
          clientConfirmed(hasActiveRelationships)(authorisedAgent)(Confirmation(true)) should thenGo(
          ConfirmCancel(HMRCMTDIT, Some("Lucy Rose"), nino)
        )
      }
      "transition to root when NO is selected" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: String) = Future(true)

        given(ConfirmClientItsa(Some("Lucy Rose"), Nino(nino), Postcode(postCode.getOrElse("")))) when
          clientConfirmed(hasActiveRelationships)(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType
        )
      }
      "transition to NotAuthorised when there are no active relationships" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: String) = Future(false)

        given(ConfirmClientItsa(Some("Lucy Rose"), Nino(nino), Postcode(postCode.getOrElse("")))) when
          clientConfirmed(hasActiveRelationships)(authorisedAgent)(Confirmation(true)) should thenGo(
          NotAuthorised(HMRCMTDIT)
        )
      }
    }
    "at state ConfirmClientIrv" should {
      "transition to ConfirmCancel when YES is selected" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: String) = Future(true)

        given(ConfirmClientIrv(Some("Lucy Rose"), Nino(nino), DOB(dob.getOrElse("")))) when
          clientConfirmed(hasActiveRelationships)(authorisedAgent)(Confirmation(true)) should thenGo(
          ConfirmCancel(HMRCPIR, Some("Lucy Rose"), nino)
        )
      }
      "transition to root when NO is selected" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: String) = Future(true)

        given(ConfirmClientIrv(Some("Lucy Rose"), Nino(nino), DOB(dob.getOrElse("")))) when
          clientConfirmed(hasActiveRelationships)(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType
        )
      }
      "transition to NotAuthorised when there are no active relationships" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: String) = Future(false)

        given(ConfirmClientIrv(Some("Lucy Rose"), Nino(nino), DOB(dob.getOrElse("")))) when
          clientConfirmed(hasActiveRelationships)(authorisedAgent)(Confirmation(true)) should thenGo(
          NotAuthorised(HMRCPIR)
        )
      }
    }
    "at state ConfirmClientPersonalVat" should {
      "transition to ConfirmCancel when YES is selected" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: String) = Future(true)

        given(ConfirmClientPersonalVat(Some("Lucy Rose"), Vrn(vrn), VatRegDate(vatRegDate.getOrElse("")))) when
          clientConfirmed(hasActiveRelationships)(authorisedAgent)(Confirmation(true)) should thenGo(
          ConfirmCancel(HMRCMTDVAT, Some("Lucy Rose"), vrn)
        )
      }
      "transition to root when NO is selected" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: String) = Future(true)

        given(ConfirmClientPersonalVat(Some("Lucy Rose"), Vrn(vrn), VatRegDate(vatRegDate.getOrElse("")))) when
          clientConfirmed(hasActiveRelationships)(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType
        )
      }
      "transition to NotAuthorised when there are no active relationships" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: String) = Future(false)

        given(ConfirmClientPersonalVat(Some("Lucy Rose"), Vrn(vrn), VatRegDate(vatRegDate.getOrElse("")))) when
          clientConfirmed(hasActiveRelationships)(authorisedAgent)(Confirmation(true)) should thenGo(
          NotAuthorised(HMRCMTDVAT)
        )
      }
    }
    "at state ConfirmClientBusiness" should {
      "transition to ConfirmCancel when YES is selected" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: String) = Future(true)

        given(ConfirmClientBusiness(Some("Lucy Rose"), Vrn(vrn), VatRegDate(vatRegDate.getOrElse("")))) when
          clientConfirmed(hasActiveRelationships)(authorisedAgent)(Confirmation(true)) should thenGo(
          ConfirmCancel(HMRCMTDVAT, Some("Lucy Rose"), vrn)
        )
      }
      "transition to root when NO is selected" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: String) = Future(true)

        given(ConfirmClientBusiness(Some("Lucy Rose"), Vrn(vrn), VatRegDate(vatRegDate.getOrElse("")))) when
          clientConfirmed(hasActiveRelationships)(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType
        )
      }
      "transition to NotAuthorised when there are no active relationships" in {
        def hasActiveRelationships(arn: Arn, clientId: String, service: String) = Future(false)

        given(ConfirmClientBusiness(Some("Lucy Rose"), Vrn(vrn), VatRegDate(vatRegDate.getOrElse("")))) when
          clientConfirmed(hasActiveRelationships)(authorisedAgent)(Confirmation(true)) should thenGo(
          NotAuthorised(HMRCMTDVAT)
        )
      }
    }
    "at state ConfirmCancel" should {
      "transition to AuthorisationCancelled when YES is selected" in {
        def deleteRelationship(service: String, arn: Arn, clientId: String) = Future(Some(true))
        def getAgencyName(arn: Arn) = Future("Popeye")

        given(ConfirmCancel(HMRCMTDIT, Some("Holly Herndon"), nino)) when cancelConfirmed(
          deleteRelationship,
          getAgencyName)(authorisedAgent)(Confirmation(true)) should thenGo(
          AuthorisationCancelled(HMRCMTDIT, Some("Holly Herndon"), "Popeye")
        )
      }
      "transiiton to select client type when NO is selected" in {
        def deleteRelationship(service: String, arn: Arn, clientId: String) = Future(Some(true))
        def getAgencyName(arn: Arn) = Future("Popeye")

        given(ConfirmCancel(HMRCMTDIT, Some("Holly Herndon"), nino)) when cancelConfirmed(
          deleteRelationship,
          getAgencyName)(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType
        )
      }
      "transition to ResponseFailed when the relationship termination fails" in {
        def deleteRelationship(service: String, arn: Arn, clientId: String) = Future(Some(false))
        def getAgencyName(arn: Arn) = Future("Popeye")

        given(ConfirmCancel(HMRCMTDIT, Some("Holly Herndon"), nino)) when cancelConfirmed(
          deleteRelationship,
          getAgencyName)(authorisedAgent)(Confirmation(true)) should thenGo(
          ResponseFailed
        )
      }
    }
  }
}
