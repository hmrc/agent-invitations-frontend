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
  val availableServices = Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)
  val nino = "AB123456A"
  val postCode = Some("BN114AW")
  val vrn = "123456"
  val vatRegDate = Some("2010-10-10")
  val dob = Some("1990-10-10")

  "AgentLedDeauthJourneyModel" when {
    "at state ClientType" should {
      "transition to SelectServicePersonal when personal is selected" in {
        given(SelectClientType) when chosenClientType(authorisedAgent)(ClientType.personal) should thenGo(
          SelectServicePersonal(availableServices))
      }
      "transition to SelectServiceBusiness when business is selected" in {
        given(SelectClientType) when chosenClientType(authorisedAgent)(ClientType.business) should thenGo(
          SelectServiceBusiness)
      }
    }
    "at state SelectServicePersonal" should {
      "transition to IdentifyClientPersonal when service is ITSA" in {
        given(SelectServicePersonal(availableServices)) when chosenPersonalService(authorisedAgent)(HMRCMTDIT) should thenGo(
          IdentifyClientPersonal(HMRCMTDIT)
        )
      }
      "transition to IdentifyClientPersonal when service is PIR" in {
        given(SelectServicePersonal(availableServices)) when chosenPersonalService(authorisedAgent)(HMRCPIR) should thenGo(
          IdentifyClientPersonal(HMRCPIR)
        )
      }
      "transition to IdentifyClientPersonal when service is VAT" in {
        given(SelectServicePersonal(availableServices)) when chosenPersonalService(authorisedAgent)(HMRCMTDVAT) should thenGo(
          IdentifyClientPersonal(HMRCMTDVAT)
        )
      }
    }
    "at state SelectServiceBusiness" should {
      "transition to IdentifyClientBusiness when YES is selected" in {
        given(SelectServiceBusiness) when chosenBusinessService(authorisedAgent)(Confirmation(true)) should thenGo(
          IdentifyClientBusiness
        )
      }
      "transition to ClientType when NO is selected" in {
        given(SelectServiceBusiness) when chosenBusinessService(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType
        )
      }
    }

    def getClientName(clientId: String, service: String) = Future(Some("John Smith"))

    "at state IdentifyClientItsa" should {
      val itsaClient = ItsaClient(nino, postCode)

      "transition to ConfirmClientItsa when known fact matches" in {
        def postcodeMatches(nino: Nino, postcode: String): Future[Some[Boolean]] = Future(Some(true))

        given(IdentifyClientPersonal(HMRCMTDIT)) when submitIdentifyClientItsa(postcodeMatches, getClientName)(
          authorisedAgent)(itsaClient) should thenGo(
          ConfirmClientItsa(Some("John Smith"), Nino(nino), Postcode(postCode.getOrElse(""))))
      }
      "transition to KnownFactNotMatched when known fact does not match" in {
        def postcodeDoesNotMatch(nino: Nino, postcode: String): Future[Some[Boolean]] = Future(Some(false))

        given(IdentifyClientPersonal(HMRCMTDIT)) when submitIdentifyClientItsa(postcodeDoesNotMatch, getClientName)(
          authorisedAgent)(itsaClient) should thenGo(KnownFactNotMatched)
      }
      "transition to NotSignedUp when client is not enrolled" in {
        def clientNotSignedUp(nino: Nino, postcode: String): Future[Option[Boolean]] = Future(None)

        given(IdentifyClientPersonal(HMRCMTDIT)) when submitIdentifyClientItsa(clientNotSignedUp, getClientName)(
          authorisedAgent)(itsaClient) should thenGo(NotSignedUp(HMRCMTDIT))

      }
    }

    "at state IdentifyClientIrv" should {
      val irvClient = IrvClient(nino, dob)

      "transition to ConfirmClientIrv when known fact matches" in {
        def dobMatches(nino: Nino, localDate: LocalDate): Future[Some[Boolean]] = Future(Some(true))

        given(IdentifyClientPersonal(HMRCPIR)) when submitIdentifyClientIrv(dobMatches, getClientName)(authorisedAgent)(
          irvClient) should thenGo(ConfirmClientIrv(Some("John Smith"), Nino(nino), DOB(dob.getOrElse(""))))
      }
      "transition to KnownFactNotMatched when known fact does not match" in {
        def dobDoesNotMatch(nino: Nino, localDate: LocalDate): Future[Some[Boolean]] = Future(Some(false))

        given(IdentifyClientPersonal(HMRCPIR)) when submitIdentifyClientIrv(dobDoesNotMatch, getClientName)(
          authorisedAgent)(irvClient) should thenGo(KnownFactNotMatched)
      }
      "transition to NotSignedUp when client is not enrolled" in {
        def clientNotSignedUp(nino: Nino, localDate: LocalDate): Future[Option[Boolean]] = Future(None)

        given(IdentifyClientPersonal(HMRCPIR)) when submitIdentifyClientIrv(clientNotSignedUp, getClientName)(
          authorisedAgent)(irvClient) should thenGo(NotSignedUp(HMRCPIR))
      }
    }

    "at state IdentifyClientVat" should {
      val vatClient = VatClient(vrn, vatRegDate)

      "transition to ConfirmClientVat when known fact matches" in {
        def vatRegDateMatches(vrn: Vrn, vatRegDate: LocalDate): Future[Some[Int]] = Future(Some(204))

        given(IdentifyClientPersonal(HMRCMTDVAT)) when submitIdentityClientVat(vatRegDateMatches, getClientName)(authorisedAgent)(
          vatClient) should thenGo(ConfirmClientPersonalVat(Some("John Smith"), Vrn(vrn), VatRegDate(vatRegDate.getOrElse(""))))
      }
      "transition to KnownFactNotMatched when known fact does not match" in {
        def vatRegDateDoesNotMatch(vrn: Vrn, vatRegDate: LocalDate): Future[Some[Int]] = Future(Some(403))

        given(IdentifyClientPersonal(HMRCMTDVAT)) when submitIdentityClientVat(vatRegDateDoesNotMatch, getClientName)(
          authorisedAgent)(vatClient) should thenGo(KnownFactNotMatched)
      }
      "transition to CannotCreateRequest when there is a data migration in progress" in {
        def cannotCreateRequest(vrn: Vrn, vatRegDate: LocalDate): Future[Some[Int]] = Future(Some(423))

        given(IdentifyClientPersonal(HMRCMTDVAT)) when submitIdentityClientVat(cannotCreateRequest, getClientName)(
          authorisedAgent)(vatClient) should thenGo(CannotCreateRequest)
      }
      "transition to NotSignedUp when client is not enrolled" in {
        def clientNotSignedUp(vrn: Vrn, vatRegDate: LocalDate): Future[Option[Int]] = Future(None)

        given(IdentifyClientPersonal(HMRCMTDVAT)) when submitIdentityClientVat(clientNotSignedUp, getClientName)(
          authorisedAgent)(vatClient) should thenGo(NotSignedUp(HMRCMTDVAT))
      }
    }
    "at state IdentityClientBusiness" should {
      val vatClient = VatClient(vrn, vatRegDate)

      "transition to ConfirmClientVat when known fact matches" in {
        def vatRegDateMatches(vrn: Vrn, vatRegDate: LocalDate): Future[Some[Int]] = Future(Some(204))

        given(IdentifyClientPersonal(HMRCMTDVAT)) when submitIdentityClientVat(vatRegDateMatches, getClientName)(authorisedAgent)(
          vatClient) should thenGo(ConfirmClientBusiness(Some("John Smith"), Vrn(vrn), VatRegDate(vatRegDate.getOrElse(""))))
      }
      "transition to KnownFactNotMatched when known fact does not match" in {
        def vatRegDateDoesNotMatch(vrn: Vrn, vatRegDate: LocalDate): Future[Some[Int]] = Future(Some(403))

        given(IdentifyClientPersonal(HMRCMTDVAT)) when submitIdentityClientVat(vatRegDateDoesNotMatch, getClientName)(
          authorisedAgent)(vatClient) should thenGo(KnownFactNotMatched)
      }
      "transition to CannotCreateRequest when there is a data migration in progress" in {
        def cannotCreateRequest(vrn: Vrn, vatRegDate: LocalDate): Future[Some[Int]] = Future(Some(423))

        given(IdentifyClientPersonal(HMRCMTDVAT)) when submitIdentityClientVat(cannotCreateRequest, getClientName)(
          authorisedAgent)(vatClient) should thenGo(CannotCreateRequest)
      }
      "transition to NotSignedUp when client is not enrolled" in {
        def clientNotSignedUp(vrn: Vrn, vatRegDate: LocalDate): Future[Option[Int]] = Future(None)

        given(IdentifyClientPersonal(HMRCMTDVAT)) when submitIdentityClientVat(clientNotSignedUp, getClientName)(
          authorisedAgent)(vatClient) should thenGo(NotSignedUp(HMRCMTDVAT))
      }
    }
  }
}
