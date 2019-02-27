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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.States._
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
      "transition to SelectClientType given showSelectClientType" in {
        given(SelectClientType(emptyBasket)) when showSelectClientType(authorisedAgent) should thenGo(
          SelectClientType(emptyBasket))
      }
      "transition to SelectPersonalService" in {
        given(SelectClientType(emptyBasket)) when selectedClientType(authorisedAgent)(ClientType.personal) should thenGo(
          SelectPersonalService(availableServices, emptyBasket))
      }
      "transition to SelectBusinessService" in {
        given(SelectClientType(emptyBasket)) when selectedClientType(authorisedAgent)(ClientType.business) should thenGo(
          SelectBusinessService(emptyBasket))
      }
    }

    "at state SelectPersonalService" should {
      "transition to SelectClientType" in {
        given(SelectPersonalService(availableServices, emptyBasket)) when start should thenGo(
          SelectClientType(emptyBasket))
      }
      "transition to IdentifyPersonalClient" in {
        await(
          given(SelectPersonalService(availableServices, emptyBasket)) when selectedPersonalService(authorisedAgent)(
            HMRCMTDIT)) should thenGo(IdentifyPersonalClient(HMRCMTDIT, emptyBasket))
      }
      "transition to SelectPersonalService" in {
        await(
          given(SelectPersonalService(availableServices, emptyBasket)) when selectedPersonalService(authorisedAgent)(
            "foo")) should thenGo(SelectPersonalService(availableServices, emptyBasket))
      }
    }

    "at state SelectBusinessService" should {
      "transition to SelectClientType" in {
        given(SelectBusinessService(emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }
      "after selectedBusinessService(true) transition to IdentifyBusinessClient" in {
        given(SelectBusinessService(emptyBasket)) when selectedBusinessService(authorisedAgent)(Confirmation(true)) should thenGo(
          IdentifyBusinessClient(emptyBasket))
      }
      "after selectedBusinessService(false) transition to SelectClientType" in {
        given(SelectBusinessService(emptyBasket)) when selectedBusinessService(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType(emptyBasket))
      }
    }

    "at state IdentifyPersonalClient" should {

      def clientName(service: String, clientId: String) = Future(Some("Piglet"))
      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)

      "transition to SelectClientType" in {
        given(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }
      "transition to ConfirmClientItsa" in {
        def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(true))
        given(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)) when identifiedItsaClient(checkPostcodeMatches)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(clientName)(authorisedAgent)(
          ItsaClient("AB123456A", Some("BN114AW"))) should matchPattern {
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
      "transition to KnownFactsNotMatched when the nino and postcode do not match" in {
        def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(false))

        given(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)) when identifiedItsaClient(checkPostcodeMatches)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(clientName)(authorisedAgent)(
          ItsaClient("AB123456A", Some("BN114AW"))) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }
      "transition to PendingInvitationExists for itsa service" in {
        def hasPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(true)
        def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(true))
        given(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)) when identifiedItsaClient(checkPostcodeMatches)(
          hasPendingInvitation)(hasNoActiveRelationship)(clientName)(authorisedAgent)(
          ItsaClient("AB123456A", Some("BN114AW"))) should thenGo(PendingInvitationExists(personal, emptyBasket))
      }
      "transition to ActiveRelationshipExists for itsa service" in {
        def hasActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(true)
        def checkPostcodeMatches(nino: Nino, postcode: String) = Future(Some(true))
        given(IdentifyPersonalClient(HMRCMTDIT, emptyBasket)) when identifiedItsaClient(checkPostcodeMatches)(
          hasNoPendingInvitation)(hasActiveRelationship)(clientName)(authorisedAgent)(
          ItsaClient("AB123456A", Some("BN114AW"))) should thenGo(
          ActiveRelationshipExists(personal, HMRCMTDIT, emptyBasket))
      }
      "transition to ConfirmClientPersonalVat" in {
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(true))
        given(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket)) when identifiedVatClient(checkRegDateMatches)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(clientName)(authorisedAgent)(
          VatClient("123456", Some("2010-10-10"))) should
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
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(false))
        given(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket)) when identifiedVatClient(checkRegDateMatches)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(clientName)(authorisedAgent)(
          VatClient("123456", Some("2010-10-10"))) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }
      "transition to PendingInvitationExists for vat service" in {
        def hasPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(true)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(true))
        given(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket)) when identifiedVatClient(checkRegDateMatches)(
          hasPendingInvitation)(hasNoActiveRelationship)(clientName)(authorisedAgent)(
          VatClient("123456", Some("2010-10-10"))) should thenGo(PendingInvitationExists(personal, emptyBasket))
      }
      "transition to ActiveRelationshipExists for vat service" in {
        def hasActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(true)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(true))
        given(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket)) when identifiedVatClient(checkRegDateMatches)(
          hasNoPendingInvitation)(hasActiveRelationship)(clientName)(authorisedAgent)(
          VatClient("123456", Some("2010-10-10"))) should thenGo(
          ActiveRelationshipExists(personal, HMRCMTDVAT, emptyBasket))
      }
      "transition to ConfirmClientIrv" in {
        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(true))
        given(IdentifyPersonalClient(HMRCPIR, emptyBasket)) when identifiedIrvClient(checkDobMatches)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(clientName)(authorisedAgent)(
          IrvClient("AB123456A", Some("1990-10-10"))) should matchPattern {
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
      "transition to KnownFactNotMatched when the nino and dob don't match" in {
        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(false))
        given(IdentifyPersonalClient(HMRCPIR, emptyBasket)) when identifiedIrvClient(checkDobMatches)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(clientName)(authorisedAgent)(
          IrvClient("AB123456A", Some("1990-10-10"))) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }
      "transition to PendingInvitationExists for irv service" in {
        def hasPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(true)
        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(true))
        given(IdentifyPersonalClient(HMRCPIR, emptyBasket)) when identifiedIrvClient(checkDobMatches)(
          hasPendingInvitation)(hasNoActiveRelationship)(clientName)(authorisedAgent)(
          IrvClient("AB123456A", Some("1990-10-10"))) should thenGo(PendingInvitationExists(personal, emptyBasket))
      }
      "transition to ActiveRelationshipExists for irv service" in {
        def hasActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(true)
        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(Some(true))
        given(IdentifyPersonalClient(HMRCPIR, emptyBasket)) when identifiedIrvClient(checkDobMatches)(
          hasNoPendingInvitation)(hasActiveRelationship)(clientName)(authorisedAgent)(
          IrvClient("AB123456A", Some("1990-10-10"))) should thenGo(
          ActiveRelationshipExists(personal, HMRCPIR, emptyBasket))
      }
      "transition to ClientNotSignedUp when client not found" in {
        def checkDobMatches(nino: Nino, dob: LocalDate) = Future(None)
        given(IdentifyPersonalClient(HMRCPIR, emptyBasket)) when identifiedIrvClient(checkDobMatches)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(clientName)(authorisedAgent)(
          IrvClient("AB123456A", Some("1990-10-10"))) should
          thenGo(ClientNotSignedUp(HMRCPIR, emptyBasket))
      }

    }

    "at state IdentifyBusinessClient" should {
      def clientName(service: String, clientId: String) = Future(Some("Piglet"))
      def hasNoPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)
      def hasNoActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
        Future.successful(false)

      "transition to SelectClientType" in {
        given(IdentifyBusinessClient(emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }
      "transition to ConfirmClientBusinessVat" in {
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(true))
        given(IdentifyBusinessClient(emptyBasket)) when identifiedVatClient(checkRegDateMatches)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(clientName)(authorisedAgent)(
          VatClient("123456", Some("2010-10-10"))) should matchPattern {
          case (
              ConfirmClientBusinessVat(
                AuthorisationRequest(
                  "Piglet",
                  VatInvitation(Some(business), Vrn("123456"), Some(VatRegDate("2010-10-10")), HMRCMTDVAT, "vrn"),
                  AuthorisationRequest.NEW,
                  _),
                `emptyBasket`),
              _) =>
        }
      }
      "transition to KnownFactNotMatched client" in {
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(false))
        given(IdentifyBusinessClient(emptyBasket)) when identifiedVatClient(checkRegDateMatches)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(clientName)(authorisedAgent)(
          VatClient("123456", Some("2010-10-10"))) should
          thenGo(KnownFactNotMatched(emptyBasket))
      }
      "transition to PendingInvitationExists" in {
        def hasPendingInvitation(arn: Arn, clientId: String, service: String): Future[Boolean] = Future.successful(true)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(true))
        given(IdentifyBusinessClient(emptyBasket)) when identifiedVatClient(checkRegDateMatches)(hasPendingInvitation)(
          hasNoActiveRelationship)(clientName)(authorisedAgent)(VatClient("123456", Some("2010-10-10"))) should thenGo(
          PendingInvitationExists(business, emptyBasket))
      }
      "transition to ActiveRelationshipExists" in {
        def hasActiveRelationship(arn: Arn, clientId: String, service: String): Future[Boolean] =
          Future.successful(true)
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(Some(true))
        given(IdentifyBusinessClient(emptyBasket)) when identifiedVatClient(checkRegDateMatches)(
          hasNoPendingInvitation)(hasActiveRelationship)(clientName)(authorisedAgent)(
          VatClient("123456", Some("2010-10-10"))) should thenGo(
          ActiveRelationshipExists(business, HMRCMTDVAT, emptyBasket))
      }
      "transition to ClientNotSignedUp" in {
        def checkRegDateMatches(vrn: Vrn, regDate: LocalDate) = Future(None)
        given(IdentifyBusinessClient(emptyBasket)) when identifiedVatClient(checkRegDateMatches)(
          hasNoPendingInvitation)(hasNoActiveRelationship)(clientName)(authorisedAgent)(
          VatClient("123456", Some("2010-10-10"))) should
          thenGo(ClientNotSignedUp(HMRCMTDVAT, emptyBasket))
      }
    }

    "at ConfirmClientItsa" should {
      "transition to SelectClientType" in {
        given(
          ConfirmClientItsa(
            AuthorisationRequest("Piglet", ItsaInvitation(Nino("AB123456A"), Some(Postcode("BN114AW")))),
            emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }
      "transition to ReviewAuthorisationsPersonal" in {
        given(
          ConfirmClientItsa(
            AuthorisationRequest("Piglet", ItsaInvitation(Nino("AB123456A"), Some(Postcode("BN114AW")))),
            emptyBasket)) when clientConfirmed(authorisedAgent)(Confirmation(true)) should thenMatch { case ReviewAuthorisationsPersonal(basket) if basket.nonEmpty => }
      }
      "transition to SelectPersonalService" in {
        given(
          ConfirmClientItsa(
            AuthorisationRequest("Piglet", ItsaInvitation(Nino("AB123456A"), Some(Postcode("BN114AW")))),
            emptyBasket)) when clientConfirmed(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyPersonalClient(HMRCMTDIT, emptyBasket))
      }
    }

    "at ConfirmClientIrv" should {
      "transition to SelectClientType" in {
        given(
          ConfirmClientIrv(
            AuthorisationRequest("Piglet", PirInvitation(Nino("AB123456A"), Some(DOB("1990-10-10")))),
            emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }
      "transition to ReviewAuthorisationsPersonal" in {
        given(
          ConfirmClientIrv(
            AuthorisationRequest("Piglet", PirInvitation(Nino("AB123456A"), Some(DOB("1990-10-10")))),
            emptyBasket)) when clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenMatch { case ReviewAuthorisationsPersonal(basket) if basket.nonEmpty => }
      }
      "transition to SelectPersonalService" in {
        given(
          ConfirmClientIrv(
            AuthorisationRequest("Piglet", PirInvitation(Nino("AB123456A"), Some(DOB("1990-10-10")))),
            emptyBasket)) when clientConfirmed(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyPersonalClient(HMRCPIR, emptyBasket))
      }
    }

    "at ConfirmClientPersonalVat" should {
      "transition to Start" in {
        given(ConfirmClientPersonalVat(
          AuthorisationRequest("Piglet", VatInvitation(Some(personal), Vrn("123456"), Some(VatRegDate("2010-10-10")))),
          emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }
      "transition to ClientConfirmedPersonal" in {
        given(ConfirmClientPersonalVat(
          AuthorisationRequest("Piglet", VatInvitation(Some(personal), Vrn("123456"), Some(VatRegDate("2010-10-10")))),
          emptyBasket)) when clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenMatch { case ReviewAuthorisationsPersonal(basket) if basket.nonEmpty => }
      }
      "transition to PersonalServiceSelected" in {
        given(ConfirmClientPersonalVat(
          AuthorisationRequest("Piglet", VatInvitation(Some(personal), Vrn("123456"), Some(VatRegDate("2010-10-10")))),
          emptyBasket)) when clientConfirmed(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyPersonalClient(HMRCMTDVAT, emptyBasket))
      }
    }

    "at ConfirmClientBusinessVat" should {
      "after start transition to Start" in {
        given(ConfirmClientBusinessVat(
          AuthorisationRequest("Piglet", VatInvitation(Some(business), Vrn("123456"), Some(VatRegDate("2010-10-10")))),
          emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }
      "after clientConfirmed(true) transition to ReviewAuthorisationsBusiness" in {
        given(ConfirmClientBusinessVat(
          AuthorisationRequest("Piglet", VatInvitation(Some(business), Vrn("123456"), Some(VatRegDate("2010-10-10")))),
          emptyBasket)) when clientConfirmed(authorisedAgent)(Confirmation(true)) should
          thenMatch { case ReviewAuthorisationsBusiness(basket) if basket.nonEmpty => }
      }
      "after clientConfirmed(false) transition to IdentifyBusinessClient" in {
        given(ConfirmClientBusinessVat(
          AuthorisationRequest("Piglet", VatInvitation(Some(business), Vrn("123456"), Some(VatRegDate("2010-10-10")))),
          emptyBasket)) when clientConfirmed(authorisedAgent)(Confirmation(false)) should
          thenGo(IdentifyBusinessClient(emptyBasket))
      }
    }

    "at ReviewAuthorisationsPersonal" should {
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      "after start transition to Start" in {
        given(ReviewAuthorisationsPersonal(emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }
      "after authorisationsReviewed(true) transition to SelectPersonalService" in {
        def createMultipleInvitations(
          arn: Arn,
          clientType: Option[ClientType],
          requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] = Future(emptyBasket)

        given(ReviewAuthorisationsPersonal(emptyBasket)) when authorisationsReviewed(createMultipleInvitations)(
          getAgentLink)(authorisedAgent)(Confirmation(true)) should
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
          createMultipleInvitations)(getAgentLink)(authorisedAgent)(Confirmation(false)) should
          thenGo(InvitationSentPersonal("invitation/link", None))
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
          createMultipleInvitations)(getAgentLink)(authorisedAgent)(Confirmation(false)) should
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
          createMultipleInvitations)(getAgentLink)(authorisedAgent)(Confirmation(false)) should
          thenGo(SomeAuthorisationsFailed(Set(authorisationRequestNew1, authorisationRequestNew2)))
      }
    }

    "at ReviewAuthorisationsBusiness" should {
      def getAgentLink(arn: Arn, clientType: Option[ClientType]) = Future("invitation/link")
      "after start transition to Start" in {
        given(ReviewAuthorisationsBusiness(emptyBasket)) when start should thenGo(SelectClientType(emptyBasket))
      }
      "after authorisationsReviewed(true) transition to ClientTypeSelected" in {
        def createMultipleInvitations(
          arn: Arn,
          clientType: Option[ClientType],
          requests: Set[AuthorisationRequest]): Future[Set[AuthorisationRequest]] = Future(emptyBasket)

        given(ReviewAuthorisationsBusiness(emptyBasket)) when authorisationsReviewed(createMultipleInvitations)(
          getAgentLink)(authorisedAgent)(Confirmation(true)) should
          thenGo(SelectBusinessService(emptyBasket))
      }
      "after authorisationsReviewed(false) transition to AuthorisationsReviewedBusiness" in {
        val authorisationRequestNew = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(business), HMRCMTDVAT, "123456", Some("2010-10-10")),
          AuthorisationRequest.NEW,
          "ABC123")
        val authorisationRequestCreated = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(business), HMRCMTDVAT, "123456", Some("2010-10-10")),
          AuthorisationRequest.CREATED,
          "ABC123")

        def createMultipleInvitations(arn: Arn, clientType: Option[ClientType], requests: Set[AuthorisationRequest]) =
          Future(Set(authorisationRequestCreated))

        given(ReviewAuthorisationsBusiness(Set(authorisationRequestNew))) when authorisationsReviewed(
          createMultipleInvitations)(getAgentLink)(authorisedAgent)(Confirmation(false)) should
          thenGo(InvitationSentBusiness("invitation/link", None))
      }
      "after authorisationsReviewed(false) when all fail transition to AuthorisationsReviewedAllFailed" in {
        val authorisationRequestNew = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(business), HMRCMTDVAT, "123456", Some("2010-10-10")),
          AuthorisationRequest.NEW,
          "ABC123")
        val authorisationRequestCreated = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(business), HMRCMTDVAT, "123456", Some("2010-10-10")),
          AuthorisationRequest.FAILED,
          "ABC123")

        def createMultipleInvitations(arn: Arn, clientType: Option[ClientType], requests: Set[AuthorisationRequest]) =
          Future(Set(authorisationRequestCreated))

        given(ReviewAuthorisationsBusiness(Set(authorisationRequestNew))) when authorisationsReviewed(
          createMultipleInvitations)(getAgentLink)(authorisedAgent)(Confirmation(false)) should
          thenGo(AllAuthorisationsFailed(Set(authorisationRequestNew)))
      }
      "after authorisationsReviewed(false) when some fail transition to AuthorisationReviewedSomeFailed" in {
        val authorisationRequestNew1 = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(business), HMRCMTDVAT, "123456", Some("2010-10-10")),
          AuthorisationRequest.NEW,
          "ABC123")
        val authorisationRequestNew2 = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(business), HMRCMTDVAT, "123457", Some("2010-10-10")),
          AuthorisationRequest.NEW,
          "ABC124")
        val authorisationRequestCreated = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(business), HMRCMTDVAT, "123456", Some("2010-10-10")),
          AuthorisationRequest.CREATED,
          "ABC123")
        val authorisationRequestFailed = AuthorisationRequest(
          "Mr Client",
          Invitation(Some(business), HMRCMTDVAT, "123457", Some("2010-10-10")),
          AuthorisationRequest.FAILED,
          "ABC124")

        def createMultipleInvitations(arn: Arn, clientType: Option[ClientType], requests: Set[AuthorisationRequest]) =
          Future(Set(authorisationRequestCreated, authorisationRequestFailed))

        given(ReviewAuthorisationsBusiness(Set(authorisationRequestNew1, authorisationRequestNew2))) when authorisationsReviewed(
          createMultipleInvitations)(getAgentLink)(authorisedAgent)(Confirmation(false)) should
          thenGo(SomeAuthorisationsFailed(Set(authorisationRequestNew1, authorisationRequestNew2)))
      }
    }
  }
}
