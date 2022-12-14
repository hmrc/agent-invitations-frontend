package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.time.LocalDate
import org.jsoup.Jsoup
import org.scalatest.BeforeAndAfter
import play.api.Application
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{Business, Personal, Trust}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Service, SuspensionDetails, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class AgentInvitationFastTrackJourneyControllerISpec
    extends BaseISpec with StateAndBreadcrumbsMatchers with BeforeAndAfter {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  override implicit lazy val app: Application = appBuilder
    .overrides(new TestAgentInvitationFastTrackJourneyModule)
    .build()

  lazy val journeyState = app.injector.instanceOf[TestAgentInvitationFastTrackJourneyService]

  lazy val controller: AgentInvitationFastTrackJourneyController =
    app.injector.instanceOf[AgentInvitationFastTrackJourneyController]

  import journeyState.model._

  val availableServices = Set(Service.PersonalIncomeRecord, Service.MtdIt, Service.Vat)
  val emptyBasket = Set.empty[AuthorisationRequest]

  implicit val timeoutDuration: Duration = Helpers.defaultAwaitTimeout.duration

  before {
    journeyState.clear
  }

  "POST /agents/fast-track" should {
    val request = FakeRequest("POST", "/agents/fast-track")

    "redirect to check-details if all values in request are valid with no continue url" when {

      "submitted NINO is uppercase" in {
        givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
        checkAgentFastTract(submittedNinoStr = "AB123456A")
      }

      "submitted NINO is lowercase (APB-3634)" in {
        givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
        checkAgentFastTract(submittedNinoStr = "ab123456a")
      }

      def checkAgentFastTract(submittedNinoStr: String) = {
        val result = controller.agentFastTrack(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "clientType"           -> "personal",
              "service"              -> "HMRC-MTD-IT",
              "clientIdentifierType" -> "ni",
              "clientIdentifier"     -> submittedNinoStr,
              "knownFact"            -> "BN32TN"),
            arn.value
          ))
        status(result) shouldBe 303
        Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)

        val expectedFtr =
          AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino(submittedNinoStr.toUpperCase), Some("BN32TN"))
        journeyState.get shouldBe Some(
          (
            CheckDetailsComplete(fastTrackRequest = expectedFtr, None),
            List(Prologue(None, None))))
      }
    }

    "redirect to check-details if all values in request are valid with a continue and error url query parameters" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
      givenAllowlistedDomains
      val request = FakeRequest(
        "POST",
        "/agents/fast-track?continue=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fselect-client&error=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fnot-authorised"
      )
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "personal",
            "service"              -> "HMRC-MTD-IT",
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "AB123456A",
            "knownFact"            -> "BN32TN"),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to check-details when there is a referer in the header" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
      journeyState.clear
      val request = FakeRequest(
        "POST",
        "/agents/fast-track"
      ).withHeaders("Referer" -> "/some/referer/url")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "personal",
            "service"              -> "HMRC-MTD-IT",
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "AB123456A",
            "knownFact"            -> "BN32TN"),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
      val expectedFtr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("BN32TN"))
      journeyState.get shouldBe Some(
        (
          CheckDetailsComplete(fastTrackRequest = expectedFtr, None),
          List(Prologue(None, Some("/some/referer/url")), Prologue(None, None))))
    }

    "redirect to check details when service is IRV" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "personal",
            "service"              -> "PERSONAL-INCOME-RECORD",
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> nino.value,
            "knownFact"            -> dateOfBirth),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to agent suspended when service is IRV and agent has been suspended for this service" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = true, Some(Set("PIR"))))
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "personal",
            "service"              -> "PERSONAL-INCOME-RECORD",
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> nino.value,
            "knownFact"            -> dateOfBirth),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showSuspended().url)
    }

    "redirect to check details when service is personal VAT" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "personal",
            "service"              -> "HMRC-MTD-VAT",
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> vrn.value,
            "knownFact"            -> validRegistrationDate),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to check details when service is business VAT" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "business",
            "service"              -> "HMRC-MTD-VAT",
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> vrn.value,
            "knownFact"            -> validRegistrationDate),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to check details when service is Trust with utr" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "business",
            "service"              -> "HMRC-TERS-ORG",
            "clientIdentifierType" -> "taxId",
            "clientIdentifier"     -> validUtr.value),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showClientType().url)
    }

    "redirect to check details when service is Trust with urn" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "business",
            "service"              -> "HMRC-TERSNT-ORG",
            "clientIdentifierType" -> "taxId",
            "clientIdentifier"     -> validUrn.value),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showClientType().url)
    }

    "redirect to agent suspended when service is Trust and agent has been suspended for this service" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, Some(Set("TRS"))))
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "business",
            "service"              -> "HMRC-TERS-ORG",
            "clientIdentifierType" -> "utr",
            "clientIdentifier"     -> validUtr.value),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showSuspended().url)
    }

    "redirect to check details when service is CGT" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "business",
            "service"              -> "HMRC-CGT-PD",
            "clientIdentifierType" -> "CGTPDRef",
            "clientIdentifier"     -> cgtRef.value),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to agent suspended when service is CGT and agent has been suspended for this service" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = true, Some(Set("CGT"))))
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "business",
            "service"              -> "HMRC-CGT-PD",
            "clientIdentifierType" -> "CGTPDRef",
            "clientIdentifier"     -> cgtRef.value),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showSuspended().url)
    }

    "redirect to check details when service is PPT" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "business",
            "service"              -> "HMRC-PPT-ORG",
            "clientIdentifierType" -> "EtmpRegistrationNumber",
            "clientIdentifier"     -> pptRef.value),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to check details when service is ITSA with no postcode" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "personal",
            "service"              -> "HMRC-MTD-IT",
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> nino.value),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to check details when service is IRV with no date of birth" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "personal",
            "service"              -> "PERSONAL-INCOME-RECORD",
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> nino.value),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to check details when service is VAT with no vat reg date" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "personal",
            "service"              -> "HMRC-MTD-VAT",
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> vrn.value),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to check details when service is VAT with no vat reg date or client type" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "service"              -> "HMRC-MTD-VAT",
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> vrn.value),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to check details when service is CGT with no client type or postcode" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "service"              -> "HMRC-CGT-PD",
            "clientIdentifierType" -> "CGTPDRef",
            "clientIdentifier"     -> cgtRef.value),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to agent suspended when service is VAT and agent has been suspended for this service" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = true, Some(Set("VATC"))))
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "service"              -> "HMRC-MTD-VAT",
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> vrn.value),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showSuspended().url)
    }



    "redirect to the error url with appended error reason if all values in request are valid with a continue and error url query parameters" in {
      givenAllowlistedDomains
      val request = FakeRequest(
        "POST",
        "/agents/fast-track?continue=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fselect-client&error=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fnot-authorised"
      )
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "personal",
            "service"              -> "foo",
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "AB123456A",
            "knownFact"            -> "BN32TN"),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(
        "http://localhost:9996/tax-history/not-authorised?issue=UNSUPPORTED_SERVICE")
    }

    "throw a Bad Request exception if the continue url is not allowlisted" in {
      givenAllowlistedDomains
      val request = FakeRequest(
        "POST",
        "/agents/fast-track?continue=https://www.google.com&error=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fnot-authorised"
      )
      intercept[BadRequestException] {
        await(
          controller.agentFastTrack(authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "clientType"           -> "personal",
              "service"              -> "foo",
              "clientIdentifierType" -> "ni",
              "clientIdentifier"     -> "AB123456A",
              "knownFact"            -> "BN32TN"),
            arn.value
          )))
      }.getMessage shouldBe "Provided URL [https://www.google.com] doesn't comply with redirect policy"
    }
    "throw a Bad Request exception if the error url is not allowlisted" in {
      givenAllowlistedDomains
      val request = FakeRequest(
        "POST",
        "/agents/fast-track?continue=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fselect-client&error=https://www.google.com"
      )
      intercept[BadRequestException] {
        await(
          controller.agentFastTrack(authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "clientType"           -> "personal",
              "service"              -> "foo",
              "clientIdentifierType" -> "ni",
              "clientIdentifier"     -> "AB123456A",
              "knownFact"            -> "BN32TN"),
            arn.value
          )))
      }.getMessage shouldBe "Provided URL [https://www.google.com] doesn't comply with redirect policy"
    }
    "throw a Bad Request exception if the continue url is invalid" in {
      val request = FakeRequest(
        "POST",
        "/agents/fast-track?continue=foo&error=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fnot-authorised"
      )
      intercept[BadRequestException] {
        await(
          controller.agentFastTrack(authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "clientType"           -> "personal",
              "service"              -> "foo",
              "clientIdentifierType" -> "ni",
              "clientIdentifier"     -> "AB123456A",
              "knownFact"            -> "BN32TN"),
            arn.value
          )))
      }.getMessage startsWith "[foo] is not a valid continue URL"
    }
    "throw a Bad Request exception if the error url is invalid" in {
      val request = FakeRequest(
        "POST",
        "/agents/fast-track?continue=continue=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fselect-client&error=bar"
      )
      intercept[BadRequestException] {
        await(
          controller.agentFastTrack(authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "clientType"           -> "personal",
              "service"              -> "foo",
              "clientIdentifierType" -> "ni",
              "clientIdentifier"     -> "AB123456A",
              "knownFact"            -> "BN32TN"),
            arn.value
          )))
      }.getMessage startsWith "[bar] is not a valid error URL"
    }
  }

  "GET /agent-suspended" should {
    "render the page content" in {
      val request = FakeRequest("GET", "/agents/suspended")
      journeyState.set(
        SuspendedAgent(Service.Vat, None), List()
      )
      val result = controller.showSuspended(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue,
        "agent-suspended.fastrack.heading",
      "agent-suspended.fastrack.p1",
        "agent-suspended.fastrack.p3",
        "agent-suspended.fastrack.p4"
      )
    }
  }

  "GET /agents/check-details" should {
    val request = FakeRequest("GET", "/agents/fast-track/check-details")
    "show the check-details page for ITSA service" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("BN32TN"))
      journeyState.set(
        CheckDetailsComplete(fastTrackRequest = ftr, None),
        List(Prologue(None, None)))

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "check-details.heading",
        "check-details.p.HMRC-MTD-IT",
        "check-details.client-type.personal")
    }
    "show the check-details page for IRV service" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, Nino("AB123456A"), Some(dateOfBirth))
      journeyState.set(CheckDetailsComplete(fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "check-details.heading",
        "check-details.p.PERSONAL-INCOME-RECORD",
        "check-details.client-type.personal")
    }
    "show the check-details page for personal VAT service" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.Vat, vrn, Some(validRegistrationDate))
      journeyState
        .set(CheckDetailsComplete(fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "check-details.heading",
        "check-details.p.HMRC-MTD-VAT",
        "check-details.client-type.personal")
    }
    "show the check-details page for business VAT service" in {
      val ftr = AgentFastTrackRequest(Some(Business), Service.Vat, vrn, Some(validRegistrationDate))
      journeyState
        .set(CheckDetailsComplete(fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "check-details.heading",
        "check-details.p.HMRC-MTD-VAT",
        "check-details.client-type.business.HMRC-MTD-VAT")
    }

    "show the check-details page for Trust service" in {
      val ftr = AgentFastTrackRequest(Some(Business), Service.Trust, validUtr, None)
      journeyState
        .set(CheckDetailsComplete(fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "check-details.heading",
        "check-details.p.HMRC-TERS-ORG",
        "check-details.client-type.business.HMRC-TERS-ORG")
    }

    "show the check-details page for business CGT service" in {
      val ftr = AgentFastTrackRequest(Some(Business), Service.CapitalGains, cgtRef, None)
      journeyState
        .set(CheckDetailsComplete(fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "check-details.heading",
        "check-details.p.HMRC-CGT-PD",
        "check-details.client-type.business.HMRC-CGT-PD")
    }

    "show the check-details page for personal CGT service" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, cgtRef, None)
      journeyState
        .set(CheckDetailsComplete(fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "check-details.heading",
        "check-details.p.HMRC-CGT-PD",
        "check-details.client-type.personal")
    }

    "show the check-details page for personal PPT service" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.Ppt, pptRef, None)
      journeyState
        .set(CheckDetailsComplete(fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "check-details.heading",
        "check-details.p.HMRC-PPT-ORG",
        "check-details.client-type.personal")
    }

    "show the check-details page for ITSA client with no postcode" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), None)
      journeyState.set(CheckDetailsNoPostcode(fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "check-details.heading",
        "check-details.p.HMRC-MTD-IT",
        "check-details.client-type.personal",
        "check-details.change.p1")
    }
    "show the check-details page for IRV client with no date of birth" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, Nino("AB123456A"), None)
      journeyState.set(CheckDetailsNoDob(fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "check-details.heading",
        "check-details.p.PERSONAL-INCOME-RECORD",
        "check-details.client-type.personal",
        "check-details.change.p1")
    }
    "show the check-details page for VAT client with no vat registration date" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.Vat, vrn, None)
      journeyState.set(CheckDetailsNoVatRegDate(fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "check-details.heading",
        "check-details.p.HMRC-MTD-VAT",
        "check-details.client-type.personal",
        "check-details.change.p1")
    }
    "show the check-details page for VAT client with no client type" in {
      val ftr = AgentFastTrackRequest(None, Service.Vat, vrn, None)
      journeyState
        .set(CheckDetailsNoClientTypeVat(fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "check-details.heading",
        "check-details.p.HMRC-MTD-VAT",
        "check-details.change.p1")
    }
  }

  "POST /agents/check-details" should {
    val request = FakeRequest("POST", "/agents/check-details")
    "redirect to invitation-sent" in new ItsaHappyScenario {
      givenGetAgencyEmailAgentStub
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, Some("BN32TN"))
      journeyState.set(
        CheckDetailsComplete(fastTrackRequest = ftr, None),
        List(Prologue(None, None)))

      val result = controller.submitCheckDetails(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
    }

    "redirect to /already-copied-across-itsa" in new ItsaHappyScenario {
      givenGetAgencyEmailAgentStub
      givenLegacySaRelationshipReturnsStatus(arn, nino, 204)
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, Some("BN32TN"))
      journeyState.set(
        CheckDetailsComplete(fastTrackRequest = ftr, None),
        List(Prologue(None, None)))

      val result = controller.submitCheckDetails(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showAlreadyCopiedAcrossItsa().url)
    }

    "redirect to /authorisation-detected" in new ItsaHappyScenario {
      givenGetAgencyEmailAgentStub
      givenLegacySaRelationshipReturnsStatus(arn, nino, 200)
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, Some("BN32TN"))
      journeyState.set(
        CheckDetailsComplete(fastTrackRequest = ftr, None),
        List(Prologue(None, None)))

      val result = controller.submitCheckDetails(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showLegacyAuthorisationDetected().url)
    }


    "redirect to client-identify for a personal service" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, Some("BN114AW"))
      journeyState.set(
        CheckDetailsComplete(fastTrackRequest = ftr, None),
        List(Prologue(None, None)))

      val result = controller.submitCheckDetails(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showIdentifyClient().url)
    }

    "redirect to client-identify for a business service" in {
      val ftr = AgentFastTrackRequest(Some(Business), Service.Vat, vrn, Some(validRegistrationDate))
      journeyState.set(
        CheckDetailsComplete(fastTrackRequest = ftr, None),
        List(Prologue(None, None)))

      val result = controller.submitCheckDetails(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showIdentifyClient().url)
    }

    "redirect to /identify-client for a trust service" in {
      val ftr = AgentFastTrackRequest(Some(Business), Service.Trust, validUtr, None)
      journeyState.set(
        CheckDetailsComplete(fastTrackRequest = ftr, None),
        List(Prologue(None, None)))

      val result = controller.submitCheckDetails(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showIdentifyClient().url)
    }

    "redirect to /identify-client for a CGT service" in {
      val ftr = AgentFastTrackRequest(Some(Business), Service.CapitalGains, cgtRef, None)
      journeyState.set(
        CheckDetailsComplete(fastTrackRequest = ftr, None),
        List(Prologue(None, None)))

      val result = controller.submitCheckDetails(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showIdentifyClient().url)
    }

    "redirect to /identify-client for a PPT service" in {
      val ftr = AgentFastTrackRequest(Some(Business), Service.Ppt, pptRef, None)
      journeyState.set(
        CheckDetailsComplete(fastTrackRequest = ftr, None),
        List(Prologue(None, None)))

      val result = controller.submitCheckDetails(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showIdentifyClient().url)
    }
  }

  "POST /agents/to-known-fact" should {
    val request = FakeRequest("POST", "/agents/to-known-fact")

    "redirect to known fact when the postcode is missing for ITSA service" in {
      givenGetAgencyEmailAgentStub
      val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, None)
      journeyState.set(CheckDetailsNoPostcode(fastTrackRequest = ftr, None), List())

      val result = controller.progressToKnownFact(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showKnownFact().url)
    }

    "redirect to known fact when the dob is missing for PIR service" in {
      givenGetAgencyEmailAgentStub
      val ftr = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, nino, None)
      journeyState.set(CheckDetailsNoDob(fastTrackRequest = ftr, None), List())

      val result = controller.progressToKnownFact(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showKnownFact().url)
    }

    "redirect to known fact when the vat reg date is missing for VAT service" in {
      givenGetAgencyEmailAgentStub
      val ftr = AgentFastTrackRequest(Some(Personal), Service.Vat, vrn, None)
      journeyState.set(CheckDetailsNoVatRegDate(fastTrackRequest = ftr, None), List())

      val result = controller.progressToKnownFact(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showKnownFact().url)
    }

    "redirect to /client-postcode for a UK based CGT client" in {
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription("GB")).toString())
      val ftr = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, cgtRef, None)
      journeyState.set(CheckDetailsComplete(fastTrackRequest = ftr, None), List())

      val result = controller.progressToKnownFact(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showConfirmCgtPostcode().url)
    }

    "redirect to /client-country for a non UK based CGT client" in {
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription()).toString())
      val ftr = AgentFastTrackRequest(Some(Personal), Service.CapitalGains, cgtRef, None)
      journeyState.set(CheckDetailsComplete(fastTrackRequest = ftr, None), List())

      val result = controller.progressToKnownFact(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showConfirmCgtCountryCode().url)
    }

    "redirect to /agents/track/ppt-registration-date for a PPT client" in {
      givenGetPptSubscriptionReturns(pptRef, 200, pptSubscriptionSuccessBodyJson(pptRef, LocalDate.parse("2019-10-10")))
      val ftr = AgentFastTrackRequest(Some(Personal), Service.Ppt, pptRef, None)
      journeyState.set(CheckDetailsComplete(fastTrackRequest = ftr, None), List())

      val result = controller.progressToKnownFact(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showConfirmPptRegDate().url)
    }
  }

  "POST /agents/to-identify-client" should {
    val request = FakeRequest("POST", "/agents/to-identify-client")

    "redirect to identify client" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.Vat, vrn, None)
      journeyState
        .set(CheckDetailsNoVatRegDate(fastTrackRequest = ftr, None), List())

      val result = controller.progressToIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showIdentifyClient().url)
    }
  }

  "GET agents/client-details" should {
    val request = FakeRequest("GET", "/agents/fast-track/identify-client")
    "show the client-details page for ITSA" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("BN114AW"))
      journeyState.set(
        IdentifyClient(ftr, None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "identify-client.header", "identify-client.postcode.label")
    }

    "show the client-details page for IRV" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, Nino("AB123456A"), Some("1990-09-09"))
      journeyState.set(
        IdentifyClient(ftr, None),
        List()
      )

      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "identify-client.header", "identify-client.irv-date-of-birth.label")
    }

    "show the client-details page for personal VAT" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.Vat, vrn, Some("2009-09-09"))
      journeyState.set(
        IdentifyClient(ftr, None),
        List()
      )

      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "identify-client.header", "identify-client.vat-registration-date.label")
    }

    "show the client-details page for business VAT" in {
      val ftr = AgentFastTrackRequest(Some(Business), Service.Vat, vrn, Some("2009-09-09"))
      journeyState.set(
        IdentifyClient(ftr, None),
        List()
      )

      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "identify-client.header", "identify-client.vat-registration-date.label")
    }

    "show the client-details page for trust" in {
      val ftr = AgentFastTrackRequest(Some(Trust), Service.Trust, validUtr, None)
      journeyState.set(
        IdentifyClient(ftr, None),
        List()
      )

      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "identify-trust-client.header.false", "identify-trust-client.p1")
      checkHtmlResultWithBodyText(result.futureValue, "A Unique Taxpayer Reference is 10 numbers, for example 1234567890")
    }

    "show the client-details page for CGT" in {
      val ftr = AgentFastTrackRequest(Some(Business), Service.CapitalGains, cgtRef, None)
      journeyState.set(
        IdentifyClient(ftr, None),
        List()
      )

      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "identify-cgt-client.header", "identify-cgt-client.p1", "identify-cgt-client.hint")
    }

    "show the client-details page for PPT" in {
      val ftr = AgentFastTrackRequest(Some(Business), Service.Ppt, pptRef, None)
      journeyState.set(
        IdentifyClient(ftr, None),
        List()
      )

      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "identify-ppt-client.header", "identify-ppt-client.p1", "identify-ppt-client.hint")
    }

    "show the client-details page when there is no client type for VAT" in {
      val ftr = AgentFastTrackRequest(None, Service.Vat, vrn, Some("2009-09-09"))
      journeyState.set(
        IdentifyClient(ftr, None),
        List()
      )

      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "identify-client.header", "identify-client.vat-registration-date.label")
    }
  }

  "POST /agents/client-identify-itsa" should {
    val request = FakeRequest("POST", "/agents/fast-track/identify-itsa-client")
    "redirect to invitation-sent" when {
      "submitted NINO is uppercase" in new ItsaHappyScenario {
        checkSubmitIdentifyItsaClient(submittedNinoStr = nino.value.toUpperCase)
      }

      "submitted NINO is lowercase (APB-3634)" in new ItsaHappyScenario {
        checkSubmitIdentifyItsaClient(submittedNinoStr = nino.value.toLowerCase)
      }

      def checkSubmitIdentifyItsaClient(submittedNinoStr: String) = {
        val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, Some("BN114AW"))
        journeyState.set(
          IdentifyClient(ftr, None),
          List(
            CheckDetailsComplete(ftr, None),
            Prologue(None, None)
          )
        )

        val result = controller.submitIdentifyItsaClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "clientIdentifier" -> submittedNinoStr,
              "postcode"         -> "BN32TN"
            ),
            arn.value))

        status(result) shouldBe 303
        Helpers.redirectLocation(result) shouldBe Some(
          routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
      }
    }
  }

  "POST /agents/client-identify-irv" should {
    val request = FakeRequest("POST", "/agents/fast-track/identify-irv-client")
    "redirect to invitation-sent" when {
      "submitted NINO is uppercase" in new IrvHappyScenario {
        checkSubmitIdentifyIrvClient(submittedNinoStr = nino.value.toUpperCase)
      }

      "submitted NINO is lowercase (APB-3634)" in new IrvHappyScenario {
        checkSubmitIdentifyIrvClient(submittedNinoStr = nino.value.toLowerCase)
      }

      def checkSubmitIdentifyIrvClient(submittedNinoStr: String) = {
        val ftr = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, nino, Some("1990-10-10"))
        journeyState.set(
          IdentifyClient(ftr, None),
          List(
            CheckDetailsComplete(ftr, None),
            Prologue(None, None)
          )
        )

        val result = controller.submitIdentifyIrvClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "clientIdentifier" -> submittedNinoStr,
              "dob.year"         -> "1990",
              "dob.month"        -> "10",
              "dob.day"          -> "10"),
            arn.value))

        status(result) shouldBe 303
        Helpers.redirectLocation(result) shouldBe Some(
          routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
      }
    }
  }

  "POST /agents/client-identify-vat" should {
    val request = FakeRequest("POST", "/agents/fast-track/identify-irv-client")
    "redirect to invitation-sent" in new VatHappyScenarioPersonal {
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, vrn, Some("2010-10-10"))
      journeyState.set(
        IdentifyClient(ftr, None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitIdentifyVatClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientIdentifier"       -> "101747696",
            "registrationDate.year"  -> "2010",
            "registrationDate.month" -> "10",
            "registrationDate.day"   -> "10"),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
    }

    "redirect to client-insolvent" in new VatHappyScenarioPersonal {
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, vrn, Some("2010-10-10"))
      givenVatRegisteredClientReturns(vrn, LocalDate.parse("2010-10-10"), 403, true)
      journeyState.set(
        IdentifyClient(ftr, None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitIdentifyVatClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientIdentifier"       -> "101747696",
            "registrationDate.year"  -> "2010",
            "registrationDate.month" -> "10",
            "registrationDate.day"   -> "10"),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showClientInsolvent().url)
    }

    "redirect to cannot-create-request" in new VatHappyScenarioPersonal {
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, vrn, Some("2010-10-10"))
      givenVatRegisteredClientReturns(vrn, LocalDate.parse("2010-10-10"), 423)
      journeyState.set(
        IdentifyClient(ftr, None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitIdentifyVatClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientIdentifier"       -> "101747696",
            "registrationDate.year"  -> "2010",
            "registrationDate.month" -> "10",
            "registrationDate.day"   -> "10"),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCannotCreateFastTrackRequest().url)

    }

    "redirect to not matched" in new VatHappyScenarioPersonal {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.Vat, vrn, Some("2010-10-11"))
      givenVatRegisteredClientReturns(vrn, LocalDate.parse("2010-10-10"), 403)
      journeyState.set(
        IdentifyClient(ftr, None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitIdentifyVatClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientIdentifier"       -> "101747696",
            "registrationDate.year"  -> "2010",
            "registrationDate.month" -> "10",
            "registrationDate.day"   -> "10"),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showNotMatched().url)

    }

    "redirect to invitation-sent for business VAT" in new VatHappyScenarioBusiness {
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Vat, vrn, Some("2010-10-10"))
      journeyState.set(IdentifyClient(ftr, None), List())

      val result = controller.submitIdentifyVatClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientIdentifier"       -> "101747696",
            "registrationDate.year"  -> "2010",
            "registrationDate.month" -> "10",
            "registrationDate.day"   -> "10"),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
    }
  }

  "POST /agents/client-identify-trust" should {
    val request = FakeRequest("POST", "/agents/fast-track/identify-irv-client")
    "redirect to /agents/confirm-trust-client" in new TrustHappyScenario {
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Trust, validUtr, None)
      journeyState.set(
        IdentifyClient(ftr, None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitIdentifyTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("taxId" -> validUtr.value),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(
        routes.AgentInvitationFastTrackJourneyController.showConfirmTrustClient().url)
    }
  }

  "POST /agents/client-details-ppt" should {
    val request = FakeRequest("POST", "/agents/fast-track/identify-ppt-client")
    "redirect to invitation-sent" in {
      givenPptCheckKnownFactReturns(PptClient(pptRef,pptDefaultRegDate.toString), 200)
      givenGetPptCustomerName(pptRef, "a ppt customer")
        val ftr = AgentFastTrackRequest(Some(Personal), Service.Ppt, pptRef, Some(pptDefaultRegDate.toString))
        journeyState.set(
          IdentifyClient(ftr, None),
          List(
            CheckDetailsComplete(ftr, None),
            Prologue(None, None)
          )
        )
        val result = controller.submitIdentifyPptClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "pptRef" -> "XAPPT0000012345",
              "registrationDate.year" -> "2021",
              "registrationDate.month" -> "01",
              "registrationDate.day" -> "01"),
            arn.value))

        status(result) shouldBe 303
        Helpers.redirectLocation(result) shouldBe Some(
          routes.AgentInvitationFastTrackJourneyController.showConfirmClientPpt().url)
    }
  }


  "POST /agents/client-details-cgt" should {
    val request = FakeRequest("POST", "/agents/client-details-cgt")

    "redirect to /agents/client-postcode" in new CgtHappyScenario {
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription("GB")).toString())
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Business), Service.CapitalGains, cgtRef, None)
      journeyState.set(
        IdentifyClient(ftr, None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitIdentifyCgtClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("cgtRef" -> cgtRef.value),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(
        routes.AgentInvitationFastTrackJourneyController.showConfirmCgtPostcode().url)
    }
  }

  "POST /agents/select-client-type-cgt" should {
    val request = FakeRequest("POST", "/agents/client-details-cgt")

    "redirect to /agents/client-postcode" in new CgtHappyScenario {
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription("GB")).toString())
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Business), Service.CapitalGains, cgtRef, None)
      journeyState.set(
        SelectClientType(ftr, None),
        List(
          IdentifyClient(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitClientType(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("clientType" -> "personal"),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(
        routes.AgentInvitationFastTrackJourneyController.showConfirmCgtPostcode().url)
    }

    "redirect to /agents/client-country" in new CgtHappyScenario {
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription("FR")).toString())
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Business), Service.CapitalGains, cgtRef, None)
      journeyState.set(
        SelectClientType(ftr, None),
        List(
          IdentifyClient(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitClientType(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("clientType" -> "personal"),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(
        routes.AgentInvitationFastTrackJourneyController.showConfirmCgtCountryCode().url)
    }
  }

  "GET /agent/confirm-trust-client" should {
    val request = FakeRequest("POST", "/agents/confirm-trust-client")
    "show the confirm client page as expected" in new CgtHappyScenario {
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Trust, validUtr, None)
      journeyState.set(
        ConfirmClientTrust(ftr, None, "trustName"),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showConfirmTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("utr" -> validUtr.value),
          arn.value
        ))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "global.yes", "global.no-start-over")
      checkIncludesText(result,"Is this the client you want authorisation from?", "Is trustName the client you want authorisation from?")
    }
  }

  "POST /agent/confirm-trust-client" should {
    val request = FakeRequest("POST", "/agents/confirm-trust-client")
    "create an invitation as expected if there are no pending invitation exist" in new TrustHappyScenario {
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Business), Service.Trust, validUtr, None)
      journeyState.set(
        ConfirmClientTrust(ftr, None, "trustName"),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitConfirmTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("accepted" -> "true"),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
    }
  }

  "GET /agent/confirm-cgt-client" should {
    val request = FakeRequest("GET", "/agents/confirm-cgt-client")
    "show the confirm client page as expected" in new CgtHappyScenario {
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Business), Service.CapitalGains, cgtRef, None)
      journeyState.set(
        ConfirmClientCgt(ftr, None, "some-cgt-name"),
        List(
          ConfirmCountryCodeCgt(ftr, None, "FR", "some-cgt-name"),
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showConfirmClientCgt(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "global.yes", "global.no-start-over")
      checkIncludesText(result, "Is this the client you want authorisation from?", "Is some-cgt-name the client you want authorisation from?")
    }
  }

  "POST /agent/confirm-cgt-client" should {
    val request = FakeRequest("POST", "/agents/confirm-cgt-client")
    "create an invitation as expected if there are no pending invitation exist" in new CgtHappyScenario {
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Business), Service.CapitalGains, cgtRef, None)
      journeyState.set(
        ConfirmClientCgt(ftr, None, "some-cgt-name"),
        List(
          ConfirmCountryCodeCgt(ftr, None, "FR", "some-cgt-name"),
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitConfirmCgtClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("accepted" -> "true"),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
    }
  }

  "GET /agents/track/client-postcode" should {
    val request = FakeRequest("POST", "/agents/track/client-postcode")

    "redirect user to /agents/track/confirm-cgt-client if postcodes match for a UK client" in new CgtHappyScenario {
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Business), Service.CapitalGains, cgtRef, None)
      journeyState.set(
        ConfirmPostcodeCgt(ftr, None, Some("BN13 1FN"), "firstName lastName"),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showConfirmCgtPostcode(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "confirm-postcode-cgt.header", "confirm-postcode-cgt.p1")
    }

    "redirect user to /agents/no-match if postcodes do not match for a UK client" in new CgtHappyScenario {
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Business), Service.CapitalGains, cgtRef, None)
      journeyState.set(
        ConfirmPostcodeCgt(ftr, None, Some("BN13 1FN"), "firstName lastName"),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitConfirmCgtPostcode(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("postcode" -> "BN13 1XX"),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showNotMatched().url)
    }
  }

  "GET /agents/track/client-country" should {
    val request = FakeRequest("POST", "/agents/track/client-country")

    "redirect user to /agents/track/confirm-cgt-client if country codes match for a non UK client" in new CgtHappyScenario {
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Business), Service.CapitalGains, cgtRef, None)
      journeyState.set(
        ConfirmCountryCodeCgt(ftr, None, "FR", "firstName lastName"),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showConfirmCgtCountryCode(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "confirm-countryCode-cgt.header", "confirm-countryCode-cgt.p1", "confirm-countryCode-cgt.hint")
    }

    "redirect user to /agents/no-match if countryCodes do not match for a non UK client" in new CgtHappyScenario {
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Business), Service.CapitalGains, cgtRef, None)
      journeyState.set(
        ConfirmCountryCodeCgt(ftr, None, "FR", "firstName lastName"),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitConfirmCgtCountryCode(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("countryCode" -> "IN"),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showNotMatched().url)
    }
  }

  "GET /agents/more-details" should {
    val request = FakeRequest("GET", "/agents/fast-track/more-details")
    "show the more-details page for ITSA service" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("BN114AW"))
      journeyState.set(
        NoPostcode(ftr, None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showKnownFact(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "known-fact.HMRC-MTD-IT.heading", "known-fact.HMRC-MTD-IT.helper")
    }

    "show the more-details page for IRV service" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, Nino("AB123456A"), Some(dateOfBirth))
      journeyState.set(NoDob(ftr, None), List())

      val result = controller.showKnownFact(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "known-fact.PERSONAL-INCOME-RECORD.heading",
        "known-fact.PERSONAL-INCOME-RECORD.helper")
    }

    "show the more-details page for VAT service" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.Vat, vrn, Some(validRegistrationDate))
      journeyState.set(NoVatRegDate(ftr, None), List())

      val result = controller.showKnownFact(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "known-fact.HMRC-MTD-VAT.heading", "known-fact.HMRC-MTD-VAT.helper")
    }
  }

  "POST /agents/more-details-itsa" should {
    val request = FakeRequest("POST", "/agents/fast-track/more-details-itsa")
    "redirect to invitation-sent" in new ItsaHappyScenario {
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), None)
      journeyState.set(
        NoPostcode(ftr, None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitKnownFactItsa(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("knownFact" -> "BN32TN"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
    }

    "redirect to not-matched (Postcode)" in new ItsaNotMatchedScenario {
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), None)
      journeyState.set(
        NoPostcode(ftr, None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitKnownFactItsa(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("knownFact" -> "BN32TN"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showNotMatched().url)
    }

    "redirect to client-not-registered when the client is not signed up for the service and there is no SAUTR on CiD record" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, nino.value, Service.MtdIt)
      givenCheckRelationshipItsaWithStatus(arn, nino, 404)
      givenNotEnrolledClientITSA(nino, "BN32TN")
      givenPartialAuthNotExists(arn, nino.value)

      val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), None)
      journeyState.set(NoPostcode(ftr, None), List())

      val result = controller.submitKnownFactItsa(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("knownFact" -> "BN32TN"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(
        routes.AgentInvitationFastTrackJourneyController.showClientNotRegistered().url)

    }
  }

  "POST /agents/more-details-irv" should {
    val request = FakeRequest("POST", "/agents/fast-track/more-details-irv")
    "redirect to invitation-sent" in new IrvHappyScenario {
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, nino, None)
      journeyState.set(
        NoDob(ftr, None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val requestWithForm = request.withFormUrlEncodedBody(
        "knownFact.year"  -> "1990",
        "knownFact.month" -> "10",
        "knownFact.day"   -> "10"
      )

      val result = controller.submitKnownFactIrv(authorisedAsValidAgent(requestWithForm, arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
    }

    "redirect to not-matched (DOB)" in new IrvNotMatchedScenario {
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.PersonalIncomeRecord, nino, None)
      journeyState.set(
        NoDob(ftr, None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val requestWithForm = request.withFormUrlEncodedBody(
        "knownFact.year"  -> "1990",
        "knownFact.month" -> "10",
        "knownFact.day"   -> "10"
      )

      val result = controller.submitKnownFactIrv(authorisedAsValidAgent(requestWithForm, arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showNotMatched().url)
    }
  }

  "POST /agents/more-details-vat" should {
    val request = FakeRequest("POST", "/agents/fast-track/more-details-vat")
    "redirect to invitation-sent" in new VatHappyScenarioPersonal {
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, vrn, Some("2010-10-10"))
      journeyState.set(
        NoVatRegDate(ftr, None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val requestWithForm = request.withFormUrlEncodedBody(
        "knownFact.year"  -> "2010",
        "knownFact.month" -> "10",
        "knownFact.day"   -> "10"
      )

      val result = controller.submitKnownFactVat(authorisedAsValidAgent(requestWithForm, arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
    }

    "redirect to not-matched (Vat Reg. Date)" in new VatNotMatchedScenario {
      val originalFtr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, vrn, Some("1990-10-10"))
      val ftr: AgentFastTrackRequest = AgentFastTrackRequest(Some(Personal), Service.Vat, vrn, None)
      journeyState.set(
        NoVatRegDate(ftr, None),
        List(CheckDetailsComplete(originalFtr, None), Prologue(None, None))
      )

      val requestWithForm = request.withFormUrlEncodedBody(
        "knownFact.year"  -> "2010",
        "knownFact.month" -> "10",
        "knownFact.day"   -> "10"
      )

      val result = controller.submitKnownFactVat(authorisedAsValidAgent(requestWithForm, arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showNotMatched().url)
    }
  }

  "GET /agents/type-of-client" should {
    val request = FakeRequest("GET", "/agents/fast-track/client-type")
    "show the client-type page" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.Vat, vrn, Some(validRegistrationDate))
      journeyState.set(
        SelectClientType(ftr, None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showClientType(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result.futureValue, htmlEscapedMessage("client-type.header"))
    }
  }

  "GET /agents/sent-invitation" should {
    val request = FakeRequest("GET", "/agents/fast-track/sent-invitation")
    "show the invitation sent page for a personal service without the first step - check client is signed up for the service- when the service is MTDIT" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("BN114AW"))
      journeyState.set(
        InvitationSent(ClientType.Personal, "invitation/sent/url", None, "abc@xyz.com", Service.MtdIt, isAltItsa = Some(false)),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showInvitationSent(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "invitation-sent.header",
        "invitation-sent.link-text",
      "invitation-sent.select-link",
      "invitation-sent.client-warning")

      checkHtmlResultWithNotBodyText(result.futureValue, "Check with your client that they have a Government Gateway user ID for their personal tax affairs and that they have signed up to Making Tax Digital for VAT.",
      "Check with your client that they have a Government Gateway user ID for their personal tax affairs.",
      "Check with your client that they have signed up to Making Tax Digital for VAT.")
    }

    "show the invitation sent page for a personal service with the first step - check client is signed up for the service- when the service is VAT" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.Vat, Vrn("580659315"), Some("2019-10-10"))
      journeyState.set(
        InvitationSent(ClientType.Personal, "invitation/sent/url", None, "abc@xyz.com", Service.Vat, isAltItsa = Some(false)),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showInvitationSent(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "invitation-sent.header",
        "invitation-sent.step1.personal.vat",
        "invitation-sent.link-text",
        "invitation-sent.select-link",
        "invitation-sent.client-warning")

      checkHtmlResultWithNotBodyText(result.futureValue, "Check with your client that they have a Government Gateway user ID for their personal tax affairs and that they have signed up to MTD VAT.",
        "Check with your client that they have a Government Gateway user ID for their personal tax affairs.")
    }

    "show the invitation sent page for a personal service with alt Itsa" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, Some("2019-10-10"))
      journeyState.set(
        InvitationSent(ClientType.Personal, "invitation/sent/url", None, "abc@xyz.com", Service.MtdIt, isAltItsa = Some(true)),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showInvitationSent(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result,"Sign up your client for Making Tax Digital for Income Tax")
    }

    "show the invitation sent page for a business service" in {
      journeyState.set(InvitationSent(ClientType.Business, "invitation/sent/url", None, "abc@xyz.com", Service.Vat), List())

      val result = controller.showInvitationSent(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "invitation-sent.header",
        "invitation-sent.step1.business.vat",
        "invitation-sent.link-text",
        "invitation-sent.select-link",
        "invitation-sent.client-warning")

      checkHtmlResultWithNotBodyText(result.futureValue, "Check with your client that they have a Government Gateway user ID for their personal tax affairs and that they have signed up to Making Tax Digital for VAT.",
        "Check with your client that they have a Government Gateway user ID for their personal tax affairs.")
    }
  }

  "GET /agents/not-signed-up-single" should {
    val request = FakeRequest("GET", "/agents/fast-track/not-signed-up")
    "show the client-type page" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("BN114AW"))
      journeyState.set(
        ClientNotSignedUp(ftr, None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showClientNotSignedUp(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result.futureValue, htmlEscapedMessage("not-enrolled.title", "signed up to Making Tax Digital for Income Tax"))
      checkHtmlResultWithBodyText(result.futureValue, htmlEscapedMessage("not-enrolled.p", "signed up."))
      checkHtmlResultWithBodyText(result.futureValue, htmlEscapedMessage("not-enrolled.existing.header", "Self Assessment"))
      checkResultContainsLink(result,"http://localhost:9438/agent-mapping/start","copy across an existing authorisation")
      checkResultContainsLink(result,"http://localhost:9438/agent-mapping/start","copy across an existing authorisation")

    }
  }

  "GET /agents/no-match" should {
    val request = FakeRequest("GET", "/agents/fast-track/no-match")

    "show the client-type page" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("BN114AW"))
      journeyState.set(
        ClientNotFound(ftr, None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showNotMatched(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "not-matched.header")
    }
  }

  "GET /agents/authorisation-already-pending" should {
    val request = FakeRequest("GET", "/agents/fast-track/already-authorisation-pending")
    "show the already-authorisation-pending page" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, nino, Some("BN114AW"))
      journeyState.set(
        PendingInvitationExists(ftr, "/invitation-link/ABC123", "Charmarti Ltd.", None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showPendingAuthorisationExists(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      val html = Jsoup.parse(Helpers.contentAsString(result))
      html.title shouldBe  "You already created an authorisation request for this tax service - Ask a client to authorise you - GOV.UK"
      html.select("main h1").text() shouldBe "You already created an authorisation request for this tax service"
      html.select("main p").get(0).text() shouldBe "You cannot continue until Charmarti Ltd. has accepted the authorisation request link."
      html.select("main p").get(1).text() shouldBe "Resend the authorisation request link that was created when you originally asked Charmarti Ltd. to authorise you:"
      html.select("main p").get(2).text() should include ("/invitation-link/ABC123")
      html.select("main p").get(2).classNames() contains "govuk-!-font-weight-bold"
      html.select("main p").get(2).classNames() contains "govuk-body"
      html.select("main .govuk-button").text() shouldBe "Manage your authorisation requests"
      html.select("main .govuk-button").attr("href") should startWith("/invitations/")
    }
  }

  "GET /agents/authorisation-already-present" should {
    val request = FakeRequest("GET", "/agents/fast-track/already-authorisation-present")
    "show the already-authorisation-pending page" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("BN114AW"))
      journeyState.set(
        ActiveAuthorisationExists(ftr, None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showActiveAuthorisationExists(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "active-authorisation-exists.header")
    }

    "show the already-authorisation-active page when there is a PartialAuth invitation" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("BN114AW"))
      journeyState.set(
        PartialAuthorisationExists(ftr, None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showActiveAuthorisationExists(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "partial-authorisation-exists.header")
    }

    "show the already copied across warning page when there is a legacy mapping" in {
      val request = FakeRequest("GET", "/agents/track/already-copied-across-itsa")
      //ignore unused warning
      val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("BN114AW"))
      journeyState.set(
        AlreadyCopiedAcrossItsa,
        Nil
      )

      val result = controller.showAlreadyCopiedAcrossItsa(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "already-copied.header")
    }

    "show the client-not-registered page when client does not have an SAUTR or MTDITID" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("BN114AW"))
      journeyState.set(
        ClientNotRegistered(ftr, None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showClientNotRegistered(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "client-not-registered.header")
    }
  }

  "GET /agents/track/authorisation-detected" should {
    val request = FakeRequest("GET", "/agents/track/authorisation-detected")
    "display the content" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("BN114AW"))
      journeyState.set(
        LegacyAuthorisationDetected(ftr, arn, Invitation(Some(Personal), Service.MtdIt, nino), None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showLegacyAuthorisationDetected(authorisedAsValidAgent(request, arn.value)).futureValue

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "legacy-auth-detected.title", "legacy-auth-detected.yes", "legacy-auth-detected.no")
    }
  }

  "POST /agents/track/authorisation-detected" should {
    val request = FakeRequest("POST", "/agents/track/authorisation-detected")
    "redirect to mapping when agent selects 'yes' (agent has a legacy SA relationship with the client)" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("BN114AW"))
      journeyState.set(
        LegacyAuthorisationDetected(ftr, arn, Invitation(Some(Personal), Service.MtdIt, nino), None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitLegacyAuthorisationDetected(authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result.futureValue) shouldBe 303
      redirectLocation(result) shouldBe Some("http://localhost:9438/agent-mapping/start")
    }

    "redirect to /invitation-sent when agent selects 'no' (agent does not have a legacy SA relationship with the client)" in {
      val ftr = AgentFastTrackRequest(Some(Personal), Service.MtdIt, Nino("AB123456A"), Some("BN114AW"))
      journeyState.set(
        LegacyAuthorisationDetected(ftr, arn, Invitation(Some(Personal), Service.MtdIt, nino), None),
        List(
          CheckDetailsComplete(ftr, None),
          Prologue(None, None)
        )
      )

      givenGetAllPendingInvitationsReturnsEmpty(arn, nino.value, Service.MtdIt)
      givenInvitationCreationSucceeds(arn, Some(Personal), nino.value, invitationIdITSA, nino.value, "ni", Service.MtdIt, "NI")
      givenAgentReference(arn, "uid", Personal)
      givenGetAgencyEmailAgentStub

      val result = controller.submitLegacyAuthorisationDetected(authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result.futureValue) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/sent-invitation")
    }
  }

  class ItsaHappyScenario {
    givenGetAllPendingInvitationsReturnsEmpty(arn, nino.value, Service.MtdIt)
    givenCheckRelationshipItsaWithStatus(arn, nino, 404)
    givenMatchingClientIdAndPostcode(nino, "BN32TN")
    givenTradingName(nino, "Sylvia Plath")
    givenInvitationCreationSucceeds(arn, Some(Personal), nino.value, invitationIdITSA, nino.value, "ni", Service.MtdIt, "NI")
    givenAgentReferenceRecordExistsForArn(arn, "FOO")
    givenAgentReference(arn, "uid", Personal)
    givenGetAgencyEmailAgentStub
    givenLegacySaRelationshipReturnsStatus(arn, nino, 404)
    givenPartialAuthNotExists(arn, nino.value)
    givenGetAllPendingInvitationsReturnsEmpty(arn, nino.value, Service.MtdIt)
  }

  class ItsaNotMatchedScenario {
    givenGetAllPendingInvitationsReturnsEmpty(arn, nino.value, Service.MtdIt)
    givenCheckRelationshipItsaWithStatus(arn, nino, 404)
    givenNonMatchingClientIdAndPostcode(nino, "BN32TN")
  }

  class IrvHappyScenario {
    givenGetAllPendingInvitationsReturnsEmpty(arn, nino.value, Service.PersonalIncomeRecord)
    givenAfiRelationshipNotFoundForAgent(arn, nino)
    givenMatchingCitizenRecord(nino, LocalDate.parse("1990-10-10"))
    givenCitizenDetailsAreKnownFor(nino, "Virginia", "Woolf")
    givenInvitationCreationSucceeds(arn, Some(Personal), nino.value, invitationIdPIR, nino.value, "ni", Service.PersonalIncomeRecord, "NI")
    givenAgentReferenceRecordExistsForArn(arn, "FOO")
    givenAgentReference(arn, "uid", Personal)
    givenGetAgencyEmailAgentStub
  }

  class IrvNotMatchedScenario {
    givenGetAllPendingInvitationsReturnsEmpty(arn, nino.value, Service.PersonalIncomeRecord)
    givenAfiRelationshipNotFoundForAgent(arn, nino)
    givenNonMatchingCitizenRecord(nino, LocalDate.parse("1990-10-10"))
  }

  class VatHappyScenarioPersonal {
    givenGetAllPendingInvitationsReturnsEmpty(arn, vrn.value, Service.Vat)
    givenVatRegisteredClientReturns(vrn, LocalDate.parse("2010-10-10"), 204)
    givenClientDetails(vrn)
    givenCheckRelationshipVatWithStatus(arn, vrn, 404)
    givenInvitationCreationSucceeds(arn, Some(Personal), nino.value, invitationIdVAT, vrn.value, "vrn", Service.Vat, "VRN")
    givenAgentReferenceRecordExistsForArn(arn, "FOO")
    givenAgentReference(arn, "uid", Personal)
    givenGetAgencyEmailAgentStub
  }

  class VatHappyScenarioBusiness {
    givenGetAllPendingInvitationsReturnsEmpty(arn, vrn.value, Service.Vat)
    givenVatRegisteredClientReturns(vrn, LocalDate.parse("2010-10-10"), 204)
    givenClientDetails(vrn)
    givenCheckRelationshipVatWithStatus(arn, vrn, 404)
    givenInvitationCreationSucceeds(arn, Some(Business), nino.value, invitationIdVAT, vrn.value, "vrn", Service.Vat, "VRN")
    givenAgentReferenceRecordExistsForArn(arn, "FOO")
    givenAgentReference(arn, "uid", Business)
    givenGetAgencyEmailAgentStub
  }

  class TrustHappyScenario {
    givenGetAllPendingInvitationsReturnsEmpty(arn, validUtr.value, Service.Trust)

    givenTrustClientReturns(validUtr, 200, Json.toJson(trustResponse).toString())
    givenCheckRelationshipVatWithStatus(arn, vrn, 404)
    givenInvitationCreationSucceeds(
      arn,
      Some(Trust),
      validUtr.value,
      invitationIdTrust,
      validUtr.value,
      "utr",
      Service.Trust,
      "UTR")
    givenAgentReferenceRecordExistsForArn(arn, "FOO")
    givenAgentReference(arn, "uid", Business)
    givenGetAgencyEmailAgentStub
  }


  class CgtHappyScenario {
    givenGetAllPendingInvitationsReturnsEmpty(arn, cgtRef.value, Service.CapitalGains)
    givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription()).toString())
    givenCheckRelationshipVatWithStatus(arn, vrn, 404)
    givenInvitationCreationSucceeds(
      arn,
      Some(Business),
      cgtRef.value,
      invitationIdCgt,
      cgtRef.value,
      "CGTPDRef",
      Service.CapitalGains,
      "CGTPDRef")
    givenAgentReferenceRecordExistsForArn(arn, "FOO")
    givenAgentReference(arn, "uid", Business)
    givenGetAgencyEmailAgentStub
  }

  class PPTHappyScenario {
    givenGetAllPendingInvitationsReturnsEmpty(arn, pptRef.value, Service.Ppt)
    givenGetPptSubscriptionReturns(pptRef, 200, Json.toJson(pptSubscriptionSuccessBodyJson(pptRef,pptDefaultRegDate)).toString())
    givenCheckRelationshipPptWithStatus(arn, pptRef.value, 404)
    givenInvitationCreationSucceeds(
      arn,
      Some(Business),
      pptRef.value,
      invitationIdPpt,
      pptRef.value,
      "EtmpRegistrationNumber",
      Service.Ppt,
      "EtmpRegistrationNumber")
    givenAgentReferenceRecordExistsForArn(arn, "FOO")
    givenAgentReference(arn, "uid", Business)
    givenGetAgencyEmailAgentStub
  }

  class VatNotMatchedScenario {
    givenGetAllPendingInvitationsReturnsEmpty(arn, vrn.value, Service.Vat)
    givenVatRegisteredClientReturns(vrn, LocalDate.parse("2010-10-10"), 403)
    givenCheckRelationshipVatWithStatus(arn, vrn, 404)
  }
}
