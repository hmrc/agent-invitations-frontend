package uk.gov.hmrc.agentinvitationsfrontend.controllers

import org.joda.time.LocalDate
import org.scalatest.BeforeAndAfter
import play.api.Application
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCCGTPD, HMRCMTDIT, HMRCMTDVAT, HMRCPIR, TRUST}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.Vrn
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier}

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationFastTrackJourneyControllerISpec
    extends BaseISpec with StateAndBreadcrumbsMatchers with BeforeAndAfter {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  override implicit lazy val app: Application = appBuilder(featureFlags)
    .overrides(new TestAgentInvitationFastTrackJourneyModule)
    .build()

  implicit lazy val appOpposite: Application = appBuilder(oppositeFeatureFlags)
    .overrides(new TestAgentInvitationFastTrackJourneyModule)
    .build()

  lazy val journeyState = app.injector.instanceOf[TestAgentInvitationFastTrackJourneyService]
  lazy val appOppositeJourneyState = appOpposite.injector.instanceOf[TestAgentInvitationFastTrackJourneyService]

  lazy val controller: AgentInvitationFastTrackJourneyController =
    app.injector.instanceOf[AgentInvitationFastTrackJourneyController]

  lazy val oppositeController = appOpposite.injector.instanceOf[AgentInvitationFastTrackJourneyController]

  import journeyState.model.State._

  val availableServices = Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)
  val emptyBasket = Set.empty[AuthorisationRequest]

  before {
    journeyState.clear
  }

  "POST /agents/fast-track" should {
    val request = FakeRequest("POST", "/agents/fast-track")

    "redirect to check-details if all values in request are valid with no continue url" when {

      "submitted NINO is uppercase" in {
        checkAgentFastTract(submittedNinoStr = "AB123456A")
      }

      "submitted NINO is lowercase (APB-3634)" in {
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
        redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)

        val expectedFtr =
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", submittedNinoStr.toUpperCase, Some("BN32TN"))
        journeyState.get shouldBe Some(
          (
            CheckDetailsCompleteItsa(originalFastTrackRequest = expectedFtr, fastTrackRequest = expectedFtr, None),
            List(Prologue(None, None))))
      }
    }

    "redirect to check-details if all values in request are valid with a continue and error url query parameters" in {
      givenWhitelistedDomains
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
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to check-details when there is a referer in the header" in {
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
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
      val expectedFtr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN32TN"))
      journeyState.get shouldBe Some(
        (
          CheckDetailsCompleteItsa(originalFastTrackRequest = expectedFtr, fastTrackRequest = expectedFtr, None),
          List(Prologue(None, Some("/some/referer/url")), Prologue(None, None))))
    }

    "redirect to check details when service is IRV" in {
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "personal",
            "service"              -> "PERSONAL-INCOME-RECORD",
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> nino,
            "knownFact"            -> dateOfBirth),
          arn.value
        ))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to check details when service is personal VAT" in {
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "personal",
            "service"              -> "HMRC-MTD-VAT",
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> vrn,
            "knownFact"            -> validRegistrationDate),
          arn.value
        ))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to check details when service is business VAT" in {
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "business",
            "service"              -> "HMRC-MTD-VAT",
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> vrn,
            "knownFact"            -> validRegistrationDate),
          arn.value
        ))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to check details when service is Trust" in {
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
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to check details when service is CGT" in {
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
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to check details when service is ITSA with no postcode" in {
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "personal",
            "service"              -> "HMRC-MTD-IT",
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> nino),
          arn.value
        ))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to check details when service is IRV with no date of birth" in {
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "personal",
            "service"              -> "PERSONAL-INCOME-RECORD",
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> nino),
          arn.value
        ))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to check details when service is VAT with no vat reg date" in {
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "personal",
            "service"              -> "HMRC-MTD-VAT",
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> vrn),
          arn.value
        ))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to check details when service is VAT with no vat reg date or client type" in {
      journeyState.clear
      val request = FakeRequest("POST", "/agents/fast-track")
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "service"              -> "HMRC-MTD-VAT",
            "clientIdentifierType" -> "vrn",
            "clientIdentifier"     -> vrn),
          arn.value
        ))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
    }

    "redirect to the error url with appended error reason if all values in request are valid with a continue and error url query parameters" in {
      givenWhitelistedDomains
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
      redirectLocation(result) shouldBe Some(
        "http://localhost:9996/tax-history/not-authorised?issue=UNSUPPORTED_SERVICE")
    }

    "throw a Bad Request exception if the continue url is not whitelisted" in {
      givenWhitelistedDomains
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
    "throw a Bad Request exception if the error url is not whitelisted" in {
      givenWhitelistedDomains
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

  "GET /agents/check-details" should {
    val request = FakeRequest("GET", "/agents/fast-track/check-details")
    "show the check-details page for ITSA service" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN32TN"))
      journeyState.set(
        CheckDetailsCompleteItsa(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None),
        List(Prologue(None, None)))

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "check-details.heading",
        "check-details.p.HMRC-MTD-IT",
        "check-details.client-type.personal")
    }
    "show the check-details page for IRV service" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", "AB123456A", Some(dateOfBirth))
      journeyState.set(CheckDetailsCompleteIrv(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "check-details.heading",
        "check-details.p.PERSONAL-INCOME-RECORD",
        "check-details.client-type.personal")
    }
    "show the check-details page for personal VAT service" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, Some(validRegistrationDate))
      journeyState
        .set(CheckDetailsCompletePersonalVat(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "check-details.heading",
        "check-details.p.HMRC-MTD-VAT",
        "check-details.client-type.personal")
    }
    "show the check-details page for business VAT service" in {
      val ftr = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "vrn", vrn, Some(validRegistrationDate))
      journeyState
        .set(CheckDetailsCompleteBusinessVat(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "check-details.heading",
        "check-details.p.HMRC-MTD-VAT",
        "check-details.client-type.business.HMRC-MTD-VAT")
    }

    "show the check-details page for Trust service" in {
      val ftr = AgentFastTrackRequest(Some(business), TRUST, "utr", validUtr.value, None)
      journeyState
        .set(CheckDetailsCompleteTrust(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "check-details.heading",
        "check-details.p.HMRC-TERS-ORG",
        "check-details.client-type.business.HMRC-TERS-ORG")
    }

    "show the check-details page for business CGT service" in {
      val ftr = AgentFastTrackRequest(Some(business), HMRCCGTPD, "CGTPDRef", cgtRef.value, None)
      journeyState
        .set(CheckDetailsCompleteCgt(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "check-details.heading",
        "check-details.p.HMRC-CGT-PD.business",
        "check-details.client-type.business.HMRC-CGT-PD")
    }

    "show the check-details page for personal CGT service" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCCGTPD, "CGTPDRef", cgtRef.value, None)
      journeyState
        .set(CheckDetailsCompleteCgt(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "check-details.heading",
        "check-details.p.HMRC-CGT-PD.personal",
        "check-details.client-type.personal")
    }

    "show the check-details page for ITSA client with no postcode" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", None)
      journeyState.set(CheckDetailsNoPostcode(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "check-details.heading",
        "check-details.p.HMRC-MTD-IT",
        "check-details.client-type.personal",
        "check-details.change.p1")
    }
    "show the check-details page for IRV client with no date of birth" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", "AB123456A", None)
      journeyState.set(CheckDetailsNoDob(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "check-details.heading",
        "check-details.p.PERSONAL-INCOME-RECORD",
        "check-details.client-type.personal",
        "check-details.change.p1")
    }
    "show the check-details page for VAT client with no vat registration date" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, None)
      journeyState.set(CheckDetailsNoVatRegDate(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "check-details.heading",
        "check-details.p.HMRC-MTD-VAT",
        "check-details.client-type.personal",
        "check-details.change.p1")
    }
    "show the check-details page for VAT client with no client type" in {
      val ftr = AgentFastTrackRequest(None, HMRCMTDVAT, "vrn", vrn, None)
      journeyState
        .set(CheckDetailsNoClientTypeVat(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None), List())

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "check-details.heading",
        "check-details.p.HMRC-MTD-VAT",
        "check-details.change.p1")
    }
  }

  "POST /agents/check-details" should {
    val request = FakeRequest("POST", "/agents/check-details")
    "redirect to invitation-sent" in new ItsaHappyScenario {
      givenGetAgencyEmailAgentStub
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, Some("BN32TN"))
      journeyState.set(
        CheckDetailsCompleteItsa(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None),
        List(Prologue(None, None)))

      val result = controller.submitCheckDetails(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
    }

    "redirect to client-identify for a personal service" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, Some("BN114AW"))
      journeyState.set(
        CheckDetailsCompleteItsa(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None),
        List(Prologue(None, None)))

      val result = controller.submitCheckDetails(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showIdentifyClient().url)
    }

    "redirect to client-identify for a business service" in {
      val ftr = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "vrn", vrn, Some(validRegistrationDate))
      journeyState.set(
        CheckDetailsCompleteBusinessVat(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None),
        List(Prologue(None, None)))

      val result = controller.submitCheckDetails(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showIdentifyClient().url)
    }

    "redirect to /identify-client for a trust service" in {
      val ftr = AgentFastTrackRequest(Some(business), TRUST, "utr", validUtr.value, None)
      journeyState.set(
        CheckDetailsCompleteTrust(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None),
        List(Prologue(None, None)))

      val result = controller.submitCheckDetails(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showIdentifyClient().url)
    }

    "redirect to /identify-client for a CGT service" in {
      val ftr = AgentFastTrackRequest(Some(business), HMRCCGTPD, "CGTPDRef", cgtRef.value, None)
      journeyState.set(
        CheckDetailsCompleteCgt(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None),
        List(Prologue(None, None)))

      val result = controller.submitCheckDetails(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showIdentifyClient().url)
    }
  }

  "POST /agents/to-known-fact" should {
    val request = FakeRequest("POST", "/agents/to-known-fact")

    "redirect to known fact when the postcode is missing for ITSA service" in {
      givenGetAgencyEmailAgentStub
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, None)
      journeyState.set(CheckDetailsNoPostcode(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None), List())

      val result = controller.progressToKnownFact(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showKnownFact().url)
    }

    "redirect to known fact when the dob is missing for PIR service" in {
      givenGetAgencyEmailAgentStub
      val ftr = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, None)
      journeyState.set(CheckDetailsNoDob(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None), List())

      val result = controller.progressToKnownFact(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showKnownFact().url)
    }

    "redirect to known fact when the vat reg date is missing for VAT service" in {
      givenGetAgencyEmailAgentStub
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, None)
      journeyState.set(CheckDetailsNoVatRegDate(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None), List())

      val result = controller.progressToKnownFact(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showKnownFact().url)
    }

    "redirect to /client-postcode for a UK based CGT client" in {
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription("GB")).toString())
      val ftr = AgentFastTrackRequest(Some(personal), HMRCCGTPD, "CGTPDRef", cgtRef.value, None)
      journeyState.set(CheckDetailsCompleteCgt(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None), List())

      val result = controller.progressToKnownFact(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showConfirmCgtPostcode().url)
    }

    "redirect to /client-country for a non UK based CGT client" in {
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription()).toString())
      val ftr = AgentFastTrackRequest(Some(personal), HMRCCGTPD, "CGTPDRef", cgtRef.value, None)
      journeyState.set(CheckDetailsCompleteCgt(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None), List())

      val result = controller.progressToKnownFact(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showConfirmCgtCountryCode().url)
    }
  }

  "POST /agents/to-identify-client" should {
    val request = FakeRequest("POST", "/agents/to-identify-client")

    "redirect to identify client" in {
      val ftr = AgentFastTrackRequest(None, HMRCMTDVAT, "vrn", vrn, None)
      journeyState
        .set(CheckDetailsNoClientTypeVat(originalFastTrackRequest = ftr, fastTrackRequest = ftr, None), List())

      val result = controller.progressToIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showIdentifyClient().url)
    }
  }

  "GET agents/client-details" should {
    val request = FakeRequest("GET", "/agents/fast-track/identify-client")
    "show the client-details page for ITSA" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW"))
      journeyState.set(
        IdentifyPersonalClient(ftr, ftr, None),
        List(
          CheckDetailsCompleteItsa(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "identify-client.header", "identify-client.postcode.label")
    }

    "show the client-details page for IRV" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", "AB123456A", Some("1990-09-09"))
      journeyState.set(
        IdentifyPersonalClient(ftr, ftr, None),
        List()
      )

      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "identify-client.header", "identify-client.irv-date-of-birth.label")
    }

    "show the client-details page for personal VAT" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, Some("2009-09-09"))
      journeyState.set(
        IdentifyPersonalClient(ftr, ftr, None),
        List()
      )

      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "identify-client.header", "identify-client.vat-registration-date.label")
    }

    "show the client-details page for business VAT" in {
      val ftr = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "vrn", vrn, Some("2009-09-09"))
      journeyState.set(
        IdentifyBusinessClient(ftr, ftr, None),
        List()
      )

      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "identify-client.header", "identify-client.vat-registration-date.label")
    }

    "show the client-details page for trust" in {
      val ftr = AgentFastTrackRequest(Some(business), TRUST, "utr", validUtr.value, None)
      journeyState.set(
        IdentifyTrustClient(ftr, ftr, None),
        List()
      )

      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "identify-trust-client.header", "identify-trust-client.p1")
      checkHtmlResultWithBodyText(result, "A Unique Taxpayer Reference is 10 numbers, for example 1234567890")
    }

    "show the client-details page for CGT" in {
      val ftr = AgentFastTrackRequest(Some(business), HMRCCGTPD, "CGTPDRef", cgtRef.value, None)
      journeyState.set(
        IdentifyCgtClient(ftr, ftr, None),
        List()
      )

      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "identify-cgt-client.header.business", "identify-cgt-client.p1", "identify-cgt-client.p2", "identify-cgt-client.hint")
    }

    "show the client-details page when there is no client type for VAT" in {
      val ftr = AgentFastTrackRequest(None, HMRCMTDVAT, "vrn", vrn, Some("2009-09-09"))
      journeyState.set(
        IdentifyNoClientTypeClient(ftr, ftr, None),
        List()
      )

      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "identify-client.header", "identify-client.vat-registration-date.label")
    }
  }

  "GET /agents/client-identify-itsa" should {
    val request = FakeRequest("GET", "/agents/client-identify-itsa")
    "redirect to the identify client page" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW"))
      journeyState.set(
        IdentifyPersonalClient(ftr, ftr, None),
        Nil
      )

      val result = controller.identifyClientRedirect()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showIdentifyClient().url)
    }
  }

  "POST /agents/client-identify-itsa" should {
    val request = FakeRequest("POST", "/agents/fast-track/identify-itsa-client")
    "redirect to invitation-sent" when {
      "submitted NINO is uppercase" in new ItsaHappyScenario {
        checkSubmitIdentifyItsaClient(submittedNinoStr = nino.toUpperCase)
      }

      "submitted NINO is lowercase (APB-3634)" in new ItsaHappyScenario {
        checkSubmitIdentifyItsaClient(submittedNinoStr = nino.toLowerCase)
      }

      def checkSubmitIdentifyItsaClient(submittedNinoStr: String) = {
        val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, Some("BN114AW"))
        journeyState.set(
          IdentifyPersonalClient(ftr, ftr, None),
          List(
            CheckDetailsCompleteItsa(ftr, ftr, None),
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
        redirectLocation(result) shouldBe Some(
          routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
      }
    }
  }

  "POST /agents/client-identify-irv" should {
    val request = FakeRequest("POST", "/agents/fast-track/identify-irv-client")
    "redirect to invitation-sent" when {
      "submitted NINO is uppercase" in new IrvHappyScenario {
        checkSubmitIdentifyIrvClient(submittedNinoStr = nino.toUpperCase)
      }

      "submitted NINO is lowercase (APB-3634)" in new IrvHappyScenario {
        checkSubmitIdentifyIrvClient(submittedNinoStr = nino.toLowerCase)
      }

      def checkSubmitIdentifyIrvClient(submittedNinoStr: String) = {
        val ftr = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, Some("1990-10-10"))
        journeyState.set(
          IdentifyPersonalClient(ftr, ftr, None),
          List(
            CheckDetailsCompleteIrv(ftr, ftr, None),
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
        redirectLocation(result) shouldBe Some(
          routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
      }
    }
  }

  "POST /agents/client-identify-vat" should {
    val request = FakeRequest("POST", "/agents/fast-track/identify-irv-client")
    "redirect to invitation-sent" in new VatHappyScenarioPersonal {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, Some("2010-10-10"))
      journeyState.set(
        IdentifyPersonalClient(ftr, ftr, None),
        List(
          CheckDetailsCompletePersonalVat(ftr, ftr, None),
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
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
    }

    "redirect to invitation-sent for business VAT" in new VatHappyScenarioBusiness {
      val ftr = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "vrn", vrn, Some("2010-10-10"))
      journeyState.set(IdentifyBusinessClient(ftr, ftr, None), List())

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
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
    }
  }

  "POST /agents/client-identify-trust" should {
    val request = FakeRequest("POST", "/agents/fast-track/identify-irv-client")
    "redirect to /agents/confirm-trust-client" in new TrustHappyScenario {
      val ftr = AgentFastTrackRequest(Some(business), TRUST, "utr", validUtr.value, None)
      journeyState.set(
        IdentifyTrustClient(ftr, ftr, None),
        List(
          CheckDetailsCompleteTrust(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitIdentifyTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("utr" -> validUtr.value),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(
        routes.AgentInvitationFastTrackJourneyController.showConfirmTrustClient().url)
    }
  }


  "POST /agents/client-details-cgt" should {
    val request = FakeRequest("POST", "/agents/client-details-cgt")
    "redirect to /agents/track/client-postcode" in new CgtHappyScenario {
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription("GB")).toString())
      val ftr = AgentFastTrackRequest(Some(business), HMRCCGTPD, "CGTPDRef", cgtRef.value, None)
      journeyState.set(
        IdentifyCgtClient(ftr, ftr, None),
        List(
          CheckDetailsCompleteCgt(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitIdentifyCgtClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("cgtRef" -> cgtRef.value),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(
        routes.AgentInvitationFastTrackJourneyController.showConfirmCgtPostcode().url)
    }

    "redirect to /agents/track/client-country" in new CgtHappyScenario {
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription()).toString())
      val ftr = AgentFastTrackRequest(Some(business), HMRCCGTPD, "CGTPDRef", cgtRef.value, None)
      journeyState.set(
        IdentifyCgtClient(ftr, ftr, None),
        List(
          CheckDetailsCompleteCgt(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitIdentifyCgtClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("cgtRef" -> cgtRef.value),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(
        routes.AgentInvitationFastTrackJourneyController.showConfirmCgtCountryCode().url)
    }
  }

  "GET /agent/confirm-trust-client" should {
    val request = FakeRequest("POST", "/agents/confirm-trust-client")
    "show the confirm client page as expected" in new CgtHappyScenario {
      val ftr = AgentFastTrackRequest(Some(business), TRUST, "utr", validUtr.value, None)
      journeyState.set(
        ConfirmClientTrust(ftr, ftr, None, "trustName"),
        List(
          CheckDetailsCompleteTrust(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showConfirmTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("utr" -> validUtr.value),
          arn.value
        ))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "confirm-client.header", "confirm-client.yes", "confirm-client.no")
      checkIncludesText(result, validUtr.value)
    }
  }

  "POST /agent/confirm-trust-client" should {
    val request = FakeRequest("POST", "/agents/confirm-trust-client")
    "create an invitation as expected if there are no pending invitation exist" in new TrustHappyScenario {
      val ftr = AgentFastTrackRequest(Some(business), TRUST, "utr", validUtr.value, None)
      journeyState.set(
        ConfirmClientTrust(ftr, ftr, None, "trustName"),
        List(
          CheckDetailsCompleteTrust(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitConfirmTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("accepted" -> "true"),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
    }
  }

  "GET /agent/confirm-cgt-client" should {
    val request = FakeRequest("GET", "/agents/confirm-cgt-client")
    "show the confirm client page as expected" in new CgtHappyScenario {
      val ftr = AgentFastTrackRequest(Some(business), HMRCCGTPD, "CGTPDRef", cgtRef.value, None)
      journeyState.set(
        ConfirmClientCgt(ftr, ftr, None, "some-cgt-name"),
        List(
          ConfirmCountryCodeCgt(ftr, ftr, None, "FR", "some-cgt-name"),
          CheckDetailsCompleteTrust(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showConfirmClientCgt(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "confirm-client.header", "confirm-client.yes", "confirm-client.no")
      checkIncludesText(result, cgtRef.value)
    }
  }

  "POST /agent/confirm-cgt-client" should {
    val request = FakeRequest("POST", "/agents/confirm-cgt-client")
    "create an invitation as expected if there are no pending invitation exist" in new CgtHappyScenario {
      val ftr = AgentFastTrackRequest(Some(business), HMRCCGTPD, "CGTPDRef", cgtRef.value, None)
      journeyState.set(
        ConfirmClientCgt(ftr, ftr, None, "some-cgt-name"),
        List(
          ConfirmCountryCodeCgt(ftr, ftr, None, "FR", "some-cgt-name"),
          CheckDetailsCompleteCgt(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitConfirmCgtClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("accepted" -> "true"),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
    }
  }

  "GET /agents/track/client-postcode" should {
    val request = FakeRequest("POST", "/agents/track/client-postcode")

    "redirect user to /agents/track/confirm-cgt-client if postcodes match for a UK client" in new CgtHappyScenario {
      val ftr = AgentFastTrackRequest(Some(business), HMRCCGTPD, "CGTPDRef", cgtRef.value, None)
      journeyState.set(
        ConfirmPostcodeCgt(ftr, ftr, None, Some("BN13 1FN"), "firstName lastName"),
        List(
          CheckDetailsCompleteCgt(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showConfirmCgtPostcode(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "confirm-postcode-cgt.header", "confirm-postcode-cgt.p1")
    }

    "redirect user to /agents/no-match if postcodes do not match for a UK client" in new CgtHappyScenario {
      val ftr = AgentFastTrackRequest(Some(business), HMRCCGTPD, "CGTPDRef", cgtRef.value, None)
      journeyState.set(
        ConfirmPostcodeCgt(ftr, ftr, None, Some("BN13 1FN"), "firstName lastName"),
        List(
          CheckDetailsCompleteCgt(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitConfirmCgtPostcode(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("postcode" -> "BN13 1XX"),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showNotMatched().url)
    }
  }

  "GET /agents/track/client-country" should {
    val request = FakeRequest("POST", "/agents/track/client-country")

    "redirect user to /agents/track/confirm-cgt-client if country codes match for a non UK client" in new CgtHappyScenario {
      val ftr = AgentFastTrackRequest(Some(business), HMRCCGTPD, "CGTPDRef", cgtRef.value, None)
      journeyState.set(
        ConfirmCountryCodeCgt(ftr, ftr, None, "FR", "firstName lastName"),
        List(
          CheckDetailsCompleteCgt(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showConfirmCgtCountryCode(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "confirm-countryCode-cgt.header", "confirm-countryCode-cgt.p1", "confirm-countryCode-cgt.hint")
    }

    "redirect user to /agents/no-match if countryCodes do not match for a non UK client" in new CgtHappyScenario {
      val ftr = AgentFastTrackRequest(Some(business), HMRCCGTPD, "CGTPDRef", cgtRef.value, None)
      journeyState.set(
        ConfirmCountryCodeCgt(ftr, ftr, None, "FR", "firstName lastName"),
        List(
          CheckDetailsCompleteCgt(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitConfirmCgtCountryCode(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("countryCode" -> "IN"),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showNotMatched().url)
    }
  }

  "GET /agents/more-details" should {
    val request = FakeRequest("GET", "/agents/fast-track/more-details")
    "show the more-details page for ITSA service" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW"))
      journeyState.set(
        NoPostcode(ftr, ftr, None),
        List(
          CheckDetailsCompleteItsa(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showKnownFact(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "known-fact.HMRC-MTD-IT.heading", "known-fact.HMRC-MTD-IT.helper")
    }

    "show the more-details page for IRV service" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", "AB123456A", Some(dateOfBirth))
      journeyState.set(NoDob(ftr, ftr, None), List())

      val result = controller.showKnownFact(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "known-fact.PERSONAL-INCOME-RECORD.heading",
        "known-fact.PERSONAL-INCOME-RECORD.helper")
    }

    "show the more-details page for VAT service" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "VRN", vrn, Some(validRegistrationDate))
      journeyState.set(NoVatRegDate(ftr, ftr, None), List())

      val result = controller.showKnownFact(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "known-fact.HMRC-MTD-VAT.heading", "known-fact.HMRC-MTD-VAT.helper")
    }
  }

  "GET /agents/more-details-itsa" should {
    val request = FakeRequest("GET", "/agents/more-details-itsa")
    "redirect to the identify client page" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", None)
      journeyState.set(NoPostcode(ftr, ftr, None), Nil)

      val result = controller.knownFactRedirect()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showKnownFact().url)
    }
  }

  "POST /agents/more-details-itsa" should {
    val request = FakeRequest("POST", "/agents/fast-track/more-details-itsa")
    "redirect to invitation-sent" in new ItsaHappyScenario {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", None)
      journeyState.set(
        NoPostcode(ftr, ftr, None),
        List(
          CheckDetailsCompleteItsa(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitKnownFactItsa(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("knownFact" -> "BN32TN"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
    }

    "redirect to not-matched (Postcode)" in new ItsaNotMatchedScenario {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", None)
      journeyState.set(
        NoPostcode(ftr, ftr, None),
        List(
          CheckDetailsCompleteItsa(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.submitKnownFactItsa(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("knownFact" -> "BN32TN"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showNotMatched().url)
    }

    "redirect to not-signed-up when the client is not signed up for the service" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, nino, HMRCMTDIT)
      givenCheckRelationshipItsaWithStatus(arn, nino, 404)
      givenNotEnrolledClientITSA(Nino(nino), "BN32TN")

      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", None)
      journeyState.set(NoPostcode(ftr, ftr, None), List())

      val result = controller.submitKnownFactItsa(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("knownFact" -> "BN32TN"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(
        routes.AgentInvitationFastTrackJourneyController.showClientNotSignedUp().url)

    }
  }

  "POST /agents/more-details-irv" should {
    val request = FakeRequest("POST", "/agents/fast-track/more-details-irv")
    "redirect to invitation-sent" in new IrvHappyScenario {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, None)
      journeyState.set(
        NoDob(ftr, ftr, None),
        List(
          CheckDetailsCompleteIrv(ftr, ftr, None),
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
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
    }

    "redirect to not-matched (DOB)" in new IrvNotMatchedScenario {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, None)
      journeyState.set(
        NoDob(ftr, ftr, None),
        List(
          CheckDetailsCompleteIrv(ftr, ftr, None),
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
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showNotMatched().url)
    }
  }

  "POST /agents/more-details-vat" should {
    val request = FakeRequest("POST", "/agents/fast-track/more-details-vat")
    "redirect to invitation-sent" in new VatHappyScenarioPersonal {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, Some("2010-10-10"))
      journeyState.set(
        NoVatRegDate(ftr, ftr, None),
        List(
          CheckDetailsCompletePersonalVat(ftr, ftr, None),
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
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
    }

    "redirect to not-matched (Vat Reg. Date)" in new VatNotMatchedScenario {
      val originalFtr = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, Some("1990-10-10"))
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, None)
      journeyState.set(
        NoVatRegDate(originalFtr, ftr, None),
        List(CheckDetailsCompletePersonalVat(originalFtr, originalFtr, None), Prologue(None, None))
      )

      val requestWithForm = request.withFormUrlEncodedBody(
        "knownFact.year"  -> "2010",
        "knownFact.month" -> "10",
        "knownFact.day"   -> "10"
      )

      val result = controller.submitKnownFactVat(authorisedAsValidAgent(requestWithForm, arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showNotMatched().url)
    }
  }

  "GET /agents/type-of-client" should {
    val request = FakeRequest("GET", "/agents/fast-track/client-type")
    "show the client-type page" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW"))
      journeyState.set(
        SelectClientTypeVat(ftr, ftr, None),
        List(
          CheckDetailsCompleteItsa(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showClientType(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-type.header"))
    }
  }

  "GET /agents/sent-invitation" should {
    val request = FakeRequest("GET", "/agents/fast-track/sent-invitation")
    "show the invitation sent page for a personal service" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW"))
      journeyState.set(
        InvitationSentPersonal("invitation/sent/url", None, "abc@xyz.com"),
        List(
          CheckDetailsCompleteItsa(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showInvitationSent(authorisedAsValidAgent(request, arn.value))
      val inferredDate = controller.inferredExpiryDate
      //not sure whys its coded like this in the views
      val expiryDate =
        if(inferredDate.getDayOfMonth < 10) {
          inferredDate.toString("d MMMM yyyy")
        } else {
          inferredDate.toString("dd MMMM yyyy")
        }

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "invitation-sent.header",
        "invitation-sent.l1",
        "invitation-sent.l1.p.personal.personal")
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.email.p", "abc@xyz.com", expiryDate))
    }

    "show the invitation sent page for a business service" in {
      val ftr = AgentFastTrackRequest(Some(business), HMRCMTDVAT, "vrn", vrn, Some(validRegistrationDate))
      journeyState.set(InvitationSentBusiness("invitation/sent/url", None, "abc@xyz.com"), List())

      val result = controller.showInvitationSent(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "invitation-sent.header",
        "invitation-sent.l1",
        "invitation-sent.l1.p.business.HMRC-MTD-VAT")
    }
  }

  "GET /agents/not-signed-up-single" should {
    val request = FakeRequest("GET", "/agents/fast-track/not-signed-up")
    "show the client-type page" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW"))
      journeyState.set(
        ClientNotSignedUp(ftr, None),
        List(
          CheckDetailsCompleteItsa(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showClientNotSignedUp(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("problem.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-enrolled.p1.HMRC-MTD-IT"))
    }
  }

  "GET /agents/no-match" should {
    val request = FakeRequest("GET", "/agents/fast-track/no-match")

    "show the client-type page" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW"))
      journeyState.set(
        KnownFactNotMatched(ftr, ftr, None),
        List(
          CheckDetailsCompleteItsa(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showNotMatched(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "not-matched.header")
    }
  }

  "GET /agents/authorisation-already-pending" should {
    val request = FakeRequest("GET", "/agents/fast-track/already-authorisation-pending")
    "show the already-authorisation-pending page" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW"))
      journeyState.set(
        PendingInvitationExists(ftr, None),
        List(
          CheckDetailsCompleteItsa(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showPendingAuthorisationExists(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "pending-authorisation-exists.no-requests.p")
      checkHtmlResultWithBodyMsgs(result, "pending-authorisation-exists.track.button")
    }
  }

  "GET /agents/authorisation-already-present" should {
    val request = FakeRequest("GET", "/agents/fast-track/already-authorisation-present")
    "show the already-authorisation-pending page" in {
      val ftr = AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW"))
      journeyState.set(
        ActiveAuthorisationExists(ftr, None),
        List(
          CheckDetailsCompleteItsa(ftr, ftr, None),
          Prologue(None, None)
        )
      )

      val result = controller.showActiveAuthorisationExists(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "active-authorisation-exists.header")
    }
  }

  class ItsaHappyScenario {
    givenGetAllPendingInvitationsReturnsEmpty(arn, nino, HMRCMTDIT)
    givenCheckRelationshipItsaWithStatus(arn, nino, 404)
    givenMatchingClientIdAndPostcode(Nino(nino), "BN32TN")
    givenTradingName(Nino(nino), "Sylvia Plath")
    givenInvitationCreationSucceeds(arn, Some(personal), nino, invitationIdITSA, nino, "ni", HMRCMTDIT, "NI")
    givenAgentReferenceRecordExistsForArn(arn, "FOO")
    givenAgentReference(arn, "uid", personal)
    givenGetAgencyEmailAgentStub
  }

  class ItsaNotMatchedScenario {
    givenGetAllPendingInvitationsReturnsEmpty(arn, nino, HMRCMTDIT)
    givenCheckRelationshipItsaWithStatus(arn, nino, 404)
    givenNonMatchingClientIdAndPostcode(Nino(nino), "BN32TN")
  }

  class IrvHappyScenario {
    givenGetAllPendingInvitationsReturnsEmpty(arn, nino, HMRCPIR)
    givenAfiRelationshipNotFoundForAgent(arn, Nino(nino))
    givenMatchingCitizenRecord(Nino(nino), LocalDate.parse("1990-10-10"))
    givenCitizenDetailsAreKnownFor(nino, "Virginia", "Woolf")
    givenInvitationCreationSucceeds(arn, Some(personal), nino, invitationIdPIR, nino, "ni", HMRCPIR, "NI")
    givenAgentReferenceRecordExistsForArn(arn, "FOO")
    givenAgentReference(arn, "uid", personal)
    givenGetAgencyEmailAgentStub
  }

  class IrvNotMatchedScenario {
    givenGetAllPendingInvitationsReturnsEmpty(arn, nino, HMRCPIR)
    givenAfiRelationshipNotFoundForAgent(arn, Nino(nino))
    givenNonMatchingCitizenRecord(Nino(nino), LocalDate.parse("1990-10-10"))
  }

  class VatHappyScenarioPersonal {
    givenGetAllPendingInvitationsReturnsEmpty(arn, vrn, HMRCMTDVAT)
    givenVatRegisteredClientReturns(Vrn(vrn), LocalDate.parse("2010-10-10"), 204)
    givenClientDetails(Vrn(vrn))
    givenCheckRelationshipVatWithStatus(arn, vrn, 404)
    givenInvitationCreationSucceeds(arn, Some(personal), nino, invitationIdVAT, vrn, "vrn", HMRCMTDVAT, "VRN")
    givenAgentReferenceRecordExistsForArn(arn, "FOO")
    givenAgentReference(arn, "uid", personal)
    givenGetAgencyEmailAgentStub
  }

  class VatHappyScenarioBusiness {
    givenGetAllPendingInvitationsReturnsEmpty(arn, vrn, HMRCMTDVAT)
    givenVatRegisteredClientReturns(Vrn(vrn), LocalDate.parse("2010-10-10"), 204)
    givenClientDetails(Vrn(vrn))
    givenCheckRelationshipVatWithStatus(arn, vrn, 404)
    givenInvitationCreationSucceeds(arn, Some(business), nino, invitationIdVAT, vrn, "vrn", HMRCMTDVAT, "VRN")
    givenAgentReferenceRecordExistsForArn(arn, "FOO")
    givenAgentReference(arn, "uid", business)
    givenGetAgencyEmailAgentStub
  }

  class TrustHappyScenario {
    givenGetAllPendingInvitationsReturnsEmpty(arn, validUtr.value, TRUST)

    givenTrustClientReturns(validUtr, 200, Json.toJson(trustResponse).toString())
    givenCheckRelationshipVatWithStatus(arn, validUtr.value, 404)
    givenInvitationCreationSucceeds(
      arn,
      Some(business),
      validUtr.value,
      invitationIdTrust,
      validUtr.value,
      "utr",
      TRUST,
      "UTR")
    givenAgentReferenceRecordExistsForArn(arn, "FOO")
    givenAgentReference(arn, "uid", business)
    givenGetAgencyEmailAgentStub
  }


  class CgtHappyScenario {
    givenGetAllPendingInvitationsReturnsEmpty(arn, cgtRef.value, HMRCCGTPD)
    givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription()).toString())
    givenCheckRelationshipVatWithStatus(arn, cgtRef.value, 404)
    givenInvitationCreationSucceeds(
      arn,
      Some(business),
      cgtRef.value,
      invitationIdCgt,
      cgtRef.value,
      "CGTPDRef",
      HMRCCGTPD,
      "CGTPDRef")
    givenAgentReferenceRecordExistsForArn(arn, "FOO")
    givenAgentReference(arn, "uid", business)
    givenGetAgencyEmailAgentStub
  }

  class VatNotMatchedScenario {
    givenGetAllPendingInvitationsReturnsEmpty(arn, vrn, HMRCMTDVAT)
    givenVatRegisteredClientReturns(Vrn(vrn), LocalDate.parse("2010-10-10"), 403)
    givenCheckRelationshipVatWithStatus(arn, vrn, 404)
  }
}
