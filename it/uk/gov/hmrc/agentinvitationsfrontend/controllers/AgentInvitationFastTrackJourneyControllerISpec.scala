package uk.gov.hmrc.agentinvitationsfrontend.controllers

import com.codahale.metrics.SharedMetricRegistries
import org.joda.time.LocalDate
import org.scalatest.BeforeAndAfter
import play.api.Application
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.personal
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
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
        journeyState.get shouldBe Some(
          CheckDetailsCompleteItsa(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", submittedNinoStr.toUpperCase, Some("BN32TN")),
            None),
          List(Prologue(None)))
      }
    }

    "redirect to check-details if all values in request are valid with a continue and error url query parameters" in {
      withWhitelistedDomains
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

    "redirect to the error url with appended error reason if all values in request are valid with a continue and error url query parameters" in {
      withWhitelistedDomains
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
      withWhitelistedDomains
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
      withWhitelistedDomains
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
    "show the check-details page" in {
      journeyState.set(
        CheckDetailsCompleteItsa(
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN32TN")),
          None),
        List(Prologue(None)))

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "check-details.heading",
        "check-details.p.HMRC-MTD-IT",
        "check-details.client-type.personal")
    }
  }

  "POST /agents/check-details" should {
    val request = FakeRequest("POST", "/agents/fast-track/check-details")
    "redirect to invitation-sent" in new ItsaHappyScenario {
      givenGetAgencyEmailAgentStub
      journeyState.set(
        CheckDetailsCompleteItsa(AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, Some("BN32TN")), None),
        List(Prologue(None)))

      val result = controller.submitCheckDetails(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
    }

    "redirect to client-identify" in {
      journeyState.set(
        CheckDetailsCompleteItsa(AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, Some("BN114AW")), None),
        List(Prologue(None)))

      val result = controller.submitCheckDetails(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showIdentifyClient().url)
    }
  }

  "GET agents/client-identify" should {
    val request = FakeRequest("GET", "/agents/fast-track/identify-client")
    "show the identify-client page" in {
      journeyState.set(
        IdentifyPersonalClient(
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW")),
          None),
        List(
          CheckDetailsCompleteItsa(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW")),
            None),
          Prologue(None))
      )

      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "identify-client.header", "identify-client.postcode.label")
    }
  }

  "GET /agents/client-identify-itsa" should {
    val request = FakeRequest("GET", "/agents/client-identify-itsa")
    "redirect to the identify client page" in {
      journeyState.set(
        IdentifyPersonalClient(
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW")),
          None),
        Nil)

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
        journeyState.set(
          IdentifyPersonalClient(AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, Some("BN114AW")), None),
          List(
            CheckDetailsCompleteItsa(
              AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", nino, Some("BN114AW")),
              None),
            Prologue(None))
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
        journeyState.set(
          IdentifyPersonalClient(AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, Some("1990-10-10")), None),
          List(
            CheckDetailsCompleteIrv(
              AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, Some("1990-10-10")),
              None),
            Prologue(None))
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
    "redirect to invitation-sent" in new VatHappyScenario {
      journeyState.set(
        IdentifyPersonalClient(AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, Some("2010-10-10")), None),
        List(
          CheckDetailsCompletePersonalVat(
            AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, Some("1990-10-10")),
            None),
          Prologue(None))
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
  }

  "GET /agents/more-details" should {
    val request = FakeRequest("GET", "/agents/fast-track/more-details")
    "show the more-details page" in {
      journeyState.set(
        NoPostcode(AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW")), None),
        List(
          CheckDetailsCompleteItsa(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW")),
            None),
          Prologue(None))
      )

      val result = controller.showKnownFact(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "known-fact.HMRC-MTD-IT.heading", "known-fact.HMRC-MTD-IT.helper")
    }
  }

  "GET /agents/more-details-itsa" should {
    val request = FakeRequest("GET", "/agents/more-details-itsa")
    "redirect to the identify client page" in {
      journeyState.set(NoPostcode(AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", None), None), Nil)

      val result = controller.knownFactRedirect()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showKnownFact().url)
    }
  }

  "POST /agents/more-details-itsa" should {
    val request = FakeRequest("POST", "/agents/fast-track/more-details-itsa")
    "redirect to invitation-sent" in new ItsaHappyScenario {
      journeyState.set(
        NoPostcode(AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", None), None),
        List(
          CheckDetailsCompleteItsa(AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", None), None),
          Prologue(None))
      )

      val result = controller.submitKnownFactItsa(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("knownFact" -> "BN32TN"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showInvitationSent().url)
    }
  }

  "POST /agents/more-details-irv" should {
    val request = FakeRequest("POST", "/agents/fast-track/more-details-irv")
    "redirect to invitation-sent" in new IrvHappyScenario {
      journeyState.set(
        NoDob(AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, None), None),
        List(
          CheckDetailsCompleteIrv(AgentFastTrackRequest(Some(personal), HMRCPIR, "ni", nino, None), None),
          Prologue(None))
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
  }

  "POST /agents/more-details-vat" should {
    val request = FakeRequest("POST", "/agents/fast-track/more-details-vat")
    "redirect to invitation-sent" in new VatHappyScenario {
      journeyState.set(
        NoVatRegDate(AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, Some("2010-10-10")), None),
        List(
          CheckDetailsCompletePersonalVat(
            AgentFastTrackRequest(Some(personal), HMRCMTDVAT, "vrn", vrn, Some("1990-10-10")),
            None),
          Prologue(None))
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
  }

  "GET /agents/type-of-client" should {
    val request = FakeRequest("GET", "/agents/fast-track/client-type")
    "show the client-type page" in {
      journeyState.set(
        SelectClientTypeVat(AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW")), None),
        List(
          CheckDetailsCompleteItsa(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW")),
            None),
          Prologue(None))
      )

      val result = controller.showClientType(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-type.header"))
    }
  }

  "GET /agents/sent-invitation" should {
    val request = FakeRequest("GET", "/agents/fast-track/invitation-sent")
    "show the client-type page" in {
      journeyState.set(
        InvitationSentPersonal("invitation/sent/url", None, "abc@xyz.com"),
        List(
          CheckDetailsCompleteItsa(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW")),
            None),
          Prologue(None))
      )

      val result = controller.showInvitationSent(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.header"))
    }
  }

  "GET /agents/not-signed-up-single" should {
    val request = FakeRequest("GET", "/agents/fast-track/not-signed-up")
    "show the client-type page" in {
      journeyState.set(
        ClientNotSignedUp(AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW")), None),
        List(
          CheckDetailsCompleteItsa(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW")),
            None),
          Prologue(None))
      )

      val result = controller.showClientNotSignedUp(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("problem.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-enrolled.p1.HMRC-MTD-IT"))
    }
  }

  "GET /agents/not-matched-single" should {
    val request = FakeRequest("GET", "/agents/fast-track/not-matched")
    "show the client-type page" in {
      journeyState.set(
        KnownFactNotMatched(AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW")), None),
        List(
          CheckDetailsCompleteItsa(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW")),
            None),
          Prologue(None))
      )

      val result = controller.showNotMatched(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "not-matched.header")
    }
  }

  "GET /agents/authorisation-already-pending" should {
    val request = FakeRequest("GET", "/agents/fast-track/already-authorisation-pending")
    "show the already-authorisation-pending page" in {
      journeyState.set(
        PendingInvitationExists(
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW")),
          None),
        List(
          CheckDetailsCompleteItsa(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW")),
            None),
          Prologue(None))
      )

      val result = controller.showPendingAuthorisationExists(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "pending-authorisation-exists.no-requests.p")
      checkHtmlResultWithBodyMsgs(result, "pending-authorisation-exists.track.button")
    }

    "show the already-authorisation-pending page with no track button when track request flag is off" in {
      SharedMetricRegistries.clear()
      appOppositeJourneyState.set(
        PendingInvitationExists(
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW")),
          None),
        List(
          CheckDetailsCompleteItsa(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW")),
            None),
          Prologue(None))
      )

      val result = oppositeController.showPendingAuthorisationExists(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "pending-authorisation-exists.no-requests.p")
      checkHtmlResultWithNotBodyText(result, "Track your authorisation requests")
    }
  }

  "GET /agents/authorisation-already-present" should {
    val request = FakeRequest("GET", "/agents/fast-track/already-authorisation-present")
    "show the already-authorisation-pending page" in {
      journeyState.set(
        ActiveAuthorisationExists(
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW")),
          None),
        List(
          CheckDetailsCompleteItsa(
            AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN114AW")),
            None),
          Prologue(None))
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

  class VatHappyScenario {
    givenGetAllPendingInvitationsReturnsEmpty(arn, vrn, HMRCMTDVAT)
    givenVatRegisteredClientReturns(Vrn(vrn), LocalDate.parse("2010-10-10"), 204)
    givenClientDetails(Vrn(vrn))
    givenCheckRelationshipVatWithStatus(arn, vrn, 404)
    givenInvitationCreationSucceeds(arn, Some(personal), nino, invitationIdVAT, vrn, "vrn", HMRCMTDVAT, "VRN")
    givenAgentReferenceRecordExistsForArn(arn, "FOO")
    givenAgentReference(arn, "uid", personal)
    givenGetAgencyEmailAgentStub
  }
}
