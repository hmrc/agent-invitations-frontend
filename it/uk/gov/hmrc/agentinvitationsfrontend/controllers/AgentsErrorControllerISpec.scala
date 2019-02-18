package uk.gov.hmrc.agentinvitationsfrontend.controllers
import play.api.test.FakeRequest
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId
import play.api.test.Helpers._

import scala.concurrent.ExecutionContext.Implicits.global

class AgentsErrorControllerISpec extends BaseISpec with AuthBehaviours {

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  lazy val controller: AgentsErrorController = app.injector.instanceOf[AgentsErrorController]

  val itsaInvitation = ItsaInvitation(validNino, Some(Postcode(validPostcode)))
  val pirInvitation = PirInvitation(validNino, Some(DOB(dateOfBirth)))
  val vatInvitation = VatInvitation(business, validVrn, Some(VatRegDate(validRegistrationDate)))

  "GET /agents/not-matched" should {
    val request = FakeRequest("GET", "/agents/not-matched")
    val notMatched = controller.notMatched()

    "return 403 for authorised Agent who submitted not matching known facts if they have empty basket" in {
      val result = notMatched(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 403
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("generic.title", "There is a problem", htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, "The details you entered do not match to a client.")
      checkHtmlResultWithBodyText(result, "Check them and try again.")
      checkHtmlResultWithBodyText(result, "Try again")
      checkHtmlResultWithNotBodyText(result, "Return to your authorisation requests")

      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "return 403 for authorised Agent who submitted not matching known facts if they have a session with no basket" in {
      testAgentMultiAuthorisationJourneyStateCache.save(AgentMultiAuthorisationJourneyState("", Set.empty))

      val result = notMatched(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 403
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("generic.title", "There is a problem", htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, "The details you entered do not match to a client.")
      checkHtmlResultWithBodyText(result, "Check them and try again.")
      checkHtmlResultWithBodyText(result, "Try again")
      checkHtmlResultWithNotBodyText(result, "Return to your authorisation requests")
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "return 403 for authorised Agent who submitted not matching known facts if they have a basket" in {
      testAgentMultiAuthorisationJourneyStateCache.save(
        AgentMultiAuthorisationJourneyState(
          "personal",
          Set(AuthorisationRequest("Gareth Gates", itsaInvitation))))

      val result = notMatched(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 403
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("generic.title", "There is a problem", htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, "The details you entered do not match to a client.")
      checkHtmlResultWithBodyText(result, "Check them and try again.")
      checkHtmlResultWithBodyText(result, "Try again")
      checkHtmlResultWithBodyText(result, "Return to your authorisation requests")
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedAgentEndpoint(request, notMatched)
  }

  "GET /all-create-authorisation-failed" should {
    val request = FakeRequest("GET", "/all-create-authorisation-failed")
    "display the all create authorisation failed error page" in {
      val clientDetail1 =
        AuthorisationRequest(
          "Gareth Gates Sr",
          itsaInvitation,
          state = AuthorisationRequest.FAILED)
      val clientDetail2 =
        AuthorisationRequest(
          "Malcolm Pirson",
          pirInvitation,
          state = AuthorisationRequest.FAILED)
      val clientDetail3 =
        AuthorisationRequest(
          "Sara Vaterloo",
          vatInvitation,
          state = AuthorisationRequest.FAILED)

      testAgentMultiAuthorisationJourneyStateCache.save(
        AgentMultiAuthorisationJourneyState("personal", Set(clientDetail1, clientDetail2, clientDetail3)))

      val result = controller.allCreateAuthorisationFailed()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Sorry, there is a problem with the service",
        "We could not create the following authorisation requests.",
        "Gareth Gates Sr",
        "Report their income and expenses through software",
        "Malcolm Pirson",
        "View their PAYE income record",
        "Sara Vaterloo",
        "Report their VAT returns through software",
        "Try again"
      )
    }

    "throw an Exception if there is nothing in the cache" in {
      val result = controller.allCreateAuthorisationFailed()(authorisedAsValidAgent(request, arn.value))

      intercept[Exception] {
        await(result)
      }.getMessage shouldBe "Cached session state expected but not found"
    }
  }

  "GET /some-create-authorisation-failed" should {
    val request = FakeRequest("GET", "/some-create-authorisation-failed")
    "display the some create authorisation failed error page with more than one failed request" in {
      val clientDetail1 =
        AuthorisationRequest(
          "Gareth Gates Sr",
          itsaInvitation,
          state = AuthorisationRequest.FAILED)
      val clientDetail2 =
        AuthorisationRequest(
          "Malcolm Pirson",
          pirInvitation,
          state = AuthorisationRequest.CREATED)
      val clientDetail3 =
        AuthorisationRequest(
          "Sara Vaterloo",
          vatInvitation,
          state = AuthorisationRequest.FAILED)

      testAgentMultiAuthorisationJourneyStateCache.save(
        AgentMultiAuthorisationJourneyState("personal", Set(clientDetail1, clientDetail2, clientDetail3)))

      val result = controller.someCreateAuthorisationFailed()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Sorry, there is a problem with the service",
        "We could not create the following authorisation requests.",
        "Gareth Gates Sr",
        "Report their income and expenses through software",
        "Sara Vaterloo",
        "Report their VAT returns through software",
        "You can continue without these requests",
        "Continue",
        "You can continue without these requests"
      )
      checkHtmlResultWithNotBodyText(result, "Malcolm Pirson", "View their PAYE income record")
    }

    "display the some creation failed error page with one failed request" in {
      val clientDetail1 =
        AuthorisationRequest(
          "Gareth Gates Sr",
          itsaInvitation,
          state = AuthorisationRequest.FAILED)
      val clientDetail2 =
        AuthorisationRequest(
          "Malcolm Pirson",
          pirInvitation,
          state = AuthorisationRequest.CREATED)
      val clientDetail3 =
        AuthorisationRequest(
          "Sara Vaterloo",
          vatInvitation,
          state = AuthorisationRequest.CREATED)

      testAgentMultiAuthorisationJourneyStateCache.save(
        AgentMultiAuthorisationJourneyState("personal", Set(clientDetail1, clientDetail2, clientDetail3)))

      val result = controller.someCreateAuthorisationFailed()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "We could not create the following authorisation request.",
        "You can continue without this request"
      )
      checkHtmlResultWithNotBodyText(result, "Malcolm Pirson", "View their PAYE income record")

    }

    "throw an Exception if there is nothing in the cache" in {
      val result = controller.someCreateAuthorisationFailed()(authorisedAsValidAgent(request, arn.value))

      intercept[Exception] {
        await(result)
      }.getMessage shouldBe "Cached session state expected but not found"
    }
  }

  "GET /already-authorisation-present" should {
    val request = FakeRequest("GET", "/already-authorisation-present")
    "display the already authorisation present page when there are no requests in the journey cache" in {
      testAgentMultiAuthorisationJourneyStateCache.save(AgentMultiAuthorisationJourneyState("personal", Set.empty))
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(Some("personal"), serviceITSA, "ni", nino, Some(validPostcode)))

      val result = controller.activeRelationshipExists()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "You are already authorised",
        "This client has already authorised you to report their income and expenses through software.",
        "Start a new request"
      )
    }

    "display the already authorisation present page when there are some requests in the journey cache" in {
      val clientDetail1 =
        AuthorisationRequest(
          "Gareth Gates Sr",
          itsaInvitation,
          state = AuthorisationRequest.FAILED)
      testAgentMultiAuthorisationJourneyStateCache.save(
        AgentMultiAuthorisationJourneyState("personal", Set(clientDetail1)))
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(Some("personal"), serviceITSA, "ni", nino, Some(validPostcode)))

      val result = controller.activeRelationshipExists()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "You are already authorised",
        "This client has already authorised you to report their income and expenses through software.",
        "Return to your authorisation requests"
      )
    }

    "Display the page when there is nothing in the journeyStateCache" in {
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(Some("personal"), serviceITSA, "ni", nino, Some(validPostcode)))

      val result = controller.activeRelationshipExists()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "You are already authorised",
        "This client has already authorised you to report their income and expenses through software.",
        "Start a new request"
      )
    }

    "Display the page when coming from fast track" in {
      val clientDetail1 =
        AuthorisationRequest(
          "Gareth Gates Sr",
          itsaInvitation,
          state = AuthorisationRequest.FAILED)
      testAgentMultiAuthorisationJourneyStateCache.save(
        AgentMultiAuthorisationJourneyState("personal", Set(clientDetail1)))
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(Some("personal"), serviceITSA, "ni", nino, Some(validPostcode), true))

      val result = controller.activeRelationshipExists()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "You are already authorised",
        "This client has already authorised you to report their income and expenses through software."
      )
      checkHtmlResultWithNotBodyText(result, "Start a new request", "Return to your authorisation requests")
    }

    "throw an Exception if there is nothing in either of the caches" in {
      val result = controller.activeRelationshipExists()(authorisedAsValidAgent(request, arn.value))

      intercept[Exception] {
        await(result)
      }.getMessage shouldBe "Cached session state expected but not found"

    }
  }

  "GET /agents/cannot-create-request" should {
    val request = FakeRequest("GET", "/agents/cannot-create-request")

    "display cannot-create-request page when there are existing authorisation requests in the basket" in {
      val clientDetail1 =
        AuthorisationRequest(
          "Gareth Gates Sr",
          vatInvitation,
          state = AuthorisationRequest.FAILED)
      testAgentMultiAuthorisationJourneyStateCache.save(
        AgentMultiAuthorisationJourneyState("personal", Set(clientDetail1)))
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(Some("personal"), serviceVAT, "vrn", validVrn.value, Some(validRegistrationDate), false))

      val result = controller.cannotCreateRequest()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "You cannot ask this client to authorise you yet",
        htmlEscapedMessage("We are processing this client's sign-up to Making Tax Digital for VAT."),
        "Once this is done, you can ask the client to authorise you for this service.",
        "This may take up to 72 hours, so try again later.",
        "Return to your authorisation requests"
      )
      checkHtmlResultWithNotBodyText(result, "Start a new request")
    }

    "display cannot-create-request page when the basket is empty" in {
      testAgentMultiAuthorisationJourneyStateCache.save(
        AgentMultiAuthorisationJourneyState("personal", Set.empty))
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(Some("personal"), serviceVAT, "vrn", validVrn.value, Some(validRegistrationDate), false))

      val result = controller.cannotCreateRequest()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "You cannot ask this client to authorise you yet",
        htmlEscapedMessage("We are processing this client's sign-up to Making Tax Digital for VAT."),
        "Once this is done, you can ask the client to authorise you for this service.",
        "This may take up to 72 hours, so try again later.",
        "Start a new request"
      )
      checkHtmlResultWithNotBodyText(result, "Return to your authorisation requests")
    }

    "display cannot-create-request page when going through fast-track" in {
      testAgentMultiAuthorisationJourneyStateCache.save(
        AgentMultiAuthorisationJourneyState("personal", Set.empty))
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(Some("personal"), serviceVAT, "vrn", validVrn.value, Some(validRegistrationDate), true))

      val result = controller.cannotCreateRequest()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "You cannot ask this client to authorise you yet",
        htmlEscapedMessage("We are processing this client's sign-up to Making Tax Digital for VAT."),
        "Once this is done, you can ask the client to authorise you for this service.",
        "This may take up to 72 hours, so try again later.",
        "Start a new request"
      )
      checkHtmlResultWithNotBodyText(result, "Return to your authorisation requests")
    }
  }
}
