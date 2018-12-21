package uk.gov.hmrc.agentinvitationsfrontend.controllers

import org.joda.time.LocalDate
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationsIRVControllerJourneyISpec extends BaseISpec with AuthBehaviours {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  "POST /agents/select-service" should {
    val request = FakeRequest("POST", "/agents/select-service")
    val submitService = controller.submitSelectService()

    "return 303 for authorised Agent with valid Personal Income Record service, redirect to identify client" in {
      testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(personal, servicePIR))
      val serviceForm = agentInvitationServiceForm.fill(UserInputNinoAndPostcode(personal, servicePIR, None, None))
      val result =
        submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/identify-client")
      verifyAuthoriseAttempt()
    }
  }

  "GET /agents/identify-client" should {
    val request = FakeRequest("GET", "/agents/identify-client")
    val showIdentifyClientForm = controller.showIdentifyClient()

    behave like anAuthorisedAgentEndpoint(request, showIdentifyClientForm)

    "return 200 for an Agent with HMRC-AS-AGENT enrolment for IRV service" in {
      testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(personal, servicePIR))
      val result = showIdentifyClientForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200

      checkHtmlResultWithBodyMsgs(result, "identify-client.header", "title.suffix.agents")

      checkHtmlResultWithBodyMsgs(
        result,
        "identify-client.itsa.p1",
        "identify-client.nino.hint",
        "identify-client.irv-date-of-birth.label",
        "identify-client.irv-date-of-birth.hint"
      )

      checkHasAgentSignOutLink(result)
    }
  }

  "POST /agents/identify-client" when {
    val request = FakeRequest("POST", "/agents/identify-client")
    val submitIdentifyClient = controller.submitIdentifyClient()

    behave like anAuthorisedAgentEndpoint(request, submitIdentifyClient)

    "service is PERSONAL-INCOME-RECORD" should {

      "redirect to review-authorisation when a valid NINO is submitted" in {
        givenInvitationCreationSucceeds(arn, validNino.value, invitationIdPIR, validNino.value, "ni", servicePIR, "NI")
        givenAgentReference(arn, "ABCDEFGH", "personal")
        givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
        givenCitizenDetailsAreKnownFor(validNino.value, "First", "Last")
        givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, servicePIR)

        testCurrentAuthorisationRequestCache.save(
          CurrentAuthorisationRequest(personal, servicePIR, "ni", validNino.value, Some(dateOfBirth)))
        val requestWithForm =
          request.withFormUrlEncodedBody(
            "clientType"       -> "personal",
            "service"          -> servicePIR,
            "clientIdentifier" -> validNino.value,
            "knownFact.year"   -> "1980",
            "knownFact.month"  -> "07",
            "knownFact.day"    -> "07"
          )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showReviewAuthorisations().url)
      }

      "redirect to client-type when a valid NINO is submitted but cache is empty" in {
        givenInvitationCreationSucceeds(arn, validNino.value, invitationIdPIR, validNino.value, "ni", servicePIR, "NI")
        givenAgentReference(arn, "ABCDEFGH", "personal")
        givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
        givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, servicePIR)

        val requestWithForm =
          request.withFormUrlEncodedBody(
            "clientType"       -> "personal",
            "service"          -> servicePIR,
            "clientIdentifier" -> validNino.value,
            "knownFact.year"   -> "1980",
            "knownFact.month"  -> "07",
            "knownFact.day"    -> "07"
          )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showClientType().url)
      }

      "redirect to already-authorisation-pending when a valid NINO is submitted but authorisation already exists" in {
        givenGetAllPendingInvitationsReturnsSome(arn, validNino.value, servicePIR)

        testCurrentAuthorisationRequestCache.save(
          CurrentAuthorisationRequest(personal, servicePIR, "ni", validNino.value, Some(dateOfBirth)))
        val requestWithForm =
          request.withFormUrlEncodedBody(
            "clientType"       -> "personal",
            "service"          -> servicePIR,
            "clientIdentifier" -> validNino.value,
            "knownFact.year"   -> "1980",
            "knownFact.month"  -> "07",
            "knownFact.day"    -> "07"
          )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.pendingAuthorisationExists().url)
      }

      "redisplay page with errors when an empty NINO is submitted" in {
        val requestWithForm = request.withFormUrlEncodedBody("service" -> servicePIR, "clientIdentifier" -> "")
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.nino.required")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an invalid NINO is submitted" in {
        val requestWithForm = request.withFormUrlEncodedBody("service" -> servicePIR, "clientIdentifier" -> "invalid")
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "enter-nino.invalid-format")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an no date of birth is submitted" in {
        val requestWithForm = request.withFormUrlEncodedBody(
          "service"          -> servicePIR,
          "clientIdentifier" -> validNino.value,
          "knownFact.year"   -> "",
          "knownFact.month"  -> "",
          "knownFact.day"    -> ""
        )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.irv-date-of-birth.required")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an invalid date of birth is submitted" in {
        val requestWithForm = request.withFormUrlEncodedBody(
          "service"          -> servicePIR,
          "clientIdentifier" -> validNino.value,
          "knownFact.year"   -> "9999",
          "knownFact.month"  -> "99",
          "knownFact.day"    -> "99"
        )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "enter-irv-date-of-birth.invalid-format")
        checkHasAgentSignOutLink(result)
      }

      "redirect to /agents/select-service if service is missing" in {
        val requestWithForm = request.withFormUrlEncodedBody("service" -> "", "clientIdentifier" -> validNino.value)
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showSelectService().url)
      }
    }
  }

  "GET /agents/invitation-sent" should {
    val request = FakeRequest("GET", "/agents/invitation-sent")
    val invitationSent = controller.showInvitationSent()

    "return 200 for authorised Agent successfully created IRV invitation and redirected to Confirm Invitation Page (secureFlag = false) with no continue Url" in {
      givenAgentReference(arn, uid, "personal")
      val authRequest =
        AuthorisationRequest("clienty name", servicePIR, validNino.value, AuthorisationRequest.CREATED, "itemId")
      testAgentMultiAuthorisationJourneyStateCache.save(
        AgentMultiAuthorisationJourneyState("personal", Set(authRequest)))

      val result = invitationSent(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("invitation-sent-link.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.header"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l2", "someurl"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l1.p.personal"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.p2.personal"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.trackRequests.button"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueToASAccount.button"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.startNewAuthRequest"))
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          s"$wireMockBaseUrlAsString${routes.ClientsMultiInvitationController.warmUp("personal", uid, "99-with-flake")}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkInviteSentExitSurveyAgentSignOutLink(result)

      verifyAuthoriseAttempt()
      await(testCurrentAuthorisationRequestCache.fetch).get shouldBe CurrentAuthorisationRequest()
    }

    "throw a IllegalStateException when there is no link in the cache" in {
      val result = invitationSent(authorisedAsValidAgent(request, arn.value))
      intercept[IllegalStateException] {
        await(result)
      }.getMessage shouldBe "Cached session state expected but not found"
    }
  }

}
