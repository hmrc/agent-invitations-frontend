package uk.gov.hmrc.agentinvitationsfrontend.controllers

import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.models.{CurrentInvitationInput, UserInputNinoAndPostcode, UserInputNinoAndDob}
import uk.gov.hmrc.agentinvitationsfrontend.support.{BaseISpec, TestDataCommonSupport}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationsITSAControllerJourneyISpec extends BaseISpec with AuthBehaviours with TestDataCommonSupport {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  "POST /agents/select-service" should {
    val request = FakeRequest("POST", "/agents/select-service")
    val submitService = controller.submitService()

    "return 303 for authorised Agent with valid ITSA service, redirect to enter identify-client page" in {
      testFastTrackCache.save(CurrentInvitationInput(serviceITSA))
      val serviceForm = agentInvitationServiceForm.fill(UserInputNinoAndPostcode(serviceITSA, None, None))
      val result =
        submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/identify-client")
      verifyAuthoriseAttempt()
    }
  }

  "GET /agents/identify-client" should {
    val request = FakeRequest("GET", "/agents/identify-client")
    val showIdentifyClientForm = controller.showIdentifyClientForm()

    behave like anAuthorisedAgentEndpoint(request, showIdentifyClientForm)

    "return 200 for an Agent with HMRC-AS-AGENT enrolment for ITSA service" in {
      testFastTrackCache.save(CurrentInvitationInput(serviceITSA))
      val result = showIdentifyClientForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(
        result,
        hasMessage(
          "generic.title",
          htmlEscapedMessage("identify-client.header"),
          htmlEscapedMessage("title.suffix.agents")))

      checkHtmlResultWithBodyMsgs(
        result,
        "identify-client.header",
        "identify-client.itsa.p1",
        "identify-client.nino.label",
        "identify-client.nino.hint",
        "identify-client.postcode.label",
        "identify-client.postcode.hint"
      )

      checkHasAgentSignOutLink(result)
    }
  }

  "POST /agents/identify-client" when {
    val request = FakeRequest("POST", "/agents/identify-client")
    val submitIdentifyClient = controller.submitIdentifyClient()

    behave like anAuthorisedAgentEndpoint(request, submitIdentifyClient)

    "service is HMRC-MTD-IT" should {

      "redirect to /agents/complete when a valid NINO and postcode are submitted" in {
        createInvitationStub(
          arn,
          validNino.value,
          invitationIdITSA,
          validNino.value,
          "ni",
          "HMRC-MTD-IT",
          "NI")
        givenMatchingClientIdAndPostcode(validNino, validPostcode)
        getInvitationStub(arn, validNino.value, invitationIdITSA, serviceITSA, "NI", "Pending")

        testFastTrackCache.save(
          CurrentInvitationInput(Some("HMRC-MTD-IT"), None, Some(validNino.value), Some(validPostcode)))
        val requestWithForm = request.withFormUrlEncodedBody(
          "service" -> "HMRC-MTD-IT",
          "clientIdentifier" -> validNino.value,
          "knownFact" -> validPostcode)
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.invitationSent().url)
      }

      "redisplay page with errors when an empty NINO is submitted" in {
        val requestWithForm = request
          .withFormUrlEncodedBody("service" -> "HMRC-MTD-IT", "clientIdentifier" -> "", "knownFact" -> validPostcode)
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.nino.required")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an invalid NINO is submitted" in {
        val requestWithForm = request.withFormUrlEncodedBody(
          "service" -> "HMRC-MTD-IT",
          "clientIdentifier" -> "invalid",
          "knownFact" -> validPostcode)
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "enter-nino.invalid-format")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an empty postcode is submitted" in {
        val requestWithForm = request
          .withFormUrlEncodedBody("service" -> "HMRC-MTD-IT", "clientIdentifier" -> validNino.value, "knownFact" -> "")
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.postcode.required")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when a postcode with invalid format is submitted" in {
        val requestWithForm = request.withFormUrlEncodedBody(
          "service" -> "HMRC-MTD-IT",
          "clientIdentifier" -> validNino.value,
          "knownFact" -> "invalid")
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "enter-postcode.invalid-format")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when a postcode with invalid characters is submitted" in {
        val requestWithForm = request.withFormUrlEncodedBody(
          "service" -> "HMRC-MTD-IT",
          "clientIdentifier" -> validNino.value,
          "knownFact" -> "invalid%")
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "enter-postcode.invalid-characters")
        checkHasAgentSignOutLink(result)
      }

      "redirect to /agents/select-service if service is missing" in {
        val requestWithForm = request
          .withFormUrlEncodedBody("service" -> "", "clientIdentifier" -> validNino.value, "knownFact" -> validPostcode)
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.selectService().url)
      }
    }
  }

    "GET /agents/invitation-sent" should {
      val request = FakeRequest("GET", "/agents/invitation-sent")
      val invitationSent = controller.invitationSent()

      "return 200 for authorised Agent successfully created ITSA invitation and redirected to Confirm Invitation Page (secureFlag = false) with no continue Url" in {
        val invitation =
          CurrentInvitationInput(Some(serviceITSA), Some("ni"), Some(validNino.value), Some("AB101AB"))
        testFastTrackCache.save(invitation)
        testFastTrackCache.currentSession.currentInvitationInput.get shouldBe invitation

        val result = invitationSent(
          authorisedAsValidAgent(
            request.withSession("invitationId" -> "ABERULMHCKKW3", "deadline" -> "27 December 2017"),
            arn.value))
        status(result) shouldBe 200
        checkHtmlResultWithBodyText(
          result,
          htmlEscapedMessage(
            "generic.title",
            htmlEscapedMessage("invitation-sent-link.header"),
            htmlEscapedMessage("title.suffix.agents")))
        checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.header"))
        checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l2", "someurl"))
        checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.p1"))
        checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.p2", "27 December 2017"))
        checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.trackRequests.button"))
        checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueToASAccount.button"))
        checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.startNewAuthRequest"))
        checkHtmlResultWithBodyText(
          result,
          htmlEscapedMessage(s"$wireMockBaseUrlAsString${routes.ClientsInvitationController.start(invitationIdITSA)}"))
        checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
        checkInviteSentExitSurveyAgentSignOutLink(result)

        verifyAuthoriseAttempt()
        await(testFastTrackCache.fetch()).get shouldBe CurrentInvitationInput()
      }
    }

    "GET /agents/not-matched" should {
      val request = FakeRequest("GET", "/agents/not-matched")
      val notMatched = controller.notMatched()

      "return 403 for authorised Agent who submitted not matching known facts for ITSA" in {
        val invitation =
          CurrentInvitationInput(Some(serviceITSA), Some("ni"), Some(validNino.value), Some("AB101AB"))
        testFastTrackCache.save(invitation)

        val result = notMatched(authorisedAsValidAgent(request, arn.value))

        status(result) shouldBe 403
        checkHtmlResultWithBodyText(
          result,
          htmlEscapedMessage(
            "generic.title",
            htmlEscapedMessage("not-matched.itsa.header"),
            htmlEscapedMessage("title.suffix.agents")))
        checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-matched.itsa.description"))
        checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-matched.itsa.button"))
        checkHasAgentSignOutLink(result)
        verifyAuthoriseAttempt()
        await(testFastTrackCache.fetch()).get shouldBe invitation
      }
    }

    "GET /agents/not-enrolled" should {
      val request = FakeRequest("GET", "/agents/not-enrolled")
      val notEnrolled = controller.notEnrolled()
      val featureFlags = FeatureFlags()

      "return 403 for authorised Agent who submitted known facts of an not enrolled ITSA client" in {
        testFastTrackCache.save(CurrentInvitationInput(serviceITSA))
        val ninoForm =
          agentInvitationIdentifyClientFormItsa(featureFlags).fill(UserInputNinoAndPostcode(serviceITSA, None, None))
        val result =
          notEnrolled(authorisedAsValidAgent(request.withFormUrlEncodedBody(ninoForm.data.toSeq: _*), arn.value))

        status(result) shouldBe 403
        checkHtmlResultWithBodyText(
          result,
          htmlEscapedMessage(
            "generic.title",
            htmlEscapedMessage("not-enrolled.itsa.header"),
            htmlEscapedMessage("title.suffix.agents")))
        checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-enrolled.itsa.description"))
        checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-enrolled.itsa.button"))
        checkHasAgentSignOutLink(result)
        verifyAuthoriseAttempt()
        await(testFastTrackCache.fetch()).get shouldBe CurrentInvitationInput()

      }
    }


    //  "GET /confirm-client" should {
    //    val request = FakeRequest("GET", "/agents/confirm-client")
    //    val showConfirmClient = controller.showConfirmClient()
    //
    //    "return 200 and show client name" in {
    //      testFastTrackCache.save(
    //        CurrentInvitationInput(
    //          Some(serviceITSA),
    //          Some("ni"),
    //          Some(validNino.value),
    //          Some(validPostcode),
    //          None,
    //          fromFastTrack))
    //      givenTradingName(validNino, "64 Bit")
    //      val result = showConfirmClient(authorisedAsValidAgent(request, arn.value))
    //      status(result) shouldBe 200
    //      checkHtmlResultWithBodyText(result, "64 Bit")
    //      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")
    //      checkHtmlResultWithBodyMsgs(result, "confirm-client.yes")
    //      checkHtmlResultWithBodyMsgs(result, "confirm-client.no")
    //    }
    //
    //    "return 200 and no client name was found" in {
    //      testFastTrackCache.save(
    //        CurrentInvitationInput(
    //          Some(serviceITSA),
    //          Some("ni"),
    //          Some(validNino.value),
    //          Some(validPostcode),
    //          None,
    //          fromFastTrack))
    //      givenTradingNameMissing(validNino)
    //      val result = showConfirmClient(authorisedAsValidAgent(request, arn.value))
    //      status(result) shouldBe 200
    //      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")
    //      checkHtmlResultWithBodyMsgs(result, "confirm-client.yes")
    //      checkHtmlResultWithBodyMsgs(result, "confirm-client.no")
    //    }
    //
    //    behaveLikeMissingCacheScenarios(showConfirmClient, request)
    //  }
    //
    //  "POST /confirm-client" should {
    //    val request = FakeRequest("POST", "/agents/confirm-client")
    //    val submitConfirmClient = controller.submitConfirmClient()
    //
    //    "redirect to invitation-sent" in {
    //      testFastTrackCache.save(
    //        CurrentInvitationInput(
    //          Some(serviceITSA),
    //          Some("ni"),
    //          Some(validNino.value),
    //          Some(validPostcode),
    //          None,
    //          fromFastTrack))
    //      createInvitationStubWithKnownFacts(
    //        arn,
    //        mtdItId.value,
    //        invitationIdITSA,
    //        validNino.value,
    //        serviceITSA,
    //        "NI",
    //        Some(validPostcode))
    //      givenTradingName(validNino, "64 Bit")
    //      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, "NI", "Pending")
    //      val choice = agentConfirmClientForm.fill(Confirmation(true))
    //      val result =
    //        submitConfirmClient(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
    //      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    //      status(result) shouldBe 303
    //    }
    //
    //    "return 200 for not selecting an option" in {
    //      testFastTrackCache.save(
    //        CurrentInvitationInput(
    //          Some(serviceITSA),
    //          Some("ni"),
    //          Some(validNino.value),
    //          Some(validPostcode),
    //          None,
    //          fromFastTrack))
    //      givenTradingName(validNino, "64 Bit")
    //      val result = submitConfirmClient(authorisedAsValidAgent(request, arn.value))
    //      status(result) shouldBe 200
    //      givenTradingName(validNino, "64 Bit")
    //      checkHtmlResultWithBodyMsgs(result, "error.confirm-client.required")
    //      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")
    //      checkHtmlResultWithBodyMsgs(result, "confirm-client.yes")
    //      checkHtmlResultWithBodyMsgs(result, "confirm-client.no")
    //    }
    //
    //    behaveLikeMissingCacheScenarios(submitConfirmClient, request)
    //  }
    //
    //  def behaveLikeMissingCacheScenarios(action: Action[AnyContent], request: FakeRequest[AnyContentAsEmpty.type]) = {
    //    "return to identify-client no client identifier found in cache" in {
    //      testFastTrackCache.save(CurrentInvitationInput(Some(serviceITSA), None, None, None, None, fromFastTrack))
    //      val result = action(authorisedAsValidAgent(request, arn.value))
    //      status(result) shouldBe 303
    //      redirectLocation(result) shouldBe Some("/invitations/agents/identify-client")
    //    }
    //
    //    "return to select-service for no cache" in {
    //      val result = action(authorisedAsValidAgent(request, arn.value))
    //      status(result) shouldBe 303
    //      redirectLocation(result) shouldBe Some("/invitations/agents/select-service")
    //    }
    //  }
}