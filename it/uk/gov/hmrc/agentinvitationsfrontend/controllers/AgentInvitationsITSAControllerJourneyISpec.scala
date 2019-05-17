package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.util.UUID

import play.api.mvc.{Action, AnyContent, AnyContentAsEmpty}
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.retired.AgentsInvitationController
import uk.gov.hmrc.agentinvitationsfrontend.controllers.retired.AgentsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.forms.{ItsaClientForm, ServiceTypeForm}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.personal
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

class AgentInvitationsITSAControllerJourneyISpec extends BaseISpec with AuthBehaviours {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId(UUID.randomUUID().toString)))

  "POST /agents/select-personal-service" should {
    val request = FakeRequest("POST", "/agents/select-personal-service")
    val submitService = controller.submitSelectPersonalService()

    "return 303 for authorised Agent with valid ITSA service, redirect to enter identify-client page" in {
      await(sessionStore.save(AgentSession(Some(personal), Some(serviceITSA))))
      val serviceForm = ServiceTypeForm.form.fill(serviceITSA)
      val result =
        submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations2/agents/identify-client")
      verifyAuthoriseAttempt()
    }
  }

  "GET /agents/identify-client" should {
    val request = FakeRequest("GET", "/agents/identify-client")
    val showIdentifyClientForm = controller.showIdentifyClient()

    behave like anAuthorisedAgentEndpoint(request, showIdentifyClientForm)

    "return 200 for an Agent with HMRC-AS-AGENT enrolment for ITSA service" in {
      await(sessionStore.save(AgentSession(Some(personal), Some(serviceITSA))))
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
    val request = FakeRequest("POST", "/agents/identify-itsa-client")
    val submitIdentifyClient = controller.submitIdentifyClientItsa()

    behave like anAuthorisedAgentEndpoint(request, submitIdentifyClient)

    "service is HMRC-MTD-IT" should {

      "redirect to confirm-client when a valid NINO and postcode are submitted" in {
        val authRequest = AuthorisationRequest("clientName", ItsaInvitation(validNino, Some(Postcode(validPostcode))))
        await(
          sessionStore.save(
            AgentSession(
              Some(personal),
              Some(serviceITSA),
              Some("ni"),
              Some(validNino.value),
              Some(validPostcode),
              requests = Set(authRequest))))
        givenInvitationCreationSucceeds(
          arn,
          Some(personal),
          validNino.value,
          invitationIdITSA,
          validNino.value,
          "ni",
          "HMRC-MTD-IT",
          "NI")
        givenMatchingClientIdAndPostcode(validNino, validPostcode)
        givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, serviceITSA)
        givenAgentReferenceRecordExistsForArn(arn, "uid")

        val requestWithForm =
          request.withFormUrlEncodedBody("clientIdentifier" -> validNino.value, "postcode" -> validPostcode)
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result).get shouldBe retired.routes.AgentsInvitationController.showConfirmClient().url
      }

      "redirect to client-type when a valid NINO and postcode are submitted but cache is empty" in {
        await(sessionStore.save(AgentSession()))
        givenInvitationCreationSucceeds(
          arn,
          Some(personal),
          validNino.value,
          invitationIdITSA,
          validNino.value,
          "ni",
          "HMRC-MTD-IT",
          "NI")
        givenMatchingClientIdAndPostcode(validNino, validPostcode)
        givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, serviceITSA)

        val requestWithForm =
          request.withFormUrlEncodedBody("clientIdentifier" -> validNino.value, "postcode" -> validPostcode)
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result).get shouldBe retired.routes.AgentsInvitationController.showClientType().url
      }

      "redisplay page with errors when an empty NINO is submitted" in {
        await(sessionStore.save(AgentSession(Some(personal), Some(serviceITSA))))
        val requestWithForm = request
          .withFormUrlEncodedBody("clientIdentifier" -> "", "postcode" -> validPostcode)
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.nino.required")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an invalid NINO is submitted" in {
        await(sessionStore.save(AgentSession(Some(personal), Some(serviceITSA))))
        val requestWithForm =
          request.withFormUrlEncodedBody("clientIdentifier" -> "invalid", "postcode" -> validPostcode)
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "enter-nino.invalid-format")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an empty postcode is submitted" in {
        await(sessionStore.save(AgentSession(Some(personal), Some(serviceITSA))))
        val requestWithForm = request
          .withFormUrlEncodedBody("clientIdentifier" -> validNino.value, "postcode" -> "")
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.postcode.required")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when a postcode with invalid format is submitted" in {
        await(sessionStore.save(AgentSession(Some(personal), Some(serviceITSA))))
        val requestWithForm =
          request.withFormUrlEncodedBody("clientIdentifier" -> validNino.value, "postcode" -> "invalid")
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "enter-postcode.invalid-format")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when a postcode with invalid characters is submitted" in {
        await(sessionStore.save(AgentSession(Some(personal), Some(serviceITSA))))
        val requestWithForm =
          request.withFormUrlEncodedBody("clientIdentifier" -> validNino.value, "postcode" -> "invalid%")
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "enter-postcode.invalid-characters")
        checkHasAgentSignOutLink(result)
      }
    }
  }

  "GET /agents/invitation-sent" should {
    val request = FakeRequest("GET", "/agents/invitation-sent")
    val invitationSent = controller.showInvitationSent()

    "return 200 for authorised Agent successfully created ITSA invitation and redirected to Confirm Invitation Page (secureFlag = false) with no continue Url" in {
      givenAgentReference(arn, uid, personal)
      await(
        sessionStore.save(
          AgentSession(
            Some(personal),
            Some(serviceITSA),
            Some("ni"),
            Some(validNino.value),
            Some(validPostcode),
            clientTypeForInvitationSent = Some(personal))))

      val result = invitationSent(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("invitation-sent.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.header"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l2", "someurl"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l4.personal"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.trackRequests"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueToASAccount.button"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.startNewAuthRequest"))
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          s"$wireMockBaseUrlAsString${routes.ClientInvitationJourneyController.warmUp("personal", uid, "99-with-flake")}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkInviteSentExitSurveyAgentSignOutLink(result)

      //check if we have cleared everything in cache except clientTypeForInvitationsSent
      await(sessionStore.hardGet) shouldBe AgentSession(clientTypeForInvitationSent = Some(personal))

      verifyAuthoriseAttempt()
    }
  }

  "GET /agents/not-signed-up" should {
    val request = FakeRequest("GET", "/agents/not-signed-up")
    val notEnrolled = controller.notSignedUp()
    val featureFlags = FeatureFlags()

    "return 403 for authorised Agent who submitted known facts of an not enrolled ITSA client when there are no requests in basket" in {
      await(sessionStore.save(AgentSession(Some(personal), Some(serviceITSA))))
      val ninoForm = ItsaClientForm.form(featureFlags.showKfcMtdIt).fill(ItsaClient("", None))
      val result =
        notEnrolled(authorisedAsValidAgent(request.withFormUrlEncodedBody(ninoForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 403
      checkHtmlResultWithBodyText(
        result,
        "This client has not signed up to report their income and expenses through software.",
        "Start a new request")
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "return 403 for authorised Agent who submitted known facts of an not enrolled ITSA client when there are requests in basket" in {
      val authRequest = AuthorisationRequest("clientName", ItsaInvitation(validNino, Some(Postcode(validPostcode))))
      await(sessionStore.save(AgentSession(Some(personal), Some(serviceITSA), requests = Set(authRequest))))
      val ninoForm = ItsaClientForm.form(featureFlags.showKfcMtdIt).fill(ItsaClient("", None))
      val result =
        notEnrolled(authorisedAsValidAgent(request.withFormUrlEncodedBody(ninoForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 403
      checkHtmlResultWithBodyText(
        result,
        "This client has not signed up to report their income and expenses through software.",
        "Return to your authorisation requests")
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }
  }

  "GET /confirm-client" should {
    val request = FakeRequest("GET", "/agents/confirm-client")
    val showConfirmClient = controller.showConfirmClient()

    "return 200 and show client trading name" in {
      await(
        sessionStore.save(
          AgentSession(
            None,
            Some(serviceITSA),
            Some("ni"),
            Some(validNino.value),
            Some(validPostcode),
            fromFastTrack = fromManual)))
      givenTradingName(validNino, "64 Bit")

      val result = showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "64 Bit")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.yes")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.no")
    }

    "return 200 and show client's name" in {
      await(
        sessionStore.save(
          AgentSession(
            None,
            Some(serviceITSA),
            Some("ni"),
            Some(validNino.value),
            Some(validPostcode),
            fromFastTrack = fromManual)))
      givenTradingNameMissing(validNino)
      givenCitizenDetailsAreKnownFor(validNino.value, "Anne Marri", "Son Pear")

      val result = showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Anne Marri Son Pear")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.yes")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.no")
    }

    "return 200 and no client name was found" in {
      await(
        sessionStore.save(
          AgentSession(
            None,
            Some(serviceITSA),
            Some("ni"),
            Some(validNino.value),
            Some(validPostcode),
            fromFastTrack = fromManual)))
      givenTradingNameMissing(validNino)
      givenCitizenDetailsReturns404For(validNino.value)

      val result = showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.yes")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.no")
    }

    behaveLikeMissingCacheScenarios(showConfirmClient, request)
  }

  "POST /confirm-client" should {
    val request = FakeRequest("POST", "/agents/confirm-client")
    val submitConfirmClient = controller.submitConfirmClient()

    "redirect to show-review-authorisations when YES is selected" in {
      await(
        sessionStore.save(
          AgentSession(
            Some(personal),
            Some(serviceITSA),
            Some("ni"),
            Some(validNino.value),
            Some(validPostcode),
            fromFastTrack = fromManual)))
      givenInvitationCreationSucceeds(
        arn,
        Some(personal),
        mtdItId.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        serviceITSA,
        "NI")
      givenTradingName(validNino, "64 Bit")
      givenAgentReference(arn, "ABCDEFGH", personal)
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, serviceITSA)
      givenCheckRelationshipItsaWithStatus(arn, validNino.value, 404)

      val choice = agentConfirmationForm("error message").fill(Confirmation(true))
      val result =
        submitConfirmClient(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
      redirectLocation(result).get shouldBe retired.routes.AgentsInvitationController.showReviewAuthorisations().url
      status(result) shouldBe 303
    }

    "redirect to show identify client when NO is selected" in {
      await(
        sessionStore.save(
          AgentSession(
            None,
            Some(serviceITSA),
            Some("ni"),
            Some(validNino.value),
            Some(validPostcode),
            fromFastTrack = fromManual)))
      givenInvitationCreationSucceeds(
        arn,
        Some(personal),
        mtdItId.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        serviceITSA,
        "NI")
      givenTradingName(validNino, "64 Bit")
      givenAgentReference(arn, "ABCDEFGH", personal)
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, serviceITSA)

      val choice = agentConfirmationForm("error message").fill(Confirmation(false))
      val result =
        submitConfirmClient(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
      redirectLocation(result).get shouldBe retired.routes.AgentsInvitationController.showIdentifyClient().url
      status(result) shouldBe 303
    }

    "redirect to already-invitations-pending when YES is selected but there are already invitations for this client" in {
      val authRequest = AuthorisationRequest("clientName", ItsaInvitation(validNino, Some(Postcode(validPostcode))))
      await(
        sessionStore.save(
          AgentSession(
            None,
            Some(serviceITSA),
            Some("ni"),
            Some(validNino.value),
            Some(validPostcode),
            fromFastTrack = fromManual,
            requests = Set(authRequest))))
      givenGetAllPendingInvitationsReturnsSome(arn, validNino.value, serviceITSA)

      val choice = agentConfirmationForm("error message").fill(Confirmation(true))
      val result =
        submitConfirmClient(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
      redirectLocation(result).get shouldBe retired.routes.AgentsInvitationController.pendingAuthorisationExists().url
      status(result) shouldBe 303
    }

    "redirect to already-invitations-pending when YES is selected but there are already invitations in the basket for this client" in {
      val authRequest = AuthorisationRequest("clientName", ItsaInvitation(validNino, Some(Postcode(validPostcode))))
      await(
        sessionStore.save(
          AgentSession(
            None,
            Some(serviceITSA),
            Some("ni"),
            Some(validNino.value),
            Some(validPostcode),
            fromFastTrack = fromManual,
            requests = Set(authRequest))))
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, serviceITSA)

      val choice = agentConfirmationForm("error message").fill(Confirmation(true))
      val result =
        submitConfirmClient(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
      redirectLocation(result).get shouldBe retired.routes.AgentsInvitationController.pendingAuthorisationExists().url
      status(result) shouldBe 303
    }

    "redirect to already-authorisation-present when YES is selected but there is already an active relationship for this agent and client" in {

      await(
        sessionStore.save(
          AgentSession(
            None,
            Some(serviceITSA),
            Some("ni"),
            Some(validNino.value),
            Some(validPostcode),
            fromFastTrack = fromManual)))

      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, serviceITSA)
      givenCheckRelationshipItsaWithStatus(arn, validNino.value, 200)
      val choice = agentConfirmationForm("error message").fill(Confirmation(true))
      val result =
        submitConfirmClient(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
      redirectLocation(result).get shouldBe retired.routes.AgentsErrorController.activeRelationshipExists().url
      status(result) shouldBe 303
    }

    "redirect to select client type when client type in cache is not supported" in {
      await(
        sessionStore.save(
          AgentSession(
            None,
            Some(serviceITSA),
            Some("ni"),
            Some(validNino.value),
            Some(validPostcode),
            fromFastTrack = fromManual)))
      givenInvitationCreationSucceeds(
        arn,
        Some(personal),
        mtdItId.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        serviceITSA,
        "NI")
      givenTradingName(validNino, "64 Bit")
      givenAgentReference(arn, "ABCDEFGH", personal)
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, serviceITSA)

      val choice = agentConfirmationForm("error message").fill(Confirmation(true))
      val result =
        submitConfirmClient(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
      redirectLocation(result).get shouldBe retired.routes.AgentsInvitationController.showClientType().url
      status(result) shouldBe 303
    }

    "return 200 for not selecting an option" in {
      await(
        sessionStore.save(
          AgentSession(
            None,
            Some(serviceITSA),
            Some("ni"),
            Some(validNino.value),
            Some(validPostcode),
            fromFastTrack = fromManual)))
      givenTradingName(validNino, "64 Bit")
      val result = submitConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      givenTradingName(validNino, "64 Bit")
      checkHtmlResultWithBodyMsgs(result, "error.confirm-client.required")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.yes")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.no")
    }
  }

  def behaveLikeMissingCacheScenarios(action: Action[AnyContent], request: FakeRequest[AnyContentAsEmpty.type]) = {
    "return to identify-client no client identifier found in cache" in {
      await(
        sessionStore.save(
          AgentSession(Some(personal), Some(serviceITSA), Some("ni"), None, None, fromFastTrack = fromManual)))
      val result = action(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe retired.routes.AgentsInvitationController.showIdentifyClient().url
    }

    "return to client-type for no cache" in {
      await(sessionStore.delete())
      val result = action(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe retired.routes.AgentsInvitationController.showClientType().url
    }
  }
}
