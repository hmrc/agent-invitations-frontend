package uk.gov.hmrc.agentinvitationsfrontend.controllers

import org.joda.time.LocalDate
import play.api.mvc.{Action, AnyContent, AnyContentAsEmpty}
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.forms.VatClientForm
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.logging.SessionId
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier}

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationsVATControllerJourneyISpec extends BaseISpec with AuthBehaviours {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  "POST /agents/select-service" should {
    val request = FakeRequest("POST", "/agents/select-service")
    val submitService = controller.submitSelectService()

    "return 303 for authorised Agent with valid VAT service when YES is selected, redirect to identify-client" in {
      testAgentSessionCache.save(AgentSession(business, Some(serviceVAT)))
      val confirmForm = agentConfirmationForm("error").fill(Confirmation(true))
      val result =
        submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(confirmForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/identify-client")
      verifyAuthoriseAttempt()
    }

    "return 303 for an authorised Agent when NO is selected, redirect to select-client-type" in {
      testAgentSessionCache.save(AgentSession(business, Some(serviceVAT)))
      val confirmForm = agentConfirmationForm("error").fill(Confirmation(false))

      val result = submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(confirmForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/client-type")
      verifyAuthoriseAttempt()
    }

    "return 303 for authorised Agent with valid VAT service when there is no valid clientType in cache, redirect to client-type" in {
      testAgentSessionCache.save(AgentSession(Some("foo"), Some(serviceVAT)))
      val result =
        submitService(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/client-type")
    }
  }

  "GET /agents/identify-client" should {
    val request = FakeRequest("GET", "/agents/identify-client")
    val showIdentifyClientForm = controller.showIdentifyClient()

    behave like anAuthorisedAgentEndpoint(request, showIdentifyClientForm)

    "return 200 for an Agent with HMRC-AS-AGENT enrolment for VAT service" in {
      testAgentSessionCache.save(AgentSession(business, Some(serviceVAT)))
      val result = showIdentifyClientForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200

      checkHtmlResultWithBodyMsgs(result, "identify-client.header", "title.suffix.agents")

      checkHtmlResultWithBodyMsgs(
        result,
        "identify-client.header",
        "identify-client.vat.p1",
        "identify-client.vrn.label",
        "identify-client.vrn.hint",
        "identify-client.vat-registration-date.label",
        "identify-client.vat-registration-date.hint"
      )

      checkHasAgentSignOutLink(result)
    }

  }

  "POST /agents/identify-client" when {
    val request = FakeRequest("POST", "/agents/identify-client")
    val submitIdentifyClient = controller.submitIdentifyClient()

    behave like anAuthorisedAgentEndpoint(request, submitIdentifyClient)

    "service is HMRC-MTD-VAT" should {

      "redirect to confirm-client when a valid VRN and registrationDate are submitted" in {
        givenInvitationCreationSucceeds(
          arn,
          personal,
          validVrn.value,
          invitationIdVAT,
          validVrn.value,
          "vrn",
          serviceVAT,
          identifierVAT)
        givenVatRegisteredClientReturns(validVrn, LocalDate.parse("2007-07-07"), 204)
        givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)

        testAgentSessionCache.save(
          AgentSession(business, Some(serviceVAT), Some("vrn"), Some(validVrn.value), Some(validRegistrationDate)))
        val requestWithForm = request.withFormUrlEncodedBody(
          "clientIdentifier" -> validVrn.value,
          "registrationDate.year"   -> "2007",
          "registrationDate.month"  -> "7",
          "registrationDate.day"    -> "7"
        )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result).get shouldBe routes.AgentsInvitationController.showConfirmClient().url
      }

      "redirect to client-type when a valid VRN and registrationDate are submitted but cache is empty" in {
        givenInvitationCreationSucceeds(
          arn,
          personal,
          validVrn.value,
          invitationIdVAT,
          validVrn.value,
          "vrn",
          serviceVAT,
          identifierVAT)
        givenVatRegisteredClientReturns(validVrn, LocalDate.parse("2007-07-07"), 204)
        givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)

        val requestWithForm = request.withFormUrlEncodedBody(
          "clientIdentifier" -> validVrn.value,
          "registrationDate.year"   -> "2007",
          "registrationDate.month"  -> "7",
          "registrationDate.day"    -> "7"
        )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result).get shouldBe routes.AgentsInvitationController.showClientType().url
      }

      "redisplay page with errors when an empty VRN is submitted" in {
        testAgentSessionCache.save(AgentSession(business, Some(serviceVAT)))
        val requestWithForm = request.withFormUrlEncodedBody(
          "clientIdentifier" -> "",
          "registrationDate"        -> validRegistrationDate)
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.vrn.required")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an invalid VRN is submitted" in {
        testAgentSessionCache.save(AgentSession(business, Some(serviceVAT)))
        val requestWithForm = request.withFormUrlEncodedBody(
          "clientIdentifier" -> "invalid",
          "registrationDate"        -> validRegistrationDate)
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "enter-vrn.regex-failure")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an empty registrationDate is submitted" in {
        testAgentSessionCache.save(AgentSession(business, Some(serviceVAT)))
        val requestWithForm = request.withFormUrlEncodedBody(
          "clientIdentifier" -> validVrn.value,
          "registrationDate.year"   -> "2008",
          "registrationDate.month"  -> "",
          "registrationDate.day"    -> "12"
        )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.vat-registration-date.required")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an invalid registrationDate is submitted" in {
        testAgentSessionCache.save(AgentSession(business, Some(serviceVAT)))
        val requestWithForm = request.withFormUrlEncodedBody(
          "clientIdentifier" -> validVrn.value,
          "registrationDate.year"   -> "2007",
          "registrationDate.month"  -> "17",
          "registrationDate.day"    -> "07"
        )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "enter-vat-registration-date.invalid-format")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when invalid registrationDate fields are submitted" in {
        testAgentSessionCache.save(AgentSession(business, Some(serviceVAT)))
        val requestWithForm = request.withFormUrlEncodedBody(
          "clientIdentifier" -> validVrn.value,
          "registrationDate.year"   -> "INVALID",
          "registrationDate.month"  -> "INVALID",
          "registrationDate.day"    -> "INVALID"
        )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.day.invalid-format")
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.month.invalid-format")
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.year.invalid-format")
        checkHasAgentSignOutLink(result)
      }

      "redirect to /agents/select-service if service is missing" in {
        testAgentSessionCache.save(AgentSession(business))
        val requestWithForm = request.withFormUrlEncodedBody(
          "clientIdentifier" -> validVrn.value,
          "registrationDate"        -> validRegistrationDate)
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showSelectService().url)
      }
    }
  }

  "GET /agents/invitation-sent" should {
    val request = FakeRequest("GET", "/agents/invitation-sent")
    val invitationSent = controller.showInvitationSent()

    "return 200 for authorised Agent successfully created VAT invitation and redirected to Confirm Invitation Page (secureFlag = false) with no continue Url" in {
      givenAgentReference(arn, uid, "business")
      testAgentSessionCache.save(
        AgentSession(business, Some(serviceVAT), Some("vrn"), Some(validVrn.value), Some(validRegistrationDate)))

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
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l1.p.business"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l4.business"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.trackRequests.button"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueToASAccount.button"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.startNewAuthRequest"))
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          s"$wireMockBaseUrlAsString${routes.ClientsMultiInvitationController.warmUp("business", uid, "99-with-flake")}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkInviteSentExitSurveyAgentSignOutLink(result)

      //check if we have cleared everything in cache except clientType
      await(testAgentSessionCache.get) shouldBe AgentSession(business)

      verifyAuthoriseAttempt()
    }
  }

  "GET /agents/not-signed-up" should {
    val request = FakeRequest("GET", "/agents/not-signed-up")
    val notEnrolled = controller.notSignedUp()
    val featureFlags = FeatureFlags()

    "return 403 for authorised Agent who submitted known facts of an not enrolled VAT client with no requests in basket" in {
      testAgentSessionCache.save(AgentSession(business, Some(serviceVAT)))
      val form = VatClientForm.form(true).fill(VatClient(validVrn.value, None))
      val result =
        notEnrolled(authorisedAsValidAgent(request.withFormUrlEncodedBody(form.data.toSeq: _*), arn.value))

      status(result) shouldBe 403
      checkHtmlResultWithBodyText(
        result,
        "This client has not signed up to report their VAT returns through software.",
        "Start a new request")
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "return 403 for authorised Agent who submitted known facts of an not enrolled VAT client with requests in basket" in {
      val authRequest: AuthorisationRequest = AuthorisationRequest( "clientName", VatInvitation(business, validVrn, Some(VatRegDate(validRegistrationDate))))
      testAgentSessionCache.save(AgentSession(business, Some(serviceVAT), requests = Set(authRequest)))
      val form = VatClientForm.form(true).fill(VatClient(validVrn.value, None))
      val result =
        notEnrolled(authorisedAsValidAgent(request.withFormUrlEncodedBody(form.data.toSeq: _*), arn.value))

      status(result) shouldBe 403
      checkHtmlResultWithBodyText(
        result,
        "This client has not signed up to report their VAT returns through software.",
        "Return to your authorisation requests")
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }
  }

  "GET /confirm-client" should {
    val request = FakeRequest("GET", "/agents/confirm-client")
    val showConfirmClient = controller.showConfirmClient()

    "return 200 and show client name" in {
      testAgentSessionCache.save(
        AgentSession(business, Some(serviceVAT), Some("vrn"), Some(validVrn.value), Some(validRegistrationDate), fromFastTrack = fromFastTrack))
      givenClientDetails(validVrn)
      val result = showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "GDT")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.yes")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.no")
    }

    "return 200 and no client name was found" in {
      testAgentSessionCache.save(
        AgentSession(business, Some(serviceVAT), Some("vrn"), Some(validVrn.value), Some(validRegistrationDate), fromFastTrack = fromFastTrack))
      givenClientDetailsNotFound(validVrn)
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

    "redirect to review-authorisations if client type is personal" in {
      testAgentSessionCache.save(
        AgentSession(personal, Some(serviceVAT), Some("vrn"), Some(validVrn.value), Some(validRegistrationDate), fromFastTrack = fromFastTrack))
      givenInvitationCreationSucceeds(
        arn,
        personal,
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        identifierVAT)
      givenAgentReference(arn, "ABCDEFGH", "business")
      givenClientDetails(validVrn)
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)

      val choice = agentConfirmationForm("error-message").fill(Confirmation(true))
      val result =
        submitConfirmClient(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showReviewAuthorisations().url)
      status(result) shouldBe 303
    }

    "redirect to invitation-sent if client type is business" in {
      testAgentSessionCache.save(
        AgentSession(business, Some(serviceVAT), Some("vrn"), Some(validVrn.value), Some(validRegistrationDate), fromFastTrack = fromFastTrack))
      givenInvitationCreationSucceeds(
        arn,
        business,
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        identifierVAT)
      givenAgentReference(arn, "ABCDEFGH", "business")
      givenClientDetails(validVrn)
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenAgentReferenceRecordExistsForArn(arn, "uid")

      val choice = agentConfirmationForm("error-message").fill(Confirmation(true))
      val result =
        submitConfirmClient(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showInvitationSent().url)
      status(result) shouldBe 303
    }

    "redirect to pending authorisations exist if there are already pending invitations for this client" in {
      testAgentSessionCache.save(
        AgentSession(business, Some(serviceVAT), Some("vrn"), Some(validVrn.value), Some(validRegistrationDate), fromFastTrack = fromFastTrack))
      givenGetAllPendingInvitationsReturnsSome(arn, validVrn.value, serviceVAT)

      val choice = agentConfirmationForm("error-message").fill(Confirmation(true))
      val result =
        submitConfirmClient(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.pendingAuthorisationExists().url)
      status(result) shouldBe 303
    }

    "redirect to pending authorisations exist if there are already pending invitations in the basket for this client" in {
      val authRequest = AuthorisationRequest( "clientName", VatInvitation(personal, validVrn, Some(VatRegDate(validRegistrationDate))))
      testAgentSessionCache.save(
        AgentSession(business, Some(serviceVAT), Some("vrn"), Some(validVrn.value), Some(validRegistrationDate), requests = Set(authRequest), fromFastTrack = fromFastTrack))
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)

      val choice = agentConfirmationForm("error-message").fill(Confirmation(true))
      val result =
        submitConfirmClient(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.pendingAuthorisationExists().url)
      status(result) shouldBe 303
    }

    "redirect to already-authorisation-present when YES is selected but there is already an active relationship for this agent and client" in {
     val authRequest = AuthorisationRequest( "clientName", VatInvitation(business, validVrn9755, Some(VatRegDate(validRegistrationDate))))
      testAgentSessionCache.save(
        AgentSession(business, Some(serviceVAT), Some("vrn"), Some(validVrn.value), Some(validRegistrationDate), requests = Set(authRequest), fromFastTrack = fromFastTrack))
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 200)

      val choice = agentConfirmationForm("error message").fill(Confirmation(true))
      val result =
        submitConfirmClient(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
      redirectLocation(result).get shouldBe routes.AgentsErrorController.activeRelationshipExists().url
      status(result) shouldBe 303
    }

    "fail when creation of invitation is unsuccessful" in {
      testAgentSessionCache.save(
        AgentSession(business, Some(serviceVAT), Some("vrn"), Some(validVrn.value), Some(validRegistrationDate), fromFastTrack = fromFastTrack))
      givenInvitationCreationFails(arn)
      givenAgentReference(arn, "ABCDEFGH", "business")
      givenClientDetails(validVrn)
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)

      val choice = agentConfirmationForm("error-message").fill(Confirmation(true))
      val result =
        submitConfirmClient(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))

      a[BadRequestException] shouldBe thrownBy {
        await(result)
      }
    }

    "return 200 for not selecting an option" in {
      testAgentSessionCache.save(
        AgentSession(business, Some(serviceVAT), Some("vrn"), Some(validVrn.value), Some(validRegistrationDate), fromFastTrack = fromFastTrack))
      givenClientDetails(validVrn)
      val result = submitConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "GDT")
      checkHtmlResultWithBodyMsgs(result, "error.confirm-client.required")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.yes")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.no")
    }
  }

  def behaveLikeMissingCacheScenarios(action: Action[AnyContent], request: FakeRequest[AnyContentAsEmpty.type]) = {
    "return to identify-client no client identifier found in cache" in {
      testAgentSessionCache.save(
        AgentSession(business, Some(serviceVAT), Some(""), Some(""), fromFastTrack = fromManual))
      val result = action(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showIdentifyClient().url
    }

    "return to client-type for no cache" in {
      val result = action(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showClientType().url
    }
  }
}
