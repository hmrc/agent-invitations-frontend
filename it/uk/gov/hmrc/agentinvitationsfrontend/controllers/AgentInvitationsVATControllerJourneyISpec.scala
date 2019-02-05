package uk.gov.hmrc.agentinvitationsfrontend.controllers

import org.joda.time.LocalDate
import play.api.mvc.{Action, AnyContent, AnyContentAsEmpty}
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.forms.{ServiceTypeForm, VatClientForm}
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

    "return 303 for authorised Agent with valid VAT service, redirect to identify-client" in {
      testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(business, serviceVAT))
      val serviceForm = ServiceTypeForm.form.fill(serviceVAT)
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

    "return 200 for an Agent with HMRC-AS-AGENT enrolment for VAT service" in {
      testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(business, serviceVAT))
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
        val journeyState = AgentMultiAuthorisationJourneyState("business", Set.empty)
        testAgentMultiAuthorisationJourneyStateCache.save(journeyState)
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

        testCurrentAuthorisationRequestCache.save(
          CurrentAuthorisationRequest(
            business,
            "HMRC-MTD-VAT",
            "vrn",
            validVrn.value,
            Some(validRegistrationDate)
          ))
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
        val journeyState = AgentMultiAuthorisationJourneyState("business", Set.empty)
        testAgentMultiAuthorisationJourneyStateCache.save(journeyState)
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
        testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(business, serviceVAT))
        val requestWithForm = request.withFormUrlEncodedBody(
          "clientIdentifier" -> "",
          "registrationDate"        -> validRegistrationDate)
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.vrn.required")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an invalid VRN is submitted" in {
        testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(business, serviceVAT))
        val requestWithForm = request.withFormUrlEncodedBody(
          "clientIdentifier" -> "invalid",
          "registrationDate"        -> validRegistrationDate)
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "enter-vrn.regex-failure")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an empty registrationDate is submitted" in {
        testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(business, serviceVAT))
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
        testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(business, serviceVAT))
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
        testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(business, serviceVAT))
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
        testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(business))
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
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(Some("business"), serviceVAT, "vrn", validVrn.value, Some(validVrn.value)))

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

      verifyAuthoriseAttempt()
    }

    "throw a IllegalStateException when there is nothing in the cache" in {
      val result = invitationSent(authorisedAsValidAgent(request, arn.value))
      intercept[IllegalStateException] {
        await(result)
      }.getMessage shouldBe "Cached session state expected but not found"
    }
  }

  "GET /agents/not-signed-up" should {
    val request = FakeRequest("GET", "/agents/not-signed-up")
    val notEnrolled = controller.notSignedUp()
    val featureFlags = FeatureFlags()

    "return 403 for authorised Agent who submitted known facts of an not enrolled VAT client with no requests in basket" in {
      testAgentMultiAuthorisationJourneyStateCache.save(AgentMultiAuthorisationJourneyState("business", Set.empty))
      testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(business, serviceVAT))
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
      testAgentMultiAuthorisationJourneyStateCache.save(
        AgentMultiAuthorisationJourneyState("business", Set(authRequest)))
      testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(business, serviceVAT))
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
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(
          business,
          serviceVAT,
          "vrn",
          validVrn.value,
          Some(validRegistrationDate),
          fromFastTrack))
      givenClientDetails(validVrn)
      val result = showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "GDT")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.yes")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.no")
    }

    "return 200 and no client name was found" in {
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(
          business,
          serviceVAT,
          "vrn",
          validVrn.value,
          Some(validRegistrationDate),
          fromFastTrack))
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
      val journeyState = AgentMultiAuthorisationJourneyState("business", Set.empty)
      testAgentMultiAuthorisationJourneyStateCache.save(journeyState)
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(
          personal,
          serviceVAT,
          "vrn",
          validVrn.value,
          Some(validRegistrationDate),
          fromFastTrack))
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
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(
          business,
          serviceVAT,
          "vrn",
          validVrn.value,
          Some(validRegistrationDate),
          fromFastTrack))
      testAgentMultiAuthorisationJourneyStateCache.save(AgentMultiAuthorisationJourneyState(business.get, Set.empty))
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
      val journeyState = AgentMultiAuthorisationJourneyState("business", Set.empty)
      testAgentMultiAuthorisationJourneyStateCache.save(journeyState)
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(
          personal,
          serviceVAT,
          "vrn",
          validVrn.value,
          Some(validRegistrationDate),
          fromFastTrack))
      givenGetAllPendingInvitationsReturnsSome(arn, validVrn.value, serviceVAT)

      val choice = agentConfirmationForm("error-message").fill(Confirmation(true))
      val result =
        submitConfirmClient(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.pendingAuthorisationExists().url)
      status(result) shouldBe 303
    }

    "redirect to pending authorisations exist if there are already pending invitations in the basket for this client" in {
      val journeyState = AgentMultiAuthorisationJourneyState(
        "business",
        Set(AuthorisationRequest( "clientName", VatInvitation(personal, validVrn, Some(VatRegDate(validRegistrationDate))))))
      testAgentMultiAuthorisationJourneyStateCache.save(journeyState)
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(
          personal,
          serviceVAT,
          "vrn",
          validVrn.value,
          Some(validRegistrationDate),
          fromFastTrack))
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)

      val choice = agentConfirmationForm("error-message").fill(Confirmation(true))
      val result =
        submitConfirmClient(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.pendingAuthorisationExists().url)
      status(result) shouldBe 303
    }

    "redirect to already-authorisation-present when YES is selected but there is already an active relationship for this agent and client" in {
      val journeyState = AgentMultiAuthorisationJourneyState(
        "business",
        Set(AuthorisationRequest( "clientName", VatInvitation(business, validVrn9755, Some(VatRegDate(validRegistrationDate))))))
      testAgentMultiAuthorisationJourneyStateCache.save(journeyState)
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(
          business,
          serviceVAT,
          "vrn",
          validVrn.value,
          Some(validRegistrationDate),
          fromFastTrack))

      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 200)

      val choice = agentConfirmationForm("error message").fill(Confirmation(true))
      val result =
        submitConfirmClient(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
      redirectLocation(result).get shouldBe routes.AgentsErrorController.activeRelationshipExists().url
      status(result) shouldBe 303
    }

    "fail when creation of invitation is unsuccessful" in {
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(
          business,
          serviceVAT,
          "vrn",
          validVrn.value,
          Some(validRegistrationDate),
          fromFastTrack))
      testAgentMultiAuthorisationJourneyStateCache.save(AgentMultiAuthorisationJourneyState(business.get, Set.empty))
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
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(
          business,
          serviceVAT,
          "vrn",
          validVrn.value,
          Some(validRegistrationDate),
          fromFastTrack))
      givenClientDetails(validVrn)
      val result = submitConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "GDT")
      checkHtmlResultWithBodyMsgs(result, "error.confirm-client.required")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.yes")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.no")
    }

    behaveLikeMissingCacheScenarios(submitConfirmClient, request)
  }

  def behaveLikeMissingCacheScenarios(action: Action[AnyContent], request: FakeRequest[AnyContentAsEmpty.type]) = {
    "return to identify-client no client identifier found in cache" in {
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(business, serviceVAT, "", "", None, fromManual))
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
