package uk.gov.hmrc.agentinvitationsfrontend.controllers

import org.joda.time.LocalDate
import play.api.mvc.{Action, AnyContent, AnyContentAsEmpty}
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier}
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationsVATControllerJourneyISpec extends BaseISpec with AuthBehaviours {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  "POST /agents/select-service" should {
    val request = FakeRequest("POST", "/agents/select-service")
    val submitService = controller.submitService()

    "return 303 for authorised Agent with valid VAT service, redirect to identify-client" in {
      testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(business, serviceVAT))
      val serviceForm = agentInvitationServiceForm.fill(UserInputNinoAndPostcode(business, serviceVAT, None, None))
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
        givenInvitationCreationSucceeds(
          arn,
          validVrn.value,
          invitationIdVAT,
          validVrn.value,
          "vrn",
          serviceVAT,
          identifierVAT)
        givenVatRegisteredClientReturns(validVrn, LocalDate.parse("2007-07-07"), 204)

        testCurrentAuthorisationRequestCache.save(
          CurrentAuthorisationRequest(
            business,
            "HMRC-MTD-VAT",
            "vrn",
            validVrn.value,
            Some(validRegistrationDate)
          ))
        val requestWithForm = request.withFormUrlEncodedBody(
          "clientType"       -> "business",
          "service"          -> "HMRC-MTD-VAT",
          "clientIdentifier" -> validVrn.value,
          "knownFact.year"   -> "2007",
          "knownFact.month"  -> "7",
          "knownFact.day"    -> "7"
        )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result).get shouldBe routes.AgentsInvitationController.showConfirmClient().url
      }

      "redirect to client-type when a valid VRN and registrationDate are submitted but cache is empty" in {
        givenInvitationCreationSucceeds(
          arn,
          validVrn.value,
          invitationIdVAT,
          validVrn.value,
          "vrn",
          serviceVAT,
          identifierVAT)
        givenVatRegisteredClientReturns(validVrn, LocalDate.parse("2007-07-07"), 204)

        val requestWithForm = request.withFormUrlEncodedBody(
          "clientType"       -> "business",
          "service"          -> "HMRC-MTD-VAT",
          "clientIdentifier" -> validVrn.value,
          "knownFact.year"   -> "2007",
          "knownFact.month"  -> "7",
          "knownFact.day"    -> "7"
        )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result).get shouldBe routes.AgentsInvitationController.showClientType().url
      }

      "redisplay page with errors when an empty VRN is submitted" in {
        val requestWithForm = request.withFormUrlEncodedBody(
          "service"          -> "HMRC-MTD-VAT",
          "clientIdentifier" -> "",
          "knownFact"        -> validRegistrationDate)
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.vrn.required")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an invalid VRN is submitted" in {
        val requestWithForm = request.withFormUrlEncodedBody(
          "service"          -> "HMRC-MTD-VAT",
          "clientIdentifier" -> "invalid",
          "knownFact"        -> validRegistrationDate)
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "enter-vrn.regex-failure")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an empty registrationDate is submitted" in {
        val requestWithForm = request.withFormUrlEncodedBody(
          "service"          -> "HMRC-MTD-VAT",
          "clientIdentifier" -> validVrn.value,
          "knownFact.year"   -> "2008",
          "knownFact.month"  -> "",
          "knownFact.day"    -> "12"
        )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.vat-registration-date.required")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an invalid registrationDate is submitted" in {
        val requestWithForm = request.withFormUrlEncodedBody(
          "service"          -> "HMRC-MTD-VAT",
          "clientIdentifier" -> validVrn.value,
          "knownFact.year"   -> "2007",
          "knownFact.month"  -> "17",
          "knownFact.day"    -> "07"
        )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "enter-vat-registration-date.invalid-format")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when invalid registrationDate fields are submitted" in {
        val requestWithForm = request.withFormUrlEncodedBody(
          "service"          -> "HMRC-MTD-VAT",
          "clientIdentifier" -> validVrn.value,
          "knownFact.year"   -> "INVALID",
          "knownFact.month"  -> "INVALID",
          "knownFact.day"    -> "INVALID"
        )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.day.invalid-format")
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.month.invalid-format")
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.year.invalid-format")
        checkHasAgentSignOutLink(result)
      }

      "redirect to /agents/select-service if service is missing" in {
        val requestWithForm = request.withFormUrlEncodedBody(
          "service"          -> "",
          "clientIdentifier" -> validVrn.value,
          "knownFact"        -> validRegistrationDate)
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.selectService().url)
      }
    }
  }

  "GET /agents/invitation-sent" should {
    val request = FakeRequest("GET", "/agents/invitation-sent")
    val invitationSent = controller.invitationSent()

    "return 200 for authorised Agent successfully created VAT invitation and redirected to Confirm Invitation Page (secureFlag = false) with no continue Url" in {
      val invitation =
        CurrentAuthorisationRequest(business, serviceVAT, "ni", validVrn.value, Some(validRegistrationDate))
      testCurrentAuthorisationRequestCache.save(invitation)
      testCurrentAuthorisationRequestCache.currentSession.item.get shouldBe invitation

      val result = invitationSent(
        authorisedAsValidAgent(
          request.withSession(
            "invitationLink" -> s"/invitations/business/ABCDEFGH/my-agency-name",
            "clientType"     -> "business"),
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
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l1.p.business"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.p2.business"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.trackRequests.button"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueToASAccount.button"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.startNewAuthRequest"))
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          s"$wireMockBaseUrlAsString${routes.ClientsMultiInvitationController.warmUp("business", "ABCDEFGH", "my-agency-name")}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkInviteSentExitSurveyAgentSignOutLink(result)

      verifyAuthoriseAttempt()
      await(testCurrentAuthorisationRequestCache.fetch).get shouldBe CurrentAuthorisationRequest()
    }
  }

  "GET /agents/not-enrolled" should {
    val request = FakeRequest("GET", "/agents/not-enrolled")
    val notEnrolled = controller.notEnrolled()
    val featureFlags = FeatureFlags()

    "return 403 for authorised Agent who submitted known facts of an not enrolled VAT client" in {
      testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(business, serviceVAT))
      val vrnForm =
        agentInvitationIdentifyClientFormVat(featureFlags).fill(
          UserInputVrnAndRegDate(business, serviceVAT, None, None))
      val result =
        notEnrolled(authorisedAsValidAgent(request.withFormUrlEncodedBody(vrnForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 403
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("not-enrolled.vat.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-enrolled.vat.description"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-enrolled.vat.button"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
      await(testCurrentAuthorisationRequestCache.fetch) shouldBe None
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
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(
          personal,
          serviceVAT,
          "vrn",
          validVrn.value,
          Some(validRegistrationDate),
          fromFastTrack))
      testAgentMultiAuthorisationJourneyStateCache.save(AgentMultiAuthorisationJourneyState(personal.get, Set.empty))
      givenInvitationCreationSucceeds(
        arn,
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
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.invitationSent().url)
      status(result) shouldBe 303
    }

    "redirect to already-authorisation-pending when the agent has already created an invitation for this client and service" in {
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(
          business,
          serviceVAT,
          "vrn",
          validVrn.value,
          Some(validRegistrationDate),
          fromFastTrack))
      givenInvitationExists(arn, validVrn.value, invitationIdVAT, serviceVAT, "vat", "Pending")
      givenAgentReference(arn, "ABCDEFGH", "business")
      givenClientDetails(validVrn)
      givenGetAllPendingInvitationsReturnsSome(arn, validVrn.value, serviceVAT)

      val choice = agentConfirmationForm("error message").fill(Confirmation(true))
      val result =
        submitConfirmClient(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.pendingAuthorisationExists().url
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
