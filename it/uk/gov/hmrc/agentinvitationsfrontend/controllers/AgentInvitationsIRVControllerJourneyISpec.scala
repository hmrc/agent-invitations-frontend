package uk.gov.hmrc.agentinvitationsfrontend.controllers

import org.joda.time.LocalDate
import play.api.mvc.{Action, AnyContent, AnyContentAsEmpty}
import play.api.test.FakeRequest
import play.api.test.Helpers.redirectLocation
import uk.gov.hmrc.agentinvitationsfrontend.models.{Confirmation, CurrentInvitationInput, UserInputNinoAndPostcode}
import uk.gov.hmrc.agentinvitationsfrontend.support.{BaseISpec, TestDataCommonSupport}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationsIRVControllerJourneyISpec extends BaseISpec with AuthBehaviours with TestDataCommonSupport {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  "POST /agents/select-service" should {
    val request = FakeRequest("POST", "/agents/select-service")
    val submitService = controller.submitService()

    "return 303 for authorised Agent with valid Personal Income Record service, redirect to identify client" in {
      testFastTrackCache.save(CurrentInvitationInput(servicePIR))
      val serviceForm = agentInvitationServiceForm.fill(UserInputNinoAndPostcode(servicePIR, None, None))
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

    "return 200 for an Agent with HMRC-AS-AGENT enrolment for IRV service" in {
      testFastTrackCache.save(CurrentInvitationInput(servicePIR))
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

      "redirect to /agents/invitation-sent when a valid NINO is submitted" in {
        createInvitationStub(
          arn,
          validNino.value,
          invitationIdPIR,
          validNino.value,
          "ni",
          servicePIR,
          "NI")
        givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
        getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, "NI", "Pending")

        testFastTrackCache.save(CurrentInvitationInput(servicePIR, "ni", validNino.value, Some(dateOfBirth)))
        val requestWithForm =
          request.withFormUrlEncodedBody(
            "service" -> servicePIR,
            "clientIdentifier" -> validNino.value,
            "knownFact.year" -> "1980",
            "knownFact.month" -> "07",
            "knownFact.day" -> "07")
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.invitationSent().url)
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
          "service" -> servicePIR,
          "clientIdentifier" -> validNino.value,
          "knownFact.year" -> "",
          "knownFact.month" -> "",
          "knownFact.day" -> ""
        )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.irv-date-of-birth.required")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an invalid date of birth is submitted" in {
        val requestWithForm = request.withFormUrlEncodedBody(
          "service" -> servicePIR,
          "clientIdentifier" -> validNino.value,
          "knownFact.year" -> "9999",
          "knownFact.month" -> "99",
          "knownFact.day" -> "99"
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
        redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.selectService().url)
      }
    }
  }

  "GET /agents/invitation-sent" should {
    val request = FakeRequest("GET", "/agents/invitation-sent")
    val invitationSent = controller.invitationSent()

    "return 200 for authorised Agent successfully created IRV invitation and redirected to Confirm Invitation Page (secureFlag = false) with no continue Url" in {
      val invitation =
        CurrentInvitationInput(servicePIR, "ni", validNino.value, None)
      testFastTrackCache.save(invitation)
      testFastTrackCache.currentSession.currentInvitationInput.get shouldBe invitation

      val result = invitationSent(
        authorisedAsValidAgent(
          request.withSession("invitationId" -> s"${invitationIdPIR.value}", "deadline" -> "27 December 2017"),
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
        htmlEscapedMessage(s"$wireMockBaseUrlAsString${routes.ClientsInvitationController.start(invitationIdPIR)}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkInviteSentExitSurveyAgentSignOutLink(result)

      verifyAuthoriseAttempt()
      await(testFastTrackCache.fetch()).get shouldBe CurrentInvitationInput()
    }
  }

  "GET /agents/not-matched" should {
    val request = FakeRequest("GET", "/agents/not-matched")
    val notMatched = controller.notMatched()

    "return 403 for authorised Agent who enter nino for IRV but no record found" in {
      val invitation = CurrentInvitationInput(servicePIR)
      testFastTrackCache.save(invitation)

      val result = notMatched(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 403
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("not-matched.afi.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-matched.afi.description"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-matched.afi.button"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
      await(testFastTrackCache.fetch()).get shouldBe CurrentInvitationInput(servicePIR)
    }

    behave like anAuthorisedAgentEndpoint(request, notMatched)
  }

  //  "GET /confirm-client" should {
//    val request = FakeRequest("GET", "/agents/confirm-client")
//    val showConfirmClient = controller.showConfirmClient()
//
//    "return 200 and show client name for PERSONAL-INCOME-RECORD" in {
//      testFastTrackCache.save(
//        CurrentInvitationInput(Some(servicePIR), Some("ni"), Some(validNino.value), None, None, fromFastTrack))
//      givenCitizenDetailsAreKnownFor(validNino.value, "64", "Bit")
//      val result = showConfirmClient(authorisedAsValidAgent(request, arn.value))
//      status(result) shouldBe 200
//      checkHtmlResultWithBodyText(result, "64 Bit")
//      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")
//      checkHtmlResultWithBodyMsgs(result, "confirm-client.yes")
//      checkHtmlResultWithBodyMsgs(result, "confirm-client.no")
//    }
//
//    "return 200 and no client name was found for PERSONAL-INCOME-RECORD" in {
//      testFastTrackCache.save(
//        CurrentInvitationInput(Some(servicePIR), Some("ni"), Some(validNino.value), None, None, fromFastTrack))
//      givenCitizenDetailsReturns404For(validNino.value)
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
//    "redirect to invitation-sent and create invitation for PERSONAL-INCOME-RECORD" in {
//      testFastTrackCache.save(
//        CurrentInvitationInput(Some(servicePIR), Some("ni"), Some(validNino.value), None, None, fromFastTrack))
//      createInvitationStubForNoKnownFacts(
//        arn,
//        validNino.value,
//        invitationIdPIR,
//        validNino.value,
//        "ni",
//        servicePIR,
//        "NI")
//      givenCitizenDetailsAreKnownFor(validNino.value, "64", "Bit")
//      getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, "NI", "Pending")
//      val choice = agentConfirmClientForm.fill(Confirmation(true))
//      val result =
//        submitConfirmClient(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
//      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
//      status(result) shouldBe 303
//    }
//
//    "return 200 for not selecting an option for PERSONAL-INCOME-RECORD" in {
//      testFastTrackCache.save(
//        CurrentInvitationInput(Some(servicePIR), Some("ni"), Some(validNino.value), None, None, fromFastTrack))
//      givenCitizenDetailsAreKnownFor(validNino.value, "64", "Bit")
//      val result = submitConfirmClient(authorisedAsValidAgent(request, arn.value))
//      status(result) shouldBe 200
//      checkHtmlResultWithBodyText(result, "64 Bit")
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
//      testFastTrackCache.save(CurrentInvitationInput(Some(servicePIR), None, None, None, None, fromFastTrack))
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
