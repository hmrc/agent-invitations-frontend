package uk.gov.hmrc.agentinvitationsfrontend.controllers

import org.joda.time.LocalDate
import play.api.mvc.{Action, AnyContent, AnyContentAsEmpty}
import play.api.test.FakeRequest
import play.api.test.Helpers.redirectLocation
import uk.gov.hmrc.agentinvitationsfrontend.models.{Confirmation, CurrentInvitationInput, UserInputNinoAndPostcode, UserInputVrnAndRegDate}
import uk.gov.hmrc.agentinvitationsfrontend.support.{BaseISpec, TestDataCommonSupport}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationsVATControllerJourneyISpec extends BaseISpec with AuthBehaviours {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))


  "POST /agents/select-service" should {
    val request = FakeRequest("POST", "/agents/select-service")
    val submitService = controller.submitService()

    "return 303 for authorised Agent with valid VAT service, redirect to identify-client" in {
      testFastTrackCache.save(CurrentInvitationInput(business, serviceVAT))
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
    val showIdentifyClientForm = controller.showIdentifyClientForm()

    behave like anAuthorisedAgentEndpoint(request, showIdentifyClientForm)

    "return 200 for an Agent with HMRC-AS-AGENT enrolment for VAT service" in {
      testFastTrackCache.save(CurrentInvitationInput(business, serviceVAT))
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

      "redirect to /agents/invitation-sent when a valid VRN and registrationDate are submitted" in {
        createInvitationStub(
          arn,
          validVrn.value,
          invitationIdVAT,
          validVrn.value,
          "vrn",
          serviceVAT,
          identifierVAT)
        getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
        checkVatRegisteredClientStub(validVrn, LocalDate.parse("2007-07-07"), 204)

        testFastTrackCache.save(
          CurrentInvitationInput(
            business,
            "HMRC-MTD-VAT",
            "vrn",
            validVrn.value,
            Some(validRegistrationDate)
          ))
        val requestWithForm = request.withFormUrlEncodedBody(
          "clientType" -> "business",
          "service" -> "HMRC-MTD-VAT",
          "clientIdentifier" -> validVrn.value,
          "knownFact.year" -> "2007",
          "knownFact.month" -> "7",
          "knownFact.day" -> "7"
        )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.invitationSent().url)
      }

      "redisplay page with errors when an empty VRN is submitted" in {
        val requestWithForm = request.withFormUrlEncodedBody(
          "service" -> "HMRC-MTD-VAT",
          "clientIdentifier" -> "",
          "knownFact" -> validRegistrationDate)
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.vrn.required")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an invalid VRN is submitted" in {
        val requestWithForm = request.withFormUrlEncodedBody(
          "service" -> "HMRC-MTD-VAT",
          "clientIdentifier" -> "invalid",
          "knownFact" -> validRegistrationDate)
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "enter-vrn.regex-failure")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an empty registrationDate is submitted" in {
        val requestWithForm = request.withFormUrlEncodedBody(
          "service" -> "HMRC-MTD-VAT",
          "clientIdentifier" -> validVrn.value,
          "knownFact.year" -> "2008",
          "knownFact.month" -> "",
          "knownFact.day" -> "12"
        )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.vat-registration-date.required")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an invalid registrationDate is submitted" in {
        val requestWithForm = request.withFormUrlEncodedBody(
          "service" -> "HMRC-MTD-VAT",
          "clientIdentifier" -> validVrn.value,
          "knownFact.year" -> "2007",
          "knownFact.month" -> "17",
          "knownFact.day" -> "07"
        )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "enter-vat-registration-date.invalid-format")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when invalid registrationDate fields are submitted" in {
        val requestWithForm = request.withFormUrlEncodedBody(
          "service" -> "HMRC-MTD-VAT",
          "clientIdentifier" -> validVrn.value,
          "knownFact.year" -> "INVALID",
          "knownFact.month" -> "INVALID",
          "knownFact.day" -> "INVALID"
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
          "service" -> "",
          "clientIdentifier" -> validVrn.value,
          "knownFact" -> validRegistrationDate)
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
        CurrentInvitationInput(business, serviceVAT, "ni", validVrn.value, Some(validRegistrationDate))
      testFastTrackCache.save(invitation)
      testFastTrackCache.currentSession.currentInvitationInput.get shouldBe invitation

      val result = invitationSent(
        authorisedAsValidAgent(
          request.withSession("invitationId" -> s"${invitationIdVAT.value}", "deadline" -> "27 December 2017"),
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
        htmlEscapedMessage(s"$wireMockBaseUrlAsString${routes.ClientsInvitationController.start(invitationIdVAT)}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkInviteSentExitSurveyAgentSignOutLink(result)

      verifyAuthoriseAttempt()
      await(testFastTrackCache.fetch()).get shouldBe CurrentInvitationInput()
    }
  }

  "GET /agents/not-matched" should {
    val request = FakeRequest("GET", "/agents/not-matched")
    val notMatched = controller.notMatched()

    "return 403 for authorised Agent who submitted not matching known facts for VAT" in {
      val invitation = CurrentInvitationInput(business, serviceVAT, "vrn", validVrn.value, Some(validRegistrationDate))
      testFastTrackCache.save(invitation)

      val result = notMatched(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 403
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("not-matched.vat.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-matched.vat.description"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-matched.vat.button"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
      await(testFastTrackCache.fetch()).get shouldBe invitation
    }
  }

  "GET /agents/not-enrolled" should {
    val request = FakeRequest("GET", "/agents/not-enrolled")
    val notEnrolled = controller.notEnrolled()
    val featureFlags = FeatureFlags()

    "return 403 for authorised Agent who submitted known facts of an not enrolled VAT client" in {
      testFastTrackCache.save(CurrentInvitationInput(business, serviceVAT))
      val vrnForm =
        agentInvitationIdentifyClientFormVat(featureFlags).fill(UserInputVrnAndRegDate(business, serviceVAT, None, None))
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
//          Some(serviceVAT),
//          Some("vrn"),
//          Some(validVrn.value),
//          None,
//          Some(validRegistrationDate),
//          fromFastTrack))
//      givenClientDetails(validVrn)
//      val result = showConfirmClient(authorisedAsValidAgent(request, arn.value))
//      status(result) shouldBe 200
//      checkHtmlResultWithBodyText(result, "GDT")
//      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")
//      checkHtmlResultWithBodyMsgs(result, "confirm-client.yes")
//      checkHtmlResultWithBodyMsgs(result, "confirm-client.no")
//    }
//
//    "return 200 and no client name was found" in {
//      testFastTrackCache.save(
//        CurrentInvitationInput(
//          Some(serviceVAT),
//          Some("vrn"),
//          Some(validVrn.value),
//          None,
//          Some(validRegistrationDate),
//          fromFastTrack))
//      givenClientDetailsNotFound(validVrn)
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
//          Some(serviceVAT),
//          Some("vrn"),
//          Some(validVrn.value),
//          None,
//          Some(validRegistrationDate),
//          fromFastTrack))
//      createInvitationStubForNoKnownFacts(
//        arn,
//        validVrn.value,
//        invitationIdVAT,
//        validVrn.value,
//        "vrn",
//        serviceVAT,
//        identifierVAT)
//      givenClientDetails(validVrn)
//      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
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
//          Some(serviceVAT),
//          Some("vrn"),
//          Some(validVrn.value),
//          None,
//          Some(validRegistrationDate),
//          fromFastTrack))
//      givenClientDetails(validVrn)
//      val result = submitConfirmClient(authorisedAsValidAgent(request, arn.value))
//      status(result) shouldBe 200
//      checkHtmlResultWithBodyText(result, "GDT")
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
//      testFastTrackCache.save(CurrentInvitationInput(Some(serviceVAT), None, None, None, None, fromFastTrack))
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
