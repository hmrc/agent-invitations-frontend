package uk.gov.hmrc.agentinvitationsfrontend.controllers
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.{agentFastTrackForm, agentInvitationServiceForm}
import uk.gov.hmrc.agentinvitationsfrontend.models.{CurrentAuthorisationRequest, UserInputNinoAndPostcode}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class FastTrackITSAISpec extends BaseISpec {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  "POST /agents/client-type" should {
    val request = FakeRequest("POST", "/agents/client-type")
    val submitClientType = controller.submitClientType()
    "return 303 for authorised Agent with valid Nino and Known Fact, then selected Individual, redirect to invitation-sent" in {
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(None, serviceITSA, "ni", validNino.value, Some(validPostcode), fromFastTrack))
      givenInvitationCreationSucceeds(
        arn,
        personal,
        mtdItId.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        serviceITSA,
        "NI")
      givenAgentReference(arn, "AAAAAAAA", "personal")
      givenMatchingClientIdAndPostcode(validNino, validPostcode)

      val serviceForm = agentInvitationServiceForm.fill(UserInputNinoAndPostcode(personal, serviceITSA, None, None))
      val result =
        submitClientType(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      verifyAuthoriseAttempt()
    }

    "return 303 for authorised Agent with valid Nino and Known Fact, then selected Individual, redirect to select-service when cache is empty" in {
      givenInvitationCreationSucceeds(
        arn,
        personal,
        mtdItId.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        serviceITSA,
        "NI")
      givenAgentReference(arn, "AAAAAAAA", "personal")
      givenMatchingClientIdAndPostcode(validNino, validPostcode)

      val serviceForm = agentInvitationServiceForm.fill(UserInputNinoAndPostcode(personal, serviceITSA, None, None))
      val result =
        submitClientType(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/select-service")
      verifyAuthoriseAttempt()
    }
  }

  "POST /agents/select-service" should {
    val request = FakeRequest("POST", "/agents/select-service")
    val submitService = controller.submitSelectService()

    "return 303 for authorised Agent with valid Nino and Known Fact, then selected ITSA, redirect to invitation-sent" in {
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(personal, "", "ni", validNino.value, Some(validPostcode), fromFastTrack))
      givenInvitationCreationSucceeds(
        arn,
        personal,
        mtdItId.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        serviceITSA,
        "NI")
      givenAgentReference(arn, "AAAAAAAA", "personal")
      givenMatchingClientIdAndPostcode(validNino, validPostcode)

      val serviceForm = agentInvitationServiceForm.fill(UserInputNinoAndPostcode(personal, serviceITSA, None, None))
      val result =
        submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      verifyAuthoriseAttempt()
    }
  }

  "POST /agents/fast-track" should {
    val request = FakeRequest(
      "POST",
      "/agents/fast-track?continue=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fselect-client&error=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fnot-authorised"
    )
    val fastTrack = controller.agentFastTrack()

    "return 303 check-details if service calling fast-track is correct for ITSA" in {
      val formData =
        CurrentAuthorisationRequest(personal, serviceITSA, "ni", validNino.value, Some(validPostcode), fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showCheckDetails().url
    }

    "return 303 check-details if service calling fast-track for does not contain postcode for ITSA" in {

      val formData =
        CurrentAuthorisationRequest(personal, serviceITSA, "ni", validNino.value, None, fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showCheckDetails().url)
    }

    "return 303 if service calling fast-track contains invalid postcode for ITSA" in {
      val formData =
        CurrentAuthorisationRequest(
          personal,
          serviceITSA,
          "ni",
          validNino.value,
          Some("Invalid_Postcode"),
          fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showCheckDetails().url)
    }

    "return 303 check-details if service calling fast-track for does not contain client type" in {

      val formData =
        CurrentAuthorisationRequest(None, serviceITSA, "ni", validNino.value, Some(validPostcode), fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showCheckDetails().url)
    }

    "return 303 and redirect to error url if service calling fast-track for ITSA contains invalid nino" in {
      val formData =
        CurrentAuthorisationRequest(personal, serviceITSA, "ni", "INVALID_NINO", None, fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withHeaders()
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe
        Some("http://localhost:9996/tax-history/not-authorised?issue=INVALID_CLIENT_ID_RECEIVED:INVALID_NINO")
    }

    "return 303 and redirect to error url if service calling fast-track for ITSA does not contain nino" in {
      val formData = CurrentAuthorisationRequest(personal, serviceITSA).copy(fromFastTrack = fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe
        Some(
          "http://localhost:9996/tax-history/not-authorised?issue=UNSUPPORTED_CLIENT_ID_TYPE INVALID_CLIENT_ID_RECEIVED:NOTHING")
    }

    "return 303 and redirect to error url if there is no service but all other fields are valid for ITSA" in {
      val formData =
        CurrentAuthorisationRequest(personal, "", "ni", validNino.value, Some(validPostcode), fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe
        Some("http://localhost:9996/tax-history/not-authorised?issue=UNSUPPORTED_SERVICE")
    }

//    "return 303 and redirect to error url if there is no client-type but all other fields are valid for ITSA" in {
//      val formData =
//        CurrentInvitationInput(None, serviceITSA, "ni", validNino.value, Some(validPostcode), fromFastTrack)
//      val fastTrackFormData = agentFastTrackForm.fill(formData)
//      val result = fastTrack(
//        authorisedAsValidAgent(request, arn.value)
//          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))
//
//      status(result) shouldBe SEE_OTHER
//      redirectLocation(result) shouldBe
//        Some("http://localhost:9996/tax-history/not-authorised?issue=UNSUPPORTED_CLIENT_TYPE")
//    }
  }

  "GET /agents/check-details" should {

    val request = FakeRequest()

    "display the check details page when known fact is required and provided for ITSA for short postcode without spaces" in {
      val formData =
        CurrentAuthorisationRequest(personal, serviceITSA, "ni", validNino.value, Some(validPostcode), fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(controller.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("income and expenses through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Individual or sole trader"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("National Insurance number"))
      checkHtmlResultWithBodyText(result, "AB 12 34 56 A")
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Postcode"))
      checkHtmlResultWithBodyText(result, "DH1 4EJ")
    }

    "display the check details page when known fact is required and provided for ITSA for short postcode with spaces" in {
      val formData =
        CurrentAuthorisationRequest(
          personal,
          serviceITSA,
          "ni",
          validNino.value,
          Some(validPostcodeSpaces),
          fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(controller.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("income and expenses through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Individual or sole trader"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("National Insurance number"))
      checkHtmlResultWithBodyText(result, "AB 12 34 56 A")
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Postcode"))
      checkHtmlResultWithBodyText(result, "DH1 4EJ")
    }

    "display the check details page when known fact is required and provided for ITSA for long postcode" in {
      val formData =
        CurrentAuthorisationRequest(
          personal,
          serviceITSA,
          "ni",
          validNino.value,
          Some(validPostcodeLong),
          fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(controller.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("income and expenses through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Individual or sole trader"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("National Insurance number"))
      checkHtmlResultWithBodyText(result, "AB 12 34 56 A")
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Postcode"))
      checkHtmlResultWithBodyText(result, "BN11 4AW")
    }

    "display alternate check details page when known fact is required but not provided for ITSA" in {
      val formData =
        CurrentAuthorisationRequest(personal, serviceITSA, "ni", validNino.value, None, fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(controller.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("income and expenses through software"))
      checkHtmlResultWithBodyText(result, "Individual or sole trader")
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("National Insurance number"))
      checkHtmlResultWithBodyText(result, "AB 12 34 56 A")
      checkHtmlResultWithBodyText(result, "Change this information")
      checkHtmlResultWithBodyText(result, "We need some more details")
    }

    "display check details page when client type is not provided for ITSA" in {
      val formData =
        CurrentAuthorisationRequest(None, serviceITSA, "ni", validNino.value, Some(validPostcode), fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(controller.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("income and expenses through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("National Insurance number"))
      checkHtmlResultWithBodyText(result, "AB 12 34 56 A")
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Postcode"))
      checkHtmlResultWithBodyText(result, "DH1 4EJ")
      checkHtmlResultWithBodyText(result, "Change this information")
      checkHtmlResultWithBodyText(result, "We need some more details")
      checkHtmlResultWithNotBodyText(result, "Individual or sole trader")
    }
  }

  "POST /agents/check-details" should {
    val request = FakeRequest()
    "redirect to confirm_invitation when YES is selected for ITSA service" in {
      givenInvitationCreationSucceeds(
        arn,
        personal,
        mtdItId.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        serviceITSA,
        "MTDITID")
      givenAgentReference(arn, "AAAAAAAA", "personal")
      givenMatchingClientIdAndPostcode(validNino, validPostcode)

      val formData =
        CurrentAuthorisationRequest(personal, serviceITSA, "ni", validNino.value, Some(validPostcode), fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(
        controller.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "redirect to identify-client when NO is selected for ITSA service" in {
      givenInvitationCreationSucceeds(
        arn,
        personal,
        mtdItId.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        serviceITSA,
        "MTDITID")
      givenMatchingClientIdAndPostcode(validNino, validPostcode)

      val formData =
        CurrentAuthorisationRequest(personal, serviceITSA, "ni", validNino.value, Some(validPostcode), fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(
        controller.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "false")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/identify-client")
    }

    "return 303 not-matched if nino and postcode do not match for ITSA" in {
      val formData =
        CurrentAuthorisationRequest(personal, serviceITSA, "ni", validNino.value, Some(validPostcode), fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      testCurrentAuthorisationRequestCache.currentSession.item.get shouldBe formData
      givenNonMatchingClientIdAndPostcode(validNino, validPostcode)

      val form = agentFastTrackForm.fill(formData)
      val result = await(
        controller.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/not-matched")

      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value, validNino.value, "ni", "Fail", serviceITSA)
      await(testCurrentAuthorisationRequestCache.fetch).get shouldBe formData
    }

    "return 303 not-enrolled if Agent attempts to invite client who does not have an ITSA enrolment" in {
      val formData =
        CurrentAuthorisationRequest(personal, serviceITSA, "ni", validNino.value, Some(validPostcode), fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      testCurrentAuthorisationRequestCache.currentSession.item.get shouldBe formData
      givenNotEnrolledClientITSA(validNino, validPostcode)

      val form = agentFastTrackForm.fill(formData)
      val result = await(
        controller.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/not-enrolled")

      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value, validNino.value, "ni", "Fail", serviceITSA)
      await(testCurrentAuthorisationRequestCache.fetch).get shouldBe formData
    }
  }

  "GET agents/more-details" should {
    val request = FakeRequest()
    "display the known fact page when known fact is required and not provided for ITSA" in {
      val formData =
        CurrentAuthorisationRequest(personal, serviceITSA, "ni", validNino.value, None, fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(controller.showKnownFact(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, "What is your client's postcode?")
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("This will help us match their details against information we hold."))
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("This is the postcode of your client's registered address"))
    }
  }

  "POST /agents/more-details" should {
    val request = FakeRequest("POST", "/agents/identify-client")
    "redirect to invitation sent when client details are valid and match for ITSA" in {
      givenMatchingClientIdAndPostcode(validNino, validPostcode)
      givenInvitationCreationSucceeds(
        arn,
        personal,
        validNino.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        "HMRC-MTD-IT",
        "NI")
      givenAgentReference(arn, "AAAAAAAA", "personal")

      val requestWithForm = request.withFormUrlEncodedBody(
        "clientType"           -> "personal",
        "service"              -> "HMRC-MTD-IT",
        "clientIdentifierType" -> "ni",
        "clientIdentifier"     -> validNino.value,
        "knownFact"            -> "DH14EJ")
      val formData =
        CurrentAuthorisationRequest(personal, serviceITSA, "ni", validNino.value, None, fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(controller.submitKnownFact(authorisedAsValidAgent(requestWithForm, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "redisplay the page with errors when the known fact is not provided for ITSA" in {
      val formData =
        CurrentAuthorisationRequest(personal, serviceITSA, "ni", validNino.value, None, fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val requestWithForm = request.withFormUrlEncodedBody(
        "service"              -> "HMRC-MTD-IT",
        "clientIdentifierType" -> "ni",
        "clientIdentifier"     -> validNino.value,
        "knownFact"            -> "")
      val result = await(controller.submitKnownFact(authorisedAsValidAgent(requestWithForm, arn.value)))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Enter your client's postcode"))
    }
  }

}
