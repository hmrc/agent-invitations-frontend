package uk.gov.hmrc.agentinvitationsfrontend.controllers
import java.util.UUID

import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsFastTrackInvitationController.agentFastTrackForm
import uk.gov.hmrc.agentinvitationsfrontend.forms.ClientTypeForm
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.personal
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentFastTrackRequest, AgentSession}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

class FastTrackITSAISpec extends BaseISpec {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  lazy val fastTrackController: AgentsFastTrackInvitationController =
    app.injector.instanceOf[AgentsFastTrackInvitationController]
  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId(UUID.randomUUID().toString)))

  "POST /agents/client-type" should {
    val request = FakeRequest("POST", "/agents/client-type")
    val submitClientType = controller.submitClientType()
    "return 303 for authorised Agent with valid Nino and Known Fact, then selected Individual, redirect to invitation-sent" in {
      await(
        sessionStore.save(
          AgentSession(
            None,
            Some(serviceITSA),
            Some("ni"),
            Some(validNino.value),
            Some(validPostcode),
            fromFastTrack = fromFastTrack)))
      givenInvitationCreationSucceeds(
        arn,
        Some(personal),
        mtdItId.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        serviceITSA,
        "NI")
      givenAgentReference(arn, "AAAAAAAA", personal)
      givenMatchingClientIdAndPostcode(validNino, validPostcode)
      givenAgentReferenceRecordExistsForArn(arn, "uid")
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, serviceITSA)

      val clientTypeForm = ClientTypeForm.form.fill(personal)
      val result =
        submitClientType(
          authorisedAsValidAgent(request.withFormUrlEncodedBody(clientTypeForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

//    "return 303 for authorised Agent with valid Nino and Known Fact, then selected Individual, redirect to select-service when cache is empty" in {
//      givenInvitationCreationSucceeds(
//        arn,
//        personal,
//        mtdItId.value,
//        invitationIdITSA,
//        validNino.value,
//        "ni",
//        serviceITSA,
//        "NI")
//      givenAgentReference(arn, "AAAAAAAA", "personal")
//      givenMatchingClientIdAndPostcode(validNino, validPostcode)
//
//      val clientTypeForm = ClientTypeForm.form.fill("personal")
//      val result =
//        submitClientType(authorisedAsValidAgent(request.withFormUrlEncodedBody(clientTypeForm.data.toSeq: _*), arn.value))
//
//      status(result) shouldBe 303
//      redirectLocation(result) shouldBe Some("/invitations/agents/select-service")
//      verifyAuthoriseAttempt()
//    }
  }

  "POST /agents/fast-track" should {
    val request = FakeRequest(
      "POST",
      "/agents/fast-track?continue=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fselect-client&error=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fnot-authorised"
    )
    val fastTrack = fastTrackController.agentFastTrack()

    "return 303 check-details if service calling fast-track is correct for ITSA" in {
      val formData =
        AgentFastTrackRequest(Some(personal), serviceITSA, "ni", validNino.value, Some(validPostcode))
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsFastTrackInvitationController.showCheckDetails().url
    }

    "return 303 check-details if service calling fast-track for does not contain postcode for ITSA" in {

      val formData =
        AgentFastTrackRequest(Some(personal), serviceITSA, "ni", validNino.value, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsFastTrackInvitationController.showCheckDetails().url)
    }

    "return 303 if service calling fast-track contains invalid postcode for ITSA" in {
      val formData =
        AgentFastTrackRequest(Some(personal), serviceITSA, "ni", validNino.value, Some("invali_postcode"))
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsFastTrackInvitationController.showCheckDetails().url)
    }

    "return 303 check-details if service calling fast-track for does not contain client type" in {

      val formData =
        AgentFastTrackRequest(None, serviceITSA, "ni", validNino.value, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsFastTrackInvitationController.showCheckDetails().url)
    }

    "return 303 and redirect to error url if service calling fast-track for ITSA contains invalid nino" in {
      val formData =
        AgentFastTrackRequest(Some(personal), serviceITSA, "ni", "INVALID_NINO", None).copy()
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
      val formData = AgentFastTrackRequest(Some(personal), serviceITSA)
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
        AgentFastTrackRequest(Some(personal), "", "ni", validNino.value, None)
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
      val agentSession = AgentSession(
        Some(personal),
        Some(serviceITSA),
        Some("ni"),
        Some(validNino.value),
        Some(validPostcode),
        fromFastTrack = fromFastTrack)
      await(sessionStore.save(agentSession))
      val result = await(
        fastTrackController.showCheckDetails(
          authorisedAsValidAgent(request.withHeaders("Referer" -> "/go/back/to/this"), arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("income and expenses through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Individual or sole trader"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("National Insurance number"))
      checkHtmlResultWithBodyText(result, "AB 12 34 56 A")
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Postcode"))
      checkHtmlResultWithBodyText(result, "DH1 4EJ")
      checkResultContainsBackLink(result, "/go/back/to/this")
    }

    "display the check details page without a back link when no Referer header is found" in {
      val agentSession = AgentSession(
        Some(personal),
        Some(serviceITSA),
        Some("ni"),
        Some(validNino.value),
        Some(validPostcode),
        fromFastTrack = fromFastTrack)
      await(sessionStore.save(agentSession))
      val result = await(fastTrackController.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithNotBodyText(result, "Back")
      checkHtmlResultWithBodyText(result, "DH1 4EJ")
    }

    "display the check details page when known fact is required and provided for ITSA for short postcode with spaces" in {
      val formData =
        AgentSession(
          Some(personal),
          Some(serviceITSA),
          Some("ni"),
          Some(validNino.value),
          Some(validPostcodeSpaces),
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))
      val result = await(fastTrackController.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
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
        AgentSession(
          Some(personal),
          Some(serviceITSA),
          Some("ni"),
          Some(validNino.value),
          Some(validPostcodeLong),
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))
      val result = await(fastTrackController.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
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
        AgentSession(
          Some(personal),
          Some(serviceITSA),
          Some("ni"),
          Some(validNino.value),
          knownFact = None,
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))
      val result = await(fastTrackController.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
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
        AgentSession(
          None,
          Some(serviceITSA),
          Some("ni"),
          Some(validNino.value),
          Some(validPostcode),
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))
      val result = await(fastTrackController.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
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
        Some(personal),
        mtdItId.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        serviceITSA,
        "MTDITID")
      givenAgentReference(arn, "AAAAAAAA", personal)
      givenMatchingClientIdAndPostcode(validNino, validPostcode)
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, serviceITSA)
      givenCheckRelationshipItsaWithStatus(arn, validNino.value, 404)
      givenAgentReferenceRecordExistsForArn(arn, "uid")

      val formData =
        AgentSession(
          Some(personal),
          Some(serviceITSA),
          Some("ni"),
          Some(validNino.value),
          Some(validPostcode),
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))
      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "redirect to identify-client when NO is selected for ITSA service" in {
      givenInvitationCreationSucceeds(
        arn,
        Some(personal),
        mtdItId.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        serviceITSA,
        "MTDITID")
      givenMatchingClientIdAndPostcode(validNino, validPostcode)

      val formData =
        AgentSession(
          None,
          Some(serviceITSA),
          Some("ni"),
          Some(validNino.value),
          Some(validPostcode),
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "false")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/identify-client")
    }

    "redirect to already-authorisation-pending when YES is selected for ITSA service but there is already a pending invitation" in {
      givenGetAllPendingInvitationsReturnsSome(arn, validNino.value, serviceITSA)
      givenCheckRelationshipItsaWithStatus(arn, validNino.value, 404)
      givenMatchingClientIdAndPostcode(validNino, validPostcode)

      val formData =
        AgentSession(
          None,
          Some(serviceITSA),
          Some("ni"),
          Some(validNino.value),
          Some(validPostcode),
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/already-authorisation-pending")
    }

    "redirect to already-authorisation-exists when YES is selected for ITSA service but there is already a relationship for this service" in {
      givenMatchingClientIdAndPostcode(validNino, validPostcode)
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, serviceITSA)
      givenCheckRelationshipItsaWithStatus(arn, validNino.value, 200)

      val formData =
        AgentSession(
          None,
          Some(serviceITSA),
          Some("ni"),
          Some(validNino.value),
          Some(validPostcode),
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))
      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/already-authorisation-present")
    }

    "return 303 not-matched if nino and postcode do not match for ITSA" in {
      val formData =
        AgentSession(
          Some(personal),
          Some(serviceITSA),
          Some("ni"),
          Some(validNino.value),
          Some(validPostcode),
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))
      givenNonMatchingClientIdAndPostcode(validNino, validPostcode)
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, serviceITSA)
      givenCheckRelationshipItsaWithStatus(arn, validNino.value, 404)
      givenAgentReferenceRecordExistsForArn(arn, "uid")

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "true")))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/not-matched")

      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEventFailed(arn.value, "personal", validNino.value, "ni", "Fail", serviceITSA)
      await(sessionStore.fetch).get shouldBe formData
    }

    "return 303 not-signed-up if Agent attempts to invite client who does not have an ITSA enrolment" in {
      val formData =
        AgentSession(
          Some(personal),
          Some(serviceITSA),
          Some("ni"),
          Some(validNino.value),
          Some(validPostcode),
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))
      givenNotEnrolledClientITSA(validNino, validPostcode)
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, serviceITSA)
      givenCheckRelationshipItsaWithStatus(arn, validNino.value, 404)
      givenAgentReferenceRecordExistsForArn(arn, "uid")

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "true")))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/not-signed-up")

      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEventFailed(arn.value, "personal", validNino.value, "ni", "Fail", serviceITSA)
      await(sessionStore.fetch).get shouldBe formData
    }
  }

  "GET agents/more-details" should {
    val request = FakeRequest()
    "display the known fact page when known fact is required and not provided for ITSA" in {
      val formData =
        AgentSession(
          None,
          Some(serviceITSA),
          Some("ni"),
          Some(validNino.value),
          Some(validPostcode),
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))
      val result = await(fastTrackController.showKnownFact(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, "What is your client's postcode?")
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("This will help us match their details against information we hold."))
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("This is the postcode of your client's registered address"))
      checkResultContainsBackLink(result, "/invitations/agents/check-details")
    }
  }

  "POST /agents/more-details" should {
    val request = FakeRequest("POST", "/agents/identify-client")
    "redirect to invitation sent when client details are valid and match for ITSA" in {
      givenMatchingClientIdAndPostcode(validNino, validPostcode)
      givenInvitationCreationSucceeds(
        arn,
        Some(personal),
        validNino.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        "HMRC-MTD-IT",
        "NI")
      givenAgentReference(arn, "AAAAAAAA", personal)
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, serviceITSA)
      givenCheckRelationshipItsaWithStatus(arn, validNino.value, 404)
      givenAgentReferenceRecordExistsForArn(arn, "uid")

      val requestWithForm = request.withFormUrlEncodedBody(
        "clientType"           -> "personal",
        "service"              -> "HMRC-MTD-IT",
        "clientIdentifierType" -> "ni",
        "clientIdentifier"     -> validNino.value,
        "knownFact"            -> "DH14EJ")

      val formData =
        AgentSession(
          Some(personal),
          Some(serviceITSA),
          Some("ni"),
          Some(validNino.value),
          knownFact = Some(validPostcode),
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))
      val result = await(fastTrackController.submitKnownFactItsa(authorisedAsValidAgent(requestWithForm, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "redirect to already-authorisation-pending when there is already a pending invitation" in {
      givenGetAllPendingInvitationsReturnsSome(arn, validNino.value, serviceITSA)
      givenCheckRelationshipItsaWithStatus(arn, validNino.value, 404)
      givenMatchingClientIdAndPostcode(validNino, validPostcode)

      val requestWithForm = request.withFormUrlEncodedBody(
        "clientType"           -> "personal",
        "service"              -> "HMRC-MTD-IT",
        "clientIdentifierType" -> "ni",
        "clientIdentifier"     -> validNino.value,
        "knownFact"            -> "DH14EJ")

      val formData =
        AgentSession(
          None,
          Some(serviceITSA),
          Some("ni"),
          Some(validNino.value),
          knownFact = None,
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))
      val result = await(fastTrackController.submitKnownFactItsa(authorisedAsValidAgent(requestWithForm, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/already-authorisation-pending")
    }

    "redirect to already-authorisation-present when there is already a relationship" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, serviceITSA)
      givenCheckRelationshipItsaWithStatus(arn, validNino.value, 200)
      givenMatchingClientIdAndPostcode(validNino, validPostcode)

      val requestWithForm = request.withFormUrlEncodedBody(
        "clientType"           -> "personal",
        "service"              -> "HMRC-MTD-IT",
        "clientIdentifierType" -> "ni",
        "clientIdentifier"     -> validNino.value,
        "knownFact"            -> "DH14EJ")

      val formData =
        AgentSession(
          None,
          Some(serviceITSA),
          Some("ni"),
          Some(validNino.value),
          knownFact = None,
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))
      val result = await(fastTrackController.submitKnownFactItsa(authorisedAsValidAgent(requestWithForm, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/already-authorisation-present")
    }

    "redisplay the page with errors when the known fact is not provided for ITSA" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, serviceITSA)
      givenCheckRelationshipItsaWithStatus(arn, validNino.value, 404)

      val formData = AgentSession(
        Some(personal),
        Some(serviceITSA),
        Some("ni"),
        Some(validNino.value),
        knownFact = None,
        fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))

      val requestWithForm = request.withFormUrlEncodedBody(
        "service"              -> "HMRC-MTD-IT",
        "clientIdentifierType" -> "ni",
        "clientIdentifier"     -> validNino.value,
        "knownFact"            -> "")
      val result = await(fastTrackController.submitKnownFactItsa(authorisedAsValidAgent(requestWithForm, arn.value)))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Enter your client's postcode"))
    }
  }

}
