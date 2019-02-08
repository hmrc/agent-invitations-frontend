package uk.gov.hmrc.agentinvitationsfrontend.controllers
import org.joda.time.LocalDate
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsFastTrackInvitationController.agentFastTrackForm
import uk.gov.hmrc.agentinvitationsfrontend.forms.ClientTypeForm
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentFastTrackRequest, AgentSession}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class FastTrackIRVISpec extends BaseISpec {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  lazy val fastTrackController: AgentsFastTrackInvitationController = app.injector.instanceOf[AgentsFastTrackInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))
  val agentSession = AgentSession(personal, Some(servicePIR), Some("ni"), Some(validNino.value), Some(dateOfBirth), fromFastTrack = fromFastTrack)

  "POST /agents/client-type" should {
    val request = FakeRequest("POST", "/agents/client-type")
    val submitClientType = controller.submitClientType()
    "return 303 for authorised Agent with valid Nino then selected personal, redirect to invitation-sent" in {
      testAgentSessionCache.save(agentSession)
      givenInvitationCreationSucceeds(
        arn,
        personal,
        validNino.value,
        invitationIdPIR,
        validNino.value,
        "ni",
        servicePIR,
        "NI")
      givenAgentReference(arn, "BBBBBBBB", "personal")
      givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
      givenAgentReferenceRecordExistsForArn(arn, "uid")
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, servicePIR)

      val clientTypeForm = ClientTypeForm.form.fill("personal")
      val result =
        submitClientType(authorisedAsValidAgent(request.withFormUrlEncodedBody(clientTypeForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

//    "return 303 for authorised Agent with valid Nino then selected personal, redirect to select-service when cache is empty" in {
//      givenInvitationCreationSucceeds(
//        arn,
//        personal,
//        validNino.value,
//        invitationIdPIR,
//        validNino.value,
//        "ni",
//        servicePIR,
//        "NI")
//      givenAgentReference(arn, "BBBBBBBB", "personal")
//      givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
//
//      testAgentSessionCache.save(agentSession.copy(clientType = None))
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

    "return 303 check-details if service calling fast-track is correct for IRV" in {
      val formData =
        AgentFastTrackRequest(personal, servicePIR, "ni", validNino.value, Some(dateOfBirth))
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsFastTrackInvitationController.showCheckDetails().url
    }

    "return 303 and redirect to error url if service calling fast-track for PIR contains invalid nino" in {
      val formData =
        AgentFastTrackRequest(personal, servicePIR, "ni", "INVALID_NINO" , Some(dateOfBirth))
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe
        Some("http://localhost:9996/tax-history/not-authorised?issue=INVALID_CLIENT_ID_RECEIVED:INVALID_NINO")
    }

    "return 303 and redirect to error url if service calling fast-track for IRV does not contain nino" in {
      val formData = AgentFastTrackRequest(personal, servicePIR, "", "", Some(dateOfBirth))
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe
        Some(
          "http://localhost:9996/tax-history/not-authorised?issue=UNSUPPORTED_CLIENT_ID_TYPE INVALID_CLIENT_ID_RECEIVED:NOTHING")
    }

    "return 303 and redirect to error url if there is no service but all other fields are valid for IRV" in {
      val formData = AgentFastTrackRequest(personal, "", "ni", validNino.value, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe
        Some("http://localhost:9996/tax-history/not-authorised?issue=UNSUPPORTED_SERVICE")
    }

//    "return 303 and redirect to error url if there is no client-type but all other fields are valid for IRV" in {
//      val formData = CurrentInvitationInput(None,servicePIR, "ni", validNino.value, None, fromFastTrack)
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

    "display the check details page when known fact is required and provided for IRV" in {
      testAgentSessionCache.save(agentSession)

      val result = await(fastTrackController.showCheckDetails(authorisedAsValidAgent(request, arn.value)))

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("view a client's PAYE income record"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Individual or sole trader"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("National Insurance number"))
      checkHtmlResultWithBodyText(result, "AB 12 34 56 A")
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Date of birth"))
      checkHtmlResultWithBodyText(result, "07 July 1980")
      checkHtmlResultWithBodyText(result, "Yes - I want to ask this client for authorisation")
      checkHtmlResultWithBodyText(result, "No - I need to change them")
    }

    "display alternate check details page when known fact is required and not provided for IRV" in {
      testAgentSessionCache.save(agentSession.copy(knownFact = None))
      val result = await(fastTrackController.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("view a client's PAYE income record"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Individual or sole trader"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("National Insurance number"))
      checkHtmlResultWithBodyText(result, "AB 12 34 56 A")
      checkHtmlResultWithBodyText(result, "Change this information")
      checkHtmlResultWithBodyText(result, "We need some more details")
    }
  }

  "POST /agents/check-details" should {
    val request = FakeRequest()

    "redirect to confirm_invitation when YES is selected for IRV service" in {
      givenInvitationCreationSucceeds(
        arn,
        personal,
        validNino.value,
        invitationIdPIR,
        validNino.value,
        "ni",
        servicePIR,
        "NI")
      givenAgentReference(arn, "BBBBBBBB", "personal")
      givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, servicePIR)
      givenAfiRelationshipNotFoundForAgent(arn, validNino)
      givenAgentReferenceRecordExistsForArn(arn, "uid")

      testAgentSessionCache.save(agentSession)
      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "redirect to identify-client when NO is selected for IRV service" in {
      givenInvitationCreationSucceeds(
        arn,
        personal,
        validNino.value,
        invitationIdPIR,
        validNino.value,
        "ni",
        servicePIR,
        "NI")
      givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))

      testAgentSessionCache.save(agentSession)
      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "false")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/identify-client")
    }

    "redirect to already-authorisation-pending when YES is selected for IRV service and there is already a pending invitation" in {
      givenGetAllPendingInvitationsReturnsSome(arn, validNino.value, servicePIR)
      givenAfiRelationshipNotFoundForAgent(arn, validNino)
      givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))

      testAgentSessionCache.save(agentSession)
      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/already-authorisation-pending")
    }

    "redirect to already-authorisation-present when YES is selected for IRV service and there is already a relationship" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, servicePIR)
      givenAfiRelationshipIsActiveForAgent(arn, validNino)
      givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))

      testAgentSessionCache.save(agentSession)
      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/already-authorisation-present")
    }

    "return 303 invitation-sent if nino that does not return citizen-details record" in {
      testAgentSessionCache.save(agentSession)
      givenCitizenDetailsReturns404For(validNino.value)
      givenInvitationCreationSucceeds(
        arn,
        personal,
        validNino.value,
        invitationIdPIR,
        validNino.value,
        "ni",
        servicePIR,
        "NI")
      givenAgentReference(arn, "BBBBBBBB", "personal")
      givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, servicePIR)
      givenAfiRelationshipNotFoundForAgent(arn, validNino)
      givenAgentReferenceRecordExistsForArn(arn, "uid")

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }
  }

  "GET agents/more-details" should {
    val request = FakeRequest()
    "display the known fact page when known fact is required and provided for IRV" in {
      testAgentSessionCache.save(agentSession.copy(knownFact = None))
      val result = await(fastTrackController.showKnownFact(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, "What is your client's date of birth?")
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("This will help us match their details against information we hold."))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("For example, 31 3 1980"))
    }
  }

  "POST /agents/more-details" should {
    val request = FakeRequest("POST", "/agents/identify-client")
    "redirect to invitation sent when client details are valid and match for IRV" in {
      givenInvitationCreationSucceeds(
        arn,
        personal,
        validNino.value,
        invitationIdPIR,
        validNino.value,
        "ni",
        servicePIR,
        "NI")
      givenAgentReference(arn, "BBBBBBBB", "personal")
      givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, servicePIR)
      givenAfiRelationshipNotFoundForAgent(arn, validNino)
      givenAgentReferenceRecordExistsForArn(arn, "uid")

      val requestWithForm = request.withFormUrlEncodedBody(
        "clientType"           -> "personal",
        "service"              -> "PERSONAL-INCOME-RECORD",
        "clientIdentifierType" -> "ni",
        "clientIdentifier"     -> validNino.value,
        "knownFact.year"       -> "1980",
        "knownFact.month"      -> "07",
        "knownFact.day"        -> "07"
      )
      testAgentSessionCache.save(agentSession)
      val result = await(fastTrackController.submitKnownFact(authorisedAsValidAgent(requestWithForm, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "redirect to already-authorisation-present when a relationship already exists between agent and client" in {
      givenAgentReference(arn, "BBBBBBBB", "personal")
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, servicePIR)
      givenAfiRelationshipIsActiveForAgent(arn, validNino)
      givenAgentReferenceRecordExistsForArn(arn, "uid")

      val requestWithForm = request.withFormUrlEncodedBody(
        "clientType"           -> "personal",
        "service"              -> "PERSONAL-INCOME-RECORD",
        "clientIdentifierType" -> "ni",
        "clientIdentifier"     -> validNino.value,
        "knownFact.year"       -> "1980",
        "knownFact.month"      -> "07",
        "knownFact.day"        -> "07"
      )

      testAgentSessionCache.save(agentSession)

      val result = await(fastTrackController.submitKnownFact(authorisedAsValidAgent(requestWithForm, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/already-authorisation-present")
    }

    "redirect to already-authorisation-pending when a relationship already exists between agent and client when there is no end date" in {
      givenAgentReference(arn, "BBBBBBBB", "personal")
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, servicePIR)
      givenAfiRelationshipIsActiveForAgentNoEndDate(arn, validNino)
      givenAgentReferenceRecordExistsForArn(arn, "uid")

      val requestWithForm = request.withFormUrlEncodedBody(
        "clientType"           -> "personal",
        "service"              -> "PERSONAL-INCOME-RECORD",
        "clientIdentifierType" -> "ni",
        "clientIdentifier"     -> validNino.value,
        "knownFact.year"       -> "1980",
        "knownFact.month"      -> "07",
        "knownFact.day"        -> "07"
      )

      testAgentSessionCache.save(agentSession)

      val result = await(fastTrackController.submitKnownFact(authorisedAsValidAgent(requestWithForm, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/already-authorisation-present")
    }

    "redisplay the page with errors when known fact is not valid for IRV" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, servicePIR)
      givenAfiRelationshipNotFoundForAgent(arn, validNino)

      val requestWithForm = request.withFormUrlEncodedBody(
        "clientType"           -> "personal",
        "service"              -> "PERSONAL-INCOME-RECORD",
        "clientIdentifierType" -> "NI",
        "clientIdentifier"     -> validNino.value,
        "knownFact.year"       -> "aaaa",
        "knownFact.month"      -> "aa",
        "knownFact.day"        -> "aa"
      )
      testAgentSessionCache.save(agentSession.copy(knownFact = None))
      val result = await(fastTrackController.submitKnownFact(authorisedAsValidAgent(requestWithForm, arn.value)))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Year must only include numbers")
      checkHtmlResultWithBodyText(result, "Month must only include numbers")
      checkHtmlResultWithBodyText(result, "Day must only include numbers")
    }
  }

}
