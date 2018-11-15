package uk.gov.hmrc.agentinvitationsfrontend.controllers
import org.joda.time.LocalDate
import play.api.test.FakeRequest
import play.api.test.Helpers.redirectLocation
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.{agentFastTrackForm, agentInvitationServiceForm}
import uk.gov.hmrc.agentinvitationsfrontend.models.{CurrentInvitationInput, UserInputNinoAndPostcode}
import uk.gov.hmrc.agentinvitationsfrontend.support.{BaseISpec, TestDataCommonSupport}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global
import play.api.test.Helpers._

class FastTrackIRVISpec extends BaseISpec {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  "POST /agents/client-type" should {
    val request = FakeRequest("POST", "/agents/client-type")
    val submitClientType = controller.submitClientType()
    "return 303 for authorised Agent with valid Nino then selected personal, redirect to invitation-sent" in {
      testFastTrackCache.save(
        CurrentInvitationInput(None, servicePIR, "ni", validNino.value, Some(dateOfBirth), fromFastTrack))
      createInvitationStub(arn, validNino.value, invitationIdPIR, validNino.value, "ni", servicePIR, "NI")
      getAgentLinkStub(arn, "BBBBBBBB", "personal")
      givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
      getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, "NI", "Pending")
      val serviceForm = agentInvitationServiceForm.fill(UserInputNinoAndPostcode(personal, servicePIR, None, None))
      val result =
        submitClientType(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      verifyAuthoriseAttempt()
    }
  }

  "POST /agents/select-service" should {
    val request = FakeRequest("POST", "/agents/select-service")
    val submitService = controller.submitService()

    "return 303 for authorised Agent with valid Nino then selected IRV, redirect to invitation-sent" in {
      testFastTrackCache.save(
        CurrentInvitationInput(personal, "", "ni", validNino.value, Some(dateOfBirth), fromFastTrack))
      createInvitationStub(arn, validNino.value, invitationIdPIR, validNino.value, "ni", servicePIR, "NI")
      getAgentLinkStub(arn, "BBBBBBBB", "personal")
      givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
      getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, "NI", "Pending")
      val serviceForm = agentInvitationServiceForm.fill(UserInputNinoAndPostcode(personal, servicePIR, None, None))
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

    "return 303 check-details if service calling fast-track is correct for IRV" in {
      val formData =
        CurrentInvitationInput(personal, servicePIR, "ni", validNino.value, Some(dateOfBirth), fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.checkDetails().url
    }

    "return 303 and redirect to error url if service calling fast-track for PIR contains invalid nino" in {
      val formData =
        CurrentInvitationInput(personal, servicePIR, "ni", "INVALID_NINO", None, fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe
        Some("http://localhost:9996/tax-history/not-authorised?issue=INVALID_CLIENT_ID_RECEIVED:INVALID_NINO")
    }

    "return 303 and redirect to error url if service calling fast-track for IRV does not contain nino" in {
      val formData = CurrentInvitationInput(personal, servicePIR).copy(fromFastTrack = fromFastTrack)
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
      val formData = CurrentInvitationInput(personal, "", "ni", validNino.value, None, fromFastTrack)
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
      val formData =
        CurrentInvitationInput(personal, servicePIR, "ni", validNino.value, Some(dateOfBirth), fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.checkDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("view a client's PAYE income record"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("National Insurance number"))
      checkHtmlResultWithBodyText(result, "AB 12 34 56 A")
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Date of birth"))
      checkHtmlResultWithBodyText(result, "07 July 1980")
    }

    "display the check details page when client type is not provided for IRV" in {
      val formData =
        CurrentInvitationInput(None, servicePIR, "ni", validNino.value, Some(dateOfBirth), fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.checkDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("view a client's PAYE income record"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("National Insurance number"))
      checkHtmlResultWithBodyText(result, "AB 12 34 56 A")
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Date of birth"))
      checkHtmlResultWithBodyText(result, "07 July 1980")
      checkHtmlResultWithNotBodyText(result, "Change this information")
      checkHtmlResultWithNotBodyText(result, "We need some more details")
    }

    "display alternate check details page when known fact is required and not provided for IRV" in {
      val formData =
        CurrentInvitationInput(personal, servicePIR, "ni", validNino.value, None, fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.checkDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("view a client's PAYE income record"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("National Insurance number"))
      checkHtmlResultWithBodyText(result, "AB 12 34 56 A")
      checkHtmlResultWithBodyText(result, "Change this information")
      checkHtmlResultWithBodyText(result, "We need some more details")
    }
  }

  "POST /agents/check-details" should {
    val request = FakeRequest()

    "redirect to confirm_invitation when YES is selected for IRV service" in {
      createInvitationStub(arn, validNino.value, invitationIdPIR, validNino.value, "ni", servicePIR, "NI")
      getAgentLinkStub(arn, "BBBBBBBB", "personal")
      givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
      getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, "NI", "Pending")

      val formData =
        CurrentInvitationInput(personal, servicePIR, "ni", validNino.value, Some(dateOfBirth), fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(
        controller.submitDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "redirect to identify-client when NO is selected for IRV service" in {
      createInvitationStub(arn, validNino.value, invitationIdPIR, validNino.value, "ni", servicePIR, "NI")
      givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
      getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, "NI", "Pending")

      val formData =
        CurrentInvitationInput(personal, servicePIR, "ni", validNino.value, Some(dateOfBirth), fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(
        controller.submitDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "false")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/identify-client")
    }

    "return 303 invitation-sent if nino that does not return citizen-details record" in {
      val formData =
        CurrentInvitationInput(personal, servicePIR, "ni", validNino.value, Some(dateOfBirth), fromFastTrack)
      testFastTrackCache.save(formData)
      testFastTrackCache.currentSession.currentInvitationInput.get shouldBe formData
      givenCitizenDetailsReturns404For(validNino.value)
      createInvitationStub(arn, validNino.value, invitationIdPIR, validNino.value, "ni", servicePIR, "NI")
      getAgentLinkStub(arn, "BBBBBBBB", "personal")
      givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
      getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, "NI", "Pending")

      val form = agentFastTrackForm.fill(formData)
      val result = await(
        controller.submitDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")

      verifyAuthoriseAttempt()
      await(testFastTrackCache.fetch()).get shouldBe formData
    }
  }

  "GET agents/more-details" should {
    val request = FakeRequest()
    "display the known fact page when known fact is required and provided for IRV" in {
      val formData =
        CurrentInvitationInput(personal, servicePIR, "ni", validNino.value, None, fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.knownFact(authorisedAsValidAgent(request, arn.value)))
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
      createInvitationStub(arn, validNino.value, invitationIdPIR, validNino.value, "ni", servicePIR, "NI")
      getAgentLinkStub(arn, "BBBBBBBB", "personal")
      givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
      getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, "NI", "Pending")

      val requestWithForm = request.withFormUrlEncodedBody(
        "clientType"           -> "personal",
        "service"              -> "PERSONAL-INCOME-RECORD",
        "clientIdentifierType" -> "ni",
        "clientIdentifier"     -> validNino.value,
        "knownFact.year"       -> "1980",
        "knownFact.month"      -> "07",
        "knownFact.day"        -> "07"
      )
      val formData =
        CurrentInvitationInput(personal, servicePIR, "ni", validNino.value, None, fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.submitKnownFact(authorisedAsValidAgent(requestWithForm, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "redisplay the page with errors when known fact is not valid for IRV" in {
      createInvitationStub(arn, validNino.value, invitationIdPIR, validNino.value, "ni", servicePIR, "NI")
      checkVatRegisteredClientStub(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 204)
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, "VRN", "Pending")

      val requestWithForm = request.withFormUrlEncodedBody(
        "service"              -> "HMRC-MTD-VAT",
        "clientIdentifierType" -> "vrn",
        "clientIdentifier"     -> validVrn.value,
        "knownFact.year"       -> "aaaa",
        "knownFact.month"      -> "aa",
        "knownFact.day"        -> "aa"
      )
      val formData =
        CurrentInvitationInput(business, serviceVAT, "ni", validVrn.value, None, fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.submitKnownFact(authorisedAsValidAgent(requestWithForm, arn.value)))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Year must only include numbers")
      checkHtmlResultWithBodyText(result, "Month must only include numbers")
      checkHtmlResultWithBodyText(result, "Day must only include numbers")
    }
  }

}
