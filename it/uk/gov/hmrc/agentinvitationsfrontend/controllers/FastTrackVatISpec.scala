package uk.gov.hmrc.agentinvitationsfrontend.controllers
import org.joda.time.LocalDate
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.{agentFastTrackForm, agentInvitationServiceForm}
import uk.gov.hmrc.agentinvitationsfrontend.models.{CurrentAuthorisationRequest, UserInputNinoAndPostcode}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class FastTrackVatISpec extends BaseISpec {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  "POST /agents/client-type" should {
    val request = FakeRequest("POST", "/agents/client-type")
    val submitClientType = controller.submitClientType()
    "return 303 for authorised Agent with valid VAT information and selected Organisation, redirect to invitation-sent" in {
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(
          Some("business"),
          serviceVAT,
          "vrn",
          validVrn.value,
          Some(validRegistrationDate),
          fromFastTrack))
      givenInvitationCreationSucceeds(
        arn,
        business,
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        identifierVAT)
      givenAgentReference(arn, "BBBBBBBB", "business")
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse("2007-07-07"), 204)

      val serviceForm = agentInvitationServiceForm.fill(UserInputNinoAndPostcode(business, serviceVAT, None, None))
      val result =
        submitClientType(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      verify2AuthoriseAttempt()
    }

    "return 303 for authorised Agent with valid VAT information and selected Organisation, redirect to select-service when cache is empty" in {
      givenInvitationCreationSucceeds(
        arn,
        business,
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        identifierVAT)
      givenAgentReference(arn, "BBBBBBBB", "business")
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse("2007-07-07"), 204)

      val serviceForm = agentInvitationServiceForm.fill(UserInputNinoAndPostcode(business, serviceVAT, None, None))
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

    "return 303 for authorised Agent with valid VAT information and selected VAT, redirect to invitation-sent" in {
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(business, "", "vrn", validVrn.value, Some(validRegistrationDate), fromFastTrack))
      givenInvitationCreationSucceeds(
        arn,
        business,
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        identifierVAT)
      givenAgentReference(arn, "BBBBBBBB", "business")
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse("2007-07-07"), 204)

      val serviceForm = agentInvitationServiceForm.fill(UserInputNinoAndPostcode(business, serviceVAT, None, None))
      val result =
        submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      verify2AuthoriseAttempt()
    }
  }

  "POST /agents/fast-track" should {
    val request = FakeRequest(
      "POST",
      "/agents/fast-track?continue=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fselect-client&error=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fnot-authorised"
    )
    val fastTrack = controller.agentFastTrack()

    "return 303 check-details if service calling fast-track is correct for VAT" in {
      val formData =
        CurrentAuthorisationRequest(
          business,
          serviceVAT,
          "vrn",
          validVrn.value,
          Some(validRegistrationDate),
          fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showCheckDetails().url
    }

    "return 303 check-details if service calling fast-track does not contain vat-reg-date for VAT" in {
      val formData =
        CurrentAuthorisationRequest(business, serviceVAT, "vrn", validVrn.value, None, fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)

      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showCheckDetails().url)
    }

    "return 303 check-details if service calling fast-track contains invalid vat-reg-date for VAT" in {
      val formData =
        CurrentAuthorisationRequest(
          business,
          serviceVAT,
          "vrn",
          validVrn.value,
          Some("Invalid_Reg_Date"),
          fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)

      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showCheckDetails().url)
    }

    "return 303 check-details if service calling fast-track does not contain client type" in {
      val formData =
        CurrentAuthorisationRequest(None, serviceVAT, "vrn", validVrn.value, Some(validRegistrationDate), fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)

      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showCheckDetails().url)
    }

    "return 303 and redirect to error url if service calling fast-track for VAT contains invalid vrn" in {
      val formData = CurrentAuthorisationRequest(business, serviceVAT, "", "INVALID_VRN", None, fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe
        Some(
          "http://localhost:9996/tax-history/not-authorised?issue=UNSUPPORTED_CLIENT_ID_TYPE INVALID_CLIENT_ID_RECEIVED:INVALID_VRN")
    }

    "return 303 and redirect to error url if service calling fast-track for VAT does not contain vrn" in {
      val formData = CurrentAuthorisationRequest(business, serviceVAT).copy(fromFastTrack = fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe
        Some(
          "http://localhost:9996/tax-history/not-authorised?issue=UNSUPPORTED_CLIENT_ID_TYPE INVALID_CLIENT_ID_RECEIVED:NOTHING")
    }

    "return 303 and redirect to error url if there is no service but all other fields are valid for VAT" in {
      val formData =
        CurrentAuthorisationRequest(business, "", "vrn", validVrn.value, Some(validRegistrationDate), fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe
        Some("http://localhost:9996/tax-history/not-authorised?issue=UNSUPPORTED_SERVICE")
    }

//    "return 303 and redirect to error url if there is no client-type but all other fields are valid for VAT" in {
//      val formData =
//        CurrentInvitationInput(None, serviceVAT, "vrn", validVrn.value, Some(validRegistrationDate), fromFastTrack)
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
    "display the check details page when known fact is required and provided for VAT" in {
      val formData =
        CurrentAuthorisationRequest(
          business,
          serviceVAT,
          "vrn",
          validVrn.value,
          Some(validRegistrationDate),
          fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(controller.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("report a client's VAT returns through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Company or partnership"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("VAT registration number"))
      checkHtmlResultWithBodyText(result, validVrn.value)
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("VAT registration date"))
      checkHtmlResultWithBodyText(result, "07 July 2007")
    }

    "display alternate check details page when known fact is required and not provided for VAT" in {
      val formData =
        CurrentAuthorisationRequest(business, serviceVAT, "vrn", validVrn.value, None, fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(controller.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("report a client's VAT returns through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Company or partnership"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("VAT registration number"))
      checkHtmlResultWithBodyText(result, validVrn.value)
      checkHtmlResultWithBodyText(result, "Change this information")
      checkHtmlResultWithBodyText(result, "We need some more details")
    }

    "display alternate check details page when client-type is required and not provided for VAT" in {
      val formData =
        CurrentAuthorisationRequest(None, serviceVAT, "vrn", validVrn.value, Some(validRegistrationDate), fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(controller.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("report a client's VAT returns through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("VAT registration number"))
      checkHtmlResultWithBodyText(result, validVrn.value)
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("VAT registration date"))
      checkHtmlResultWithBodyText(result, "07 July 2007")
      checkHtmlResultWithBodyText(result, "Change this information")
      checkHtmlResultWithBodyText(result, "We need some more details")
      checkHtmlResultWithNotBodyText(result, "Company or partnership")
    }
  }

  "POST /agents/check-details" should {
    val request = FakeRequest()

    "redirect to confirm_invitation when YES is selected for VAT service" in {
      givenInvitationCreationSucceeds(
        arn,
        business,
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        "VRN")
      givenAgentReference(arn, "BBBBBBBB", "business")
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 200)

      val formData =
        CurrentAuthorisationRequest(
          business,
          serviceVAT,
          "vrn",
          validVrn.value,
          Some(validRegistrationDate),
          fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(
        controller.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "redirect to client-type when client type is not provided and YES is selected for VAT service" in {
      givenInvitationCreationSucceeds(
        arn,
        business,
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        "VRN")
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 200)

      val formData =
        CurrentAuthorisationRequest(None, serviceVAT, "vrn", validVrn.value, Some(validRegistrationDate), fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(
        controller.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/client-type")
    }

    "redirect to client-type when client type and known fact are not provided and YES is selected for VAT service" in {
      givenInvitationCreationSucceeds(
        arn,
        business,
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        "VRN")
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 200)

      val formData =
        CurrentAuthorisationRequest(None, serviceVAT, "vrn", validVrn.value, None, fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(
        controller.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/client-type")
    }

    "redirect to more-details when known fact is not provided and YES is selected for VAT service" in {
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 200)

      val formData =
        CurrentAuthorisationRequest(personal, serviceVAT, "vrn", validVrn.value, None, fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(
        controller.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showKnownFact().url
    }

    "redirect to identify-client when NO is selected for VAT service" in {
      givenInvitationCreationSucceeds(
        arn,
        business,
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        "VRN")
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 200)

      val formData =
        CurrentAuthorisationRequest(
          business,
          serviceVAT,
          "vrn",
          validVrn.value,
          Some(validRegistrationDate),
          fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(
        controller.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "false")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/identify-client")
    }

    "return 303 not-matched if vrn and vat-reg-date does not match for VAT" in {
      val invitation =
        CurrentAuthorisationRequest(
          business,
          serviceVAT,
          "vrn",
          validVrn.value,
          Some(validRegistrationDate),
          fromFastTrack)

      testCurrentAuthorisationRequestCache.save(invitation)
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse("2007-07-07"), 403)

      val form = agentFastTrackForm.fill(invitation)
      val result = await(
        controller.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))

      status(result) shouldBe 303
      header("Set-Cookie", result) shouldBe None
      redirectLocation(result) shouldBe Some("/invitations/agents/not-matched")
      verifyCheckVatRegisteredClientStubAttempt(validVrn, LocalDate.parse("2007-07-07"))
      await(testCurrentAuthorisationRequestCache.fetch).get shouldBe invitation
    }

    "return 303 not-signed-up if Agent attempted to invite a client for VAT" in {
      val invitation =
        CurrentAuthorisationRequest(
          business,
          serviceVAT,
          "vrn",
          validVrn.value,
          Some(validRegistrationDate),
          fromFastTrack)

      testCurrentAuthorisationRequestCache.save(invitation)
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse("2007-07-07"), 404)

      val form = agentFastTrackForm.fill(invitation)
      val result = await(
        controller.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))

      status(result) shouldBe 303
      header("Set-Cookie", result) shouldBe None
      redirectLocation(result) shouldBe Some("/invitations/agents/not-signed-up")
      verifyCheckVatRegisteredClientStubAttempt(validVrn, LocalDate.parse("2007-07-07"))
      await(testCurrentAuthorisationRequestCache.fetch).get shouldBe invitation

    }
  }

  "GET /agents/more-details" should {
    val request = FakeRequest()
    "display the known fact page when known fact is required and provided for VAT" in {
      val formData =
        CurrentAuthorisationRequest(business, serviceVAT, "vrn", validVrn.value, None, fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(controller.showKnownFact(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, "What is your client's VAT registration date?")
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("This will help us match their details against information we hold."))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("For example, 31 8 2015"))
    }
  }

  "POST /agents/more-details" should {
    val request = FakeRequest("POST", "/agents/identify-client")
    "redirect to invitation sent when client details are valid and match for VAT" in {
      givenInvitationCreationSucceeds(
        arn,
        business,
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        identifierVAT)
      givenAgentReference(arn, "BBBBBBBB", "business")
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 204)

      val requestWithForm = request.withFormUrlEncodedBody(
        "clientType"           -> "business",
        "service"              -> "HMRC-MTD-VAT",
        "clientIdentifierType" -> "vrn",
        "clientIdentifier"     -> validVrn.value,
        "knownFact.year"       -> "2007",
        "knownFact.month"      -> "07",
        "knownFact.day"        -> "07"
      )
      val formData =
        CurrentAuthorisationRequest(business, serviceVAT, "vrn", validVrn.value, None, fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(controller.submitKnownFact(authorisedAsValidAgent(requestWithForm, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "redisplay the page with errors when known fact is not valid for VAT" in {
      givenInvitationCreationSucceeds(
        arn,
        business,
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        "VRN")
      givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))

      val requestWithForm = request.withFormUrlEncodedBody(
        "service"              -> "PERSONAL-INCOME-RECORD",
        "clientIdentifierType" -> "ni",
        "clientIdentifier"     -> validNino.value,
        "knownFact.year"       -> "aaaa",
        "knownFact.month"      -> "aa",
        "knownFact.day"        -> "aa"
      )
      val formData =
        CurrentAuthorisationRequest(business, servicePIR, "ni", validNino.value, None, fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(controller.submitKnownFact(authorisedAsValidAgent(requestWithForm, arn.value)))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Year must only include numbers")
      checkHtmlResultWithBodyText(result, "Month must only include numbers")
      checkHtmlResultWithBodyText(result, "Day must only include numbers")
    }
  }

}
