package uk.gov.hmrc.agentinvitationsfrontend.controllers.retired

import java.util.UUID

import org.joda.time.LocalDate
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.retired
import uk.gov.hmrc.agentinvitationsfrontend.controllers.retired.AgentsFastTrackInvitationController.agentFastTrackForm
import uk.gov.hmrc.agentinvitationsfrontend.forms.ClientTypeForm
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

class FastTrackVatISpec extends BaseISpec {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  lazy val fastTrackController: AgentsFastTrackInvitationController =
    app.injector.instanceOf[AgentsFastTrackInvitationController]
  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId(UUID.randomUUID().toString)))

  "POST /agents/client-type" should {
    val request = FakeRequest("POST", "/agents/client-type")
    val submitClientType = controller.submitClientType()
    "return 303 for authorised Agent with valid VAT information and selected Organisation, redirect to invitation-sent" in {
      await(
        sessionStore.save(
          AgentSession(
            Some(business),
            Some(serviceVAT),
            Some("vrn"),
            Some(validVrn.value),
            Some(validRegistrationDate),
            fromFastTrack = fromFastTrack)))
      givenInvitationCreationSucceeds(
        arn,
        Some(business),
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        identifierVAT)
      givenAgentReference(arn, "BBBBBBBB", business)
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse("2007-07-07"), 204)
      givenAgentReferenceRecordExistsForArn(arn, "uid")
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)

      val clientTypeForm = ClientTypeForm.form.fill(business)
      val result =
        submitClientType(
          authorisedAsValidAgent(request.withFormUrlEncodedBody(clientTypeForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations2/agents/invitation-sent")
    }

    "return 303 for authorised Agent with valid VAT information and selected Organisation, redirect to select-service when cache is empty" in {
      sessionStore.delete()
      givenInvitationCreationSucceeds(
        arn,
        Some(business),
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        identifierVAT)
      givenAgentReference(arn, "BBBBBBBB", business)
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse("2007-07-07"), 204)

      val clientTypeForm = ClientTypeForm.form.fill(business)
      val result =
        submitClientType(
          authorisedAsValidAgent(request.withFormUrlEncodedBody(clientTypeForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations2/agents/select-service")
      verifyAuthoriseAttempt()
    }
  }

  "POST /agents/fast-track" should {
    val request = FakeRequest(
      "POST",
      "/agents/fast-track?continue=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fselect-client&error=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fnot-authorised"
    )
    val fastTrack = fastTrackController.agentFastTrack()

    "return 303 check-details if service calling fast-track is correct for VAT" in {
      val formData =
        AgentFastTrackRequest(Some(business), serviceVAT, "vrn", validVrn.value, Some(validRegistrationDate))
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe retired.routes.AgentsFastTrackInvitationController.showCheckDetails().url
    }

    "return 303 check-details if service calling fast-track does not contain vat-reg-date for VAT" in {
      val formData =
        AgentFastTrackRequest(Some(business), serviceVAT, "vrn", validVrn.value, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)

      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(retired.routes.AgentsFastTrackInvitationController.showCheckDetails().url)
    }

    "return 303 check-details if service calling fast-track contains invalid vat-reg-date for VAT" in {
      val formData =
        AgentFastTrackRequest(Some(business), serviceVAT, "vrn", validVrn.value, Some("invalid_reg_date"))
      val fastTrackFormData = agentFastTrackForm.fill(formData)

      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(retired.routes.AgentsFastTrackInvitationController.showCheckDetails().url)
    }

    "return 303 check-details if service calling fast-track does not contain client type" in {
      val formData =
        AgentFastTrackRequest(None, serviceVAT, "vrn", validVrn.value, Some(validRegistrationDate))
      val fastTrackFormData = agentFastTrackForm.fill(formData)

      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(retired.routes.AgentsFastTrackInvitationController.showCheckDetails().url)
    }

    "return 303 and redirect to error url if service calling fast-track for VAT contains invalid vrn" in {
      val formData = AgentFastTrackRequest(Some(business), serviceVAT, "", "INVALID_VRN", None)
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
      val formData = AgentFastTrackRequest(Some(business), serviceVAT)
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
        AgentFastTrackRequest(Some(business), "", "vrn", validVrn.value, Some(validRegistrationDate))
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
        AgentSession(
          Some(business),
          Some(serviceVAT),
          Some("vrn"),
          Some(validVrn.value),
          Some(validRegistrationDate),
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))
      val result = await(fastTrackController.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
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
        AgentSession(
          Some(business),
          Some(serviceVAT),
          Some("vrn"),
          Some(validVrn.value),
          None,
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))
      val result = await(fastTrackController.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
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
        AgentSession(
          None,
          Some(serviceVAT),
          Some("vrn"),
          Some(validVrn.value),
          Some(validRegistrationDate),
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))
      val result = await(fastTrackController.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
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
        Some(business),
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        "VRN")
      givenAgentReference(arn, "BBBBBBBB", business)
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 200)
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)
      givenAgentReferenceRecordExistsForArn(arn, "uid")

      val agentSession =
        AgentSession(
          Some(business),
          Some(serviceVAT),
          Some("vrn"),
          Some(validVrn.value),
          Some(validRegistrationDate),
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(agentSession))

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations2/agents/invitation-sent")
    }

    "redirect to client-type when client type is not provided and YES is selected for VAT service" in {
      givenInvitationCreationSucceeds(
        arn,
        Some(business),
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        "VRN")
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 200)
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)

      val agentSession =
        AgentSession(
          None,
          Some(serviceVAT),
          Some("vrn"),
          Some(validVrn.value),
          Some(validRegistrationDate),
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(agentSession))

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations2/agents/client-type")
    }

    "redirect to client-type when client type and known fact are not provided and YES is selected for VAT service" in {
      givenInvitationCreationSucceeds(
        arn,
        Some(business),
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        "VRN")
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 200)
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)

      val agentSession =
        AgentSession(None, Some(serviceVAT), Some("vrn"), Some(validVrn.value), None, fromFastTrack = fromFastTrack)
      await(sessionStore.save(agentSession))

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations2/agents/client-type")
    }

    "redirect to more-details when known fact is not provided and YES is selected for VAT service" in {
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 200)
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)

      val agentSession =
        AgentSession(
          Some(personal),
          Some(serviceVAT),
          Some("vrn"),
          Some(validVrn.value),
          None,
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(agentSession))

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe retired.routes.AgentsFastTrackInvitationController.showKnownFact().url
    }

    "redirect to already-authorisation-pending when YES is selected for VAT service and there is already a pending invitation" in {
      givenGetAllPendingInvitationsReturnsSome(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 200)

      val agentSession =
        AgentSession(
          Some(business),
          Some(serviceVAT),
          Some("vrn"),
          Some(validVrn.value),
          None,
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(agentSession))

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations2/agents/already-authorisation-pending")
    }

    "redirect to already-authorisation-present when YES is selected for VAT service and there is already a relationship" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 200)
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 200)

      val agentSession =
        AgentSession(None, Some(serviceVAT), Some("vrn"), Some(validVrn.value), None, fromFastTrack = fromFastTrack)
      await(sessionStore.save(agentSession))

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations2/agents/already-authorisation-present")
    }

    "redirect to identify-client when NO is selected for VAT service" in {
      givenInvitationCreationSucceeds(
        arn,
        Some(business),
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        "VRN")
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 200)

      val agentSession =
        AgentSession(None, Some(serviceVAT), Some("vrn"), Some(validVrn.value), None, fromFastTrack = fromFastTrack)
      await(sessionStore.save(agentSession))

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "false")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations2/agents/identify-client")
    }

    "return 303 not-matched if vrn and vat-reg-date does not match for VAT" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse("2007-07-07"), 403)

      val agentSession =
        AgentSession(
          Some(personal),
          Some(serviceVAT),
          Some("vrn"),
          Some(validVrn.value),
          Some("2007-07-07"),
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(agentSession))

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "true")))

      status(result) shouldBe 303
      header("Set-Cookie", result) shouldBe None
      redirectLocation(result) shouldBe Some("/invitations2/agents/not-matched")
      verifyCheckVatRegisteredClientStubAttempt(validVrn, LocalDate.parse("2007-07-07"))
    }

    "return 303 not-signed-up if Agent attempted to invite a client for VAT" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse("2007-07-07"), 404)

      val agentSession =
        AgentSession(
          Some(personal),
          Some(serviceVAT),
          Some("vrn"),
          Some(validVrn.value),
          Some("2007-07-07"),
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(agentSession))

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "true")))

      status(result) shouldBe 303
      header("Set-Cookie", result) shouldBe None
      redirectLocation(result) shouldBe Some("/invitations2/agents/not-signed-up")
      verifyCheckVatRegisteredClientStubAttempt(validVrn, LocalDate.parse("2007-07-07"))

    }
  }

  "GET /agents/more-details" should {
    val request = FakeRequest()
    "display the known fact page when known fact is required and provided for VAT" in {
      val agentSession =
        AgentSession(None, Some(serviceVAT), Some("vrn"), Some(validVrn.value), None, fromFastTrack = fromFastTrack)
      await(sessionStore.save(agentSession))
      val result = await(fastTrackController.showKnownFact(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, "What is your client's VAT registration date?")
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("This will help us match their details against information we hold."))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("For example, 31 8 2015"))
    }
  }

  "POST /agents/more-details" should {
    val request = FakeRequest("POST", "/agents/more-details")
    "redirect to invitation sent when client details are valid and match for VAT" in {
      givenInvitationCreationSucceeds(
        arn,
        Some(business),
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        identifierVAT)
      givenAgentReference(arn, "BBBBBBBB", business)
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 204)
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)
      givenAgentReferenceRecordExistsForArn(arn, "uid")

      val requestWithForm = request.withFormUrlEncodedBody(
        "clientType"           -> "business",
        "service"              -> "HMRC-MTD-VAT",
        "clientIdentifierType" -> "vrn",
        "clientIdentifier"     -> validVrn.value,
        "knownFact.year"       -> "2007",
        "knownFact.month"      -> "07",
        "knownFact.day"        -> "07"
      )

      val agentSession =
        AgentSession(
          Some(business),
          Some(serviceVAT),
          Some("vrn"),
          Some(validVrn.value),
          None,
          fromFastTrack = fromFastTrack)
      await(sessionStore.save(agentSession))

      val result = await(fastTrackController.submitKnownFactVat(authorisedAsValidAgent(requestWithForm, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations2/agents/invitation-sent")
    }

    "redisplay the page with errors when known fact is not provided for VAT" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)

      val requestWithForm = request.withFormUrlEncodedBody(
        "service"              -> "HMRC-MTD-VAT",
        "clientIdentifierType" -> "vrn",
        "clientIdentifier"     -> validVrn.value,
        "knownFact.year"       -> "aaaa",
        "knownFact.month"      -> "aa",
        "knownFact.day"        -> "aa"
      )

      val agentSession =
        AgentSession(None, Some(serviceVAT), Some("vrn"), Some(validVrn.value), None, fromFastTrack = fromFastTrack)
      await(sessionStore.save(agentSession))

      val result = await(fastTrackController.submitKnownFactVat(authorisedAsValidAgent(requestWithForm, arn.value)))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Year must only include numbers")
      checkHtmlResultWithBodyText(result, "Month must only include numbers")
      checkHtmlResultWithBodyText(result, "Day must only include numbers")
    }
  }

}