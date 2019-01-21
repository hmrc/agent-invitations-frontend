package uk.gov.hmrc.agentinvitationsfrontend.controllers

import com.google.inject.AbstractModule
import org.joda.time.LocalDate
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.forms.ClientTypeForm
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentMultiAuthorisationJourneyState, CurrentAuthorisationRequest}
import uk.gov.hmrc.agentinvitationsfrontend.services.{AgentMultiAuthorisationJourneyStateCache, ContinueUrlCache, CurrentAuthorisationRequestCache}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class FastTrackVatOppositeFlagsISpec extends BaseISpec {

  import scala.concurrent.duration._
  override implicit val defaultTimeout = 35 seconds

  override protected def appBuilder: GuiceApplicationBuilder =
    new GuiceApplicationBuilder()
      .configure(
        "microservice.services.auth.port"                                     -> wireMockPort,
        "microservice.services.agent-client-authorisation.port"               -> wireMockPort,
        "microservice.services.agent-client-relationships.port"               -> wireMockPort,
        "microservice.services.agent-services-account.port"                   -> wireMockPort,
        "microservice.services.company-auth.login-url"                        -> wireMockHost,
        "microservice.services.company-auth.port"                             -> wireMockPort,
        "microservice.services.des.port"                                      -> wireMockPort,
        "microservice.services.agent-fi-relationship.port"                    -> wireMockPort,
        "microservice.services.citizen-details.host"                          -> wireMockHost,
        "microservice.services.citizen-details.port"                          -> wireMockPort,
        "microservice.services.agent-invitations-frontend.external-url"       -> wireMockBaseUrlAsString,
        "microservice.services.agent-services-account-frontend.external-url"  -> wireMockBaseUrlAsString,
        "microservice.services.company-auth-frontend.external-url"            -> companyAuthUrl,
        "microservice.services.company-auth-frontend.sign-out.path"           -> companyAuthSignOutPath,
        "microservice.services.business-tax-account.external-url"             -> businessTaxAccountUrl,
        "microservice.services.tax-account-router-frontend.account-url"       -> taxAccountRelativeUrl,
        "microservice.services.personal-tax-account.external-url"             -> personalTaxAccountUrl,
        "auditing.enabled"                                                    -> true,
        "auditing.consumer.baseUri.host"                                      -> wireMockHost,
        "auditing.consumer.baseUri.port"                                      -> wireMockPort,
        "features.show-hmrc-mtd-it"                                           -> true,
        "features.show-personal-income"                                       -> true,
        "features.show-hmrc-mtd-vat"                                          -> true,
        "features.show-kfc-mtd-it"                                            -> false,
        "features.show-kfc-personal-income"                                   -> false,
        "features.show-kfc-mtd-vat"                                           -> false,
        "features.enable-fast-track"                                          -> true,
        "features.redirect-to-confirm-personal-income"                        -> true,
        "features.redirect-to-confirm-mtd-it"                                 -> false,
        "features.redirect-to-confirm-mtd-vat"                                -> false,
        "microservice.services.agent-subscription-frontend.external-url"      -> "someSubscriptionExternalUrl",
        "microservice.services.agent-client-management-frontend.external-url" -> "someAgentClientManagementFrontendExternalUrl"
      )
      .overrides(new TestGuiceModule)

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    testCurrentAuthorisationRequestCache.clear()
    testContinueUrlKeyStoreCache.clear()
  }

  private class TestGuiceModule extends AbstractModule {
    override def configure(): Unit = {
      bind(classOf[CurrentAuthorisationRequestCache]).toInstance(testCurrentAuthorisationRequestCache)
      bind(classOf[ContinueUrlCache]).toInstance(testContinueUrlKeyStoreCache)
      bind(classOf[AgentMultiAuthorisationJourneyStateCache]).toInstance(testAgentMultiAuthorisationJourneyStateCache)
    }
  }

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

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
    "display the check details page when known fact is not required and provided for VAT" in {
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
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("VAT registration number"))
      checkHtmlResultWithBodyText(result, validVrn.value)
    }

    "display alternate check details page when known fact is not required and not provided for VAT" in {
      val formData =
        CurrentAuthorisationRequest(business, serviceVAT, "vrn", validVrn.value, None, fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      val result = await(controller.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("report a client's VAT returns through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("VAT registration number"))
      checkHtmlResultWithBodyText(result, validVrn.value)
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
    }
  }

  "POST /agents/check-details" should {
    val request = FakeRequest()

    "redirect to confirm_invitation when YES is selected for VAT service" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)
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
      testAgentMultiAuthorisationJourneyStateCache.save(AgentMultiAuthorisationJourneyState("business", Set.empty))

      val result = await(
        controller.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "render confirm_invitations when client type is provided, known fact is not required and YES is selected for VAT service" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)
      givenInvitationCreationSucceeds(
        arn,
        personal,
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        "VRN")
      givenAgentReference(arn, "BBBBBBBB", "personal")
      val formData =
        CurrentAuthorisationRequest(personal, serviceVAT, "vrn", validVrn.value, None, fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      testAgentMultiAuthorisationJourneyStateCache.save(AgentMultiAuthorisationJourneyState("personal", Set.empty))

      val result = await(
        controller.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "redirect to client-type when client type is not provided and YES is selected for VAT service" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)
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
      testAgentMultiAuthorisationJourneyStateCache.save(AgentMultiAuthorisationJourneyState("personal", Set.empty))

      val result = await(
        controller.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/client-type")
    }

    "redirect to client-type when client type and known fact are not provided and YES is selected for VAT service" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)
      val formData =
        CurrentAuthorisationRequest(None, serviceVAT, "vrn", validVrn.value, None, fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      testAgentMultiAuthorisationJourneyStateCache.save(AgentMultiAuthorisationJourneyState("personal", Set.empty))

      val result = await(
        controller.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/client-type")
    }

    "redirect to client-type when not provided and then submit and redirect to complete" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)
      givenInvitationCreationSucceeds(
        arn,
        personal,
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        "VRN")
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 200)
      givenAgentReference(arn, "BBBBBBBB", "personal")

      val formData =
        CurrentAuthorisationRequest(None, serviceVAT, "vrn", validVrn.value, None, fromFastTrack)
      testCurrentAuthorisationRequestCache.save(formData)
      testAgentMultiAuthorisationJourneyStateCache.save(AgentMultiAuthorisationJourneyState("personal", Set.empty))

      val result = await(
        controller.submitCheckDetails(
          authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/client-type")

      val serviceForm = ClientTypeForm.form.fill("personal")

      val result2 = await(
        controller.submitClientType(
          authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value)))
      status(result2) shouldBe 303
      redirectLocation(result2) shouldBe Some("/invitations/agents/invitation-sent")
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
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)

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
      testAgentMultiAuthorisationJourneyStateCache.save(AgentMultiAuthorisationJourneyState("business", Set.empty))
      val result = await(controller.submitKnownFact(authorisedAsValidAgent(requestWithForm, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }
  }

}
