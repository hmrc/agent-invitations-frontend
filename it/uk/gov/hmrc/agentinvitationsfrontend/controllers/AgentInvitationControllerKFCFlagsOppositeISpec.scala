package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.util.UUID

import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.{Action, AnyContent, AnyContentAsEmpty}
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsFastTrackInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.agentConfirmationForm
import uk.gov.hmrc.agentinvitationsfrontend.forms.{IrvClientForm, ItsaClientForm, VatClientForm}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentSession, _}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

class AgentInvitationControllerKFCFlagsOppositeISpec extends BaseISpec {

  override protected def appBuilder: GuiceApplicationBuilder =
    new GuiceApplicationBuilder()
      .configure(
        "microservice.services.auth.port"                                     -> wireMockPort,
        "microservice.services.agent-client-authorisation.port"               -> wireMockPort,
        "microservice.services.agent-services-account.port"                   -> wireMockPort,
        "microservice.services.company-auth.login-url"                        -> wireMockHost,
        "microservice.services.company-auth.port"                             -> wireMockPort,
        "microservice.services.des.port"                                      -> wireMockPort,
        "microservice.services.agent-fi-relationship.port"                    -> wireMockPort,
        "microservice.services.citizen-details.host"                          -> wireMockHost,
        "microservice.services.agent-client-relationships.port"               -> wireMockPort,
        "microservice.services.agent-client-relationships.host"               -> wireMockHost,
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
        "microservice.services.agent-client-management-frontend.external-url" -> "someAgentClientManagementFrontendExternalUrl",
        "mongodb.uri" -> s"$mongoUri"
      )

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  lazy val fastTrackController: AgentsFastTrackInvitationController =
    app.injector.instanceOf[AgentsFastTrackInvitationController]
  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId(UUID.randomUUID().toString)))

  "GET /agents/identify-client" when {
    val request = FakeRequest("GET", "/agents/identify-client")

    "not show a postcode entry field if service is ITSA" in {
      await(sessionStore.save(AgentSession(Some(personal), Some(serviceITSA))))

      val resultFuture = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(resultFuture) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        resultFuture,
        "identify-client.nino.header",
        "identify-client.nino.label",
        "identify-client.nino.hint")

      checkResultContainsBackLink(resultFuture, "/invitations/agents/select-service")

      val result = await(resultFuture)
      bodyOf(result) should not include htmlEscapedMessage("identify-client.postcode.label")
      bodyOf(result) should not include htmlEscapedMessage("identify-client.postcode.hint")
    }

    "not show a vat registration date entry field if service is VAT" in {
      await(sessionStore.save(AgentSession(Some(business), Some(serviceVAT))))

      val resultFuture = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(resultFuture) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        resultFuture,
        "identify-client.vrn.header",
        "identify-client.vrn.label",
        "identify-client.vrn.hint")

      val result = await(resultFuture)
      bodyOf(result) should not include htmlEscapedMessage("identify-client.vat-registration-date.label")
      bodyOf(result) should not include htmlEscapedMessage("identify-client.vat-registration-date.hint")
    }

    "not show a date of birth entry field if service is IRV" in {
      await(sessionStore.save(AgentSession(Some(business), Some(servicePIR))))

      val resultFuture = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(resultFuture) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        resultFuture,
        "identify-client.nino.header",
        "identify-client.itsa.p1",
        "identify-client.nino.hint")

      val result = await(resultFuture)
      bodyOf(result) should not include htmlEscapedMessage("identify-client.vat-registration-date.label")
      bodyOf(result) should not include htmlEscapedMessage("identify-client.vat-registration-date.hint")
    }
  }

  "POST /agents/identify-client" should {
    val request = FakeRequest("POST", "/agents/identify-client")
    val submitIdentifyClient = controller.submitIdentifyClient()

    "return 303 review-authorisation for ITSA" in {
      val formData = AgentSession(Some(personal), Some(serviceITSA), Some(""), Some(""), fromFastTrack = fromManual)
      await(sessionStore.save(formData))

      givenTradingName(validNino, "64 Bit")
      givenInvitationCreationSucceeds(
        arn,
        Some(personal),
        validNino.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        serviceITSA,
        "NI")
      givenAgentReference(arn, "ABCDEFGH", personal)
      givenCitizenDetailsAreKnownFor(validNino.value, "64", "Bit")
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, serviceITSA)
      givenCheckRelationshipItsaWithStatus(arn, validNino.value, 404)

      val form = ItsaClientForm.form(true).fill(ItsaClient(validNino.nino, None))

      val result = submitIdentifyClient(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(form.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showReviewAuthorisations().url
    }

    "return 303 already-authorisation-present when there is already a relationship between the agent and client" in {
      val formData = AgentSession(Some(personal), Some(serviceITSA), Some("ni"), Some(validNino.value), fromFastTrack = fromManual)
      await(sessionStore.save(formData))

      givenTradingName(validNino, "64 Bit")
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, serviceITSA)
      givenCheckRelationshipItsaWithStatus(arn, validNino.value, 200)

      val form = ItsaClientForm.form(true).fill(ItsaClient(validNino.nino, None))
      val result = submitIdentifyClient(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(form.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentsErrorController.activeRelationshipExists().url

    }

    "return 303 confirm-client for IRV" in {
      givenCitizenDetailsAreKnownFor(validNino.value, "64", "Bit")
      val formData = AgentSession(Some(personal), Some(servicePIR), Some(""), Some(""), fromFastTrack = fromManual)
      await(sessionStore.save(formData))
      val form = IrvClientForm.form(true).fill(IrvClient(validNino.nino, None))
      givenInvitationCreationSucceeds(
        arn,
        Some(personal),
        validNino.value,
        invitationIdPIR,
        validNino.value,
        "ni",
        servicePIR,
        identifierPIR)
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, servicePIR)

      val result = submitIdentifyClient(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(form.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showConfirmClient().url
    }

    "return 303 invitation-sent for VAT" in {
      val formData = AgentSession(Some(business), Some(serviceVAT), Some(""), Some(""), fromFastTrack = fromManual)
      await(sessionStore.save(formData))
      val form = VatClientForm.form(true).fill(VatClient(validVrn.value, None))
      givenInvitationCreationSucceeds(
        arn,
        Some(business),
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        identifierVAT)
      givenAgentReference(arn, "ABCDEFGH", business)
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)
      givenAgentReferenceRecordExistsForArn(arn, "uid")

      val result = submitIdentifyClient(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(form.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showInvitationSent().url
    }

    "return 303 already-authorisation-present when there is already a relationship for the agent and client" in {
      val formData = AgentSession(Some(business), Some(serviceVAT), Some(""), Some(""), fromFastTrack = fromManual)
      await(sessionStore.save(formData))
      val form = VatClientForm.form(true).fill(VatClient(validVrn.value, None))
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 200)

      val result = submitIdentifyClient(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(form.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentsErrorController.activeRelationshipExists().url
    }

  }

  "POST /agents/fast-track" should {
    val request = FakeRequest("POST", "/agents/fast-track")
    val fastTrack = fastTrackController.agentFastTrack()

    "return 303 check-details when service and valid nino are provided and kfc flag is off for ITSA service" in {

      val formData = AgentFastTrackRequest(Some(personal), serviceITSA, "ni", validNino.value, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      givenInvitationCreationSucceeds(
        arn,
        Some(personal),
        validNino.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        serviceITSA,
        "NI")
      givenCitizenDetailsAreKnownFor(validNino.value, "64", "Bit")

      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsFastTrackInvitationController.showCheckDetails().url
    }

    "return 303 check-details when service and valid vrn are provided and kfc flag is true for VAT service" in {
      val formData = AgentFastTrackRequest(Some(personal), serviceVAT, "vrn", validVrn.value, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      givenInvitationCreationSucceeds(
        arn,
        Some(business),
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        identifierVAT)

      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsFastTrackInvitationController.showCheckDetails().url
    }

    "return 303 check-details if service calling fast-track is correct for IRV and kfc flag is on" in {
      val formData = AgentFastTrackRequest(Some(personal), servicePIR, "ni", validNino.value, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      givenCitizenDetailsAreKnownFor(validNino.value, "64", "Bit")
      givenInvitationCreationSucceeds(
        arn,
        Some(personal),
        validNino.value,
        invitationIdPIR,
        validNino.value,
        "ni",
        servicePIR,
        identifierPIR)

      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsFastTrackInvitationController.showCheckDetails().url
    }

  }

  "GET /agents/check-details" should {

    val request = FakeRequest()

    "display the check details page without known fact when KFC flag is off for ITSA" in {
      val formData = AgentSession(Some(personal), Some(serviceITSA), Some("ni"), Some(validNino.value), Some(validPostcode), fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))

      val result = await(fastTrackController.showCheckDetails(authorisedAsValidAgent(request, arn.value)))

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("income and expenses through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("National Insurance number"))
      checkHtmlResultWithBodyText(result, validNinoSpace.value)
    }

    "display the check details page without known fact when KFC flag is off for IRV" in {
      val formData = AgentSession(Some(personal), Some(servicePIR), Some("ni"), Some(validNino.value), Some(dateOfBirth), fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))
      val result = await(fastTrackController.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("view a client's PAYE income record"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("National Insurance number"))
      checkHtmlResultWithBodyText(result, validNinoSpace.value)
    }

    "display the check details page without known fact when KFC flag is off for VAT" in {
      val formData = AgentSession(Some(business), Some(serviceVAT), Some("vrn"), Some(validVrn.value), Some(validRegistrationDate), fromFastTrack = fromFastTrack)
      await(sessionStore.save(formData))

      val result = await(fastTrackController.showCheckDetails(authorisedAsValidAgent(request, arn.value)))

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("report a client's VAT returns through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("VAT registration number"))
      checkHtmlResultWithBodyText(result, validVrn.value)
    }
  }

  "GET /confirm-client" should {
    val request = FakeRequest("GET", "/agents/confirm-client")
    val showConfirmClient = controller.showConfirmClient()

    "return 200 and show client name for PERSONAL-INCOME-RECORD" in {
      await(sessionStore.save(AgentSession(Some(personal), Some(servicePIR), Some("ni"), Some(validNino.value), Some(dateOfBirth), fromFastTrack = fromManual)))

      givenCitizenDetailsAreKnownFor(validNino.value, "64", "Bit")

      val result = showConfirmClient(authorisedAsValidAgent(request,    arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "64 Bit")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.yes")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.no")
    }

    "return 200 and no client name was found for PERSONAL-INCOME-RECORD" in {
      await(sessionStore.save(
        AgentSession(Some(personal), Some(servicePIR), Some("ni"), Some(validNino.value), Some(dateOfBirth), fromFastTrack = fromManual)))

      givenCitizenDetailsReturns404For(validNino.value)

      val result = showConfirmClient(authorisedAsValidAgent(request,    arn.value))
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

    "redirect to review-authorisation and create invitation for PERSONAL-INCOME-RECORD" in {
      await(sessionStore.save(
        AgentSession(Some(personal), Some(servicePIR), Some("ni"), Some(validNino.value), Some(dateOfBirth))))

      givenInvitationCreationSucceeds(
        arn,
        Some(personal),
        validNino.value,
        invitationIdPIR,
        validNino.value,
        "ni",
        servicePIR,
        "NI")
      givenAgentReference(arn, "ABCDEFGH", personal)
      givenCitizenDetailsAreKnownFor(validNino.value, "64", "Bit")
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, servicePIR)

      val choice = agentConfirmationForm("error-message").fill(Confirmation(true))
      val result =
        submitConfirmClient(authorisedAsValidAgent(request,    arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showReviewAuthorisations().url
      status(result) shouldBe 303
    }

    "return 200 for not selecting an option for PERSONAL-INCOME-RECORD" in {
      await(sessionStore.save(
        AgentSession(Some(personal), Some(servicePIR), Some("ni"), Some(validNino.value), Some(dateOfBirth))))

      givenCitizenDetailsAreKnownFor(validNino.value, "64", "Bit")

      val result = submitConfirmClient(authorisedAsValidAgent(request,    arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "64 Bit")
      checkHtmlResultWithBodyMsgs(result, "error.confirm-client.required")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.yes")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.no")
    }

    "redirect to already-authorisation-pending if there are already authorisations pending for this client" in {
      await(sessionStore.save(
        AgentSession(Some(personal), Some(servicePIR), Some("ni"), Some(validNino.value), Some(dateOfBirth))))
      givenGetAllPendingInvitationsReturnsSome(arn, validNino.value, servicePIR)

      val choice = agentConfirmationForm("error-message").fill(Confirmation(true))
      val result =
        submitConfirmClient(authorisedAsValidAgent(request,    arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.pendingAuthorisationExists().url
      status(result) shouldBe 303
    }

    "redirect to already-authorisation-pending if this authorisation is already in the basket" in {
      await(sessionStore.save(AgentSession(Some(personal), Some(servicePIR), Some("ni"), Some(validNino.value), Some(dateOfBirth), requests = Set(AuthorisationRequest( "clientName", PirInvitation(validNino, Some(DOB(dateOfBirth))), "itemId")))))
      givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, servicePIR)

      val choice = agentConfirmationForm("error-message").fill(Confirmation(true))
      val result =
        submitConfirmClient(authorisedAsValidAgent(request,    arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.pendingAuthorisationExists().url
      status(result) shouldBe 303
    }
  }

  def behaveLikeMissingCacheScenarios(action: Action[AnyContent], request: FakeRequest[AnyContentAsEmpty.type]) = {
    "return to identify-client no client identifier found in cache" in {
      await(sessionStore.save(AgentSession(Some(personal), Some(servicePIR))))
      val result = action(authorisedAsValidAgent(request,    arn.value))
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
