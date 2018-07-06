package uk.gov.hmrc.agentinvitationsfrontend.controllers

import com.google.inject.AbstractModule
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AgentInvitationEvent
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.{agentFastTrackForm, agentInvitationServiceForm}
import uk.gov.hmrc.agentinvitationsfrontend.models.{CurrentInvitationInput, UserInputNinoAndPostcode, UserInputVrnAndRegDate}
import uk.gov.hmrc.agentinvitationsfrontend.services.{ContinueUrlStoreService, FastTrackCache}
import uk.gov.hmrc.agentinvitationsfrontend.support.{BaseISpec, TestDataCommonSupport}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationControllerKFCFlagsOppositeISpec extends BaseISpec with TestDataCommonSupport {

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
        "features.show-kfc-personal-income"                                   -> true,
        "features.show-kfc-mtd-vat"                                           -> false,
        "features.enable-fast-track"                                          -> true,
        "microservice.services.agent-subscription-frontend.external-url"      -> "someSubscriptionExternalUrl",
        "microservice.services.agent-client-management-frontend.external-url" -> "someAgentClientManagementFrontendExternalUrl"
      )
      .overrides(new TestGuiceModule)

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    testFastTrackCache.clear()
    continueUrlKeyStoreCache.clear()
  }

  private class TestGuiceModule extends AbstractModule {
    override def configure(): Unit = {
      bind(classOf[FastTrackCache]).toInstance(testFastTrackCache)
      bind(classOf[ContinueUrlStoreService]).toInstance(continueUrlKeyStoreCache)
    }
  }

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  "GET /agents/identify-client" when {
    val request = FakeRequest("GET", "/agents/identify-client")

    "not show a postcode entry field if service is ITSA" in {
      testFastTrackCache.save(CurrentInvitationInput(serviceITSA))

      val form =
        controller.agentInvitationIdentifyClientFormItsa.fill(UserInputNinoAndPostcode(serviceITSA, None, None))
      val resultFuture = controller.showIdentifyClientForm(authorisedAsValidAgent(request, arn.value))

      status(resultFuture) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        resultFuture,
        "identify-client.nino.header",
        "identify-client.nino.label",
        "identify-client.nino.hint")

      val result = await(resultFuture)
      bodyOf(result) should not include htmlEscapedMessage("identify-client.postcode.label")
      bodyOf(result) should not include htmlEscapedMessage("identify-client.postcode.hint")
    }

    "not show a vat registration date entry field if service is VAT" in {
      testFastTrackCache.save(CurrentInvitationInput(serviceVAT))

      val form = controller.agentInvitationIdentifyClientFormVat.fill(UserInputVrnAndRegDate(serviceVAT, None, None))
      val resultFuture = controller.showIdentifyClientForm(authorisedAsValidAgent(request, arn.value))

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
  }

  "POST /agents/identify-client" should {
    val request = FakeRequest("POST", "/agents/identify-client")
    val submitIdentifyClient = controller.submitIdentifyClient()

    "return 303 invitation-sent for ITSA" in {
      givenTradingName(validNino, "64 Bit")
      val formData =
        CurrentInvitationInput(Some(serviceITSA), None, None, None, None, fromManual)
      testFastTrackCache.save(formData)
      val form =
        controller.agentInvitationIdentifyClientFormItsa.fill(
          UserInputNinoAndPostcode(serviceITSA, Some(validNino.nino), None))
      createInvitationStubForNoKnownFacts(
        arn,
        validNino.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        serviceITSA,
        "NI")
      givenCitizenDetailsAreKnownFor(validNino.value, "64", "Bit")
      getInvitationStub(arn, validNino.value, invitationIdITSA, serviceITSA, "NI", "Pending")

      val result = submitIdentifyClient(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(form.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "return 303 invitation-sent for IRV" in {
      givenCitizenDetailsAreKnownFor(validNino.value, "64", "Bit")
      val formData =
        CurrentInvitationInput(Some(servicePIR), None, None, None, None, fromManual)
      testFastTrackCache.save(formData)
      val form =
        controller.agentInvitationIdentifyClientFormIrv.fill(
          UserInputNinoAndPostcode(servicePIR, Some(validNino.nino), None))
      createInvitationStubForNoKnownFacts(
        arn,
        validNino.value,
        invitationIdPIR,
        validNino.value,
        "ni",
        servicePIR,
        "NI")
      getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, "NI", "Pending")

      val result = submitIdentifyClient(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(form.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "return 303 invitation-sent for VAT" in {
      givenClientDetails(validVrn)
      val formData =
        CurrentInvitationInput(Some(serviceVAT), None, None, None, None, fromManual)
      testFastTrackCache.save(formData)
      val form =
        controller.agentInvitationIdentifyClientFormVat.fill(
          UserInputVrnAndRegDate(serviceVAT, Some(validVrn.value), None))
      createInvitationStubForNoKnownFacts(
        arn,
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        identifierVAT)
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")

      val result = submitIdentifyClient(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(form.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

  }

  "POST /agents/fast-track" should {
    val request = FakeRequest("POST", "/agents/fast-track")
    val fastTrack = controller.agentFastTrack()

    "return 303 invitation-sent when service and valid nino are provided and kfc flag is off for ITSA service" in {

      val formData =
        CurrentInvitationInput(Some(serviceITSA), Some("ni"), Some(validNino.value), None, None, fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      createInvitationStubForNoKnownFacts(
        arn,
        validNino.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        serviceITSA,
        "NI")
      givenCitizenDetailsAreKnownFor(validNino.value, "64", "Bit")
      getInvitationStub(arn, validNino.value, invitationIdITSA, serviceITSA, "NI", "Pending")
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.invitationSent().url
    }

    "return 303 invitation-sent when service and valid vrn are provided and kfc flag is true for VAT service" in {
      val formData =
        CurrentInvitationInput(Some(serviceVAT), Some("vrn"), Some(validVrn.value), None, None, fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      createInvitationStubForNoKnownFacts(
        arn,
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        identifierVAT)
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.invitationSent().url
    }

    "return 303 invitation-sent if service calling fast-track is correct for IRV and kfc flag is on" in {
      val formData =
        CurrentInvitationInput(Some(servicePIR), Some("ni"), Some(validNino.value), None, None, fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      givenCitizenDetailsAreKnownFor(validNino.value, "64", "Bit")
      createInvitationStubForNoKnownFacts(
        arn,
        validNino.value,
        invitationIdPIR,
        validNino.value,
        "ni",
        servicePIR,
        "NI")
      getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, "NI", "Pending")
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.invitationSent().url
    }

  }

  def verifyAgentClientInvitationSubmittedEvent(
    arn: String,
    clientId: String,
    clientIdType: String,
    result: String,
    service: String): Unit =
    verifyAuditRequestSent(
      1,
      AgentInvitationEvent.AgentClientAuthorisationRequestCreated,
      detail = Map(
        "factCheck"            -> result,
        "agentReferenceNumber" -> arn,
        "clientIdType"         -> clientIdType,
        "clientId"             -> clientId,
        "service"              -> service
      ),
      tags = Map(
        "transactionName" -> "Agent client service authorisation request created"
      )
    )

}
