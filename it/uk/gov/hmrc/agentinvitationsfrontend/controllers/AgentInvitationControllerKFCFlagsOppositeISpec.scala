package uk.gov.hmrc.agentinvitationsfrontend.controllers

import com.google.inject.AbstractModule
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AgentInvitationEvent
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.{agentFastTrackForm, agentInvitationIdentifyClientFormIrv}
import uk.gov.hmrc.agentinvitationsfrontend.models.{CurrentInvitationInput, UserInputNinoAndPostcode, UserInputVrnAndRegDate}
import uk.gov.hmrc.agentinvitationsfrontend.services.{ContinueUrlStoreService, FastTrackCache}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationControllerKFCFlagsOppositeISpec extends BaseISpec {

  override protected def appBuilder: GuiceApplicationBuilder = {
    new GuiceApplicationBuilder()
      .configure(
        "microservice.services.auth.port" -> wireMockPort,
        "microservice.services.agent-client-authorisation.port" -> wireMockPort,
        "microservice.services.agent-services-account.port" -> wireMockPort,
        "microservice.services.company-auth.login-url" -> wireMockHost,
        "microservice.services.company-auth.port" -> wireMockPort,
        "microservice.services.des.port" -> wireMockPort,
        "microservice.services.agent-fi-relationship.port" -> wireMockPort,
        "microservice.services.agent-invitations-frontend.external-url" -> wireMockBaseUrlAsString,
        "microservice.services.agent-services-account-frontend.external-url" -> wireMockBaseUrlAsString,
        "microservice.services.company-auth-frontend.external-url" -> companyAuthUrl,
        "microservice.services.company-auth-frontend.sign-out.path" -> companyAuthSignOutPath,
        "microservice.services.business-tax-account.external-url" -> businessTaxAccountUrl,
        "microservice.services.tax-account-router-frontend.account-url" -> taxAccountRelativeUrl,
        "microservice.services.personal-tax-account.external-url" -> personalTaxAccountUrl,
        "auditing.enabled" -> true,
        "auditing.consumer.baseUri.host" -> wireMockHost,
        "auditing.consumer.baseUri.port" -> wireMockPort,
        "features.show-hmrc-mtd-it" -> true,
        "features.show-personal-income" -> true,
        "features.show-hmrc-mtd-vat" -> true,
        "features.show-kfc-mtd-it" -> false,
        "features.show-kfc-personal-income" -> true,
        "features.show-kfc-mtd-vat" -> false,
        "features.enable-fast-track" -> true,
        "microservice.services.agent-subscription-frontend.external-url" -> "someSubscriptionExternalUrl",
        "microservice.services.agent-client-management-frontend.external-url" -> "someAgentClientManagementFrontendExternalUrl"
      ).overrides(new TestGuiceModule)
  }

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
  val arn = Arn("TARN0000001")
  val mtdItId = MtdItId("ABCDEF123456789")
  private val validNino = Nino("AB123456A")
  private val validNinoSpace = Nino("AB 12 34 56 A")
  val serviceITSA = "HMRC-MTD-IT"
  val servicePIR = "PERSONAL-INCOME-RECORD"
  val validPostcode = "BN12 6BX"
  val invitationIdITSA = InvitationId("ABERULMHCKKW3")
  val invitationIdPIR = InvitationId("B9SCS2T4NZBAX")

  val invitationIdVAT = InvitationId("CZTW1KY6RTAAT")
  val serviceVAT = "HMRC-MTD-VAT"
  val identifierVAT = "VRN"
  val validVrn97 = Vrn("101747696")
  val validRegDateForVrn97 = Some("2007-07-07")
  val validVrn9755 = Vrn("101747641")

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  "GET /agents/identify-client" when {
    val request = FakeRequest("GET", "/agents/identify-client")

    "not show a postcode entry field if service is ITSA" in {
      testFastTrackCache.save(CurrentInvitationInput(serviceITSA))

      val form = controller.agentInvitationIdentifyClientFormItsa.fill(UserInputNinoAndPostcode(serviceITSA, None, None))
      val resultFuture = controller.showIdentifyClientForm(authorisedAsValidAgent(request, arn.value))

      status(resultFuture) shouldBe 200
      checkHtmlResultWithBodyMsgs(resultFuture,
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
      checkHtmlResultWithBodyMsgs(resultFuture,
        "identify-client.vrn.header",
        "identify-client.vrn.label",
        "identify-client.vrn.hint")

      val result = await(resultFuture)
      bodyOf(result) should not include htmlEscapedMessage("identify-client.vat-registration-date.label")
      bodyOf(result) should not include htmlEscapedMessage("identify-client.vat-registration-date.hint")
    }
  }

  "POST /agents/fast-track" should {
    val request = FakeRequest("POST", "/agents/fast-track")
    val fastTrack = controller.agentFastTrack()

    "return 303 invitation-sent when service and valid nino are provided and kfc flag is off for ITSA service" in {

      val formData = CurrentInvitationInput(Some(serviceITSA), Some("ni"), Some(validNino.value), None, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      createInvitationStubForNoKnownFacts(arn, validNino.value, invitationIdITSA, validNino.value, "ni", serviceITSA, "NI")
      getInvitationStub(arn, validNino.value, invitationIdITSA, serviceITSA, "NI", "Pending")
      val result = fastTrack(authorisedAsValidAgent(request, arn.value)
        .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.invitationSent().url
    }

    "return 303 invitation-sent when service and valid vrn are provided and kfc flag is off for VAT service" in {
      val formData = CurrentInvitationInput(Some(serviceVAT), Some("vrn"), Some(validVrn97.value), None, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      createInvitationStubForNoKnownFacts(arn, validVrn97.value, invitationIdVAT, validVrn97.value, "vrn", serviceVAT, identifierVAT)
      getInvitationStub(arn, validVrn97.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      val result = fastTrack(authorisedAsValidAgent(request, arn.value)
        .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.invitationSent().url
    }

    "throw an exception when service and valid nino are provided and kfc flag is on for PIR service" in {
      val formData = CurrentInvitationInput(Some(servicePIR), Some("ni"), Some(validNino.value), None, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(authorisedAsValidAgent(request, arn.value)
        .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))
      an[Exception] shouldBe thrownBy(
        await(fastTrack(authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))))
    }
  }

    def verifyAgentClientInvitationSubmittedEvent(arn: String, clientId: String, clientIdType: String, result: String, service: String): Unit = {
      verifyAuditRequestSent(1, AgentInvitationEvent.AgentClientAuthorisationRequestCreated,
        detail = Map(
          "factCheck" -> result,
          "agentReferenceNumber" -> arn,
          "clientIdType" -> clientIdType,
          "clientId" -> clientId,
          "service" -> service
        ),
        tags = Map(
          "transactionName" -> "Agent client service authorisation request created"
        )
      )
    }

}
