package uk.gov.hmrc.agentinvitationsfrontend.controllers

import com.google.inject.AbstractModule
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import uk.gov.hmrc.agentinvitationsfrontend.models.CurrentInvitationInput
import uk.gov.hmrc.agentinvitationsfrontend.services.{FastTrackCache, FastTrackKeyStoreCache}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.{agentFastTrackForm, agentInvitationIdentifyClientFormIrv}
import play.api.test.Helpers._

class AgentInvitationControllerFastTrackOffISpec extends BaseISpec {

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
        "features.show-kfc-personal-income" -> false,
        "features.show-kfc-mtd-vat" -> false,
        "features.enable-fast-track" -> false,
        "microservice.services.agent-subscription-frontend.external-url" -> "someSubscriptionExternalUrl",
        "microservice.services.agent-client-management-frontend.external-url" -> "someAgentClientManagementFrontendExternalUrl"
      ).overrides(new TestGuiceModule)
  }

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  val arn = Arn("TARN0000001")
  val mtdItId = MtdItId("ABCDEF123456789")
  private val validNino = Nino("AB123456A")
  val serviceITSA = "HMRC-MTD-IT"
  val servicePIR = "PERSONAL-INCOME-RECORD"
  val validPostcode = "DH14EJ"
  val invitationIdITSA = InvitationId("ABERULMHCKKW3")
  val invitationIdPIR = InvitationId("B9SCS2T4NZBAX")

  val invitationIdVAT = InvitationId("CZTW1KY6RTAAT")
  val serviceVAT = "HMRC-MTD-VAT"
  val identifierVAT = "VRN"
  val validVrn97 = Vrn("101747696")
  val invalidVrn = Vrn("101747692")
  val validRegDateForVrn97 = Some("2007-07-07")
  val validVrn9755 = Vrn("101747641")

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    testFastTrackCache.clear()
  }

  private class TestGuiceModule extends AbstractModule {
    override def configure(): Unit = {
      bind(classOf[FastTrackCache]).toInstance(testFastTrackCache)
    }
  }

  "Show Fast Track flag is switched off" should {
    val request = FakeRequest("POST", "/agents/fast-track")
    val fastTrack = controller.agentFastTrack()

    "through fast-track, return 400 and prevent agents" when {

      "creating an ITSA invitation" in {
        val formData = CurrentInvitationInput(Some(serviceITSA), Some("ni"), Some(validNino.value), Some(validPostcode), None)
        val fastTrackFormData = agentFastTrackForm.fill(formData)
        val result = fastTrack(authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

        status(result) shouldBe BAD_REQUEST
      }

      "creating an IRV invitation" in {
        val formData = CurrentInvitationInput(Some(servicePIR), Some("ni"), Some(validNino.value), None, None)
        val fastTrackFormData = agentFastTrackForm.fill(formData)
        val result = fastTrack(authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

        status(result) shouldBe BAD_REQUEST
      }

      "creating an VAT invitation" in {
        val formData = CurrentInvitationInput(Some(serviceVAT), Some("vrn"), Some(validVrn97.value), None, validRegDateForVrn97)
        val fastTrackFormData = agentFastTrackForm.fill(formData)
        val result = fastTrack(authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

        status(result) shouldBe BAD_REQUEST
      }
    }
  }

}
