package uk.gov.hmrc.agentinvitationsfrontend.controllers

import com.google.inject.AbstractModule
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsFastTrackInvitationController.agentFastTrackForm
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentFastTrackRequest
import uk.gov.hmrc.agentinvitationsfrontend.services.AgentSessionCache
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

class AgentInvitationsControllerShowFlagsOffISpec extends BaseISpec {

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
        "features.show-hmrc-mtd-it"                                           -> false,
        "features.show-personal-income"                                       -> false,
        "features.show-hmrc-mtd-vat"                                          -> false,
        "features.show-kfc-mtd-it"                                            -> false,
        "features.show-kfc-personal-income"                                   -> true,
        "features.show-kfc-mtd-vat"                                           -> false,
        "features.enable-fast-track"                                          -> true,
        "microservice.services.agent-subscription-frontend.external-url"      -> "someSubscriptionExternalUrl",
        "microservice.services.agent-client-management-frontend.external-url" -> "someAgentClientManagementFrontendExternalUrl"
      )
      .overrides(new TestGuiceModule)

  lazy val fastTrackController: AgentsFastTrackInvitationController = app.injector.instanceOf[AgentsFastTrackInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    testAgentSessionCache.clear()
  }

  private class TestGuiceModule extends AbstractModule {
    override def configure(): Unit =
      bind(classOf[AgentSessionCache]).toInstance(testAgentSessionCache)
  }

  "Show Feature Flags are switched off" should {
    val request = FakeRequest("POST", "/agents/fast-track")
    val fastTrack = fastTrackController.agentFastTrack()

    "through fast-track, return 400 and prevent agents" when {

      "creating an ITSA invitation" in {
        val formData =
          AgentFastTrackRequest(Some("personal"), serviceITSA, "ni", nino, Some(validPostcode))
        val fastTrackFormData = agentFastTrackForm.fill(formData)
        val result = fastTrack(
          authorisedAsValidAgent(request, arn.value)
            .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

        status(result) shouldBe BAD_REQUEST
      }

      "creating an IRV invitation" in {
        val formData = AgentFastTrackRequest(Some("personal"), servicePIR, "ni", nino, Some(validPostcode))
        val fastTrackFormData = agentFastTrackForm.fill(formData)
        val result = fastTrack(
          authorisedAsValidAgent(request, arn.value)
            .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

        status(result) shouldBe BAD_REQUEST
      }

      "creating an VAT invitation" in {
        val formData =
          AgentFastTrackRequest(business, serviceVAT, "vrn", validVrn.value, Some(validRegistrationDate))
        val fastTrackFormData = agentFastTrackForm.fill(formData)
        val result = fastTrack(
          authorisedAsValidAgent(request, arn.value)
            .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

        status(result) shouldBe BAD_REQUEST
      }
    }
  }
}
