package uk.gov.hmrc.agentinvitationsfrontend.controllers

import com.google.inject.AbstractModule
import play.api.inject.guice.GuiceApplicationBuilder
import uk.gov.hmrc.agentinvitationsfrontend.services.FastTrackKeyStoreCache
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec

class AgentInvitationsControllerShowFlagsOffISpec extends BaseISpec {

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
        "features.show-hmrc-mtd-it" -> false,
        "features.show-personal-income" -> false,
        "features.show-hmrc-mtd-vat" -> false,
        "features.show-kfc-mtd-it" -> false,
        "features.show-kfc-personal-income" -> true,
        "features.show-kfc-mtd-vat" -> false,
        "microservice.services.agent-subscription-frontend.external-url" -> "someSubscriptionExternalUrl",
        "microservice.services.agent-client-management-frontend.external-url" -> "someAgentClientManagementFrontendExternalUrl"
      ).overrides(new TestGuiceModule)
  }

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    fastTrackKeyStoreCache.clear()
  }

  private class TestGuiceModule extends AbstractModule {
    override def configure(): Unit = {
      bind(classOf[FastTrackKeyStoreCache]).toInstance(fastTrackKeyStoreCache)
    }
  }


  "ITSA FeatureFlag turned off" should {
    "return 400 when going through fast-track" in {

    }

    "return 400 when going through manually" in {

    }

  }

  "IRV FeatureFlag turned off" should {
    "return 400 when going through fast-track" in {

    }

    "return 400 when going through manually" in {

    }

  }

  "VAT FeatureFlag Turned off" should {
    "return 400 when going through fast-track" in {

    }

    "return 400 when going through manually" in {

    }

  }

}
