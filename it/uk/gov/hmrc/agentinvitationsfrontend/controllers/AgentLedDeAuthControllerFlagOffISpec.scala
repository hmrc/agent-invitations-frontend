package uk.gov.hmrc.agentinvitationsfrontend.controllers

import com.google.inject.AbstractModule
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.agentConfirmationForm
import uk.gov.hmrc.agentinvitationsfrontend.models.Confirmation
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec

class AgentLedDeAuthControllerFlagOffISpec extends BaseISpec with AuthBehaviours {

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
        "features.show-agent-led-de-auth"                                     -> false,
        "microservice.services.agent-subscription-frontend.external-url"      -> "someSubscriptionExternalUrl",
        "microservice.services.agent-client-management-frontend.external-url" -> "someAgentClientManagementFrontendExternalUrl",
        "mongodb.uri" -> s"$mongoUri"
      )
      .overrides(new TestGuiceModule)

  private class TestGuiceModule extends AbstractModule {
    override def configure(): Unit = {
    }
  }

  lazy val controller: AgentLedDeAuthController = app.injector.instanceOf[AgentLedDeAuthController]

  "GET /cancel-authorisation/client-type" should {

    "return 401 when flag is off" in {
      val request = FakeRequest("GET", "/agents/cancel-authorisation/client-type")
      val selectClientType = controller.showClientType()

      val result = selectClientType(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 501
    }
  }

  "POST /cancel-authorisation/client-type" should {

    "return 401 when flag is off" in {
      val request = FakeRequest("POST", "/agents/cancel-authorisation/client-type")
      val submitClientType = controller.submitClientType()

      val result = submitClientType(authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> "personal"), arn.value))
      status(result) shouldBe 501
    }
  }

  "GET /cancel-authorisation/select-service" should {

    "return 401 when flag is off" in {
      val request = FakeRequest("GET", "/agents/cancel-authorisation/select-service")
      val showSelectService = controller.showSelectService()

      val result = showSelectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 501
    }
  }

  "POST /cancel-authorisation/select-service" should {

    "return 401 when flag is off" in {
      val request = FakeRequest("POST", "/agents/cancel-authorisation/select-personal-service")
      val submitSelectService = controller.submitSelectPersonalService()

      val result = submitSelectService(authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> "HMRC-MTD-IT"), arn.value))
      status(result) shouldBe 501
    }
  }

  "GET /agents/cancel-authorisation/identify-client" should {

    "return 401 when flag is off"  in {
      val request = FakeRequest("GET", "/agents/cancel-authorisation/identify-client")
      val showIdentifyClient = controller.showIdentifyClient()

      val result = showIdentifyClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 501
    }
  }

  "POST /agents/cancel-authorisation/identify-client" when {

    "return 401 when flag is off" in {
        val request = FakeRequest("POST", "/agents/cancel-authorisation/identify-client")
        val submitIdentifyClient = controller.submitIdentifyClient()

        val requestWithForm =
          request.withFormUrlEncodedBody(
            "clientIdentifier" -> validNino.value,
            "dob.year"   -> "1980",
            "dob.month"  -> "07",
            "dob.day"    -> "07"
          )

        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 501
      }
  }

  "GET  /agents/cancel-authorisation/confirm-client" should {

    "return 401 when flag is off" in {
      val request = FakeRequest("GET", "/agents/cancel-authorisation/confirm-client")
      val showConfirmClient = controller.showConfirmClient()

      val result = showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 501
    }
  }

  "POST  /agents/cancel-authorisation/confirm-client" when {

      "return 401 when flag is off" in {
        val request = FakeRequest("POST", "/agents/cancel-authorisation/confirm-client")
        val submitConfirmClient = controller.submitConfirmClient()

        val choice = agentConfirmationForm("error message").fill(Confirmation(true))
        val requestWithForm = request.withFormUrlEncodedBody(choice.data.toSeq: _*)

        val result = submitConfirmClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 501
      }
  }

  "GET  /agents/cancel-authorisation/confirm-cancel" should {

    "return 401 when flag is off" in {
      val request = FakeRequest("GET", "/agents/cancel-authorisation/confirm-cancel")
      val showConfirmCancel = controller.showConfirmCancel()

      val result = showConfirmCancel(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 501
    }
  }

  "POST  /agents/cancel-authorisation/confirm-cancel" when {

    "return 401 when flag is off" in {
        val request = FakeRequest("POST", "/agents/cancel-authorisation/confirm-cancel")
        val submitConfirmCancel = controller.submitConfirmCancel()

        val choice = agentConfirmationForm("error message").fill(Confirmation(true))
        val requestWithForm = request.withFormUrlEncodedBody(choice.data.toSeq: _*)

        val result = submitConfirmCancel(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 501
      }
  }

  "GET  /agents/cancel-authorisation/cancelled" should {

    "return 401 when flag is off" in {
      val request = FakeRequest("GET", "/agents/cancel-authorisation/cancelled")
      val showCancelled = controller.showCancelled()

      val result = showCancelled(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 501
    }
  }

}
