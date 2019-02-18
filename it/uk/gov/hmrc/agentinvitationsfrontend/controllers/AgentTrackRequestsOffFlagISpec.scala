package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.util.UUID

import com.google.inject.AbstractModule
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentSession
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.personal
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.binders.ContinueUrl

class AgentTrackRequestsOffFlagISpec extends BaseISpec {

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
        "features.show-hmrc-mtd-it"                                           -> true,
        "features.show-personal-income"                                       -> true,
        "features.show-hmrc-mtd-vat"                                          -> true,
        "features.show-kfc-mtd-it"                                            -> false,
        "features.show-kfc-personal-income"                                   -> false,
        "features.show-kfc-mtd-vat"                                           -> false,
        "features.enable-fast-track"                                          -> true,
        "features.enable-track-requests"                                      -> false,
        "microservice.services.agent-subscription-frontend.external-url"      -> "someSubscriptionExternalUrl",
        "microservice.services.agent-client-management-frontend.external-url" -> "someAgentClientManagementFrontendExternalUrl",
        "mongodb.uri" -> s"$mongoUri"
      )
      .overrides(new TestGuiceModule)

  private class TestGuiceModule extends AbstractModule {
    override def configure(): Unit = {
    }
  }

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  lazy val requestTrackingController: AgentsRequestTrackingController =
    app.injector.instanceOf[AgentsRequestTrackingController]

  "GET /agents/invitation-sent" should {
    val request = FakeRequest("GET", "/agents/invitation-sent")
    val invitationSent = controller.showInvitationSent()
    "return 200 with the only option to continue where user left off" in {
      givenAgentReference(arn, uid, personal)
      val continueUrl = ContinueUrl("/someITSA/Url")
      val sessionId = UUID.randomUUID().toString
      implicit val hc: HeaderCarrier = headerCarrier(sessionId)
      await(sessionStore.save(
        AgentSession( Some(personal), Some(serviceITSA), Some("ni"), Some(nino), Some(validPostcode), continueUrl = Some(continueUrl.url), clientTypeForInvitationSent =  Some(personal))))
      val result = invitationSent(authorisedAsValidAgent(request, arn.value, sessionId))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          s"$wireMockBaseUrlAsString${routes.ClientsMultiInvitationController.warmUp("personal", uid, "99-with-flake")}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkHtmlResultWithBodyText(result, routes.AgentsInvitationController.continueAfterInvitationSent().url)
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueJourney.button"))
      await(bodyOf(result)) should not include hasMessage("invitation-sent.trackRequests.button")
      await(bodyOf(result)) should not include hasMessage("invitation-sent.continueToASAccount.button")
      await(bodyOf(result)) should not include hasMessage("invitation-sent.startNewAuthRequest")

      verifyAuthoriseAttempt()
      await(sessionStore.hardGet) shouldBe AgentSession(continueUrl = Some(continueUrl.url), clientTypeForInvitationSent =  Some(personal))
    }

    "return 200 with two options; agent-services-account and a link to create new invitation" in {
      givenAgentReference(arn, uid, personal)
      val sessionId = UUID.randomUUID().toString
      implicit val hc: HeaderCarrier = headerCarrier(sessionId)
      await(sessionStore.save(
        AgentSession( Some(personal), Some(serviceITSA), Some("ni"), Some(nino), Some(validPostcode), clientTypeForInvitationSent =  Some(personal))))
      val result = invitationSent(authorisedAsValidAgent(request, arn.value, sessionId))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          s"$wireMockBaseUrlAsString${routes.ClientsMultiInvitationController.warmUp("personal", uid, "99-with-flake")}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkHtmlResultWithBodyText(result, routes.AgentsInvitationController.continueAfterInvitationSent().url)
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.startNewAuthRequest"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueToASAccount.button"))
      await(bodyOf(result)) should not include hasMessage("invitation-sent.trackRequests.button")
      verifyAuthoriseAttempt()
      await(sessionStore.hardGet) shouldBe AgentSession(clientTypeForInvitationSent =  Some(personal))
    }

  }

  "GET /track" should {

    val request = FakeRequest("GET", "/track/")

    "return a bad request when the enable-track-requests flag is off" in {
      val result = requestTrackingController.showTrackRequests(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 400
    }
  }

}
