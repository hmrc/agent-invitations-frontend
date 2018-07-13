package uk.gov.hmrc.agentinvitationsfrontend.controllers

import com.google.inject.AbstractModule
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import uk.gov.hmrc.agentinvitationsfrontend.services.{ContinueUrlStoreService, FastTrackCache}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId
import uk.gov.hmrc.play.binders.ContinueUrl

import scala.concurrent.ExecutionContext.Implicits.global

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
        "microservice.services.agent-client-management-frontend.external-url" -> "someAgentClientManagementFrontendExternalUrl"
      )
      .overrides(new TestGuiceModule)

  private class TestGuiceModule extends AbstractModule {
    override def configure(): Unit = {
      bind(classOf[FastTrackCache]).toInstance(testFastTrackCache)
      bind(classOf[ContinueUrlStoreService]).toInstance(continueUrlKeyStoreCache)
    }
  }

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  lazy val requestTrackingController: AgentsRequestTrackingController = app.injector.instanceOf[AgentsRequestTrackingController]
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
    continueUrlKeyStoreCache.clear()
  }

  "GET /agents/invitation-sent" should {
    val request = FakeRequest("GET", "/agents/invitation-sent")
    val invitationSent = controller.invitationSent()
    "return 200 with the only option to continue where user left off" in {
      val continueUrl = ContinueUrl("/someITSA/Url")
      continueUrlKeyStoreCache.cacheContinueUrl(continueUrl)
      val result = invitationSent(
        authorisedAsValidAgent(
          request.withSession("invitationId" -> invitationIdITSA.value, "deadline" -> "27 December 2017"),
          arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(s"$wireMockBaseUrlAsString${routes.ClientsInvitationController.start(invitationIdITSA)}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkHtmlResultWithBodyText(result, routes.AgentsInvitationController.continueAfterInvitationSent().url)
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueJourney.button"))
      await(bodyOf(result)) should not include hasMessage("invitation-sent.trackRequests.button")
      await(bodyOf(result)) should not include hasMessage("invitation-sent.continueToASAccount.button")
      await(bodyOf(result)) should not include hasMessage("invitation-sent.startNewAuthRequest")

      verifyAuthoriseAttempt()
      await(continueUrlKeyStoreCache.fetchContinueUrl).get shouldBe continueUrl
    }

    "return 200 with two options; agent-services-account and a link to create new invitaiton" in {
      val result = invitationSent(
        authorisedAsValidAgent(
          request.withSession(
            "invitationId" -> invitationIdPIR.value,
            "deadline"     -> "27 December 2017",
            "sessionId"    -> "session12345"),
          arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(s"$wireMockBaseUrlAsString${routes.ClientsInvitationController.start(invitationIdPIR)}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkHtmlResultWithBodyText(result, routes.AgentsInvitationController.continueAfterInvitationSent().url)
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.startNewAuthRequest"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueToASAccount.button"))
      await(bodyOf(result)) should not include hasMessage("invitation-sent.trackRequests.button")
      verifyAuthoriseAttempt()
      await(continueUrlKeyStoreCache.fetchContinueUrl) shouldBe None
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
