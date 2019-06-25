package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.util.UUID

import com.google.inject.AbstractModule
import play.api.Application
import play.api.test.FakeRequest
import uk.gov.hmrc.agentinvitationsfrontend.controllers.retired.AgentsInvitationController
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentSession
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.personal
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId
import uk.gov.hmrc.play.bootstrap.binders.{RedirectUrl, UnsafePermitAll}
import uk.gov.hmrc.play.bootstrap.binders.RedirectUrl._

class AgentTrackRequestsOffFlagISpec extends BaseISpec {

  override implicit lazy val app: Application = appBuilder(oppositeFeatureFlags)
    .build()

  private class TestGuiceModule extends AbstractModule {
    override def configure(): Unit = {}
  }

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId(UUID.randomUUID().toString)))
  lazy val requestTrackingController: AgentsRequestTrackingController =
    app.injector.instanceOf[AgentsRequestTrackingController]
  private val policy = UnsafePermitAll

  "GET /agents/invitation-sent" should {
    val request = FakeRequest("GET", "/agents/invitation-sent")
    val invitationSent = controller.showInvitationSent()
    "return 200 with the only option to continue where user left off" in {
      givenAgentReference(arn, uid, personal)
      val redirectUrl = RedirectUrl("/someITSA/Url")
      givenGetAgencyEmailAgentStub
      await(
        sessionStore.save(AgentSession(
          Some(personal),
          Some(serviceITSA),
          Some("ni"),
          Some(nino),
          Some(validPostcode),
          continueUrl = Some(redirectUrl.get(policy).url),
          clientTypeForInvitationSent = Some(personal)
        )))
      val result = invitationSent(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          s"$wireMockBaseUrlAsString${routes.ClientInvitationJourneyController.warmUp("personal", uid, "99-with-flake")}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueJourney.button"))
      await(bodyOf(result)) should not include hasMessage("invitation-sent.continueToASAccount.button")
      await(bodyOf(result)) should not include hasMessage("invitation-sent.startNewAuthRequest")

      verifyAuthoriseAttempt()
      await(sessionStore.hardGet) shouldBe AgentSession(
        continueUrl = Some(redirectUrl.get(policy).url),
        clientTypeForInvitationSent = Some(personal))
    }

    "return 200 with two options; agent-services-account and a link to create new invitation" in {
      givenAgentReference(arn, uid, personal)
      givenGetAgencyEmailAgentStub
      await(
        sessionStore.save(
          AgentSession(
            Some(personal),
            Some(serviceITSA),
            Some("ni"),
            Some(nino),
            Some(validPostcode),
            clientTypeForInvitationSent = Some(personal))))
      val result = invitationSent(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          s"$wireMockBaseUrlAsString${routes.ClientInvitationJourneyController.warmUp("personal", uid, "99-with-flake")}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.startNewAuthRequest"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueToASAccount.button"))
      verifyAuthoriseAttempt()
      await(sessionStore.hardGet) shouldBe AgentSession(clientTypeForInvitationSent = Some(personal))
    }
  }
}
