package uk.gov.hmrc.agentinvitationsfrontend.controllers

import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentSession
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId
import uk.gov.hmrc.play.binders.ContinueUrl

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationsControllerContinueUrlISpec extends BaseISpec {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  "GET /agents/invitation-sent" should {
    val request = FakeRequest("GET", "/agents/invitation-sent")
    val invitationSent = controller.showInvitationSent()
    "return 200 for authorised Agent with valid postcode and redirected to Confirm Invitation Page (secureFlag = false) for ITSA service" in {
      givenAgentReference(arn, uid, "personal")
      val continueUrl = ContinueUrl("/someITSA/Url")
      testAgentSessionCache.save(
        AgentSession(Some("personal"), Some(serviceITSA), Some("ni"), Some(nino), Some(validPostcode), continueUrl = Some("/someITSA/Url")))

      val result = invitationSent(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("invitation-sent-link.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.header"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l2", "someurl"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l4.personal"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueJourney.button"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.new-window"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.trackRequests.button"))
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          s"$wireMockBaseUrlAsString${routes.ClientsMultiInvitationController.warmUp("personal", uid, "99-with-flake")}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkHtmlResultWithBodyText(result, routes.AgentsInvitationController.continueAfterInvitationSent().url)
      checkInviteSentExitSurveyAgentSignOutLink(result)

      verifyAuthoriseAttempt()
      await(testAgentSessionCache.fetch).get.continueUrl shouldBe Some("/someITSA/Url")
    }

    "return 200 for authorised Agent, redirected to Confirm Invitation Page (secureFlag = false) for PIR service" in {
      givenAgentReference(arn, uid, "personal")
      val continueUrl = ContinueUrl("http://localhost:9996/tax-history/select-client")
      testAgentSessionCache.save(
        AgentSession(Some("personal"), Some(serviceITSA), Some("ni"), Some(nino), Some(validPostcode), continueUrl = Some("http://localhost:9996/tax-history/select-client")))

      val result = invitationSent(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("invitation-sent-link.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.header"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l2", "someurl"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l4.personal"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueJourney.button"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.new-window"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.trackRequests.button"))
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          s"$wireMockBaseUrlAsString${routes.ClientsMultiInvitationController.warmUp("personal", uid, "99-with-flake")}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkHtmlResultWithBodyText(result, routes.AgentsInvitationController.continueAfterInvitationSent().url)
      checkInviteSentExitSurveyAgentSignOutLink(result)

      verifyAuthoriseAttempt()
      await(testAgentSessionCache.fetch).get.continueUrl shouldBe Some(continueUrl.url)
    }

    "return 200 for authorised Agent with valid vat-reg-date and redirected to Confirm Invitation Page (secureFlag = false) for VAT service" in {
      givenAgentReference(arn, uid, "business")
      val continueUrl = ContinueUrl("/someVat/Url")
      testAgentSessionCache.save(AgentSession(Some("business"), Some(serviceITSA), Some("ni"), Some(nino), Some(validPostcode), continueUrl = Some(continueUrl.url)))

      val result = invitationSent(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("invitation-sent-link.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.header"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l2", "someurl"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l4.business"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueJourney.button"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.new-window"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.trackRequests.button"))
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          s"$wireMockBaseUrlAsString${routes.ClientsMultiInvitationController.warmUp("business", uid, "99-with-flake")}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkHtmlResultWithBodyText(result, routes.AgentsInvitationController.continueAfterInvitationSent().url)
      checkInviteSentExitSurveyAgentSignOutLink(result)

      verifyAuthoriseAttempt()
      await(testAgentSessionCache.fetch).get.continueUrl shouldBe Some(continueUrl.url)
    }

  }

  "GET /agents/invitation-sent-continue" should {
    val request = FakeRequest("GET", "/agents/invitation-sent-continue")
    val continueAfter = controller.continueAfterInvitationSent

    "redirect to where ever user came from" in {
      testAgentSessionCache.save(AgentSession(continueUrl = Some("/tax-history/select-service")))
      val result = continueAfter(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe "/tax-history/select-service"
      await(testAgentSessionCache.fetch) shouldBe Some(AgentSession())
    }

    "redirect to agent-services-account if no continue-url is stored in cache" in {
      testAgentSessionCache.save(AgentSession())
      val result = continueAfter(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe s"$wireMockBaseUrlAsString/agent-services-account"
      await(testAgentSessionCache.fetch) shouldBe Some(AgentSession())
    }
  }
}
