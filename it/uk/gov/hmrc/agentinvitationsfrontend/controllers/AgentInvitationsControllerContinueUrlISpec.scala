package uk.gov.hmrc.agentinvitationsfrontend.controllers

import play.api.test.FakeRequest
import play.api.test.Helpers.redirectLocation
import uk.gov.hmrc.agentinvitationsfrontend.support.{BaseISpec, TestDataCommonSupport}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId
import uk.gov.hmrc.play.binders.ContinueUrl
import play.api.test.Helpers._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationsControllerContinueUrlISpec extends BaseISpec {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  "GET /agents/invitation-sent" should {
    val request = FakeRequest("GET", "/agents/invitation-sent")
    val invitationSent = controller.invitationSent()
    "return 200 for authorised Agent with valid postcode and redirected to Confirm Invitation Page (secureFlag = false) for ITSA service" in {
      val continueUrl = ContinueUrl("/someITSA/Url")
      continueUrlKeyStoreCache.save(continueUrl)
      val result = invitationSent(
        authorisedAsValidAgent(
          request.withSession("invitationLink" -> "/invitations/personal/ABCDEFGH/my-agency-name", "clientType" -> "personal"),
          arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("invitation-sent-link.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.header"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l2", "someurl"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.p1"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.p2", "27 December 2017"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueJourney.button"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.new-window"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.trackRequests.button"))
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(s"$wireMockBaseUrlAsString${routes.ClientsMultiInvitationController.warmUp("personal", "ABCDEFGH", "my-agency-name")}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkHtmlResultWithBodyText(result, routes.AgentsInvitationController.continueAfterInvitationSent().url)
      checkInviteSentExitSurveyAgentSignOutLink(result)

      verifyAuthoriseAttempt()
      await(continueUrlKeyStoreCache.fetch).get shouldBe continueUrl
    }

    "return 200 for authorised Agent, redirected to Confirm Invitation Page (secureFlag = false) for PIR service" in {
      val continueUrl = ContinueUrl("http://localhost:9996/tax-history/select-client")
      continueUrlKeyStoreCache.save(continueUrl)
      val result = invitationSent(
        authorisedAsValidAgent(
          request.withSession(
            "invitationLink" -> "/invitations/personal/ABCDEFGH/my-agency-name",
            "clientType" -> "personal",
            "sessionId"    -> "session12345"),
          arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("invitation-sent-link.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.header"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l2", "someurl"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.p1"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.p2", "27 December 2017"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueJourney.button"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.new-window"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.trackRequests.button"))
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(s"$wireMockBaseUrlAsString${routes.ClientsMultiInvitationController.warmUp("personal", "ABCDEFGH", "my-agency-name")}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkHtmlResultWithBodyText(result, routes.AgentsInvitationController.continueAfterInvitationSent().url)
      checkInviteSentExitSurveyAgentSignOutLink(result)

      verifyAuthoriseAttempt()
      await(continueUrlKeyStoreCache.fetch).get shouldBe continueUrl
    }

    "return 200 for authorised Agent with valid vat-reg-date and redirected to Confirm Invitation Page (secureFlag = false) for VAT service" in {
      val continueUrl = ContinueUrl("/someVat/Url")
      continueUrlKeyStoreCache.save(continueUrl)
      val result = invitationSent(
        authorisedAsValidAgent(
          request.withSession("invitationLink" -> "/invitations/personal/ABCDEFGH/my-agency-name",
            "clientType" -> "personal"),
          arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("invitation-sent-link.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.header"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l2", "someurl"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.p1"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.p2", "27 December 2017"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueJourney.button"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.new-window"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.trackRequests.button"))
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(s"$wireMockBaseUrlAsString${routes.ClientsMultiInvitationController.warmUp("personal", "ABCDEFGH", "my-agency-name")}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkHtmlResultWithBodyText(result, routes.AgentsInvitationController.continueAfterInvitationSent().url)
      checkInviteSentExitSurveyAgentSignOutLink(result)

      verifyAuthoriseAttempt()
      await(continueUrlKeyStoreCache.fetch).get shouldBe continueUrl
    }

  }

  "GET /agents/invitation-sent-continue" should {
    val request = FakeRequest("GET", "/agents/invitation-sent-continue")
    val continueAfter = controller.continueAfterInvitationSent

    "redirect to where ever user came from" in {
      continueUrlKeyStoreCache.save(ContinueUrl("/tax-history/select-service"))
      val result = continueAfter(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe "/tax-history/select-service"
      await(continueUrlKeyStoreCache.fetch) shouldBe None
    }

    "redirect to agent-services-account if no continue-url is stored in cache" in {
      val result = continueAfter(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe "/agent-services-account"
      await(continueUrlKeyStoreCache.fetch) shouldBe None
    }
  }
}
