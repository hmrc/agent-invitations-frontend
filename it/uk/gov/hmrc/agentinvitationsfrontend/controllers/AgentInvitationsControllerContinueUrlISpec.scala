package uk.gov.hmrc.agentinvitationsfrontend.controllers

import play.api.test.FakeRequest
import play.api.test.Helpers.redirectLocation
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
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
  val arn = Arn("TARN0000001")
  val mtdItId = MtdItId("ABCDEF123456789")
  private val validNino = Nino("AB123456A")
  private val validNinoSpace = Nino("AB 12 34 56 A")
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

  "GET /agents/invitation-sent" should {
    val request = FakeRequest("GET", "/agents/invitation-sent")
    val invitationSent = controller.invitationSent()
    "return 200 for authorised Agent with valid postcode and redirected to Confirm Invitation Page (secureFlag = false) for ITSA service" in {
      val continueUrl = ContinueUrl("/someITSA/Url")
      continueUrlKeyStoreCache.cacheContinueUrl(continueUrl)
      val result = invitationSent(
        authorisedAsValidAgent(
          request.withSession("invitationId" -> invitationIdITSA.value, "deadline" -> "27 December 2017"),
          arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("invitation-sent-link.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.header"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.description.advice.pt1", "27 December 2017"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.description.advice.pt2"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.description.advice.pt3"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueJourney.button"))
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(s"$wireMockBaseUrlAsString${routes.ClientsInvitationController.start(invitationIdITSA)}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkHtmlResultWithBodyText(result, routes.AgentsInvitationController.continueAfterInvitationSent().url)
      checkInviteSentExitSurveyAgentSignOutLink(result)

      verifyAuthoriseAttempt()
      await(continueUrlKeyStoreCache.fetchContinueUrl).get shouldBe continueUrl
    }

    "return 200 for authorised Agent, redirected to Confirm Invitation Page (secureFlag = false) for PIR service" in {
      val continueUrl = ContinueUrl("http://localhost:9996/tax-history/select-client")
      continueUrlKeyStoreCache.cacheContinueUrl(continueUrl)
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
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("invitation-sent-link.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.header"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.description.advice.pt1", "27 December 2017"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.description.advice.pt2"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.description.advice.pt3"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueJourney.button"))
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(s"$wireMockBaseUrlAsString${routes.ClientsInvitationController.start(invitationIdPIR)}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkHtmlResultWithBodyText(result, routes.AgentsInvitationController.continueAfterInvitationSent().url)
      checkInviteSentExitSurveyAgentSignOutLink(result)

      verifyAuthoriseAttempt()
      await(continueUrlKeyStoreCache.fetchContinueUrl).get shouldBe continueUrl
    }

    "return 200 for authorised Agent with valid vat-reg-date and redirected to Confirm Invitation Page (secureFlag = false) for VAT service" in {
      val continueUrl = ContinueUrl("/someVat/Url")
      continueUrlKeyStoreCache.cacheContinueUrl(continueUrl)
      val result = invitationSent(
        authorisedAsValidAgent(
          request.withSession("invitationId" -> invitationIdVAT.value, "deadline" -> "27 December 2017"),
          arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("invitation-sent-link.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.header"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.description.advice.pt1", "27 December 2017"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.description.advice.pt2"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.description.advice.pt3"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueJourney.button"))
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(s"$wireMockBaseUrlAsString${routes.ClientsInvitationController.start(invitationIdVAT)}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkHtmlResultWithBodyText(result, routes.AgentsInvitationController.continueAfterInvitationSent().url)
      checkInviteSentExitSurveyAgentSignOutLink(result)

      verifyAuthoriseAttempt()
      await(continueUrlKeyStoreCache.fetchContinueUrl).get shouldBe continueUrl
    }

  }

  "GET /agents/invitation-sent-continue" should {
    val request = FakeRequest("GET", "/agents/invitation-sent-continue")
    val continueAfter = controller.continueAfterInvitationSent

    "redirect to where ever user came from" in {
      continueUrlKeyStoreCache.cacheContinueUrl(ContinueUrl("/tax-history/select-service"))
      val result = continueAfter(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe "/tax-history/select-service"
      await(continueUrlKeyStoreCache.fetchContinueUrl) shouldBe None
    }

    "redirect to agent-services-account if no continue-url is stored in cache" in {
      val result = continueAfter(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe "/agent-services-account"
      await(continueUrlKeyStoreCache.fetchContinueUrl) shouldBe None
    }
  }
}
