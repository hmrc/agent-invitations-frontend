package uk.gov.hmrc.agentinvitationsfrontend.controllers
import play.api.test.FakeRequest
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentMultiAuthorisationJourneyState, AuthorisationRequest, CurrentAuthorisationRequest}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId
import play.api.test.Helpers._


import scala.concurrent.ExecutionContext.Implicits.global

class AgentsErrorControllerISpec extends BaseISpec with AuthBehaviours {

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  lazy val controller: AgentsErrorController = app.injector.instanceOf[AgentsErrorController]

  "GET /agents/not-matched" should {
    val request = FakeRequest("GET", "/agents/not-matched")
    val notMatched = controller.notMatched()

    "return 403 for authorised Agent who submitted not matching known facts if they have empty basket" in {
      val result = notMatched(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 403
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          "There is a problem",
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-matched.description"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-matched.advice"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-matched.button"))
      checkHtmlResultWithNotBodyText(result, "Return to your authorisation requests")

      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "return 403 for authorised Agent who submitted not matching known facts if they have a basket" in {
      testAgentMultiAuthorisationJourneyStateCache.save(
        AgentMultiAuthorisationJourneyState("personal", Set(AuthorisationRequest("Gareth Gates", serviceITSA, mtdItId.value))))

      val result = notMatched(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 403
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          "There is a problem",
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, "The details you entered do not match to the same client.")
      checkHtmlResultWithBodyText(result, "Check them and try again.")
      checkHtmlResultWithBodyText(result,"Try again")
      checkHtmlResultWithBodyText(result, "Return to your authorisation requests")
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedAgentEndpoint(request, notMatched)
  }
}
