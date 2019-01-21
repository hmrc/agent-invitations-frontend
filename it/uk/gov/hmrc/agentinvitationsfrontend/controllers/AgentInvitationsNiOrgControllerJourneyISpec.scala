package uk.gov.hmrc.agentinvitationsfrontend.controllers

import play.api.mvc.{Action, AnyContent, AnyContentAsEmpty}
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.forms.ServiceTypeForm
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationsNiOrgControllerJourneyISpec extends BaseISpec with AuthBehaviours {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  "POST /agents/select-service" should {
    val request = FakeRequest("POST", "/agents/select-service")
    val submitService = controller.submitSelectService()

    "return 303 for authorised Agent with valid NI-ORG service, redirect to identify-client" in {

      Set("personal", "business").foreach { clientType =>
        testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(Some(clientType)))
        val serviceForm = ServiceTypeForm.form.fill(serviceNiOrg)
        val result = submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some("/invitations/agents/identify-client")
      }
    }
  }

  "GET /agents/identify-client" should {
    val request = FakeRequest("GET", "/agents/identify-client")
    val showIdentifyClientForm = controller.showIdentifyClient()

    behave like anAuthorisedAgentEndpoint(request, showIdentifyClientForm)

    "return 200 for an Agent with HMRC-AS-AGENT enrolment for NI-ORG service" in {
      Set("personal", "business").foreach { clientType =>
        testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(Some(clientType), serviceNiOrg))
        val result = showIdentifyClientForm(authorisedAsValidAgent(request, arn.value))
        status(result) shouldBe 200

        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "title.suffix.agents")

        checkHtmlResultWithBodyMsgs(
          result,
          "identify-client.header",
          "identify-client.utr.p1",
          "identify-client.utr.label",
          "identify-client.utr.hint",
          "utr.hint.expandable",
          "identify-client.postcode.hint",
          "identify-client.postcode.label"
        )

        checkHasAgentSignOutLink(result)
      }
    }
  }

  def behaveLikeMissingCacheScenarios(action: Action[AnyContent], request: FakeRequest[AnyContentAsEmpty.type]) = {
    "return to identify-client no client identifier found in cache" in {
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(business, serviceVAT, "", "", None, fromManual))
      val result = action(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showIdentifyClient().url
    }

    "return to client-type for no cache" in {
      val result = action(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showClientType().url
    }
  }
}