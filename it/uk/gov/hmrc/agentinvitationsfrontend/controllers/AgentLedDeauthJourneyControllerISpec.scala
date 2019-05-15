package uk.gov.hmrc.agentinvitationsfrontend.controllers
import play.api.Application
import play.api.test.FakeRequest
import play.api.test.Helpers.redirectLocation
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State.{SelectClientType, SelectServiceBusiness, SelectServicePersonal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.duration._

class AgentLedDeauthJourneyControllerISpec extends BaseISpec with StateAndBreadcrumbsMatchers {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  override implicit lazy val app: Application = appBuilder
    .overrides(new TestAgentLedDeauthJourneyModule)
    .build()

  lazy val journeyState = app.injector.instanceOf[TestAgentLedDeauthJourneyService]

  val availableServices = Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)

  val controller = app.injector.instanceOf[AgentLedDeauthJourneyController]

  "GET /fsm/agents/cancel-authorisation/client-type" should {
    "display the client type page " in {
      val request = FakeRequest("GET", "/agents/cancel-authorisation/client-type")
      val selectClientType = controller.showClientType()

      val result = selectClientType(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("cancel-authorisation.client-type.header"),
          htmlEscapedMessage("title.suffix.agents")),
        htmlEscapedMessage("cancel-authorisation.client-type.header"),
        hasMessage("cancel-authorisation.client-type.p1")
      )
      checkResultContainsBackLink(result, s"http://localhost:$wireMockPort/agent-services-account")
    }
  }

  "POST /fsm/agents/cancel-authorisation/client-type" should {
    "redirect to select service page when client type is personal" in {
      journeyState.set(SelectClientType, Nil)
      val request = FakeRequest("POST", "/agents/cancel-authorisation/client-type")

      val result =
        controller.submitClientType(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> "personal"), arn.value))
      status(result) shouldBe 303
      val timeout = 2.seconds
      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showSelectService().url
    }
    "redirect to select service page when client type is business" in {
      journeyState.set(SelectClientType, Nil)
      val request = FakeRequest("POST", "/agents/cancel-authorisation/client-type")

      val result =
        controller.submitClientType(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> "business"), arn.value))
      status(result) shouldBe 303
      val timeout = 2.seconds
      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showSelectService().url
    }
  }

  "GET /fsm/agents/cancel-authorisation/select-service" should {
    "show the select service page for personal services" in {
      journeyState.set(SelectServicePersonal(availableServices), Nil)
      val request = FakeRequest("GET", "/agents/cancel-authorisation/select-service")
      val result = controller.showSelectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("cancel-authorisation.select-service.header"),
          htmlEscapedMessage("title.suffix.agents")),
        htmlEscapedMessage("cancel-authorisation.select-service.header"),
        hasMessage("cancel-authorisation.select-service.hint")
      )
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/client-type")
    }
    "show the select service page for business service" in {
      journeyState.set(SelectServiceBusiness, Nil)
      val request = FakeRequest("GET", "/agents/cancel-authorisation/select-service")
      val result = controller.showSelectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        hasMessage(
          "generic.title",
          htmlEscapedMessage("cancel-authorisation.business-select-service.header"),
          htmlEscapedMessage("title.suffix.agents")),
        htmlEscapedMessage("cancel-authorisation.business-select-service.header"),
        hasMessage("business-select-service.yes"),
        hasMessage("business-select-service.no")
      )
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/client-type")
    }
  }
}
