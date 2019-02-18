package uk.gov.hmrc.agentinvitationsfrontend.controllers.journeys
import play.api.Application
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationJourneyControllerISpec extends BaseISpec {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  override implicit lazy val app: Application = appBuilder
    .overrides(new TestAgentInvitationJourneyModule)
    .build()

  lazy val testJourneyService = app.injector.instanceOf[TestAgentInvitationJourneyService]
  lazy val controller: AgentInvitationJourneyController = app.injector.instanceOf[AgentInvitationJourneyController]

  import testJourneyService.model.States._

  "GET /agents" should {
    val request = FakeRequest("GET", "/agents")

    "redirect to /agents/client-type if no current state" in {
      val result = controller.agentsRoot()(request)
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientType().url)
      testJourneyService.get shouldBe Some((Start, Nil))
    }

    "redirect to /agents/client-type" in {
      testJourneyService.set(Start, Nil)
      val result = controller.agentsRoot()(request)
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientType().url)
      testJourneyService.get shouldBe Some((Start, Nil))
    }
  }

  "GET /agents/client-type" should {
    val request = FakeRequest("GET", "/agents/client-type")

    "show the client type page" in {
      testJourneyService.set(Start, Nil)

      val result = controller.showClientType()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("client-type.header"),
          htmlEscapedMessage("title.suffix.agents")),
        htmlEscapedMessage("client-type.header"),
        hasMessage("client-type.p1")
      )
      testJourneyService.get shouldBe Some((SelectClientType, List(Start)))
    }
  }

  "POST /agents/client-type" should {
    val request = FakeRequest("POST", "/agents/client-type")

    "redirect to /agents/select-service after selecting personal client type" in {
      testJourneyService.set(SelectClientType, List(Start))

      val result =
        controller.submitClientType(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> "personal"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSelectService().url)
      testJourneyService.get shouldBe Some((SelectedClientType(ClientType.personal), List(SelectClientType, Start)))
    }

    "redirect to /agents/select-service after selecting business client type" in {
      testJourneyService.set(SelectClientType, List(Start))

      val result =
        controller.submitClientType(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> "business"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSelectService().url)
      testJourneyService.get shouldBe Some((SelectedClientType(ClientType.business), List(SelectClientType, Start)))
    }
  }
}
