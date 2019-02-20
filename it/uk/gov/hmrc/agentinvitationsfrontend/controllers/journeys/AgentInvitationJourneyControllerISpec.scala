package uk.gov.hmrc.agentinvitationsfrontend.controllers.journeys
import play.api.Application
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationJourneyControllerISpec extends BaseISpec {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  override implicit lazy val app: Application = appBuilder
    .overrides(new TestAgentInvitationJourneyModule)
    .build()

  lazy val journeyState = app.injector.instanceOf[TestAgentInvitationJourneyService]
  lazy val controller: AgentInvitationJourneyController = app.injector.instanceOf[AgentInvitationJourneyController]

  import journeyState.model.States._

  "GET /agents" should {
    val request = FakeRequest("GET", "/agents")

    "redirect to /agents/client-type if no current state" in {
      val result = controller.agentsRoot()(request)
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientType().url)
      journeyState.get shouldBe Some((Start, Nil))
    }

    "redirect to /agents/client-type" in {
      journeyState.set(Start, Nil)
      val result = controller.agentsRoot()(request)
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientType().url)
      journeyState.get shouldBe Some((Start, Nil))
    }
  }

  "GET /agents/client-type" should {
    val request = FakeRequest("GET", "/agents/client-type")

    "show the client type page" in {
      journeyState.set(Start, Nil)

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
      journeyState.get shouldBe Some((SelectClientType, List(Start)))
    }
  }

  "POST /agents/client-type" should {
    val request = FakeRequest("POST", "/agents/client-type")

    "redirect to /agents/select-service after selecting personal client type" in {
      journeyState.set(SelectClientType, List(Start))

      val result =
        controller.submitClientType(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> "personal"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSelectService().url)
      journeyState.get shouldBe Some((ClientTypeSelected(ClientType.personal), List(SelectClientType, Start)))
    }

    "redirect to /agents/select-service after selecting business client type" in {
      journeyState.set(SelectClientType, List(Start))

      val result =
        controller.submitClientType(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> "business"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSelectService().url)
      journeyState.get shouldBe Some((ClientTypeSelected(ClientType.business), List(SelectClientType, Start)))
    }
  }

  "GET /agents/select-service" should {
    val request = FakeRequest("GET", "/agents/select-service")

    "show the select service page" in {
      journeyState.set(ClientTypeSelected(ClientType.personal), List(SelectClientType, Start))
      val result = controller.showSelectService()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("select-service.header"),
          htmlEscapedMessage("title.suffix.agents")),
        htmlEscapedMessage("select-service.header"),
        htmlEscapedMessage("personal-select-service.itsa"),
        htmlEscapedMessage("personal-select-service.personal-income-viewer"),
        htmlEscapedMessage("select-service.vat")
      )
      journeyState.get shouldBe Some(
        (
          SelectPersonalService(Nil, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)),
          List(ClientTypeSelected(ClientType.personal), SelectClientType, Start)))
    }
  }

  "POST /agents/select-personal-service" should {
    val request = FakeRequest("POST", "/agents/select-personal-service")

    "accept valid service choice and redirect" in {
      journeyState.set(
        SelectPersonalService(Nil, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)),
        List(ClientTypeSelected(ClientType.personal), SelectClientType, Start))

      val result = controller.submitPersonalSelectService(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> "HMRC-MTD-IT"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)

      journeyState.get shouldBe Some(
        PersonalServiceSelected(HMRCMTDIT, Nil),
        List(
          SelectPersonalService(Nil, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)),
          ClientTypeSelected(ClientType.personal),
          SelectClientType,
          Start)
      )
    }
  }

  "GET /agents/identify-client" should {
    val request = FakeRequest("GET", "/agents/identify-client")

    "show identify client page" in {
      journeyState.set(
        PersonalServiceSelected(HMRCMTDIT, Nil),
        List(
          SelectPersonalService(Nil, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)),
          ClientTypeSelected(ClientType.personal),
          SelectClientType,
          Start))

      val result = controller.showIdentifyClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "identify-client.header",
        "identify-client.itsa.p1",
        "identify-client.nino.label",
        "identify-client.nino.hint",
        "identify-client.postcode.label",
        "identify-client.postcode.hint"
      )

      journeyState.get shouldBe Some(
          IdentifyClient(HMRCMTDIT, Nil),
          List(PersonalServiceSelected(HMRCMTDIT, Nil), SelectPersonalService(Nil, Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)), ClientTypeSelected(ClientType.personal), SelectClientType, Start))
    }
  }
  }
