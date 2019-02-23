package uk.gov.hmrc.agentinvitationsfrontend.controllers.journeys
import play.api.Application
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.personal
import uk.gov.hmrc.agentinvitationsfrontend.models.{AuthorisationRequest, ClientType, Invitation}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.Vrn
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
      journeyState.get shouldBe Some((SelectClientType(Set.empty), List(Start)))
    }
  }

  "POST /agents/client-type" should {
    val request = FakeRequest("POST", "/agents/client-type")

    "redirect to /agents/select-service after selecting personal client type" in {
      journeyState.set(SelectClientType(Set.empty), List(Start))

      val result =
        controller.submitClientType(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> "personal"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSelectService().url)
      journeyState.get shouldBe Some((ClientTypeSelected(ClientType.personal, Set.empty), List(SelectClientType(Set.empty), Start)))
    }

    "redirect to /agents/select-service after selecting business client type" in {
      journeyState.set(SelectClientType(Set.empty), List(Start))

      val result =
        controller.submitClientType(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> "business"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSelectService().url)
      journeyState.get shouldBe Some((ClientTypeSelected(ClientType.business, Set.empty), List(SelectClientType(Set.empty), Start)))
    }

    "redisplay the page with errors if nothing is selected" in {
      journeyState.set(SelectClientType(Set.empty), List(Start))

      val result =
        controller.submitClientType()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result, "This field is required")

      journeyState.get shouldBe Some((SelectClientType(Set.empty), List(Start)))
    }
  }

  "GET /agents/select-service" should {
    val request = FakeRequest("GET", "/agents/select-service")

    "show the select service page" in {
      journeyState.set(ClientTypeSelected(ClientType.personal, Set.empty), List(SelectClientType(Set.empty), Start))
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
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
          List(ClientTypeSelected(ClientType.personal, Set.empty),
            SelectClientType(Set.empty),
            Start)))
    }
  }

  "POST /agents/select-personal-service" should {
    val request = FakeRequest("POST", "/agents/select-personal-service")

    "accept valid service choice and redirect" in {
      journeyState.set(
        SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
        List(ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start))

      val result = controller.submitPersonalSelectService(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> "HMRC-MTD-IT"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)

      journeyState.get shouldBe Some(
        PersonalServiceSelected(HMRCMTDIT, Set.empty),
        List(SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
          ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start))
    }
  }

  "POST /agents/select-business-service" should {
    val request = FakeRequest("POST", "/agents/select-business-service")

    "redirect to identify-client when yes is selected" in {
      journeyState.set(
        SelectBusinessService(Set.empty),
        List(ClientTypeSelected(ClientType.business, Set.empty),
          SelectClientType(Set.empty),
          Start))

      val result = controller.submitBusinessSelectService(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)

      journeyState.get shouldBe Some(
        BusinessServiceSelected(Set.empty),
        List(SelectBusinessService(Set.empty),
          ClientTypeSelected(ClientType.business, Set.empty),
          SelectClientType(Set.empty),
          Start))
    }
    "redirect to select-client-type when no is selected" in {
      journeyState.set(
        SelectBusinessService(Set.empty),
        List(ClientTypeSelected(ClientType.business, Set.empty),
          SelectClientType(Set.empty),
          Start))

      val result = controller.submitBusinessSelectService(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientType().url)

      journeyState.get shouldBe Some(
        Start,
        List(SelectBusinessService(Set.empty),
          ClientTypeSelected(ClientType.business, Set.empty),
          SelectClientType(Set.empty),
          Start)
      )
    }
  }

  "GET /agents/identify-client" should {
    val request = FakeRequest("GET", "/agents/identify-client")

    "show identify client page" in {
      journeyState.set(
        PersonalServiceSelected(HMRCMTDIT, Set.empty),
        List(SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
          ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
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
          IdentifyPersonalClient(HMRCMTDIT, Set.empty),
          List(PersonalServiceSelected(HMRCMTDIT, Set.empty),
            SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
            ClientTypeSelected(ClientType.personal, Set.empty),
            SelectClientType(Set.empty),
            Start))
    }
  }

  "POST /agents/identify-itsa-client" should {
    val request = FakeRequest("POST", "/agents/identify-itsa-client")

    "redirect to confirm client" in {
      journeyState.set(IdentifyPersonalClient(HMRCMTDIT, Set.empty),
        List(PersonalServiceSelected(HMRCMTDIT, Set.empty),
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
          ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start))

      val result = controller.submitIdentifyItsaClient(authorisedAsValidAgent(request.withFormUrlEncodedBody("clientIdentifier" -> "AB123456A", "postcode" -> "BN114AW"), arn.value))

    status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showConfirmClient().url)

      journeyState.get shouldBe Some(
        ItsaIdentifiedClient("AB123456A", Some("BN114AW"), Set.empty),
        List(IdentifyPersonalClient(HMRCMTDIT, Set.empty),
          PersonalServiceSelected(HMRCMTDIT, Set.empty),
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
          ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start))
    }
  }

  "POST /agents/identify-vat-client" should {
    val request = FakeRequest("POST", "/agents/identify-vat-client")

    "redirect to confirm client" in {
      journeyState.set(IdentifyBusinessClient(Set.empty),
        List(PersonalServiceSelected(HMRCMTDVAT, Set.empty),
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
          ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start))

      val result = controller.submitIdentifyVatClient(authorisedAsValidAgent(request.withFormUrlEncodedBody("clientIdentifier" -> "202949960", "registrationDate.year" -> "2010", "registrationDate.month" -> "10", "registrationDate.day" -> "10"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showConfirmClient().url)

      journeyState.get shouldBe Some(
        VatIdentifiedBusinessClient("202949960", Some("2010-10-10"), Set.empty),
        List(IdentifyBusinessClient(Set.empty),
          PersonalServiceSelected(HMRCMTDVAT, Set.empty),
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
          ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start))
    }
  }


  "POST /agents/identify-irv-client" should {
    val request = FakeRequest("POST", "/agents/identify-irv-client")

    "redirect to confirm client" in {
      journeyState.set(IdentifyPersonalClient(HMRCPIR, Set.empty),
        List(PersonalServiceSelected(HMRCPIR, Set.empty),
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
          ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start))

      val result = controller.submitIdentifyIrvClient(authorisedAsValidAgent(request.withFormUrlEncodedBody("clientIdentifier" -> "AB123456A", "dob.year" -> "1990",  "dob.month" -> "10",  "dob.day" -> "10"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showConfirmClient().url)

      journeyState.get shouldBe Some(
        IrvIdentifiedClient("AB123456A", Some("1990-10-10"), Set.empty),
        List(IdentifyPersonalClient(HMRCPIR, Set.empty),
          PersonalServiceSelected(HMRCPIR, Set.empty),
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
          ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start))
    }
  }

  "GET /agents/confirm-client" should {
    val request = FakeRequest("GET", "/agents/confirm-client")

    "show the confirm client page for ITSA service" in {
      givenTradingName(validNino, "Sylvia Plath")
      journeyState.set(ItsaIdentifiedClient("AB123456A", Some("BN114AW"), Set.empty),
        List(IdentifyPersonalClient(HMRCMTDIT, Set.empty),
          PersonalServiceSelected(HMRCMTDIT, Set.empty),
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
          ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start))

      val result = controller.showConfirmClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Sylvia Plath")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")

      journeyState.get shouldBe Some(ConfirmClientItsa("Sylvia Plath", Set.empty),
        List(ItsaIdentifiedClient("AB123456A", Some("BN114AW"), Set.empty),
          IdentifyPersonalClient(HMRCMTDIT, Set.empty),
          PersonalServiceSelected(HMRCMTDIT, Set.empty),
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
          ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start))
    }

    "show the confirm client page for IRV service" in {
      givenCitizenDetailsAreKnownFor(validNino.value, "Virginia", "Woolf")
      journeyState.set(IrvIdentifiedClient("AB123456A", Some("1990-10-10"), Set.empty),
        List(IdentifyPersonalClient(HMRCPIR, Set.empty),
          PersonalServiceSelected(HMRCPIR, Set.empty),
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
          ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start))

      val result = controller.showConfirmClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Virginia Woolf")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")

      journeyState.get shouldBe Some(ConfirmClientIrv("Virginia Woolf", Set.empty),
        List(IrvIdentifiedClient("AB123456A", Some("1990-10-10"), Set.empty),
          IdentifyPersonalClient(HMRCPIR, Set.empty),
          PersonalServiceSelected(HMRCPIR, Set.empty),
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
          ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start))
    }

    "show the confirm client page for VAT service" in {
      givenClientDetails(Vrn("202949960"))
      journeyState.set(VatIdentifiedBusinessClient("202949960", Some("2010-10-10"), Set.empty),
        List(IdentifyBusinessClient(Set.empty),
          BusinessServiceSelected(Set.empty),
          SelectBusinessService(Set.empty),
          ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start))

      val result = controller.showConfirmClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "GDT")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")

      journeyState.get shouldBe Some(ConfirmClientBusinessVat("GDT", Set.empty),
        List(VatIdentifiedBusinessClient("202949960", Some("2010-10-10"), Set.empty),
          IdentifyBusinessClient(Set.empty),
          BusinessServiceSelected(Set.empty),
          SelectBusinessService(Set.empty),
          ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start))
    }
  }

  "POST /agents/confirm-client" should {
    val request = FakeRequest("POST", "/agents/confirm-client")

    "redirect to the review authorisations page when yes is selected" in {
      journeyState.set(ConfirmClientPersonalVat("GDT", Set.empty),
        List(VatIdentifiedPersonalClient("202949960", Some("2010-10-10"), Set.empty),
          IdentifyPersonalClient(HMRCMTDVAT, Set.empty), PersonalServiceSelected(HMRCMTDVAT, Set.empty),
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
          ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start))

      val result = controller.clientConfirmed(authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showReviewAuthorisations().url)

      journeyState.get shouldBe Some((ClientConfirmedPersonal(Set.empty),
        List(ConfirmClientPersonalVat("GDT", Set.empty),
          VatIdentifiedPersonalClient("202949960", Some("2010-10-10"), Set.empty),
          IdentifyPersonalClient(HMRCMTDVAT, Set.empty),
          PersonalServiceSelected(HMRCMTDVAT, Set.empty),
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
          ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start)))
    }

    "redirect to the identify-client page when no is selected" in {
      journeyState.set(ConfirmClientPersonalVat("GDT", Set.empty),
        List(VatIdentifiedPersonalClient("202949960", Some("2010-10-10"), Set.empty), IdentifyPersonalClient(HMRCMTDVAT, Set.empty), PersonalServiceSelected(HMRCMTDVAT, Set.empty), SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty), ClientTypeSelected(ClientType.personal, Set.empty), SelectClientType(Set.empty), Start))

      val result = controller.clientConfirmed(authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)

      journeyState.get shouldBe Some((PersonalServiceSelected(HMRCMTDVAT, Set.empty),
        List(ConfirmClientPersonalVat("GDT", Set.empty), VatIdentifiedPersonalClient("202949960", Some("2010-10-10"), Set.empty), IdentifyPersonalClient(HMRCMTDVAT, Set.empty), PersonalServiceSelected(HMRCMTDVAT, Set.empty), SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty), ClientTypeSelected(ClientType.personal, Set.empty), SelectClientType(Set.empty), Start)))
    }
  }

  "GET /agents/review-authorisations" should {
    val fullBasket = Set(AuthorisationRequest("James Client", Invitation(Some(ClientType.personal), HMRCMTDIT, "AB123456A", Some("BN114AW")), itemId = "ABC"))
    val request = FakeRequest("GET", "/agents/review-authorisations")

    "show the review authorisations page" in {
      journeyState.set(ClientConfirmedPersonal(fullBasket),
        List(ConfirmClientItsa("GDT", Set.empty),
          ItsaIdentifiedClient("202949960", Some("2010-10-10"), Set.empty),
          IdentifyPersonalClient(HMRCMTDIT, Set.empty), PersonalServiceSelected(HMRCMTDIT, Set.empty),
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty), ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start))

      val result = controller.showReviewAuthorisations()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyText(
        result,
        "Review your authorisation requests",
        "You have added 1 authorisation request.",
        "Report their income and expenses through software",
        "James Client",
        "Do you need to add another authorisation for this client?"
      )

      journeyState.get shouldBe Some((ReviewAuthorisationsPersonal(fullBasket),
        List(ClientConfirmedPersonal(fullBasket),
          ConfirmClientItsa("GDT", Set.empty),
          ItsaIdentifiedClient("202949960", Some("2010-10-10"), Set.empty),
          IdentifyPersonalClient(HMRCMTDIT, Set.empty),
          PersonalServiceSelected(HMRCMTDIT, Set.empty),
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
          ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start)))
    }
  }

  "POST /agents/review-authorisations" should {
    val fullBasket = Set(AuthorisationRequest("James Client", Invitation(Some(ClientType.personal), HMRCMTDIT, "AB123456A", Some("BN114AW")), itemId = "ABC"))
    val request = FakeRequest("POST", "/agents/review-authorisations")

    "redirect to invitation-sent page when no is selected" in {
      journeyState.set(ReviewAuthorisationsPersonal(fullBasket),
        List(ClientConfirmedPersonal(fullBasket),
          ConfirmClientItsa("GDT", Set.empty),
          ItsaIdentifiedClient("202949960", Some("2010-10-10"), Set.empty),
          IdentifyPersonalClient(HMRCMTDIT, Set.empty), PersonalServiceSelected(HMRCMTDIT, Set.empty),
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
          ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty), Start))

      val result = controller.authorisationsReviewed(authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showInvitationSent().url)

      journeyState.get shouldBe Some((AuthorisationsReviewedPersonal,
        List(ReviewAuthorisationsPersonal(fullBasket),
          ClientConfirmedPersonal(fullBasket),
          ConfirmClientItsa("GDT", Set.empty),
          ItsaIdentifiedClient("202949960", Some("2010-10-10"), Set.empty),
          IdentifyPersonalClient(HMRCMTDIT, Set.empty), PersonalServiceSelected(HMRCMTDIT, Set.empty),
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty), ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start)))
    }

    "redirect to select-service when yes is selected" in {
      journeyState.set(ReviewAuthorisationsPersonal(fullBasket),
        List(ClientConfirmedPersonal(fullBasket),
          ConfirmClientItsa("GDT", Set.empty), ItsaIdentifiedClient("202949960", Some("2010-10-10"), Set.empty), IdentifyPersonalClient(HMRCMTDIT, Set.empty), PersonalServiceSelected(HMRCMTDIT, Set.empty), SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty), ClientTypeSelected(ClientType.personal, Set.empty), SelectClientType(Set.empty), Start))

      val result = controller.authorisationsReviewed(authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSelectService().url)

      journeyState.get shouldBe Some(ClientTypeSelected(personal, fullBasket),
        List(ReviewAuthorisationsPersonal(fullBasket),
          ClientConfirmedPersonal(fullBasket),
          ConfirmClientItsa("GDT", Set.empty),
          ItsaIdentifiedClient("202949960", Some("2010-10-10"), Set.empty),
          IdentifyPersonalClient(HMRCMTDIT, Set.empty),
          PersonalServiceSelected(HMRCMTDIT, Set.empty),
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), Set.empty),
          ClientTypeSelected(ClientType.personal, Set.empty),
          SelectClientType(Set.empty),
          Start))

    }

  }
}
