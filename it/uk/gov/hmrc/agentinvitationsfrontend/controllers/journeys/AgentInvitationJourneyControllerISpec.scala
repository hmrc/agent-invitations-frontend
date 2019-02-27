package uk.gov.hmrc.agentinvitationsfrontend.controllers.journeys
import org.joda.time.LocalDate
import play.api.Application
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.personal
import uk.gov.hmrc.agentinvitationsfrontend.models.{AuthorisationRequest, ClientType, Invitation}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.Vrn
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationJourneyControllerISpec extends BaseISpec with StateAndBreadcrumbsMatchers {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  override implicit lazy val app: Application = appBuilder
    .overrides(new TestAgentInvitationJourneyModule)
    .build()

  lazy val journeyState = app.injector.instanceOf[TestAgentInvitationJourneyService]
  lazy val controller: AgentInvitationJourneyController = app.injector.instanceOf[AgentInvitationJourneyController]

  import journeyState.model.State
  import journeyState.model.States._

  val availableServices = Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)
  val emptyBasket = Set.empty[AuthorisationRequest]

  "GET /agents" should {
    val request = FakeRequest("GET", "/agents")

    "redirect to /agents/client-type if no current state" in {
      val result = controller.agentsRoot()(request)
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientType().url)
      journeyState.get should have[State](SelectClientType(emptyBasket), Nil)
    }

    "redirect to /agents/client-type" in {
      journeyState.set(SelectClientType(emptyBasket), Nil)
      val result = controller.agentsRoot()(request)
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientType().url)
      journeyState.get should have[State](SelectClientType(emptyBasket), Nil)
    }
  }

  "GET /agents/client-type" should {
    val request = FakeRequest("GET", "/agents/client-type")

    "show the client type page" in {
      journeyState.set(SelectClientType(emptyBasket), Nil)

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
      journeyState.get should have[State](SelectClientType(emptyBasket), Nil)
    }
  }

  "POST /agents/client-type" should {
    val request = FakeRequest("POST", "/agents/client-type")

    "redirect to /agents/select-service after selecting personal client type" in {
      journeyState.set(SelectClientType(emptyBasket), Nil)

      val result =
        controller.submitClientType(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> "personal"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSelectService().url)
      journeyState.get shouldBe Some(
        (SelectPersonalService(availableServices, emptyBasket), List(SelectClientType(emptyBasket))))
    }

    "redirect to /agents/select-service after selecting business client type" in {
      journeyState.set(SelectClientType(emptyBasket), Nil)

      val result =
        controller.submitClientType(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> "business"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSelectService().url)
      journeyState.get should have[State](SelectBusinessService(emptyBasket), List(SelectClientType(emptyBasket)))
    }

    "redisplay the page with errors if nothing is selected" in {
      journeyState.set(SelectClientType(emptyBasket), Nil)

      val result =
        controller.submitClientType()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "This field is required")

      journeyState.get should have[State](SelectClientType(emptyBasket), Nil)

    }

    "GET /agents/select-service" should {
      val request = FakeRequest("GET", "/agents/select-service")

      "show the select service page" in {
        journeyState.set(SelectPersonalService(availableServices, emptyBasket), List(SelectClientType(emptyBasket)))
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
          (SelectPersonalService(availableServices, emptyBasket), List(SelectClientType(emptyBasket))))
      }

      "go back to the select service page" in {
        journeyState.set(
          IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
          List(SelectPersonalService(availableServices, emptyBasket), SelectClientType(emptyBasket))
        )
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
        journeyState.get should have[State](
          SelectPersonalService(availableServices, emptyBasket),
          List(SelectClientType(emptyBasket)))
      }
    }

    "POST /agents/select-personal-service" should {
      val request = FakeRequest("POST", "/agents/select-personal-service")

      "accept valid service choice and redirect" in {
        journeyState.set(SelectPersonalService(availableServices, emptyBasket), List(SelectClientType(emptyBasket)))

        val result = controller.submitPersonalSelectService(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> "HMRC-MTD-IT"), arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)

        journeyState.get should have[State](
          IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
          List(SelectPersonalService(availableServices, emptyBasket), SelectClientType(emptyBasket)))
      }
    }

    "POST /agents/select-business-service" should {
      val request = FakeRequest("POST", "/agents/select-business-service")

      "redirect to identify-client when yes is selected" in {
        journeyState.set(SelectBusinessService(emptyBasket), List(SelectClientType(emptyBasket)))

        val result = controller.submitBusinessSelectService(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)

        journeyState.get should have[State](
          IdentifyBusinessClient(emptyBasket),
          List(SelectBusinessService(emptyBasket), SelectClientType(emptyBasket)))
      }
      "redirect to select-client-type when no is selected" in {
        journeyState.set(SelectBusinessService(emptyBasket), List(SelectClientType(emptyBasket)))

        val result = controller.submitBusinessSelectService(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientType().url)

        journeyState.get should have[State](
          SelectClientType(emptyBasket),
          List(SelectBusinessService(emptyBasket), SelectClientType(emptyBasket)))
      }
    }

    "GET /agents/identify-client" should {
      val request = FakeRequest("GET", "/agents/identify-client")

      "show identify client page" in {
        journeyState.set(
          IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
          List(SelectPersonalService(availableServices, emptyBasket), SelectClientType(emptyBasket)))

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

        journeyState.get should have[State](
          IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
          List(SelectPersonalService(availableServices, emptyBasket), SelectClientType(emptyBasket)))
      }
    }

    "POST /agents/identify-itsa-client" should {
      val request = FakeRequest("POST", "/agents/identify-itsa-client")

      "redirect to confirm client" in {
        givenMatchingClientIdAndPostcode(Nino(nino), "BN114AW")
        givenGetAllPendingInvitationsReturnsEmpty(arn, nino, HMRCMTDIT)
        givenCheckRelationshipItsaWithStatus(arn, nino, 404)
        givenTradingName(Nino(nino), "Sylvia Plath")

        journeyState.set(
          IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
          List(SelectPersonalService(availableServices, emptyBasket), SelectClientType(emptyBasket)))

        val result = controller.submitIdentifyItsaClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("clientIdentifier" -> nino, "postcode" -> "BN114AW"),
            arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showConfirmClient().url)

        journeyState.get should have[State](
          ConfirmClientItsa("Sylvia Plath", emptyBasket),
          List(
            IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket))
        )
      }

      "redirect to not matched when clientId and known fact don't match" in {
        givenNonMatchingClientIdAndPostcode(Nino(nino), "BN114AW")

        journeyState.set(
          IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
          List(SelectPersonalService(availableServices, emptyBasket), SelectClientType(emptyBasket)))

        val result = controller.submitIdentifyItsaClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("clientIdentifier" -> nino, "postcode" -> "BN114AW"),
            arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showNotMatched().url)

        journeyState.get should have[State](
          KnownFactNotMatched(emptyBasket),
          List(
            IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket))
        )
      }
    }

    "POST /agents/identify-vat-client" should {
      val request = FakeRequest("POST", "/agents/identify-vat-client")

      "redirect to confirm client" in {
        givenVatRegisteredClientReturns(Vrn("202949960"), LocalDate.parse("2010-10-10"), 204)
        givenGetAllPendingInvitationsReturnsEmpty(arn, "202949960", HMRCMTDVAT)
        givenCheckRelationshipVatWithStatus(arn, "202949960", 404)
        givenClientDetails(Vrn("202949960"))

        journeyState.set(
          IdentifyPersonalClient(HMRCMTDVAT, emptyBasket),
          List(SelectPersonalService(availableServices, emptyBasket), SelectClientType(emptyBasket)))

        val result = controller.submitIdentifyVatClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "clientIdentifier"       -> "202949960",
              "registrationDate.year"  -> "2010",
              "registrationDate.month" -> "10",
              "registrationDate.day"   -> "10"),
            arn.value
          ))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showConfirmClient().url)

        journeyState.get should have[State](
          ConfirmClientPersonalVat("GDT", emptyBasket),
          List(
            IdentifyPersonalClient(HMRCMTDVAT, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket))
        )
      }

      "redirect to not matched when clientId and known fact don't match" in {
        givenVatRegisteredClientReturns(Vrn("202949960"), LocalDate.parse("2010-10-10"), 403)

        journeyState.set(
          IdentifyPersonalClient(HMRCMTDVAT, emptyBasket),
          List(SelectPersonalService(availableServices, emptyBasket), SelectClientType(emptyBasket)))

        val result = controller.submitIdentifyVatClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "clientIdentifier"       -> "202949960",
              "registrationDate.year"  -> "2010",
              "registrationDate.month" -> "10",
              "registrationDate.day"   -> "10"),
            arn.value
          ))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showNotMatched().url)

        journeyState.get should have[State](
          KnownFactNotMatched(emptyBasket),
          List(
            IdentifyPersonalClient(HMRCMTDVAT, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket))
        )
      }
    }

    "POST /agents/identify-irv-client" should {
      val request = FakeRequest("POST", "/agents/identify-irv-client")

      "redirect to confirm client" in {
        givenMatchingCitizenRecord(Nino(nino), LocalDate.parse("1990-10-10"))
        givenGetAllPendingInvitationsReturnsEmpty(arn, nino, HMRCPIR)
        givenAfiRelationshipNotFoundForAgent(arn, Nino(nino))
        givenCitizenDetailsAreKnownFor(nino, "Virginia", "Woolf")

        journeyState.set(
          IdentifyPersonalClient(HMRCPIR, emptyBasket),
          List(SelectPersonalService(availableServices, emptyBasket), SelectClientType(emptyBasket)))

        val result = controller.submitIdentifyIrvClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "clientIdentifier" -> nino,
              "dob.year"         -> "1990",
              "dob.month"        -> "10",
              "dob.day"          -> "10"),
            arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showConfirmClient().url)

        journeyState.get should have[State](
          ConfirmClientIrv("Virginia Woolf", emptyBasket),
          List(
            IdentifyPersonalClient(HMRCPIR, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket))
        )
      }

      "redirect to not matched when clientId and known fact don't match" in {
        givenNonMatchingCitizenRecord(Nino(nino), LocalDate.parse("1990-10-10"))

        journeyState.set(
          IdentifyPersonalClient(HMRCPIR, emptyBasket),
          List(SelectPersonalService(availableServices, emptyBasket), SelectClientType(emptyBasket)))

        val result = controller.submitIdentifyIrvClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "clientIdentifier" -> nino,
              "dob.year"         -> "1990",
              "dob.month"        -> "10",
              "dob.day"          -> "10"),
            arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showNotMatched().url)

        journeyState.get should have[State](
          KnownFactNotMatched(emptyBasket),
          List(
            IdentifyPersonalClient(HMRCPIR, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket))
        )
      }

    }

    "GET /agents/confirm-client" should {
      val request = FakeRequest("GET", "/agents/confirm-client")

      "show the confirm client page for ITSA service" in {
        givenTradingName(validNino, "Sylvia Plath")
        journeyState.set(
          ConfirmClientItsa("Sylvia Plath", emptyBasket),
          List(
            IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket))
        )

        val result = controller.showConfirmClient()(authorisedAsValidAgent(request, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyText(result, "Sylvia Plath")
        checkHtmlResultWithBodyMsgs(result, "confirm-client.header")

        journeyState.get should have[State](
          ConfirmClientItsa("Sylvia Plath", emptyBasket),
          List(
            IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket))
        )
      }

      "show the confirm client page for IRV service" in {
        givenCitizenDetailsAreKnownFor(validNino.value, "Virginia", "Woolf")
        journeyState.set(
          ConfirmClientIrv("Virginia Woolf", emptyBasket),
          List(
            IdentifyPersonalClient(HMRCPIR, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket))
        )

        val result = controller.showConfirmClient()(authorisedAsValidAgent(request, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyText(result, "Virginia Woolf")
        checkHtmlResultWithBodyMsgs(result, "confirm-client.header")

        journeyState.get should have[State](
          ConfirmClientIrv("Virginia Woolf", emptyBasket),
          List(
            IdentifyPersonalClient(HMRCPIR, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket))
        )
      }

      "show the confirm client page for VAT service" in {
        givenClientDetails(Vrn("202949960"))
        journeyState.set(
          ConfirmClientBusinessVat("GDT", emptyBasket),
          List(IdentifyBusinessClient(emptyBasket), SelectBusinessService(emptyBasket), SelectClientType(emptyBasket))
        )

        val result = controller.showConfirmClient()(authorisedAsValidAgent(request, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyText(result, "GDT")
        checkHtmlResultWithBodyMsgs(result, "confirm-client.header")

        journeyState.get should have[State](
          ConfirmClientBusinessVat("GDT", emptyBasket),
          List(IdentifyBusinessClient(emptyBasket), SelectBusinessService(emptyBasket), SelectClientType(emptyBasket))
        )
      }
    }

    "POST /agents/confirm-client" should {
      val request = FakeRequest("POST", "/agents/confirm-client")

      "redirect to the review authorisations page when yes is selected" in {
        journeyState.set(
          ConfirmClientPersonalVat("GDT", emptyBasket),
          List(
            IdentifyPersonalClient(HMRCMTDVAT, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket)
          )
        )

        val result = controller.submitConfirmClient(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showReviewAuthorisations().url)

        journeyState.get shouldBe Some(
          (
            ReviewAuthorisationsPersonal(emptyBasket),
            List(
              ConfirmClientPersonalVat("GDT", emptyBasket),
              IdentifyPersonalClient(HMRCMTDVAT, emptyBasket),
              SelectPersonalService(availableServices, emptyBasket),
              SelectClientType(emptyBasket)
            )))
      }

      "redirect to the identify-client page when no is selected" in {
        journeyState.set(
          ConfirmClientPersonalVat("GDT", emptyBasket),
          List(
            IdentifyPersonalClient(HMRCMTDVAT, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket))
        )

        val result = controller.submitConfirmClient(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)

        journeyState.get should have[State](
          IdentifyPersonalClient(HMRCMTDVAT, emptyBasket),
          List(
            ConfirmClientPersonalVat("GDT", emptyBasket),
            IdentifyPersonalClient(HMRCMTDVAT, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket)
          )
        )
      }
    }

    "GET /agents/review-authorisations" should {
      val fullBasket = Set(
        AuthorisationRequest(
          "James Client",
          Invitation(Some(ClientType.personal), HMRCMTDIT, nino, Some("BN114AW")),
          itemId = "ABC"))
      val request = FakeRequest("GET", "/agents/review-authorisations")

      "show the review authorisations page" in {
        journeyState.set(
          ReviewAuthorisationsPersonal(fullBasket),
          List(
            ConfirmClientItsa("GDT", fullBasket),
            IdentifyPersonalClient(HMRCMTDIT, fullBasket),
            SelectPersonalService(availableServices, fullBasket),
            SelectClientType(fullBasket)
          )
        )

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

        journeyState.get shouldBe Some(
          (
            ReviewAuthorisationsPersonal(fullBasket),
            List(
              ConfirmClientItsa("GDT", fullBasket),
              IdentifyPersonalClient(HMRCMTDIT, fullBasket),
              SelectPersonalService(availableServices, fullBasket),
              SelectClientType(fullBasket)
            )))
      }
    }

    "POST /agents/review-authorisations" should {
      val request = FakeRequest("POST", "/agents/review-authorisations")

      "redirect to invitation-sent page when no is selected" in {
        givenInvitationCreationSucceeds(arn, Some(personal), nino, invitationIdITSA, nino, "ni", HMRCMTDIT, "NI")
        givenAgentReferenceRecordExistsForArn(arn, "FOO")
        givenAgentReference(arn, nino, personal)
        journeyState.set(
          ReviewAuthorisationsPersonal(emptyBasket),
          List(
            ConfirmClientItsa("GDT", emptyBasket),
            IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket)
          )
        )

        val result = controller.authorisationsReviewed(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showInvitationSent().url)

        journeyState.get should have[State](
          InvitationSentPersonal("/invitations/personal/AB123456A/99-with-flake", None),
          List(
            ReviewAuthorisationsPersonal(emptyBasket),
            ConfirmClientItsa("GDT", emptyBasket),
            IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket)
          )
        )
      }

      "redirect to select-service when yes is selected" in {
        journeyState.set(
          ReviewAuthorisationsPersonal(emptyBasket),
          List(
            ConfirmClientItsa("GDT", emptyBasket),
            IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
            SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), emptyBasket),
            SelectClientType(emptyBasket)
          )
        )

        val result = controller.authorisationsReviewed(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSelectService().url)

        journeyState.get should have[State](
          SelectPersonalService(availableServices, emptyBasket),
          List(
            ReviewAuthorisationsPersonal(emptyBasket),
            ConfirmClientItsa("GDT", emptyBasket),
            IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket)
          )
        )
      }
    }
  }
}
