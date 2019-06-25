package uk.gov.hmrc.agentinvitationsfrontend.controllers

import org.joda.time.LocalDate
import org.scalatest.BeforeAndAfter
import play.api.Application
import play.api.mvc.Flash
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.Vrn
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationJourneyControllerISpec extends BaseISpec with StateAndBreadcrumbsMatchers with BeforeAndAfter {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  override implicit lazy val app: Application = appBuilder(featureFlags)
    .overrides(new TestAgentInvitationJourneyModule)
    .build()

  lazy val journeyState = app.injector.instanceOf[TestAgentInvitationJourneyService]
  lazy val controller: AgentInvitationJourneyController = app.injector.instanceOf[AgentInvitationJourneyController]
  lazy val externalUrls = app.injector.instanceOf[ExternalUrls]

  import journeyState.model.State
  import journeyState.model.State._

  val availableServices = Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)
  val emptyBasket = Set.empty[AuthorisationRequest]

  before {
    journeyState.clear(hc, ec)
  }

  "GET /agents" should {
    val request = FakeRequest("GET", "/agents")

    "redirect to /agents/client-type if no current state" in {
      journeyState.clear(hc, ec)
      val result = controller.agentsRoot()(request)
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientType().url)
      journeyState.get shouldBe None
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

    "show the client type page" when {
      "there is no journey history (first visit)" in {
        journeyState.clear(hc, ec)

        behave like itShowsClientTypePage(
          withBackLinkUrl = s"${externalUrls.agentServicesAccountUrl}/agent-services-account")

        journeyState.get should have[State](SelectClientType(emptyBasket), List.empty)
      }

      "the current state is SelectClientType but there's no breadcrumbs" in {
        journeyState.set(SelectClientType(emptyBasket), List.empty)

        behave like itShowsClientTypePage(
          withBackLinkUrl = s"${externalUrls.agentServicesAccountUrl}/agent-services-account")

        journeyState.get should have[State](SelectClientType(emptyBasket), List.empty)
      }

      "the current state is SelectClientType and there are breadcrumbs" in {
        journeyState.set(
          state = SelectClientType(emptyBasket),
          breadcrumbs = List(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
        )

        behave like itShowsClientTypePage(
          withBackLinkUrl = routes.AgentInvitationJourneyController.showInvitationSent().url)

        journeyState.get should have[State](
          state = SelectClientType(emptyBasket),
          breadcrumbs = List(InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
        )
      }

      def itShowsClientTypePage(withBackLinkUrl: String) = {
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

        checkResultContainsBackLink(result, withBackLinkUrl)
      }
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
      journeyState.get should have[State](SelectBusinessService, List(SelectClientType(emptyBasket)))
    }

    "redisplay the page with errors if nothing is selected" in {
      journeyState.set(SelectClientType(emptyBasket), Nil)

      val result =
        controller.submitClientType()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientType().url)
      flash(result) shouldBe Flash(Map("dummy" -> "")) // "dummy" comes from a workaround introduced in https://github.com/hmrc/play-fsm/releases/tag/v0.20.0

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
        journeyState.set(SelectBusinessService, List(SelectClientType(emptyBasket)))

        val result = controller.submitBusinessSelectService(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)

        journeyState.get should have[State](
          IdentifyBusinessClient,
          List(SelectBusinessService, SelectClientType(emptyBasket)))
      }
      "redirect to select-client-type when no is selected" in {
        journeyState.set(SelectBusinessService, List(SelectClientType(emptyBasket)))

        val result = controller.submitBusinessSelectService(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientType().url)

        journeyState.get should have[State](
          SelectClientType(emptyBasket),
          List(SelectBusinessService, SelectClientType(emptyBasket)))
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

    "GET /agents/identify-itsa-client" should {
      val request = FakeRequest("GET", "/agents/identify-itsa-client")
      "redirect to the identify client page" in {
        journeyState.set(
          IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
          List(SelectPersonalService(availableServices, emptyBasket), SelectClientType(emptyBasket)))

        val result = controller.identifyClientRedirect()(authorisedAsValidAgent(request, arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)
      }
    }

    "POST /agents/identify-itsa-client" should {
      val request = FakeRequest("POST", "/agents/identify-itsa-client")

      "redirect to /agents/confirm-client" when {
        "nino is uppercase" in {
          checkSubmitIdentifyItsaClient(submittedNinoStr = nino.toUpperCase)
        }

        "nino is lowercase (APB-3634 bug fix)" in {
          checkSubmitIdentifyItsaClient(submittedNinoStr = nino.toLowerCase)
        }

        def checkSubmitIdentifyItsaClient(submittedNinoStr: String) = {
          val nino = Nino(submittedNinoStr.toUpperCase)
          givenMatchingClientIdAndPostcode(nino, "BN114AW")
          givenTradingName(nino, "Sylvia Plath")

          journeyState.set(
            IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
            List(SelectPersonalService(availableServices, emptyBasket), SelectClientType(emptyBasket)))

          val result = controller.submitIdentifyItsaClient(
            authorisedAsValidAgent(
              request.withFormUrlEncodedBody(
                "clientIdentifier" -> submittedNinoStr,
                "postcode"         -> "BN114AW"
              ),
              arn.value))

          status(result) shouldBe 303
          redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showConfirmClient().url)

          journeyState.get should havePattern[State](
            {
              case ConfirmClientItsa(
                  AuthorisationRequest(
                    "Sylvia Plath",
                    ItsaInvitation(nino, Postcode("BN114AW"), _, _, _),
                    _,
                    _
                  ),
                  `emptyBasket`
                  ) =>
            },
            List(
              IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
              SelectPersonalService(availableServices, emptyBasket),
              SelectClientType(emptyBasket)
            )
          )
        }
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

      "redirect to /agents/confirm-client" in {
        givenVatRegisteredClientReturns(Vrn("202949960"), LocalDate.parse("2010-10-10"), 204)
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

        journeyState.get should havePattern[State](
          {
            case ConfirmClientPersonalVat(
                AuthorisationRequest(
                  "GDT",
                  VatInvitation(Some(`personal`), Vrn("202949960"), VatRegDate("2010-10-10"), _, _),
                  _,
                  _),
                `emptyBasket`) =>
          },
          List(
            IdentifyPersonalClient(HMRCMTDVAT, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket))
        )
      }

      "redirect to /agents/not-matched when clientId and known fact don't match" in {
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

    "POST /agents/identify-irv-client" when {
      val request = FakeRequest("POST", "/agents/identify-irv-client")

      "redirect to /agents/review-authorisations because flag is off" when {
        "submitted NINO is uppercase" in {
          checkSubmitIdentifyIrvClient(submittedNinoStr = nino.toUpperCase)
        }

        "submitted NINO is lowercase (APB-3525)" in {
          checkSubmitIdentifyIrvClient(submittedNinoStr = nino.toLowerCase)
        }

        def checkSubmitIdentifyIrvClient(submittedNinoStr: String) = {
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
                "clientIdentifier" -> submittedNinoStr,
                "dob.year"         -> "1990",
                "dob.month"        -> "10",
                "dob.day"          -> "10"),
              arn.value))

          status(result) shouldBe 303
          redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showReviewAuthorisations().url)

          journeyState.get should havePattern[State](
            { case ReviewAuthorisationsPersonal(basket) if basket.nonEmpty => },
            List(
              IdentifyPersonalClient(HMRCPIR, emptyBasket),
              SelectPersonalService(availableServices, emptyBasket),
              SelectClientType(emptyBasket))
          )
        }
      }

      "redirect to /agents/not-matched when clientId and known fact don't match" in {
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
          ConfirmClientItsa(
            AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW"))),
            emptyBasket),
          List(
            IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket))
        )

        val result = controller.showConfirmClient()(authorisedAsValidAgent(request, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyText(result, "Sylvia Plath")
        checkHtmlResultWithBodyMsgs(result, "confirm-client.header")

        journeyState.get should havePattern[State](
          {
            case ConfirmClientItsa(
                AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(`nino`), Postcode("BN114AW"), _, _, _), _, _),
                `emptyBasket`) =>
          },
          List(
            IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket))
        )
      }

      "show the confirm client page for IRV service" in {
        givenCitizenDetailsAreKnownFor(validNino.value, "Virginia", "Woolf")
        journeyState.set(
          ConfirmClientIrv(
            AuthorisationRequest("Virginia Woolf", PirInvitation(Nino(nino), DOB("1990-10-10"))),
            emptyBasket),
          List(
            IdentifyPersonalClient(HMRCPIR, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket))
        )

        val result = controller.showConfirmClient()(authorisedAsValidAgent(request, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyText(result, "Virginia Woolf")
        checkHtmlResultWithBodyMsgs(result, "confirm-client.header")

        journeyState.get should havePattern[State](
          {
            case ConfirmClientIrv(
                AuthorisationRequest("Virginia Woolf", PirInvitation(Nino(nino), DOB("1990-10-10"), _, _, _), _, _),
                `emptyBasket`) =>
          },
          List(
            IdentifyPersonalClient(HMRCPIR, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket))
        )
      }

      "show the confirm client page for VAT service" in {
        givenClientDetails(Vrn("202949960"))
        journeyState.set(
          ConfirmClientBusinessVat(
            AuthorisationRequest("GDT", VatInvitation(Some(business), Vrn(vrn), VatRegDate("10/10/10")))),
          List(IdentifyBusinessClient, SelectBusinessService, SelectClientType(emptyBasket))
        )

        val result = controller.showConfirmClient()(authorisedAsValidAgent(request, arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyText(result, "GDT")
        checkHtmlResultWithBodyMsgs(result, "confirm-client.header")

        journeyState.get should havePattern[State](
          {
            case ConfirmClientBusinessVat(
                AuthorisationRequest(
                  "GDT",
                  VatInvitation(Some(business), Vrn(vrn), VatRegDate("10/10/10"), _, _),
                  _,
                  _)) =>
          },
          List(IdentifyBusinessClient, SelectBusinessService, SelectClientType(emptyBasket))
        )
      }
    }

    "POST /agents/confirm-client" should {
      val request = FakeRequest("POST", "/agents/confirm-client")

      "redirect to the review authorisations page when yes is selected" in {
        givenGetAllPendingInvitationsReturnsEmpty(arn, vrn, HMRCMTDVAT)
        givenCheckRelationshipVatWithStatus(arn, vrn, 404)
        journeyState.set(
          ConfirmClientPersonalVat(
            AuthorisationRequest("GDT", VatInvitation(Some(personal), Vrn(vrn), VatRegDate("10/10/10"))),
            `emptyBasket`),
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

      }

      "redirect to the identify-client page when no is selected" in {
        journeyState.set(
          ConfirmClientPersonalVat(
            AuthorisationRequest("GDT", VatInvitation(Some(personal), Vrn(vrn), VatRegDate("10/10/10"))),
            `emptyBasket`),
          List(
            IdentifyPersonalClient(HMRCMTDVAT, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket))
        )

        val result = controller.submitConfirmClient(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)

        journeyState.get should have[State](IdentifyPersonalClient(HMRCMTDVAT, emptyBasket))
      }
    }

    "GET /agents/review-authorisations" should {
      val fullBasket = Set(
        AuthorisationRequest(
          "James Client",
          Invitation(Some(ClientType.personal), HMRCMTDIT, nino, "BN114AW"),
          itemId = "ABC"))
      val request = FakeRequest("GET", "/agents/review-authorisations")

      "show the review authorisations page" in {
        journeyState.set(
          ReviewAuthorisationsPersonal(fullBasket),
          List(
            ConfirmClientItsa(
              AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW"))),
              fullBasket),
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
          "Send their Income Tax updates through software",
          "James Client",
          "Do you need to add another authorisation for this client?"
        )

        journeyState.get should have[State](ReviewAuthorisationsPersonal(fullBasket))
      }
    }

    "POST /agents/review-authorisations" should {
      val request = FakeRequest("POST", "/agents/review-authorisations")

      "redirect to invitation-sent page when no is selected" in {
        givenInvitationCreationSucceeds(arn, Some(personal), nino, invitationIdITSA, nino, "ni", HMRCMTDIT, "NI")
        givenAgentReferenceRecordExistsForArn(arn, "FOO")
        givenAgentReference(arn, nino, personal)
        givenGetAgencyEmailAgentStub
        journeyState.set(
          ReviewAuthorisationsPersonal(emptyBasket),
          List(
            ConfirmClientItsa(
              AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW"))),
              emptyBasket),
            IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
            SelectPersonalService(availableServices, emptyBasket),
            SelectClientType(emptyBasket)
          )
        )

        val result = controller.submitReviewAuthorisations(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showInvitationSent().url)

        journeyState.get should have[State](
          InvitationSentPersonal("/invitations/personal/AB123456A/99-with-flake", None, "abc@xyz.com"))
      }

      "redirect to select-service when yes is selected" in {
        journeyState.set(
          ReviewAuthorisationsPersonal(emptyBasket),
          List(
            ConfirmClientItsa(
              AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW"))),
              emptyBasket),
            IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
            SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), emptyBasket),
            SelectClientType(emptyBasket)
          )
        )

        val result = controller.submitReviewAuthorisations(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSelectService().url)

        journeyState.get should have[State](SelectPersonalService(availableServices, emptyBasket))
      }
    }
    "GET /invitation-sent" should {
      "show the invitation sent page" in {
        journeyState.set(
          InvitationSentPersonal("invitation/link", None, "abc@xyz.com"),
          List(
            ReviewAuthorisationsPersonal(Set.empty),
            ConfirmClientItsa(
              AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW"))),
              emptyBasket),
            IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
            SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), emptyBasket),
            SelectClientType(emptyBasket)
          )
        )
        val result = controller.showInvitationSent()(authorisedAsValidAgent(request, arn.value))

        status(result) shouldBe 200

        checkHtmlResultWithBodyText(
          result,
          htmlEscapedMessage(
            "generic.title",
            htmlEscapedMessage("invitation-sent.header"),
            htmlEscapedMessage("title.suffix.agents"),
            htmlEscapedMessage("invitation-sent.email.p", "abc@xyz.com")
          )
        )

        journeyState.get should have[State](InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
    }
    "GET /delete" should {
      "show the delete page" in {
        journeyState.set(
          DeleteAuthorisationRequestPersonal(
            AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW")), itemId = "itemId"),
            Set.empty),
          List(
            ReviewAuthorisationsPersonal(Set.empty),
            ConfirmClientItsa(
              AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW"))),
              emptyBasket),
            IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
            SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), emptyBasket),
            SelectClientType(emptyBasket)
          )
        )
        val result = controller.showDeleteAuthorisation("ABC123")(authorisedAsValidAgent(request, arn.value))

        status(result) shouldBe 200

        checkHtmlResultWithBodyText(
          result,
          "Are you sure you want to remove your authorisation request for Sylvia Plath?",
          "You will not send them an authorisation request to send their Income Tax updates through software"
        )

        journeyState.get should have[State](
          DeleteAuthorisationRequestPersonal(
            AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW")), itemId = "itemId"),
            Set.empty))
      }
    }
    "POST /delete" should {
      "redirect to review-authorisations when yes is selected and there is something left in the basket" in {
        journeyState.set(
          DeleteAuthorisationRequestPersonal(
            AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW"))),
            Set(
              AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW")), itemId = "itemId"))
          ),
          List(
            ReviewAuthorisationsPersonal(
              Set(
                AuthorisationRequest(
                  "Sylvia Plath",
                  ItsaInvitation(Nino(nino), Postcode("BN114AW")),
                  itemId = "itemId"))),
            ConfirmClientItsa(
              AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW"))),
              emptyBasket),
            IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
            SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), emptyBasket),
            SelectClientType(emptyBasket)
          )
        )

        val result = controller.submitDeleteAuthorisation(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showReviewAuthorisations().url)

        journeyState.get should have[State](
          ReviewAuthorisationsPersonal(Set(
            AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW")), itemId = "itemId"))))
      }
      "redirect to all-authorisations-removed when yes is selected and there is nothing left in the basket" in {
        journeyState.set(
          DeleteAuthorisationRequestPersonal(
            AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW")), itemId = "ABC123"),
            Set(
              AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW")), itemId = "ABC123"))
          ),
          List(
            ReviewAuthorisationsPersonal(Set.empty),
            ConfirmClientItsa(
              AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW"))),
              emptyBasket),
            IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
            SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), emptyBasket),
            SelectClientType(emptyBasket)
          )
        )

        val result = controller.submitDeleteAuthorisation(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(
          routes.AgentInvitationJourneyController.showAllAuthorisationsRemoved().url)

        journeyState.get should have[State](AllAuthorisationsRemoved)
      }
      "redirect to review-authorisations when no is selected and keep basket the same" in {
        journeyState.set(
          DeleteAuthorisationRequestPersonal(
            AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW")), itemId = "ABC123"),
            Set(
              AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW")), itemId = "ABC123"))
          ),
          List(
            ReviewAuthorisationsPersonal(Set.empty),
            ConfirmClientItsa(
              AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW"))),
              emptyBasket),
            IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
            SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), emptyBasket),
            SelectClientType(emptyBasket)
          )
        )

        val result = controller.submitDeleteAuthorisation(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showReviewAuthorisations().url)

        journeyState.get should have[State](
          ReviewAuthorisationsPersonal(Set(
            AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW")), itemId = "ABC123"))))
      }
    }
    "POST /some-create-authorisations-failed" should {
      "redirect to invitation sent" in {
        journeyState.set(
          SomeAuthorisationsFailed("invitation/link", None, "abc@xyz.com", Set.empty),
          List(
            SomeAuthorisationsFailed("invitation/link", None, "abc@xyz.com", Set.empty),
            ReviewAuthorisationsPersonal(Set.empty),
            ConfirmClientItsa(
              AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino), Postcode("BN114AW"))),
              emptyBasket),
            IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
            SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), emptyBasket),
            SelectClientType(emptyBasket)
          )
        )

        val result = controller.submitSomeAuthorisationsFailed(authorisedAsValidAgent(request, arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showInvitationSent().url)

        journeyState.get should have[State](InvitationSentPersonal("invitation/link", None, "abc@xyz.com"))
      }
    }
  }
}
