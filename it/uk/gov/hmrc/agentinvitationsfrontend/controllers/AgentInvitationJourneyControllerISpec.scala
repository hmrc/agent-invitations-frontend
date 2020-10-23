package uk.gov.hmrc.agentinvitationsfrontend.controllers

import org.joda.time.LocalDate
import org.scalatest.BeforeAndAfter
import play.api.Application
import play.api.libs.json.Json
import play.api.mvc.Flash
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.Vrn
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationJourneyControllerISpec extends BaseISpec with StateAndBreadcrumbsMatchers with BeforeAndAfter {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  override implicit lazy val app: Application = appBuilder
    .overrides(new TestAgentInvitationJourneyModule)
    .build()

  lazy private val journeyState = app.injector.instanceOf[TestAgentInvitationJourneyService]
  lazy private val controller: AgentInvitationJourneyController = app.injector.instanceOf[AgentInvitationJourneyController]
  lazy private val externalUrls = app.injector.instanceOf[ExternalUrls]

  import journeyState.model._

  private val availableServices = Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT, HMRCCGTPD)
  private val availableTrustServices = Set(TRUST, HMRCCGTPD)
  private val emptyBasket = Set.empty[AuthorisationRequest]

  before {
    journeyState.clear
  }

  "GET /agents" should {
    val request = FakeRequest("GET", "/agents")

    "redirect to /agents/client-type if no current state" in {
      journeyState.clear
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
        journeyState.clear

        behave like itShowsClientTypePage(
          withBackLinkUrl = externalUrls.agentServicesAccountUrl)

        journeyState.get should have[State](SelectClientType(emptyBasket), List.empty)
      }

      "the current state is SelectClientType but there's no breadcrumbs" in {
        journeyState.set(SelectClientType(emptyBasket), List.empty)

        behave like itShowsClientTypePage(
          withBackLinkUrl = externalUrls.agentServicesAccountUrl)

        journeyState.get should have[State](SelectClientType(emptyBasket), List.empty)
      }

      "the current state is SelectClientType and there are breadcrumbs" in {
        journeyState.set(
          state = SelectClientType(emptyBasket),
          breadcrumbs = List(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Set.empty))
        )

        behave like itShowsClientTypePage(
          withBackLinkUrl = routes.AgentInvitationJourneyController.showInvitationSent().url)

        journeyState.get should have[State](
          state = SelectClientType(emptyBasket),
          breadcrumbs = List(InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Set.empty))
        )
      }

      def itShowsClientTypePage(withBackLinkUrl: String): Unit = {
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

    "redirect to /agents/select-service after selecting trust client type" in {
      journeyState.set(SelectClientType(emptyBasket), Nil)

      val result =
        controller.submitClientType(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> "trust"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSelectService().url)
      journeyState.get should have[State](SelectTrustService(availableTrustServices, emptyBasket), List(SelectClientType(emptyBasket)))
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
  }

  "GET /agents/select-service" should {
    val request = FakeRequest("GET", "/agents/select-service")

    "show the select service page for a personal service" in {
      journeyState.set(SelectPersonalService(availableServices, emptyBasket), List(SelectClientType(emptyBasket)))
      val result = controller.showSelectService()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("select-service.header"),
          htmlEscapedMessage("title.suffix.agents")),
        htmlEscapedMessage("select-service.HMRC-MTD-IT.personal"),
        htmlEscapedMessage("select-service.PERSONAL-INCOME-RECORD.personal"),
        htmlEscapedMessage("select-service.HMRC-MTD-VAT.personal")
      )
      journeyState.get shouldBe Some(
        (SelectPersonalService(availableServices, emptyBasket), List(SelectClientType(emptyBasket))))
    }

    "show the select service page for a business service" in {
      journeyState.set(SelectBusinessService, List(SelectClientType(emptyBasket)))
      val result = controller.showSelectService()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("select-service.no"),
        htmlEscapedMessage("select-service.yes"),
        htmlEscapedMessage("select-service.alternative")
      )
      journeyState.get shouldBe Some((SelectBusinessService, List(SelectClientType(emptyBasket))))
    }

    "show the correct service page content for trust clients" in {
      journeyState.set(SelectTrustService(Set(TRUST), emptyBasket), List(SelectClientType(emptyBasket)))
      val result = controller.showSelectService()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("select-single-service.HMRC-TERS-ORG.business.header"),
        htmlEscapedMessage("select-service.yes"),
        htmlEscapedMessage("select-service.no")
      )
      journeyState.get shouldBe Some((SelectTrustService(Set(TRUST), emptyBasket), List(SelectClientType(emptyBasket))))
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
        htmlEscapedMessage("select-service.HMRC-MTD-IT.personal"),
        htmlEscapedMessage("select-service.PERSONAL-INCOME-RECORD.personal"),
        htmlEscapedMessage("select-service.HMRC-MTD-VAT.personal")
      )
      journeyState.get should have[State](
        SelectPersonalService(availableServices, emptyBasket),
        List(SelectClientType(emptyBasket)))
    }
  }

  "POST /agents/select-personal-service" should {
    val request = FakeRequest("POST", "/agents/select-personal-service")

    "accept valid service choice and redirect" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
      journeyState.set(SelectPersonalService(availableServices, emptyBasket), List(SelectClientType(emptyBasket)))

      val result = controller.submitPersonalSelectService(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> "HMRC-MTD-IT"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)

      journeyState.get should have[State](
        IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
        List(SelectPersonalService(availableServices, emptyBasket), SelectClientType(emptyBasket)))
    }

    "redirect to agent suspended if the agent is suspended for the selected service" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = true, Some(Set("ITSA"))))
      journeyState.set(SelectPersonalService(availableServices, emptyBasket), List(SelectClientType(emptyBasket)))

      val result = controller.submitPersonalSelectService(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> "HMRC-MTD-IT"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showAgentSuspended().url)

      journeyState.get should have[State](
        AgentSuspended(HMRCMTDIT, emptyBasket),
        List(SelectPersonalService(availableServices, emptyBasket), SelectClientType(emptyBasket)))
    }
  }

  "POST /agents/select-business-service" should {
    val request = FakeRequest("POST", "/agents/select-business-service")

    "redirect to identify-client when yes is selected" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
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

  "POST /agents/select-trust-service" should {

    val request = FakeRequest("POST", "/agents/select-trust-service")

    "redirect to identify-client when yes is selected" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
      journeyState.set(SelectTrustService(availableTrustServices, emptyBasket), List(SelectClientType(emptyBasket)))

      val result = controller.submitTrustSelectSingle(TRUST)(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)

      journeyState.get should have[State](IdentifyTrustClient(TRUST, emptyBasket),
        List(SelectTrustService(availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))
    }

    "redirect to select-client-type when no is selected" in {

      journeyState.set(SelectTrustService(availableTrustServices, emptyBasket), List(SelectClientType(emptyBasket)))

      val result = controller.submitTrustSelectSingle(TRUST)(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientType().url)

      journeyState.get should have[State](
        SelectClientType(emptyBasket),
        List(SelectTrustService(availableTrustServices, emptyBasket), SelectClientType(emptyBasket)))
    }

    "do not blow up if user enters invalid value in the form for confirmation" in {

      journeyState.set(SelectTrustService(availableTrustServices, emptyBasket), List(SelectClientType(emptyBasket)))

      val result = controller.submitTrustSelectSingle(TRUST)(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "foo"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSelectService().url)
    }
  }

  "GET /agents/identify-client" should {
    val request = FakeRequest("GET", "/agents/identify-client")

    "show identify client page" in {
      journeyState.set(
        IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
        List(SelectPersonalService(availableServices, emptyBasket), SelectClientType(emptyBasket)))

      controller.showIdentifyClient()(authorisedAsValidAgent(request, arn.value))
    }

    "show identify client page for IRV service" in {
      journeyState.set(
        IdentifyPersonalClient(HMRCPIR, emptyBasket),
        List(SelectPersonalService(availableServices, emptyBasket), SelectClientType(emptyBasket)))

      val result = controller.showIdentifyClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "identify-client.header",
        "identify-client.itsa.p1",
        "identify-client.nino.label",
        "identify-client.nino.hint",
        "identify-client.irv-date-of-birth.label",
        "identify-client.irv-date-of-birth.hint"
      )

      journeyState.get should have[State](
        IdentifyPersonalClient(HMRCPIR, emptyBasket),
        List(SelectPersonalService(availableServices, emptyBasket), SelectClientType(emptyBasket)))
    }

    "show identify client page for personal VAT service" in {
      journeyState.set(
        IdentifyPersonalClient(HMRCMTDVAT, emptyBasket),
        List(SelectPersonalService(availableServices, emptyBasket), SelectClientType(emptyBasket)))

      val result = controller.showIdentifyClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "identify-client.header",
        "identify-client.vat.p1",
        "identify-client.vrn.label",
        "identify-client.vrn.hint",
        "identify-client.vat-registration-date.label",
        "identify-client.vat-registration-date.hint"
      )

      journeyState.get should have[State](
        IdentifyPersonalClient(HMRCMTDVAT, emptyBasket),
        List(SelectPersonalService(availableServices, emptyBasket), SelectClientType(emptyBasket)))
    }

    "show the identify client page for business VAT service" in {
      journeyState.set(IdentifyBusinessClient, List(SelectBusinessService, SelectClientType(emptyBasket)))

      val result = controller.showIdentifyClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "identify-client.header",
        "identify-client.vat.p1",
        "identify-client.vrn.label",
        "identify-client.vrn.hint",
        "identify-client.vat-registration-date.label",
        "identify-client.vat-registration-date.hint"
      )

      journeyState.get should have[State](
        IdentifyBusinessClient,
        List(SelectBusinessService, SelectClientType(emptyBasket)))
    }

    "show the identify client page for TRUST service" in {

      journeyState.set(
        IdentifyTrustClient(TRUST, emptyBasket),
        List(
          SelectTrustService(availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.showIdentifyClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "identify-trust-client.header",
        "identify-trust-client.p1",
        "continue.button"
      )

      journeyState.get should have[State](
        IdentifyTrustClient(TRUST, emptyBasket),
        List(
          SelectTrustService(availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))
    }

    "show the identify client page for Personal CGT service" in {

      journeyState.set(
        IdentifyPersonalClient(HMRCCGTPD, emptyBasket),
        List(
          SelectPersonalService(availableServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.showIdentifyClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "identify-cgt-client.header",
        "identify-cgt-client.p1",
        "identify-cgt-client.hint",
        "continue.button"
      )

      journeyState.get should have[State](
        IdentifyPersonalClient(HMRCCGTPD, emptyBasket),
        List(
          SelectPersonalService(availableServices, emptyBasket),
          SelectClientType(emptyBasket)))
    }

    "show the identify client page for Trust CGT service" in {

      journeyState.set(
        IdentifyTrustClient(HMRCCGTPD, emptyBasket),
        List(
          SelectTrustService(availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.showIdentifyClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "identify-cgt-client.header",
        "identify-cgt-client.p1",
        "identify-cgt-client.hint",
        "continue.button"
      )

      journeyState.get should have[State](
        IdentifyTrustClient(HMRCCGTPD, emptyBasket),
        List(
          SelectTrustService(availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))
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
                  ItsaInvitation(_, _, _, _),
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

    "redirect to client not signed up when the client is not signed up for the service" in {
      givenNotEnrolledClientITSA(Nino(nino), "BN114AW")

      journeyState.set(IdentifyPersonalClient(HMRCMTDIT, emptyBasket), List())

      val result = controller.submitIdentifyItsaClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("clientIdentifier" -> nino, "postcode" -> "BN114AW"),
          arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientNotSignedUp().url)
    }
  }

  "POST /agents/identify-vat-client" should {
    val request = FakeRequest("POST", "/agents/identify-vat-client")

    "redirect to /agents/confirm-client for personal vat" in {
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
                VatInvitation(Some(`personal`), Vrn("202949960"), _, _),
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

    "redirect to /agents/confirm-client for business vat" in {
      givenVatRegisteredClientReturns(Vrn("202949960"), LocalDate.parse("2010-10-10"), 204)
      givenClientDetails(Vrn("202949960"))

      journeyState.set(IdentifyBusinessClient, List())

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
          case ConfirmClientBusinessVat(
              AuthorisationRequest(
                "GDT",
                VatInvitation(Some(`business`), Vrn("202949960"), _, _),
                _,
                _)) =>
        },
        List(IdentifyBusinessClient)
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
    "redirect to /agents/cannot-create-request when  there is a migration in progress" in {
      givenVatRegisteredClientReturns(Vrn("202949960"), LocalDate.parse("2010-10-10"), 423)

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
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showCannotCreateRequest().url)

    }
  }

  "POST /agents/identify-trust-client" should {

    val request = FakeRequest("POST", "/agents/identify-trust-client")

    "redirect to /agents/confirm-client" in {
      givenTrustClientReturns(validUtr, 200, Json.toJson(trustResponse).toString())

      journeyState.set(
        IdentifyTrustClient(TRUST, emptyBasket),
        List(SelectTrustService(availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("utr" -> validUtr.value),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showConfirmClient().url)

      journeyState.get should havePattern[State](
        {
          case ConfirmClientTrust(AuthorisationRequest("some-trust", TrustInvitation(_, _, _, _), _, _), _) =>
        },
        List(
          IdentifyTrustClient(TRUST, emptyBasket),
          SelectTrustService(availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }

    "handle invalid Utr passed in by the user and redirect back to previous /identify-client page " in {

      journeyState.set(IdentifyTrustClient(TRUST, emptyBasket), List(SelectTrustService(availableTrustServices, emptyBasket), SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("utr" -> "493745"),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)
    }

    "redirect to /agents/not-found when utr passed in does not match to any trust" in {
      givenTrustClientReturns(validUtr, 200, trustNotFoundJson)

      journeyState.set(
        IdentifyTrustClient(TRUST, emptyBasket),
        List(SelectTrustService(availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("utr" -> validUtr.value),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showNotMatched().url)

      journeyState.get should have[State](
        TrustNotFound(emptyBasket),
        List(IdentifyTrustClient(TRUST, emptyBasket),
          SelectTrustService(availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }

    "redirect to /agents/not-found when trust is in invalid state" in {

      givenTrustClientReturns(validUtr, 200, invalidTrustJson)

      journeyState.set(
        IdentifyTrustClient(TRUST, emptyBasket),
        List(SelectTrustService(availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("utr" -> validUtr.value),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showNotMatched().url)

      journeyState.get should have[State](
        TrustNotFound(emptyBasket),
        List(
          IdentifyTrustClient(TRUST, emptyBasket),
          SelectTrustService(availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }
  }

  "POST /agents/identify-cgt-client" should {

    val request = FakeRequest("POST", "/agents/identify-cgt-client")

    "redirect to /agents/client-postcode" in {
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription("GB")).toString())

      journeyState.set(
        IdentifyTrustClient(HMRCCGTPD, emptyBasket),
        List(SelectTrustService(availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyCgtClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("cgtRef" -> cgtRef.value),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showConfirmCgtPostcode().url)

      journeyState.get should havePattern[State](
        {
          case ConfirmPostcodeCgt(cgtRef, personal, emptyBasket, _, _) =>
        },
        List(
          IdentifyTrustClient(HMRCCGTPD, emptyBasket),
          SelectTrustService(availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }

    "handle invalid CgtRef passed in by the user and redirect back to previous /identify-client page " in {

      journeyState.set(IdentifyTrustClient(HMRCCGTPD, emptyBasket), List(SelectTrustService(availableTrustServices, emptyBasket), SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("cgtRef" -> "12345"),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)
    }

    "redirect to /agents/not-matched when cgtRef passed in does not match to any cgt client" in {
      givenGetCgtSubscriptionReturns(cgtRef, 404, cgtNotFoundJson)

      journeyState.set(
        IdentifyTrustClient(HMRCCGTPD, emptyBasket),
        List(SelectTrustService(availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyCgtClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("cgtRef" -> cgtRef.value),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showNotMatched().url)

      journeyState.get should have[State](
        CgtRefNotFound(cgtRef, emptyBasket),
        List(IdentifyTrustClient(HMRCCGTPD, emptyBasket),
          SelectTrustService(availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }
  }

  "GET /agents/client-postcode" should {

    val request = FakeRequest("GET", "/agents/client-postcode")

    "display the page as expected" in {

      journeyState.set(
        ConfirmPostcodeCgt(cgtRef, personal, emptyBasket, Some("BN13 1FN"), "firstName lastName"),
        List(
          IdentifyPersonalClient(HMRCCGTPD, emptyBasket),
          SelectPersonalService(availableServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.showConfirmCgtPostcode()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "confirm-postcode-cgt.header",
        "confirm-postcode-cgt.p1",
        "confirm-postcode-cgt.hint",
        "continue.button"
      )

      journeyState.get should have[State](
        ConfirmPostcodeCgt(cgtRef, personal, emptyBasket, Some("BN13 1FN"), "firstName lastName"),
        List(
          IdentifyPersonalClient(HMRCCGTPD, emptyBasket),
          SelectPersonalService(availableServices, emptyBasket),
          SelectClientType(emptyBasket)))
    }
  }

  "POST /agents/client-postcode" should {

    val request = FakeRequest("POST", "/agents/client-postcode")

    "redirect to /confirm-client if postcode matches for a UK client" in {

      journeyState.set(
        ConfirmPostcodeCgt(cgtRef, personal, emptyBasket, Some("BN13 1FN"), "firstName lastName"),
        List(
          IdentifyPersonalClient(HMRCCGTPD, emptyBasket),
          SelectPersonalService(availableServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitConfirmCgtPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody("postcode" -> "BN13 1FN"), arn.value))

      status(result) shouldBe 303

      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showConfirmClient().url)

      journeyState.get should havePattern[State](
        {case ConfirmClientCgt(AuthorisationRequest("firstName lastName", CgtInvitation(cgtRef, Some(personal), _, _), _, _), emptyBasket) => },
        List(
          ConfirmPostcodeCgt(cgtRef, personal, emptyBasket, Some("BN13 1FN"), "firstName lastName"),
          IdentifyPersonalClient(HMRCCGTPD, emptyBasket),
          SelectPersonalService(availableServices, emptyBasket),
          SelectClientType(emptyBasket)))
    }

    "redirect to /not-matched if postcode does not matche for a UK client" in {

      journeyState.set(
        ConfirmPostcodeCgt(cgtRef, personal, emptyBasket, Some("BN13 1FN"), "firstName lastName"),
        List(
          IdentifyPersonalClient(HMRCCGTPD, emptyBasket),
          SelectPersonalService(availableServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitConfirmCgtPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody("postcode" -> "BN13 1XX"), arn.value))

      status(result) shouldBe 303

      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showNotMatched().url)
    }
  }

  "GET /agents/client-country" should {

    val request = FakeRequest("GET", "/agents/client-country")

    "display the page as expected" in {

      journeyState.set(
        ConfirmCountryCodeCgt(cgtRef, personal, emptyBasket, "FR", "firstName lastName"),
        List(
          IdentifyPersonalClient(HMRCCGTPD, emptyBasket),
          SelectPersonalService(availableServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.showConfirmCgtCountryCode()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "confirm-countryCode-cgt.header",
        "confirm-countryCode-cgt.p1",
        "confirm-countryCode-cgt.hint",
        "continue.button"
      )

      journeyState.get should have[State](
        ConfirmCountryCodeCgt(cgtRef, personal, emptyBasket, "FR", "firstName lastName"),
        List(
          IdentifyPersonalClient(HMRCCGTPD, emptyBasket),
          SelectPersonalService(availableServices, emptyBasket),
          SelectClientType(emptyBasket)))
    }
  }

  "POST /agents/client-country" should {

    val request = FakeRequest("POST", "/agents/client-country")

    "redirect to /confirm-client if country code matches for a non UK client" in {

      journeyState.set(
        ConfirmCountryCodeCgt(cgtRef, personal, emptyBasket, "FR", "firstName lastName"),
        List(
          IdentifyPersonalClient(HMRCCGTPD, emptyBasket),
          SelectPersonalService(availableServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitConfirmCgtCountryCode(authorisedAsValidAgent(request.withFormUrlEncodedBody("countryCode" -> "FR"), arn.value))

      status(result) shouldBe 303

      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showConfirmClient().url)

      journeyState.get should havePattern[State](
        {case ConfirmClientCgt(AuthorisationRequest("firstName lastName", CgtInvitation(cgtRef, Some(personal), _, _), _, _), emptyBasket) => },
        List(
          ConfirmCountryCodeCgt(cgtRef, personal, emptyBasket, "FR", "firstName lastName"),
          IdentifyPersonalClient(HMRCCGTPD, emptyBasket),
          SelectPersonalService(availableServices, emptyBasket),
          SelectClientType(emptyBasket)))
    }

    "redirect to /not-matched if country code does not match for a non UK client" in {

      journeyState.set(
        ConfirmCountryCodeCgt(cgtRef, personal, emptyBasket, "FR", "firstName lastName"),
        List(
          IdentifyPersonalClient(HMRCCGTPD, emptyBasket),
          SelectPersonalService(availableServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitConfirmCgtCountryCode(authorisedAsValidAgent(request.withFormUrlEncodedBody("countryCode" -> "IN"), arn.value))

      status(result) shouldBe 303

      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showNotMatched().url)
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
          { case ReviewAuthorisationsPersonal(_, basket) if basket.nonEmpty => },
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
          AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino))),
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
              AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(`nino`), _, _, _), _, _),
              `emptyBasket`) =>
        },
        List(
          IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
          SelectPersonalService(availableServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }

    "show the confirm client page for personal VAT service" in {
      givenClientDetails(Vrn("202949960"))
      journeyState.set(
        ConfirmClientPersonalVat(
          AuthorisationRequest("GDT", VatInvitation(Some(business), Vrn(vrn))),
          emptyBasket),
        List(
          IdentifyPersonalClient(HMRCMTDVAT, emptyBasket),
          SelectPersonalService(availableServices, emptyBasket),
          SelectClientType(emptyBasket))
      )

      val result = controller.showConfirmClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "GDT")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")

      journeyState.get should havePattern[State](
        {
          case ConfirmClientPersonalVat(
              AuthorisationRequest("GDT", VatInvitation(Some(_), Vrn(_), _, _), _, _),
              _) =>
        },
        List(
          IdentifyPersonalClient(HMRCMTDVAT, emptyBasket),
          SelectPersonalService(availableServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }

    "show the confirm client page for Trust service" in {

      givenTrustClientReturns(validUtr, 200, Json.toJson(trustResponse).toString())

      journeyState.set(
        ConfirmClientTrust(AuthorisationRequest("Nelson James Trust", TrustInvitation(validUtr)), emptyBasket),
        List(
          IdentifyTrustClient(TRUST, emptyBasket),
          SelectTrustService(availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )

      val result = controller.showConfirmClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Nelson James Trust")
      checkHtmlResultWithBodyText(result, s"Unique Taxpayer Reference: ${validUtr.value}")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")

      journeyState.get should havePattern[State](
        {
          case ConfirmClientTrust(AuthorisationRequest("Nelson James Trust", TrustInvitation(_, _, _, _), _, _), _) =>
        },
        List(
          IdentifyTrustClient(TRUST, emptyBasket),
          SelectTrustService(availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }

    "show the confirm client page for business VAT service" in {
      givenClientDetails(Vrn("202949960"))
      journeyState.set(
        ConfirmClientBusinessVat(
          AuthorisationRequest("GDT", VatInvitation(Some(business), Vrn(vrn)))),
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
                VatInvitation(Some(_), Vrn(_), _, _),
                _,
                _)) =>
        },
        List(IdentifyBusinessClient, SelectBusinessService, SelectClientType(emptyBasket))
      )
    }

    "show the confirm client page for CGT clients" in {
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription()).toString())
      journeyState.set(
        ConfirmClientCgt(AuthorisationRequest("CGT_NAME", CgtInvitation(cgtRef, Some(business))), emptyBasket),
        List(ConfirmCountryCodeCgt(cgtRef, business, emptyBasket, "FR", "firstName lastName"), SelectBusinessService, SelectClientType(emptyBasket))
      )

      val result = controller.showConfirmClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "CGT_NAME")
      checkHtmlResultWithBodyText(result, "Capital Gains Tax account reference: XMCGTP123456789")
      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")
    }
  }

  "POST /agents/confirm-client" should {
    val request = FakeRequest("POST", "/agents/confirm-client")

    "redirect to the review authorisations page when yes is selected" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, vrn, HMRCMTDVAT)
      givenCheckRelationshipVatWithStatus(arn, vrn, 404)
      journeyState.set(
        ConfirmClientPersonalVat(
          AuthorisationRequest("GDT", VatInvitation(Some(personal), Vrn(vrn))),
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

    "redirect to invitation sent for business" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, vrn, HMRCMTDVAT)
      givenCheckRelationshipVatWithStatus(arn, vrn, 404)
      givenInvitationCreationSucceeds(arn, Some(business), vrn, invitationIdVAT, vrn, "vrn", HMRCMTDVAT, "VRN")
      givenAgentReferenceRecordExistsForArn(arn, "FOO")
      givenAgentReference(arn, nino, business)
      givenGetAgencyEmailAgentStub
      journeyState.set(
        ConfirmClientBusinessVat(
          AuthorisationRequest("GDT", VatInvitation(Some(business), Vrn(vrn)))),
        List(
          IdentifyBusinessClient,
          SelectBusinessService,
          SelectClientType(emptyBasket)
        )
      )

      val result = controller.submitConfirmClient(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showInvitationSent().url)
    }

    "redirect to the identify-client page when no is selected" in {
      journeyState.set(
        ConfirmClientPersonalVat(
          AuthorisationRequest("GDT", VatInvitation(Some(personal), Vrn(vrn))),
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

    "redirect to pending invitation exists when a pending invitation already exists for this service" in {
      givenGetAllPendingInvitationsReturnsSome(arn, vrn, HMRCMTDVAT)
      givenCheckRelationshipVatWithStatus(arn, vrn, 404)
      journeyState.set(
        ConfirmClientPersonalVat(
          AuthorisationRequest("GDT", VatInvitation(Some(personal), Vrn(vrn))),
          `emptyBasket`),
        List()
      )

      val result = controller.submitConfirmClient(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(
        routes.AgentInvitationJourneyController.showPendingAuthorisationExists().url)
    }

    "redirect to active authorisation exists when there is already an active authorisation for this service" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, vrn, HMRCMTDVAT)
      givenCheckRelationshipVatWithStatus(arn, vrn, 200)
      journeyState.set(
        ConfirmClientPersonalVat(
          AuthorisationRequest("GDT", VatInvitation(Some(personal), Vrn(vrn))),
          `emptyBasket`),
        List())

      val result = controller.submitConfirmClient(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(
        routes.AgentInvitationJourneyController.showActiveAuthorisationExists().url)
    }

    "redirect to active authorisation exists when there is already an active authorisation for Trusts" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validUtr.value, TRUST)
      givenCheckRelationshipTrustWithStatus(arn, validUtr.value, 200)
      givenAgentReferenceRecordExistsForArn(arn, "FOO")
      givenAgentReference(arn, nino, business)
      givenGetAgencyEmailAgentStub

      journeyState.set(
        ConfirmClientTrust(AuthorisationRequest("GDT", TrustInvitation(validUtr, Some(business))), emptyBasket),
        List())

      val result = controller.submitConfirmClient(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(
        routes.AgentInvitationJourneyController.showActiveAuthorisationExists().url)
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
        ReviewAuthorisationsPersonal(availableServices, fullBasket),
        List(
          ConfirmClientItsa(
            AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino))),
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
        "Check your authorisation requests",
        "You have added 1 authorisation request.",
        "Manage their Income Tax",
        "James Client",
        "Do you need to add another authorisation for this client?"
      )

      journeyState.get should have[State](ReviewAuthorisationsPersonal(availableServices, fullBasket))
    }
  }

  "POST /agents/review-authorisations" should {
    val request = FakeRequest("POST", "/agents/review-authorisations")

    "redirect to invitation-sent page for a personal service when no is selected" in {
      givenInvitationCreationSucceeds(arn, Some(personal), nino, invitationIdITSA, nino, "ni", HMRCMTDIT, "NI")
      givenAgentReferenceRecordExistsForArn(arn, "FOO")
      givenAgentReference(arn, nino, personal)
      givenGetAgencyEmailAgentStub
      journeyState.set(
        ReviewAuthorisationsPersonal(availableServices, emptyBasket),
        List(
          ConfirmClientItsa(
            AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino))),
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
        InvitationSentPersonal("/invitations/personal/AB123456A/99-with-flake", None, "abc@xyz.com", Set.empty))
    }

    "redirect to select-service when yes is selected" in {
      journeyState.set(
        ReviewAuthorisationsPersonal(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), emptyBasket),
        List(
          ConfirmClientItsa(
            AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino))),
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

    "redirect to some authorisations failed when some of the invitation creations fail" in {
      givenInvitationCreationSucceeds(arn, Some(personal), nino, invitationIdITSA, nino, "ni", HMRCMTDIT, "NI")
      givenInvitationCreationFailsForService(arn, Some(personal), nino, invitationIdPIR, nino, "ni", HMRCPIR, "NI")
      givenAgentReferenceRecordExistsForArn(arn, "FOO")
      givenAgentReference(arn, nino, personal)
      givenGetAgencyEmailAgentStub
      journeyState.set(
        ReviewAuthorisationsPersonal(
          availableServices,
          Set(
            AuthorisationRequest("client name", Invitation(Some(personal), HMRCMTDIT, nino, validPostcode)),
            AuthorisationRequest("client name", Invitation(Some(personal), HMRCPIR, nino, dateOfBirth))
          )),
        List()
      )

      val result = controller.submitReviewAuthorisations(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSomeAuthorisationsFailed().url)
    }

    "redirect to all authorisation failed when all of the invitation creations fail" in {
      givenInvitationCreationFailsForService(arn, Some(personal), nino, invitationIdPIR, nino, "ni", HMRCPIR, "NI")
      givenAgentReferenceRecordExistsForArn(arn, "FOO")
      givenAgentReference(arn, nino, personal)
      givenGetAgencyEmailAgentStub
      journeyState.set(
        ReviewAuthorisationsPersonal(
          availableServices,
          Set(
            AuthorisationRequest("client name", Invitation(Some(personal), HMRCPIR, nino, dateOfBirth))
          )),
        List()
      )

      val result = controller.submitReviewAuthorisations(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showAllAuthorisationsFailed().url)
    }
  }

  "GET /invitation-sent" should {
    val request = FakeRequest("GET", "/agents/invitation-sent")

    "show the invitation sent page for a personal service" in {
      journeyState.set(
        InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)),
        List(
          ReviewAuthorisationsPersonal(availableServices, Set.empty),
          ConfirmClientItsa(
            AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino))),
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
          htmlEscapedMessage("invitation-sent.email.p", "abc@xyz.com"),
          htmlEscapedMessage("invitation-sent.l46")
        )
      )

      checkInviteSentPageContainsSurveyLink(result, true)

      journeyState.get should have[State](InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)))
    }

    "show the invitation sent page for a business service" in {
      journeyState.set(
        InvitationSentBusiness("invitation/link", None, "abc@xyz.com", Set(HMRCMTDVAT)),
        List(
          ConfirmClientBusinessVat(
            AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino)))),
          IdentifyBusinessClient,
          SelectBusinessService,
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
          htmlEscapedMessage("invitation-sent.email.p", "abc@xyz.com"),
          htmlEscapedMessage("invitation-sent.l46")
        )
      )

      checkInviteSentPageContainsSurveyLink(result, true)

      journeyState.get should have[State](InvitationSentBusiness("invitation/link", None, "abc@xyz.com", Set(HMRCMTDVAT)))
    }
  }

  "GET /delete" should {
    val request = FakeRequest("GET", "/agents/delete")

    "show the delete page" in {
      journeyState.set(
        DeleteAuthorisationRequestPersonal(
          AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino)), itemId = "itemId"),
          Set.empty),
        List(
          ReviewAuthorisationsPersonal(availableServices, Set.empty),
          ConfirmClientItsa(
            AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino))),
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
        "You will not send an authorisation request to manage their Income Tax."
      )

      journeyState.get should have[State](
        DeleteAuthorisationRequestPersonal(
          AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino)), itemId = "itemId"),
          Set.empty))
    }
  }

  "POST /delete" should {
    val request = FakeRequest("POST", "/agents/invitation-sent")

    "redirect to review-authorisations when yes is selected and there is something left in the basket" in {
      journeyState.set(
        DeleteAuthorisationRequestPersonal(
          AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino))),
          Set(AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino)), itemId = "itemId"))
        ),
        List(
          ReviewAuthorisationsPersonal(
            availableServices,
            Set(
              AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino)), itemId = "itemId")
            )),
          ConfirmClientItsa(
            AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino))),
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

      journeyState.get should have[State](ReviewAuthorisationsPersonal(
        availableServices,
        Set(AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino)), itemId = "itemId"))))
    }
    "redirect to all-authorisations-removed when yes is selected and there is nothing left in the basket" in {
      journeyState.set(
        DeleteAuthorisationRequestPersonal(
          AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino)), itemId = "ABC123"),
          Set(AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino)), itemId = "ABC123"))
        ),
        List(
          ReviewAuthorisationsPersonal(availableServices, Set.empty),
          ConfirmClientItsa(
            AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino))),
            emptyBasket),
          IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), emptyBasket),
          SelectClientType(emptyBasket)
        )
      )

      val result = controller.submitDeleteAuthorisation(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showAllAuthorisationsRemoved().url)

      journeyState.get should have[State](AllAuthorisationsRemoved)
    }
    "redirect to review-authorisations when no is selected and keep basket the same" in {
      journeyState.set(
        DeleteAuthorisationRequestPersonal(
          AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino)), itemId = "ABC123"),
          Set(AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino)), itemId = "ABC123"))
        ),
        List(
          ReviewAuthorisationsPersonal(availableServices, Set.empty),
          ConfirmClientItsa(
            AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino))),
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

      journeyState.get should have[State](ReviewAuthorisationsPersonal(
        availableServices,
        Set(AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino)), itemId = "ABC123"))))
    }
  }
  "POST /some-create-authorisations-failed" should {
    val request = FakeRequest("GET", "/agents/some-create-authorisations-failed")

    "redirect to invitation sent" in {
      journeyState.set(
        SomeAuthorisationsFailed("invitation/link", None, "abc@xyz.com", Set.empty),
        List(
          SomeAuthorisationsFailed("invitation/link", None, "abc@xyz.com", Set.empty),
          ReviewAuthorisationsPersonal(availableServices, Set.empty),
          ConfirmClientItsa(
            AuthorisationRequest("Sylvia Plath", ItsaInvitation(Nino(nino))),
            emptyBasket),
          IdentifyPersonalClient(HMRCMTDIT, emptyBasket),
          SelectPersonalService(Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT), emptyBasket),
          SelectClientType(emptyBasket)
        )
      )

      val result = controller.submitSomeAuthorisationsFailed(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showInvitationSent().url)

      journeyState.get should have[State](InvitationSentPersonal("invitation/link", None, "abc@xyz.com", Set.empty))
    }
  }

  "GET /not-matched" should {
    val request = FakeRequest("GET", "/agents/not-matched")

    "display the known facts not matched page" in {
      journeyState.set(
        KnownFactNotMatched(emptyBasket),
        List()
      )
      val result = controller.showNotMatched(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyMsgs(
        result,
        "not-matched.header",
        "not-matched.description",
        "not-matched.advice",
        "not-matched.button"
      )
    }
  }

  "GET /cannot-create-request" should {
    val request = FakeRequest("GET", "/agents/cannot-create-request")

    "display the cannot create request page" in {
      journeyState.set(
        CannotCreateRequest(emptyBasket),
        List()
      )
      val result = controller.showCannotCreateRequest(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyMsgs(
        result,
        "cannot-create-request.header",
        "cannot-create-request.p1",
        "cannot-create-request.p2",
        "cannot-create-request.p3"
      )
    }
  }

  "GET /some-create-authorisation-failed" should {
    val request = FakeRequest("GET", "/agents/some-create-authorisation-failed")

    "display the some create authorisations failed" in {
      journeyState.set(
        SomeAuthorisationsFailed("/invitation/link", None, "abc@xyz.com", Set(AuthorisationRequest("CGT_NAME", CgtInvitation(cgtRef, Some(business)), AuthorisationRequest.FAILED))),
        List()
      )
      val result = controller.showSomeAuthorisationsFailed(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyMsgs(
        result,
        "create-auth-failed.header",
        "create-auth-failed.button.continue",
        "create-auth-failed.HMRC-CGT-PD"
      )
    }
  }

  "GET /all-create-authorisation-failed" should {
    val request = FakeRequest("GET", "/agents/all-create-authorisation-failed")

    "display the all create authorisations failed page" in {
      journeyState.set(
        AllAuthorisationsFailed(Set(AuthorisationRequest("CGT_NAME", CgtInvitation(cgtRef, Some(business)), AuthorisationRequest.FAILED))),
        List()
      )
      val result = controller.showAllAuthorisationsFailed(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyMsgs(
        result,
        "create-auth-failed.header",
        "create-auth-failed.button.try",
        "create-auth-failed.HMRC-CGT-PD"
      )
    }
  }

  "GET /already-authorisation-present" should {
    val request = FakeRequest("GET", "/agents/already-authorisation-present")

    "display the already authorisation present page" in {

      supportedServices.foreach { service =>
        journeyState.set(
          ActiveAuthorisationExists(personal, service, Set(AuthorisationRequest("CGT_NAME", CgtInvitation(cgtRef, Some(personal))))),
          List()
        )
        val result = controller.showActiveAuthorisationExists(authorisedAsValidAgent(request, arn.value))

        status(result) shouldBe 200

        val serviceKey = if(service == "HMRC-CGT-PD") "active-authorisation-exists.p1.HMRC-CGT-PD" else s"active-authorisation-exists.p1.$service"

        checkHtmlResultWithBodyMsgs(
          result,
          "active-authorisation-exists.header",
          serviceKey,
          "active-authorisation-exists.p2"
        )
      }
    }
  }

  "GET /already-authorisation-pending" should {
    val request = FakeRequest("GET", "/agents/already-authorisation-pending")

    "display the already authorisation pending page" in {
      journeyState.set(
        PendingInvitationExists(personal, emptyBasket),
        List()
      )
      val result = controller.showPendingAuthorisationExists(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyMsgs(
        result,
        "pending-authorisation-exists.header",
        "pending-authorisation-exists.p",
        "pending-authorisation-exists.new-request.button"
      )
    }
  }

  "GET /not-signed-up" should {
    val request = FakeRequest("GET", "/agents/not-signed-up")

    "display the not signed up page" in {
      journeyState.set(
        ClientNotSignedUp(HMRCMTDIT, emptyBasket),
        List()
      )
      val result = controller.showClientNotSignedUp(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-enrolled.title", "signed up to Making Tax Digital for Income Tax"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-enrolled.p", "signed up."))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-enrolled.existing.header", "Self Assessment"))
      checkResultContainsLink(result,"http://localhost:9438/agent-mapping/start","copy across an existing authorisation")
      checkResultContainsLink(result,"http://localhost:9438/agent-mapping/start","copy across an existing authorisation")

    }
  }

  "GET /all-authorisations-removed" should {
    val request = FakeRequest("GET", "/agents/all-authorisations-removed")

    "display the all authorisations removed page" in {
      journeyState.set(
        AllAuthorisationsRemoved,
        List()
      )
      val result = controller.showAllAuthorisationsRemoved(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyMsgs(
        result,
        "all-authorisations-removed.header",
        "all-authorisations-removed.p",
        "new-request.button"
      )
    }
  }

  "GET /access-removed" should {
    val request = FakeRequest("GET", "/agents/access-removed")

    "display the access removed page" in {
      journeyState.set(AgentSuspended(HMRCMTDIT, Set.empty), List())
      val result = controller.showAgentSuspended(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "agent-suspended.heading.single", "agent-suspended.p1.HMRC-MTD-IT", "agent-suspended.p2.single")
    }
  }
}
