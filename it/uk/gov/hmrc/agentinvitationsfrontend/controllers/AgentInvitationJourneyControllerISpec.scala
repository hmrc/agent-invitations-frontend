package uk.gov.hmrc.agentinvitationsfrontend.controllers

import org.joda.time.LocalDate
import org.jsoup.Jsoup
import org.scalatest.BeforeAndAfter
import play.api.Application
import play.api.libs.json.Json
import play.api.mvc.{AnyContentAsEmpty, Flash}
import play.api.test.{FakeRequest, Helpers}
import play.api.test.Helpers._
import uk.gov.hmrc.agentmtdidentifiers.model.{Service, SuspensionDetails, Vrn}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{Business, Personal, Trust}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class AgentInvitationJourneyControllerISpec extends BaseISpec with StateAndBreadcrumbsMatchers with BeforeAndAfter {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  override implicit lazy val app: Application = appBuilder
    .overrides(new TestAgentInvitationJourneyModule)
    .build()

  lazy private val journeyState = app.injector.instanceOf[TestAgentInvitationJourneyService]
  lazy private val controller: AgentInvitationJourneyController = app.injector.instanceOf[AgentInvitationJourneyController]
  lazy private val externalUrls = app.injector.instanceOf[ExternalUrls]

  import journeyState.model._

  private val availablePersonalServices: Set[Service] = Set(Service.PersonalIncomeRecord, Service.MtdIt, Service.Vat, Service.CapitalGains, Service.Ppt)
  private val availableBusinessServices: Set[Service] = Set(Service.Vat, Service.Ppt)
  private val availableTrustServices: Set[Service] = Set(Service.Trust, Service.CapitalGains, Service.Ppt)
  private val emptyBasket = Set.empty[AuthorisationRequest]

  implicit val timeoutDuration: Duration = Helpers.defaultAwaitTimeout.duration

  before {
    journeyState.clear
  }

  "GET /agents" should {
    val request = FakeRequest("GET", "/agents")

    "redirect to /agents/client-type if no current state" in {
      journeyState.clear
      val result = controller.agentsRoot()(request)
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientType().url)
      journeyState.get shouldBe None
    }

    "redirect to /agents/client-type" in {
      journeyState.set(SelectClientType(emptyBasket), Nil)
      val result = controller.agentsRoot()(request)
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientType().url)
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
          breadcrumbs = List(InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Set.empty, isAltItsa = Some(false)))
        )

        behave like itShowsClientTypePage(
          withBackLinkUrl = routes.AgentInvitationJourneyController.showInvitationSent().url)

        journeyState.get should have[State](
          state = SelectClientType(emptyBasket),
          breadcrumbs = List(InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Set.empty, isAltItsa = Some(false)))
        )
      }

      def itShowsClientTypePage(withBackLinkUrl: String): Unit = {
        val result = controller.showClientType()(authorisedAsValidAgent(request, arn.value))
        status(result) shouldBe 200

        checkHtmlResultWithBodyText(
          result.futureValue,
          htmlEscapedMessage(
            "generic.title",
            htmlEscapedMessage("client-type.header"),
            htmlEscapedMessage("service.name.agents.auth")),
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
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSelectService().url)
      journeyState.get shouldBe Some(
        (SelectService(Personal, availablePersonalServices, emptyBasket), List(SelectClientType(emptyBasket))))
    }

    "redirect to /agents/select-service after selecting business client type" in {
      journeyState.set(SelectClientType(emptyBasket), Nil)

      val result =
        controller.submitClientType(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> "business"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSelectService().url)
      journeyState.get should have [State](SelectService(Business, availableBusinessServices, emptyBasket), List(SelectClientType(emptyBasket)))
    }

    "redirect to /agents/select-service after selecting trust client type" in {
      journeyState.set(SelectClientType(emptyBasket), Nil)

      val result =
        controller.submitClientType(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> "trust"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSelectService().url)
      journeyState.get should have[State](SelectService(Trust, availableTrustServices, emptyBasket), List(SelectClientType(emptyBasket)))
    }

    "redisplay the page with errors if nothing is selected" in {
      journeyState.set(SelectClientType(emptyBasket), Nil)

      val result =
        controller.submitClientType()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientType().url)
      flash(result) shouldBe Flash(Map("dummy" -> "")) // "dummy" comes from a workaround introduced in https://github.com/hmrc/play-fsm/releases/tag/v0.20.0

      journeyState.get should have[State](SelectClientType(emptyBasket), Nil)
    }
  }

  "GET /agents/select-service" should {
    val request = FakeRequest("GET", "/agents/select-service")

    "show the select service page for a personal service" in {
      journeyState.set(SelectService(Personal, availablePersonalServices, emptyBasket), List(SelectClientType(emptyBasket)))
      val result = controller.showSelectService()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("select-service.header"),
          htmlEscapedMessage("service.name.agents.auth")),
        htmlEscapedMessage("select-service.HMRC-MTD-IT.personal"),
        htmlEscapedMessage("select-service.PERSONAL-INCOME-RECORD.personal"),
        htmlEscapedMessage("select-service.HMRC-MTD-VAT.personal")
      )
      journeyState.get shouldBe Some(
        (SelectService(Personal, availablePersonalServices, emptyBasket), List(SelectClientType(emptyBasket))))
    }

    "show the yes/no select service page for a business service when only one service is available" in {
      journeyState.set(SelectService(Business, Set(Service.Vat), emptyBasket), List(SelectClientType(emptyBasket)))
      val result = controller.showSelectService()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(

        result.futureValue,
        htmlEscapedMessage("global.no"),
        htmlEscapedMessage("global.yes"),
        htmlEscapedMessage("select-service.alternative")
      )
      journeyState.get shouldBe Some((SelectService(Business, Set(Service.Vat), emptyBasket), List(SelectClientType(emptyBasket))))
    }

    "show the correct service page content for trust clients" in {
      journeyState.set(SelectService(Trust, Set(Service.Trust), emptyBasket), List(SelectClientType(emptyBasket)))
      val result = controller.showSelectService()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        htmlEscapedMessage("select-single-service.HMRC-TERS-ORG.business.header"),
        htmlEscapedMessage("global.yes"),
        htmlEscapedMessage("global.no")
      )
      journeyState.get shouldBe Some((SelectService(Trust, Set(Service.Trust), emptyBasket), List(SelectClientType(emptyBasket))))
    }

    "go back to the select service page" in {
      journeyState.set(
        IdentifyClient(Personal, Service.MtdIt, emptyBasket),
        List(SelectService(Personal, availablePersonalServices, emptyBasket), SelectClientType(emptyBasket))
      )
      val result = controller.showSelectService()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("select-service.header"),
          htmlEscapedMessage("service.name.agents.auth")),
        htmlEscapedMessage("select-service.HMRC-MTD-IT.personal"),
        htmlEscapedMessage("select-service.PERSONAL-INCOME-RECORD.personal"),
        htmlEscapedMessage("select-service.HMRC-MTD-VAT.personal")
      )
      journeyState.get should have[State](
        SelectService(Personal, availablePersonalServices, emptyBasket),
        List(SelectClientType(emptyBasket)))
    }
  }

  def assertAgentSuspendedStateForService(request: FakeRequest[AnyContentAsEmpty.type], supportedServicesForClientType: Set[Service], mapServiceTypeToExpectedSuspendedService: Map[String, Service], regimes: Set[String]) =
    mapServiceTypeToExpectedSuspendedService.foreach { case (serviceType, expectedSuspendedService) =>
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = true, Some(regimes)))
      journeyState.set(SelectService(Personal, supportedServicesForClientType, emptyBasket), List(SelectClientType(emptyBasket)))

      val result =
        controller.submitSelectServiceMulti(authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> serviceType), arn.value))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showAgentSuspended().url)

      journeyState.get should have[State](
        AgentSuspended(expectedSuspendedService, emptyBasket),
        List(SelectService(Personal, supportedServicesForClientType, emptyBasket), SelectClientType(emptyBasket)))
    }

  "POST /agents/select-personal-service" should {
    val request = FakeRequest("POST", "/agents/select-personal-service")

    "accept valid service choice and redirect" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
      journeyState.set(SelectService(Personal, availablePersonalServices, emptyBasket), List(SelectClientType(emptyBasket)))

      val result = controller.submitSelectServiceMulti(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> "HMRC-MTD-IT"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)

      journeyState.get should have[State](
        IdentifyClient(Personal, Service.MtdIt, emptyBasket),
        List(SelectService(Personal, availablePersonalServices, emptyBasket), SelectClientType(emptyBasket)))
    }

    "redirect to agent suspended if the agent is suspended for the selected service" in {
      val regimes = Set("ITSA")

      assertAgentSuspendedStateForService(request, availablePersonalServices, Map("HMRC-MTD-IT" -> Service.MtdIt), regimes)
    }

    "redirect to agent suspended if the agent is suspended for all services with AGSV" in {
      val regimes = Set("AGSV")

      assertAgentSuspendedStateForService(request, availablePersonalServices,
        Map("HMRC-MTD-IT" -> Service.MtdIt, "HMRC-MTD-VAT" -> Service.Vat, "HMRC-CGT-PD" -> Service.CapitalGains,
          "HMRC-PPT-ORG" -> Service.Ppt, "PERSONAL-INCOME-RECORD" -> Service.PersonalIncomeRecord), regimes)
    }

    "redirect to agent suspended if the agent is suspended for all services with ALL" in {
      val regimes = Set("ALL")

      assertAgentSuspendedStateForService(request, availablePersonalServices,
        Map("HMRC-MTD-IT" -> Service.MtdIt, "HMRC-MTD-VAT" -> Service.Vat, "HMRC-CGT-PD" -> Service.CapitalGains,
          "HMRC-PPT-ORG" -> Service.Ppt, "PERSONAL-INCOME-RECORD" -> Service.PersonalIncomeRecord), regimes)
    }
  }

  "POST /agents/select-business-service" should {
    val request = FakeRequest("POST", "/agents/select-business-service")

    "redirect to identify-client when only one service is available and yes is selected" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
      journeyState.set(SelectService(Business, Set(Service.Vat), emptyBasket), List(SelectClientType(emptyBasket)))

      val result = controller.submitSelectServiceSingle(Service.Vat.id, ClientType.Business.toString)(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)

      journeyState.get should have[State](
        IdentifyClient(Business, Service.Vat, emptyBasket),
        List(SelectService(Business, Set(Service.Vat), emptyBasket), SelectClientType(emptyBasket)))
    }

    "redirect to select-client-type when only one service is available and no is selected" in {

      journeyState.set(SelectService(Business, Set(Service.Vat), Set.empty), List(SelectClientType(emptyBasket)))

      val result = controller.submitSelectServiceSingle(Service.Vat.id, ClientType.Business.toString)(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientType().url)

      journeyState.get should have[State](
        SelectClientType(emptyBasket),
        List())
    }

    "redirect to agent suspended if the agent is suspended for the selected service" in {
      val regimes = Set("VATC")

      assertAgentSuspendedStateForService(request, availableBusinessServices, Map("HMRC-MTD-VAT" -> Service.Vat), regimes)
    }

    "redirect to agent suspended if the agent is suspended for all services with AGSV" in {
      val regimes = Set("AGSV")

      assertAgentSuspendedStateForService(request, availableBusinessServices,
        Map("HMRC-MTD-VAT" -> Service.Vat, "HMRC-PPT-ORG" -> Service.Ppt), regimes)
    }

    "redirect to agent suspended if the agent is suspended for all services with ALL" in {
      val regimes = Set("ALL")

      assertAgentSuspendedStateForService(request, availableBusinessServices,
        Map("HMRC-MTD-VAT" -> Service.Vat, "HMRC-PPT-ORG" -> Service.Ppt), regimes)
    }
  }

  "POST /agents/select-trust-service" should {

    val request = FakeRequest("POST", "/agents/select-trust-service")

    "redirect to identify-client when yes is selected" in {
      givenGetSuspensionDetailsAgentStub(SuspensionDetails(suspensionStatus = false, None))
      journeyState.set(SelectService(Trust, availableTrustServices, emptyBasket), List(SelectClientType(emptyBasket)))

      val result = controller.submitSelectServiceSingle(Service.Trust.id, ClientType.Trust.toString)(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)

     journeyState.get should have[State](IdentifyClient(Trust, Service.Trust, emptyBasket),
        List(SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))
    }

    "redirect to select-client-type when no is selected" in {

      journeyState.set(SelectService(Trust, availableTrustServices, emptyBasket), List(SelectClientType(emptyBasket)))

      val result = controller.submitSelectServiceSingle(Service.Trust.id, ClientType.Trust.toString)(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientType().url)

      journeyState.get should have[State](
        SelectClientType(emptyBasket),
        List())
    }

    "do not blow up if user enters invalid value in the form for confirmation" in {

      journeyState.set(SelectService(Trust, availableTrustServices, emptyBasket), List(SelectClientType(emptyBasket)))

      val result = controller.submitSelectServiceSingle(Service.Trust.id, ClientType.Trust.toString)(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "foo"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSelectService().url)
    }

    "redirect to agent suspended if the agent is suspended for the selected service" in {
      val regimes = Set("PPT")

      assertAgentSuspendedStateForService(request, availableTrustServices, Map("HMRC-PPT-ORG" -> Service.Ppt), regimes)
    }

    "redirect to agent suspended if the agent is suspended for all services with AGSV" in {
      val regimes = Set("AGSV")

      assertAgentSuspendedStateForService(request, availableTrustServices,
        Map("HMRC-CGT-PD" -> Service.CapitalGains, "HMRC-PPT-ORG" -> Service.Ppt, "HMRC-TERS-ORG" -> Service.Trust), regimes)
    }

    "redirect to agent suspended if the agent is suspended for all services with ALL" in {
      val regimes = Set("ALL")

      assertAgentSuspendedStateForService(request, availableTrustServices,
        Map("HMRC-CGT-PD" -> Service.CapitalGains, "HMRC-PPT-ORG" -> Service.Ppt, "HMRC-TERS-ORG" -> Service.Trust), regimes)
    }
  }

  "GET /agents/identify-client" should {
    val request = FakeRequest("GET", "/agents/identify-client")

    "show identify client page" in {
      journeyState.set(
        IdentifyClient(Personal, Service.MtdIt, emptyBasket),
        List(SelectService(Personal, availablePersonalServices, emptyBasket), SelectClientType(emptyBasket)))

      controller.showIdentifyClient()(authorisedAsValidAgent(request, arn.value))
    }

    "show identify client page for IRV service" in {
      journeyState.set(
        IdentifyClient(Personal, Service.PersonalIncomeRecord, emptyBasket),
        List(SelectService(Personal, availablePersonalServices, emptyBasket), SelectClientType(emptyBasket)))

      val result = controller.showIdentifyClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "identify-client.header",
        "identify-client.itsa.p1",
        "identify-client.nino.label",
        "identify-client.nino.hint",
        "identify-client.irv-date-of-birth.label",
        "identify-client.irv-date-of-birth.hint"
      )

      journeyState.get should have[State](
        IdentifyClient(Personal, Service.PersonalIncomeRecord, emptyBasket),
        List(SelectService(Personal, availablePersonalServices, emptyBasket), SelectClientType(emptyBasket)))
    }

    "show identify client page for personal VAT service" in {
      journeyState.set(
        IdentifyClient(Personal, Service.Vat, emptyBasket),
        List(SelectService(Personal, availablePersonalServices, emptyBasket), SelectClientType(emptyBasket)))

      val result = controller.showIdentifyClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "identify-client.header",
        "identify-client.vat.p1",
        "identify-client.vrn.label",
        "identify-client.vrn.hint",
        "identify-client.vat-registration-date.label",
        "identify-client.vat-registration-date.hint"
      )

      journeyState.get should have[State](
        IdentifyClient(Personal, Service.Vat, emptyBasket),
        List(SelectService(Personal, availablePersonalServices, emptyBasket), SelectClientType(emptyBasket)))
    }

    "show the identify client page for business VAT service" in {
      journeyState.set(IdentifyClient(Business, Service.Vat, emptyBasket), List(SelectService(Business, availablePersonalServices, emptyBasket), SelectClientType(emptyBasket)))

      val result = controller.showIdentifyClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "identify-client.header",
        "identify-client.vat.p1",
        "identify-client.vrn.label",
        "identify-client.vrn.hint",
        "identify-client.vat-registration-date.label",
        "identify-client.vat-registration-date.hint"
      )

      journeyState.get should have[State](
        IdentifyClient(Business, Service.Vat, emptyBasket),
        List(SelectService(Business, availablePersonalServices, emptyBasket), SelectClientType(emptyBasket)))
    }

    "show the identify client page for TRUST service" in {

      journeyState.set(
        IdentifyClient(Trust, Service.Trust, emptyBasket),
        List(
          SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.showIdentifyClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "identify-trust-client.header.true",
        "identify-trust-client.p1",
        "identify-trust-client.p2.true",
        "identify-trust-alternative",
        "identify-trust.suggestion",
        "continue.button"
      )
      checkHtmlResultWithBodyText(result.futureValue, "A Unique Taxpayer Reference is 10 numbers, for example 1234567890. It will be on tax returns and other letters about Self Assessment. It may be called ‘reference’, ‘UTR’ or ‘official use’")

      journeyState.get should have[State](
        IdentifyClient(Trust, Service.Trust, emptyBasket),
        List(
          SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))
    }

    "show the identify client page for Personal CGT service" in {

      journeyState.set(
        IdentifyClient(Personal, Service.CapitalGains, emptyBasket),
        List(
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.showIdentifyClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "identify-cgt-client.header",
        "identify-cgt-client.p1",
        "identify-cgt-client.hint",
        "continue.button"
      )

      journeyState.get should have[State](
        IdentifyClient(Personal, Service.CapitalGains, emptyBasket),
        List(
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket)))
    }

    "show the identify client page for Trust CGT service" in {

      journeyState.set(
        IdentifyClient(Trust, Service.CapitalGains, emptyBasket),
        List(
          SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.showIdentifyClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "identify-cgt-client.header",
        "identify-cgt-client.p1",
        "identify-cgt-client.hint",
        "continue.button"
      )

      journeyState.get should have[State](
        IdentifyClient(Trust, Service.CapitalGains, emptyBasket),
        List(
          SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))
    }

    "show the identify client page for Personal PPT service" in {

      journeyState.set(
        IdentifyClient(Personal, Service.Ppt, emptyBasket),
        List(
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.showIdentifyClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "identify-ppt-client.header",
        "identify-ppt-client.p1",
        "identify-ppt-client.hint",
        "continue.button"
      )

      journeyState.get should have[State](
        IdentifyClient(Personal, Service.Ppt, emptyBasket),
        List(
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket)))
    }

    "show the identify client page for Trust PPT service" in {

      journeyState.set(
        IdentifyClient(Trust, Service.Ppt, emptyBasket),
        List(
          SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.showIdentifyClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "identify-ppt-client.header",
        "identify-ppt-client.p1",
        "identify-ppt-client.hint",
        "continue.button"
      )

      journeyState.get should have[State](
        IdentifyClient(Trust, Service.Ppt, emptyBasket),
        List(
          SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))
    }
  }

  "POST /agents/identify-itsa-client" should {
    val request = FakeRequest("POST", "/agents/identify-itsa-client")

    "redirect to /agents/confirm-client" when {
      "nino is uppercase" in {
        checkSubmitIdentifyItsaClient(submittedNinoStr = nino.value.toUpperCase)
      }

      "nino is lowercase (APB-3634 bug fix)" in {
        checkSubmitIdentifyItsaClient(submittedNinoStr = nino.value.toLowerCase)
      }

      def checkSubmitIdentifyItsaClient(submittedNinoStr: String) = {
        val nino = Nino(submittedNinoStr.toUpperCase)
        givenMatchingClientIdAndPostcode(nino, "BN114AW")
        givenTradingName(nino, "Sylvia Plath")

        journeyState.set(
          IdentifyClient(Personal, Service.MtdIt, emptyBasket),
          List(SelectService(Personal, availablePersonalServices, emptyBasket), SelectClientType(emptyBasket)))

        val result = controller.submitIdentifyItsaClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "clientIdentifier" -> submittedNinoStr,
              "postcode"         -> "BN114AW"
            ),
            arn.value))

        status(result) shouldBe 303
        Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showConfirmClient().url)

        journeyState.get should havePattern[State](
          {
            case ConfirmClient(
                AuthorisationRequest(
                  "Sylvia Plath",
                  Invitation(_, Service.MtdIt, _),
                  _,
                  _
                ),
                `emptyBasket`, _
                ) =>
          },
          List(
            IdentifyClient(Personal, Service.MtdIt, emptyBasket),
            SelectService(Personal, availablePersonalServices, emptyBasket),
            SelectClientType(emptyBasket)
          )
        )
      }
    }

    "redirect to not matched when clientId and known fact don't match" in {
      givenNonMatchingClientIdAndPostcode(nino, "BN114AW")

      journeyState.set(
        IdentifyClient(Personal, Service.MtdIt, emptyBasket),
        List(SelectService(Personal, availablePersonalServices, emptyBasket), SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyItsaClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("clientIdentifier" -> nino.value, "postcode" -> "BN114AW"),
          arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showNotMatched().url)

      journeyState.get should have[State](
        KnownFactNotMatched(emptyBasket),
        List(
          IdentifyClient(Personal, Service.MtdIt, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }

    "redirect to client not registered when the client is not signed up for the service and no SAUTR was found on CiD record" in {
      givenNotEnrolledClientITSA(nino, "BN114AW")

      journeyState.set(IdentifyClient(Personal, Service.MtdIt, emptyBasket), List())

      val result = controller.submitIdentifyItsaClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("clientIdentifier" -> nino.value, "postcode" -> "BN114AW"),
          arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientNotRegistered().url)
    }
  }

  "POST /agents/identify-vat-client" should {
    val request = FakeRequest("POST", "/agents/identify-vat-client")

    "redirect to /agents/confirm-client for personal vat" in {
      givenVatRegisteredClientReturns(Vrn("202949960"), LocalDate.parse("2010-10-10"), 204)
      givenClientDetails(Vrn("202949960"))

      journeyState.set(
        IdentifyClient(Personal, Service.Vat, emptyBasket),
        List(SelectService(Personal, availablePersonalServices, emptyBasket), SelectClientType(emptyBasket)))

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
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showConfirmClient().url)

      journeyState.get should havePattern[State](
        {
          case ConfirmClient(
              AuthorisationRequest(
                "GDT",
                Invitation(Some(Personal), Service.Vat, Vrn("202949960")),
                _,
                _),
              `emptyBasket`,
          Some(false)) =>
        },
        List(
          IdentifyClient(Personal, Service.Vat, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }

    "redirect to /agents/confirm-client for business vat" in {
      givenVatRegisteredClientReturns(Vrn("202949960"), LocalDate.parse("2010-10-10"), 204)
      givenClientDetails(Vrn("202949960"))

      journeyState.set(IdentifyClient(Business, Service.Vat, emptyBasket), List())

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
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showConfirmClient().url)

      journeyState.get should havePattern[State](
        {
          case ConfirmClient(
              AuthorisationRequest(
                "GDT",
                Invitation(Some(Business), Service.Vat, Vrn("202949960")),
                _,
                _), _, Some(false)) =>
        },
        List(IdentifyClient(Business, Service.Vat, emptyBasket))
      )
    }

    "redirect to /agents/not-matched when clientId and known fact don't match" in {
      givenVatRegisteredClientReturns(Vrn("202949960"), LocalDate.parse("2010-10-10"), 403)

      journeyState.set(
        IdentifyClient(Personal, Service.Vat, emptyBasket),
        List(SelectService(Personal, availablePersonalServices, emptyBasket), SelectClientType(emptyBasket)))

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
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showNotMatched().url)

      journeyState.get should have[State](
        KnownFactNotMatched(emptyBasket),
        List(
          IdentifyClient(Personal, Service.Vat, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }
    "redirect to /agents/cannot-create-request when  there is a migration in progress" in {
      givenVatRegisteredClientReturns(Vrn("202949960"), LocalDate.parse("2010-10-10"), 423)

      journeyState.set(
        IdentifyClient(Personal, Service.Vat, emptyBasket),
        List(SelectService(Personal, availablePersonalServices, emptyBasket), SelectClientType(emptyBasket)))

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
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showCannotCreateRequest().url)

    }
  }

  "POST /agents/identify-trust-client" should {

    val request = FakeRequest("POST", "/agents/identify-trust-client")

    "redirect to /agents/confirm-client - UTR" in {
      givenTrustClientReturns(validUtr, 200, Json.toJson(trustResponse).toString())

      journeyState.set(
        IdentifyClient(Trust, Service.Trust, emptyBasket),
        List(SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("taxId" -> validUtr.value),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showConfirmClient().url)

      journeyState.get should havePattern[State](
        {
          case ConfirmClient(AuthorisationRequest("some-trust", Invitation(_, Service.Trust, _), _, _), _, _) =>
        },
        List(
          IdentifyClient(Trust, Service.Trust, emptyBasket),
          SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }

    "redirect to /agents/confirm-client - URN" in {
      givenTrustClientReturns(validUrn, 200, Json.toJson(trustResponse).toString())

      journeyState.set(
        IdentifyClient(Trust, Service.TrustNT, emptyBasket),
        List(SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("taxId" -> validUrn.value),
          arn.value
        ))
      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showConfirmClient().url)

      journeyState.get should havePattern[State](
        {
          case ConfirmClient(AuthorisationRequest("some-trust", Invitation(_, Service.TrustNT, _), _, _), _, _) =>
        },
        List(
          IdentifyClient(Trust, Service.TrustNT, emptyBasket),
          SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }

    "handle invalid Utr passed in by the user and redirect back to previous /identify-client page " in {

      journeyState.set(IdentifyClient(Trust, Service.Trust, emptyBasket), List(SelectService(Trust, availableTrustServices, emptyBasket), SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("utr" -> "493745"),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)
    }

    "handle invalid Urn passed in by the user and redirect back to previous /identify-client page " in {

      journeyState.set(IdentifyClient(Trust, Service.TrustNT, emptyBasket), List(SelectService(Trust, availableTrustServices, emptyBasket), SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("urn" -> "493745"),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)
    }

    "redirect to /agents/not-found when utr passed in does not match to any trust" in {
      givenTrustClientReturns(validUtr, 200, trustNotFoundJson)

      journeyState.set(
        IdentifyClient(Trust, Service.Trust, emptyBasket),
        List(SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("taxId" -> validUtr.value),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showNotMatched().url)

      journeyState.get should have[State](
        TrustNotFound(emptyBasket),
        List(IdentifyClient(Trust, Service.Trust, emptyBasket),
          SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }

    "redirect to /agents/not-found when urn passed in does not match to any trust" in {
      givenTrustClientReturns(validUrn, 200, trustNotFoundJson)

      journeyState.set(
        IdentifyClient(Trust, Service.Trust, emptyBasket),
        List(SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("taxId" -> validUrn.value),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showNotMatched().url)

      journeyState.get should have[State](
        TrustNotFound(emptyBasket),
        List(IdentifyClient(Trust, Service.Trust, emptyBasket),
          SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }

    "redirect to /agents/not-found when trust is in invalid state - UTR" in {

      givenTrustClientReturns(validUtr, 200, invalidTrustJson)

      journeyState.set(
        IdentifyClient(Trust, Service.Trust, emptyBasket),
        List(SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("taxId" -> validUtr.value),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showNotMatched().url)

      journeyState.get should have[State](
        TrustNotFound(emptyBasket),
        List(
          IdentifyClient(Trust, Service.Trust, emptyBasket),
          SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }

    "redirect to /agents/not-found when trust is in invalid state - URN" in {

      givenTrustClientReturns(validUrn, 200, invalidTrustJson)

      journeyState.set(
        IdentifyClient(Trust, Service.Trust, emptyBasket),
        List(SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("taxId" -> validUrn.value),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showNotMatched().url)

      journeyState.get should have[State](
        TrustNotFound(emptyBasket),
        List(
          IdentifyClient(Trust, Service.Trust, emptyBasket),
          SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }
  }

  "POST /agents/identify-cgt-client" should {

    val request = FakeRequest("POST", "/agents/identify-cgt-client")

    "redirect to /agents/client-postcode" in {
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription("GB")).toString())

      journeyState.set(
        IdentifyClient(Trust, Service.CapitalGains, emptyBasket),
        List(SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyCgtClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("cgtRef" -> cgtRef.value),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showConfirmCgtPostcode().url)

      journeyState.get should havePattern[State](
        {
          case ConfirmPostcodeCgt(cgtRef, personal, emptyBasket, _, _) =>
        },
        List(
          IdentifyClient(Trust, Service.CapitalGains, emptyBasket),
          SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }

    "handle invalid CgtRef passed in by the user and redirect back to previous /identify-client page " in {

      journeyState.set(IdentifyClient(Trust, Service.CapitalGains, emptyBasket), List(SelectService(Trust, availableTrustServices, emptyBasket), SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("cgtRef" -> "12345"),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)
    }

    "redirect to /agents/not-matched when cgtRef passed in does not match to any cgt client" in {
      givenGetCgtSubscriptionReturns(cgtRef, 404, cgtNotFoundJson)

      journeyState.set(
        IdentifyClient(Trust, Service.CapitalGains, emptyBasket),
        List(SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyCgtClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("cgtRef" -> cgtRef.value),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showNotMatched().url)

      journeyState.get should have[State](
        CgtRefNotFound(cgtRef, emptyBasket),
        List(IdentifyClient(Trust, Service.CapitalGains, emptyBasket),
          SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }
  }

  "POST /agents/identify-ppt-client" should {

    val request = FakeRequest("POST", "/agents/identify-ppt-client")

    "handle invalid PptRef passed in by the user and redirect back to previous /identify-client page " in {

      journeyState.set(IdentifyClient(Trust, Service.Ppt, emptyBasket), List(SelectService(Trust, availableTrustServices, emptyBasket), SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyTrustClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("pptRef" -> "12345", "pptRef" -> pptRef.value, "registrationDate.year" -> "2021", "registrationDate.month" -> "1", "registrationDate.day" -> "1"),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)
    }

    "redirect to /agents/not-matched when pptRef passed in does not match to any ppt client" in {
      givenGetPptSubscriptionReturns(pptRef, 404, pptNotFoundJson)

      journeyState.set(
        IdentifyClient(Trust, Service.Ppt, emptyBasket),
        List(SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyPptClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("pptRef" -> pptRef.value, "registrationDate.year" -> "2021", "registrationDate.month" -> "1", "registrationDate.day" -> "1"),
          arn.value
        ))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showNotMatched().url)

      journeyState.get should have[State](
        PptRefNotFound(pptRef, emptyBasket),
        List(IdentifyClient(Trust, Service.Ppt, emptyBasket),
          SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }
  }


  "GET /agents/client-postcode" should {

    val request = FakeRequest("GET", "/agents/client-postcode")

    "display the page as expected" in {

      journeyState.set(
        ConfirmPostcodeCgt(cgtRef, Personal, emptyBasket, Some("BN13 1FN"), "firstName lastName"),
        List(
          IdentifyClient(Personal, Service.CapitalGains, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.showConfirmCgtPostcode()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "confirm-postcode-cgt.header",
        "confirm-postcode-cgt.p1",
        "confirm-postcode-cgt.hint",
        "continue.button"
      )

      journeyState.get should have[State](
        ConfirmPostcodeCgt(cgtRef, Personal, emptyBasket, Some("BN13 1FN"), "firstName lastName"),
        List(
          IdentifyClient(Personal, Service.CapitalGains, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket)))
    }
  }

  "POST /agents/client-postcode" should {

    val request = FakeRequest("POST", "/agents/client-postcode")

    "redirect to /confirm-client if postcode matches for a UK client" in {

      journeyState.set(
        ConfirmPostcodeCgt(cgtRef, Personal, emptyBasket, Some("BN13 1FN"), "firstName lastName"),
        List(
          IdentifyClient(Personal, Service.CapitalGains, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitConfirmCgtPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody("postcode" -> "BN13 1FN"), arn.value))

      status(result) shouldBe 303

      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showConfirmClient().url)

      journeyState.get should havePattern[State](
        {case ConfirmClient(AuthorisationRequest("firstName lastName", Invitation(Some(Personal), Service.CapitalGains, cgtRef), _, _), emptyBasket, _) => },
        List(
          ConfirmPostcodeCgt(cgtRef, Personal, emptyBasket, Some("BN13 1FN"), "firstName lastName"),
          IdentifyClient(Personal, Service.CapitalGains, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket)))
    }

    "redirect to /not-matched if postcode does not matche for a UK client" in {

      journeyState.set(
        ConfirmPostcodeCgt(cgtRef, Personal, emptyBasket, Some("BN13 1FN"), "firstName lastName"),
        List(
          IdentifyClient(Personal, Service.CapitalGains, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitConfirmCgtPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody("postcode" -> "BN13 1XX"), arn.value))

      status(result) shouldBe 303

      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showNotMatched().url)
    }
  }

  "GET /agents/client-country" should {

    val request = FakeRequest("GET", "/agents/client-country")

    "display the page as expected" in {

      journeyState.set(
        ConfirmCountryCodeCgt(cgtRef, Personal, emptyBasket, "FR", "firstName lastName"),
        List(
          IdentifyClient(Personal, Service.CapitalGains, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.showConfirmCgtCountryCode()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "confirm-countryCode-cgt.header",
        "confirm-countryCode-cgt.p1",
        "confirm-countryCode-cgt.hint",
        "continue.button"
      )

      journeyState.get should have[State](
        ConfirmCountryCodeCgt(cgtRef, Personal, emptyBasket, "FR", "firstName lastName"),
        List(
          IdentifyClient(Personal, Service.CapitalGains, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket)))
    }
  }

  "POST /agents/client-country" should {

    val request = FakeRequest("POST", "/agents/client-country")

    "redirect to /confirm-client if country code matches for a non UK client" in {

      journeyState.set(
        ConfirmCountryCodeCgt(cgtRef, Personal, emptyBasket, "FR", "firstName lastName"),
        List(
          IdentifyClient(Personal, Service.CapitalGains, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitConfirmCgtCountryCode(authorisedAsValidAgent(request.withFormUrlEncodedBody("countryCode" -> "FR"), arn.value))

      status(result) shouldBe 303

      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showConfirmClient().url)

      journeyState.get should havePattern[State](
        {case ConfirmClient(AuthorisationRequest("firstName lastName", Invitation(Some(Personal), Service.CapitalGains, cgtRef), _, _), emptyBasket, _) => },
        List(
          ConfirmCountryCodeCgt(cgtRef, Personal, emptyBasket, "FR", "firstName lastName"),
          IdentifyClient(Personal, Service.CapitalGains, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket)))
    }

    "redirect to /not-matched if country code does not match for a non UK client" in {

      journeyState.set(
        ConfirmCountryCodeCgt(cgtRef, Personal, emptyBasket, "FR", "firstName lastName"),
        List(
          IdentifyClient(Personal, Service.CapitalGains, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket)))

      val result = controller.submitConfirmCgtCountryCode(authorisedAsValidAgent(request.withFormUrlEncodedBody("countryCode" -> "IN"), arn.value))

      status(result) shouldBe 303

      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showNotMatched().url)
    }
  }

  "POST /agents/identify-irv-client" when {
    val request = FakeRequest("POST", "/agents/identify-irv-client")

    "redirect to /agents/review-authorisations because flag is off" when {
      "submitted NINO is uppercase" in {
        checkSubmitIdentifyIrvClient(submittedNinoStr = nino.value.toUpperCase)
      }

      "submitted NINO is lowercase (APB-3525)" in {
        checkSubmitIdentifyIrvClient(submittedNinoStr = nino.value.toLowerCase)
      }

      def checkSubmitIdentifyIrvClient(submittedNinoStr: String) = {
        givenMatchingCitizenRecord(nino, LocalDate.parse("1990-10-10"))
        givenGetAllPendingInvitationsReturnsEmpty(arn, nino.value, Service.PersonalIncomeRecord)
        givenAfiRelationshipNotFoundForAgent(arn, nino)
        givenCitizenDetailsAreKnownFor(nino, "Virginia", "Woolf")

        journeyState.set(
          IdentifyClient(Personal, Service.PersonalIncomeRecord, emptyBasket),
          List(SelectService(Personal, availablePersonalServices, emptyBasket), SelectClientType(emptyBasket)))

        val result = controller.submitIdentifyIrvClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "clientIdentifier" -> submittedNinoStr,
              "dob.year"         -> "1990",
              "dob.month"        -> "10",
              "dob.day"          -> "10"),
            arn.value))

        status(result) shouldBe 303
        Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showReviewAuthorisations().url)

        journeyState.get should havePattern[State](
          { case ReviewAuthorisations(Personal, _, basket) if basket.nonEmpty => },
          List(
            IdentifyClient(Personal, Service.PersonalIncomeRecord, emptyBasket),
            SelectService(Personal, availablePersonalServices, emptyBasket),
            SelectClientType(emptyBasket))
        )
      }
    }

    "redirect to /agents/not-matched when clientId and known fact don't match" in {
      givenNonMatchingCitizenRecord(nino, LocalDate.parse("1990-10-10"))

      journeyState.set(
        IdentifyClient(Personal, Service.PersonalIncomeRecord, emptyBasket),
        List(SelectService(Personal, availablePersonalServices, emptyBasket), SelectClientType(emptyBasket)))

      val result = controller.submitIdentifyIrvClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientIdentifier" -> nino.value,
            "dob.year"         -> "1990",
            "dob.month"        -> "10",
            "dob.day"          -> "10"),
          arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showNotMatched().url)

      journeyState.get should have[State](
        KnownFactNotMatched(emptyBasket),
        List(
          IdentifyClient(Personal, Service.PersonalIncomeRecord, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }

  }

  "GET /agents/confirm-client" should {
    val request = FakeRequest("GET", "/agents/confirm-client")

    "show the confirm client page for ITSA service" in {
      givenTradingName(nino, "Sylvia Plath")
      journeyState.set(
        ConfirmClient(
          AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)),
          emptyBasket),
        List(
          IdentifyClient(Personal, Service.MtdIt, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket))
      )

      val result = controller.showConfirmClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result.futureValue, "Is this the client you want authorisation from?","Is Sylvia Plath the client you want authorisation from?")

      journeyState.get should havePattern[State](
        {
          case ConfirmClient(
              AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, `nino`), _, _),
              `emptyBasket`, _) =>
        },
        List(
          IdentifyClient(Personal, Service.MtdIt, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }

    "show the confirm client page for personal VAT service" in {
      givenClientDetails(Vrn("202949960"))
      journeyState.set(
        ConfirmClient(
          AuthorisationRequest("GDT", Invitation(Some(Personal), Service.Vat, vrn)),
          emptyBasket, clientInsolvent = Some(false)),
        List(
          IdentifyClient(Personal, Service.Vat, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket))
      )

      val result = controller.showConfirmClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result.futureValue, "Is this the client you want authorisation from?","Is GDT the client you want authorisation from?")

      journeyState.get should havePattern[State](
        {
          case ConfirmClient(
              AuthorisationRequest("GDT", Invitation(_, Service.Vat, _), _, _),
              _, Some(false)) =>
        },
        List(
          IdentifyClient(Personal, Service.Vat, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }

    "show the confirm client page for Trust service - UTR" in {

      givenTrustClientReturns(validUtr, 200, Json.toJson(trustResponse).toString())

      journeyState.set(
        ConfirmClient(AuthorisationRequest("Nelson James Trust", Invitation(Some(ClientType.Trust), Service.Trust, validUtr)), emptyBasket),
        List(
          IdentifyClient(Trust, Service.Trust, emptyBasket),
          SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )

      val result = controller.showConfirmClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result.futureValue, "Is this the client you want authorisation from?", "Is Nelson James Trust the client you want authorisation from?")

      journeyState.get should havePattern[State](
        {
          case ConfirmClient(AuthorisationRequest("Nelson James Trust", Invitation(_, Service.Trust, _), _, _), _, _) =>
        },
        List(
          IdentifyClient(Trust, Service.Trust, emptyBasket),
          SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }

    "show the confirm client page for Trust service - URN" in {

      givenTrustClientReturns(validUrn, 200, Json.toJson(trustResponse).toString())

      journeyState.set(
        ConfirmClient(AuthorisationRequest("Nelson James Trust", Invitation(Some(ClientType.Trust), Service.TrustNT, validUrn)), emptyBasket),
        List(
          IdentifyClient(Trust, Service.TrustNT, emptyBasket),
          SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )

      val result = controller.showConfirmClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result.futureValue, "Is this the client you want authorisation from?", "Is Nelson James Trust the client you want authorisation from?")

      journeyState.get should havePattern[State](
        {
          case ConfirmClient(AuthorisationRequest("Nelson James Trust", Invitation(_, Service.TrustNT, _), _, _), _, _) =>
        },
        List(
          IdentifyClient(Trust, Service.TrustNT, emptyBasket),
          SelectService(Trust, availableTrustServices, emptyBasket),
          SelectClientType(emptyBasket))
      )
    }

    "show the confirm client page for business VAT service" in {
      givenClientDetails(Vrn("202949960"))
      journeyState.set(
        ConfirmClient(
          AuthorisationRequest("GDT", Invitation(Some(Business), Service.Vat, vrn)), emptyBasket, clientInsolvent = Some(false)),
        List(IdentifyClient(Business, Service.Vat, emptyBasket), SelectService(Business, availablePersonalServices, emptyBasket), SelectClientType(emptyBasket))
      )

      val result = controller.showConfirmClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result.futureValue, "Is this the client you want authorisation from?", "Is GDT the client you want authorisation from?")

      journeyState.get should havePattern[State](
        {
          case ConfirmClient(
              AuthorisationRequest(
                "GDT",
                Invitation(Some(ClientType.Business), Service.Vat, Vrn(_)),
                _,
                _), _, Some(false)) =>
        },
        List(IdentifyClient(Business, Service.Vat, emptyBasket), SelectService(Business, availablePersonalServices, emptyBasket), SelectClientType(emptyBasket))
      )
    }

    "show the confirm client page for CGT clients" in {
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription()).toString())
      journeyState.set(
        ConfirmClient(AuthorisationRequest("CGT_NAME", Invitation(Some(ClientType.Business), Service.CapitalGains, cgtRef)), emptyBasket),
        List(ConfirmCountryCodeCgt(cgtRef, Business, emptyBasket, "FR", "firstName lastName"), SelectService(Business, availablePersonalServices, emptyBasket), SelectClientType(emptyBasket))
      )

      val result = controller.showConfirmClient()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result.futureValue, "Is this the client you want authorisation from?", "Is CGT_NAME the client you want authorisation from?")
    }
  }

  "POST /agents/confirm-client" when {

    "yes is selected and it is an ITSA request" should {
      val request = FakeRequest("POST", "/agents/confirm-client")

      "redirect to the authorisation detected page if legacy relationship exists" in {
        givenGetAllPendingInvitationsReturnsEmpty(arn, nino.value, Service.MtdIt)
        givenCheckRelationshipItsaWithStatus(arn, nino,404)
        givenPartialAuthNotExists(arn, nino.value)
        givenLegacySaRelationshipReturnsStatus(arn, nino, 200)

        journeyState.set(
          ConfirmClient(
            AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)),
            emptyBasket),
          List(
            IdentifyClient(Personal, Service.MtdIt, emptyBasket),
            SelectService(Personal, availablePersonalServices, emptyBasket),
            SelectClientType(emptyBasket))
        )

        val result = controller.submitConfirmClient(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

        status(result.futureValue) shouldBe 303
        redirectLocation(result) shouldBe Some("/invitations/agents/authorisation-detected")
      }

      "redirect to already mapped page if legacy relationship exists and is copied across" in {
        givenGetAllPendingInvitationsReturnsEmpty(arn, nino.value, Service.MtdIt)
        givenCheckRelationshipItsaWithStatus(arn, nino,404)
        givenPartialAuthNotExists(arn, nino.value)
        givenLegacySaRelationshipReturnsStatus(arn, nino, 204)

        journeyState.set(
          ConfirmClient(
            AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)),
            emptyBasket),
          List(
            IdentifyClient(Personal, Service.MtdIt, emptyBasket),
            SelectService(Personal, availablePersonalServices, emptyBasket),
            SelectClientType(emptyBasket))
        )

        val result = controller.submitConfirmClient(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

        status(result.futureValue) shouldBe 303
        redirectLocation(result) shouldBe Some("/invitations/agents/already-copied-across-itsa")
      }

      "redirect to the review authorisations page if legacy relationship does not exist" in {
        givenGetAllPendingInvitationsReturnsEmpty(arn, nino.value, Service.MtdIt)
        givenCheckRelationshipItsaWithStatus(arn, nino,404)
        givenPartialAuthNotExists(arn, nino.value)
        givenLegacySaRelationshipReturnsStatus(arn, nino, 404)

        journeyState.set(
          ConfirmClient(
            AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)),
            emptyBasket),
          List(
            IdentifyClient(Personal, Service.MtdIt, emptyBasket),
            SelectService(Personal, availablePersonalServices, emptyBasket),
            SelectClientType(emptyBasket))
        )

        val result = controller.submitConfirmClient(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

        status(result.futureValue) shouldBe 303
        redirectLocation(result) shouldBe Some("/invitations/agents/review-authorisations")
      }
    }
  }

  "POST /agents/confirm-client" should {
    val request = FakeRequest("POST", "/agents/confirm-client")

    "redirect to the review authorisations page when yes is selected" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, vrn.value, Service.Vat)
      givenCheckRelationshipVatWithStatus(arn, vrn, 404)
      journeyState.set(
        ConfirmClient(
          AuthorisationRequest("GDT", Invitation(Some(Personal), Service.Vat, vrn)),
          `emptyBasket`),
        List(
          IdentifyClient(Personal, Service.Vat, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket)
        )
      )

      val result = controller.submitConfirmClient(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showReviewAuthorisations().url)
    }

    "redirect to client insolvent page when yes is selected and client is insolvent" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, vrn.value, Service.Vat)
      givenCheckRelationshipVatWithStatus(arn, vrn, 404)
      journeyState.set(
        ConfirmClient(
          AuthorisationRequest("GDT", Invitation(Some(Personal), Service.Vat, vrn)),
          `emptyBasket`, clientInsolvent = Some(true)),
        List(
          IdentifyClient(Personal, Service.Vat, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket)
        )
      )

      val result = controller.submitConfirmClient(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showClientInsolvent().url)
    }

    "redirect to review authorisations for business journeys also" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, vrn.value, Service.Vat)
      givenCheckRelationshipVatWithStatus(arn, vrn, 404)
      givenInvitationCreationSucceeds(arn, Some(Business), vrn.value, invitationIdVAT, vrn.value, "vrn", Service.Vat, "VRN")
      givenAgentReferenceRecordExistsForArn(arn, "FOO")
      givenAgentReference(arn, nino.value, Business)
      givenGetAgencyEmailAgentStub
      journeyState.set(
        ConfirmClient(
          AuthorisationRequest("GDT", Invitation(Some(Business), Service.Vat, vrn)), emptyBasket),
        List(
          IdentifyClient(Business, Service.Vat, emptyBasket),
          SelectService(Business, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket)
        )
      )

      val result = controller.submitConfirmClient(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showReviewAuthorisations().url)
    }

    "redirect to the identify-client page when no is selected" in {
      journeyState.set(
        ConfirmClient(
          AuthorisationRequest("GDT", Invitation(Some(Personal), Service.Vat, vrn)),
          `emptyBasket`),
        List(
          IdentifyClient(Personal, Service.Vat, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket))
      )

      val result = controller.submitConfirmClient(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showIdentifyClient().url)

      journeyState.get should have[State](IdentifyClient(Personal, Service.Vat, emptyBasket))
    }

    "redirect to pending invitation exists when a pending invitation already exists for this service" in {
      givenGetAllPendingInvitationsReturnsSome(arn, vrn.value, Service.Vat)
      givenCheckRelationshipVatWithStatus(arn, vrn, 404)
      givenAgentReference(arn, "uid", ClientType.Personal)
      journeyState.set(
        ConfirmClient(
          AuthorisationRequest("GDT", Invitation(Some(Personal), Service.Vat, vrn)),
          `emptyBasket`),
        List()
      )

      val result = controller.submitConfirmClient(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(
        routes.AgentInvitationJourneyController.showPendingAuthorisationExists().url)
    }

    "redirect to active authorisation exists when there is already an active authorisation for this service" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, vrn.value, Service.Vat)
      givenCheckRelationshipVatWithStatus(arn, vrn, 200)
      journeyState.set(
        ConfirmClient(
          AuthorisationRequest("GDT", Invitation(Some(Personal), Service.Vat, vrn)),
          `emptyBasket`),
        List())

      val result = controller.submitConfirmClient(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(
        routes.AgentInvitationJourneyController.showActiveAuthorisationExists().url)
    }

    "redirect to active authorisation exists when there is already an active authorisation for Trusts - UTR" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validUtr.value, Service.Trust)
      givenCheckRelationshipTrustWithStatus(arn, validUtr, 200)
      givenAgentReferenceRecordExistsForArn(arn, "FOO")
      givenAgentReference(arn, nino.value, Business)
      givenGetAgencyEmailAgentStub

      journeyState.set(
        ConfirmClient(AuthorisationRequest("GDT", Invitation(Some(ClientType.Trust), Service.Trust, validUtr)), emptyBasket),
        List())

      val result = controller.submitConfirmClient(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(
        routes.AgentInvitationJourneyController.showActiveAuthorisationExists().url)
    }

    "redirect to active authorisation exists when there is already an active authorisation for Trusts - URN" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validUrn.value, Service.TrustNT)
      givenCheckRelationshipTrustWithStatus(arn, validUrn, 200)
      givenAgentReferenceRecordExistsForArn(arn, "FOO")
      givenAgentReference(arn, nino.value, Business)
      givenGetAgencyEmailAgentStub

      journeyState.set(
        ConfirmClient(AuthorisationRequest("GDT", Invitation(Some(ClientType.Trust), Service.TrustNT, validUrn)), emptyBasket),
        List())

      val result = controller.submitConfirmClient(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(
        routes.AgentInvitationJourneyController.showActiveAuthorisationExists().url)
    }

    "redirect to active authorisation exists when there is already a partial authorisation for ITSA" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, nino.value, Service.MtdIt)
      givenCheckRelationshipItsaWithStatus(arn, nino, 404)
      givenPartialAuthorisationExists(arn, nino.value)

      givenAgentReferenceRecordExistsForArn(arn, "FOO")
      givenAgentReference(arn, nino.value, Business)
      givenGetAgencyEmailAgentStub

      journeyState.set(
        ConfirmClient(AuthorisationRequest("GDT", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)), emptyBasket),
        List())

      val result = controller.submitConfirmClient(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(
        routes.AgentInvitationJourneyController.showActiveAuthorisationExists().url)
    }
  }

  "GET /agents/review-authorisations (personal)" should {
    val fullBasket = Set(
      AuthorisationRequest(
        "James Client",
        Invitation(Some(ClientType.Personal), Service.MtdIt, nino),
        itemId = "ABC"))
    val request = FakeRequest("GET", "/agents/review-authorisations")

    "show the review authorisations page" in {
      journeyState.set(
        ReviewAuthorisations(Personal, availablePersonalServices, fullBasket),
        List(
          ConfirmClient(
            AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)),
            fullBasket),
          IdentifyClient(Personal, Service.MtdIt, fullBasket),
          SelectService(Personal, availablePersonalServices, fullBasket),
          SelectClientType(fullBasket)
        )
      )

      val result = controller.showReviewAuthorisations()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyText(
        result.futureValue,
        "Check your authorisation requests",
        "You have added 1 authorisation request.",
        "Manage their Making Tax Digital for Income Tax",
        "James Client",
        "Do you need to add another authorisation for this client?"
      )

      journeyState.get should have[State](ReviewAuthorisations(Personal, availablePersonalServices, fullBasket))
    }
  }

  "GET /agents/review-authorisations (business)" should {
    val fullBasket = Set(
      AuthorisationRequest(
        "Client Biz",
        Invitation(Some(ClientType.Business), Service.Vat, vrn),
        itemId = "ABC"))
    val request = FakeRequest("GET", "/agents/review-authorisations")

    "show the review authorisations page" in {
      journeyState.set(
        ReviewAuthorisations(Business, availableBusinessServices, fullBasket),
        List(
          ConfirmClient(
            AuthorisationRequest("Sylvia Plath", Invitation(Some(Business), Service.Vat, vrn)),
            fullBasket),
          IdentifyClient(Business, Service.Vat, fullBasket),
          SelectService(Personal, availableBusinessServices, fullBasket),
          SelectClientType(fullBasket)
        )
      )

      val result = controller.showReviewAuthorisations()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyText(
        result.futureValue,
        "Check your authorisation requests",
        "You have added 1 authorisation request.",
        "Manage their VAT",
        "Client Biz",
        "Do you need to add another authorisation for this client?"
      )

      journeyState.get should have[State](ReviewAuthorisations(Business, availableBusinessServices, fullBasket))
    }
  }

  "GET /agents/review-authorisations (trust)" should {
    val fullBasket = Set(
      AuthorisationRequest(
        "Client Trust",
        Invitation(Some(ClientType.Trust), Service.CapitalGains, cgtRef),
        itemId = "ABC"))
    val request = FakeRequest("GET", "/agents/review-authorisations")

    "show the review authorisations page" in {
      journeyState.set(
        ReviewAuthorisations(Trust, availableTrustServices, fullBasket),
        List(
          ConfirmClient(
            AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Trust), Service.CapitalGains, cgtRef)),
            fullBasket),
          IdentifyClient(Trust, Service.CapitalGains, fullBasket),
          SelectService(Personal, availableTrustServices, fullBasket),
          SelectClientType(fullBasket)
        )
      )

      val result = controller.showReviewAuthorisations()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyText(
        result.futureValue,
        "Check your authorisation requests",
        "You have added 1 authorisation request.",
        "Manage their Capital Gains Tax",
        "Client Trust",
        "Do you need to add another authorisation for this client?"
      )

      journeyState.get should have[State](ReviewAuthorisations(Trust, availableTrustServices, fullBasket))
    }
  }

  "POST /agents/review-authorisations" should {
    val request = FakeRequest("POST", "/agents/review-authorisations")

    "redirect to invitation-sent page for a personal service when no is selected" in {
      givenInvitationCreationSucceeds(arn, Some(Personal), nino.value, invitationIdITSA, nino.value, "ni", Service.MtdIt, "NI")
      givenAgentReferenceRecordExistsForArn(arn, "FOO")
      givenAgentReference(arn, nino.value, Personal)
      givenGetAgencyEmailAgentStub
      journeyState.set(
        ReviewAuthorisations(Personal, availablePersonalServices, emptyBasket),
        List(
          ConfirmClient(
            AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)),
            emptyBasket),
          IdentifyClient(Personal, Service.MtdIt, emptyBasket),
          SelectService(Personal, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket)
        )
      )

      val result = controller.submitReviewAuthorisations(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showInvitationSent().url)

      journeyState.get should have[State](
        InvitationSent(ClientType.Personal, "/invitations/personal/AB123456A/99-with-flake", None, "abc@xyz.com", Set.empty, isAltItsa = Some(false)))
    }

    "redirect to select-service when yes is selected" in {
      val testAvailableServices: Set[Service] = Set(Service.PersonalIncomeRecord, Service.MtdIt, Service.Vat, Service.Ppt)
      journeyState.set(
        ReviewAuthorisations(Personal, testAvailableServices, emptyBasket),
        List(
          ConfirmClient(
            AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)),
            emptyBasket),
          IdentifyClient(Personal, Service.MtdIt, emptyBasket),
          SelectService(Personal, testAvailableServices, emptyBasket),
          SelectClientType(emptyBasket)
        )
      )

      val result = controller.submitReviewAuthorisations(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSelectService().url)

      journeyState.get should have[State](SelectService(Personal, testAvailableServices, emptyBasket))
    }

    "redirect to some authorisations failed when some of the invitation creations fail" in {
      givenInvitationCreationSucceeds(arn, Some(Personal), nino.value, invitationIdITSA, nino.value, "ni", Service.MtdIt, "NI")
      givenInvitationCreationFailsForService(arn, Some(Personal), nino.value, invitationIdPIR, nino.value, "ni", Service.PersonalIncomeRecord, "NI")
      givenAgentReferenceRecordExistsForArn(arn, "FOO")
      givenAgentReference(arn, nino.value, Personal)
      givenGetAgencyEmailAgentStub
      journeyState.set(
        ReviewAuthorisations(Personal, 
          availablePersonalServices,
          Set(
            AuthorisationRequest("client name", Invitation(Some(Personal), Service.MtdIt, nino)),
            AuthorisationRequest("client name", Invitation(Some(Personal), Service.PersonalIncomeRecord, nino))
          )),
        List()
      )

      val result = controller.submitReviewAuthorisations(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showSomeAuthorisationsFailed().url)
    }

    "redirect to all authorisation failed when all of the invitation creations fail" in {
      givenInvitationCreationFailsForService(arn, Some(Personal), nino.value, invitationIdPIR, nino.value, "ni", Service.PersonalIncomeRecord, "NI")
      givenAgentReferenceRecordExistsForArn(arn, "FOO")
      givenAgentReference(arn, nino.value, Personal)
      givenGetAgencyEmailAgentStub
      journeyState.set(
        ReviewAuthorisations(Personal, 
          availablePersonalServices,
          Set(
            AuthorisationRequest("client name", Invitation(Some(Personal), Service.PersonalIncomeRecord, nino))
          )),
        List()
      )

      val result = controller.submitReviewAuthorisations(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showAllAuthorisationsFailed().url)
    }
  }

  "GET /invitation-sent" should {
    val request = FakeRequest("GET", "/agents/invitation-sent")

    "show the invitation sent page for a personal service" in {
      journeyState.set(
        InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Set(Service.PersonalIncomeRecord, Service.MtdIt, Service.Vat), isAltItsa = Some(false)),
        List(
          ReviewAuthorisations(Personal, availablePersonalServices, Set.empty),
          ConfirmClient(
            AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)),
            emptyBasket),
          IdentifyClient(Personal, Service.MtdIt, emptyBasket),
          SelectService(Personal, Set(Service.MtdIt), emptyBasket),
          SelectClientType(emptyBasket)
        )
      )
      val result = controller.showInvitationSent()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyText(
        result.futureValue,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("invitation-sent.header"),
          htmlEscapedMessage("service.name.agents.auth"),
          htmlEscapedMessage("invitation-sent.email.p", "abc@xyz.com"),
          htmlEscapedMessage("invitation-sent.l9")
        )
      )

      checkInviteSentPageContainsSurveyLink(result, isAgent = true)

      journeyState.get should have[State](InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Set(Service.PersonalIncomeRecord, Service.MtdIt, Service.Vat), isAltItsa = Some(false)))
    }

    "show the invitation sent page for a personal service alt itsa" in {
      journeyState.set(
        InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Set(Service.MtdIt), isAltItsa = Some(true)),
        List(
          ReviewAuthorisations(Personal, availablePersonalServices, Set.empty),
          ConfirmClient(
            AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)),
            emptyBasket),
          IdentifyClient(Personal, Service.MtdIt, emptyBasket),
          SelectService(Personal, Set(Service.MtdIt), emptyBasket),
          SelectClientType(emptyBasket)
        )
      )
      val result = controller.showInvitationSent()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result,"Sign up your client for Making Tax Digital for Income Tax")
      checkInviteSentPageContainsSurveyLink(result, isAgent = true)
      journeyState.get should have[State](InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Set(Service.MtdIt), isAltItsa = Some(true)))
    }

    "show the invitation sent page for a personal service alt itsa with other services" in {
      journeyState.set(
        InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Set(Service.MtdIt, Service.Vat), isAltItsa = Some(true)),
        List(
          ReviewAuthorisations(Personal, availablePersonalServices, Set.empty),
          ConfirmClient(
            AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)),
            emptyBasket),
          IdentifyClient(Personal, Service.MtdIt, emptyBasket),
          SelectService(Personal, Set(Service.MtdIt, Service.Vat), emptyBasket),
          SelectClientType(emptyBasket)
        )
      )
      val result = controller.showInvitationSent()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result,"Sign up your client for Making Tax Digital for Income Tax")
      checkHtmlResultWithBodyText(result,"Check with your client that they have signed up to Making Tax Digital for VAT")
      checkInviteSentPageContainsSurveyLink(result, isAgent = true)
      journeyState.get should have[State](InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Set(Service.MtdIt, Service.Vat), isAltItsa = Some(true)))
    }

    "show the already copied across warning page when there is a legacy mapping" in {
      val request = FakeRequest("GET", "/agents/already-copied-across-itsa")

      journeyState.set(
        AlreadyCopiedAcrossItsa,
        Nil
      )

      val result = controller.showAlreadyCopiedAcrossItsa(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "already-copied.header")
    }

    "show the invitation sent page for a business service" in {
      journeyState.set(
        InvitationSent(ClientType.Business,  "invitation/link", None, "abc@xyz.com", Set(Service.Vat)),
        List(
          ConfirmClient(
            AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Business), Service.Vat, validVrn)), emptyBasket),
          IdentifyClient(Business, Service.Vat, emptyBasket),
          SelectService(Business, availablePersonalServices, emptyBasket),
          SelectClientType(emptyBasket)
        )
      )
      val result = controller.showInvitationSent()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyText(
        result.futureValue,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("invitation-sent.header"),
          htmlEscapedMessage("service.name.agents.auth"),
          htmlEscapedMessage("invitation-sent.email.p", "abc@xyz.com"),
          htmlEscapedMessage("invitation-sent.l9")
        )
      )

      checkInviteSentPageContainsSurveyLink(result, isAgent = true)

      journeyState.get should have[State](InvitationSent(ClientType.Business,  "invitation/link", None, "abc@xyz.com", Set(Service.Vat)))
    }
  }

  "GET /delete" should {
    val request = FakeRequest("GET", "/agents/delete")

    "show the delete page" in {
      journeyState.set(
        DeleteAuthorisationRequest(Personal, 
          AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino), itemId = "itemId"),
          Set.empty),
        List(
          ReviewAuthorisations(Personal, availablePersonalServices, Set.empty),
          ConfirmClient(
            AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)),
            emptyBasket),
          IdentifyClient(Personal, Service.MtdIt, emptyBasket),
          SelectService(Personal, Set(Service.PersonalIncomeRecord, Service.MtdIt, Service.Vat), emptyBasket),
          SelectClientType(emptyBasket)
        )
      )
      val result = controller.showDeleteAuthorisation("ABC123")(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyText(
        result.futureValue,
        "Are you sure you want to remove your authorisation request for Sylvia Plath?",
        "You will not send an authorisation request to manage their Making Tax Digital for Income Tax."
      )

      journeyState.get should have[State](
        DeleteAuthorisationRequest(Personal, 
          AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino), itemId = "itemId"),
          Set.empty))
    }
  }

  "POST /delete" should {
    val request = FakeRequest("POST", "/agents/invitation-sent")

    "redirect to review-authorisations when yes is selected and there is something left in the basket" in {
      journeyState.set(
        DeleteAuthorisationRequest(Personal, 
          AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)),
          Set(AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino), itemId = "itemId"))
        ),
        List(
          ReviewAuthorisations(Personal, 
            availablePersonalServices,
            Set(
              AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino), itemId = "itemId")
            )),
          ConfirmClient(
            AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)),
            emptyBasket),
          IdentifyClient(Personal, Service.MtdIt, emptyBasket),
          SelectService(Personal, Set(Service.PersonalIncomeRecord, Service.MtdIt, Service.Vat, Service.Ppt), emptyBasket),
          SelectClientType(emptyBasket)
        )
      )

      val result = controller.submitDeleteAuthorisation(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showReviewAuthorisations().url)

      journeyState.get should have[State](ReviewAuthorisations(Personal, 
        availablePersonalServices,
        Set(AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino), itemId = "itemId"))))
    }
    "redirect to all-authorisations-removed when yes is selected and there is nothing left in the basket" in {
      journeyState.set(
        DeleteAuthorisationRequest(Personal, 
          AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino), itemId = "ABC123"),
          Set(AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino), itemId = "ABC123"))
        ),
        List(
          ReviewAuthorisations(Personal, availablePersonalServices, Set.empty),
          ConfirmClient(
            AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)),
            emptyBasket),
          IdentifyClient(Personal, Service.MtdIt, emptyBasket),
          SelectService(Personal, Set(Service.PersonalIncomeRecord, Service.MtdIt, Service.Vat), emptyBasket),
          SelectClientType(emptyBasket)
        )
      )

      val result = controller.submitDeleteAuthorisation(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showAllAuthorisationsRemoved().url)

      journeyState.get should have[State](AllAuthorisationsRemoved)
    }
    "redirect to review-authorisations when no is selected and keep basket the same" in {
      journeyState.set(
        DeleteAuthorisationRequest(Personal, 
          AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino), itemId = "ABC123"),
          Set(AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino), itemId = "ABC123"))
        ),
        List(
          ReviewAuthorisations(Personal, availablePersonalServices, Set.empty),
          ConfirmClient(
            AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)),
            emptyBasket),
          IdentifyClient(Personal, Service.MtdIt, emptyBasket),
          SelectService(Personal, Set(Service.PersonalIncomeRecord, Service.MtdIt, Service.Vat, Service.Ppt), emptyBasket),
          SelectClientType(emptyBasket)
        )
      )

      val result = controller.submitDeleteAuthorisation(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showReviewAuthorisations().url)

      journeyState.get should have[State](ReviewAuthorisations(Personal, 
        availablePersonalServices,
        Set(AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino), itemId = "ABC123"))))
    }
  }
  "POST /some-create-authorisations-failed" should {
    val request = FakeRequest("GET", "/agents/some-create-authorisations-failed")

    "redirect to invitation sent" in {
      journeyState.set(
        SomeAuthorisationsFailed("invitation/link", None, "abc@xyz.com", Set.empty),
        List(
          SomeAuthorisationsFailed("invitation/link", None, "abc@xyz.com", Set.empty),
          ReviewAuthorisations(Personal, availablePersonalServices, Set.empty),
          ConfirmClient(
            AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)),
            emptyBasket),
          IdentifyClient(Personal, Service.MtdIt, emptyBasket),
          SelectService(Personal, Set(Service.PersonalIncomeRecord, Service.MtdIt, Service.Vat), emptyBasket),
          SelectClientType(emptyBasket)
        )
      )

      val result = controller.submitSomeAuthorisationsFailed(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result) shouldBe Some(routes.AgentInvitationJourneyController.showInvitationSent().url)

      journeyState.get should have[State](InvitationSent(ClientType.Personal, "invitation/link", None, "abc@xyz.com", Set.empty, isAltItsa = None))
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
        result.futureValue,
        "not-matched.header",
        "not-matched.description",
        "not-matched.advice",
        "try-again.button"
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
        result.futureValue,
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
        SomeAuthorisationsFailed("/invitation/link", None, "abc@xyz.com", Set(AuthorisationRequest("CGT_NAME", Invitation(Some(Business), Service.CapitalGains, cgtRef), AuthorisationRequest.FAILED))),
        List()
      )
      val result = controller.showSomeAuthorisationsFailed(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "create-auth-failed.header",
        "continue.button",
        "create-auth-failed.HMRC-CGT-PD"
      )
    }
  }

  "GET /all-create-authorisation-failed" should {
    val request = FakeRequest("GET", "/agents/all-create-authorisation-failed")

    "display the all create authorisations failed page" in {
      journeyState.set(
        AllAuthorisationsFailed(Set(AuthorisationRequest("CGT_NAME", Invitation(Some(Business), Service.CapitalGains, cgtRef), AuthorisationRequest.FAILED))),
        List()
      )
      val result = controller.showAllAuthorisationsFailed(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "create-auth-failed.header",
        "try-again.button",
        "create-auth-failed.HMRC-CGT-PD"
      )
    }
  }

  "GET /already-authorisation-present" should {
    val request = FakeRequest("GET", "/agents/already-authorisation-present")

    "display the already authorisation present page" in {

      supportedServices.foreach { service =>
        journeyState.set(
          ActiveAuthorisationExists(Personal, service, Set(AuthorisationRequest("CGT_NAME", Invitation(Some(Personal), Service.CapitalGains, cgtRef)))),
          List()
        )
        val result = controller.showActiveAuthorisationExists(authorisedAsValidAgent(request, arn.value))

        status(result) shouldBe 200

        val serviceKey = if(service == Service.CapitalGains) "active-authorisation-exists.p1.HMRC-CGT-PD" else s"active-authorisation-exists.p1.${service.id}"

        checkHtmlResultWithBodyMsgs(
          result.futureValue,
          "active-authorisation-exists.header",
          serviceKey,
          "active-authorisation-exists.p2"
        )
      }
    }
  }

  "GET /agents/authorisation-detected" should {

    val request = FakeRequest("GET", "/agents/authorisation-detected")

    "display the legacy authorisation detected page" in {
      journeyState.set(
        LegacyAuthorisationDetected(Set(AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)))),
        List()
      )
      val result = controller.showLegacyAuthorisationDetected(
        authorisedAsValidAgent(request, arn.value))

      status(result.futureValue) shouldBe 200

      checkHtmlResultWithBodyMsgs(result,
      "legacy-auth-detected.header",
      "legacy-auth-detected.yes",
      "legacy-auth-detected.no")
    }
  }

  "POST /agents/authorisation-detected" should {

    val request = FakeRequest("POST", "/agents/authorisation-detected")

    "display the page with errors when input invalid" in {
      journeyState.set(
        LegacyAuthorisationDetected(Set(AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)))),
        List()
      )
      val result = controller.submitLegacyAuthorisationDetected(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "INVALID"), arn.value))

      status(result.futureValue) shouldBe 200

      checkHtmlResultWithBodyMsgs(result,"error.legacy-auth-detected.required")
    }

    "redirect to /agent-mapping/start when yes" in {
      journeyState.set(
        LegacyAuthorisationDetected(Set(AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)))),
        List()
      )
      val result = controller.submitLegacyAuthorisationDetected(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))

      status(result.futureValue) shouldBe 303

      redirectLocation(result) shouldBe Some("http://localhost:9438/agent-mapping/start")

    }

    "redirect to /invitations/agents/review-authorisations when no" in {
      journeyState.set(
        LegacyAuthorisationDetected(Set(AuthorisationRequest("Sylvia Plath", Invitation(Some(ClientType.Personal), Service.MtdIt, nino)))),
        List()
      )
      val result = controller.submitLegacyAuthorisationDetected(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "false"), arn.value))

      status(result.futureValue) shouldBe 303

      redirectLocation(result) shouldBe Some("/invitations/agents/review-authorisations")
    }
  }

  "GET /already-authorisation-pending" should {
    val request = FakeRequest("GET", "/agents/already-authorisation-pending")

    "display the already authorisation pending page" in {
      journeyState.set(
        PendingInvitationExists(Personal, "Charmarti Ltd.", "/invitation-link/ABC123", emptyBasket),
        List()
      )
      val result = controller.showPendingAuthorisationExists(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      val html = Jsoup.parse(Helpers.contentAsString(result))
      html.title shouldBe  "You already created an authorisation request for this tax service - Ask a client to authorise you - GOV.UK"
      html.select("main h1").text() shouldBe "You already created an authorisation request for this tax service"
      html.select("main p").get(0).text() shouldBe "You cannot continue until Charmarti Ltd. has accepted the authorisation request link."
      html.select("main h2").text() shouldBe "What you can do next"
      html.select("main p").get(1).text() shouldBe "Resend the authorisation request link that was created when you originally asked Charmarti Ltd. to authorise you:"
      html.select("main p").get(2).text() should include ("/invitation-link/ABC123")
      html.select("main p").get(2).classNames() contains "govuk-!-font-weight-bold"
      html.select("main p").get(2).classNames() contains "govuk-body"
      html.select("main .govuk-button").text() shouldBe "Start new request"
      html.select("main .govuk-button").attr("href") should startWith("/invitations/")
    }
  }

  "GET /not-signed-up" should {
    val request = FakeRequest("GET", "/agents/not-signed-up")

    "display the not signed up page" in {
      journeyState.set(
        ClientNotSignedUp(Service.MtdIt, emptyBasket),
        List()
      )
      val result = controller.showClientNotSignedUp(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result.futureValue, htmlEscapedMessage("not-enrolled.title", "signed up to Making Tax Digital for Income Tax"))
      checkHtmlResultWithBodyText(result.futureValue, htmlEscapedMessage("not-enrolled.p", "signed up."))
      checkHtmlResultWithBodyText(result.futureValue, htmlEscapedMessage("not-enrolled.existing.header", "Self Assessment"))
      checkResultContainsLink(result,"http://localhost:9438/agent-mapping/start","copy across an existing authorisation")
      checkResultContainsLink(result,"http://localhost:9438/agent-mapping/start","copy across an existing authorisation")

    }
  }

  "GET /client-not-registered" should {
    val request = FakeRequest("GET", "/agents/client-not-registered")

    "display the client not registered page" in {
      journeyState.set(
        ClientNotRegistered(emptyBasket),
        List()
      )
      val result = controller.showClientNotRegistered(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "client-not-registered.header",
        "client-not-registered.p1",
        "client-not-registered.h2"
      )
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
        result.futureValue,
        "all-authorisations-removed.header",
        "all-authorisations-removed.p",
        "new-request.button"
      )
    }
  }

  "GET /access-removed" should {
    val request = FakeRequest("GET", "/agents/access-removed")

    "display the access removed page" in {
      journeyState.set(AgentSuspended(Service.MtdIt, Set.empty), List())
      val result = controller.showAgentSuspended(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "agent-suspended.heading.single", "agent-suspended.p1.HMRC-MTD-IT", "agent-suspended.p2.single")
    }
  }
}
