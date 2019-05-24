package uk.gov.hmrc.agentinvitationsfrontend.controllers
import org.joda.time.LocalDate
import org.scalatest.BeforeAndAfter
import play.api.Application
import play.api.test.FakeRequest
import play.api.test.Helpers.redirectLocation
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.duration._

class AgentLedDeauthJourneyControllerISpec extends BaseISpec with StateAndBreadcrumbsMatchers with BeforeAndAfter {

  val enabledServices: Set[String] = Set(HMRCMTDIT, HMRCPIR, HMRCMTDVAT)

  implicit val hc: HeaderCarrier = HeaderCarrier()
  override implicit lazy val app: Application = appBuilder
    .overrides(new TestAgentLedDeauthJourneyModule)
    .build()

  val timeout = 2.seconds

  lazy val journeyState = app.injector.instanceOf[TestAgentLedDeauthJourneyService]

  val availableServices = Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)

  val controller = app.injector.instanceOf[AgentLedDeauthJourneyController]

  before {
    journeyState.clear
  }

  "GET /fsm/agents/cancel-authorisation" should {
    "redirect to the client type page when there is no current state" in {
      journeyState.clear(hc, ec)
      val request = FakeRequest("GET", "/agents/cancel-authorisation/client-type")
      val root = controller.agentLedDeauthRoot()
      val result = root(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 303
      redirectLocation(result)(timeout) shouldBe Some(routes.AgentLedDeauthJourneyController.showClientType().url)
    }
  }

  "GET /fsm/agents/cancel-authorisation/client-type" should {
    "display the client type page" when {
      "there is a current state of SelectClientType and no breadcrumbs" in {
        journeyState.set(SelectClientType, Nil)
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
      "there is no state or breadcrumbs redirect to the client type page" in {
        journeyState.clear(hc, ec)
        val request = FakeRequest("GET", "/agents/cancel-authorisation/client-type")
        val selectClientType = controller.showClientType()

        val result = selectClientType(authorisedAsValidAgent(request, arn.value))
        status(result) shouldBe 303
        redirectLocation(result)(timeout) shouldBe Some(routes.AgentLedDeauthJourneyController.showClientType().url)
      }
      "there is a state and breadcrumbs" in {
        journeyState
          .set(SelectClientType, List(AuthorisationCancelled("HMRC-MTD-IT", Some("clienty name"), "agenty name")))
        val request = FakeRequest("GET", "/agents/cancel-authorisation/client-type")
        val selectClientType = controller.showClientType()

        val result = selectClientType(authorisedAsValidAgent(request, arn.value))
        status(result) shouldBe 200
        checkHtmlResultWithBodyText(result, htmlEscapedMessage("cancel-authorisation.client-type.header"))
        checkResultContainsBackLink(result, routes.AgentLedDeauthJourneyController.showAuthorisationCancelled().url)
      }
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

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showSelectService().url
    }
    "redirect to select service page when client type is business" in {
      journeyState.set(SelectClientType, Nil)
      val request = FakeRequest("POST", "/agents/cancel-authorisation/client-type")

      val result =
        controller.submitClientType(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> "business"), arn.value))
      status(result) shouldBe 303

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
      checkResultContainsBackLink(result, "/invitations/fsm/agents/cancel-authorisation/client-type")
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
      checkResultContainsBackLink(result, "/invitations/fsm/agents/cancel-authorisation/client-type")
    }
  }
  "POST /fsm/agents/cancel-authorisation/select-personal-service" should {
    "redirect to identify client for personal service" in {
      journeyState.set(SelectServicePersonal(enabledServices), Nil)
      val request = FakeRequest("POST", "/agents/cancel-authorisation/select-personal-service")

      val result =
        controller.submitPersonalService(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> "HMRC-MTD-IT"), arn.value))
      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showIdentifyClient().url
    }
  }
  "POST /fsm/agents/cancel-authorisation/select-business-service" should {
    "redirect to identify client for business service when yes is selected" in {
      journeyState.set(SelectServiceBusiness, Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/select-business-service")

      val result =
        controller.submitBusinessService(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))
      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showIdentifyClient().url
    }
  }
  "GET /fsm/agents/cancel-authorisation/identify-client" should {
    "display the identify client page for itsa service" in {
      journeyState.set(IdentifyClientPersonal(HMRCMTDIT), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/identify-client")
      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("identify-client.nino.label"),
        htmlEscapedMessage("identify-client.postcode.label"))
    }
    "display the identify client page for irv service" in {
      journeyState.set(IdentifyClientPersonal(HMRCPIR), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/identify-client")
      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("identify-client.nino.label"),
        htmlEscapedMessage("identify-client.irv-date-of-birth.label"))
    }
    "display the identify client page for personal vat service" in {
      journeyState.set(IdentifyClientPersonal(HMRCMTDVAT), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/identify-client")
      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("identify-client.vrn.label"),
        htmlEscapedMessage("identify-client.vat-registration-date.label"))
    }
    "display the identify client page for business service" in {
      journeyState.set(IdentifyClientBusiness, Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/identify-client")
      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("identify-client.vrn.label"),
        htmlEscapedMessage("identify-client.vat-registration-date.label"))
    }
  }
  "POST /fsm/agents/cancel-authorisation/identify-itsa-client" should {
    "redirect to confirm client" in {
      journeyState.set(IdentifyClientPersonal(HMRCMTDIT), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/identify-itsa-client")

      givenMatchingClientIdAndPostcode(validNino, validPostcode)
      givenTradingName(validNino, "Betty Boop")

      val result =
        controller.submitIdentifyItsaClient(authorisedAsValidAgent(
          request.withFormUrlEncodedBody("clientIdentifier" -> s"${validNino.value}", "postcode" -> s"$validPostcode"),
          arn.value))
      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showConfirmClient().url
    }
    "redirect to not matched when the clientId and postcode don't match" in {
      journeyState.set(IdentifyClientPersonal(HMRCMTDIT), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/identify-itsa-client")

      givenNonMatchingClientIdAndPostcode(validNino, validPostcode)

      val result =
        controller.submitIdentifyItsaClient(authorisedAsValidAgent(
          request.withFormUrlEncodedBody("clientIdentifier" -> s"${validNino.value}", "postcode" -> s"$validPostcode"),
          arn.value))
      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController
        .showKnownFactNotMatched()
        .url
    }
    "redirect to not signed up when the client is not enrolled for ITSA" in {
      journeyState.set(IdentifyClientPersonal(HMRCMTDIT), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/identify-itsa-client")

      givenNotEnrolledClientITSA(validNino, validPostcode)

      val result =
        controller.submitIdentifyItsaClient(authorisedAsValidAgent(
          request.withFormUrlEncodedBody("clientIdentifier" -> s"${validNino.value}", "postcode" -> s"$validPostcode"),
          arn.value))
      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController
        .showNotSignedUp()
        .url
    }
  }
  "POST /fsm/agents/cancel-authorisation/identify-irv-client" should {
    "redirect to confirm cancel because redirect to confirm flag is off by default" in {
      journeyState.set(IdentifyClientPersonal(HMRCPIR), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/identify-irv-client")

      givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
      givenCitizenDetailsAreKnownFor(validNino.value, "Barry", "Block")
      givenAfiRelationshipIsActiveForAgent(arn, validNino)

      val result =
        controller.submitIdentifyIrvClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "clientIdentifier" -> s"${validNino.value}",
              "dob.year"         -> "1980",
              "dob.month"        -> "07",
              "dob.day"          -> "07"),
            arn.value))
      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showConfirmCancel().url
    }
  }
  "POST /fsm/agents/cancel-authorisation/identify-vat-client" should {
    "redirect to confirm client for personal VAT" in {
      journeyState.set(IdentifyClientPersonal(HMRCMTDVAT), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/identify-vat-client")

      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(validRegistrationDate), 204)
      givenClientDetails(validVrn)

      val result =
        controller.submitIdentifyVatClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "clientIdentifier"       -> s"${validVrn.value}",
              "registrationDate.year"  -> "2007",
              "registrationDate.month" -> "7",
              "registrationDate.day"   -> "7"),
            arn.value
          ))
      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showConfirmClient().url
    }
    "redirect to confirm client for business VAT" in {
      journeyState.set(IdentifyClientBusiness, Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/identify-vat-client")

      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(validRegistrationDate), 204)
      givenClientDetails(validVrn)

      val result =
        controller.submitIdentifyVatClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "clientIdentifier"       -> s"${validVrn.value}",
              "registrationDate.year"  -> "2007",
              "registrationDate.month" -> "7",
              "registrationDate.day"   -> "7"),
            arn.value
          ))
      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showConfirmClient().url
    }
    "redirect to cannot create request when a migration is happening" in {
      journeyState.set(IdentifyClientBusiness, Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/identify-vat-client")

      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(validRegistrationDate), 423)
      givenClientDetails(validVrn)

      val result =
        controller.submitIdentifyVatClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "clientIdentifier"       -> s"${validVrn.value}",
              "registrationDate.year"  -> "2007",
              "registrationDate.month" -> "7",
              "registrationDate.day"   -> "7"),
            arn.value
          ))
      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController
        .showCannotCreateRequest()
        .url
    }
  }
  "GET /fsm/agents/cancel-authorisation/confirm-client" should {
    "display the confirm client page for ITSA" in {
      journeyState.set(ConfirmClientItsa(Some("Barry Block"), validNino), List(IdentifyClientPersonal(HMRCMTDIT)))
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/confirm-client")
      val result = controller.showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Barry Block",
        htmlEscapedMessage("cancel-authorisation.confirm-client.header"),
        htmlEscapedMessage("cancel-authorisation.confirm-client.yes")
      )
      checkResultContainsBackLink(result, "/invitations/fsm/agents/cancel-authorisation/identify-client")
    }
    "display the confirm client page for IRV" in {
      journeyState
        .set(ConfirmClientIrv(Some("Barry Block"), validNino), List(IdentifyClientPersonal(HMRCPIR)))
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/confirm-client")
      val result = controller.showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Barry Block",
        htmlEscapedMessage("cancel-authorisation.confirm-client.header"),
        htmlEscapedMessage("cancel-authorisation.confirm-client.yes")
      )
      checkResultContainsBackLink(result, "/invitations/fsm/agents/cancel-authorisation/identify-client")
    }
    "display the confirm client page for personal VAT" in {
      journeyState
        .set(ConfirmClientPersonalVat(Some("Barry Block"), validVrn), List(IdentifyClientPersonal(HMRCMTDVAT)))
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/confirm-client")
      val result = controller.showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Barry Block",
        htmlEscapedMessage("cancel-authorisation.confirm-client.header"),
        htmlEscapedMessage("cancel-authorisation.confirm-client.yes")
      )
      checkResultContainsBackLink(result, "/invitations/fsm/agents/cancel-authorisation/identify-client")
    }
    "display the confirm client page for business VAT" in {
      journeyState.set(ConfirmClientBusiness(Some("Barry Block"), validVrn), List(IdentifyClientBusiness))
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/confirm-client")
      val result = controller.showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Barry Block",
        htmlEscapedMessage("cancel-authorisation.confirm-client.header"),
        htmlEscapedMessage("cancel-authorisation.confirm-client.yes")
      )
      checkResultContainsBackLink(result, "/invitations/fsm/agents/cancel-authorisation/identify-client")
    }
  }

  "POST /fsm/agents/cancel-authorisation/confirm-client" should {
    "redirect to confirm cancel when YES is selected" in {
      journeyState.set(ConfirmClientItsa(Some("Sufjan Stevens"), Nino(nino)), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-client")

      givenCheckRelationshipItsaWithStatus(arn, nino, 200)

      val result =
        controller.submitConfirmClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("accepted" -> "true"),
            arn.value
          ))
      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showConfirmCancel().url
    }
    "redirect to not authorised when there are is no active relationship to de-authorise" in {
      journeyState.set(ConfirmClientItsa(Some("Sufjan Stevens"), Nino(nino)), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-client")

      givenCheckRelationshipItsaWithStatus(arn, nino, 404)

      val result =
        controller.submitConfirmClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("accepted" -> "true"),
            arn.value
          ))
      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showNotAuthorised().url
    }
  }
  "GET /fsm/agents/cancel-authorisation/confirm-cancel" should {
    "display the confirm cancel page" in {
      journeyState.set(
        ConfirmCancel(HMRCMTDIT, Some("Barry Block"), validNino.value),
        List(ConfirmClientItsa(Some("Barry Block"), validNino)))
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/confirm-cancel")
      val result = controller.showConfirmCancel(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("cancel-authorisation.confirm-cancel.header"),
        htmlEscapedMessage("cancel-authorisation.confirm-cancel.p1.HMRC-MTD-IT", "Barry Block")
      )
      checkResultContainsBackLink(result, "/invitations/fsm/agents/cancel-authorisation/confirm-client")
    }
  }
  "POST /fsm/agents/cancel-authorisation/confirm-cancel" should {
    "redirect to the authorisation cancelled page" in {
      journeyState.set(ConfirmCancel(HMRCMTDIT, Some("Sufjan Stevens"), nino), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-cancel")

      givenCancelledAuthorisationItsa(arn, Nino(nino), 204)
      givenGetAgencyNameClientStub(arn)

      val result =
        controller.submitConfirmCancel(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("accepted" -> "true"),
            arn.value
          ))
      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController
        .showAuthorisationCancelled()
        .url
    }
    "redirect to response failed page when the relationship termination fails" in {
      journeyState.set(ConfirmCancel(HMRCMTDIT, Some("Sufjan Stevens"), nino), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-cancel")

      givenCancelledAuthorisationItsa(arn, Nino(nino), 404)
      givenGetAgencyNameClientStub(arn)

      val result =
        controller.submitConfirmCancel(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("accepted" -> "true"),
            arn.value
          ))
      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController
        .showResponseFailed()
        .url
    }
  }
  "GET /fsm/agents/cancel-authorisation/cancelled" should {
    "display the authorisation cancelled page" in {
      journeyState.set(AuthorisationCancelled(HMRCMTDIT, Some("client man"), "Agent man"), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/cancelled")
      val result = controller.showAuthorisationCancelled(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("cancel-authorisation.cancelled.header"),
        htmlEscapedMessage("cancel-authorisation.cancelled.p1.HMRC-MTD-IT", "Agent man", "client man")
      )
    }
  }
  "GET /fsm/agents/cancel-authorisation/not-matched" should {
    "display the known facts dont match page" in {
      journeyState.set(KnownFactNotMatched, Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/not-matched")
      val result = controller.showKnownFactNotMatched(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("cancel-authorisation.not-matched.header"),
        htmlEscapedMessage("cancel-authorisation.not-matched.description"))
    }
  }
  "GET /fsm/agents/cancel-authorisation/not-signed-up" should {
    "display the not enrolled page" in {
      journeyState.set(NotSignedUp(HMRCMTDIT), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/not-signed-up")
      val result = controller.showNotSignedUp(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("not-enrolled.p1.HMRC-MTD-IT"),
        htmlEscapedMessage("not-enrolled.p2"))
    }
  }
  "GET /fsm/agents/cancel-authorisation/not-signed-up" should {
    "display the cannot create request page" in {
      journeyState.set(CannotCreateRequest, Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/not-signed-up")
      val result = controller.showCannotCreateRequest(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("cannot-create-request.header"),
        htmlEscapedMessage("cannot-create-request.p1"))
    }
  }
  "GET /fsm/agents/cancel-authorisation/not-authorised" should {
    "display the not authorised page" in {
      journeyState.set(NotAuthorised(HMRCMTDIT), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/not-authorised")
      val result = controller.showNotAuthorised(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("problem.header"),
        htmlEscapedMessage("not-authorised.HMRC-MTD-IT.p"))
    }
  }
  "GET /fsm/agents/cancel-authorisation/response-failed" should {
    "display the response failed page" in {
      journeyState.set(ResponseFailed, Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/response-failed")
      val result = controller.showResponseFailed(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "cancel-authorisation.response-failed.header",
        "cancel-authorisation.response-failed.description"
      )
    }
  }

}
