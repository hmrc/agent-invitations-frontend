package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.time.{LocalDate, LocalDateTime}
import org.jsoup.Jsoup
import org.scalatest.BeforeAndAfter
import play.api.Application
import play.api.libs.json.Json
import play.api.test.Helpers.{defaultAwaitTimeout, redirectLocation}
import play.api.test.{FakeRequest, Helpers}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.models.{ClientType, Services}
import uk.gov.hmrc.agentinvitationsfrontend.support.{BaseISpec, Css}
import uk.gov.hmrc.agentmtdidentifiers.model.{Service, SuspensionDetails}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class AgentLedDeauthJourneyControllerISpec extends BaseISpec with StateAndBreadcrumbsMatchers with BeforeAndAfter {

  val enabledServices: Set[Service] = Set(Service.MtdIt, Service.PersonalIncomeRecord, Service.Vat, Service.CapitalGains, Service.Ppt)

  implicit val hc: HeaderCarrier = HeaderCarrier()
  override implicit lazy val app: Application = appBuilder
    .overrides(new TestAgentLedDeauthJourneyModule)
    .build()

  def app(extraConfig: Map[String, Any]): Application = appBuilder
    .configure(extraConfig)
    .overrides(new TestAgentLedDeauthJourneyModule)
    .build()

  implicit val timeout = 2.seconds

  lazy val journeyState = app.injector.instanceOf[TestAgentLedDeauthJourneyService]

  val controller: AgentLedDeauthJourneyController = app.injector.instanceOf[AgentLedDeauthJourneyController]

  before {
    journeyState.clear
  }

  "GET /agents/cancel-authorisation" should {
    "redirect to the client type page when there is no current state" in {
      journeyState.clear
      val request = FakeRequest("GET", "/agents/cancel-authorisation/client-type")
      val root = controller.agentLedDeauthRoot()
      val result = root(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 303
      Helpers.redirectLocation(result)(timeout) shouldBe Some(routes.AgentLedDeauthJourneyController.showClientType.url)
    }
  }

  "GET /agents/cancel-authorisation/client-type" should {

    lazy val request = FakeRequest("GET", "/agents/cancel-authorisation/client-type")

    "display the client type page" when {

      "there is a current state of SelectClientType and no breadcrumbs" in {
        journeyState.set(SelectClientType, Nil)
        val selectClientType = controller.showClientType()

        val result = selectClientType(authorisedAsValidAgent(request, arn.value))
        status(result) shouldBe 200
        checkHtmlResultWithBodyText(
          result.futureValue,
          hasMessage(
            "generic.title",
            htmlEscapedMessage("cancel-authorisation.client-type.header"),
            htmlEscapedMessage("service.name.agents.de-auth")
          ),
          htmlEscapedMessage("cancel-authorisation.client-type.header"),
          hasMessage("cancel-authorisation.client-type.p1")
        )
        checkResultContainsBackLink(result, s"http://localhost:$wireMockPort/agent-services-account/home")
      }
      "there is no state or breadcrumbs redirect to the client type page" in {
        journeyState.clear
        val selectClientType = controller.showClientType()

        val result = selectClientType(authorisedAsValidAgent(request, arn.value))
        status(result) shouldBe 303
        Helpers.redirectLocation(result)(timeout) shouldBe Some(routes.AgentLedDeauthJourneyController.showClientType.url)
      }
      "there is a state and breadcrumbs" in {
        journeyState
          .set(SelectClientType, List(AuthorisationCancelled(Service.MtdIt, Some("clienty name"), "agenty name")))
        val selectClientType = controller.showClientType()

        val result = selectClientType(authorisedAsValidAgent(request, arn.value))
        status(result) shouldBe 200
        checkHtmlResultWithBodyText(result.futureValue, htmlEscapedMessage("cancel-authorisation.client-type.header"))
        checkResultContainsBackLink(result, routes.AgentLedDeauthJourneyController.showAuthorisationCancelled.url)
      }
    }

    "redirect to ACRF to cancel auth request when the enableAcrfRedirects feature is enabled" in {

      val appWithAcrfFeature: Application = appBuilder
        .overrides(new TestAgentInvitationJourneyModule)
        .configure("features.enable-acrf-redirects" -> true)
        .build()

      val controllerWithAcrfFeature: AgentLedDeauthJourneyController =
        appWithAcrfFeature.injector.instanceOf[AgentLedDeauthJourneyController]

      val result = controllerWithAcrfFeature.showClientType()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(appConfig.cancelAuthRequestUrl)
    }
  }

  "POST /agents/cancel-authorisation/client-type" should {
    "redirect to select service page when client type is personal" in {
      List("personal", "business", "trust").foreach { clientType =>
        journeyState.set(SelectClientType, Nil)
        val request = FakeRequest("POST", "/agents/cancel-authorisation/client-type")

        val result =
          controller.submitClientType(authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> clientType), arn.value))
        status(result) shouldBe 303

        Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showSelectService.url
      }

    }
  }

  "GET /agents/cancel-authorisation/select-service" should {
    "show the select service page for personal services" in {
      journeyState.set(SelectService(ClientType.Personal, Services.supportedServicesFor(ClientType.Personal)), Nil)
      val request = FakeRequest("GET", "/agents/cancel-authorisation/select-service")
      val result = controller.showSelectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        hasMessage(
          "generic.title",
          htmlEscapedMessage("cancel-authorisation.personal.select-service.header"),
          htmlEscapedMessage("service.name.agents.de-auth")
        ),
        htmlEscapedMessage("cancel-authorisation.personal.select-service.header")
      )
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/client-type")
    }
    "show the select service page for business service (single service version when only one service is available)" in {
      val myApp = app(Map("features.show-plastic-packaging-tax" -> false))
      val myController: AgentLedDeauthJourneyController = myApp.injector.instanceOf[AgentLedDeauthJourneyController]
      val myJourneyState = myApp.injector.instanceOf[TestAgentLedDeauthJourneyService]

      myJourneyState.set(SelectService(ClientType.Business, Set(Service.Vat)), Nil)
      val request = FakeRequest("GET", "/agents/cancel-authorisation/select-business-service")
      val result = myController.showSelectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        hasMessage(
          "generic.title",
          htmlEscapedMessage("cancel-authorisation.business.select-service.single.header"),
          htmlEscapedMessage("service.name.agents.de-auth")
        ),
        htmlEscapedMessage("cancel-authorisation.business.select-service.single.header"),
        hasMessage("global.yes"),
        hasMessage("global.no")
      )
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/client-type")
    }
    "show the select service page for business service (multiple service version when more than one service is available)" in {
      journeyState.set(SelectService(ClientType.Business, Set(Service.Vat, Service.Ppt)), Nil)
      val request = FakeRequest("GET", "/agents/cancel-authorisation/select-service")
      val result = controller.showSelectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        hasMessage(
          "generic.title",
          htmlEscapedMessage("cancel-authorisation.business.select-service.header"),
          htmlEscapedMessage("service.name.agents.de-auth")
        ),
        htmlEscapedMessage("cancel-authorisation.business.select-service.header")
      )
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/client-type")
    }

    "show the select service page for trust service" in {
      journeyState.set(SelectService(ClientType.Trust, Services.supportedServicesFor(ClientType.Trust)), Nil)
      val request = FakeRequest("GET", "/agents/cancel-authorisation/select-service")
      val result = controller.showSelectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        hasMessage(
          "generic.title",
          htmlEscapedMessage("cancel-authorisation.trust.select-service.header"),
          htmlEscapedMessage("service.name.agents.de-auth")
        ),
        htmlEscapedMessage("cancel-authorisation.trust.select-service.header"),
        hasMessage("cancel-authorisation.personal.select-service.HMRC-TERS-ORG"),
        hasMessage("cancel-authorisation.personal.select-service.HMRC-CGT-PD")
      )
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/client-type")
    }
  }

  "POST /agents/cancel-authorisation/select-personal-service" should {
    "redirect to identify client for personal service" in {
      journeyState.set(SelectService(ClientType.Personal, Services.supportedServicesFor(ClientType.Personal)), Nil)
      val request = FakeRequest("POST", "/agents/cancel-authorisation/select-personal-service")

      val result =
        controller.submitSelectService(authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> "HMRC-MTD-IT"), arn.value))
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showIdentifyClient.url
    }
  }

  // Business service select when only one service available
  "POST /agents/cancel-authorisation/select-business-service-single" should {
    "redirect to identify client for business service when yes is selected" in {
      val myApp = app(Map("features.show-plastic-packaging-tax" -> false))
      val myController: AgentLedDeauthJourneyController = myApp.injector.instanceOf[AgentLedDeauthJourneyController]
      val myJourneyState = myApp.injector.instanceOf[TestAgentLedDeauthJourneyService]

      myJourneyState.set(SelectService(ClientType.Business, Set(Service.Vat)), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/select-business-service-single")

      val result =
        myController.submitBusinessServiceSingle(authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value))
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showIdentifyClient.url
    }
  }

  // Business service select when more than one service available
  "POST /agents/cancel-authorisation/select-business-service" should {
    "redirect to identify client for business service when a valid service is selected" in {
      // there should be two services enabled for Business by default so no config change is required
      journeyState.set(SelectService(ClientType.Business, Set(Service.Vat, Service.Ppt)), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/select-business-service")

      val result =
        controller.submitSelectService(authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> "HMRC-MTD-VAT"), arn.value))
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showIdentifyClient.url
    }
  }

  "POST /agents/cancel-authorisation/select-trust-service" should {
    "redirect to identify trust client when trust is selected" in {
      journeyState.set(SelectService(ClientType.Trust, Services.supportedServicesFor(ClientType.Trust)), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/select-trust-service")

      val result =
        controller.submitSelectService(authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> "HMRC-TERS-ORG"), arn.value))
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showIdentifyClient.url
    }

    "redirect to identify cgt client when cgt is selected" in {
      journeyState.set(SelectService(ClientType.Trust, Services.supportedServicesFor(ClientType.Trust)), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/select-trust-service")

      val result =
        controller.submitSelectService(authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> "HMRC-CGT-PD"), arn.value))
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showIdentifyClient.url
    }
  }

  "GET /agents/cancel-authorisation/identify-client" should {
    "display the identify client page for itsa service" in {
      journeyState.set(IdentifyClient(ClientType.Personal, Service.MtdIt), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/identify-client")
      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        htmlEscapedMessage("identify-client.nino.label"),
        htmlEscapedMessage("identify-client.postcode.label")
      )
    }
    "display the identify client page for irv service" in {
      journeyState.set(IdentifyClient(ClientType.Personal, Service.PersonalIncomeRecord), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/identify-client")
      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        htmlEscapedMessage("identify-client.nino.label"),
        htmlEscapedMessage("identify-client.irv-date-of-birth.label")
      )
    }
    "display the identify client page for personal vat service" in {
      journeyState.set(IdentifyClient(ClientType.Personal, Service.Vat), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/identify-client")
      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        htmlEscapedMessage("identify-client.vrn.label"),
        htmlEscapedMessage("identify-client.vat-registration-date.label")
      )
    }

    "display the identify client page for personal cgt service" in {
      journeyState.set(IdentifyClient(ClientType.Personal, Service.CapitalGains), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/identify-client")
      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        htmlEscapedMessage("identify-cgt-client.header"),
        htmlEscapedMessage("identify-cgt-client.hint")
      )
    }

    "display the identify client page for business service" in {
      journeyState.set(IdentifyClient(ClientType.Business, Service.Vat), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/identify-client")
      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        htmlEscapedMessage("identify-client.vrn.label"),
        htmlEscapedMessage("identify-client.vat-registration-date.label")
      )
    }

    "display the identify client page for trust service" in {
      journeyState.set(IdentifyClient(ClientType.Trust, Service.Trust), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/identify-client")
      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "identify-trust-client.header",
        "identify-trust-client.p1",
        "identify-trust-client.p2",
        "identify-trust-alternative",
        "identify-trust.suggestion",
        "continue.button"
      )
      checkHtmlResultWithBodyText(
        result.futureValue,
        "Use a Unique Taxpayer Reference (UTR) if the trust is taxable or a Unique Reference Number (URN) if the trust is non-taxable."
      )

    }

    "display the identify client page for cgt trust service" in {
      journeyState.set(IdentifyClient(ClientType.Trust, Service.CapitalGains), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/identify-client")
      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result.futureValue, "identify-cgt-client.header", "identify-cgt-client.p1")
    }
  }
  "POST /agents/cancel-authorisation/identify-itsa-client" should {
    "redirect to confirm client" in {
      journeyState.set(IdentifyClient(ClientType.Personal, Service.MtdIt), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/identify-itsa-client")

      givenMatchingClientIdAndPostcode(validNino, validPostcode)
      givenTradingName(validNino, "Betty Boop")

      val result =
        controller.submitIdentifyItsaClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("clientIdentifier" -> s"${validNino.value}", "postcode" -> s"$validPostcode"),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showConfirmClient.url
    }
    "redirect to not matched when the clientId and postcode don't match" in {
      journeyState.set(IdentifyClient(ClientType.Personal, Service.MtdIt), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/identify-itsa-client")

      givenNonMatchingClientIdAndPostcode(validNino, validPostcode)

      val result =
        controller.submitIdentifyItsaClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("clientIdentifier" -> s"${validNino.value}", "postcode" -> s"$validPostcode"),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showKnownFactNotMatched.url
    }
    "redirect to not signed up when the client is not enrolled for ITSA" in {
      journeyState.set(IdentifyClient(ClientType.Personal, Service.MtdIt), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/identify-itsa-client")

      givenNotEnrolledClientITSA(validNino, validPostcode)

      val result =
        controller.submitIdentifyItsaClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("clientIdentifier" -> s"${validNino.value}", "postcode" -> s"$validPostcode"),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showNotSignedUp.url
    }
  }
  "POST /agents/cancel-authorisation/identify-irv-client" should {
    "redirect to confirm cancel" in {
      journeyState.set(IdentifyClient(ClientType.Personal, Service.PersonalIncomeRecord), Nil)
      val request = FakeRequest("POST", "agents/cancel-authorisation/identify-irv-client")

      givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
      givenCitizenDetailsAreKnownFor(validNino, "Barry", "Block")
      givenAfiRelationshipIsActiveForAgent(arn, validNino)

      val result =
        controller.submitIdentifyIrvClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("clientIdentifier" -> s"${validNino.value}", "dob.year" -> "1980", "dob.month" -> "07", "dob.day" -> "07"),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showConfirmCancel.url
    }
  }
  "POST /agents/cancel-authorisation/identify-vat-client" should {
    "redirect to confirm client for personal VAT" in {
      journeyState.set(IdentifyClient(ClientType.Personal, Service.Vat), Nil)
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
              "registrationDate.day"   -> "7"
            ),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showConfirmClient.url
    }

    "redirect to confirm client for personal VAT when client insolvent" in {
      journeyState.set(IdentifyClient(ClientType.Personal, Service.Vat), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/identify-vat-client")

      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(validRegistrationDate), 403, true)
      givenClientDetails(validVrn)

      val result =
        controller.submitIdentifyVatClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "clientIdentifier"       -> s"${validVrn.value}",
              "registrationDate.year"  -> "2007",
              "registrationDate.month" -> "7",
              "registrationDate.day"   -> "7"
            ),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showConfirmClient.url
    }
    "redirect to confirm client for business VAT" in {
      journeyState.set(IdentifyClient(ClientType.Business, Service.Vat), Nil)
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
              "registrationDate.day"   -> "7"
            ),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showConfirmClient.url
    }
  }

  "POST /agents/cancel-authorisation/identify-trust-client - UTR" should {

    "redirect to confirm client for trust" in {
      givenTrustClientReturns(validUtr, 200, Json.toJson(trustResponse).toString())
      journeyState.set(IdentifyClient(ClientType.Trust, Service.Trust), Nil)

      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/identify-trust-client")

      val result =
        controller.submitIdentifyTrustClient(authorisedAsValidAgent(request.withFormUrlEncodedBody("taxId" -> s"${validUtr.value}"), arn.value))

      status(result) shouldBe 303

    }
    "redirect to confirm client for trust with urn" in {
      givenTrustClientReturns(validUrn, 200, Json.toJson(trustResponse).toString())
      journeyState.set(IdentifyClient(ClientType.Trust, Service.TrustNT), Nil)

      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/identify-trust-client")

      val result =
        controller.submitIdentifyTrustClient(authorisedAsValidAgent(request.withFormUrlEncodedBody("taxId" -> s"${validUrn.value}"), arn.value))

      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showConfirmClient.url
    }
    "redirect to /not-found for trust if trust details are not found for given utr" in {
      givenTrustClientReturns(validUtr, 200, trustNotFoundJson)
      journeyState.set(IdentifyClient(ClientType.Trust, Service.Trust), Nil)

      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/identify-trust-client")

      val result =
        controller.submitIdentifyTrustClient(authorisedAsValidAgent(request.withFormUrlEncodedBody("taxId" -> s"${validUtr.value}"), arn.value))

      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showKnownFactNotMatched.url
    }
    "redirect to /not-found for trust if trust details are not found for given urn" in {
      givenTrustClientReturns(validUrn, 200, trustNotFoundJson)
      journeyState.set(IdentifyClient(ClientType.Trust, Service.TrustNT), Nil)

      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/identify-trust-client")

      val result =
        controller.submitIdentifyTrustClient(authorisedAsValidAgent(request.withFormUrlEncodedBody("taxId" -> s"${validUrn.value}"), arn.value))

      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showKnownFactNotMatched.url
    }
  }

  "POST /agents/cancel-authorisation/identify-cgt-client" should {

    "redirect to ConfirmPostcodeCgt for UK based clients" in {
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription("GB")).toString())
      journeyState.set(IdentifyClient(ClientType.Personal, Service.CapitalGains), Nil)

      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/identify-cgt-client")

      val result =
        controller.submitIdentifyCgtClient(authorisedAsValidAgent(request.withFormUrlEncodedBody("cgtRef" -> s"${cgtRef.value}"), arn.value))

      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showPostcodeCgt.url
    }

    "redirect to ConfirmCountryCodeCgt for UK based clients" in {
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription("FR")).toString())
      journeyState.set(IdentifyClient(ClientType.Personal, Service.CapitalGains), Nil)

      val request = FakeRequest("POST", "/agents/cancel-authorisation/identify-cgt-client")

      val result =
        controller.submitIdentifyCgtClient(authorisedAsValidAgent(request.withFormUrlEncodedBody("cgtRef" -> s"${cgtRef.value}"), arn.value))

      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showCountryCodeCgt.url
    }

    "redirect to /agents/not-matched when cgtRef passed in does not match to any cgt client" in {
      givenGetCgtSubscriptionReturns(cgtRef, 404, cgtNotFoundJson)

      journeyState.set(IdentifyClient(ClientType.Personal, Service.CapitalGains), Nil)

      val request = FakeRequest("POST", "/agents/cancel-authorisation/identify-cgt-client")

      val result = controller.submitIdentifyCgtClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("cgtRef" -> cgtRef.value),
          arn.value
        )
      )

      status(result) shouldBe 303
      Helpers.redirectLocation(result)(timeout) shouldBe Some(routes.AgentLedDeauthJourneyController.showKnownFactNotMatched.url)

    }
  }

  "GET /agents/cancel-authorisation/client-postcode" should {

    val request = FakeRequest("GET", "/agents/cancel-authorisation/client-postcode")

    "display the page as expected" in {

      journeyState.set(ConfirmPostcodeCgt(ClientType.Personal, cgtRef, Some("BN13 1FN"), "firstName lastName"), Nil)

      val result = controller.showPostcodeCgt()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "confirm-postcode-cgt.header",
        "confirm-postcode-cgt.p1",
        "confirm-postcode-cgt.hint",
        "continue.button"
      )
    }
  }

  "POST /agents/cancel-authorisation/client-postcode" should {

    val request = FakeRequest("POST", "/agents/cancel-authorisation/client-postcode")

    "redirect to /confirm-client if postcode matches for a UK client" in {

      journeyState.set(ConfirmPostcodeCgt(ClientType.Personal, cgtRef, Some("BN13 1FN"), "firstName lastName"), Nil)

      val result = controller.submitConfirmCgtPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody("postcode" -> "BN13 1FN"), arn.value))

      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout) shouldBe Some(routes.AgentLedDeauthJourneyController.showConfirmClient.url)
    }

    "redirect to /not-matched if postcode does not match for a UK client" in {

      journeyState.set(ConfirmPostcodeCgt(ClientType.Personal, cgtRef, Some("BN13 1FN"), "firstName lastName"), Nil)

      val result = controller.submitConfirmCgtPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody("postcode" -> "BN13 1XX"), arn.value))

      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout) shouldBe Some(routes.AgentLedDeauthJourneyController.showKnownFactNotMatched.url)
    }
  }

  "GET /agents/cancel-authorisation/client-country" should {

    val request = FakeRequest("GET", "/agents/cancel-authorisation/client-country")

    "display the page as expected" in {

      journeyState.set(ConfirmCountryCodeCgt(ClientType.Personal, cgtRef, "FR", "firstName lastName"), Nil)

      val result = controller.showCountryCodeCgt()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "confirm-countryCode-cgt.header",
        "confirm-countryCode-cgt.p1",
        "confirm-countryCode-cgt.hint",
        "continue.button"
      )
    }
  }

  "POST /agents/cancel-authorisation/client-country" should {

    val request = FakeRequest("POST", "/agents/cancel-authorisation/client-country")

    "redirect to /confirm-client if country code matches for a non UK client" in {

      journeyState.set(ConfirmCountryCodeCgt(ClientType.Personal, cgtRef, "FR", "firstName lastName"), Nil)

      val result = controller.submitConfirmCgtCountryCode(authorisedAsValidAgent(request.withFormUrlEncodedBody("countryCode" -> "FR"), arn.value))

      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout) shouldBe Some(routes.AgentLedDeauthJourneyController.showConfirmClient.url)
    }

    "redirect to /not-matched if country code does not match for a non UK client" in {

      journeyState.set(ConfirmCountryCodeCgt(ClientType.Personal, cgtRef, "FR", "firstName lastName"), Nil)

      val result = controller.submitConfirmCgtCountryCode(authorisedAsValidAgent(request.withFormUrlEncodedBody("countryCode" -> "IN"), arn.value))

      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout) shouldBe Some(routes.AgentLedDeauthJourneyController.showKnownFactNotMatched.url)
    }
  }

  "GET /agents/cancel-authorisation/confirm-client" should {
    "display the confirm client page for ITSA" in {
      journeyState.set(
        ConfirmClient(ClientType.Personal, Service.MtdIt, Some("Barry Block"), validNino),
        List(IdentifyClient(ClientType.Personal, Service.MtdIt))
      )
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/confirm-client")
      val result = controller.showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        "Is this the client you want to cancel your authorisation for?",
        "Is Barry Block the client you want to cancel your authorisation for?",
        htmlEscapedMessage("cancel-authorisation.confirm-client.yes")
      )
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/identify-client")
    }
    "display the confirm client page for IRV" in {
      journeyState
        .set(
          ConfirmClient(ClientType.Personal, Service.PersonalIncomeRecord, Some("Barry Block"), validNino),
          List(IdentifyClient(ClientType.Personal, Service.PersonalIncomeRecord))
        )
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/confirm-client")
      val result = controller.showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        "Is this the client you want to cancel your authorisation for?",
        "Is Barry Block the client you want to cancel your authorisation for?",
        htmlEscapedMessage("cancel-authorisation.confirm-client.yes")
      )
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/identify-client")
    }
    "display the confirm client page for personal VAT" in {
      journeyState
        .set(ConfirmClient(ClientType.Personal, Service.Vat, Some("Barry Block"), validVrn), List(IdentifyClient(ClientType.Personal, Service.Vat)))
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/confirm-client")
      val result = controller.showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        "Is this the client you want to cancel your authorisation for?",
        "Is Barry Block the client you want to cancel your authorisation for?",
        htmlEscapedMessage("cancel-authorisation.confirm-client.yes")
      )
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/identify-client")
    }
    "display the confirm client page for business VAT" in {
      journeyState.set(
        ConfirmClient(ClientType.Business, Service.Vat, Some("Barry Block"), validVrn),
        List(IdentifyClient(ClientType.Business, Service.Vat))
      )
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/confirm-client")
      val result = controller.showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        "Is this the client you want to cancel your authorisation for?",
        "Is Barry Block the client you want to cancel your authorisation for?",
        htmlEscapedMessage("cancel-authorisation.confirm-client.yes")
      )
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/identify-client")
    }

    "display the confirm client page for Trust - UTR" in {
      journeyState.set(
        ConfirmClient(ClientType.Trust, Service.Trust, Some("some-trust"), validUtr),
        List(IdentifyClient(ClientType.Trust, Service.Trust))
      )
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/confirm-client")
      val result = controller.showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        "Is this the client you want to cancel your authorisation for?",
        "Is some-trust the client you want to cancel your authorisation for?",
        htmlEscapedMessage("cancel-authorisation.confirm-client.yes")
      )
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/identify-client")
    }

    "display the confirm client page for Trust - URN" in {
      journeyState.set(
        ConfirmClient(ClientType.Trust, Service.TrustNT, Some("some-trust"), validUrn),
        List(IdentifyClient(ClientType.Trust, Service.TrustNT))
      )
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/confirm-client")
      val result = controller.showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        "Is this the client you want to cancel your authorisation for?",
        "Is some-trust the client you want to cancel your authorisation for?",
        htmlEscapedMessage("cancel-authorisation.confirm-client.yes")
      )
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/identify-client")
    }

    "display the confirm client page for CGT" in {
      journeyState.set(
        ConfirmClient(ClientType.Personal, Service.CapitalGains, Some("some-cgt-client"), cgtRef),
        List(IdentifyClient(ClientType.Personal, Service.CapitalGains))
      )
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/confirm-client")
      val result = controller.showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        "Is this the client you want to cancel your authorisation for?",
        "Is some-cgt-client the client you want to cancel your authorisation for?",
        htmlEscapedMessage("cancel-authorisation.confirm-client.yes")
      )
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/identify-client")
    }

  }

  "POST /agents/cancel-authorisation/confirm-client" should {
    "redirect to confirm cancel when YES is selected" in {
      journeyState.set(ConfirmClient(ClientType.Personal, Service.MtdIt, Some("Sufjan Stevens"), nino), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-client")

      givenCheckRelationshipItsaWithStatus(arn, nino, 200)

      val result =
        controller.submitConfirmClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("accepted" -> "true"),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showConfirmCancel.url
    }
    "redirect to confirm cancel when YES is selected for alt-itsa" in {
      journeyState.set(ConfirmClient(ClientType.Personal, Service.MtdIt, Some("Sufjan Stevens"), nino), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-client")

      givenCheckRelationshipItsaWithStatus(arn, nino, 404)
      givenPartialAuthorisationExists(arn, nino.value)

      val result =
        controller.submitConfirmClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("accepted" -> "true"),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showConfirmCancel.url
    }
    "redirect to not authorised when there are is no active relationship or partial auth to de-authorise" in {
      journeyState.set(ConfirmClient(ClientType.Personal, Service.MtdIt, Some("Sufjan Stevens"), nino), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-client")

      givenCheckRelationshipItsaWithStatus(arn, nino, 404)
      givenPartialAuthNotExists(arn, nino.value)

      val result =
        controller.submitConfirmClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("accepted" -> "true"),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showNotAuthorised.url
    }

    "redirect to confirm cancel when YES is selected for trust - UTR" in {
      journeyState.set(ConfirmClient(ClientType.Trust, Service.Trust, Some("some-trust"), validUtr), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-client")

      givenCheckRelationshipTrustWithStatus(arn, validUtr, 200)

      val result =
        controller.submitConfirmClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("accepted" -> "true"),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showConfirmCancel.url
    }

    "redirect to confirm cancel when YES is selected for trust - URN" in {
      journeyState.set(ConfirmClient(ClientType.Trust, Service.TrustNT, Some("some-trust"), validUrn), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-client")

      givenCheckRelationshipTrustWithStatus(arn, validUrn, 200)

      val result =
        controller.submitConfirmClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("accepted" -> "true"),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showConfirmCancel.url
    }

    "redirect to not authorised when there are is no active relationship to de-authorise for trusts - UTR" in {
      journeyState.set(ConfirmClient(ClientType.Trust, Service.Trust, Some("some-trust"), validUtr), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-client")

      givenCheckRelationshipTrustWithStatus(arn, validUtr, 404)

      val result =
        controller.submitConfirmClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("accepted" -> "true"),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showNotAuthorised.url
    }

    "redirect to not authorised when there are is no active relationship to de-authorise for trusts - URN" in {
      journeyState.set(ConfirmClient(ClientType.Trust, Service.TrustNT, Some("some-trust"), validUrn), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-client")

      givenCheckRelationshipTrustWithStatus(arn, validUtr, 404)

      val result =
        controller.submitConfirmClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("accepted" -> "true"),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showNotAuthorised.url
    }
  }
  "GET /agents/cancel-authorisation/confirm-cancel" should {
    "display the confirm cancel page with additional panel content for ITSA" in {
      journeyState.set(
        ConfirmCancel(Service.MtdIt, Some("Barry Block"), validNino.value),
        List(ConfirmClient(ClientType.Personal, Service.MtdIt, Some("Barry Block"), validNino))
      )
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/confirm-cancel")
      val result = controller.showConfirmCancel(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        htmlEscapedMessage("cancel-authorisation.confirm-cancel.header"),
        htmlEscapedMessage("cancel-authorisation.confirm-cancel.p1.HMRC-MTD-IT", "Barry Block"),
        htmlEscapedMessage("cancel-authorisation.confirm-cancel.itsa-panel")
      )
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/confirm-client")
    }

    "display the confirm cancel page without additional panel content when service is not ITSA" in {
      journeyState.set(
        ConfirmCancel(Service.Vat, Some("Barry Block"), validVrn.value),
        List(ConfirmClient(ClientType.Personal, Service.Vat, Some("Barry Block"), validVrn))
      )
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/confirm-cancel")
      val result = controller.showConfirmCancel(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        htmlEscapedMessage("cancel-authorisation.confirm-cancel.header"),
        htmlEscapedMessage("cancel-authorisation.confirm-cancel.p1.HMRC-MTD-VAT", "Barry Block")
      )
      checkHtmlResultWithoutBodyMsgs(result.futureValue, "cancel-authorisation.confirm-cancel.itsa-panel")

      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/confirm-client")
    }

    "display the confirm cancel page for alt-itsa" in {
      journeyState.set(
        ConfirmCancel(Service.MtdIt, Some("Barry Block"), validNino.value, isPartialAuth = true),
        List(ConfirmClient(ClientType.Personal, Service.MtdIt, Some("Barry Block"), validNino))
      )
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/confirm-cancel")
      val result = controller.showConfirmCancel(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        htmlEscapedMessage("cancel-authorisation.confirm-cancel.header"),
        htmlEscapedMessage("cancel-authorisation.confirm-cancel.p1.HMRC-MTD-IT", "Barry Block"),
        htmlEscapedMessage("cancel-authorisation.confirm-cancel.itsa-panel")
      )
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/confirm-client")
    }
  }
  "POST /agents/cancel-authorisation/confirm-cancel" should {
    "redirect to the authorisation cancelled page" in {
      journeyState.set(ConfirmCancel(Service.MtdIt, Some("Sufjan Stevens"), nino.value), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-cancel")

      givenCancelledAuthorisationItsa(arn, nino, 204)
      givenGetAgencyNameClientStub(arn)
      givenASingleAcceptedInvitation(arn, nino.value, Service.MtdIt, "NI", LocalDateTime.now())

      val result =
        controller.submitConfirmCancel(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("accepted" -> "true"),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showAuthorisationCancelled.url
    }

    "redirect to the authorisation cancelled page for alt-itsa" in {
      journeyState.set(ConfirmCancel(Service.MtdIt, Some("Sufjan Stevens"), nino.value, isPartialAuth = true), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-cancel")

      givenGetAgencyNameClientStub(arn)
      givenASingleAcceptedInvitation(arn, nino.value, Service.MtdIt, "NI", LocalDateTime.now(), isPartialAuth = true)
      givenSetRelationshipEndedReturns(arn, nino.value, Service.MtdIt, 204)

      val result =
        controller.submitConfirmCancel(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("accepted" -> "true"),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showAuthorisationCancelled.url
    }

    "redirect to the authorisation cancelled page for trusts - UTR" in {
      journeyState.set(ConfirmCancel(Service.Trust, Some("Sufjan Stevens"), validUtr.value), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-cancel")

      givenCancelledAuthorisationTrust(arn, validUtr, 204)
      givenGetAgencyNameClientStub(arn)
      givenASingleAcceptedInvitation(arn, validUtr.value, Service.Trust, "UTR", LocalDateTime.now())

      val result =
        controller.submitConfirmCancel(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("accepted" -> "true"),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showAuthorisationCancelled.url
    }

    "redirect to the authorisation cancelled page for trusts - URN" in {
      journeyState.set(ConfirmCancel(Service.TrustNT, Some("Sufjan Stevens"), validUrn.value), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-cancel")

      givenCancelledAuthorisationTrust(arn, validUrn, 204)
      givenGetAgencyNameClientStub(arn)
      givenASingleAcceptedInvitation(arn, validUrn.value, Service.TrustNT, "URN", LocalDateTime.now())

      val result =
        controller.submitConfirmCancel(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("accepted" -> "true"),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showAuthorisationCancelled.url
    }

    "redirect to the authorisation cancelled page when no Accepted invitation found - UTR" in {
      journeyState.set(ConfirmCancel(Service.Trust, Some("Sufjan Stevens"), validUtr.value), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-cancel")

      givenCancelledAuthorisationTrust(arn, validUtr, 204)
      givenGetAgencyNameClientStub(arn)
      givenNoAcceptedInvitationFound(arn, validUtr.value, Service.Trust)

      val result =
        controller.submitConfirmCancel(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("accepted" -> "true"),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showAuthorisationCancelled.url
    }

    "redirect to the authorisation cancelled page when no Accepted invitation found - URN" in {
      journeyState.set(ConfirmCancel(Service.TrustNT, Some("Sufjan Stevens"), validUrn.value), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-cancel")

      givenCancelledAuthorisationTrust(arn, validUrn, 204)
      givenGetAgencyNameClientStub(arn)
      givenNoAcceptedInvitationFound(arn, validUrn.value, Service.TrustNT)

      val result =
        controller.submitConfirmCancel(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("accepted" -> "true"),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showAuthorisationCancelled.url
    }

    "redirect to response failed page when the relationship termination fails" in {
      journeyState.set(ConfirmCancel(Service.MtdIt, Some("Sufjan Stevens"), nino.value), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-cancel")

      givenCancelledAuthorisationItsa(arn, nino, 404)
      givenGetAgencyNameClientStub(arn)

      val result =
        controller.submitConfirmCancel(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("accepted" -> "true"),
            arn.value
          )
        )
      status(result) shouldBe 303

      Helpers.redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showResponseFailed.url
    }
  }
  "GET /agents/cancel-authorisation/cancelled" should {
    "display the authorisation cancelled page" in {
      journeyState.set(AuthorisationCancelled(Service.MtdIt, Some("client man"), "Agent man"), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/cancelled")
      val result = controller.showAuthorisationCancelled(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        htmlEscapedMessage("cancel-authorisation.cancelled.header"),
        htmlEscapedMessage("cancel-authorisation.cancelled.p1.HMRC-MTD-IT", "Agent man", "client man")
      )
    }

    "display the extra 'check self assessment' lines when cancelling MTD for Income Tax" in {
      journeyState.set(AuthorisationCancelled(Service.MtdIt, Some("client man"), "Agent man"), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/cancelled")
      val result = controller.showAuthorisationCancelled(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        htmlEscapedMessage("cancel-authorisation.cancelled.header"),
        htmlEscapedMessage("cancel-authorisation.cancelled.p1.HMRC-MTD-IT", "Agent man", "client man"),
        htmlEscapedMessage("authorisation-cancelled.check-sa.subheader"),
        htmlEscapedMessage("authorisation-cancelled.check-sa.p1"),
        htmlEscapedMessage("authorisation-cancelled.check-sa.l1"),
        htmlEscapedMessage("authorisation-cancelled.check-sa.l3"),
        htmlEscapedMessage("cancel-authorisation.cancelled.print")
      )

      val htmlString = Helpers.contentAsString(result)
      val html = Jsoup.parse(htmlString)

      val signInLink = html.select("a[href='https://www.gov.uk/guidance/self-assessment-for-agents-online-service']")
      signInLink.text() shouldBe "sign in to your HMRC online services for agents account"
      signInLink.hasClass("govuk-link")

      val asaLink = html.select(s"a[href='http://localhost:$wireMockPort/agent-services-account/home']")
      asaLink.text() shouldBe "Return to agent services account"
      asaLink.hasClass("govuk-button")
      asaLink.attr("role") shouldBe "button"

    }

    "not display the extra 'check self assessment' lines when cancelling authorisations other than Income Tax" in {
      journeyState.set(AuthorisationCancelled(Service.Vat, Some("client man"), "Agent man"), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/cancelled")
      val result = controller.showAuthorisationCancelled(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithoutBodyMsgs(
        result.futureValue,
        "authorisation-cancelled.check-sa.subheader",
        "authorisation-cancelled.check-sa.p1",
        "authorisation-cancelled.check-sa.l1",
        "authorisation-cancelled.check-sa.l2",
        "authorisation-cancelled.check-sa.l3"
      )
    }
  }
  "GET /agents/cancel-authorisation/client-not-found" should {
    "display the known facts dont match page" in {
      journeyState.set(KnownFactNotMatched, Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/client-not-found")
      val result = controller.showKnownFactNotMatched(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result.futureValue,
        htmlEscapedMessage("cancel-authorisation.not-matched.header"),
        htmlEscapedMessage("cancel-authorisation.not-matched.description")
      )
    }
  }
  "GET /agents/cancel-authorisation/not-signed-up" should {
    "display the not enrolled page" in {
      journeyState.set(NotSignedUp(Service.MtdIt), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/not-signed-up")
      val result = controller.showNotSignedUp(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200

      val html = Jsoup.parse(Helpers.contentAsString(result))
      html.title() shouldBe "This client has not signed up to Making Tax Digital for Income Tax - Cancel a client’s authorisation - GOV.UK"
      html.select(Css.H1).text() shouldBe "This client has not signed up to Making Tax Digital for Income Tax"

      val paragraphs = html.select(Css.paragraphs)
      paragraphs.get(0).text() shouldBe "They cannot authorise you for this service until they have signed up."
      paragraphs
        .get(1)
        .text() startsWith "If you copied an existing Self Assessment authorisation for this client to your agent services account, you can "
      paragraphs.get(1).select("a").text() shouldBe "sign them up to Making Tax Digital pilot for Income Tax (opens in a new tab)."
      paragraphs.get(1).select("a").attr("href") shouldBe "https://www.gov.uk/guidance/agents-use-software-to-send-income-tax-updates"

      paragraphs.get(2).text() startsWith "Find out how to "
      paragraphs.get(2).select("a").text() shouldBe "copy across an existing authorisation"
      paragraphs.get(2).select("a").attr("href") shouldBe "http://localhost:9438/agent-mapping/start"
      html.select("a#button-link").text() shouldBe "Start new request"
      html.select("a#button-link").hasClass("govuk-button")
      html.select("a#button-link").attr("href") shouldBe "/invitations/agents/cancel-authorisation"

    }
  }
  "GET /agents/cancel-authorisation/not-authorised" should {
    "display the not authorised page" in {
      journeyState.set(NotAuthorised(Service.MtdIt), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/not-authorised")
      val result = controller.showNotAuthorised(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result.futureValue, htmlEscapedMessage("not-authorised.h1"), htmlEscapedMessage("not-authorised.HMRC-MTD-IT.p"))
    }
  }
  "GET /agents/cancel-authorisation/response-failed" should {
    "display the response failed page" in {
      journeyState.set(ResponseFailed(Service.MtdIt, Some("Peter rabbit"), "AB123456A"), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/response-failed")
      val result = controller.showResponseFailed(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result.futureValue,
        "cancel-authorisation.response-failed.header",
        "cancel-authorisation.response-failed.description"
      )
    }
  }

  "Trying to display any page" should {
    "redirect to ASAF 'account limited' page if the agent is suspended" in {
      val fakeRequest = authorisedAsValidAgent(FakeRequest("GET", ""), arn.value, suspended = true)
      givenGetSuspensionDetailsClientStub(arn, SuspensionDetails(suspensionStatus = true, Some(Set("ALL"))))
      journeyState.set(SelectClientType, Nil)

      val result = controller.showClientType(fakeRequest)
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(appConfig.accountLimitedUrl)
    }
  }

}
