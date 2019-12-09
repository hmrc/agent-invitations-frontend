package uk.gov.hmrc.agentinvitationsfrontend.controllers

import org.joda.time.LocalDate
import org.scalatest.BeforeAndAfter
import play.api.Application
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers.redirectLocation
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.duration._

class AgentLedDeauthJourneyControllerISpec extends BaseISpec with StateAndBreadcrumbsMatchers with BeforeAndAfter {

  val enabledServices: Set[String] = Set(HMRCMTDIT, HMRCPIR, HMRCMTDVAT, HMRCCGTPD)

  implicit val hc: HeaderCarrier = HeaderCarrier()
  override implicit lazy val app: Application = appBuilder
    .overrides(new TestAgentLedDeauthJourneyModule)
    .build()

  val timeout = 2.seconds

  lazy val journeyState = app.injector.instanceOf[TestAgentLedDeauthJourneyService]

  val controller = app.injector.instanceOf[AgentLedDeauthJourneyController]

  before {
    journeyState.clear
  }

  "GET /agents/cancel-authorisation" should {
    "redirect to the client type page when there is no current state" in {
      journeyState.clear(hc, ec)
      val request = FakeRequest("GET", "/agents/cancel-authorisation/client-type")
      val root = controller.agentLedDeauthRoot()
      val result = root(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 303
      redirectLocation(result)(timeout) shouldBe Some(routes.AgentLedDeauthJourneyController.showClientType().url)
    }
  }

  "GET /agents/cancel-authorisation/client-type" should {
    "display the client type page" when {
      "there is a current state of SelectClientType and no breadcrumbs" in {
        journeyState.set(SelectClientType, Nil)
        val request = FakeRequest("GET", "/agents/cancel-authorisation/client-type")
        val selectClientType = controller.showClientType()

        val result = selectClientType(authorisedAsValidAgent(request, arn.value))
        status(result) shouldBe 200
        checkHtmlResultWithBodyText(
          result,
          hasMessage(
            "generic.title",
            htmlEscapedMessage("cancel-authorisation.client-type.header"),
            htmlEscapedMessage("title.suffix.agents.de-auth")),
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

  "POST /agents/cancel-authorisation/client-type" should {
    "redirect to select service page when client type is personal" in {
      List("personal", "business", "trust").foreach { clientType =>
        journeyState.set(SelectClientType, Nil)
        val request = FakeRequest("POST", "/agents/cancel-authorisation/client-type")

        val result =
          controller.submitClientType(
            authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> clientType), arn.value))
        status(result) shouldBe 303

        redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showSelectService().url
      }

    }
  }

  "GET /agents/cancel-authorisation/select-service" should {
    "show the select service page for personal services" in {
      journeyState.set(SelectServicePersonal(enabledServices), Nil)
      val request = FakeRequest("GET", "/agents/cancel-authorisation/select-service")
      val result = controller.showSelectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        hasMessage(
          "generic.title",
          htmlEscapedMessage("cancel-authorisation.select-service.header"),
          htmlEscapedMessage("title.suffix.agents.de-auth")),
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
          htmlEscapedMessage("title.suffix.agents.de-auth")),
        htmlEscapedMessage("cancel-authorisation.business-select-service.header"),
        hasMessage("select-service.yes"),
        hasMessage("select-service.no")
      )
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/client-type")
    }

    "show the select service page for trust service" in {
      journeyState.set(SelectServiceTrust(Set(TRUST, HMRCCGTPD)), Nil)
      val request = FakeRequest("GET", "/agents/cancel-authorisation/select-service")
      val result = controller.showSelectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        hasMessage(
          "generic.title",
          htmlEscapedMessage("cancel-authorisation.trust-select-service.header"),
          htmlEscapedMessage("title.suffix.agents.de-auth")),
        htmlEscapedMessage("cancel-authorisation.trust-select-service.header"),
        hasMessage("cancel-authorisation.select-service.trust"),
        hasMessage("cancel-authorisation.select-service.cgt")
      )
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/client-type")
    }
  }

  "POST /agents/cancel-authorisation/select-personal-service" should {
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

  "POST /agents/cancel-authorisation/select-business-service" should {
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

  "POST /agents/cancel-authorisation/select-trust-service" should {
    "redirect to identify trust client when trust is selected" in {
      journeyState.set(SelectServiceTrust(Set(TRUST, HMRCCGTPD)), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/select-trust-service")

      val result =
        controller.submitTrustService(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> "HMRC-TERS-ORG"), arn.value))
      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showIdentifyClient().url
    }

    "redirect to identify cgt client when cgt is selected" in {
      journeyState.set(SelectServiceTrust(Set(TRUST, HMRCCGTPD)), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/select-trust-service")

      val result =
        controller.submitTrustService(
          authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> "HMRC-CGT-PD"), arn.value))
      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showIdentifyClient().url
    }
  }

  "GET /agents/cancel-authorisation/identify-client" should {
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

    "display the identify client page for personal cgt service" in {
      journeyState.set(IdentifyClientPersonal(HMRCCGTPD), Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/identify-client")
      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("identify-cgt-client.header"),
        htmlEscapedMessage("identify-cgt-client.hint"))
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

    "display the identify client page for trust service" in {
      journeyState.set(IdentifyClientTrust, Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/identify-client")
      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "identify-trust-client.header",
       "identify-trust-client.p1")
    }

    "display the identify client page for cgt trust service" in {
      journeyState.set(IdentifyClientCgt, Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/identify-client")
      val result = controller.showIdentifyClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "identify-cgt-client.header",
        "identify-cgt-client.p1",
        "identify-cgt-client.p2")
    }
  }
  "GET /agents/identify-itsa-client" should {
    val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/identify-itsa-client")
    "redirect to the identify client page" in {
      journeyState.set(IdentifyClientPersonal(HMRCMTDIT), Nil)

      val result = controller.identifyClientRedirect()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 303
      redirectLocation(result)(timeout) shouldBe Some(routes.AgentLedDeauthJourneyController.showIdentifyClient().url)
    }
  }
  "POST /agents/cancel-authorisation/identify-itsa-client" should {
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
  "POST /agents/cancel-authorisation/identify-irv-client" should {
    "redirect to confirm cancel" in {
      journeyState.set(IdentifyClientPersonal(HMRCPIR), Nil)
      val request = FakeRequest("POST", "agents/cancel-authorisation/identify-irv-client")

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
  "POST /agents/cancel-authorisation/identify-vat-client" should {
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
  }

  "POST /agents/cancel-authorisation/identify-trust-client" should {

    "redirect to confirm client for trust" in {
      givenTrustClientReturns(validUtr, 200, Json.toJson(trustResponse).toString())
      journeyState.set(IdentifyClientTrust, Nil)

      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/identify-trust-client")

      val result =
        controller.submitIdentifyTrustClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "utr"       -> s"${validUtr.value}"), arn.value))

      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showConfirmClient().url
    }

    "redirect to /not-found for trust if trust details are not found for given utr" in {
      givenTrustClientReturns(validUtr, 200, trustNotFoundJson)
      journeyState.set(IdentifyClientTrust, Nil)

      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/identify-trust-client")

      val result =
        controller.submitIdentifyTrustClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "utr"       -> s"${validUtr.value}"), arn.value))

      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showKnownFactNotMatched().url
    }
  }

  "POST /agents/cancel-authorisation/identify-cgt-client" should {

    "redirect to ConfirmPostcodeCgt for UK based clients" in {
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription("GB")).toString())
      journeyState.set(IdentifyClientCgt, Nil)

      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/identify-cgt-client")

      val result =
        controller.submitIdentifyCgtClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "cgtRef"       -> s"${cgtRef.value}"), arn.value))

      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showPostcodeCgt().url
    }

    "redirect to ConfirmCountryCodeCgt for UK based clients" in {
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription("FR")).toString())
      journeyState.set(IdentifyClientCgt, Nil)

      val request = FakeRequest("POST", "/agents/cancel-authorisation/identify-cgt-client")

      val result =
        controller.submitIdentifyCgtClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(
              "cgtRef"       -> s"${cgtRef.value}"), arn.value))

      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showCountryCodeCgt().url
    }

    "redirect to /agents/not-matched when cgtRef passed in does not match to any cgt client" in {
      givenGetCgtSubscriptionReturns(cgtRef, 404, cgtNotFoundJson)

      journeyState.set(IdentifyClientCgt, Nil)

      val request = FakeRequest("POST", "/agents/cancel-authorisation/identify-cgt-client")

      val result = controller.submitIdentifyCgtClient(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody("cgtRef" -> cgtRef.value),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result)(timeout) shouldBe Some(routes.AgentLedDeauthJourneyController.showKnownFactNotMatched().url)

    }
  }


  "GET /agents/cancel-authorisation/client-postcode" should {

    val request = FakeRequest("GET", "/agents/cancel-authorisation/client-postcode")

    "display the page as expected" in {

      journeyState.set(ConfirmPostcodeCgt(cgtRef, Some("BN13 1FN"), "firstName lastName"), Nil)

      val result = controller.showPostcodeCgt()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "confirm-postcode-cgt.header",
        "confirm-postcode-cgt.p1",
        "confirm-postcode-cgt.label",
        "confirm-postcode-cgt.hint",
        "continue.button"
      )
    }
  }

  "POST /agents/cancel-authorisation/client-postcode" should {

    val request = FakeRequest("POST", "/agents/cancel-authorisation/client-postcode")

    "redirect to /confirm-client if postcode matches for a UK client" in {

      journeyState.set(ConfirmPostcodeCgt(cgtRef, Some("BN13 1FN"), "firstName lastName"), Nil)

      val result = controller.submitConfirmCgtPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody("postcode" -> "BN13 1FN"), arn.value))

      status(result) shouldBe 303

      redirectLocation(result)(timeout) shouldBe Some(routes.AgentLedDeauthJourneyController.showConfirmClient().url)
    }

    "redirect to /not-matched if postcode does not match for a UK client" in {

      journeyState.set(ConfirmPostcodeCgt(cgtRef, Some("BN13 1FN"), "firstName lastName"),Nil)

      val result = controller.submitConfirmCgtPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody("postcode" -> "BN13 1XX"), arn.value))

      status(result) shouldBe 303

      redirectLocation(result)(timeout) shouldBe Some(routes.AgentLedDeauthJourneyController.showKnownFactNotMatched().url)
    }
  }

  "GET /agents/cancel-authorisation/client-country" should {

    val request = FakeRequest("GET", "/agents/cancel-authorisation/client-country")

    "display the page as expected" in {

      journeyState.set(ConfirmCountryCodeCgt(cgtRef, "FR", "firstName lastName"),Nil)

      val result = controller.showCountryCodeCgt()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "confirm-countryCode-cgt.header",
        "confirm-countryCode-cgt.p1",
        "confirm-countryCode-cgt.label",
        "confirm-countryCode-cgt.hint",
        "continue.button"
      )
    }
  }

  "POST /agents/cancel-authorisation/client-country" should {

    val request = FakeRequest("POST", "/agents/cancel-authorisation/client-country")

    "redirect to /confirm-client if country code matches for a non UK client" in {

      journeyState.set(ConfirmCountryCodeCgt(cgtRef, "FR", "firstName lastName"),Nil)

      val result = controller.submitConfirmCgtCountryCode(authorisedAsValidAgent(request.withFormUrlEncodedBody("countryCode" -> "FR"), arn.value))

      status(result) shouldBe 303

      redirectLocation(result)(timeout) shouldBe Some(routes.AgentLedDeauthJourneyController.showConfirmClient().url)
    }

    "redirect to /not-matched if country code does not match for a non UK client" in {

      journeyState.set(ConfirmCountryCodeCgt(cgtRef, "FR", "firstName lastName"),Nil)

      val result = controller.submitConfirmCgtCountryCode(authorisedAsValidAgent(request.withFormUrlEncodedBody("countryCode" -> "IN"), arn.value))

      status(result) shouldBe 303

      redirectLocation(result)(timeout) shouldBe Some(routes.AgentLedDeauthJourneyController.showKnownFactNotMatched().url)
    }
  }

  "GET /agents/cancel-authorisation/confirm-client" should {
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
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/identify-client")
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
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/identify-client")
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
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/identify-client")
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
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/identify-client")
    }

    "display the confirm client page for Trust" in {
      journeyState.set(ConfirmClientTrust("some-trust", validUtr), List(IdentifyClientTrust))
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/confirm-client")
      val result = controller.showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "some-trust",
        htmlEscapedMessage("cancel-authorisation.confirm-client.header"),
        htmlEscapedMessage("cancel-authorisation.confirm-client.yes")
      )
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/identify-client")
    }

    "display the confirm client page for CGT" in {
      journeyState.set(ConfirmClientCgt(cgtRef, "some-cgt-client"), List(IdentifyClientCgt))
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/confirm-client")
      val result = controller.showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "some-cgt-client",
        cgtRef.value,
        htmlEscapedMessage("cancel-authorisation.confirm-client.header"),
        htmlEscapedMessage("cancel-authorisation.confirm-client.yes")
      )
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/identify-client")
    }

  }

  "POST /agents/cancel-authorisation/confirm-client" should {
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

    "redirect to confirm cancel when YES is selected for trust" in {
      journeyState.set(ConfirmClientTrust("some-trust", validUtr), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-client")

      givenCheckRelationshipTrustWithStatus(arn, validUtr.value, 200)

      val result =
        controller.submitConfirmClient(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody("accepted" -> "true"),
            arn.value
          ))
      status(result) shouldBe 303

      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeauthJourneyController.showConfirmCancel().url
    }

    "redirect to not authorised when there are is no active relationship to de-authorise for trusts" in {
      journeyState.set(ConfirmClientTrust("some-trust", validUtr), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-client")

      givenCheckRelationshipTrustWithStatus(arn, validUtr.value, 404)

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
  "GET /agents/cancel-authorisation/confirm-cancel" should {
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
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/confirm-client")
    }
  }
  "POST /agents/cancel-authorisation/confirm-cancel" should {
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

    "redirect to the authorisation cancelled page for trusts" in {
      journeyState.set(ConfirmCancel(TRUST, Some("Sufjan Stevens"), validUtr.value), Nil)
      val request = FakeRequest("POST", "fsm/agents/cancel-authorisation/confirm-cancel")

      givenCancelledAuthorisationTrust(arn,validUtr, 204)
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
  "GET /agents/cancel-authorisation/cancelled" should {
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
  "GET /agents/cancel-authorisation/client-not-found" should {
    "display the known facts dont match page" in {
      journeyState.set(KnownFactNotMatched, Nil)
      val request = FakeRequest("GET", "fsm/agents/cancel-authorisation/client-not-found")
      val result = controller.showKnownFactNotMatched(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("cancel-authorisation.not-matched.header"),
        htmlEscapedMessage("cancel-authorisation.not-matched.description"))
    }
  }
  "GET /agents/cancel-authorisation/not-signed-up" should {
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
  "GET /agents/cancel-authorisation/not-authorised" should {
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
  "GET /agents/cancel-authorisation/response-failed" should {
    "display the response failed page" in {
      journeyState.set(ResponseFailed(HMRCMTDIT, Some("Peter rabbit"), "AB123456A"), Nil)
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
