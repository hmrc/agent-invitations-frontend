package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import java.util.UUID

import akka.util.Timeout
import org.joda.time.LocalDate
import play.api.test.FakeRequest
import play.api.test.Helpers.redirectLocation
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.agentConfirmationForm
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentSession, Confirmation, Services}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.duration._

class AgentLedDeAuthControllerISpec extends BaseISpec with AuthBehaviours {

  lazy val controller: AgentLedDeAuthController = app.injector.instanceOf[AgentLedDeAuthController]
  implicit val timeout: Timeout = 2.seconds

  "GET /cancel-authorisation/client-type" should {

    "return 200 with expected page content" in {
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
  }

  "POST /cancel-authorisation/client-type" should {

    "return redirect to /select-service page after storing client_type in the cache" in {
      val request = FakeRequest("POST", "/agents/cancel-authorisation/client-type")
      val submitClientType = controller.submitClientType()

      val sessionId = UUID.randomUUID().toString
      implicit val hc: HeaderCarrier = headerCarrier(sessionId)

      val result = submitClientType(authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> "personal"), arn.value, sessionId))
      status(result) shouldBe 303
      val timeout = 2.seconds
      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeAuthController.showSelectService().url

      await(sessionStore.fetch).get shouldBe AgentSession(Some(personal), clientTypeForInvitationSent = Some(personal))
    }
  }

  "GET /cancel-authorisation/select-service" should {
    "return 200 with expected page content when the clientType is personal" in {
      val sessionId = UUID.randomUUID().toString
      implicit val hc: HeaderCarrier = headerCarrier(sessionId)
      await(sessionStore.save(AgentSession(Some(personal))))

      val request = FakeRequest("GET", "/agents/cancel-authorisation/select-service")
      val showSelectService = controller.showSelectService()

      val result = showSelectService(authorisedAsValidAgent(request, arn.value, sessionId))
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
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/client-type")
    }

    "return 200 with expected page content when the clientType is business" in {
      val sessionId = UUID.randomUUID().toString
      implicit val hc: HeaderCarrier = headerCarrier(sessionId)
      await(sessionStore.save(AgentSession(Some(business))))

      val request = FakeRequest("GET", "/agents/cancel-authorisation/select-service")
      val showSelectService = controller.showSelectService()

      val result = showSelectService(authorisedAsValidAgent(request, arn.value, sessionId))
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
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/client-type")
    }
  }

  "POST /cancel-authorisation/select-service" should {

    "return redirect after storing service_type in the cache" in {
      val sessionId = UUID.randomUUID().toString
      implicit val hc: HeaderCarrier = headerCarrier(sessionId)
      await(sessionStore.save(AgentSession(Some(personal))))

      val request = FakeRequest("POST", "/agents/cancel-authorisation/select-personal-service")
      val submitSelectService = controller.submitSelectPersonalService()

      val result = submitSelectService(authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> "HMRC-MTD-IT"), arn.value, sessionId))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentLedDeAuthController.showIdentifyClient().url

      await(sessionStore.fetch).get.service shouldBe Some("HMRC-MTD-IT")
    }

    "handle the confirmation form if the client_type is business" in {
      val sessionId = UUID.randomUUID().toString
      implicit val hc: HeaderCarrier = headerCarrier(sessionId)
      await(sessionStore.save(AgentSession(Some(business))))

      val request = FakeRequest("POST", "/agents/cancel-authorisation/select-business-service")
      val submitSelectService = controller.submitSelectBusinessService()

      val result = submitSelectService(authorisedAsValidAgent(request.withFormUrlEncodedBody("accepted" -> "true"), arn.value, sessionId))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentLedDeAuthController.showIdentifyClient().url

      await(sessionStore.fetch).get.service shouldBe Some("HMRC-MTD-VAT")
    }

    "handle forms with invalid service_types" in {
      val sessionId = UUID.randomUUID().toString
      implicit val hc: HeaderCarrier = headerCarrier(sessionId)
      await(sessionStore.save(AgentSession(Some(personal))))

      val request = FakeRequest("POST", "/agents/cancel-authorisation/select-personal-service")
      val submitSelectService = controller.submitSelectPersonalService()

      val result = submitSelectService(authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> "HMRC-BLAH"), arn.value, sessionId))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "error.prefix",
          htmlEscapedMessage("cancel-authorisation.select-service.header"),
          htmlEscapedMessage("title.suffix.agents")),
        htmlEscapedMessage("cancel-authorisation.select-service.header"),
        hasMessage("cancel-authorisation.select-service.hint"),
        hasMessage("service.type.invalid")
      )
    }
  }

  "GET /agents/cancel-authorisation/identify-client" should {

    val request = FakeRequest("GET", "/agents/cancel-authorisation/identify-client")
    val showIdentifyClient = controller.showIdentifyClient()

    "display correct identify client page based on selected service" in {

      Services.supportedServices.foreach { service =>
        val sessionId = UUID.randomUUID().toString
        implicit val hc: HeaderCarrier = headerCarrier(sessionId)
        await(sessionStore.save(AgentSession(Some(personal), Some(service))))

        val result = showIdentifyClient(authorisedAsValidAgent(request, arn.value, sessionId))
        status(result) shouldBe 200

        if (service == HMRCPIR) {
          checkHtmlResultWithBodyText(
            result,
            htmlEscapedMessage("identify-client.nino.label"),
            htmlEscapedMessage("identify-client.irv-date-of-birth.label"))
        } else if (service == HMRCMTDIT) {
          checkHtmlResultWithBodyText(
            result,
            htmlEscapedMessage("identify-client.nino.label"),
            htmlEscapedMessage("identify-client.postcode.label"))
        } else if (service == HMRCMTDVAT) {
          checkHtmlResultWithBodyText(
            result,
            htmlEscapedMessage("identify-client.vrn.label"),
            htmlEscapedMessage("identify-client.vat-registration-date.label"))
        }

        checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/select-service")
      }
    }

    "redirect to /cancel-authorisation/select-service page if service in the cache is not a valid service" in {
      val sessionId = UUID.randomUUID().toString
      implicit val hc: HeaderCarrier = headerCarrier(sessionId)
      await(sessionStore.save(AgentSession(Some(personal), Some("blah service"))))

      val result = showIdentifyClient(authorisedAsValidAgent(request, arn.value, sessionId))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentLedDeAuthController.showSelectService().url
    }

    "redirect to /cancel-authorisation/client-type page if there is no cache found" in {
      val sessionId = UUID.randomUUID().toString
      implicit val hc: HeaderCarrier = headerCarrier(sessionId)
      await(sessionStore.save(AgentSession()))

      val result = showIdentifyClient(authorisedAsValidAgent(request, arn.value, sessionId))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentLedDeAuthController.showSelectService().url

    }
  }

  "POST /agents/cancel-authorisation/identify-client" when {
    val request = FakeRequest("POST", "/agents/cancel-authorisation/identify-client")
    val submitIdentifyClient = controller.submitIdentifyClient()

    "service is PERSONAL-INCOME-RECORD" should {
      "handle the form correctly and redirect" in {
        val sessionId = UUID.randomUUID().toString
        implicit val hc: HeaderCarrier = headerCarrier(sessionId)
        await(sessionStore.save(AgentSession(Some(personal), Some(servicePIR), Some("ni"), Some(validNino.value), Some(dateOfBirth))))

        givenAgentReference(arn, "ABCDEFGH", personal)
        givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
        givenCitizenDetailsAreKnownFor(validNino.value, "First", "Last")
        givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, servicePIR)

        val requestWithForm =
          request.withFormUrlEncodedBody(
            "clientIdentifier" -> validNino.value,
            "dob.year"         -> "1980",
            "dob.month"        -> "07",
            "dob.day"          -> "07"
          )

        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value, sessionId))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentLedDeAuthController.showConfirmCancel().url)
      }

      "redirect to not-authorised when there is no relationship to deauthorise" in {
        givenAfiRelationshipNotFoundForAgent(arn, validNino)
        givenAgentReference(arn, "ABCDEFGH", personal)
        val sessionId = UUID.randomUUID().toString
        implicit val hc: HeaderCarrier = headerCarrier(sessionId)
        await(sessionStore.save(AgentSession(Some(personal), Some(servicePIR), Some("ni"), Some(validNino.value), Some(dateOfBirth), isDeAuthJourney = true)))

        givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
        givenCitizenDetailsAreKnownFor(validNino.value, "First", "Last")
        givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, servicePIR)

        val requestWithForm =
          request.withFormUrlEncodedBody(
            "clientIdentifier" -> validNino.value,
            "dob.year"   -> "1980",
            "dob.month"  -> "07",
            "dob.day"    -> "07"
          )

        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value, sessionId))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentsErrorController.notAuthorised().url)
      }
    }

    "service is HMRC-MTD-IT" should {
      "handle the form correctly and redirect" in {
        val sessionId = UUID.randomUUID().toString
        implicit val hc: HeaderCarrier = headerCarrier(sessionId)
        await(sessionStore.save(AgentSession(Some(personal), Some(serviceITSA), Some("ni"), Some(validNino.value), Some(validPostcode))))

        givenAgentReference(arn, "ABCDEFGH", personal)
        givenMatchingClientIdAndPostcode(validNino, validPostcode)
        givenCitizenDetailsAreKnownFor(validNino.value, "First", "Last")
        givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, serviceITSA)

        val requestWithForm =
          request.withFormUrlEncodedBody(
            "clientIdentifier" -> validNino.value,
            "postcode"         -> s"$validPostcode"
          )

        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value, sessionId))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentLedDeAuthController.showConfirmClient().url)
      }
    }

    "service is HMRC-MTD-VAT" should {
      "handle the form correctly and redirect" in {
        val sessionId = UUID.randomUUID().toString
        implicit val hc: HeaderCarrier = headerCarrier(sessionId)
        await(sessionStore.save(AgentSession(Some(personal), Some(serviceVAT), Some("vrn"), Some(validVrn.value), Some(validRegistrationDate))))
        givenAgentReference(arn, "ABCDEFGH", personal)
        givenVatRegisteredClientReturns(validVrn, LocalDate.parse(validRegistrationDate), 204)
        givenCitizenDetailsAreKnownFor(validNino.value, "First", "Last")
        givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)

        val requestWithForm =
          request.withFormUrlEncodedBody(
            "clientIdentifier"       -> validVrn.value,
            "registrationDate.year"  -> "2007",
            "registrationDate.month" -> "7",
            "registrationDate.day"   -> "7"
          )

        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value, sessionId))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentLedDeAuthController.showConfirmClient().url)
      }
    }
  }

  "GET  /agents/cancel-authorisation/confirm-client" should {

    val request = FakeRequest("GET", "/agents/cancel-authorisation/confirm-client")
    val showConfirmClient = controller.showConfirmClient()

    "display the page correctly" in {
      val sessionId = UUID.randomUUID().toString
      implicit val hc: HeaderCarrier = headerCarrier(sessionId)
      await(sessionStore.save(
        AgentSession(Some(personal), Some(serviceITSA), Some("ni"), Some(validNino.value), Some(validPostcode))))
      givenTradingName(validNino, "some trading name")

      val result = showConfirmClient(authorisedAsValidAgent(request, arn.value, sessionId))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "some trading name")
      checkHtmlResultWithBodyMsgs(result, "cancel-authorisation.confirm-client.header")
      checkHtmlResultWithBodyMsgs(result, "cancel-authorisation.confirm-client.yes")
      checkHtmlResultWithBodyMsgs(result, "cancel-authorisation.confirm-client.no")
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/identify-client")
    }
  }

  "POST  /agents/cancel-authorisation/confirm-client" when {

    val request = FakeRequest("POST", "/agents/cancel-authorisation/confirm-client")
    val submitConfirmClient = controller.submitConfirmClient()

    "user selects Yes and clicks Continue" should {
      "show /cancel-authorisation/confirm-cancel page as expected" in {
        val sessionId = UUID.randomUUID().toString
        implicit val hc: HeaderCarrier = headerCarrier(sessionId)
        await(sessionStore.save(
          AgentSession(Some(personal), Some(serviceITSA), Some("ni"), Some(validNino.value), Some(validPostcode), isDeAuthJourney = true)))

        givenCheckRelationshipItsaWithStatus(arn, validNino.value, 200)
        givenTradingName(validNino, "My Trading Name")

        val choice = agentConfirmationForm("error message").fill(Confirmation(true))
        val requestWithForm = request.withFormUrlEncodedBody(choice.data.toSeq: _*)

        val result = submitConfirmClient(authorisedAsValidAgent(requestWithForm, arn.value, sessionId))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentLedDeAuthController.showConfirmCancel().url)
      }

      "redirect to /not-authorised when there is no relationship to de-authorise" in {
        givenCheckRelationshipItsaWithStatus(arn, validNino.value, 404)
        givenTradingName(validNino, "My Trading Name")
        val sessionId = UUID.randomUUID().toString
        implicit val hc: HeaderCarrier = headerCarrier(sessionId)
        await(sessionStore.save(
          AgentSession(Some(personal), Some(serviceITSA), Some("ni"), Some(validNino.value), Some(validPostcode))))
        val choice = agentConfirmationForm("error message").fill(Confirmation(true))
        val requestWithForm = request.withFormUrlEncodedBody(choice.data.toSeq: _*)

        val result = submitConfirmClient(authorisedAsValidAgent(requestWithForm, arn.value, sessionId))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentsErrorController.notAuthorised().url)
      }
    }

    "user selects No and clicks Continue" should {

      "show /cancel-authorisation/client-type page as expected" in {
        val sessionId = UUID.randomUUID().toString
        implicit val hc: HeaderCarrier = headerCarrier(sessionId)
        await(sessionStore.save(
          AgentSession(Some(personal), Some(serviceITSA), Some("ni"), Some(validNino.value), Some(validPostcode))))

        val choice = agentConfirmationForm("error message").fill(Confirmation(false))
        val requestWithForm = request.withFormUrlEncodedBody(choice.data.toSeq: _*)

        val result = submitConfirmClient(authorisedAsValidAgent(requestWithForm, arn.value, sessionId))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentLedDeAuthController.showClientType().url)
      }
    }
  }

  "GET  /agents/cancel-authorisation/confirm-cancel" should {

    val request = FakeRequest("GET", "/agents/cancel-authorisation/confirm-cancel")
    val showConfirmCancel = controller.showConfirmCancel()

    "display the page correctly" in {
      val sessionId = UUID.randomUUID().toString
      implicit val hc: HeaderCarrier = headerCarrier(sessionId)
      await(sessionStore.save(
        AgentSession(Some(personal), Some(serviceITSA), Some("ni"), Some(validNino.value), Some(validPostcode))))
      givenTradingName(validNino, "some trading name")

      val result = showConfirmCancel(authorisedAsValidAgent(request, arn.value, sessionId))
      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "cancel-authorisation.confirm-cancel.header")
      checkHtmlResultWithBodyText(
        result,
        "If you cancel your authorisation, you will not be able to report income and expenses for some trading name")
      checkResultContainsBackLink(result, "/invitations/agents/cancel-authorisation/confirm-client")
    }
  }

  "POST  /agents/cancel-authorisation/confirm-cancel" when {

    val request = FakeRequest("POST", "/agents/cancel-authorisation/confirm-cancel")
    val submitConfirmCancel = controller.submitConfirmCancel()

    "user selects Yes and clicks Continue" should {

      "show /cancel-authorisation/cancelled page as expected" in {
        val sessionId = UUID.randomUUID().toString
        implicit val hc: HeaderCarrier = headerCarrier(sessionId)
        await(sessionStore.save(
          AgentSession(Some(personal), Some(serviceITSA), Some("ni"), Some(validNino.value), Some(validPostcode))))

        givenCancelledAuthorisationItsa(arn, validNino, 204)

        val choice = agentConfirmationForm("error message").fill(Confirmation(true))
        val requestWithForm = request.withFormUrlEncodedBody(choice.data.toSeq: _*)

        val result = submitConfirmCancel(authorisedAsValidAgent(requestWithForm, arn.value, sessionId))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentLedDeAuthController.showCancelled().url)
      }
    }

    "user selects No and clicks Continue" should {

      "show /cancel-authorisation/client-type page as expected" in {
        val sessionId = UUID.randomUUID().toString
        implicit val hc: HeaderCarrier = headerCarrier(sessionId)
        await(sessionStore.save(
          AgentSession(Some(personal), Some(serviceITSA), Some("ni"), Some(validNino.value), Some(validPostcode))))

        val choice = agentConfirmationForm("error message").fill(Confirmation(false))
        val requestWithForm = request.withFormUrlEncodedBody(choice.data.toSeq: _*)

        val result = submitConfirmCancel(authorisedAsValidAgent(requestWithForm, arn.value, sessionId))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentLedDeAuthController.showClientType().url)
      }
    }
  }

  "GET  /agents/cancel-authorisation/cancelled" should {

    val request = FakeRequest("GET", "/agents/cancel-authorisation/cancelled")
    val showCancelled = controller.showCancelled()

    "display the page correctly" in {
      val sessionId = UUID.randomUUID().toString
      implicit val hc: HeaderCarrier = headerCarrier(sessionId)
      await(sessionStore.save(
        AgentSession(Some(personal), Some(serviceITSA), Some("ni"), Some(validNino.value), Some(validPostcode))))
      givenGetAgencyNameClientStub(arn)
      givenTradingName(validNino, "Some Client Company")

      val result = showCancelled(authorisedAsValidAgent(request, arn.value, sessionId))
      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "cancel-authorisation.cancelled.header",
        "cancel-authorisation.cancelled.subheader",
        "cancel-authorisation.cancelled.print")

      val today = java.time.LocalDate.now().format(DateTimeFormatter.ofPattern("d MMMM YYYY").withZone(ZoneOffset.UTC))

      checkHtmlResultWithBodyText(result, today)
      checkHtmlResultWithBodyText(
        result,
        "My Agency is no longer authorised by Some Client Company to report their income or expenses through software.")
    }
  }

  "GET /no-client-found" should {

    "display the page correctly" in {
      val request = FakeRequest("GET", "/agents/cancel-authorisation/no-client-found")
      val result = controller.noClientFound()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "cancel-authorisation.not-matched.header",
        "cancel-authorisation.not-matched.description",
        "cancel-authorisation.not-matched.advice",
        "cancel-authorisation.start-over.button"
      )
    }
  }

  "GET /response-failed" should {

    "display the page correctly" in {
      val request = FakeRequest("GET", "/agents/cancel-authorisation/response-failed")
      val result = controller.responseFailed()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result,
        "cancel-authorisation.response-failed.header",
        "cancel-authorisation.response-failed.description",
        "cancel-authorisation.response-failed.advice",
        "cancel-authorisation.response-failed.tryAgain"
      )
    }
  }
}
