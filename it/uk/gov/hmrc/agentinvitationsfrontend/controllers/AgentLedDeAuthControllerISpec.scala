package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.time.ZoneOffset
import java.time.format.DateTimeFormatter

import akka.util.Timeout
import org.joda.time.LocalDate
import play.api.test.FakeRequest
import play.api.test.Helpers.redirectLocation
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.agentConfirmationForm
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models.{Confirmation, CurrentAuthorisationRequest, Services}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class AgentLedDeAuthControllerISpec extends BaseISpec with AuthBehaviours {

  lazy val controller: AgentLedDeAuthController = app.injector.instanceOf[AgentLedDeAuthController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

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
    }
  }

  "POST /cancel-authorisation/client-type" should {

    "return redirect to /select-service page after storing client_type in the cache" in {
      val request = FakeRequest("POST", "/agents/cancel-authorisation/client-type")
      val submitClientType = controller.submitClientType()

      val result = submitClientType(authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> "personal"), arn.value))
      status(result) shouldBe 303
      val timeout = 2.seconds
      redirectLocation(result)(timeout).get shouldBe routes.AgentLedDeAuthController.showSelectService().url

      await(testCurrentAuthorisationRequestCache.fetch).get shouldBe CurrentAuthorisationRequest(Some("personal"))
    }
  }

  "GET /cancel-authorisation/select-service" should {
    "return 200 with expected page content" in {
      testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(Some("personal")))
      val request = FakeRequest("GET", "/agents/cancel-authorisation/select-service")
      val showSelectService = controller.showSelectService()

      val result = showSelectService(authorisedAsValidAgent(request, arn.value))
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
    }
  }

  "POST /cancel-authorisation/select-service" should {

    "return redirect after storing service_type in the cache" in {
      testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(Some("personal")))
      val request = FakeRequest("POST", "/agents/cancel-authorisation/select-service")
      val submitSelectService = controller.submitSelectService()

      val result = submitSelectService(authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> "HMRC-MTD-IT"), arn.value))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentLedDeAuthController.showIdentifyClient().url

      await(testCurrentAuthorisationRequestCache.fetch).get.service shouldBe "HMRC-MTD-IT"
    }

    "handle forms with invalid service_types" in {
      testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(Some("personal")))
      val request = FakeRequest("POST", "/agents/cancel-authorisation/select-service")
      val submitSelectService = controller.submitSelectService()

      val result = submitSelectService(authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> "HMRC-BLAH"), arn.value))
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

      Services.supportedServicesForCancelAuthorisation.foreach { service =>
        testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(Some("personal"), service))
        val result = showIdentifyClient(authorisedAsValidAgent(request, arn.value))
        status(result) shouldBe 200

        if(service == HMRCPIR) {
          checkHtmlResultWithBodyText(result,
            htmlEscapedMessage("identify-client.nino.label"),
            htmlEscapedMessage("identify-client.irv-date-of-birth.label")
          )
        } else if(service == HMRCMTDIT) {
          checkHtmlResultWithBodyText(result,
            htmlEscapedMessage("identify-client.nino.label"),
            htmlEscapedMessage("identify-client.postcode.label")
          )
        } else if(service == HMRCMTDVAT) {
          checkHtmlResultWithBodyText(result,
            htmlEscapedMessage("identify-client.vrn.label"),
            htmlEscapedMessage("identify-client.vat-registration-date.label")
          )
        }
      }
    }

    "redirect to /cancel-authorisation/select-service page if service in the cache is not a valid service" in {
      testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(Some("personal"), "blah-blah service"))
      val result = showIdentifyClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentLedDeAuthController.showSelectService().url
    }

    "redirect to /cancel-authorisation/client-type page if there is no cache found" in {
      testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest())
      val result = showIdentifyClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentLedDeAuthController.showClientType().url

    }
  }

  "POST /agents/cancel-authorisation/identify-client" when {
    val request = FakeRequest("POST", "/agents/cancel-authorisation/identify-client")
    val submitIdentifyClient = controller.submitIdentifyClient()

    "service is PERSONAL-INCOME-RECORD" should {
      "handle the form correctly and redirect" in {
        testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(personal, servicePIR, "ni", validNino.value, Some(dateOfBirth)))
        givenAgentReference(arn, "ABCDEFGH", "personal")
        givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
        givenCitizenDetailsAreKnownFor(validNino.value, "First", "Last")

        val requestWithForm =
          request.withFormUrlEncodedBody(
            "clientIdentifier" -> validNino.value,
            "dob.year"   -> "1980",
            "dob.month"  -> "07",
            "dob.day"    -> "07"
          )

        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentLedDeAuthController.showConfirmCancel().url)
      }
    }

    "service is HMRC-MTD-IT" should {
      "handle the form correctly and redirect" in {
        testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(personal, serviceITSA, "ni", validNino.value, Some(validPostcode)))
        givenAgentReference(arn, "ABCDEFGH", "personal")
        givenMatchingClientIdAndPostcode(validNino, validPostcode)
        givenCitizenDetailsAreKnownFor(validNino.value, "First", "Last")

        val requestWithForm =
          request.withFormUrlEncodedBody(
            "clientIdentifier" -> validNino.value,
            "postcode"   -> s"$validPostcode"
          )

        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentLedDeAuthController.showConfirmClient().url)
      }
    }

    "service is HMRC-MTD-VAT" should {
      "handle the form correctly and redirect" in {
        testCurrentAuthorisationRequestCache.save(CurrentAuthorisationRequest(personal, serviceVAT, "vrn", validVrn.value, Some(validRegistrationDate)))
        givenAgentReference(arn, "ABCDEFGH", "personal")
        givenVatRegisteredClientReturns(validVrn, LocalDate.parse(validRegistrationDate), 204)
        givenCitizenDetailsAreKnownFor(validNino.value, "First", "Last")

        val requestWithForm =
          request.withFormUrlEncodedBody(
            "clientIdentifier" -> validVrn.value,
            "registrationDate.year"   -> "2007",
            "registrationDate.month"  -> "7",
            "registrationDate.day"    -> "7"
          )

        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentLedDeAuthController.showConfirmClient().url)
      }
    }
  }

  "GET  /agents/cancel-authorisation/confirm-client" should {

    val request = FakeRequest("GET", "/agents/cancel-authorisation/confirm-client")
    val showConfirmClient = controller.showConfirmClient()

    "display the page correctly" in {
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(personal, serviceITSA, "ni", validNino.value, Some(validPostcode)))
      givenTradingName(validNino, "some trading name")

      val result = showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "some trading name")
      checkHtmlResultWithBodyMsgs(result, "cancel-authorisation.confirm-client.header")
      checkHtmlResultWithBodyMsgs(result, "cancel-authorisation.confirm-client.yes")
      checkHtmlResultWithBodyMsgs(result, "cancel-authorisation.confirm-client.no")
    }
  }

  "POST  /agents/cancel-authorisation/confirm-client" when {

    val request = FakeRequest("POST", "/agents/cancel-authorisation/confirm-client")
    val submitConfirmClient = controller.submitConfirmClient()

    "user selects Yes and clicks Continue" should {

      "show /cancel-authorisation/confirm-cancel page as expected" in {
        testCurrentAuthorisationRequestCache.save(
          CurrentAuthorisationRequest(personal, serviceITSA, "ni", validNino.value, Some(validPostcode)))

        val choice = agentConfirmationForm("error message").fill(Confirmation(true))
        val requestWithForm = request.withFormUrlEncodedBody(choice.data.toSeq: _*)

        val result = submitConfirmClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentLedDeAuthController.showConfirmCancel().url)
      }
    }

    "user selects No and clicks Continue" should {

      "show /cancel-authorisation/client-type page as expected" in {
        testCurrentAuthorisationRequestCache.save(
          CurrentAuthorisationRequest(personal, serviceITSA, "ni", validNino.value, Some(validPostcode)))

        val choice = agentConfirmationForm("error message").fill(Confirmation(false))
        val requestWithForm = request.withFormUrlEncodedBody(choice.data.toSeq: _*)

        val result = submitConfirmClient(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentLedDeAuthController.showClientType().url)
      }
    }
  }

  "GET  /agents/cancel-authorisation/confirm-cancel" should {

    val request = FakeRequest("GET", "/agents/cancel-authorisation/confirm-cancel")
    val showConfirmCancel = controller.showConfirmCancel()

    "display the page correctly" in {
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(personal, serviceITSA, "ni", validNino.value, Some(validPostcode)))
      givenTradingName(validNino, "some trading name")

      val result = showConfirmCancel(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "cancel-authorisation.confirm-cancel.header")
      checkHtmlResultWithBodyText(result, "If you cancel your authorisation, you will not be able to report income and expenses for some trading name")
    }
  }

  "POST  /agents/cancel-authorisation/confirm-cancel" when {

    val request = FakeRequest("POST", "/agents/cancel-authorisation/confirm-cancel")
    val submitConfirmCancel = controller.submitConfirmCancel()

    "user selects Yes and clicks Continue" should {

      "show /cancel-authorisation/cancelled page as expected" in {
        testCurrentAuthorisationRequestCache.save(
          CurrentAuthorisationRequest(personal, serviceITSA, "ni", validNino.value, Some(validPostcode)))

        givenCancelledAuthorisationItsa(arn, validNino, 204)

        val choice = agentConfirmationForm("error message").fill(Confirmation(true))
        val requestWithForm = request.withFormUrlEncodedBody(choice.data.toSeq: _*)

        val result = submitConfirmCancel(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentLedDeAuthController.showCancelled().url)
      }
    }

    "user selects No and clicks Continue" should {

      "show /cancel-authorisation/client-type page as expected" in {
        testCurrentAuthorisationRequestCache.save(
          CurrentAuthorisationRequest(personal, serviceITSA, "ni", validNino.value, Some(validPostcode)))

        val choice = agentConfirmationForm("error message").fill(Confirmation(false))
        val requestWithForm = request.withFormUrlEncodedBody(choice.data.toSeq: _*)

        val result = submitConfirmCancel(authorisedAsValidAgent(requestWithForm, arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentLedDeAuthController.showClientType().url)
      }
    }
  }

  "GET  /agents/cancel-authorisation/cancelled" should {

    val request = FakeRequest("GET", "/agents/cancel-authorisation/cancelled")
    val showCancelled = controller.showCancelled()

    "display the page correctly" in {
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(personal, serviceITSA, "ni", validNino.value, Some(validPostcode)))
      givenGetAgencyNameClientStub(arn)
      givenTradingName(validNino, "Some Client Company")

      val result = showCancelled(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result,
        "cancel-authorisation.cancelled.header",
         "cancel-authorisation.cancelled.subheader",
         "cancel-authorisation.cancelled.print"
      )

      val today = java.time.LocalDate.now().format(DateTimeFormatter.ofPattern("d MMMM YYYY").withZone(ZoneOffset.UTC))

      checkHtmlResultWithBodyText(result, today)
      checkHtmlResultWithBodyText(result, "My Agency is no longer authorised by Some Client Company to report their income or expenses through software.")
    }
  }
}
