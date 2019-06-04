package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.util.UUID

import com.google.inject.AbstractModule
import play.api.Application
import play.api.test.FakeRequest
import uk.gov.hmrc.agentinvitationsfrontend.controllers.retired.AgentsInvitationController.agentConfirmationForm
import uk.gov.hmrc.agentinvitationsfrontend.models.Confirmation
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

class AgentLedDeAuthControllerFlagOffISpec extends BaseISpec with AuthBehaviours {

  override implicit lazy val app: Application = appBuilder(oppositeFeatureFlags)
    .build()

  private class TestGuiceModule extends AbstractModule {
    override def configure(): Unit = {}
  }

  lazy val controller: AgentLedDeAuthController = app.injector.instanceOf[AgentLedDeAuthController]
  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId(UUID.randomUUID().toString)))

  "GET /cancel-authorisation/client-type" should {

    "return 401 when flag is off" in {
      val request = FakeRequest("GET", "/agents/cancel-authorisation/client-type")
      val selectClientType = controller.showClientType()

      val result = selectClientType(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 501
    }
  }

  "POST /cancel-authorisation/client-type" should {

    "return 401 when flag is off" in {
      val request = FakeRequest("POST", "/agents/cancel-authorisation/client-type")
      val submitClientType = controller.submitClientType()

      val result =
        submitClientType(authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> "personal"), arn.value))
      status(result) shouldBe 501
    }
  }

  "GET /cancel-authorisation/select-service" should {

    "return 401 when flag is off" in {
      val request = FakeRequest("GET", "/agents/cancel-authorisation/select-service")
      val showSelectService = controller.showSelectService()

      val result = showSelectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 501
    }
  }

  "POST /cancel-authorisation/select-service" should {

    "return 401 when flag is off" in {
      val request = FakeRequest("POST", "/agents/cancel-authorisation/select-personal-service")
      val submitSelectService = controller.submitSelectPersonalService()

      val result = submitSelectService(
        authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> "HMRC-MTD-IT"), arn.value))
      status(result) shouldBe 501
    }
  }

  "GET /agents/cancel-authorisation/identify-client" should {

    "return 401 when flag is off" in {
      val request = FakeRequest("GET", "/agents/cancel-authorisation/identify-client")
      val showIdentifyClient = controller.showIdentifyClient()

      val result = showIdentifyClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 501
    }
  }

  "POST /agents/cancel-authorisation/identify-client" when {

    "return 401 when flag is off" in {
      val request = FakeRequest("POST", "/agents/cancel-authorisation/identify-itsa-client")
      val submitIdentifyClient = controller.submitIdentifyClientItsa()

      val requestWithForm =
        request.withFormUrlEncodedBody(
          "clientIdentifier" -> validNino.value,
          "dob.year"         -> "1980",
          "dob.month"        -> "07",
          "dob.day"          -> "07"
        )

      val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm, arn.value))

      status(result) shouldBe 501
    }
  }

  "GET  /agents/cancel-authorisation/confirm-client" should {

    "return 401 when flag is off" in {
      val request = FakeRequest("GET", "/agents/cancel-authorisation/confirm-client")
      val showConfirmClient = controller.showConfirmClient()

      val result = showConfirmClient(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 501
    }
  }

  "POST  /agents/cancel-authorisation/confirm-client" when {

    "return 401 when flag is off" in {
      val request = FakeRequest("POST", "/agents/cancel-authorisation/confirm-client")
      val submitConfirmClient = controller.submitConfirmClient()

      val choice = agentConfirmationForm("error message").fill(Confirmation(true))
      val requestWithForm = request.withFormUrlEncodedBody(choice.data.toSeq: _*)

      val result = submitConfirmClient(authorisedAsValidAgent(requestWithForm, arn.value))

      status(result) shouldBe 501
    }
  }

  "GET  /agents/cancel-authorisation/confirm-cancel" should {

    "return 401 when flag is off" in {
      val request = FakeRequest("GET", "/agents/cancel-authorisation/confirm-cancel")
      val showConfirmCancel = controller.showConfirmCancel()

      val result = showConfirmCancel(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 501
    }
  }

  "POST  /agents/cancel-authorisation/confirm-cancel" when {

    "return 401 when flag is off" in {
      val request = FakeRequest("POST", "/agents/cancel-authorisation/confirm-cancel")
      val submitConfirmCancel = controller.submitConfirmCancel()

      val choice = agentConfirmationForm("error message").fill(Confirmation(true))
      val requestWithForm = request.withFormUrlEncodedBody(choice.data.toSeq: _*)

      val result = submitConfirmCancel(authorisedAsValidAgent(requestWithForm, arn.value))

      status(result) shouldBe 501
    }
  }

  "GET  /agents/cancel-authorisation/cancelled" should {

    "return 401 when flag is off" in {
      val request = FakeRequest("GET", "/agents/cancel-authorisation/cancelled")
      val showCancelled = controller.showCancelled()

      val result = showCancelled(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 501
    }
  }

}
