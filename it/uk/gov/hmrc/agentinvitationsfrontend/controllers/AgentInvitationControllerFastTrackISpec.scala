package uk.gov.hmrc.agentinvitationsfrontend.controllers

import org.joda.time.LocalDate
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsFastTrackInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentFastTrackRequest, AgentSession}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationControllerFastTrackISpec extends BaseISpec {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  lazy val fastTrackController: AgentsFastTrackInvitationController =
    app.injector.instanceOf[AgentsFastTrackInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  "POST /agents/fast-track" should {
    val request = FakeRequest(
      "POST",
      "/agents/fast-track?continue=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fselect-client&error=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fnot-authorised"
    )
    val fastTrack = fastTrackController.agentFastTrack()

    "return 303 and redirect to error url if service calling fast-track does not have supported service in payload" in {
      val formData = AgentFastTrackRequest(Some(personal), "INVALID_SERVICE")
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe
        Some(
          "http://localhost:9996/tax-history/not-authorised?issue=UNSUPPORTED_SERVICE UNSUPPORTED_CLIENT_ID_TYPE INVALID_CLIENT_ID_RECEIVED:NOTHING")
    }

    "return 303 and redirect to error url with mixed form data" in {
      val formData =
        AgentFastTrackRequest(Some(business), serviceITSA, "vrn", validNino.value, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe
        Some("http://localhost:9996/tax-history/not-authorised?issue=INVALID_SUBMISSION")
    }

    "return 303 and redirect to error url if the form is invalid" in {
      val requestWithForm = request
        .withFormUrlEncodedBody("goo" -> "", "bah" -> "", "gah" -> "")
      val result = fastTrack(authorisedAsValidAgent(requestWithForm, arn.value))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe
        Some(
          "http://localhost:9996/tax-history/not-authorised?issue=This field is required This field is required This field is required")
    }

    "throw an exception for not providing an error url" in {
      val requestNoUrl = FakeRequest("POST", "agents/fast-track")
      val requestWithForm = requestNoUrl
        .withFormUrlEncodedBody("goo" -> "", "bah" -> "", "gah" -> "")

      intercept[IllegalStateException] {
        await(fastTrack(authorisedAsValidAgent(requestWithForm, arn.value)))
      }.getMessage shouldBe "No Error Url Provided"

    }
  }

  "GET /agents/check-details" should {

    val request = FakeRequest()

    "Throw an exception when the cache is empty" in {
      testAgentSessionCache.save(AgentSession())
      an[Exception] shouldBe thrownBy {
        await(fastTrackController.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
      }
    }

    "Redirect to select service when there is None in the cache" in {
      val result = await(fastTrackController.showCheckDetails(authorisedAsValidAgent(request, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showClientType().url
    }
  }

  "POST /agents/check-details" should {
    val request = FakeRequest()

    "show error on the page when no radio button is selected" in {
      val formData =
        AgentFastTrackRequest(Some(business), serviceVAT, "vrn", validVrn.value, Some(validRegistrationDate))
      testAgentSessionCache.save(
        AgentSession(Some(business), Some(serviceVAT), Some("vrn"), Some(validVrn.value), Some(validRegistrationDate)))
      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(
            request.withFormUrlEncodedBody(agentFastTrackForm.fill(formData).data.toSeq: _*),
            arn.value)))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Select yes if the details are correct"))
    }
  }

  "GET agents/more-details" should {
    val request = FakeRequest()

    "redirect to client-type if there is no invitation in the cache" in {
      val result = await(fastTrackController.showKnownFact(authorisedAsValidAgent(request, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showClientType().url
    }
  }

  "POST /agents/more-details" should {
    val request = FakeRequest("POST", "/agents/identify-client")

    "return form with errors when form data is invalid" in {
      givenInvitationCreationSucceeds(
        arn,
        Some(personal),
        validNino.value,
        invitationIdPIR,
        validNino.value,
        "ni",
        servicePIR,
        "NI")
      givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))

      testAgentSessionCache.save(
        AgentSession(Some(personal), Some(servicePIR), Some("ni"), Some(validNino.value), Some(validPostcode)))

      val requestWithForm = request.withFormUrlEncodedBody("foo" -> "bar")
      val result = await(fastTrackController.submitKnownFact(authorisedAsValidAgent(requestWithForm, arn.value)))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "This field is required")
    }
  }
}
