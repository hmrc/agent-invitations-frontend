package uk.gov.hmrc.agentinvitationsfrontend.controllers

import org.joda.time.LocalDate
import play.api.test.FakeRequest
import play.api.test.Helpers.redirectLocation
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.models.{CurrentInvitationInput, UserInputNinoAndPostcode}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec

import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId
import play.api.test.Helpers._


import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationControllerFastTrackISpec extends BaseISpec {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]


  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  "POST /agents/select-service" should {
    val request = FakeRequest("POST", "/agents/select-service")
    val submitService = controller.submitService()

    "return 303 for authorised Agent with valid Nino but selected VAT, redirect to identify-client" in {
      testFastTrackCache.save(
        CurrentInvitationInput("", "", "ni", validNino.value, None, fromFastTrack))
      createInvitationStub(
        arn,
        validNino.value,
        invitationIdPIR,
        validNino.value,
        "ni",
        servicePIR,
        "NI")
      getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, "NI", "Pending")
      val serviceForm = agentInvitationServiceForm.fill(UserInputNinoAndPostcode(business, serviceVAT, None, None))
      val result =
        submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/identify-client")
      verifyAuthoriseAttempt()
    }

  }

  "POST /agents/fast-track" should {
    val request = FakeRequest("POST",
      "/agents/fast-track?continue=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fselect-client&error=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fnot-authorised")
    val fastTrack = controller.agentFastTrack()

    "return 303 and redirect to error url if service calling fast-track does not have supported service in payload" in {
      val formData = CurrentInvitationInput("INVALID_CLIENT_TYPE", "INVALID_SERVICE").copy(fromFastTrack = fromFastTrack)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe
        Some("http://localhost:9996/tax-history/not-authorised?issue=UNSUPPORTED_CLIENT_TYPE UNSUPPORTED_SERVICE UNSUPPORTED_CLIENT_ID_TYPE INVALID_CLIENT_ID_RECEIVED:NOTHING")
    }

    "return 303 and redirect to error url with mixed form data" in {
      val formData =
        CurrentInvitationInput(business, serviceITSA, "vrn", validNino.value, None, fromFastTrack)
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
        Some("http://localhost:9996/tax-history/not-authorised?issue=This field is required This field is required This field is required This field is required")
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
      val formData = CurrentInvitationInput()
      testFastTrackCache.save(formData)
      an[Exception] shouldBe thrownBy {
        await(controller.checkDetails(authorisedAsValidAgent(request, arn.value)))
      }
    }

    "Redirect to select service when there is None in the cache" in {
      val result = await(controller.checkDetails(authorisedAsValidAgent(request, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.selectClientType().url
    }

    "An IllegalArgumentException should be thrown when the client identifier type is not valid" in {
      val formData =
        CurrentInvitationInput(business, serviceVAT, "foo", validVrn.value, Some(validRegistrationDate), fromFastTrack)
      testFastTrackCache.save(formData)

      an[IllegalArgumentException] shouldBe thrownBy {
        await(controller.checkDetails(authorisedAsValidAgent(request, arn.value)))
      }
    }
  }

  "POST /agents/check-details" should {
    val request = FakeRequest()

    "show error on the page when no radio button is selected" in {
      val formData =
        CurrentInvitationInput(business, serviceVAT, "vrn", validVrn.value, Some(validRegistrationDate), fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.submitDetails(authorisedAsValidAgent(request, arn.value)))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Select yes if the details are correct"))
    }
  }

  "GET agents/more-details" should {
    val request = FakeRequest()

    "redirect to select-service if there is no invitation in the cache" in {
      val result = await(controller.knownFact(authorisedAsValidAgent(request, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/select-service")
    }
  }

  "POST /agents/more-details" should {
    val request = FakeRequest("POST", "/agents/identify-client")

    "redirect to select service when form data is invalid" in {
      createInvitationStub(
        arn,
        validNino.value,
        invitationIdPIR,
        validNino.value,
        "ni",
        servicePIR,
        "NI")
      givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
      getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, "NI", "Pending")

      val requestWithForm = request.withFormUrlEncodedBody("foo" -> "bar")
      val formData =
        CurrentInvitationInput(personal, servicePIR, "ni", validNino.value, None, fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.submitKnownFact(authorisedAsValidAgent(requestWithForm, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/select-service")
    }
  }
}
