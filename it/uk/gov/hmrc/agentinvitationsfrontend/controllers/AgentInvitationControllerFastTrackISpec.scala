package uk.gov.hmrc.agentinvitationsfrontend.controllers

import org.joda.time.LocalDate
import play.api.test.FakeRequest
import play.api.test.Helpers.redirectLocation
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.models.{CurrentInvitationInput, UserInputNinoAndPostcode, UserInputVrnAndRegDate}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.audit.AgentInvitationEvent

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationControllerFastTrackISpec extends BaseISpec {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  val arn = Arn("TARN0000001")
  val mtdItId = MtdItId("ABCDEF123456789")
  private val validNino = Nino("AB123456A")
  private val validNinoSpace = Nino("AB 12 34 56 A")
  val serviceITSA = "HMRC-MTD-IT"
  val servicePIR = "PERSONAL-INCOME-RECORD"
  val validPostcode = "DH14EJ"
  val validPostcodeLong = "BN114AW"
  val validPostcodeSpaces = "DH1 4EJ"
  val invitationIdITSA = InvitationId("ABERULMHCKKW3")
  val invitationIdPIR = InvitationId("B9SCS2T4NZBAX")

  val invitationIdVAT = InvitationId("CZTW1KY6RTAAT")
  val serviceVAT = "HMRC-MTD-VAT"
  val identifierVAT = "VRN"
  val validVrn97 = Vrn("101747696")
  val invalidVrn = Vrn("101747692")
  val validRegDateForVrn97 = Some("2007-07-07")
  val dateOfBirth = "1980-07-07"
  val validVrn9755 = Vrn("101747641")
  val fromFastTrack: Boolean = true
  val fromManual: Boolean = false

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  //TODO - Along with the other tests, could be split up by service? Note: This test is based on fast-track scenarios

  "POST /agents/select-service" should {
    val request = FakeRequest("POST", "/agents/select-service")
    val submitService = controller.submitService()

    "return 303 for authorised Agent with valid Nino and Known Fact, then selected ITSA, redirect to invitation-sent" in {
      testFastTrackCache.save(
        CurrentInvitationInput("", "ni", validNino.value, Some(validPostcode), fromFastTrack))
      createInvitationStub(
        arn,
        mtdItId.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        serviceITSA,
        "NI")
      givenMatchingClientIdAndPostcode(validNino, validPostcode)
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, "NI", "Pending")
      val serviceForm = agentInvitationServiceForm.fill(UserInputNinoAndPostcode(serviceITSA, None, None))
      val result =
        submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      verifyAuthoriseAttempt()
    }

    "return 303 for authorised Agent with valid Nino then selected IRV, redirect to invitation-sent" in {
      testFastTrackCache.save(
        CurrentInvitationInput("", "ni", validNino.value, Some(dateOfBirth), fromFastTrack))
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
      val serviceForm = agentInvitationServiceForm.fill(UserInputNinoAndPostcode(servicePIR, None, None))
      val result =
        submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      verifyAuthoriseAttempt()
    }

    "return 303 for authorised Agent with valid VAT Information and selected VAT, redirect to invitation-sent" in {
      testFastTrackCache.save(
        CurrentInvitationInput("", "vrn", validVrn97.value, validRegDateForVrn97, fromFastTrack))
      createInvitationStub(
        arn,
        validVrn97.value,
        invitationIdVAT,
        validVrn97.value,
        "vrn",
        serviceVAT,
        identifierVAT)
      getInvitationStub(arn, validVrn97.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      checkVatRegisteredClientStub(validVrn97, LocalDate.parse("2007-07-07"), 204)
      val serviceForm = agentInvitationServiceForm.fill(UserInputNinoAndPostcode(serviceVAT, None, None))
      val result =
        submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      verifyAuthoriseAttempt()
    }

    "return 303 for authorised Agent with valid VAT Information but selected ITSA, redirect to /agents/identify-client" in {
      testFastTrackCache.save(
        CurrentInvitationInput("", "vrn", validVrn97.value, validRegDateForVrn97, fromFastTrack))
      createInvitationStub(
        arn,
        validVrn97.value,
        invitationIdVAT,
        validVrn97.value,
        "vrn",
        serviceVAT,
        identifierVAT)
      getInvitationStub(arn, validVrn97.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      checkVatRegisteredClientStub(validVrn97, LocalDate.parse("2007-07-07"), 204)
      val serviceForm = agentInvitationServiceForm.fill(UserInputNinoAndPostcode(serviceITSA, None, None))
      val result =
        submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/identify-client")
      verifyAuthoriseAttempt()
    }

    "return 303 for authorised Agent with valid Nino but selected VAT, redirect to identify-client" in {
      testFastTrackCache.save(
        CurrentInvitationInput("", "ni", validNino.value, None, fromFastTrack))
      createInvitationStub(
        arn,
        validNino.value,
        invitationIdPIR,
        validNino.value,
        "ni",
        servicePIR,
        "NI")
      getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, "NI", "Pending")
      val serviceForm = agentInvitationServiceForm.fill(UserInputNinoAndPostcode(serviceVAT, None, None))
      val result =
        submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/identify-client")
      verifyAuthoriseAttempt()
    }

  }

  "POST /agents/fast-track" should {
    val request = FakeRequest("POST", "/agents/fast-track")
    val fastTrack = controller.agentFastTrack()

    "return 303 check-details if service calling fast-track is correct for ITSA" in {
      val formData =
        CurrentInvitationInput(
          serviceITSA,
          "ni",
          validNino.value,
          Some(validPostcode),
          fromFastTrack)
      val fastTrackFormData = controller.agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.checkDetails().url
    }

    "return 303 check-details if service calling fast-track is correct for VAT" in {
      val formData =
        CurrentInvitationInput(
          serviceVAT,
          "vrn",
          validVrn97.value,
          validRegDateForVrn97,
          fromFastTrack)
      val fastTrackFormData = controller.agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.checkDetails().url
    }

    "return 303 check-details if service calling fast-track is correct for IRV" in {
      val formData =
        CurrentInvitationInput(servicePIR, "ni", validNino.value, Some(dateOfBirth), fromFastTrack)
      val fastTrackFormData = controller.agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.checkDetails().url
    }

    "return 303 check-details if service calling fast-track for does not contain postcode for ITSA" in {

      val formData =
        CurrentInvitationInput(serviceITSA, "ni", validNino.value, None, fromFastTrack)
      val fastTrackFormData = controller.agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.checkDetails().url)
    }

    "return 303 if service calling fast-track contains invalid postcode for ITSA" in {
      val formData =
        CurrentInvitationInput(
          serviceITSA,
          "ni",
          validNino.value,
          Some("Invalid_Postcode"),
          fromFastTrack)
      val fastTrackFormData = controller.agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.checkDetails().url)
    }

    "return 303 check-details if service calling fast-track does not contain vat-reg-date for VAT" in {
      val formData =
        CurrentInvitationInput(serviceVAT, "vrn", validVrn97.value, None, fromFastTrack)
      val fastTrackFormData = controller.agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.checkDetails().url)
    }

    "return 303 check-details if service calling fast-track contains invalid vat-reg-date for VAT" in {
      val formData =
        CurrentInvitationInput(
          serviceVAT,
          "vrn",
          validVrn97.value,
          Some("Invalid_Reg_Date"),
          fromFastTrack)
      val fastTrackFormData = controller.agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.checkDetails().url)
    }

    "return 400 if service calling fast-track does not have supported service in payload" in {
      val formData = CurrentInvitationInput("INVALID_SERVICE").copy(fromFastTrack = fromFastTrack)
      val fastTrackFormData = controller.agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe BAD_REQUEST
    }

    "return 400 if service calling fast-track for ITSA contains invalid nino" in {
      val formData =
        CurrentInvitationInput(serviceITSA, "ni", "INVALID_NINO", None, fromFastTrack)
      val fastTrackFormData = controller.agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe BAD_REQUEST
    }

    "return 400 if service calling fast-track for PIR contains invalid nino" in {
      val formData =
        CurrentInvitationInput(servicePIR, "ni", "INVALID_NINO", None, fromFastTrack)
      val fastTrackFormData = controller.agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe BAD_REQUEST
    }

    "return 400 if service calling fast-track for VAT contains invalid vrn" in {
      val formData = CurrentInvitationInput(serviceVAT, "", "INVALID_VRN", None, fromFastTrack)
      val fastTrackFormData = controller.agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe BAD_REQUEST
    }

    "return 400 if service calling fast-track for ITSA does not contain nino" in {
      val formData = CurrentInvitationInput(serviceITSA).copy(fromFastTrack = fromFastTrack)
      val fastTrackFormData = controller.agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe BAD_REQUEST
    }

    "return 400 if service calling fast-track for IRV does not contain nino" in {
      val formData = CurrentInvitationInput(servicePIR).copy(fromFastTrack = fromFastTrack)
      val fastTrackFormData = controller.agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe BAD_REQUEST
    }

    "return 400 if service calling fast-track for VAT does not contain vrn" in {
      val formData = CurrentInvitationInput(serviceVAT).copy(fromFastTrack = fromFastTrack)
      val fastTrackFormData = controller.agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe BAD_REQUEST
    }

    "return 400 if there is no service but all other fields are valid for ITSA" in {
      val formData =
        CurrentInvitationInput("", "ni", validNino.value, Some(validPostcode), fromFastTrack)
      val fastTrackFormData = controller.agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe BAD_REQUEST
    }

    "return 400 if there is no service but all other fields are valid for IRV" in {
      val formData = CurrentInvitationInput("", "ni", validNino.value, None, fromFastTrack)
      val fastTrackFormData = controller.agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe BAD_REQUEST
    }

    "return 400 if there is no service but all other fields are valid for VAT" in {
      val formData =
        CurrentInvitationInput("", "vrn", validVrn97.value, validRegDateForVrn97, fromFastTrack)
      val fastTrackFormData = controller.agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe BAD_REQUEST
    }

    "return 400 if the form is invalid" in {
      val requestWithForm = request
        .withFormUrlEncodedBody("goo" -> "", "bah" -> "", "gah" -> "")
      val result = fastTrack(authorisedAsValidAgent(requestWithForm, arn.value))

      status(result) shouldBe BAD_REQUEST
    }
  }

  "GET /agents/check-details" should {

    val request = FakeRequest()

    "display the check details page when known fact is required and provided for ITSA for short postcode without spaces" in {
      val formData =
        CurrentInvitationInput(serviceITSA, "ni", validNino.value, Some(validPostcode), fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.checkDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("income or expenses through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("National Insurance number"))
      checkHtmlResultWithBodyText(result, "AB 12 34 56 A")
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Postcode"))
      checkHtmlResultWithBodyText(result, "DH1 4EJ")
    }

    "display the check details page when known fact is required and provided for ITSA for short postcode with spaces" in {
      val formData =
        CurrentInvitationInput(serviceITSA, "ni", validNino.value, Some(validPostcodeSpaces), fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.checkDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("income or expenses through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("National Insurance number"))
      checkHtmlResultWithBodyText(result, "AB 12 34 56 A")
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Postcode"))
      checkHtmlResultWithBodyText(result, "DH1 4EJ")
    }

    "display the check details page when known fact is required and provided for ITSA for long postcode" in {
      val formData =
        CurrentInvitationInput(serviceITSA, "ni", validNino.value, Some(validPostcodeLong), fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.checkDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("income or expenses through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("National Insurance number"))
      checkHtmlResultWithBodyText(result, "AB 12 34 56 A")
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Postcode"))
      checkHtmlResultWithBodyText(result, "BN11 4AW")
    }

    "display alternate check details page when known fact is required but not provided for ITSA" in {
      val formData =
        CurrentInvitationInput(serviceITSA, "ni", validNino.value, None, fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.checkDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("income or expenses through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("National Insurance number"))
      checkHtmlResultWithBodyText(result, "AB 12 34 56 A")
      checkHtmlResultWithBodyText(result, "Change this information")
      checkHtmlResultWithBodyText(result, "We need some more details")
    }

    "display the check details page when known fact is required and provided for IRV" in {
      val formData =
        CurrentInvitationInput(servicePIR, "ni", validNino.value, Some(dateOfBirth), fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.checkDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("view a client's PAYE income record"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("National Insurance number"))
      checkHtmlResultWithBodyText(result, "AB 12 34 56 A")
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Date of birth"))
      checkHtmlResultWithBodyText(result, "07 July 1980")
    }

    "display alternate check details page when known fact is required and not provided for IRV" in {
      val formData =
        CurrentInvitationInput(servicePIR, "ni", validNino.value, None, fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.checkDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("view a client's PAYE income record"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("National Insurance number"))
      checkHtmlResultWithBodyText(result, "AB 12 34 56 A")
      checkHtmlResultWithBodyText(result, "Change this information")
      checkHtmlResultWithBodyText(result, "We need some more details")
    }

    "display the check details page when known fact is required and provided for VAT" in {
      val formData =
        CurrentInvitationInput(serviceVAT, "vrn", validVrn97.value, validRegDateForVrn97, fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.checkDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("report a client's VAT returns through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("VAT registration number"))
      checkHtmlResultWithBodyText(result, validVrn97.value)
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("VAT registration date"))
      checkHtmlResultWithBodyText(result, "07 July 2007")
    }

    "display alternate check details page when known fact is required and not provided for VAT" in {
      val formData =
        CurrentInvitationInput(serviceVAT, "vrn", validVrn97.value, None, fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.checkDetails(authorisedAsValidAgent(request, arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("report a client's VAT returns through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("VAT registration number"))
      checkHtmlResultWithBodyText(result, validVrn97.value)
      checkHtmlResultWithBodyText(result, "Change this information")
      checkHtmlResultWithBodyText(result, "We need some more details")
    }

    "Redirect to select service when there is nothing in the cache" in {
      val result = await(controller.checkDetails(authorisedAsValidAgent(request, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/select-service")
    }

    "An IllegalArgumentException should be thrown when the client identifier type is not valid" in {
      val formData =
        CurrentInvitationInput(serviceVAT, "foo", validVrn97.value, validRegDateForVrn97, fromFastTrack)
      testFastTrackCache.save(formData)

      an[IllegalArgumentException] shouldBe thrownBy {
        await(controller.checkDetails(authorisedAsValidAgent(request, arn.value)))
      }
    }
  }

  "POST /agents/check-details" should {
    val request = FakeRequest()
    "redirect to confirm_invitation when YES is selected for ITSA service" in {
      createInvitationStub(
        arn,
        mtdItId.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        serviceITSA,
        "MTDITID")
      givenMatchingClientIdAndPostcode(validNino, validPostcode)
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, "MTDITID", "Pending")

      val formData =
        CurrentInvitationInput(serviceITSA, "ni", validNino.value, Some(validPostcode), fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.submitDetails(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "redirect to confirm_invitation when YES is selected for IRV service" in {
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

      val formData =
        CurrentInvitationInput(servicePIR, "ni", validNino.value, Some(dateOfBirth), fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.submitDetails(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "redirect to confirm_invitation when YES is selected for VAT service" in {
      createInvitationStub(
        arn,
        validVrn97.value,
        invitationIdVAT,
        validVrn97.value,
        "vrn",
        serviceVAT,
        "VRN")
      checkVatRegisteredClientStub(validVrn97, LocalDate.parse(validRegDateForVrn97.get), 200)
      getInvitationStub(arn, validVrn97.value, invitationIdVAT, serviceVAT, "VRN", "Pending")

      val formData =
        CurrentInvitationInput(serviceVAT, "vrn", validVrn97.value, Some(validRegDateForVrn97.get), fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.submitDetails(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "redirect to identify-client when NO is selected for ITSA service" in {
      createInvitationStub(
        arn,
        mtdItId.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        serviceITSA,
        "MTDITID")
      givenMatchingClientIdAndPostcode(validNino, validPostcode)
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, "MTDITID", "Pending")

      val formData =
        CurrentInvitationInput(serviceITSA, "ni", validNino.value, Some(validPostcode), fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.submitDetails(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "false")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/identify-client")
    }

    "redirect to identify-client when NO is selected for IRV service" in {
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

      val formData =
        CurrentInvitationInput(servicePIR, "ni", validNino.value, Some(dateOfBirth), fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.submitDetails(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "false")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/identify-client")
    }

    "redirect to identify-client when NO is selected for VAT service" in {
      createInvitationStub(
        arn,
        validVrn97.value,
        invitationIdVAT,
        validVrn97.value,
        "vrn",
        serviceVAT,
        "VRN")
      checkVatRegisteredClientStub(validVrn97, LocalDate.parse(validRegDateForVrn97.get), 200)
      getInvitationStub(arn, validVrn97.value, invitationIdVAT, serviceVAT, "VRN", "Pending")

      val formData =
        CurrentInvitationInput((serviceVAT), ("vrn"), (validVrn97.value), validRegDateForVrn97, fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.submitDetails(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "false")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/identify-client")
    }

    "show error on the page when no radio button is selected" in {
      val formData =
        CurrentInvitationInput(serviceVAT, "vrn", validVrn97.value, validRegDateForVrn97, fromFastTrack)
      testFastTrackCache.save(formData)
      val result = await(controller.submitDetails(authorisedAsValidAgent(request, arn.value)))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Select yes if the details are correct"))
    }

    "return 303 not-matched if nino and postcode do not match for ITSA" in {
      val formData = CurrentInvitationInput(
        serviceITSA,
        "ni",
        validNino.value,
        Some(validPostcode),
        fromFastTrack)
      testFastTrackCache.save(formData)
      testFastTrackCache.currentSession.currentInvitationInput.get shouldBe formData
      givenNonMatchingClientIdAndPostcode(validNino, validPostcode)

      val form = controller.agentFastTrackForm.fill(formData)
      val result = await(controller.submitDetails(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/not-matched")

      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value, validNino.value, "ni", "Fail", serviceITSA)
      await(testFastTrackCache.fetch()).get shouldBe formData
    }

    "return 303 not-enrolled if Agent attempts to invite client who does not have an ITSA enrolment" in {
      val formData = CurrentInvitationInput(
        serviceITSA,
        "ni",
        validNino.value,
        Some(validPostcode),
        fromFastTrack)
      testFastTrackCache.save(formData)
      testFastTrackCache.currentSession.currentInvitationInput.get shouldBe formData
      givenNotEnrolledClientITSA(validNino, validPostcode)

      val form = controller.agentFastTrackForm.fill(formData)
      val result = await(controller.submitDetails(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/not-enrolled")

      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value, validNino.value, "ni", "Fail", serviceITSA)
      await(testFastTrackCache.fetch()).get shouldBe formData
    }

    "return 303 not-matched if vrn and vat-reg-date does not match for VAT" in {
      val invitation =
        CurrentInvitationInput(serviceVAT, "vrn", validVrn97.value, validRegDateForVrn97, fromFastTrack)

      testFastTrackCache.save(invitation)
      checkVatRegisteredClientStub(validVrn97, LocalDate.parse("2007-07-07"), 403)

      val form = controller.agentFastTrackForm.fill(invitation)
      val result = await(controller.submitDetails(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))

      status(result) shouldBe 303
      header("Set-Cookie", result) shouldBe None
      redirectLocation(result) shouldBe Some("/invitations/agents/not-matched")
      verifyCheckVatRegisteredClientStubAttempt(validVrn97, LocalDate.parse("2007-07-07"))
      await(testFastTrackCache.fetch()).get shouldBe invitation
    }

    "return 303 not-enrolled if Agent attempted to invite a client for VAT" in {
      val invitation =
        CurrentInvitationInput(serviceVAT, "vrn", validVrn97.value, validRegDateForVrn97, fromFastTrack)

      testFastTrackCache.save(invitation)
      checkVatRegisteredClientStub(validVrn97, LocalDate.parse("2007-07-07"), 404)

      val form = controller.agentFastTrackForm.fill(invitation)
      val result = await(controller.submitDetails(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))

      status(result) shouldBe 303
      header("Set-Cookie", result) shouldBe None
      redirectLocation(result) shouldBe Some("/invitations/agents/not-enrolled")
      verifyCheckVatRegisteredClientStubAttempt(validVrn97, LocalDate.parse("2007-07-07"))
      await(testFastTrackCache.fetch()).get shouldBe invitation

    }

    "return 303 invitation-sent if nino that does not return citizen-details record" in {
      val formData =
        CurrentInvitationInput(servicePIR, "ni", validNino.value, Some(dateOfBirth), fromFastTrack)
      testFastTrackCache.save(formData)
      testFastTrackCache.currentSession.currentInvitationInput.get shouldBe formData
      givenCitizenDetailsReturns404For(validNino.value)
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

      val form = controller.agentFastTrackForm.fill(formData)
      val result = await(controller.submitDetails(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")

      verifyAuthoriseAttempt()
      await(testFastTrackCache.fetch()).get shouldBe formData
    }
  }

  def verifyAgentClientInvitationSubmittedEvent(
    arn: String,
    clientId: String,
    clientIdType: String,
    result: String,
    service: String): Unit =
    verifyAuditRequestSent(
      1,
      AgentInvitationEvent.AgentClientAuthorisationRequestCreated,
      detail = Map(
        "factCheck"            -> result,
        "agentReferenceNumber" -> arn,
        "clientIdType"         -> clientIdType,
        "clientId"             -> clientId,
        "service"              -> service
      ),
      tags = Map(
        "transactionName" -> "Agent client service authorisation request created"
      )
    )
}
