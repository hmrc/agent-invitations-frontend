package uk.gov.hmrc.agentinvitationsfrontend.controllers

import org.joda.time.LocalDate
import play.api.test.FakeRequest
import play.api.test.Helpers.redirectLocation
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentInvitationNinoForm, AgentInvitationVatForm, FastTrackInvitation}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId
import play.api.test.Helpers._

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
  val invitationIdITSA = InvitationId("ABERULMHCKKW3")
  val invitationIdPIR = InvitationId("B9SCS2T4NZBAX")

  val invitationIdVAT = InvitationId("CZTW1KY6RTAAT")
  val serviceVAT = "HMRC-MTD-VAT"
  val identifierVAT = "VRN"
  val validVrn97 = Vrn("101747696")
  val invalidVrn = Vrn("101747692")
  val validRegDateForVrn97 = Some("2007-07-07")
  val validVrn9755 = Vrn("101747641")
  val agentFeedbackSurveyURNWithOriginToken = "/feedback-survey/?origin=INVITAGENT"

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  //TODO - Along with the other tests, could be split up by service? Note: This test is based on fast-track scenarios

  "POST /agents/select-service" should {
    val request = FakeRequest("POST", "/agents/select-service")
    val submitService = controller.submitService()

    "return 303 for authorised Agent who supplied valid Nino and Known Fact, then selected ITSA, redirect to invitation-sent" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(None, Some("ni"), Some(validNino.value), Some(validPostcode), None))
      createInvitationStubWithKnownFacts(arn, mtdItId.value, invitationIdITSA, validNino.value, serviceITSA, "NI", validPostcode)
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, "NI", "Pending")
      val serviceForm = agentInvitationServiceForm.fill(AgentInvitationNinoForm(serviceITSA, None, None))
      val result = submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      verifyAuthoriseAttempt()
    }

    "return 303 for authorised Agent who supplied valid Nino then selected IRV, redirect to invitation-sent" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(None, Some("ni"), Some(validNino.value), None, None))
      createInvitationStubForNoKnownFacts(arn, validNino.value, invitationIdPIR, validNino.value, "ni", servicePIR, "NI")
      getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, "NI", "Pending")
      val serviceForm = agentInvitationServiceForm.fill(AgentInvitationNinoForm(servicePIR, None, None))
      val result = submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      verifyAuthoriseAttempt()
    }

    "return 303 for authorised Agent with valid VAT Information and selected VAT, redirect to invitation-sent" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(None, Some("vrn"), Some(validVrn97.value), None, validRegDateForVrn97))
      createInvitationStubForNoKnownFacts(arn, validVrn97.value, invitationIdVAT, validVrn97.value, "vrn", serviceVAT, identifierVAT)
      getInvitationStub(arn, validVrn97.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      checkVatRegisteredClientStub(validVrn97, LocalDate.parse("2007-07-07"), 204)
      val serviceForm = agentInvitationServiceForm.fill(AgentInvitationNinoForm(serviceVAT, None, None))
      val result = submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      verifyAuthoriseAttempt()
    }
  }

  "POST /agents/enter-nino" should {
    val request = FakeRequest("POST", "/agents/enter-nino")
    val submitNino = controller.submitNino()

    "return 303 for authorised Agent who supplied valid ITSA service and KnownFact, then entered valid nino, redirect to invitation-sent" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceITSA), None, None, Some(validPostcode), None))
      createInvitationStubWithKnownFacts(arn, mtdItId.value, invitationIdITSA, validNino.value, serviceITSA, "NI", validPostcode)
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, "NI", "Pending")
      val ninoForm = agentInvitationNinoForm.fill(AgentInvitationNinoForm(serviceITSA, Some(validNino), None))
      val result = submitNino(authorisedAsValidAgent(request.withFormUrlEncodedBody(ninoForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      verifyAuthoriseAttempt()
    }

    "return 303 for authorised Agent who supplied valid IRV service, then entered valid nino, redirect to invitation-sent" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(servicePIR), None, None, None, None))
      createInvitationStubForNoKnownFacts(arn, validNino.value, invitationIdPIR, validNino.value, "ni", servicePIR, "NI")
      getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, "NI", "Pending")
      val ninoForm = agentInvitationNinoForm.fill(AgentInvitationNinoForm(servicePIR, Some(validNino), None))
      val result = submitNino(authorisedAsValidAgent(request.withFormUrlEncodedBody(ninoForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      verifyAuthoriseAttempt()
    }
  }

  "POST /agents/enter-vrn" should {
    val request = FakeRequest("POST", "/agents/enter-vrn")
    val submitVrn = controller.submitVrn()

    "return 303 for authorised Agent who supplied valid VAT service and KnownFact, then entered valid vrn, redirect to invitation-sent" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceVAT), None, None, None, validRegDateForVrn97))
      createInvitationStubForNoKnownFacts(arn, validVrn97.value, invitationIdVAT, validVrn97.value, "vrn", serviceVAT, "VRN")
      getInvitationStub(arn, validVrn97.value, invitationIdVAT, serviceVAT, "VRN", "Pending")
      checkVatRegisteredClientStub(validVrn97, LocalDate.parse("2007-07-07"), 204)
      val vrnForm = agentInvitationVrnForm.fill(AgentInvitationVatForm(serviceVAT, Some(validVrn97), None))
      val result = submitVrn(authorisedAsValidAgent(request.withFormUrlEncodedBody(vrnForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      verifyAuthoriseAttempt()
    }
  }

  "POST /agents/enter-vat-registration-date" should {
    val request = FakeRequest("POST", "/agents/enter-vat-registration-date")
    val submitVatRegDate = controller.submitVatRegistrationDate()

    "return 303 for authorised Agent who supplied valid VAT service and KnownFact, then entered valid vrn, redirect to invitation-sent" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceVAT), Some("vrn"), Some(validVrn97.value), None, None))
      createInvitationStubForNoKnownFacts(arn, validVrn97.value, invitationIdVAT, validVrn97.value, "vrn", serviceVAT, "VRN")
      getInvitationStub(arn, validVrn97.value, invitationIdVAT, serviceVAT, "VRN", "Pending")
      checkVatRegisteredClientStub(validVrn97, LocalDate.parse("2007-07-07"), 204)
      val vatRegDateForm = agentInvitationVatRegistrationDateForm.fill(AgentInvitationVatForm(serviceVAT, Some(validVrn97), validRegDateForVrn97))
      val result = submitVatRegDate(authorisedAsValidAgent(request.withFormUrlEncodedBody(vatRegDateForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      verifyAuthoriseAttempt()
    }
  }

  "POST /agents/enter-postcode" should {
    val request = FakeRequest("POST", "/agents/enter-postcode")
    val submitPostcode = controller.submitPostcode()

    "return 303 for authorised Agent who supplied valid ITSA service and KnownFact, then entered valid nino, redirect to invitation-sent" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceITSA), Some("ni"), Some(validNino.value), None, None))
      createInvitationStubWithKnownFacts(arn, mtdItId.value, invitationIdITSA, validNino.value, serviceITSA, "NI", validPostcode)
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, "NI", "Pending")
      val ninoForm = agentInvitationPostCodeForm.fill(AgentInvitationNinoForm(serviceITSA, Some(validNino), Some(validPostcode)))
      val result = submitPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody(ninoForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      verifyAuthoriseAttempt()
    }

  }

  "POST /agents/fast-track" should {
    val request = FakeRequest("POST", "/agents/fast-track")
    val fastTrack = controller.agentFastTrack()

    "return 303 invitation-sent if service calling fast-track is correct for ITSA" in {
      val formData = FastTrackInvitation(Some(serviceITSA), Some("ni"), Some(validNino.value), Some(validPostcode), None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      createInvitationStubWithKnownFacts(arn, mtdItId.value, invitationIdITSA, validNino.value, serviceITSA, "MTDITID", validPostcode)
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, "MTDITID", "Pending")
      val result = fastTrack(authorisedAsValidAgent(request, arn.value)
        .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.invitationSent().url
    }

    "return 303 invitation-sent if service calling fast-track is correct for VAT" in {
      val formData = FastTrackInvitation(Some(serviceVAT), Some("vrn"), Some(validVrn97.value), None, validRegDateForVrn97)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      createInvitationStubForNoKnownFacts(arn, validVrn97.value, invitationIdVAT, validVrn97.value, "vrn", serviceVAT, "VRN")
      getInvitationStub(arn, validVrn97.value, invitationIdVAT, serviceVAT, "VRN", "Pending")
      checkVatRegisteredClientStub(validVrn97, LocalDate.parse("2007-07-07"), 204)
      val result = fastTrack(authorisedAsValidAgent(request, arn.value)
        .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.invitationSent().url
    }

    "return 303 invitation-sent if service calling fast-track is correct for IRV" in {
      val formData = FastTrackInvitation(Some(servicePIR), Some("ni"), Some(validNino.value), None, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      createInvitationStubForNoKnownFacts(arn, validNino.value, invitationIdPIR, validNino.value, "ni", servicePIR, "NI")
      getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, "NI", "Pending")
      val result = fastTrack(authorisedAsValidAgent(request, arn.value)
        .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.invitationSent().url
    }

    "return 303 select-service if service calling fast-track does not have supported service in payload" in {
      val formData = FastTrackInvitation(Some("INVALID_SERVICE"), None, None, None, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(authorisedAsValidAgent(request, arn.value)
        .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.selectService().url
    }

    "return 303 enter-nino if service calling fast-track for ITSA contains invalid nino" in {
      val formData = FastTrackInvitation(Some(serviceITSA), Some("ni"), Some("INVALID_NINO"), None, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(authorisedAsValidAgent(request, arn.value)
        .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showNinoForm().url
    }

    "return 303 enter-nino if service calling fast-track for PIR contains invalid nino" in {
      val formData = FastTrackInvitation(Some(servicePIR), Some("ni"), Some("INVALID_NINO"), None, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(authorisedAsValidAgent(request, arn.value)
        .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showNinoForm().url
    }

    "return 303 enter-nino if service calling fast-track for ITSA does not contain nino" in {
      val formData = FastTrackInvitation(Some(serviceITSA), None, None, None, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(authorisedAsValidAgent(request, arn.value)
        .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showNinoForm().url
    }

    "return 303 enter-vrn if service calling fast-track for VAT contains invalid vrn" in {
      val formData =FastTrackInvitation(Some(serviceVAT), None, Some("INVALID_VRN"), None, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(authorisedAsValidAgent(request, arn.value)
        .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showVrnForm().url
    }

    "return 303 enter-vrn if service calling fast-track for VAT does not contain vrn" in {
      val formData = FastTrackInvitation(Some(serviceVAT), None, None, None, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(authorisedAsValidAgent(request, arn.value)
        .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showVrnForm().url
    }

    "return 303 enter-postcode if service calling fast-track for does not contain postcode for ITSA" in {
      val formData = FastTrackInvitation(Some(serviceITSA), Some("ni"), Some(validNino.value), None, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(authorisedAsValidAgent(request, arn.value)
        .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showPostcodeForm().url
    }

    "return 303 enter-vat-reg-date if service calling fast-track does not contain vat-reg-date for VAT" in {
      val formData = FastTrackInvitation(Some(serviceVAT), Some("vrn"), Some(validVrn97.value), None, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(authorisedAsValidAgent(request, arn.value)
        .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showVatRegistrationDateForm().url
    }

    "return 303 select-service if there is no service but all other fields are valid for ITSA" in {
      val formData = FastTrackInvitation(None, Some("ni"), Some(validNino.value), Some(validPostcode), None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(authorisedAsValidAgent(request, arn.value)
        .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.selectService().url
    }

    "return 303 select-service if there is no service but all other fields are valid for IRV" in {
      val formData = FastTrackInvitation(None, Some("ni"), Some(validNino.value), None, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(authorisedAsValidAgent(request, arn.value)
        .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.selectService().url
    }

    "return 303 select-service if there is no service but all other fields are valid for VAT" in {
      val formData = FastTrackInvitation(None, Some("vrn"), Some(validVrn97.value), None, validRegDateForVrn97)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(authorisedAsValidAgent(request, arn.value)
        .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.selectService().url
    }
  }

}
