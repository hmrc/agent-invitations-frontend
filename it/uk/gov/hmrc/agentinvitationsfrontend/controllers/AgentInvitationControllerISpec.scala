package uk.gov.hmrc.agentinvitationsfrontend.controllers

/*
 * Copyright 2017 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import org.joda.time.LocalDate
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.audit.AgentInvitationEvent
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentInvitationUserInput, AgentInvitationVatForm, FastTrackInvitation}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.auth.core.AuthorisationException
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.logging.SessionId
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

class AgentInvitationControllerISpec extends BaseISpec {

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

  "GET /agents/" should {
    "redirect to /agent/select-service" in {
      val result = controller.agentsRoot(FakeRequest())
      status(result) shouldBe 303
      val timeout = 2.seconds
      redirectLocation(result)(timeout).get should include("/agents/select-service")
    }
  }

  "GET /agents/select-service" should {
    val request = FakeRequest("GET", "/agents/select-service")
    val selectService = controller.selectService()

    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
      val result = selectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result,
        htmlEscapedMessage("generic.title", htmlEscapedMessage("select-service.header"), htmlEscapedMessage("title.suffix.agents")),
        htmlEscapedMessage("select-service.header"),
        htmlEscapedMessage("select-service.itsa"),
        htmlEscapedMessage("select-service.personal-income-viewer"),
        htmlEscapedMessage("select-service.vat"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedEndpoint(request, selectService)
  }

  "POST /agents/select-service" should {
    val request = FakeRequest("POST", "/agents/select-service")
    val submitService = controller.submitService()

    "return 303 for authorised Agent with valid ITSA service, redirect to enter nino page" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceITSA), None, None, None, None))
      val serviceForm = agentInvitationServiceForm.fill(AgentInvitationUserInput(serviceITSA, None, None))
      val result = submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/enter-nino")
      verifyAuthoriseAttempt()
    }

    "return 303 for authorised Agent with valid Personal Income Record service, redirect to enter nino" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(servicePIR), None, None, None, None))
      val serviceForm = agentInvitationServiceForm.fill(AgentInvitationUserInput(servicePIR, None, None))
      val result = submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/enter-nino")
      verifyAuthoriseAttempt()
    }

    "return 303 for authorised Agent with valid VAT service, redirect to enter vrn" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceVAT), None, None, None, None))
      val serviceForm = agentInvitationServiceForm.fill(AgentInvitationUserInput(serviceVAT, None, None))
      val result = submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/enter-vrn")
      verifyAuthoriseAttempt()
    }

    "return 200 for authorised Agent with no selected service and show error on the page" in {
      val result = submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody("service" -> ""), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage(
        "generic.title", htmlEscapedMessage("select-service.header"), htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("select-service.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.service.required"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }
    behave like anAuthorisedEndpoint(request, submitService)
  }

  "GET /agents/enter-vrn" should {
    val request = FakeRequest("GET", "/agents/enter-vrn")
    val showVrnForm = controller.showVrnForm()

    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceVAT), None, None, None, None))
      val result = showVrnForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage(
        "generic.title", hasMessage("enter-vrn.header"), htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-vrn.header"))
      checkHasAgentSignOutLink(result)

      verifyAuthoriseAttempt()
    }

    behave like noKeyStoreCacheFound(request, showVrnForm)
    behave like anAuthorisedEndpoint(request, showVrnForm)
  }

  "POST /agents/enter-vrn" should {
    val request = FakeRequest("POST", "/agents/enter-vrn")
    val submitVrn = controller.submitVrn()

    "return 303 redirect to enter-vat-reg-date for authorised Agent with valid vrn and redirected to the registration date known fact page" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceVAT), Some("vrn"), Some(validVrn97.value), None, None))
      val form = agentInvitationVrnForm.fill(AgentInvitationVatForm(serviceVAT, Some(validVrn97), None))
      val result = submitVrn(authorisedAsValidAgent(request.withFormUrlEncodedBody(form.data.toSeq: _*), arn.value))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.showVatRegistrationDateForm().url

      verifyAuthoriseAttempt()
    }

    "return 200 for authorised Agent with no vrn submitted and redisplay form with error message" in {
      val form = agentInvitationVrnForm
      val formData = Map("service" -> serviceVAT, "clientIdentifier" -> "", "postcode" -> "")
      val result = submitVrn(authorisedAsValidAgent(request
        .withFormUrlEncodedBody(form.bind(formData).data.toSeq: _*), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage(
        "generic.title", hasMessage("enter-vrn.header"), htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.vrn.required"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
      verifyAuditRequestNotSent(AgentInvitationEvent.AgentClientAuthorisationRequestCreated)
    }

    "return 200 for authorised Agent with invalid vrn less than 9 characters and redisplay form with error message" in {
      val form = agentInvitationVrnForm
      val formData = Map("service" -> serviceVAT, "clientIdentifier" -> validNino.value, "postcode" -> "")
      val result = submitVrn(authorisedAsValidAgent(request
        .withFormUrlEncodedBody(form.bind(formData).data.toSeq: _*), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage(
        "generic.title", hasMessage("enter-vrn.header"), htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-vrn.regex-failure"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
      verifyAuditRequestNotSent(AgentInvitationEvent.AgentClientAuthorisationRequestCreated)
    }

    "return 200 for authorised Agent with invalid vrn and redisplay form with error message" in {
      val form = agentInvitationVrnForm
      val formData = Map("service" -> serviceVAT, "clientIdentifier" -> invalidVrn.value, "postcode" -> "")
      val result = submitVrn(authorisedAsValidAgent(request
        .withFormUrlEncodedBody(form.bind(formData).data.toSeq: _*), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage(
        "generic.title", hasMessage("enter-vrn.header"), htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-vrn.checksum-failure"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
      verifyAuditRequestNotSent(AgentInvitationEvent.AgentClientAuthorisationRequestCreated)
    }

    behave like anAuthorisedEndpoint(request, submitVrn)
  }

  "GET /agents/enter-nino" should {
    val request = FakeRequest("GET", "/agents/enter-nino")
    val showNinoForm = controller.showNinoForm()

    "return 200 for an Agent with HMRC-AS-AGENT enrolment for ITSA service" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceITSA), None, None, None, None))
      val result = showNinoForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, hasMessage("generic.title", htmlEscapedMessage("enter-nino.header"), htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.header"))
      checkHasAgentSignOutLink(result)

      verifyAuthoriseAttempt()
    }

    "return 200 for an Agent with HMRC-AS-AGENT enrolment for PERSONAL-INCOME-RECORD service" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(servicePIR), None, None, None, None))
      val result = showNinoForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, hasMessage("generic.title", htmlEscapedMessage("enter-nino.header"), htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.header"))
      checkHasAgentSignOutLink(result)

      verifyAuthoriseAttempt()
    }

    "return 303 redirect to select-service for an Agent with HMRC-AS-AGENT enrolment when service is not available" in {
      val result = showNinoForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/select-service")
    }

    behave like anAuthorisedEndpoint(request, showNinoForm)
    behave like noKeyStoreCacheFound(request, showNinoForm)

  }

  "POST /agents/enter-nino" should {
    val request = FakeRequest("POST", "/agents/enter-nino")
    val submitNino = controller.submitNino()

    "return 303 for authorised Agent with valid nino and service HMRC-MTD-IT, redirected to enter postcode page" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceITSA), Some("ni"), Some(validNino.value), None, None))
      val ninoForm = agentInvitationNinoForm.fill(AgentInvitationUserInput(serviceITSA, Some(validNino), None))
      val result = submitNino(authorisedAsValidAgent(request.withFormUrlEncodedBody(ninoForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/enter-postcode")
    }

    "return 303 for authorised Agent with valid nino and Personal Income Record service, redirect to invitation sent page" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(servicePIR), Some("ni"), Some(validNino.value), None, None))
      createInvitationStubForNoKnownFacts(arn, validNino.value, invitationIdPIR, validNino.value, "ni", servicePIR, "NI")
      getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, "NI", "Pending")
      val ninoForm = agentInvitationNinoForm.fill(AgentInvitationUserInput(servicePIR, Some(validNino), None))
      val result = submitNino(authorisedAsValidAgent(request.withFormUrlEncodedBody(ninoForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value, validNino.value, "ni", "Not Required", servicePIR)
    }

    "return 303 for authorised Agent with valid nino that has spaces in between and Personal Income Record service, redirect to invitation sent page" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(servicePIR), Some("ni"), Some(validNinoSpace.value), None, None))
      createInvitationStubForNoKnownFacts(arn, validNino.value, invitationIdPIR, validNino.value, "ni", servicePIR, "NI")
      getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, "NI", "Pending")

      val ninoForm = agentInvitationNinoForm.fill(AgentInvitationUserInput(servicePIR, Some(validNinoSpace), None))
      val result = submitNino(authorisedAsValidAgent(request.withFormUrlEncodedBody(ninoForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value, validNino.value, "ni", "Not Required", servicePIR)
    }

    "return 200 for authorised Agent with an empty nino and show errors on the page" in {
      val result = submitNino(authorisedAsValidAgent(request.withFormUrlEncodedBody("clientIdentifier" -> "", "postcode" -> ""), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, hasMessage("generic.title", htmlEscapedMessage("enter-nino.header"), htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.nino.required"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "return 200 for authorised Agent with an invalid nino and show errors on the page" in {
      val result = submitNino(authorisedAsValidAgent(request.withFormUrlEncodedBody("clientIdentifier" -> "AB", "postcode" -> ""), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, hasMessage("generic.title", htmlEscapedMessage("enter-nino.header"), htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.invalid-format"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "return 303 for an authorised Agent if service is not found" in {
      val ninoForm = agentInvitationNinoForm.fill(AgentInvitationUserInput("", Some(validNino), None))
      val result = submitNino(authorisedAsValidAgent(request.withFormUrlEncodedBody(ninoForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.selectService().url)
      verifyAuthoriseAttempt()
    }
  }

  "GET /agents/enter-vat-registration-date" should {
    val request = FakeRequest("GET", "/agents/enter-vat-registration-date")
    val showVatRegistrationDateForm = controller.showVatRegistrationDateForm()

    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceVAT), Some("vrn"), Some(validVrn97.value), None, None))
      val result = showVatRegistrationDateForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("generic.title", htmlEscapedMessage("enter-vat-registration-date.header"), htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-vat-registration-date.header"))
      checkHasAgentSignOutLink(result)

      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedEndpoint(request, showVatRegistrationDateForm)
    behave like noKeyStoreCacheFound(request, showVatRegistrationDateForm)

  }

  "POST /agents/enter-vat-registration-date" should {
    val request = FakeRequest("POST", "/agents/enter-vrn")
    val submitVatRegistrationDate = controller.submitVatRegistrationDate()

    "return 303 for authorised Agent with valid vrn and known fact check pass to invitation sent page" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceVAT), Some("vrn"), Some(validVrn97.value), None, validRegDateForVrn97))
      createInvitationStubForNoKnownFacts(arn, validVrn97.value, invitationIdVAT, validVrn97.value, "vrn", serviceVAT, identifierVAT)
      getInvitationStub(arn, validVrn97.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      checkVatRegisteredClientStub(validVrn97, LocalDate.parse("2007-07-07"), 204)

      val form = agentInvitationVatRegistrationDateForm.fill(AgentInvitationVatForm(serviceVAT, Some(validVrn97), validRegDateForVrn97))
      val result = submitVatRegistrationDate(authorisedAsValidAgent(request.withFormUrlEncodedBody(form.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      header("Set-Cookie", result) shouldBe defined
      header("Set-Cookie", result).get should include(s"invitationId=${invitationIdVAT.value}")
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")

      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value, validVrn97.value, "vrn", "Success", serviceVAT)
      verifyCheckVatRegisteredClientStubAttempt(validVrn97, LocalDate.parse("2007-07-07"))
    }

    "return 303 when the user supplied VAT registration date doesn't not match our records" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceVAT), Some("vrn"), Some(validVrn97.value), None, validRegDateForVrn97))
      checkVatRegisteredClientStub(validVrn97, LocalDate.parse("2007-07-07"), 403)

      val form = agentInvitationVatRegistrationDateForm.fill(AgentInvitationVatForm(serviceVAT, Some(validVrn97), validRegDateForVrn97))
      val result = submitVatRegistrationDate(authorisedAsValidAgent(request.withFormUrlEncodedBody(form.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      header("Set-Cookie", result) shouldBe None
      redirectLocation(result) shouldBe Some("/invitations/agents/not-matched")
      verifyCheckVatRegisteredClientStubAttempt(validVrn97, LocalDate.parse("2007-07-07"))
    }

    "return exception when create invitation fails" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceVAT), Some("vrn"), Some(validVrn97.value), None, validRegDateForVrn97))
      checkVatRegisteredClientStub(validVrn97, LocalDate.parse("2007-07-07"), 204)
      failedCreateInvitation(arn)

      val form = agentInvitationVatRegistrationDateForm.fill(AgentInvitationVatForm(serviceVAT, Some(validVrn97), Some("2007-07-07")))
      val result = submitVatRegistrationDate(authorisedAsValidAgent(request.withFormUrlEncodedBody(form.data.toSeq: _*), arn.value))

      an[BadRequestException] should be thrownBy await(result)
      verifyAgentClientInvitationSubmittedEvent(arn.value, validVrn97.value, "vrn", "Fail", serviceVAT)
      verifyCheckVatRegisteredClientStubAttempt(validVrn97, LocalDate.parse("2007-07-07"))
    }

    "return 303 when client is not registered for VAT" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceVAT), Some("vrn"), Some(validVrn97.value), None, validRegDateForVrn97))
      checkVatRegisteredClientStub(validVrn97, LocalDate.parse("2007-07-20"), 404)

      val form = agentInvitationVatRegistrationDateForm.fill(AgentInvitationVatForm(serviceVAT, Some(validVrn97), Some("2007-07-20")))
      val result = submitVatRegistrationDate(authorisedAsValidAgent(request.withFormUrlEncodedBody(form.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      header("Set-Cookie", result) shouldBe None
      redirectLocation(result) shouldBe Some("/invitations/agents/not-enrolled")
      verifyCheckVatRegisteredClientStubAttempt(validVrn97, LocalDate.parse("2007-07-20"))
    }
  }

  "GET /agents/enter-postcode" should {
    val request = FakeRequest("GET", "/agents/enter-postcode")
    val showPostcodeForm = controller.showPostcodeForm()

    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceITSA), Some("ni"), Some(validNino.value), None, None))
      val result = showPostcodeForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("generic.title", htmlEscapedMessage("enter-postcode.header"), htmlEscapedMessage("title.suffix.agents")))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedEndpoint(request, showPostcodeForm)
    behave like noKeyStoreCacheFound(request, showPostcodeForm)

  }

  "POST /agents/enter-postcode" should {
    val request = FakeRequest("POST", "/agents/enter-postcode")
    val submitPostcode = controller.submitPostcode()

    "return 303 for authorised Agent with valid nino and redirected to invitations-sent page" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceITSA), Some("ni"), Some(validNino.value), None, None))
      createInvitationStubWithKnownFacts(arn, mtdItId.value, invitationIdITSA, validNino.value, serviceITSA, "MTDITID", validPostcode)
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, "MTDITID", "Pending")

      val form = agentInvitationPostCodeForm.fill(AgentInvitationUserInput(serviceITSA, Some(validNino), Some(validPostcode)))
      val result = submitPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody(form.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      header("Set-Cookie", result) shouldBe defined
      header("Set-Cookie", result).get should include("invitationId=ABERULMHCKKW3")

      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value, validNino.value, "ni", "Success", serviceITSA)
    }

    "return 200 for authorised Agent with empty postcode and redisplay form with error message" in {
      val form = agentInvitationPostCodeForm
      val ninoData = Map("nino" -> validNino.value, "postcode" -> "")
      val result = submitPostcode(authorisedAsValidAgent(request
        .withFormUrlEncodedBody(form.bind(ninoData).data.toSeq: _*), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("generic.title", htmlEscapedMessage("enter-postcode.header"), htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.postcode.required"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
      verifyAuditRequestNotSent(AgentInvitationEvent.AgentClientAuthorisationRequestCreated)
    }

    "return 200 for authorised Agent with invalid postcode and redisplay form with error message" in {
      val form = agentInvitationPostCodeForm
      val ninoData = Map("nino" -> validNino.value, "postcode" -> "AB")
      val result = submitPostcode(authorisedAsValidAgent(request
        .withFormUrlEncodedBody(form.bind(ninoData).data.toSeq: _*), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("generic.title", htmlEscapedMessage("enter-postcode.header"), htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-postcode.invalid-format"))
      checkHasAgentSignOutLink(result)

      verifyAuthoriseAttempt()
      verifyAuditRequestNotSent(AgentInvitationEvent.AgentClientAuthorisationRequestCreated)
    }

    "return 303 for authorised Agent when client registration not found " in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceITSA), Some("ni"), Some(validNino.value), None, None))
      failedCreateInvitationForNotEnrolled(arn)

      val form = agentInvitationPostCodeForm.fill(AgentInvitationUserInput(serviceITSA, Some(validNino), Some("AB101AB")))
      val result = submitPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody(form.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/not-enrolled")

      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value, validNino.value, "ni", "Fail", serviceITSA)
      await(fastTrackKeyStoreCache.fetchAndGetEntry()).get shouldBe FastTrackInvitation(Some(serviceITSA), None, None, None, None)
    }

    "return 303 for authorised Agent when postcode does not match " in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceITSA), Some("ni"), Some(validNino.value), None, None))
      failedCreateInvitationFoInvalidPostcode(arn)

      val form = agentInvitationPostCodeForm.fill(AgentInvitationUserInput(serviceITSA, Some(validNino), Some("AB101AB")))
      val result = submitPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody(form.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/not-matched")

      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value, validNino.value, "ni", "Fail", serviceITSA)
      await(fastTrackKeyStoreCache.fetchAndGetEntry()).get shouldBe FastTrackInvitation(Some(serviceITSA), None, None, None, None)
    }

    "return exception when create invitation fails" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceITSA), Some("ni"), Some(validNino.value), None, None))
      failedCreateInvitation(arn)

      val form = agentInvitationPostCodeForm.fill(AgentInvitationUserInput(serviceITSA, Some(validNino), Some("AB101AB")))
      val result = submitPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody(form.data.toSeq: _*), arn.value))

      an[BadRequestException] should be thrownBy await(result)
      verifyAgentClientInvitationSubmittedEvent(arn.value, validNino.value, "ni", "Fail", serviceITSA)
    }

    behave like anAuthorisedEndpoint(request, submitPostcode)
  }

  "GET /agents/invitation-sent" should {
    val request = FakeRequest("GET", "/agents/invitation-sent")
    val invitationSent = controller.invitationSent()

    "return 200 for authorised Agent with valid postcode and redirected to Confirm Invitation Page (secureFlag = false)" in {
      val result = invitationSent(authorisedAsValidAgent(request.withSession("invitationId" -> "ABERULMHCKKW3", "deadline" -> "27 December 2017"), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("generic.title", htmlEscapedMessage("invitation-sent-link.header"), htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.header"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.description.advice.pt1", "27 December 2017"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.description.advice.pt2"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.description.advice.pt3"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.button"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage(s"$wireMockBaseUrlAsString${routes.ClientsInvitationController.start(invitationIdITSA)}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkInviteSentExitSurveyAgentSignOutLink(result)

      verifyAuthoriseAttempt()
      await(fastTrackKeyStoreCache.fetchAndGetEntry()).get shouldBe FastTrackInvitation.newInstance

    }

    "return exception when no invitation id and deadline found" in {
      val result = invitationSent(authorisedAsValidAgent(request, arn.value))

      an[RuntimeException] should be thrownBy await(result)
    }

    behave like anAuthorisedEndpoint(request, invitationSent)
  }

  "GET /agents/not-enrolled" should {
    val request = FakeRequest("GET", "/agents/not-enrolled")
    val notEnrolled = controller.notEnrolled()

    "return 403 for authorised Agent who submitted known facts of an not enrolled client" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceITSA), None, None, None, None))
      val ninoForm = agentInvitationNinoForm.fill(AgentInvitationUserInput(serviceITSA, None, None))
      val result = notEnrolled(authorisedAsValidAgent(request.withFormUrlEncodedBody(ninoForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 403
      checkHtmlResultWithBodyText(result, htmlEscapedMessage(
        "generic.title", htmlEscapedMessage("not-enrolled.itsa.header"), htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-enrolled.itsa.description"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "return 403 for authorised Agent who submitted known facts of an not enrolled VAT client" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some(serviceVAT), None, None, None, None))
      val vrnForm = agentInvitationVrnForm.fill(AgentInvitationVatForm(serviceVAT, None, None))
      val result = notEnrolled(authorisedAsValidAgent(request.withFormUrlEncodedBody(vrnForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 403
      checkHtmlResultWithBodyText(result, htmlEscapedMessage(
        "generic.title", htmlEscapedMessage("not-enrolled.vat.header"), htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-enrolled.vat.description"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "return 5xx for Unsupported service" in {
      fastTrackKeyStoreCache.save(FastTrackInvitation(Some("UNSUPPORTED"), None, None, None, None))
      val unsupportedForm = agentInvitationVrnForm.fill(AgentInvitationVatForm("UNSUPPORTED", None, None))

      intercept[Exception] {
        await(notEnrolled(authorisedAsValidAgent(request.withFormUrlEncodedBody(unsupportedForm.data.toSeq: _*), arn.value)))
      }.getMessage shouldBe "Unsupported Service"
    }

    "return 5xx when there is nothing in the cache" in {
      intercept[Exception] {
        await(notEnrolled(authorisedAsValidAgent(request, arn.value)))
      }.getMessage shouldBe "Empty Cache"
    }

    behave like anAuthorisedEndpoint(request, notEnrolled)
  }

  "GET /agents/not-matched" should {
    val request = FakeRequest("GET", "/agents/not-matched")
    val notMatched = controller.notMatched()

    "return 403 for authorised Agent who submitted not matching known facts" in {
      val result = notMatched(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 403
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("generic.title", htmlEscapedMessage("not-matched.header"), htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-matched.description"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedEndpoint(request, notMatched)
  }

  def checkHasAgentSignOutLink(result: Future[Result]) = {
    checkHtmlResultWithBodyText(result, htmlEscapedMessage("common.sign-out"))
    val asAcHomepageExternalUrl = wireMockBaseUrlAsString
    val continueUrl = URLEncoder.encode(s"$asAcHomepageExternalUrl/agent-services-account", StandardCharsets.UTF_8.name())
    checkHtmlResultWithBodyText(result, s"$companyAuthUrl$companyAuthSignOutPath?continue=$continueUrl")
  }

  def checkInviteSentExitSurveyAgentSignOutLink(result: Future[Result]) = {
    checkHtmlResultWithBodyText(result, htmlEscapedMessage("common.sign-out"))
    val continueUrl = URLEncoder.encode(agentFeedbackSurveyURNWithOriginToken, StandardCharsets.UTF_8.name())
    checkHtmlResultWithBodyText(result, continueUrl)
  }

  def anAuthorisedEndpoint(request: FakeRequest[AnyContentAsEmpty.type], action: Action[AnyContent]) = {

    "return 303 for an Agent with no enrolments and redirected to Login Page" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = await(action(authenticatedClient(request, Enrolment("", "", ""))))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("someSubscriptionExternalUrl")
      verifyAuthoriseAttempt()
    }

    "return 303 for no Agent and redirected to Login Page" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = await(action(authenticatedClient(request, Enrolment("OtherEnrolment", "Key", "Value"))))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("someSubscriptionExternalUrl")
      verifyAuthoriseAttempt()
    }

    "return 303 for not logged in user and redirected to Login Page" in {
      givenUnauthorisedWith("MissingBearerToken")
      an[AuthorisationException] shouldBe thrownBy {
        await(action(request))
      }
      verifyAuthoriseAttempt()
    }
  }

  def noKeyStoreCacheFound(request: FakeRequest[AnyContentAsEmpty.type], action: Action[AnyContent]) = {
    "return 303, redirect to select-service when no keystore cache is found" in {
      val result = await(action(authorisedAsValidAgent(request, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.selectService().url)
    }
  }

  def verifyAgentClientInvitationSubmittedEvent(arn: String, clientId: String, clientIdType: String, result: String, service: String): Unit = {
    verifyAuditRequestSent(1, AgentInvitationEvent.AgentClientAuthorisationRequestCreated,
      detail = Map(
        "factCheck" -> result,
        "agentReferenceNumber" -> arn,
        "clientIdType" -> clientIdType,
        "clientId" -> clientId,
        "service" -> service
      ),
      tags = Map(
        "transactionName" -> "Agent client service authorisation request created"
      )
    )
  }
}
