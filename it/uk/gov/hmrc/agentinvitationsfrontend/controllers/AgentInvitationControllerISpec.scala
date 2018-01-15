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

import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.audit.AgentInvitationEvent
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentInvitationUserInput
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.auth.core.{AuthorisationException, InsufficientEnrolments}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.BadRequestException

import scala.concurrent.Future
import scala.concurrent.duration._

class AgentInvitationControllerISpec extends BaseISpec {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  val arn = Arn("TARN0000001")
  val mtdItId = MtdItId("ABCDEF123456789")
  private val validNino = Nino("AB123456A")
  val serviceITSA = "HMRC-MTD-IT"
  val servicePIR = "PERSONAL-INCOME-RECORD"
  val validPostcode = "BN12 6BX"
  val invitationIdITSA = InvitationId("ABERULMHCKKW3")
  val invitationIdPIR = InvitationId("B9SCS2T4NZBAX")

  val invitationIdVAT = InvitationId("CZTW1KY6RTAAT")
  val serviceVAT = "HMRC-MTD-VAT"
  val identifierVAT = "MTDVATID"
  val validVrn97 = Vrn("101747696")
  val validVrn9755 = Vrn("101747641")

  "GET /agents/" should {
    "redirect to /agent/select-service" in {
      val result = controller.agentsRoot(FakeRequest())
      status(result) shouldBe 303
      val timeout = 2.seconds
      redirectLocation(result)(timeout).get should include("/agents/select-service")
    }
  }

  "GET /agents/enter-vrn" should {
    val request = FakeRequest("GET", "/agents/enter-vrn")
    val showVrnForm = controller.showVrnForm()

    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
      val result = showVrnForm(authorisedAsValidAgent(request.withSession("service" -> serviceVAT), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-vrn.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-vrn.header"))
      checkHasAgentSignOutLink(result)

      verifyAuthoriseAttempt()
    }

    "return 303 for an Agent with HMRC-AS-AGENT enrolment when service is not available in session" in {
      val result = showVrnForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/select-service")
    }

    behave like anAuthorisedEndpoint(request, showVrnForm)
  }

  "POST /agents/enter-vrn" should {
    val request = FakeRequest("POST", "/agents/enter-vrn")
    val submitVrn = controller.submitVrn()

    "return 303 for authorised Agent with valid vrn and redirected to invitations-sent page" in {
      createInvitationStubForNoKnownFacts(arn, validVrn97.value, invitationIdVAT, validVrn97.value, "vrn", serviceVAT, identifierVAT)
      getInvitationStub(arn, validVrn97.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")

      val form = agentInvitationVrnForm.fill(AgentInvitationUserInput(serviceVAT, Some(validVrn97), None))
      val result = submitVrn(authorisedAsValidAgent(request.withFormUrlEncodedBody(form.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      header("Set-Cookie", result) shouldBe defined
      header("Set-Cookie", result).get should include(s"invitationId=${invitationIdVAT.value}")

      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value, validVrn97.value, "vrn", "Not Required", serviceVAT)
    }

    "return 200 for authorised Agent with no vrn submitted and redisplay form with error message" in {
      val form = agentInvitationVrnForm
      val formData = Map("service" -> serviceVAT, "taxIdentifier" -> "", "postcode" -> "")
      val result = submitVrn(authorisedAsValidAgent(request
        .withFormUrlEncodedBody(form.bind(formData).data.toSeq: _*), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-vrn.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.vrn.required"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
      verifyAuditRequestNotSent(AgentInvitationEvent.AgentClientAuthorisationRequestCreated)
    }

    "return 200 for authorised Agent with invalid vrn and redisplay form with error message" in {
      val form = agentInvitationVrnForm
      val formData = Map("service" -> serviceVAT, "taxIdentifier" -> validNino.value, "postcode" -> "")
      val result = submitVrn(authorisedAsValidAgent(request
        .withFormUrlEncodedBody(form.bind(formData).data.toSeq: _*), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-vrn.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-vrn.invalid-format"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
      verifyAuditRequestNotSent(AgentInvitationEvent.AgentClientAuthorisationRequestCreated)
    }

    "return exception when create invitation fails" in {
      failedCreateInvitation(arn)

      val form = agentInvitationVrnForm.fill(AgentInvitationUserInput(serviceVAT, Some(validVrn97), None))
      val result = submitVrn(authorisedAsValidAgent(request.withFormUrlEncodedBody(form.data.toSeq: _*), arn.value))

      an[BadRequestException] should be thrownBy await(result)
      verifyAgentClientInvitationSubmittedEvent(arn.value, validVrn97.value, "vrn", "Fail", serviceVAT)
    }

    behave like anAuthorisedEndpoint(request, submitVrn)
  }

  "GET /agents/enter-nino" should {
    val request = FakeRequest("GET", "/agents/enter-nino")
    val showNinoForm = controller.showNinoForm()

    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
      val result = showNinoForm(authorisedAsValidAgent(request.withSession("service" -> serviceITSA), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.header"))
      checkHasAgentSignOutLink(result)

      verifyAuthoriseAttempt()
    }

    "return 200 for an Agent with PERSONAL-INCOME-RECORD enrolment" in {
      val result = showNinoForm(authorisedAsValidAgent(request.withSession("service" -> servicePIR), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.header"))
      checkHasAgentSignOutLink(result)

      verifyAuthoriseAttempt()
    }

    "return 303 for an Agent with HMRC-AS-AGENT enrolment when service is not available in session" in {
      val result = showNinoForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/select-service")
    }


    behave like anAuthorisedEndpoint(request, showNinoForm)
  }

  "POST /agents/enter-nino" should {
    val request = FakeRequest("POST", "/agents/enter-nino")
    val submitNino = controller.submitNino()

    "return 303 for authorised Agent with valid nino and service HMRC-MTD-IT, redirected to enter postcode page" in {
      val ninoForm = agentInvitationNinoForm.fill(AgentInvitationUserInput(serviceITSA, Some(validNino), None))
      val result = submitNino(authorisedAsValidAgent(request.withFormUrlEncodedBody(ninoForm.data.toSeq: _*)
        .withSession("service" -> serviceITSA), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/enter-postcode")
      header("Set-Cookie", result) shouldBe defined
      header("Set-Cookie", result).get should include(s"taxIdentifier=${validNino.value}")
      header("Set-Cookie", result).get should include(s"service=$serviceITSA")
    }

    "return 303 for authorised Agent with valid nino and Personal Income Record service, redirect to invitation sent page" in {
      createInvitationStubForNoKnownFacts(arn, validNino.value, invitationIdPIR, validNino.value, "ni", servicePIR, "NI")
      getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, "NI", "Pending")

      val ninoForm = agentInvitationNinoForm.fill(AgentInvitationUserInput(servicePIR, Some(validNino), None))
      val result = submitNino(authorisedAsValidAgent(request.withFormUrlEncodedBody(ninoForm.data.toSeq: _*)
        .withSession("taxIdentifier" -> validNino.value, "service" -> servicePIR), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      header("Set-Cookie", result) shouldBe defined
      header("Set-Cookie", result).get should include(s"taxIdentifier=${validNino.value}")
      header("Set-Cookie", result).get should include(s"service=$servicePIR")
      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value, validNino.value, "ni", "Not Required", servicePIR)
    }

    "return 200 for authorised Agent with an empty nino and show errors on the page" in {
      val result = submitNino(authorisedAsValidAgent(request.withFormUrlEncodedBody("taxIdentifier" -> "", "postcode" -> ""), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.nino.required"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "return 200 for authorised Agent with an invalid nino and show errors on the page" in {
      val result = submitNino(authorisedAsValidAgent(request.withFormUrlEncodedBody("taxIdentifier" -> "AB", "postcode" -> ""), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.invalid-format"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "return 200 for an authorised Agent if service is not found in session" in {
      val ninoForm = agentInvitationNinoForm.fill(AgentInvitationUserInput("", Some(validNino), None))
      val result = submitNino(authorisedAsValidAgent(request.withFormUrlEncodedBody(ninoForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.service.required"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }
  }

  "GET /agents/select-service" should {
    val request = FakeRequest("GET", "/agents/select-service")
    val selectService = controller.selectService()

    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
      val result = selectService(authorisedAsValidAgent(request.withSession("nino" -> validNino.value), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result,
        htmlEscapedMessage("select-service.title"),
        htmlEscapedMessage("select-service.header"),
        htmlEscapedMessage("select-service.itsa"),
        htmlEscapedMessage("select-service.personal-income-viewer"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }


    behave like anAuthorisedEndpoint(request, selectService)
  }

  "POST /agents/select-service" should {
    val request = FakeRequest("POST", "/agents/select-service")
    val submitService = controller.submitService()

    "return 303 for authorised Agent with valid ITSA service, redirect to enter nino page" in {
      val serviceForm = agentInvitationServiceForm.fill(AgentInvitationUserInput(serviceITSA, None, None))
      val result = submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/enter-nino")
      header("Set-Cookie", result) shouldBe defined
      header("Set-Cookie", result).get should include(s"service=$serviceITSA")
      verifyAuthoriseAttempt()
    }

    "return 303 for authorised Agent with valid Personal Income Record service, redirect to enter nino" in {
      val serviceForm = agentInvitationServiceForm.fill(AgentInvitationUserInput(servicePIR, None, None))
      val result = submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/enter-nino")
      header("Set-Cookie", result) shouldBe defined
      header("Set-Cookie", result).get should include(s"service=$servicePIR")
      verifyAuthoriseAttempt()
    }

    "return 200 for authorised Agent with no selected service and show error on the page" in {
      val result = submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody("service" -> ""), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("select-service.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("select-service.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.service.required"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }
    behave like anAuthorisedEndpoint(request, submitService)
  }

  "GET /agents/enter-postcode" should {
    val request = FakeRequest("GET", "/agents/enter-postcode")
    val showPostcodeForm = controller.showPostcodeForm()

    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
      val result = showPostcodeForm(authorisedAsValidAgent(request.withSession("taxIdentifier" -> validNino.value, "service" -> serviceITSA), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-postcode.title"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "return 303 for an Agent with HMRC-AS-AGENT enrolment when nino is not available in session" in {
      val result = showPostcodeForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/enter-nino")
    }
    behave like anAuthorisedEndpoint(request, showPostcodeForm)
  }

  "POST /agents/enter-postcode" should {
    val request = FakeRequest("POST", "/agents/enter-postcode")
    val submitPostcode = controller.submitPostcode()

    "return 303 for authorised Agent with valid nino and redirected to invitations-sent page" in {
      createInvitationStubWithKnownFacts(arn, mtdItId.value, invitationIdITSA, validNino.value, validPostcode, serviceITSA, "MTDITID")
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
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-postcode.title"))
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
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-postcode.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-postcode.invalid-format"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
      verifyAuditRequestNotSent(AgentInvitationEvent.AgentClientAuthorisationRequestCreated)
    }

    "return 303 for authorised Agent when client registration not found " in {
      failedCreateInvitationForNotEnrolled(arn)

      val form = agentInvitationPostCodeForm.fill(AgentInvitationUserInput(serviceITSA, Some(validNino), Some("AB101AB")))
      val result = submitPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody(form.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/not-enrolled")

      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value, validNino.value, "ni", "Fail", serviceITSA)
    }

    "return 303 for authorised Agent when postcode does not match " in {
      failedCreateInvitationFoInvalidPostcode(arn)

      val form = agentInvitationPostCodeForm.fill(AgentInvitationUserInput(serviceITSA, Some(validNino), Some("AB101AB")))
      val result = submitPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody(form.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/not-matched")

      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value, validNino.value, "ni", "Fail", serviceITSA)
    }

    "return exception when create invitation fails" in {
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
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent-link.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.header"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.description.advice.pt1", "27 December 2017"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.description.advice.pt2"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.description.advice.pt3"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.button"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage(s"$wireMockBaseUrlAsString${routes.ClientsInvitationController.start(invitationIdITSA)}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "return exception when no invitation id and deadline found in session" in {
      val result = invitationSent(authorisedAsValidAgent(request, arn.value))

      an[RuntimeException] should be thrownBy await(result)
    }

    behave like anAuthorisedEndpoint(request, invitationSent)
  }

  "GET /agents/not-enrolled" should {
    val request = FakeRequest("GET", "/agents/not-enrolled")
    val notEnrolled = controller.notEnrolled()

    "return 403 for authorised Agent who submitted known facts of an not enrolled client" in {
      val result = notEnrolled(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 403
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-enrolled.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-enrolled.description"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedEndpoint(request, notEnrolled)
  }

  "GET /agents/not-matched" should {
    val request = FakeRequest("GET", "/agents/not-matched")
    val notMatched = controller.notMatched()

    "return 403 for authorised Agent who submitted not matching known facts" in {
      val result = notMatched(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 403
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-matched.title"))
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

  def anAuthorisedEndpoint(request: FakeRequest[AnyContentAsEmpty.type], action: Action[AnyContent]) = {

    "return 303 for an Agent with no enrolments and redirected to Login Page" in {
      givenUnauthorisedForInsufficientEnrolments()
      an[InsufficientEnrolments] shouldBe thrownBy {
        await(action(authenticatedClient(request, Enrolment("", "", ""))))
      }
      verifyAuthoriseAttempt()
    }

    "return 303 for no Agent and redirected to Login Page" in {
      givenUnauthorisedForInsufficientEnrolments()
      an[InsufficientEnrolments] shouldBe thrownBy {
        await(action(authenticatedClient(request, Enrolment("OtherEnrolment", "Key", "Value"))))
      }
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
