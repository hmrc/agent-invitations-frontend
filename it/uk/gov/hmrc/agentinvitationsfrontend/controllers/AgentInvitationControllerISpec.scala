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

import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.audit.AgentInvitationEvent
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentInvitationUserInput
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId}
import uk.gov.hmrc.auth.core.{AuthorisationException, InsufficientEnrolments}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.BadRequestException

import scala.concurrent.duration._

class AgentInvitationControllerISpec extends BaseISpec {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  val arn = Arn("TARN0000001")
  val mtdItId = MtdItId("ABCDEF123456789")
  private val validNino = Nino("AB123456A")
  val serviceITSA = "HMRC-MTD-IT"
  val servicePIR = "PERSONAL-INCOME-RECORD"
  val validPostcode = "BN12 6BX"
  val invitationId = InvitationId("ABERULMHCKKW3")

  "GET /agents/" should {
    "redirect to /agent/enter-nino" in {
      val result = controller.agentsRoot(FakeRequest())
      status(result) shouldBe 303
      val timeout = 2.seconds
      redirectLocation(result)(timeout).get should include("/agents/enter-nino")
    }
  }

  "GET /agents/enter-nino" should {
    val request = FakeRequest("GET", "/agents/enter-nino")
    val showNinoForm = controller.showNinoForm()

    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
      val result = showNinoForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.header"))
      verifyAuthoriseAttempt()
    }
    behave like anAuthorisedEndpoint(request, showNinoForm)
  }

  "POST /agents/enter-nino" should {
    val request = FakeRequest("POST", "/agents/enter-nino")
    val submitNino = controller.submitNino()

    "return 303 for authorised Agent with valid nino and redirected to Select service page" in {
      val ninoForm = agentInvitationNinoForm.fill(AgentInvitationUserInput(validNino, None, None))
      val result = submitNino(authorisedAsValidAgent(request.withFormUrlEncodedBody(ninoForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/select-service")
      header("Set-Cookie", result) shouldBe defined
      header("Set-Cookie", result).get should include(s"nino=${validNino.value}")
    }

    "return 200 for authorised Agent with an empty nino and show errors on the page" in {
      val result = submitNino(authorisedAsValidAgent(request.withFormUrlEncodedBody("nino" -> "", "postcode" -> ""), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.nino.required"))
      verifyAuthoriseAttempt()
    }

    "return 200 for authorised Agent with an invalid nino and show errors on the page" in {
      val result = submitNino(authorisedAsValidAgent(request.withFormUrlEncodedBody("nino" -> "AB", "postcode" -> ""), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.invalid-format"))
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
      verifyAuthoriseAttempt()
    }

    "return 303 for an Agent with HMRC-AS-AGENT enrolment when nino is not available in session" in {
      val result = selectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/enter-nino")
    }
    behave like anAuthorisedEndpoint(request, selectService)
  }

  "POST /agents/select-service" should {
    val request = FakeRequest("POST", "/agents/select-service")
    val submitService = controller.submitService()

    "return 303 for authorised Agent with valid nino and ITSA service, redirect to postcode page" in {
      val serviceForm = agentInvitationServiceForm.fill(AgentInvitationUserInput(validNino, Some(serviceITSA), None))
      val result = submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*)
        .withSession("nino" -> validNino.value), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/enter-postcode")
      header("Set-Cookie", result) shouldBe defined
      header("Set-Cookie", result).get should include(s"nino=${validNino.value}")
      header("Set-Cookie", result).get should include(s"service=$serviceITSA")
      verifyAuthoriseAttempt()
    }

    "return 303 for authorised Agent with valid nino and Personal Income Record service, redirect to invitation sent page" in {
      createInvitationStubForPIR(arn, validNino.value, invitationId, validNino.value, servicePIR, "NI")
      getInvitationStub(arn, validNino.value, invitationId, servicePIR, "NI")

      val serviceForm = agentInvitationServiceForm.fill(AgentInvitationUserInput(validNino, Some(servicePIR), None))
      val result = submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*)
        .withSession("nino" -> validNino.value), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      header("Set-Cookie", result) shouldBe defined
      header("Set-Cookie", result).get should include(s"nino=${validNino.value}")
      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value,validNino.value,"Not Required", servicePIR)
    }

    "return 200 for authorised Agent with no selected service and show error on the page" in {
      val result = submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody("nino" -> validNino.value, "service" -> "", "postcode" -> ""), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("select-service.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("select-service.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.service.required"))
      verifyAuthoriseAttempt()
    }
    behave like anAuthorisedEndpoint(request, submitService)
  }

  "GET /agents/enter-postcode" should {
    val request = FakeRequest("GET", "/agents/enter-postcode")
    val showPostcodeForm = controller.showPostcodeForm()

    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
      val result = showPostcodeForm(authorisedAsValidAgent(request.withSession("nino" -> validNino.value, "service" -> serviceITSA), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-postcode.title"))
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
      createInvitationStubForITSA(arn, mtdItId.value, invitationId, validNino.value, validPostcode, serviceITSA, "MTDITID")
      getInvitationStub(arn, mtdItId.value, invitationId, serviceITSA, "MTDITID")

      val form = agentInvitationPostCodeForm.fill(AgentInvitationUserInput(validNino, Some(serviceITSA), Some(validPostcode)))
      val result = submitPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody(form.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
      header("Set-Cookie", result) shouldBe defined
      header("Set-Cookie", result).get should include("invitationId=ABERULMHCKKW3")

      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value,validNino.value,"Success", serviceITSA)
    }

    "return 200 for authorised Agent with empty postcode and redisplay form with error message" in {
      val form = agentInvitationPostCodeForm
      val ninoData = Map("nino" -> validNino.value, "postcode" -> "")
      val result = submitPostcode(authorisedAsValidAgent(request
        .withFormUrlEncodedBody(form.bind(ninoData).data.toSeq: _*), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-postcode.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.postcode.required"))
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
      verifyAuthoriseAttempt()
      verifyAuditRequestNotSent(AgentInvitationEvent.AgentClientAuthorisationRequestCreated)
    }

    "return 303 for authorised Agent when client registration not found " in {
      failedCreateInvitationForNotEnrolled(arn)

      val form = agentInvitationPostCodeForm.fill(AgentInvitationUserInput(validNino, Some(serviceITSA), Some("AB101AB")))
      val result = submitPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody(form.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/not-enrolled")

      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value,validNino.value,"Fail", serviceITSA)
    }

    "return 303 for authorised Agent when postcode does not match " in {
      failedCreateInvitationFoInvalidPostcode(arn)

      val form = agentInvitationPostCodeForm.fill(AgentInvitationUserInput(validNino, Some(serviceITSA), Some("AB101AB")))
      val result = submitPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody(form.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/not-matched")

      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value,validNino.value,"Fail", serviceITSA)
    }

    "return exception when create invitation fails" in {
      failedCreateInvitation(arn)

      val form = agentInvitationPostCodeForm.fill(AgentInvitationUserInput(validNino, Some(serviceITSA), Some("AB101AB")))
      val result = submitPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody(form.data.toSeq: _*), arn.value))

      an[BadRequestException] should be thrownBy await(result)
      verifyAgentClientInvitationSubmittedEvent(arn.value,validNino.value,"Fail", serviceITSA)
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
      checkHtmlResultWithBodyText(result, htmlEscapedMessage(s"$wireMockBaseUrlAsString${routes.ClientsInvitationController.start(invitationId)}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
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
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedEndpoint(request, notMatched)
  }

  def anAuthorisedEndpoint(request: FakeRequest[AnyContentAsEmpty.type], action: Action[AnyContent]) = {

    "return 303 for an Agent with no enrolments and redirected to Login Page" in {
      an[InsufficientEnrolments] shouldBe thrownBy {
        await(action(authenticated(request, Enrolment("", "", ""))))
      }
      verifyAuthoriseAttempt()
    }

    "return 303 for no Agent and redirected to Login Page" in {
      an[InsufficientEnrolments] shouldBe thrownBy {
        await(action(authenticated(request, Enrolment("OtherEnrolment", "Key", "Value"))))
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

  def verifyAgentClientInvitationSubmittedEvent(arn: String, clientId: String, result: String, service: String): Unit = {
    verifyAuditRequestSent(1, AgentInvitationEvent.AgentClientAuthorisationRequestCreated,
      detail = Map(
        "factCheck" -> result,
        "agentReferenceNumber" -> arn,
        "clientIdType" -> "ni",
        "clientId" -> clientId,
        "service" -> s"$service"
      ),
      tags = Map(
        "transactionName" -> "Agent client service authorisation request created"
      )
    )
  }
}
