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

import play.api.mvc.{AnyContentAsEmpty, _}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.audit.AgentInvitationEvent
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.{agentInvitationNinoForm, agentInvitationPostCodeForm}
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentInvitationUserInput
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, MtdItId}
import uk.gov.hmrc.auth.core.{AuthorisationException, InsufficientEnrolments}
import uk.gov.hmrc.domain.Nino

import scala.concurrent.duration._

class AgentInvitationControllerISpec extends BaseISpec {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  val arn = Arn("TARN0000001")
  val mtdItId = MtdItId("ABCDEF123456789")
  private val validNino = Nino("AB123456A")

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
    val enterNino = controller.enterNino()

    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
      val result = enterNino(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.title"))
      verifyAuthoriseAttempt()
    }
    behave like anAuthorisedEndpoint(request, enterNino)
  }

  "POST /agents/enter-postcode" should {
    val request = FakeRequest("POST", "/agents/enter-postcode")
    val submitNino = controller.submitNino()

    "return 200 for authorised Agent with valid nino and redirected to Postcode Page" in {
      val ninoForm = agentInvitationNinoForm.fill(AgentInvitationUserInput(validNino, ""))
      val result = submitNino(authorisedAsValidAgent(request.withFormUrlEncodedBody(ninoForm.data.toSeq: _*), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-postcode.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage(s"""${validNino.value}"""))
      verifyAuthoriseAttempt()
    }

    "return 200 for authorised Agent with lowercase nino with spaces and redirected to Postcode Page with corrected Nino" in {
      val result = submitNino(authorisedAsValidAgent(
        request.withFormUrlEncodedBody(("nino", s"  ${validNino.value.toLowerCase}  "), ("postcode" , "")), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-postcode.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage(s"""${validNino.value}"""))
      verifyAuthoriseAttempt()
    }

    "return 200 for authorised Agent with invalid nino and redisplay form with error message" in {
      val ninoForm = agentInvitationNinoForm
      val ninoData = Map("nino" -> "", "postcode" -> "")
      val result = submitNino(authorisedAsValidAgent(request
        .withFormUrlEncodedBody(ninoForm.bind(ninoData).data.toSeq: _*), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.error-empty"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.invalid-format"))
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedEndpoint(request, submitNino)
  }

  "POST /agents/invitation-sent" should {
    val request = FakeRequest("POST", "/agents/invitation-sent")
    val submitPostcode = controller.submitPostcode()
    val validPostcode = "BN12 6BX"

    "return 200 for authorised Agent with valid postcode and redirected to Confirm Invitation Page (secureFlag = false)" in {
      
      createInvitationStub(arn, mtdItId, "1", validNino.value, validPostcode)
      getInvitationStub(arn, mtdItId, "1")
      val postcode = agentInvitationPostCodeForm.fill(AgentInvitationUserInput(validNino, validPostcode))
      val result = submitPostcode(authorisedAsValidAgent(request
        .withFormUrlEncodedBody(postcode.data.toSeq: _*), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent-link.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage(s"$wireMockBaseUrlAsString${routes.ClientsInvitationController.start("1")}"))
      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value,validNino.value,"Success")
    }

    "return 200 for authorised Agent with valid postcode with spaces and lower case character and redirected to Confirm Invitation Page (secureFlag = false)" in {

      createInvitationStub(arn, mtdItId, "1", validNino.value, validPostcode)
      getInvitationStub(arn, mtdItId, "1")
      val postcode = agentInvitationPostCodeForm.fill(AgentInvitationUserInput(validNino, s"  ${validPostcode.toLowerCase}  "))
      val result = submitPostcode(authorisedAsValidAgent(request
        .withFormUrlEncodedBody(postcode.data.toSeq: _*), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent-link.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage(s"$wireMockBaseUrlAsString${routes.ClientsInvitationController.start("1")}"))
      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value,validNino.value,"Success")
    }

    "return 200 for authorised Agent with invalid postcode and redisplay form with error message" in {
      val postcodeForm = agentInvitationPostCodeForm
      val postcodeData = Map("nino" -> validNino.value, "postcode" -> "")
      val result = submitPostcode(authorisedAsValidAgent(request
        .withFormUrlEncodedBody(postcodeForm.bind(postcodeData).data.toSeq: _*), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-postcode.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-postcode.error-empty"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-postcode.invalid-format"))
      verifyAuthoriseAttempt()
      verifyAuditRequestNotSent(AgentInvitationEvent.AgentClientInvitationSubmitted)
    }

    "return 303 for authorised Agent with valid NINO without HMRC_MTD-IT enrolment and a valid postcode" in {
      failedCreateInvitationForNotEnrolled(arn)
      val postcodeForm = agentInvitationPostCodeForm
      val postcodeData = Map("nino" -> validNino.value, "postcode" -> "AA11AA")

      val result = submitPostcode(authorisedAsValidAgent(request
        .withFormUrlEncodedBody(postcodeForm.bind(postcodeData).data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/not-enrolled")
      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value,validNino.value,"Fail")
    }

    "return 303 for authorised Agent with valid NINO with HMRC_MTD-IT enrolment and an non-associated but valid postcode" in {
      failedCreateInvitationFoInvalidPostcode(arn)
      val postcodeForm = agentInvitationPostCodeForm
      val postcodeData = Map("nino" -> validNino.value, "postcode" -> "AA11AA")

      val result = submitPostcode(authorisedAsValidAgent(request
        .withFormUrlEncodedBody(postcodeForm.bind(postcodeData).data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/not-matched")
      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value,validNino.value,"Fail")
    }

    "return exception when invitation could not be retrieved after creation" in {
      createInvitationStub(arn, mtdItId, "1", validNino.value, "AA11AA")
      notFoundGetInvitationStub(mtdItId, "1")
      val postcodeForm = agentInvitationPostCodeForm
      val postcodeData = Map("nino" -> validNino.value, "postcode" -> "AA11AA")

      val result = submitPostcode(authorisedAsValidAgent(request
        .withFormUrlEncodedBody(postcodeForm.bind(postcodeData).data.toSeq: _*), arn.value))

      an[Exception] shouldBe thrownBy {
        await(result)
      }
      verifyAgentClientInvitationSubmittedEvent(arn.value,validNino.value,"Fail")
    }

    behave like anAuthorisedEndpoint(request, submitPostcode)
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

  def verifyAgentClientInvitationSubmittedEvent(arn: String, nino: String, result: String): Unit = {
    verifyAuditRequestSent(1, AgentInvitationEvent.AgentClientInvitationSubmitted,
      detail = Map(
        "result" -> result,
        "agentReferenceNumber" -> arn,
        "regimeId" -> nino,
        "regime" -> "HMRC-MTD-IT"
      ),
      tags = Map(
        "transactionName" -> "agent-client-invitation-submitted",
        "path" -> "/agents/invitation-sent"
      )
    )
  }
}
