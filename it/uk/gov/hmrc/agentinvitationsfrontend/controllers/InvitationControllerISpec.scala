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

import play.api.mvc.{ AnyContentAsEmpty, _ }
import play.api.test.FakeRequest
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentInvitationsController.{ agentInvitationNinoForm, agentInvitationPostCodeForm }
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentInvitationUserInput
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{ Arn, MtdItId }
import uk.gov.hmrc.auth.core.{ AuthorisationException, InsufficientEnrolments }
import uk.gov.hmrc.domain.Nino

class InvitationControllerISpec extends BaseISpec {

  lazy val controllers: AgentInvitationsController = app.injector.instanceOf[AgentInvitationsController]
  val arn = Arn("TARN0000001")
  val mtdItId = MtdItId("ABCDEF123456789")

  "GET /agents/enter-nino" should {

    val request = FakeRequest("GET", "/agents/enter-nino")
    val enterNino = controllers.enterNino()

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
    val submitNino = controllers.submitNino()

    "return 200 for authorised Agent with valid nino and redirected to Postcode Page" in {
      val ninoForm = agentInvitationNinoForm.fill(AgentInvitationUserInput(Nino("AB123456A"), ""))
      val result = submitNino(authorisedAsValidAgent(request.withFormUrlEncodedBody(ninoForm.data.toSeq: _*), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-postcode.title"))
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
    val submitPostcode = controllers.submitPostcode()

    "return 200 for authorised Agent with valid postcode and redirected to Confirm Invitation Page" in {
      createInvitationStub(arn, mtdItId, "1")
      getInvitationStub(arn, mtdItId, "1")
      val postcode = agentInvitationPostCodeForm.fill(AgentInvitationUserInput(Nino("AB123456A"), "BN12 6BX"))
      val result = submitPostcode(authorisedAsValidAgent(request
        .withFormUrlEncodedBody(postcode.data.toSeq: _*), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent-link.title"))
      verifyAuthoriseAttempt()
    }

    "return 200 for authorised Agent with invalid postcode and redisplay form with error message" in {
      val postcodeForm = agentInvitationPostCodeForm
      val postcodeData = Map("nino" -> "AB123456A", "postcode" -> "")
      val result = controllers.submitPostcode()(authorisedAsValidAgent(request
        .withFormUrlEncodedBody(postcodeForm.bind(postcodeData).data.toSeq: _*), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-postcode.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-postcode.error-empty"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-postcode.invalid-format"))
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedEndpoint(request, submitPostcode)
  }

  def anAuthorisedEndpoint(request: FakeRequest[AnyContentAsEmpty.type], action: Action[AnyContent]) = {

    "return 303 for an Agent with no enrolments and redirected to Login Page" in {
      an[InsufficientEnrolments] shouldBe thrownBy {
        await(action(authenticated(request, Enrolment("", "", ""), isAgent = true)))
      }
      verifyAuthoriseAttempt()
    }

    "return 303 for no Agent and redirected to Login Page" in {
      an[InsufficientEnrolments] shouldBe thrownBy {
        await(action(authenticated(request, Enrolment("OtherEnrolment", "Key", "Value"), isAgent = false)))
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
}
