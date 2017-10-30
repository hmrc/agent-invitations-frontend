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

import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import uk.gov.hmrc.agentinvitationsfrontend.form.{NinoForm, PostCode, PostcodeForm}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.domain.Nino
import play.api.mvc._
import uk.gov.hmrc.auth.core.{AuthorisationException, InsufficientEnrolments, NoActiveSession}

class InvitationControllerISpec extends BaseISpec {

  lazy val controllers: InvitationsController = app.injector.instanceOf[InvitationsController]

  val arn = "TARN0000001"

  "GET /agents/enter-nino" should {

    val request = FakeRequest("GET", "/agents/enter-nino")
    val enterNino = controllers.enterNino()

    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
      val result = enterNino(authorisedAsValidAgent(request, arn))
      status(result) shouldBe 200
      verifyAuthoriseAttempt()
    }
    behave like anAuthorisedEndpoint(request, enterNino)
  }

  "POST /agents/enter-nino" should {

    val request = FakeRequest("POST", "/agents/enter-nino")
    val submitNino = controllers.submitNino()

    "return 303 for authorised Agent with valid nino and redirected to Postcode Page" in {
      val ninoForm = NinoForm.ninoForm.fill(Nino("AB123456A"))
      val result = submitNino(authorisedAsValidAgent(request.withFormUrlEncodedBody(ninoForm.data.toSeq: _*), arn))
      status(result) shouldBe 303
      verifyAuthoriseAttempt()
    }

    "return 200 for authorised Agent with invalid nino and redisplay form with error message" in {
      val result = submitNino(authorisedAsValidAgent(request, arn))
      status(result) shouldBe 200
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedEndpoint(request, submitNino)
  }

  "GET /agents/enter-postcode" should {

    val request = FakeRequest("GET", "/agents/enter-postcode")
    val enterPostcode = controllers.enterPostcode()

    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
      val result = enterPostcode(authorisedAsValidAgent(request, arn))
      status(result) shouldBe 200
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedEndpoint(request, enterPostcode)
  }

  "POST /agents/enter-postcode" should {

    val request = FakeRequest("POST", "/agents/enter-postcode")
    val submitPostcode = controllers.submitPostcode()

    "return 303 for authorised Agent with valid postcode and redirected to Confirm Invitation Page" in {
      val postcode = PostcodeForm.postCodeForm.fill(PostCode("BN12 6BX"))
      val result = submitPostcode(authorisedAsValidAgent(request.withFormUrlEncodedBody(postcode.data.toSeq: _*), arn))
      status(result) shouldBe 303
      verifyAuthoriseAttempt()
    }

    "return 200 for authorised Agent with invalid postcode and redisplay form with error message" in {
      val result = controllers.submitPostcode()(authorisedAsValidAgent(request, arn))
      status(result) shouldBe 200
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedEndpoint(request, submitPostcode)
  }

  "GET /agents/confirm-invitation" should {

    val request = FakeRequest("GET", "/agents/confirm-invitation")
    val confirmInvitation = controllers.confirmInvitation()

    "return 200 for an Agent with HMRC-AS-AGENT enrolment with valid nino and postcode" in {
      val result = confirmInvitation(authorisedAsValidAgent(request, arn))
      status(result) shouldBe 200
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedEndpoint(request, confirmInvitation)
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
