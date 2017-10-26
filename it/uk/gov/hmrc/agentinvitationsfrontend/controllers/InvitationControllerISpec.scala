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
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec

class InvitationControllerISpec extends BaseISpec {

  lazy val controllers: InvitationsController = app.injector.instanceOf[InvitationsController]

  "GET /agents/enter-nino" should {

    val request = FakeRequest("GET", "/agents/enter-nino")

    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
      val result = controllers.enterNino()(authorisedAsValidAgent(request))
      status(result) shouldBe 200
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedEndpoint(request)
  }

  "POST /agents/enter-nino" should {

    val request = FakeRequest("POST", "/agents/enter-nino")

    "return 303 for authorised Agent with valid nino and redirected to Postcode Page" in {
      val result = controllers.submitNino()(authorisedAsValidAgent(request))
      status(result) shouldBe 303
      verifyAuthoriseAttempt()
    }

    "return 200 for authorised Agent with invalid nino and redisplay form with error message" in {
      val result = controllers.submitNino()(authorisedAsValidAgent(request))
      status(result) shouldBe 200
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedEndpoint(request)
  }

  "GET /agents/enter-postcode" should {

    val request = FakeRequest("GET", "/agents/enter-postcode")

    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
      val result = controllers.enterPostcode()(authorisedAsValidAgent(request))
      status(result) shouldBe 200
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedEndpoint(request)
  }

  "POST /agents/enter-postcode" should {

    val request = FakeRequest("POST", "/agents/enter-postcode")

    "return 303 for authorised Agent with valid postcode and redirected to Confirm Invitation Page" in {
      val result = controllers.enterPostcode()(authorisedAsValidAgent(request))
      status(result) shouldBe 303
      verifyAuthoriseAttempt()
    }

    "return 200 for authorised Agent with invalid postcode and redisplay form with error message" in {
      val result = controllers.enterPostcode()(authorisedAsValidAgent(request))
      status(result) shouldBe 200
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedEndpoint(request)
  }

  "GET /agents/confirm-invitation" should {

    val request = FakeRequest("GET", "/agents/confirm-invitation")

    "return 200 for an Agent with HMRC-AS-AGENT enrolment with valid nino and postcode" in {
      val result = controllers.confirmInvitation()(authorisedAsValidAgent(request))
      status(result) shouldBe 200
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedEndpoint(request)
  }

  def anAuthorisedEndpoint(request: FakeRequest[AnyContentAsEmpty.type]) = {

    "return 303 for an Agent with no enrolments and redirected to Login Page" in {
      val result = controllers.enterNino()(authenticated(request, None, isAgent = true))
      status(result) shouldBe 303
      verifyAuthoriseAttempt()
    }

    "return 303 for no Agent and redirected to Login Page" in {
      val result = controllers.enterNino()(authenticated(request, Some("IR-PAYE"), isAgent = false))
      status(result) shouldBe 303
      verifyAuthoriseAttempt()
    }

    "return 303 for not logged in user and redirected to Login Page" in {
      val result = controllers.enterNino()(request)
      status(result) shouldBe 303
      verifyAuthoriseAttempt()
    }
  }
}
