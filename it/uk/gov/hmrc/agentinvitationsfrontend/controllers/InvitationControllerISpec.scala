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

import play.api.test.FakeRequest
import uk.gov.hmrc.agentinvitationsfrontend.stubs.AuthStub
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec

class InvitationControllerISpec extends BaseISpec with AuthStub {

  private lazy val controllers: InvitationsController = app.injector.instanceOf[InvitationsController]

  //TODO Add Auth
  "GET /agents/enter-nino" should {
    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
      val result = controllers.enterNino()(FakeRequest("GET", "/agents/enter-nino"))
      status(result) shouldBe 200
    }

    "return 303 for an Agent with no enrolments and redirected to Login Page" in {
      val result = controllers.enterNino()(FakeRequest("GET", "/agents/enter-nino"))
      status(result) shouldBe 303
    }

    "return 303 for no Agent and redirected to Login Page" in {
      val result = controllers.enterNino()(FakeRequest("GET", "/agents/enter-nino"))
      status(result) shouldBe 303
    }

    "return 303 for not logged in user and redirected to Login Page" in {
      val result = controllers.enterNino()(FakeRequest("GET", "/agents/enter-nino"))
      status(result) shouldBe 303
    }
  }

  "POST /agents/enter-nino" should {
    "return 303 for authorised Agent with valid nino and redirected to Postcode Page" in {
      val result = controllers.submitNino()(FakeRequest("POST", "/agents/enter-nino"))
      status(result) shouldBe 303
    }

    "return 200 for authorised Agent with invalid nino and redisplay form with error message" in {
      val result = controllers.submitNino()(FakeRequest("POST", "/agents/enter-nino"))
      status(result) shouldBe 200
    }
  }

  "GET /agents/enter-postcode" should {
    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
    }

    "return 303 for an Agent with no enrolments and redirected to Login Page" in {
    }

    "return 303 for no Agent and redirected to Login Page" in {

    }

    "return 303 for not logged in user and redirected to Login Page" in {
    }
  }

  "POST /agents/enter-postcode" should {
    "return 303 for authorised Agent with valid postcode and redirected to Confirm Invitation Page" in {

    }

    "return 200 for authorised Agent with invalid postcode and redisplay form with error message" in {

    }
  }

  "GET /agents/confirm-invitation" should {
    "return 200 for an Agent with HMRC-AS-AGENT enrolment with valid nino and postcode" in {
    }

    "return 303 for an Agent with no enrolments and redirected to Login Page" in {
    }

    "return 303 for no Agent and redirected to Login Page" in {

    }

    "return 303 for not logged in user and redirected to Login Page" in {
    }
  }
}
