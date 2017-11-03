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

package controllers

import javax.inject.Inject

import org.scalatest.mockito.MockitoSugar
import play.api.Configuration
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{ ClientInvitationsController, routes }

class ClientInvitationsControllerSpec @Inject() (implicit val configuration: Configuration) extends PlayMessagesSpec with MockitoSugar {

  val controller = new ClientInvitationsController(messagesApi)

  "ClientInvitationsController" should {
    "return Status: OK when loading the landing page" in {
      val result = controller.start("aToken").apply(FakeRequest())

      status(result) shouldBe OK
    }

    "redirect to confirmInvitation when clicking accept on the landing page" in {
      val result = controller.submitStart().apply(FakeRequest())

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientInvitationsController.getConfirmInvitation().url
    }

    "return Status: OK when loading the confirm invitation page" in {
      val result = controller.getConfirmInvitation().apply(FakeRequest())

      status(result) shouldBe OK
    }

    "redirect to confirmTerms when clicking continue on the confirm invitation page" in {
      val result = controller.submitConfirmInvitation().apply(FakeRequest())

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientInvitationsController.getConfirmTerms().url
    }

    "return Status: OK when loading the confirm terms page" in {
      val result = controller.getConfirmTerms().apply(FakeRequest())

      status(result) shouldBe OK
    }

    "redirect to complete when clicking confirm on the confirm terms page" in {
      val result = controller.submitConfirmTerms().apply(FakeRequest())

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientInvitationsController.getCompletePage().url
    }

    "return Status: OK when loading the complete page" in {
      val result = controller.getCompletePage().apply(FakeRequest())

      status(result) shouldBe OK
    }
  }
}
