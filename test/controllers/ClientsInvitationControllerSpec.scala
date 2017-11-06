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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{ ClientsInvitationController, routes }
import uk.gov.hmrc.agentinvitationsfrontend.services.InvitationsService
import uk.gov.hmrc.auth.core.AuthConnector

class ClientsInvitationControllerSpec @Inject() (implicit val configuration: Configuration) extends PlayMessagesSpec with MockitoSugar {
  val invitationsService = mock[InvitationsService]
  val controller = new ClientsInvitationController(invitationsService, messagesApi, mock[AuthConnector])

  "ClientsInvitationController" should {
    "return Status: OK when loading the landing page" in {
      val result = controller.start("someInvitationID").apply(FakeRequest())

      status(result) shouldBe OK
    }

    "session contains invitation ID when loading the landing page" in {
      implicit val request = FakeRequest()
      val result = controller.start("someInvitationID").apply(request)

      await(result).session.get("invitationId") shouldBe Some("someInvitationID")
    }

    "redirect to confirmInvitation when clicking accept on the landing page" in {
      val result = controller.submitStart().apply(FakeRequest())

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.getConfirmInvitation().url
    }

    "return Status: OK when loading the confirm invitation page" in {
      val result = controller.getConfirmInvitation().apply(FakeRequest())

      status(result) shouldBe OK
    }

    "reshow confirm invitation page when clicking continue on the confirm invitation page without having chosen yes or no" in {
      val result = controller.submitConfirmInvitation().apply(FakeRequest())

      status(result) shouldBe OK
    }

    "redirect to confirmTerms when clicking continue on the confirm invitation page having selected yes" in {
      val req = FakeRequest().withFormUrlEncodedBody("confirmInvite" -> "true")
      val result = controller.submitConfirmInvitation().apply(req)

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.getConfirmTerms().url
    }

    "show an error page when clicking continue on the confirm invitation page having selected no" in {
      val req = FakeRequest().withFormUrlEncodedBody("confirmInvite" -> "false")
      val result = controller.submitConfirmInvitation().apply(req)

      status(result) shouldBe NOT_IMPLEMENTED // TODO APB-1543
    }

    "return Status: OK when loading the confirm terms page" in {
      val result = controller.getConfirmTerms().apply(FakeRequest())

      status(result) shouldBe OK
    }

    "redirect to complete when clicking confirm on the confirm terms page when the checkbox was checked" in {
      val req = FakeRequest().withFormUrlEncodedBody("confirmTerms" -> "true")
      val result = controller.submitConfirmTerms().apply(req)

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.getCompletePage().url
    }

    "reshow confirm terms page when clicking confirm having not checked the checkbox" in {
      val req = FakeRequest().withFormUrlEncodedBody("confirmTerms" -> "")
      val result = controller.submitConfirmTerms().apply(req)

      status(result) shouldBe OK
    }

    "return Status: OK when loading the complete page" in {
      val result = controller.getCompletePage().apply(FakeRequest())

      status(result) shouldBe OK
    }
  }
}
