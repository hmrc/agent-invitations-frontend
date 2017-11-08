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
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{ Arn, MtdItId }

class ClientsInvitationControllerISpec extends BaseISpec {

  lazy val controller: ClientsInvitationController = app.injector.instanceOf[ClientsInvitationController]
  val mtdItId = MtdItId("ABCDEF123456789")

  "GET /:invitationId (landing page)" should {
    "show the landing page" in {
      val result = controller.start("someInvitationID")(authorisedAsValidClient(FakeRequest(), mtdItId.value))
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("landing-page.title"))
    }

    "session contains invitation ID when loading the landing page" in {
      implicit val request = authorisedAsValidClient(FakeRequest(), mtdItId.value)
      val result = controller.start("someInvitationID")(request)

      await(result).session.get("invitationId") shouldBe Some("someInvitationID")
    }
  }

  "POST / (clicking accept on the landing page)" should {
    "redirect to /accept-tax-agent-invitation/2" in {
      val result = controller.submitStart(authorisedAsValidClient(FakeRequest(), mtdItId.value))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.getConfirmInvitation().url
    }
  }

  "GET /accept-tax-agent-invitation/2 (confirm invitation page)" should {
    "show the confirm invitation page" in {
      val result = controller.getConfirmInvitation().apply(authorisedAsValidClient(FakeRequest(), mtdItId.value))

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-invitation.title"))
    }
  }

  "POST /accept-tax-agent-invitation/2 (clicking continue on the confirm invitation page)" should {
    "reshow the page when neither yes nor no choices were selected with an error message" in {
      val result = controller.submitConfirmInvitation().apply(authorisedAsValidClient(FakeRequest(), mtdItId.value))

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-invitation.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.confirmInvite.invalid"))
    }

    "redirect to confirm terms page when yes was selected" in {
      val req = authorisedAsValidClient(FakeRequest(), mtdItId.value).withFormUrlEncodedBody("confirmInvite" -> "true")
      val result = controller.submitConfirmInvitation().apply(req)

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.getConfirmTerms().url
    }

    "show an error page when no was selected" in {
      val req = authorisedAsValidClient(FakeRequest(), mtdItId.value).withFormUrlEncodedBody("confirmInvite" -> "false")
      val result = controller.submitConfirmInvitation().apply(req)

      status(result) shouldBe NOT_IMPLEMENTED // TODO APB-1543
    }
  }

  "GET /accept-tax-agent-invitation/3 (confirm terms page)" should {
    "show the confirm terms page" in {
      val req = authorisedAsValidClient(FakeRequest(), mtdItId.value)
      val result = controller.getConfirmTerms().apply(req)

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.title"))
    }
  }

  "POST /accept-tax-agent-invitation/3 (clicking confirm on the confirm terms page)" should {
    def withSessionData[A](req: FakeRequest[A], key: String, value: String): FakeRequest[A] = {
      req.withSession((req.session + (key -> value)).data.toSeq: _*)
    }

    "redirect to complete page when the checkbox was checked" in {
      acceptInvitationStub(mtdItId, "someInvitationId")

      val req = authorisedAsValidClient(FakeRequest(), mtdItId.value).withFormUrlEncodedBody("confirmTerms" -> "true")
      val reqWithSession = withSessionData(req, "invitationId", "someInvitationId")
      val result = controller.submitConfirmTerms().apply(reqWithSession)

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.getCompletePage().url
    }

    "call agent-client-authorisation to accept the invitation and create the relationship in ETMP when the checkbox was checked" in {
      acceptInvitationStub(mtdItId, "someInvitationId")

      val req = authorisedAsValidClient(FakeRequest(), mtdItId.value).withFormUrlEncodedBody("confirmTerms" -> "true")
      val reqWithSession = withSessionData(req, "invitationId", "someInvitationId")
      await(controller.submitConfirmTerms().apply(reqWithSession))

      verifyAcceptInvitationAttempt(mtdItId, "someInvitationId")
    }

    "reshow the page when the checkbox was not checked with an error message" in {
      val req = authorisedAsValidClient(FakeRequest(), mtdItId.value).withFormUrlEncodedBody("confirmTerms" -> "")
      val result = controller.submitConfirmTerms().apply(req)

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.confirmTerms.invalid"))
    }

  }

  "GET /accept-tax-agent-invitation/4 (complete page)" should {
    "show the complete page" in {
      val result = controller.getCompletePage().apply(authorisedAsValidClient(FakeRequest(), mtdItId.value))

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete.title1"))
    }
  }
}
