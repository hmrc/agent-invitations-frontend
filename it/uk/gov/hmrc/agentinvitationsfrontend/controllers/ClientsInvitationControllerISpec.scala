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

import play.api.mvc.{Action, AnyContent, Cookie}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.agentInvitationServiceForm
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ClientsInvitationController.confirmAuthorisationForm
import uk.gov.hmrc.agentinvitationsfrontend.models.UserInputNinoAndPostcode
import uk.gov.hmrc.agentinvitationsfrontend.support.TestDataCommonSupport
import uk.gov.hmrc.agentmtdidentifiers.model._

class ClientsInvitationControllerISpec extends TestDataCommonSupport {

  lazy val controller: ClientsInvitationController = app.injector.instanceOf[ClientsInvitationController]

  "GET /:invitationId (landing page)" should {

    "redirect to notFoundInvitation when the invitation ID prefix is not a known service" in {
      val result = controller.start(invalidInvitationId)(FakeRequest())
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }
  }

  "POST /:invitationId (landing page)" should {

    "redirect to notFoundInvitation when the invitation ID prefix is not a known service" in {
      val result =
        controller.submitStart(invalidInvitationId)(FakeRequest())
      status(result) shouldBe SEE_OTHER
    }

    "throw an error when the radio button selection is invalid" in {

    }
  }

  "GET /decide-later/:invitationId" should {
    "redirect to notFoundInvitation when the invitation ID prefix is not a known service" in {
      val strangePrefixInvId = InvitationId("ZTSF4OW9CCRPT")
      val result = controller.getDecideLater(strangePrefixInvId)(FakeRequest())
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }
  }

  "GET /not-sign-up/" should {
    "show not-sign-up page if user does not have a valid enrolment" in {
      val result = controller.notSignedUp(FakeRequest())
      status(result) shouldBe FORBIDDEN
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("not-signed-up.header"),
          htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-signed-up.description"))
      await(bodyOf(result)) should not include htmlEscapedMessage("common.sign-out")
    }

    "show not-sign-up page with signout button if logged in" in {
      val result = controller.notSignedUp(FakeRequest().withCookies(Cookie("mdtp", "authToken=Bearer+")))
      status(result) shouldBe FORBIDDEN
      checkHasClientSignOutUrl(result)
    }
  }

  "GET /not-authorised/" should {
    "show the unauthorised page" in {
      val result = controller.notAuthorised(FakeRequest().withCookies(Cookie("mdtp", "authToken=Bearer+")))
      status(result) shouldBe FORBIDDEN
      checkHtmlResultWithBodyText(
        result,
        hasMessage(
          "generic.title",
          htmlEscapedMessage("not-authorised.header"),
          htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-authorised.description"))
      checkHasClientSignOutUrl(result)
    }
  }

  "GET /incorrect/" should {
    "show incorrect page if user accidentally attempted to respond to another client's invitation" in {
      val result = controller.incorrectInvitation(FakeRequest())
      status(result) shouldBe FORBIDDEN
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("incorrect-invitation.header"),
          htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("incorrect-invitation.description"))
      await(bodyOf(result)) should not include htmlEscapedMessage("common.sign-out")
    }

    "show incorrect page with signout button if logged in" in {
      val result = controller.incorrectInvitation(FakeRequest().withCookies(Cookie("mdtp", "authToken=Bearer+")))
      status(result) shouldBe FORBIDDEN
      checkHasClientSignOutUrl(result)
    }
  }

  "GET /not-found/" should {
    "show not-found page if user responds to an invitation that does not exist" in {
      val result = controller.notFoundInvitation(FakeRequest())
      status(result) shouldBe NOT_FOUND
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("not-found-invitation.header"),
          htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-found-invitation.description"))
      await(bodyOf(result)) should not include htmlEscapedMessage("common.sign-out")
    }

    "show not-found page with signout button if logged in" in {
      val result = controller.notFoundInvitation(FakeRequest().withCookies(Cookie("mdtp", "authToken=Bearer+")))
      status(result) shouldBe NOT_FOUND
      checkHasClientSignOutUrl(result)
    }
  }

  "GET /already-responded/" should {
    "show already-responded page if user responds to an invitation that does not have a status Pending" in {
      val result = controller.invitationAlreadyResponded(FakeRequest())
      status(result) shouldBe FORBIDDEN
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("invitation-already-responded.header"),
          htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-already-responded.description"))
      await(bodyOf(result)) should not include htmlEscapedMessage("common.sign-out")
    }

    "show already-responded page with signout button if logged in" in {
      val result = controller.invitationAlreadyResponded(FakeRequest().withCookies(Cookie("mdtp", "authToken=Bearer+")))
      status(result) shouldBe FORBIDDEN
      checkHasClientSignOutUrl(result)
    }
  }
}
