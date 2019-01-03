/*
 * Copyright 2019 HM Revenue & Customs
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

import org.scalatestplus.play.OneAppPerSuite
import play.api.i18n.MessagesApi
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.auth.core.{InsufficientEnrolments, MissingBearerToken}
import uk.gov.hmrc.auth.otac.{NoOtacTokenInSession, OtacFailureThrowable}
import uk.gov.hmrc.http.BadGatewayException
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.Future

class ErrorHandlerSpec extends UnitSpec with OneAppPerSuite {

  val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val handler: ErrorHandler = app.injector.instanceOf[ErrorHandler]

  "ErrorHandler should show the error page" when {
    "a server error occurs" in {
      val result = handler.onServerError(FakeRequest(), new BadGatewayException(""))

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some(HTML)
      checkIncludesMessages(result, "global.error.500.title", "global.error.500.heading", "global.error.500.message")
    }

    "error occurs due to InsufficientEnrolments" in {
      val result = handler.onServerError(FakeRequest(), new InsufficientEnrolments)

      status(result) shouldBe FORBIDDEN
      contentType(result) shouldBe Some(HTML)
      checkIncludesMessages(result, "global.error.403.title", "global.error.403.heading", "global.error.403.message")
    }

    "error occurs due to InsufficientEnrolments when path contains 'agents' " in {
      val result = handler
        .onServerError(FakeRequest("GET", "http://host:port/invitations/agents/enter-nino"), new InsufficientEnrolments)

      status(result) shouldBe FORBIDDEN
      contentType(result) shouldBe Some(HTML)
      checkIncludesMessages(result, "global.error.403.title", "global.error.403.heading", "global.error.403.message")
    }

    "error occurs due to Otac Failure" in {
      val result = handler.onServerError(FakeRequest(), new OtacFailureThrowable(NoOtacTokenInSession))

      status(result) shouldBe FORBIDDEN
      contentType(result) shouldBe Some(HTML)
      checkIncludesMessages(
        result,
        "global.error.passcode.title",
        "global.error.passcode.heading",
        "global.error.passcode.message")
    }

    "a client error (400) occurs" in {
      val result = handler.onClientError(FakeRequest(), BAD_REQUEST, "")

      status(result) shouldBe BAD_REQUEST
      contentType(result) shouldBe Some(HTML)
      checkIncludesMessages(result, "global.error.400.title", "global.error.400.heading", "global.error.400.message")
    }

    "a client error (404) occurs" in {
      val result = handler.onClientError(FakeRequest(), NOT_FOUND, "")

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some(HTML)
      checkIncludesMessages(result, "global.error.404.title", "global.error.404.heading", "global.error.404.message")
    }
  }

  "ErrorHandler should redirect to GG Login" when {
    "a user attempts to access a page without authentication" in {
      val result = handler.onServerError(FakeRequest(), MissingBearerToken(""))
      val expectedRedirect: String =
        "/gg/sign-in?continue=http%3A%2F%2Flocalhost%3A9448%2F&origin=agent-invitations-frontend"
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe expectedRedirect
    }
  }

  private def checkIncludesMessages(result: Future[Result], messageKeys: String*): Unit =
    messageKeys.foreach { messageKey =>
      messagesApi.isDefinedAt(messageKey) shouldBe true
      contentAsString(result) should include(HtmlFormat.escape(messagesApi(messageKey)).toString)
    }
}
