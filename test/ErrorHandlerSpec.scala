/*
 * Copyright 2023 HM Revenue & Customs
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

import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.Application
import play.api.i18n.{Lang, MessagesApi}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.test.Helpers
import play.twirl.api.HtmlFormat
import support.{LogCapturing, UnitSpec}
import uk.gov.hmrc.auth.otac.{NoOtacTokenInSession, OtacFailureThrowable}
import uk.gov.hmrc.http.BadGatewayException

import scala.concurrent.Future

class ErrorHandlerSpec extends UnitSpec with GuiceOneServerPerSuite with LogCapturing {

  override implicit lazy val app: Application = appBuilder.build()

  protected def appBuilder: GuiceApplicationBuilder =
    new GuiceApplicationBuilder()
      .configure(
        "metrics.jvm"     -> false,
        "metrics.logback" -> false
      )

  val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val handler: ErrorHandler = app.injector.instanceOf[ErrorHandler]
  implicit val lang: Lang = Lang("en")

  "ErrorHandler should show the error page" when {
    "a server error occurs" in {
      val result = handler.onServerError(FakeRequest(), new BadGatewayException(""))

      status(result) shouldBe INTERNAL_SERVER_ERROR
      Helpers.contentType(result) shouldBe Some(HTML)
      checkIncludesMessages(result, "global.error.500.title", "global.error.500.heading", "global.error.500.message")
    }

    "error occurs due to Otac Failure" in {
      val result = handler.onServerError(FakeRequest(), new OtacFailureThrowable(NoOtacTokenInSession))

      status(result) shouldBe FORBIDDEN
      Helpers.contentType(result) shouldBe Some(HTML)
      checkIncludesMessages(result, "global.error.passcode.title", "global.error.passcode.heading", "global.error.passcode.message")
    }

    "a client error (400) occurs" in {
      val result = handler.onClientError(FakeRequest(), BAD_REQUEST, "")

      status(result) shouldBe BAD_REQUEST
      Helpers.contentType(result) shouldBe Some(HTML)
      checkIncludesMessages(result, "global.error.400.title", "global.error.400.heading", "global.error.400.message")
    }

    "a client error (404) occurs" in {
      val result = handler.onClientError(FakeRequest(), NOT_FOUND, "")

      status(result) shouldBe NOT_FOUND
      Helpers.contentType(result) shouldBe Some(HTML)
      checkIncludesText(result, "<p>If you typed the web address, check it is correct.</p>")
      checkIncludesMessages(result, "global.error.404.title", "global.error.404.heading")
    }

    """standardErrorTemplate shows up with logger.error("some error")""" in {
      withCaptureOfLoggingFrom(handler.theLogger) { logEvents =>
        handler.standardErrorTemplate("", "", "some error")(FakeRequest())
        logEvents.count(_.getMessage.contains(s"some error")) shouldBe 1
      }
    }
  }

  private def checkIncludesMessages(result: Future[Result], messageKeys: String*): Unit =
    messageKeys.foreach { messageKey =>
      messagesApi.isDefinedAt(messageKey) shouldBe true
      Helpers.contentAsString(result) should include(HtmlFormat.escape(messagesApi(messageKey)).toString)
    }

  private def checkIncludesText(result: Future[Result], messageKeys: String*): Unit =
    messageKeys.foreach { messageKey =>
      Helpers.contentAsString(result) should include(messageKey.toString)
    }
}
