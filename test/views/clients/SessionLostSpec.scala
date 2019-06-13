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

package views.clients

import org.scalatest.words.MatcherWords
import org.scalatestplus.play.OneAppPerSuite
import play.api.Configuration
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.test.FakeRequest
import support.CustomMatchers._
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.play.test.UnitSpec

class SessionLostSpec extends UnitSpec with MatcherWords with OneAppPerSuite {
  implicit val messages: Messages = Messages(Lang("en"), app.injector.instanceOf[MessagesApi])
  implicit val externalUrls: ExternalUrls = app.injector.instanceOf[ExternalUrls]
  implicit val config: Configuration = app.injector.instanceOf[Configuration]
  implicit val request = FakeRequest()

  val view = uk.gov.hmrc.agentinvitationsfrontend.views.html.clients.session_lost

  "session_lost" should {
    "show an appropriate page title" in {
      view() should havePageTitle(
        expectedTitleMessageKey = "generic.title",
        expectedMessageParamKeys = Seq(
          "session-lost-client.header",
          "title.suffix.client"
        ): _*
      )
    }

    "show header and standard paragraphs" in {
      view() should containMessages(
        "session-lost-client.header",
        "session-lost-client.description.1",
        "session-lost-client.description.2",
        "session-lost-client.description.3"
      )()
    }

    "show message with a link to /manage-agents" in {
      view() should containMessageWithParams(
        expectedMessageKey = "session-lost-client.manage-agents-link",
        expectedMessageParameters = externalUrls.agentClientManagementUrl
      )(expectHtmlEscaped = false)
    }
  }
}
