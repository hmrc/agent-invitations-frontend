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

package views.agents

import org.scalatest.words.MatcherWords
import org.scalatestplus.play.OneAppPerSuite
import play.api.Configuration
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.mvc.Call
import play.api.test.FakeRequest
import support.CustomMatchers._
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.controllers.routes
import uk.gov.hmrc.play.test.UnitSpec

class FastTrackNoMatchSpec extends UnitSpec with MatcherWords with OneAppPerSuite {
  implicit val messages: Messages = Messages(Lang("en"), app.injector.instanceOf[MessagesApi])
  implicit val externalUrls: ExternalUrls = app.injector.instanceOf[ExternalUrls]
  implicit val config: Configuration = app.injector.instanceOf[Configuration]
  implicit val request = FakeRequest()

  val view = uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.not_matched

  "not_matched" should {
    val callForTryAgain = Call("GET", "/some-try-again-uri")
    def renderedView = view(
      hasJourneyCache = false,
      tryAgainCall = callForTryAgain,
      reviewAuthsCallOpt = None
    )

    "show an appropriate page title" in {
      renderedView should havePageTitle(
        expectedTitleMessageKey = "generic.title",
        expectedMessageParamKeys = Seq(
          "not-matched.header",
          "title.suffix.agents"
        ): _*
      )
    }

    "show header and standard paragraphs" in {
      renderedView should containMessages(
        "not-matched.header",
        "not-matched.description",
        "not-matched.advice"
      )()
    }

    "show a button link to try again" in {
      renderedView should containLink(
        expectedMessageKey = "not-matched.button",
        expectedHref = callForTryAgain.url
      )
    }

    "maybe show a link to review authorisations" when {
      "hasJourneyCache is true" when {
        "reviewAuthsCallOpt is a None, show the link pointing to the default showReviewAuthorisations URI" in {
          view(
            hasJourneyCache = true,
            tryAgainCall = callForTryAgain,
            reviewAuthsCallOpt = None
          ) should containLink(
            expectedMessageKey = "review-auths.link",
            expectedHref = routes.AgentInvitationJourneyController.showReviewAuthorisations().url
          )
        }
        "reviewAuthsCallOpt is a Some, show the link pointing to the custom URI" in {
          view(
            hasJourneyCache = true,
            tryAgainCall = callForTryAgain,
            reviewAuthsCallOpt = Some(Call("GET", "/other-place-to-review-authorisations"))
          ) should containLink(
            expectedMessageKey = "review-auths.link",
            expectedHref = "/other-place-to-review-authorisations"
          )
        }
      }

      "hasJourneyCache is false, don't show the link" in {
        view(
          hasJourneyCache = false,
          tryAgainCall = callForTryAgain,
          reviewAuthsCallOpt = None
        ) shouldNot containLink(
          expectedMessageKey = "review-auths.link",
          expectedHref = routes.AgentInvitationJourneyController.showReviewAuthorisations().url
        )
      }
    }
  }
}
