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

import org.joda.time.LocalDate
import org.scalatest.words.MatcherWords
import org.scalatestplus.play.OneAppPerSuite
import play.api.Configuration
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.test.FakeRequest
import support.CustomMatchers._
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.InvitationSentPageConfig
import uk.gov.hmrc.play.test.UnitSpec

class InvitationSentSpec extends UnitSpec with MatcherWords with OneAppPerSuite {
  implicit val messages: Messages = Messages(Lang("en"), app.injector.instanceOf[MessagesApi])
  implicit val externalUrls: ExternalUrls = app.injector.instanceOf[ExternalUrls]
  implicit val config: Configuration = app.injector.instanceOf[Configuration]
  implicit val request = FakeRequest()

  val pageConf = InvitationSentPageConfig(
    relativeInvitationUrl = "/invitationUrl",
    continueUrlOpt = None,
    hasContinueUrl = false,
    clientType = "someClientType",
    expiryDate = LocalDate.parse("2001-02-03"),
    agencyEmail = "abc@xyz.com"
  )
  val view = uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.invitation_sent

  "invitations_sent" should {
    "show an appropriate page title" in {
      view(pageConf) should havePageTitle(
        expectedTitleMessageKey = "generic.title",
        expectedMessageParamKeys = Seq(
          "invitation-sent.header",
          "title.suffix.agents"
        ): _*
      )
    }

    "show header and standard paragraphs" in {
      view(pageConf) should containMessages(
        "invitation-sent.header",
        "invitation-sent.l1",
        "invitation-sent.l2"
      )()
    }

    "show the agency email address that will be sent an email upon outcome of the invitation" in {
      view(pageConf) should containMessageWithParams(
        expectedMessageKey = "invitation-sent.email.p",
        expectedMessageParameters = Seq("abc@xyz.com", "3 February 2001"): _*
      )(expectHtmlEscaped = false)
    }

    "show invitation's expiry date" in {
      view(pageConf) should containMessageWithParams(
        expectedMessageKey = "invitation-sent.l3",
        expectedMessageParameters = "3 February 2001"
      )(expectHtmlEscaped = false)
    }

    "show invitation link" in {
      view(pageConf) should containSubstrings(pageConf.relativeInvitationUrl)
    }

    "show content for client type" when {
      "client type is personal" in {
        view(pageConf.copy(clientType = "personal")) should containMessages(
          "invitation-sent.l1.p.personal.personal",
          "invitation-sent.l4.personal.personal"
        )(expectHtmlEscaped = false)
      }
      "client type is business" in {
        view(pageConf.copy(clientType = "business", serviceType = "HMRC-MTD-VAT")) should containMessages(
          "invitation-sent.l1.p.business.HMRC-MTD-VAT",
          "invitation-sent.l4.business.HMRC-MTD-VAT"
        )(expectHtmlEscaped = false)
      }
    }

    "show next action links/buttons" when {
      "there is no continue URL" should {
        val pageConfNoContinueUrl = pageConf.copy(
          continueUrlOpt = None,
          hasContinueUrl = false
        )

        "show the 'Other actions' header" in {
          view(pageConfNoContinueUrl) should containMessages("invitation-sent.header.links")()
        }

        "show a link to start a new authorisation request" in {
          view(pageConfNoContinueUrl) should containLink(
            expectedMessageKey = "invitation-sent.startNewAuthRequest",
            expectedHref = "/invitations/agents/client-type"
          )
        }
        "show a link (not a button) to agent services account home" in {
          view(pageConfNoContinueUrl) should containLink(
            expectedMessageKey = "invitation-sent.continueToASAccount.button",
            expectedHref = s"${externalUrls.agentServicesAccountUrl}/agent-services-account"
          )

          view(pageConfNoContinueUrl) shouldNot containLink(
            expectedMessageKey = "invitation-sent.continueToASAccount.button",
            expectedHref = s"${externalUrls.agentServicesAccountUrl}/agent-services-account",
            expectedClasses = Set("button")
          )
        }

        "show a link to track recent authorisation requests" in {
          view(pageConfNoContinueUrl) should containLink(
            expectedMessageKey = "invitation-sent.trackRequests",
            expectedHref = "/invitations/track"
          )
        }
      }

      "there is a continue URL (from fast track)" should {
        val pageConfContinueUrl = pageConf.copy(
          continueUrlOpt = Some("/continue-some-other-journey"),
          hasContinueUrl = true
        )

        "not show the 'Other actions' header" in {
          view(pageConfContinueUrl) shouldNot containMessages("invitation-sent.header.links")()
        }

        "show a button (not a link) to 'Continue' the journey" in {
          view(pageConfContinueUrl) should containLink(
            expectedMessageKey = "invitation-sent.continueJourney.button",
            expectedHref = "/continue-some-other-journey",
            expectedClasses = Set("button")
          )
        }

        "show a link to track recent authorisation requests, which opens in a new window" in {
          view(pageConfContinueUrl) should containLinkWithSubstring(
            expectedSubstring = messages("invitation-sent.trackRequests") + " " + messages(
              "invitation-sent.new-window"),
            expectedHref = "/invitations/track"
          )
        }
      }
    }
  }

}
