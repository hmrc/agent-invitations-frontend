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

package views

import org.jsoup.Jsoup
import org.scalatestplus.play.MixedPlaySpec
import play.api.Configuration
import play.api.i18n.Messages.Implicits.applicationMessages
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.twirl.api.Html
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.views.html.govuk_wrapper_Scope0.govuk_wrapper_Scope1.govuk_wrapper
import uk.gov.hmrc.agentinvitationsfrontend.views.html.main_template_Scope0.main_template_Scope1.main_template

class ViewSpec extends MixedPlaySpec {

  "main_template view" should {
    "render all supplied arguments" in new App {

      val view = new main_template

      val externalUrlsConfig: ExternalUrls = app.injector.instanceOf[ExternalUrls]
      val config: Configuration = app.injector.instanceOf[Configuration]

      val html = view.render(
        title = "My custom page title",
        sidebarLinks = Some(Html("My custom sidebar links")),
        contentHeader = Some(Html("My custom content header")),
        bodyClasses = Some("my-custom-body-class"),
        mainClass = Some("my-custom-main-class"),
        scriptElem = Some(Html("My custom script")),
        isAgent = true,
        bannerTitle = "Banner title",
        completedInviteGoSurvey = Some(true),
        gaCalls = None,
        mainDataAttributes = None,
        hasTimeout = true,
        mainContent = Html("My custom main content HTML"),
        messages = applicationMessages,
        request = FakeRequest(),
        configuration = config,
        externalUrls = externalUrlsConfig
      )

      val content = contentAsString(html)

      content must {
        include("My custom page title")
        include("My custom sidebar links") and
          include("My custom content header") and
          include("my-custom-body-class") and
          include("my-custom-main-class") and
          include("My custom script") and
          include("Banner title")
        include("My custom main content HTML")
      }

      val doc = Jsoup.parse(contentAsString(html))

      doc.getElementById("timeoutDialog").isBlock mustBe true

      val html2 = view.f(
        "My custom page title",
        Some(Html("My custom sidebar links")),
        Some(Html("My custom content header")),
        Some("my-custom-body-class"),
        Some("my-custom-main-class"),
        Some(Html("My custom script")),
        true,
        "Banner title",
        Some(true),
        None,
        None,
        true
      )(Html("My custom main content HTML"))(applicationMessages, FakeRequest(), config, externalUrlsConfig)
      contentAsString(html2) mustBe (content)
    }
  }

  "govuk wrapper view" should {
    "render all of the supplied arguments" in new App {

      val externalUrlsConfig: ExternalUrls = app.injector.instanceOf[ExternalUrls]
      val config: Configuration = app.injector.instanceOf[Configuration]

      val externalUrls: ExternalUrls = app.injector.instanceOf[ExternalUrls]

      val html = new govuk_wrapper().render(
        title = "My custom page title",
        mainClass = Some("my-custom-main-class"),
        mainDataAttributes = Some(Html("myCustom=\"attributes\"")),
        bodyClasses = Some("my-custom-body-class"),
        sidebar = Html("My custom sidebar"),
        contentHeader = Some(Html("My custom content header")),
        mainContent = Html("My custom main content"),
        serviceInfoContent = Html("My custom service info content"),
        scriptElem = Some(Html("My custom script")),
        gaCalls = None,
        gaCode = Seq("My custom GA code"),
        cookies = None,
        isAgent = true,
        bannerTitle = "Banner title",
        completedInviteGoSurvey = Some(true),
        messages = applicationMessages,
        configuration = config,
        externalUrls = externalUrlsConfig
      )

      val content = contentAsString(html)

      content must {
        include("My custom page title") and
          include("my-custom-main-class") and
          include("myCustom=\"attributes\"") and
          include("my-custom-body-class") and
          include("My custom sidebar") and
          include("My custom content header") and
          include("My custom main content") and
          include("My custom service info content")
        include("My custom script") and
          include("Banner title")
      }

      val html2 = new govuk_wrapper().f(
        "My custom page title",
        Some("my-custom-main-class"),
        Some(Html("myCustom=\"attributes\"")),
        Some("my-custom-body-class"),
        Html("My custom sidebar"),
        Some(Html("My custom content header")),
        Html("My custom main content"),
        Html("My custom service info content"),
        Some(Html("My custom script")),
        None,
        Seq("My custom GA code"),
        None,
        true,
        "Banner title",
        None
      )(applicationMessages, config, externalUrlsConfig)
      contentAsString(html2) mustBe (content)
    }
  }

}
