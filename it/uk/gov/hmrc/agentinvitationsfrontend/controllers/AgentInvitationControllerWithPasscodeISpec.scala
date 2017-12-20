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

import com.github.tomakehurst.wiremock.client.WireMock.{aResponse, get, stubFor, urlEqualTo, equalTo}
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.http.SessionKeys

import scala.concurrent.duration._

class AgentInvitationControllerWithPasscodeISpec extends BaseISpec {

  override implicit lazy val app: Application = appBuilder.build()

  override protected def appBuilder: GuiceApplicationBuilder = {
    new GuiceApplicationBuilder()
      .configure(
        "microservice.services.auth.port" -> wireMockPort,
        "microservice.services.agent-client-authorisation.port" -> wireMockPort,
        "microservice.services.agent-services-account.port" -> wireMockPort,
        "microservice.services.company-auth.login-url" -> wireMockHost,
        "microservice.services.company-auth.port" -> wireMockPort,
        "microservice.services.des.port" -> wireMockPort,
        "microservice.services.agent-fi-relationship.port" -> wireMockPort,
        "microservice.services.agent-invitations-frontend.base-url" -> wireMockBaseUrlAsString,
        "microservice.services.agent-services-account-frontend.external-url" -> wireMockBaseUrlAsString,
        "microservice.services.personal-tax-account.external-url" -> wireMockBaseUrlAsString,
        "auditing.enabled" -> true,
        "auditing.consumer.baseUri.host" -> wireMockHost,
        "auditing.consumer.baseUri.port" -> wireMockPort,
        "features.show-hmrc-mtd-it" -> true,
        "features.show-personal-income" -> true,
        "passcodeAuthentication.enabled" -> true
      )
  }

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  private val arn = Arn("TARN0000001")
  private val timeout = 2.seconds

  "GET /agents/enter-nino" should {
    val showNinoForm = controller.showNinoForm()

    "return 303 for an authorised Agent without OTAC token" in {
      val request = FakeRequest("GET", "/agents/enter-nino?p=foo123")
      val result = showNinoForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result)(timeout).get should be("/verification/otac/login?p=foo123")
      verifyAuthoriseAttempt()
    }

    "return 200 for an authorised Agent with OTAC token" in {
      val request = FakeRequest("GET", "/agents/enter-nino").withSession((SessionKeys.otacToken,"someOtacToken123"))
      stubFor(get(urlEqualTo("/authorise/read/agent-fi-agent-frontend"))
        .withHeader("Otac-Authorization", equalTo("someOtacToken123"))
        .willReturn(aResponse()
          .withStatus(200)))
      val result = showNinoForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("enter-nino.header"))
      verifyAuthoriseAttempt()
    }
  }
}
