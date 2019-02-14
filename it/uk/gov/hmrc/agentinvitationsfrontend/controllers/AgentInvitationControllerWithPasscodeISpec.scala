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

import com.github.tomakehurst.wiremock.client.WireMock._
import com.google.inject.AbstractModule
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.agentConfirmationForm
import uk.gov.hmrc.agentinvitationsfrontend.forms.ServiceTypeForm
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentSession, Confirmation}
import uk.gov.hmrc.agentinvitationsfrontend.services.AgentSessionCache
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.logging.SessionId
import uk.gov.hmrc.http.{HeaderCarrier, SessionKeys}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class AgentInvitationControllerWithPasscodeISpec extends BaseISpec {

  override implicit lazy val app: Application = appBuilder.build()

  override protected def appBuilder: GuiceApplicationBuilder =
    new GuiceApplicationBuilder()
      .configure(
        "microservice.services.auth.port"                                    -> wireMockPort,
        "microservice.services.agent-client-authorisation.port"              -> wireMockPort,
        "microservice.services.agent-services-account.port"                  -> wireMockPort,
        "microservice.services.company-auth.login-url"                       -> wireMockHost,
        "microservice.services.company-auth.port"                            -> wireMockPort,
        "microservice.services.des.port"                                     -> wireMockPort,
        "microservice.services.agent-fi-relationship.port"                   -> wireMockPort,
        "microservice.services.agent-invitations-frontend.external-url"      -> wireMockBaseUrlAsString,
        "microservice.services.agent-services-account-frontend.external-url" -> wireMockBaseUrlAsString,
        "microservice.services.personal-tax-account.external-url"            -> wireMockBaseUrlAsString,
        "auditing.enabled"                                                   -> true,
        "auditing.consumer.baseUri.host"                                     -> wireMockHost,
        "auditing.consumer.baseUri.port"                                     -> wireMockPort,
        "features.show-hmrc-mtd-it"                                          -> true,
        "features.show-personal-income"                                      -> true,
        "features.show-hmrc-mtd-vat"                                         -> true,
        "features.enable-fast-track"                                         -> true,
        "passcodeAuthentication.enabled"                                     -> true
      )
      .overrides(new TestGuiceModule)

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    testAgentSessionCache.clear()
  }

  private class TestGuiceModule extends AbstractModule {
    override def configure(): Unit = {
      bind(classOf[AgentSessionCache]).toInstance(testAgentSessionCache)
    }
  }

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  private val timeout = 2.seconds

  "GET /agents/client-type" should {
    "return 303 for an authorised Agent without OTAC token but with passcode" in {
      val request = FakeRequest("GET", "/agents/client-type?p=foo123")
      val result = controller.showClientType(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result)(timeout).get should be("/verification/otac/login?p=foo123")
      verifyNoAuthoriseAttempt()
    }
  }

  "GET /agents/select-service" should {
    "return 303 for an authorised Agent without OTAC token but with passcode" in {
      val request = FakeRequest("GET", "/agents/select-service?p=foo123")
      val result = controller.showSelectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result)(timeout).get should be("/verification/otac/login?p=foo123")
      verifyNoAuthoriseAttempt()
    }

    "return 200 for an authorised whitelisted Agent with OTAC session key in select service page" in {
      testAgentSessionCache.save(AgentSession(personal))
      val request =
        FakeRequest("GET", "/agents/select-service").withSession((SessionKeys.otacToken, "someOtacToken123"))
      stubFor(
        get(urlEqualTo("/authorise/read/agent-fi-agent-frontend"))
          .withHeader("Otac-Authorization", equalTo("someOtacToken123"))
          .willReturn(aResponse()
            .withStatus(200)))
      val result = controller.showSelectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("select-service.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("select-service.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("personal-select-service.personal-income-viewer"))
      verifyAuthoriseAttempt()
    }

    "return 200 and don't show the IRV option for an authorised non-whitelisted agent without passcode or OTAC session key in select service page" in {
      testAgentSessionCache.save(AgentSession(personal))
      val request = FakeRequest("GET", "/agents/select-service")
      stubFor(
        get(urlEqualTo("/authorise/read/agent-fi-agent-frontend"))
          .willReturn(aResponse()
            .withStatus(200)))
      val result = controller.showSelectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("select-service.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("select-service.header"))
      checkHtmlResultWithoutBodyText(result, htmlEscapedMessage("personal-select-service.personal-income-viewer"))
      verifyAuthoriseAttempt()
    }
  }

  "POST to /select-service" when {
    "service is IRV" should {
      "redirect to /identify-client if user is whitelisted (has valid OTAC session key)" in {
        testAgentSessionCache.save(AgentSession(personal))
        stubFor(
          get(urlEqualTo("/authorise/read/agent-fi-agent-frontend"))
            .withHeader("Otac-Authorization", equalTo("someOtacToken123"))
            .willReturn(aResponse()
              .withStatus(200)))

        val request =
          FakeRequest("POST", "/agents/select-service").withSession(SessionKeys.otacToken -> "someOtacToken123")
        val serviceForm = ServiceTypeForm.form.fill("PERSONAL-INCOME-RECORD")
        val result = controller.submitSelectService(
          authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

        status(result) shouldBe 303
        redirectLocation(result)(timeout).get shouldBe "/invitations/agents/identify-client"
        verifyAuthoriseAttempt()
      }

      "return BAD_REQUEST if user is not whitelisted (has no OTAC key in session)" in {
        testAgentSessionCache.save(AgentSession(personal))
        val request = FakeRequest("POST", "/agents/select-service")
        val serviceForm = ServiceTypeForm.form.fill("PERSONAL-INCOME-RECORD")
        val result = controller.submitSelectService(
          authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

        status(result) shouldBe 400
      }
    }

    "service is ITSA" should {
      "not be restricted by whitelisting" in {
        val request = FakeRequest("POST", "/agents/select-service")
        testAgentSessionCache.save(AgentSession(personal))
        val serviceForm = ServiceTypeForm.form.fill("HMRC-MTD-IT")
        val result = controller.submitSelectService(
          authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*), arn.value))

        status(result) shouldBe 303
        redirectLocation(result)(timeout).get shouldBe "/invitations/agents/identify-client"
      }
    }

    "service is VAT" should {
      "not be restricted by whitelisting" in {
        val request = FakeRequest("POST", "/agents/select-service")
        testAgentSessionCache.save(AgentSession(business))
        val confirmForm = agentConfirmationForm("error").fill(Confirmation(true))
        val result = controller.submitSelectService(
          authorisedAsValidAgent(request.withFormUrlEncodedBody(confirmForm.data.toSeq: _*), arn.value))

        status(result) shouldBe 303
        redirectLocation(result)(timeout).get shouldBe "/invitations/agents/identify-client"
      }
    }
  }
}
