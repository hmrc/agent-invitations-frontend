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

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import org.joda.time.LocalDate
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.audit.AgentInvitationEvent
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.models.{CurrentInvitationInput, UserInputNinoAndPostcode, UserInputVrnAndRegDate}
import uk.gov.hmrc.agentinvitationsfrontend.support.{BaseISpec, TestDataCommonSupport}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

class AgentInvitationControllerISpec extends BaseISpec with AuthBehaviours {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  "GET /agents/" should {
    "redirect to /agent/select-service" in {
      val result = controller.agentsRoot(FakeRequest())
      status(result) shouldBe 303
      val timeout = 2.seconds
      redirectLocation(result)(timeout).get shouldBe routes.AgentsInvitationController.selectClientType().url
    }
  }

  "GET /agents/client-type" should {
    val request = FakeRequest("GET", "/agents/client-type")
    val selectClientType = controller.selectClientType()

    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
      val invitation =
        CurrentInvitationInput(personal, serviceITSA, "ni", validNino.value, Some("AB101AB"))
      testFastTrackCache.save(invitation)
      val result = selectClientType(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("client-type.header"),
          htmlEscapedMessage("title.suffix.agents")),
        htmlEscapedMessage("client-type.header"),
        hasMessage("client-type.p1")
      )
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
      await(testFastTrackCache.fetch()).get shouldBe CurrentInvitationInput()
    }

    behave like anAuthorisedAgentEndpoint(request, selectClientType)
  }

  "POST /agents/client-type" should {
    val request = FakeRequest("POST", "/agents/client-type")
    val submitClientType = controller.submitClientType()

    "return 200 for authorised Agent with no selected service and show error on the page" in {
      val result = submitClientType(authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> ""), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("client-type.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-type.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.client-type.required"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedAgentEndpoint(request, submitClientType)
  }

  "GET /agents/select-service" should {
    val request = FakeRequest("GET", "/agents/select-service")
    val selectService = controller.selectService()

    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
      val invitation =
        CurrentInvitationInput(personal, serviceITSA, "ni", validNino.value, Some("AB101AB"))
      testFastTrackCache.save(invitation)
      val result = selectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("select-service.header"),
          htmlEscapedMessage("title.suffix.agents")),
        htmlEscapedMessage("select-service.header"),
        htmlEscapedMessage("select-service.itsa"),
        htmlEscapedMessage("select-service.personal-income-viewer"),
        htmlEscapedMessage("select-service.vat")
      )
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("select-service.alternative"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedAgentEndpoint(request, selectService)
  }

  "POST /agents/select-service" should {
    val request = FakeRequest("POST", "/agents/select-service")
    val submitService = controller.submitService()

    "return 200 for authorised Agent with no selected service and show error on the page" in {
      val result = submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody("service" -> ""), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("select-service.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("select-service.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.service.required"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }
    behave like anAuthorisedAgentEndpoint(request, submitService)
  }

  "GET /agents/identify-client" should {
    val request = FakeRequest("GET", "/agents/identify-client")
    val showIdentifyClientForm = controller.showIdentifyClientForm()

    behave like anAuthorisedAgentEndpoint(request, showIdentifyClientForm)

    "return 303 redirect to /agents/client-type for an Agent with HMRC-AS-AGENT enrolment when service is not available" in {
      val result = showIdentifyClientForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.selectClientType().url)
    }

    "return 303 redirect to /agents/client-type for an Agent with HMRC-AS-AGENT enrolment when service is not supported" in {
      testFastTrackCache.save(CurrentInvitationInput("UNSUPPORTED_CLIENT_TYPE", "UNSUPPORTED_SERVICE"))
      val result = showIdentifyClientForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.selectClientType().url)
    }

    "throw exception when there is no content in the cache" in {
      testFastTrackCache.save(CurrentInvitationInput())
      val result = showIdentifyClientForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.selectClientType().url)
    }
  }

  "POST /agents/identify-client" when {
    val request = FakeRequest("POST", "/agents/identify-client")
    val submitIdentifyClient = controller.submitIdentifyClient()

    behave like anAuthorisedAgentEndpoint(request, submitIdentifyClient)
  }

  "GET /agents/invitation-sent" should {
    val request = FakeRequest("GET", "/agents/invitation-sent")
    val invitationSent = controller.invitationSent()

    "return exception when no invitation id and deadline found" in {
      val result = invitationSent(authorisedAsValidAgent(request, arn.value))

      an[RuntimeException] should be thrownBy await(result)
    }

    behave like anAuthorisedAgentEndpoint(request, invitationSent)
  }

  "GET /agents/not-enrolled" should {
    val request = FakeRequest("GET", "/agents/not-enrolled")
    val notEnrolled = controller.notEnrolled()
    val featureFlags = FeatureFlags()

    "return 5xx for Unsupported service" in {
      testFastTrackCache.save(CurrentInvitationInput("UNSUPPORTED_CLIENT_TYPE", "UNSUPPORTED_SERVICE"))
      val unsupportedForm =
        agentInvitationIdentifyClientFormVat(featureFlags).fill(UserInputVrnAndRegDate("","UNSUPPORTED", None, None))

      intercept[Exception] {
        await(
          notEnrolled(
            authorisedAsValidAgent(request.withFormUrlEncodedBody(unsupportedForm.data.toSeq: _*), arn.value)))
      }.getMessage shouldBe "Unsupported Service"
    }

    "return 5xx when there is nothing in the cache" in {
      intercept[Exception] {
        await(notEnrolled(authorisedAsValidAgent(request, arn.value)))
      }.getMessage shouldBe "Empty Cache"
    }

    behave like anAuthorisedAgentEndpoint(request, notEnrolled)
  }

  "GET /agents/not-matched" should {
    val request = FakeRequest("GET", "/agents/not-matched")
    val notMatched = controller.notMatched()

    behave like anAuthorisedAgentEndpoint(request, notMatched)
  }

  def noKeyStoreCacheFound(request: FakeRequest[AnyContentAsEmpty.type], action: Action[AnyContent]) =
    "return 303, redirect to select-service when no keystore cache is found" in {
      val result = await(action(authorisedAsValidAgent(request, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.selectService().url)
    }
}
