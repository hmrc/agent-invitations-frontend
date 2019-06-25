package uk.gov.hmrc.agentinvitationsfrontend.controllers.retired

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

import java.util.UUID

import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{AuthBehaviours, FeatureFlags, retired}
import uk.gov.hmrc.agentinvitationsfrontend.forms.VatClientForm
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.duration._

class AgentInvitationControllerISpec extends BaseISpec with AuthBehaviours {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId(UUID.randomUUID().toString)))

  "GET /agents/" should {
    "redirect to /agent/select-service" in {
      val result = controller.agentsRoot(FakeRequest())
      status(result) shouldBe 303
      val timeout = 2.seconds
      redirectLocation(result)(timeout).get shouldBe retired.routes.AgentsInvitationController.showClientType().url
    }
  }

  "GET /agents/client-type" should {
    val request = FakeRequest("GET", "/agents/client-type")
    val selectClientType = controller.showClientType()

    "return 200 for an Agent with HMRC-AS-AGENT enrolment" in {
      val invitation =
        AgentSession(Some(personal), Some(serviceITSA), Some("ni"), Some(validNino.value), Some("AB101AB"))
      await(sessionStore.save(invitation))
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
      checkResultContainsBackLink(result, s"http://localhost:$wireMockPort/agent-services-account")
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
      await(sessionStore.fetch) shouldBe Some(AgentSession())
    }

    "return 200 for an Agent with HMRC-AS-AGENT enrolment when coming from fast track" in {
      val invitation =
        AgentSession(
          Some(personal),
          Some(serviceITSA),
          Some("ni"),
          Some(validNino.value),
          Some("AB101AB"),
          fromFastTrack = true)
      await(sessionStore.save(invitation))
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
      await(sessionStore.fetch) shouldBe Some(AgentSession())
    }

    behave like anAuthorisedAgentEndpoint(request, selectClientType)
  }

  "POST /agents/client-type" should {
    val request = FakeRequest("POST", "/agents/client-type")
    val submitClientType = controller.submitClientType()

    "return 200 for authorised Agent with no selected service and show error on the page" in {
      val result =
        submitClientType(authorisedAsValidAgent(request.withFormUrlEncodedBody("clientType" -> ""), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("client-type.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-type.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client.type.invalid"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    behave like anAuthorisedAgentEndpoint(request, submitClientType)
  }

  "GET /agents/select-service" should {
    val request = FakeRequest("GET", "/agents/select-service")
    val selectService = controller.showSelectService()

    "return 200 for an Agent with HMRC-AS-AGENT enrolment for personal" in {
      val invitation =
        AgentSession(Some(personal), Some(serviceITSA), Some("ni"), Some(validNino.value), Some("AB101AB"))
      await(sessionStore.save(invitation))
      val result = selectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("select-service.header"),
          htmlEscapedMessage("title.suffix.agents")),
        htmlEscapedMessage("select-service.header"),
        htmlEscapedMessage("personal-select-service.itsa"),
        htmlEscapedMessage("personal-select-service.personal-income-viewer"),
        htmlEscapedMessage("select-service.vat"),
        htmlEscapedMessage("select-service.alternative"),
        htmlEscapedMessage("select-service.alt-suggestion")
      )
      checkResultContainsBackLink(result, "/invitations2/agents/client-type")
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "return 200 for an Agent with HMRC-AS-AGENT enrolment for business" in {
      val invitation =
        AgentSession(Some(business), Some(serviceVAT), Some("vrn"), Some(validNino.value), Some("1234567"))
      await(sessionStore.save(invitation))
      val result = selectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("business-select-service.header"),
        htmlEscapedMessage("business-select-service.yes"),
        htmlEscapedMessage("business-select-service.no"),
        htmlEscapedMessage("select-service.alternative"),
        htmlEscapedMessage("select-service.alt-suggestion.vat-only")
      )

      val govUkGuidanceUrl = "https://www.gov.uk/guidance/client-authorisation-an-overview"
      checkHtmlResultWithBodyText(result, hasMessage("select-service.guidance", govUkGuidanceUrl))

      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "redirect to select client type page when the client type in the cache is not supported" in {
      val invitation =
        AgentSession(None, Some(serviceVAT), Some("vrn"), Some(validNino.value), Some("1234567"))
      await(sessionStore.save(invitation))
      val result = selectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(retired.routes.AgentsInvitationController.showClientType().url)
    }

    "redirect to select client type page when there is nothing in the cache" in {
      val result = selectService(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(retired.routes.AgentsInvitationController.showClientType().url)
    }

    behave like anAuthorisedAgentEndpoint(request, selectService)
  }

  "POST /agents/select-personal-service" should {
    val request = FakeRequest("POST", "/agents/select-personal-service")
    val submitService = controller.submitSelectPersonalService()

    "show errors on the page if the form contains invalid service selection" in {
      await(sessionStore.save(AgentSession(Some(personal))))
      val result = submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody("serviceType" -> ""), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("select-service.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("select-service.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("service.type.invalid"))
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }
  }

  "GET /agents/identify-client" should {
    val request = FakeRequest("GET", "/agents/identify-client")
    val showIdentifyClientForm = controller.showIdentifyClient()

    behave like anAuthorisedAgentEndpoint(request, showIdentifyClientForm)

    "return 303 redirect to /agents/select-service for an Agent with HMRC-AS-AGENT enrolment when service is not available" in {
      await(sessionStore.save(AgentSession(None, Some("UNSUPPORTED_SERVICE"))))
      val result = showIdentifyClientForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(retired.routes.AgentsInvitationController.showSelectService().url)
    }

    "return 303 redirect to /agents/client-type for an Agent with HMRC-AS-AGENT enrolment when service is not supported" in {
      await(sessionStore.save(AgentSession(None, None)))
      val result = showIdentifyClientForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(retired.routes.AgentsInvitationController.showSelectService().url)
    }

    "throw exception when there is no content in the cache" in {
      await(sessionStore.save(AgentSession()))
      val result = showIdentifyClientForm(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(retired.routes.AgentsInvitationController.showSelectService().url)
    }
  }

  "POST /agents/identify-client" when {
    val request = FakeRequest("POST", "/agents/identify-itsa-client")
    val submitIdentifyClient = controller.submitIdentifyClientItsa()

    behave like anAuthorisedAgentEndpoint(request, submitIdentifyClient)
  }

  "GET /agents/invitation-sent" should {
    val request = FakeRequest("GET", "/agents/invitation-sent")
    val invitationSent = controller.showInvitationSent()

    "return exception when no invitation id and deadline found" in {
      val result = invitationSent(authorisedAsValidAgent(request, arn.value))

      an[RuntimeException] should be thrownBy await(result)
    }

    behave like anAuthorisedAgentEndpoint(request, invitationSent)
  }

  "GET /agents/not-signed-up" should {
    val request = FakeRequest("GET", "/agents/not-signed-up")
    val notEnrolled = controller.notSignedUp()
    val featureFlags = FeatureFlags()

    "return 5xx for Unsupported service" in {
      await(sessionStore.save(AgentSession(None, Some("UNSUPPORTED_SERVICE"))))
      val unsupportedForm =
        VatClientForm.form.fill(VatClient("123456789", ""))

      intercept[Exception] {
        await(
          notEnrolled(
            authorisedAsValidAgent(request.withFormUrlEncodedBody(unsupportedForm.data.toSeq: _*), arn.value)))
      }.getMessage shouldBe "Unsupported Service: UNSUPPORTED_SERVICE"
    }

    "return 5xx when there is nothing in the cache" in {
      sessionStore.delete()
      intercept[Exception] {
        await(notEnrolled(authorisedAsValidAgent(request, arn.value)))
      }.getMessage shouldBe "Cached session state expected but not found"
    }

    behave like anAuthorisedAgentEndpoint(request, notEnrolled)
  }

  def noKeyStoreCacheFound(request: FakeRequest[AnyContentAsEmpty.type], action: Action[AnyContent]) =
    "return 303, redirect to select-service when no keystore cache is found" in {
      val result = await(action(authorisedAsValidAgent(request, arn.value)))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(retired.routes.AgentsInvitationController.showSelectService().url)
    }
}
