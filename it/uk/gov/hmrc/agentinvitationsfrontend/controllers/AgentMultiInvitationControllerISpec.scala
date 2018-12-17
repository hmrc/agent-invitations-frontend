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

import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentMultiAuthorisationJourneyState, AuthorisationRequest, CurrentAuthorisationRequest}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class AgentMultiInvitationControllerISpec extends BaseISpec with AuthBehaviours {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]

  lazy val errorController: AgentsErrorController = app.injector.instanceOf[AgentsErrorController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  "GET /agents/select-service" should {
    val request = FakeRequest("GET", "/agents/select-service")

    "show the select personal service page with review authorisations link if there is content in the authorisationRequest cache" in {
      testAgentMultiAuthorisationJourneyStateCache.save(
        AgentMultiAuthorisationJourneyState(
          "personal",
          Set(AuthorisationRequest("Gareth Gates", serviceITSA, mtdItId.value))))
      val result = controller.selectService()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Return to authorisation requests")
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "show the select business service page with review authorisations link if there is content in the authorisationRequest cache" in {
      testAgentMultiAuthorisationJourneyStateCache.save(
        AgentMultiAuthorisationJourneyState(
          "business",
          Set(AuthorisationRequest("Gareth Gates", serviceVAT, validVrn.value))))
      val result = controller.selectService()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "show the business select service page if there is not content in the authorisationCache but is in the fastTrackCache" in {
      testCurrentAuthorisationRequestCache.save(
        CurrentAuthorisationRequest(
          Some("business"),
          serviceITSA,
          "ni",
          validNino.value,
          Some(validPostcode),
          fromFastTrack))
      val result = controller.selectService()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "show the select service page if there is an unsupported client type in the authorisationRequest cache" in {
      testAgentMultiAuthorisationJourneyStateCache.save(
        AgentMultiAuthorisationJourneyState(
          "foo",
          Set(AuthorisationRequest("Gareth Gates", serviceVAT, validVrn.value))))
      val result = controller.selectService()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showClientType().url)
      verifyAuthoriseAttempt()
    }
  }

  "GET /agents/review-authorisations" should {
    val request = FakeRequest("GET", "/agents/review-authorisation")

    "show the review authorisations page if there is a single item in the authorisationRequest cache" in {
      testAgentMultiAuthorisationJourneyStateCache.save(
        AgentMultiAuthorisationJourneyState(
          "personal",
          Set(AuthorisationRequest("Gareth Gates", serviceITSA, mtdItId.value))))
      val result = controller.showReviewAuthorisations()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Review your authorisation requests",
        "You have added 1 authorisation request.",
        "Report their income and expenses through software",
        "Gareth Gates",
        "Do you need to add another authorisation for this client?"
      )
      verifyAuthoriseAttempt()
    }

    "show the review authorisations page if there are multiple items in the authorisationRequest cache" in new AgentAuthorisationFullCacheScenario {

      val result = controller.showReviewAuthorisations()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Review your authorisation requests",
        "You have added 3 authorisation requests.",
        "name may be recorded differently in each service.",
        "Report their income and expenses through software",
        "Viewing their PAYE income record",
        "Report their VAT returns through software",
        "Gareth Gates",
        "Sara Vaterloo",
        "Do you need to add another authorisation for this client?"
      )
      checkHtmlResultWithNotBodyText(result, "Malcolm Pirson")
      verifyAuthoriseAttempt()
    }

    "redirect to the all authorisation removed page if there are no authorisations in the cache" in {
      testAgentMultiAuthorisationJourneyStateCache.save(AgentMultiAuthorisationJourneyState("personal", Set.empty))

      val result = controller.showReviewAuthorisations()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.allAuthorisationsRemoved().url)
    }

    "redirect to select clientType page is there is nothing in the cache" in {
      val result = controller.showReviewAuthorisations()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showClientType().url)
    }
  }

  "POST /agents/review-authorisations" should {
    val request = FakeRequest("POST", "/agents/review-authorisation")
    "Redirect to select service if YES is selected on the review-authorisations page" in new AgentAuthorisationFullCacheScenario {

      val result = controller.submitReviewAuthorisations()(
        authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "true"))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.selectService().url)
    }

    "Redirect to complete if NO is selected and all invitation creation is successful" in new AgentAuthorisationFullCacheScenario {
      givenInvitationCreationSucceeds(
        arn,
        validNino.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        "HMRC-MTD-IT",
        "NI")
      givenInvitationCreationSucceeds(arn, validNino.value, invitationIdPIR, validNino.value, "ni", servicePIR, "NI")
      givenInvitationCreationSucceeds(
        arn,
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        identifierVAT)
      givenAgentReference(arn, uid, "personal")

      val result = controller.submitReviewAuthorisations()(
        authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "false"))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.invitationSent().url)
    }

    "Redirect to create authorisation failed error page if NO is selected and all invitation creations fail" in new AgentAuthorisationFullCacheScenario {
      givenInvitationCreationFails(arn)

      val result = controller.submitReviewAuthorisations()(
        authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "false"))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsErrorController.allCreateAuthorisationFailed.url)
    }

    "What to do if NO is selected and some invitation creations fail" in new AgentAuthorisationFullCacheScenario {
      givenInvitationCreationSucceeds(
        arn,
        validNino.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        "HMRC-MTD-IT",
        "NI")
      givenInvitationCreationSucceeds(arn, validNino.value, invitationIdPIR, validNino.value, "ni", servicePIR, "NI")
      givenInvitationCreationFailsForService(
        arn,
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        identifierVAT)

      val result = controller.submitReviewAuthorisations()(
        authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "false"))

      an[Exception] shouldBe thrownBy {
        await(result)
      }
    }

    "Throw an Exception if NO is selected, invitation creation is successful but link creation fails" in new AgentAuthorisationFullCacheScenario {
      givenInvitationCreationSucceeds(
        arn,
        validNino.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        "HMRC-MTD-IT",
        "NI")
      givenInvitationCreationSucceeds(arn, validNino.value, invitationIdPIR, validNino.value, "ni", servicePIR, "NI")
      givenInvitationCreationSucceeds(
        arn,
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        identifierVAT)
      givenAgentReferenceNotFound(arn, "personal")

      val result = controller.submitReviewAuthorisations()(
        authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "false"))

      an[Exception] shouldBe thrownBy {
        await(result)
      }
    }

    "Throw an Exception if there is nothing in the cache" in {
      val result = controller.submitReviewAuthorisations()(
        authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "false"))

      an[Exception] shouldBe thrownBy {
        await(result)
      }
    }

    "Redisplay the page with errors if no option is chosen" in new AgentAuthorisationFullCacheScenario {

      val result = controller.submitReviewAuthorisations()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Select yes if you want to add another authorisation for this client")
    }
  }

  "GET /agents/delete" should {
    val request = FakeRequest("GET", "/agents/delete")
    "show the delete page for an authenticated agent" in new AgentAuthorisationFullCacheScenario {

      val result = controller.showDelete(clientDetail1.itemId)(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        s"Are you sure you want to remove your authorisation request for ${clientDetail1.clientName}?",
        s"You will not send them an authorisation request to report their income and expenses through software"
      )
    }

    "throw an Exception if the item you want to delete is not in the cache" in new AgentAuthorisationFullCacheScenario {

      val result = controller.showDelete("foo")(authorisedAsValidAgent(request, arn.value))
      an[Exception] shouldBe thrownBy {
        await(result)
      }
    }
  }

  "POST /agents/delete" should {
    val request = FakeRequest("POST", "/agents/delete")

    "Redirect to review-authorisations page with selected invitation removed from cache when YES is selected" in new AgentAuthorisationFullCacheScenario {

      val result = controller.submitDelete(clientDetail1.itemId)(
        authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "true"))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showReviewAuthorisations().url)

      await(testAgentMultiAuthorisationJourneyStateCache.fetch) shouldBe Some(
        AgentMultiAuthorisationJourneyState("personal", Set(clientDetail2, clientDetail3)))
    }

    "Redirect to review-authorisations page with selected invitation not removed from cache when NO is selected" in new AgentAuthorisationFullCacheScenario {

      val result = controller.submitDelete(clientDetail1.itemId)(
        authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "false"))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showReviewAuthorisations().url)

      await(testAgentMultiAuthorisationJourneyStateCache.fetch) shouldBe Some(
        AgentMultiAuthorisationJourneyState("personal", Set(clientDetail1, clientDetail2, clientDetail3)))

    }

    "throw an Exception when the item you are searching with is not in the cache" in new AgentAuthorisationFullCacheScenario {

      val result = controller.submitDelete("foo")(
        authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> ""))

      an[Exception] shouldBe thrownBy {
        await(result)
      }
    }

    "Redirect to client-type when there is nothing in the cache" in {
      val result = controller.submitDelete("foo")(
        authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> ""))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showClientType().url)
    }

    "Redisplay the page with errors when neither radio button is selected" in new AgentAuthorisationFullCacheScenario {

      val result = controller.submitDelete(clientDetail1.itemId)(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Select yes if you want to remove the authorisation request for this client")

    }
  }

  "GET /all-authorisations-removed" should {
    val request = FakeRequest("GET", "/agents/all-authorisations-removed")
    "Display the all authorisations removed error page" in {
      val result = controller.allAuthorisationsRemoved()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Authorisation request removed",
        "You have removed all of your new authorisation requests.",
        "Start a new request")
    }
  }

  "GET /already-authorisation-pending" should {
    val request = FakeRequest("GET", "/agents/already-authorisation-pending")
    "Display the pending authorisation already exists error page" in {
      val result = controller.pendingAuthorisationExists()(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "There is a problem",
        "You have already added the same authorisation request for this client.",
        "Return to your authorisation requests")
    }
  }

  trait AgentAuthorisationFullCacheScenario {

    val clientDetail1 = AuthorisationRequest("Gareth Gates Sr", serviceITSA, validNino.value)
    val clientDetail2 = AuthorisationRequest("Malcolm Pirson", servicePIR, validNino.value)
    val clientDetail3 = AuthorisationRequest("Sara Vaterloo", serviceVAT, validVrn.value)

    testAgentMultiAuthorisationJourneyStateCache.save(
      AgentMultiAuthorisationJourneyState("personal", Set(clientDetail1, clientDetail2, clientDetail3)))

  }

}
