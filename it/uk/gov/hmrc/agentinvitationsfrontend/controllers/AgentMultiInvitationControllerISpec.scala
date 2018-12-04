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
import uk.gov.hmrc.agentinvitationsfrontend.models.{AuthorisationRequest, ClientDetail}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class AgentMultiInvitationControllerISpec extends BaseISpec with AuthBehaviours {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))


  "GET /agents/select-service" should {
    val request = FakeRequest("GET", "/agents/select-service")

    "show the select personal service page with review authorisations link if there is content in the authorisationRequest cache" in {
      testAgentAuthorisationsCache.save(AuthorisationRequest("personal", Set(ClientDetail("Gareth Gates", serviceITSA, mtdItId.value))))
      val result = controller.selectService()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Return to authorisation requests")
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "show the select business service page with review authorisations link if there is content in the authorisationRequest cache" in {
      testAgentAuthorisationsCache.save(AuthorisationRequest("business", Set(ClientDetail("Gareth Gates", serviceVAT, validVrn.value))))
      val result = controller.selectService()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "show the select service page if there is an unsupported client type in the authorisationRequest cache" in {
      testAgentAuthorisationsCache.save(AuthorisationRequest("foo", Set(ClientDetail("Gareth Gates", serviceVAT, validVrn.value))))
      val result = controller.selectService()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.selectClientType().url)
      verifyAuthoriseAttempt()
    }
  }

  "GET /agents/review-authorisations" should {
    val request = FakeRequest("GET", "/agents/review-authorisation")

    "show the review authorisations page if there is a single item in the authorisationRequest cache" in {
      testAgentAuthorisationsCache.save(AuthorisationRequest("personal", Set(ClientDetail("Gareth Gates", serviceITSA, mtdItId.value))))
      val result = controller.showReviewAuthorisations()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Review your authorisation requests",
        "You have added 1 authorisation request.",
        "Report their income and expenses through software",
        "Gareth Gates",
        "Do you need to add another authorisation for this client?")
      verifyAuthoriseAttempt()
    }

    "show the review authorisations page if there are multiple items in the authorisationRequest cache" in {
      testAgentAuthorisationsCache.save(AuthorisationRequest("personal",
        Set(ClientDetail("Gareth Gates Sr", serviceITSA, mtdItId.value),
          ClientDetail("Gareth Gates Jr", servicePIR, validNino.value),
          ClientDetail("Gareth Gates Jr", serviceVAT, validVrn.value))))
      val result = controller.showReviewAuthorisations()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Review your authorisation requests",
        "You have added 3 authorisation requests.",
        "name may be recorded differently in each service.",
        "Report their income and expenses through software",
        "Viewing their PAYE income record",
        "Report their VAT Returns through software",
        "Gareth Gates",
        "Do you need to add another authorisation for this client?")
      verifyAuthoriseAttempt()
    }

    "redirect to select clientType page is there is nothing in the cache" in {
      val result = controller.showReviewAuthorisations()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.selectClientType().url)
    }
  }

  "POST /agents/review-authorisations" should {
    val request = FakeRequest("POST", "/agents/review-authorisation")
    "Redirect to select service if YES is selected on the review-authorisations page" in {
      testAgentAuthorisationsCache.save(AuthorisationRequest("personal",
        Set(ClientDetail("Gareth Gates Sr", serviceITSA, mtdItId.value),
          ClientDetail("Gareth Gates Jr", servicePIR, validNino.value),
          ClientDetail("Gareth Gates Jr", serviceVAT, validVrn.value))))

      val result = controller.submitReviewAuthorisations()(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "true"))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.selectService().url)
    }

    "Rediplay the page with errors if no option is chosen" in {
      testAgentAuthorisationsCache.save(AuthorisationRequest("personal",
        Set(ClientDetail("Gareth Gates Sr", serviceITSA, mtdItId.value),
          ClientDetail("Gareth Gates Jr", servicePIR, validNino.value),
          ClientDetail("Gareth Gates Jr", serviceVAT, validVrn.value))))

      val result = controller.submitReviewAuthorisations()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Select yes if you want to add another authorisation for this client")
    }
  }

  "GET /agents/delete" should {
    val request = FakeRequest("GET", "/agents/delete")
    "show the delete page for an authenticated agent" in {
      val clientDetail1 = ClientDetail("Gareth Gates Sr", serviceITSA, mtdItId.value)
      testAgentAuthorisationsCache.save(AuthorisationRequest("personal",
        Set(clientDetail1,
          ClientDetail("Gareth Gates Jr", servicePIR, validNino.value),
          ClientDetail("Gareth Gates Jr", serviceVAT, validVrn.value))))

      val result = controller.showDelete(clientDetail1.itemId)(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, s"Are you sure you want to remove your authorisation request for ${clientDetail1.clientName}?",
        s"You will not send them an authorisation request to report their income and expenses through software")
    }
  }

  "POST /agents/delete" should {
    val request = FakeRequest("POST", "/agents/delete")
    "Redirect to review-authorisations page with selected invitation removed from cache when YES is selected" in {
      val clientDetail1 = ClientDetail("Gareth Gates Sr", serviceITSA, mtdItId.value)
      val clientDetail2 = ClientDetail("Gareth Gates Jr", servicePIR, validNino.value)
      val clientDetail3 = ClientDetail("Gareth Gates Jr", serviceVAT, validVrn.value)
      testAgentAuthorisationsCache.save(AuthorisationRequest("personal",
        Set(clientDetail1,
          clientDetail2,
          clientDetail3)))

      val result = controller.submitDelete(clientDetail1.itemId)(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "true"))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showReviewAuthorisations().url)

      await(testAgentAuthorisationsCache.fetch) shouldBe Some(AuthorisationRequest("personal",
        Set(clientDetail2,
          clientDetail3)))
    }

    "Redirect to review-authorisations page with selected invitation not removed from cache when NO is selected" in {
      val clientDetail1 = ClientDetail("Gareth Gates Sr", serviceITSA, mtdItId.value)
      val clientDetail2 = ClientDetail("Gareth Gates Jr", servicePIR, validNino.value)
      val clientDetail3 = ClientDetail("Gareth Gates Jr", serviceVAT, validVrn.value)
      testAgentAuthorisationsCache.save(AuthorisationRequest("personal",
        Set(clientDetail1,
          clientDetail2,
          clientDetail3)))

      val result = controller.submitDelete(clientDetail1.itemId)(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> "false"))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showReviewAuthorisations().url)

      await(testAgentAuthorisationsCache.fetch) shouldBe Some(AuthorisationRequest("personal",
        Set(clientDetail1,
          clientDetail2,
          clientDetail3)))

    }
  }


}
