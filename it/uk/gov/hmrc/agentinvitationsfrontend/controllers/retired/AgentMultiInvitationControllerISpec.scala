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

import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{AuthBehaviours, retired}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

class AgentMultiInvitationControllerISpec extends BaseISpec with AuthBehaviours {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  lazy val errorController: AgentsErrorController = app.injector.instanceOf[AgentsErrorController]
  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId(UUID.randomUUID().toString)))

  val clientDetail1 = AuthorisationRequest("Gareth Gates Sr",  ItsaInvitation(validNino, Some(Postcode(validPostcode))))
  val clientDetail2 = AuthorisationRequest("Malcolm Pirson",  PirInvitation(validNino, Some(DOB(dateOfBirth))))
  val clientDetail3 = AuthorisationRequest("Sara Vaterloo", VatInvitation(Some(personal), validVrn, Some(VatRegDate(validRegistrationDate))))

  "GET /agents/select-service" should {
    val request = FakeRequest("GET", "/agents/select-service")

    "show the select personal service page with review authorisations link if there is content in the authorisationRequest cache" in {
      await(sessionStore.save(
        AgentSession(
          Some(personal), requests =  Set(AuthorisationRequest("Gareth Gates", ItsaInvitation(validNino, Some(Postcode(validPostcode))))))))
      val result = controller.showSelectService()(authorisedAsValidAgent(request,    arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Return to authorisation requests")
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "show the select business service page with review authorisations link if there is content in the authorisationRequest cache" in {
      await(sessionStore.save(
        AgentSession(
          Some(business),
          requests = Set(AuthorisationRequest("Gareth Gates", VatInvitation(Some(business), validVrn, Some(VatRegDate(validRegistrationDate))))))))
      val result = controller.showSelectService()(authorisedAsValidAgent(request,    arn.value))
      status(result) shouldBe 200
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "show the business select service page if there is not content in the authorisationCache but is in the fastTrackCache" in {
      await(sessionStore.save(
        AgentSession( Some(business), Some(serviceITSA), Some("ni"), Some(validNino.value), Some(validPostcode), fromFastTrack = fromFastTrack)))
      val result = controller.showSelectService()(authorisedAsValidAgent(request,    arn.value))
      status(result) shouldBe 200
      checkHasAgentSignOutLink(result)
      verifyAuthoriseAttempt()
    }

    "show the select service page if there is an unsupported client type in the authorisationRequest cache" in {
      await(sessionStore.save(
        AgentSession(
          None,
          requests = Set(AuthorisationRequest("Gareth Gates", VatInvitation(Some(business), validVrn, Some(VatRegDate(validRegistrationDate))))))))
      val result = controller.showSelectService()(authorisedAsValidAgent(request,    arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(retired.routes.AgentsInvitationController.showClientType().url)
      verifyAuthoriseAttempt()
    }
  }

  "GET /agents/review-authorisations" should {
    val request = FakeRequest("GET", "/agents/review-authorisation")

    "show the review authorisations page if there is a single item in the authorisationRequest cache" in {
      await(sessionStore.save(
        AgentSession(
          Some(personal),
          requests = Set(AuthorisationRequest("Gareth Gates", ItsaInvitation(validNino, Some(Postcode(validPostcode))))))))
      val result = controller.showReviewAuthorisations()(authorisedAsValidAgent(request,    arn.value))
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

    "show the review authorisations page if there are multiple items in the authorisationRequest cache" in {
      await(sessionStore.save(AgentSession( Some(personal), requests =  Set(clientDetail1, clientDetail2, clientDetail3))))
      val result = controller.showReviewAuthorisations()(authorisedAsValidAgent(request,    arn.value))
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
      checkHtmlResultWithBodyMsgs(result, "review-authorisations.no-client-name")
      checkHtmlResultWithNotBodyText(result, "Malcolm Pirson")
      verifyAuthoriseAttempt()
    }

    "redirect to the all authorisation removed page if there are no authorisations in the cache" in {
      await(sessionStore.save(AgentSession( Some(personal))))

      val result = controller.showReviewAuthorisations()(authorisedAsValidAgent(request,    arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(retired.routes.AgentsInvitationController.allAuthorisationsRemoved().url)
    }

    "redirect to select clientType page is there is nothing in the cache" in {
      val result = controller.showReviewAuthorisations()(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(retired.routes.AgentsInvitationController.showClientType().url)
    }
  }

  "POST /agents/review-authorisations" should {
    val request = FakeRequest("POST", "/agents/review-authorisation")
    "Redirect to select service if YES is selected on the review-authorisations page" in {
      await(sessionStore.save(AgentSession( Some(personal), Some(serviceVAT))))
      val result = controller.submitReviewAuthorisations()(
        authorisedAsValidAgent(request,    arn.value).withFormUrlEncodedBody("accepted" -> "true"))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(retired.routes.AgentsInvitationController.showSelectService().url)
    }

    "Redirect to complete if NO is selected and all invitation creation is successful" in {
      givenInvitationCreationSucceeds(
        arn,
        Some(personal),
        validNino.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        "HMRC-MTD-IT",
        "NI")
      givenInvitationCreationSucceeds(
        arn,
        Some(personal),
        validNino.value,
        invitationIdPIR,
        validNino.value,
        "ni",
        servicePIR,
        "NI")
      givenInvitationCreationSucceeds(
        arn,
        Some(personal),
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        identifierVAT)
      givenAgentReference(arn, uid, personal)
      givenAgentReferenceRecordExistsForArn(arn, "uid")

      await(sessionStore.save(AgentSession( Some(personal), Some(serviceVAT))))

      val result = controller.submitReviewAuthorisations()(
        authorisedAsValidAgent(request,    arn.value).withFormUrlEncodedBody("accepted" -> "false"))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(retired.routes.AgentsInvitationController.showInvitationSent().url)
    }

    "Redirect to all create authorisation failed error page if NO is selected and all invitation creations fail" in {
      givenInvitationCreationFails(arn)
      givenAgentReference(arn, uid, personal)

      await(sessionStore.save(AgentSession( Some(personal), Some(serviceVAT), requests = Set(clientDetail1, clientDetail2, clientDetail3))))

      val result = controller.submitReviewAuthorisations()(
        authorisedAsValidAgent(request,    arn.value).withFormUrlEncodedBody("accepted" -> "false"))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(retired.routes.AgentsErrorController.allCreateAuthorisationFailed.url)
    }

    "Redirect to some create authorisation failed if NO is selected and some invitation creations fail" in {
      givenInvitationCreationSucceeds(
        arn,
        Some(personal),
        validNino.value,
        invitationIdITSA,
        validNino.value,
        "ni",
        "HMRC-MTD-IT",
        "NI")
      givenInvitationCreationSucceeds(
        arn,
        Some(personal),
        validNino.value,
        invitationIdPIR,
        validNino.value,
        "ni",
        servicePIR,
        "NI")
      givenInvitationCreationFailsForService(
        arn,
        Some(personal),
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        identifierVAT)
      givenAgentReference(arn, uid, personal)

      await(sessionStore.save(AgentSession( Some(personal), Some(serviceVAT), requests = Set(clientDetail1, clientDetail2, clientDetail3))))

      val result = controller.submitReviewAuthorisations()(
        authorisedAsValidAgent(request,    arn.value).withFormUrlEncodedBody("accepted" -> "false"))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(retired.routes.AgentsErrorController.someCreateAuthorisationFailed.url)
    }

    "Redisplay the page with errors if no option is chosen" in {


      await(sessionStore.save(AgentSession( Some(personal), Some(serviceVAT))))

      val result = controller.submitReviewAuthorisations()(authorisedAsValidAgent(request,    arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Select yes if you want to add another authorisation for this client")
    }
  }

  "GET /agents/delete" should {
    val request = FakeRequest("GET", "/agents/delete")
    "show the delete page for an authenticated agent" in {
      await(sessionStore.save(AgentSession( Some(personal), requests =  Set(clientDetail1, clientDetail2, clientDetail3))))

      val result = controller.showDelete(clientDetail1.itemId)(authorisedAsValidAgent(request,    arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        s"Are you sure you want to remove your authorisation request for ${clientDetail1.clientName}?",
        s"You will not send them an authorisation request to report their income and expenses through software"
      )
    }

    "throw an Exception if the item you want to delete is not in the cache" in {


      await(sessionStore.save(AgentSession( Some(personal), requests =  Set(clientDetail1, clientDetail2, clientDetail3))))
      val result = controller.showDelete("foo")(authorisedAsValidAgent(request,    arn.value))
      an[Exception] shouldBe thrownBy {
        await(result)
      }
    }
  }

  "POST /agents/delete" should {
    val request = FakeRequest("POST", "/agents/delete")

    "Redirect to review-authorisations page with selected invitation removed from cache when YES is selected" in {
      await(sessionStore.save(AgentSession( Some(personal), requests =  Set(clientDetail1, clientDetail2, clientDetail3))))

      val result = controller.submitDelete(clientDetail1.itemId)(
        authorisedAsValidAgent(request,    arn.value).withFormUrlEncodedBody("accepted" -> "true"))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(retired.routes.AgentsInvitationController.showReviewAuthorisations().url)


      await(sessionStore.fetch) shouldBe Some(
        AgentSession( Some(personal),requests=  Set(clientDetail2, clientDetail3)))
    }

    "Redirect to review-authorisations page with selected invitation not removed from cache when NO is selected" in {
      await(sessionStore.save(AgentSession( Some(personal),requests= Set(clientDetail1, clientDetail2, clientDetail3))))

      val result = controller.submitDelete(clientDetail1.itemId)(
        authorisedAsValidAgent(request,    arn.value).withFormUrlEncodedBody("accepted" -> "false"))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(retired.routes.AgentsInvitationController.showReviewAuthorisations().url)

      await(sessionStore.fetch) shouldBe Some(
        AgentSession( Some(personal),requests= Set(clientDetail1, clientDetail2, clientDetail3)))

    }

    "throw an Exception when the item you are searching with is not in the cache" in {
      await(sessionStore.save(AgentSession( Some(personal), requests =  Set(clientDetail1, clientDetail2, clientDetail3))))

      val result = controller.submitDelete("foo")(
        authorisedAsValidAgent(request,    arn.value).withFormUrlEncodedBody("accepted" -> ""))

      an[Exception] shouldBe thrownBy {
        await(result)
      }
    }

    "Redirect to client-type when there is nothing in the cache" in {
      val result = controller.submitDelete("foo")(
        authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody("accepted" -> ""))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(retired.routes.AgentsInvitationController.showClientType().url)
    }

    "Redisplay the page with errors when neither radio button is selected" in {
      await(sessionStore.save(AgentSession( Some(personal), requests =  Set(clientDetail1, clientDetail2, clientDetail3))))

      val result = controller.submitDelete(clientDetail1.itemId)(authorisedAsValidAgent(request,    arn.value))
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
    "Display the pending authorisation already exists error page version when there are other authorisation requests in the basket" in {
      val authRequest1 =
        AuthorisationRequest(
          "Mr Client ITSA",
          ItsaInvitation(validNino, Some(Postcode(validPostcode))),
          AuthorisationRequest.NEW,
          "itemId")

      await(sessionStore.save(AgentSession( Some(personal), Some(serviceITSA), requests = Set(authRequest1))))

      val result = controller.pendingAuthorisationExists()(authorisedAsValidAgent(request,    arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "There is a problem",
        "You have already added the same authorisation request for this client.",
        "Return to your authorisation requests")
    }

    "Display the pending authorisation already exists error page version when there are no authorisation requests left in the basket" in {
      await(sessionStore.save(AgentSession( Some(personal), Some(serviceVAT))))
      val result = controller.pendingAuthorisationExists()(authorisedAsValidAgent(request,    arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "There is a problem",
        "You already created an authorisation request for this client. They have not yet responded to this request.",
        "Track your authorisation requests",
        "Start a new request"
      )
    }

    "Display the pending authorisation already exists error page version when coming from fast track" in {
      await(sessionStore.save(AgentSession( Some(personal), Some(serviceVAT), Some("vrn"), Some(validVrn.value), Some(""), fromFastTrack = true)))
      val result = controller.pendingAuthorisationExists()(authorisedAsValidAgent(request,    arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "There is a problem",
        "You already created an authorisation request for this client. They have not yet responded to this request.",
        "Track your authorisation requests"
      )
      checkHtmlResultWithNotBodyText(result, "Start a new request", "Return to your authorisation requests")
    }
  }
}
