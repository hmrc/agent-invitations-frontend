package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.util.UUID

import org.joda.time.LocalDate
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.forms.ServiceTypeForm
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.personal
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

class AgentInvitationsIRVControllerJourneyISpec extends BaseISpec with AuthBehaviours {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId(UUID.randomUUID().toString)))

  "POST /agents/select-service" should {
    val request = FakeRequest("POST", "/agents/select-personal-service")
    val submitService = controller.submitSelectPersonalService()

    "return 303 for authorised Agent with valid Personal Income Record service, redirect to identify client" in {
      await(sessionStore.save(AgentSession(Some(personal), Some(servicePIR))))
      val serviceForm = ServiceTypeForm.form.fill(servicePIR)
      val result =
        submitService(authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*),    arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/identify-client")
      verifyAuthoriseAttempt()
    }
  }

  "GET /agents/identify-client" should {
    val request = FakeRequest("GET", "/agents/identify-client")
    val showIdentifyClientForm = controller.showIdentifyClient()

    behave like anAuthorisedAgentEndpoint(request, showIdentifyClientForm)

    "return 200 for an Agent with HMRC-AS-AGENT enrolment for IRV service" in {
      await(sessionStore.save(AgentSession(Some(personal), Some(servicePIR))))
      val result = showIdentifyClientForm(authorisedAsValidAgent(request,    arn.value))
      status(result) shouldBe 200

      checkHtmlResultWithBodyMsgs(result, "identify-client.header", "title.suffix.agents")

      checkHtmlResultWithBodyMsgs(
        result,
        "identify-client.itsa.p1",
        "identify-client.nino.hint",
        "identify-client.irv-date-of-birth.label",
        "identify-client.irv-date-of-birth.hint"
      )

      checkHasAgentSignOutLink(result)
    }
  }

  "POST /agents/identify-client" when {
    val request = FakeRequest("POST", "/agents/identify-irv-client")
    val submitIdentifyClient = controller.submitIdentifyClientIrv()

    behave like anAuthorisedAgentEndpoint(request, submitIdentifyClient)

    "service is PERSONAL-INCOME-RECORD" should {

      "redirect to review-authorisation when a valid NINO is submitted" in {
        givenInvitationCreationSucceeds(
          arn,
          Some(personal),
          validNino.value,
          invitationIdPIR,
          validNino.value,
          "ni",
          servicePIR,
          "NI")
        givenAgentReference(arn, "ABCDEFGH", personal)
        givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
        givenCitizenDetailsAreKnownFor(validNino.value, "First", "Last")
        givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, servicePIR)

        await(sessionStore.save(
          AgentSession(Some(personal), Some(servicePIR), Some("ni"), Some(validNino.value), Some(dateOfBirth))))
        val requestWithForm =
          request.withFormUrlEncodedBody(
            "clientIdentifier" -> validNino.value,
            "dob.year"         -> "1980",
            "dob.month"        -> "07",
            "dob.day"          -> "07"
          )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm,    arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showReviewAuthorisations().url)
      }

      "redirect to client-type when a valid NINO is submitted but cache is empty" in {
        givenInvitationCreationSucceeds(
          arn,
          Some(personal),
          validNino.value,
          invitationIdPIR,
          validNino.value,
          "ni",
          servicePIR,
          "NI")
        givenAgentReference(arn, "ABCDEFGH", personal)
        givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))
        givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, servicePIR)

        val requestWithForm =
          request.withFormUrlEncodedBody(
            "clientIdentifier" -> validNino.value,
            "dob.year"   -> "1980",
            "dob.month"  -> "07",
            "dob.day"    -> "07"
          )

          val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm,    arn.value))
          status(result) shouldBe 303
          redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showClientType().url)
      }

      "redirect to already-authorisation-pending when a valid NINO is submitted but authorisation already exists" in {
        await(sessionStore.save(
          AgentSession(Some(personal), Some(servicePIR), Some("ni"), Some(validNino.value), Some(dateOfBirth))))

        givenGetAllPendingInvitationsReturnsSome(arn, validNino.value, servicePIR)
        givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))

        val requestWithForm =
          request.withFormUrlEncodedBody(
            "clientIdentifier" -> validNino.value,
            "dob.year"         -> "1980",
            "dob.month"        -> "07",
            "dob.day"          -> "07"
          )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm,    arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.pendingAuthorisationExists().url)
      }

      "redirect to already-authorisation-pending when a valid NINO is submitted but it already exists in the basket" in {
        await(sessionStore.save(
          AgentSession(Some(personal), Some(servicePIR), Some("ni"), Some(validNino.value), Some(dateOfBirth), requests = Set(AuthorisationRequest( "clientName", PirInvitation(validNino, Some(DOB(dateOfBirth))))))))

        givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, servicePIR)
        givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))

        val requestWithForm =
          request.withFormUrlEncodedBody(
            "clientIdentifier" -> validNino.value,
            "dob.year"         -> "1980",
            "dob.month"        -> "07",
            "dob.day"          -> "07"
          )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm,    arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.pendingAuthorisationExists().url)
      }

      "redirect to already-authorisation-present when a valid NINO is submitted but client already has relationship with agent for this service" in {
        await(sessionStore.save(
          AgentSession(Some(personal), Some(servicePIR), Some("ni"), Some(validNino.value), Some(dateOfBirth), requests = Set(AuthorisationRequest( "clientName", PirInvitation(Nino("AB123456B"), Some(DOB(dateOfBirth))))))))

        givenMatchingCitizenRecord(validNino, LocalDate.parse(dateOfBirth))

        givenGetAllPendingInvitationsReturnsEmpty(arn, validNino.value, servicePIR)
        givenAfiRelationshipIsActiveForAgent(arn, validNino)
        val requestWithForm =
          request.withFormUrlEncodedBody(
            "clientIdentifier" -> validNino.value,
            "dob.year"         -> "1980",
            "dob.month"        -> "07",
            "dob.day"          -> "07"
          )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm,    arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentsErrorController.activeRelationshipExists().url)
      }

      "redisplay page with errors when an empty NINO is submitted" in {
        await(sessionStore.save(AgentSession(Some(personal), Some(servicePIR))))
        val requestWithForm = request.withFormUrlEncodedBody("clientIdentifier" -> "")
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm,    arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.nino.required")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an invalid NINO is submitted" in {
        await(sessionStore.save(AgentSession(Some(personal), Some(servicePIR))))
        val requestWithForm = request.withFormUrlEncodedBody("clientIdentifier" -> "invalid")
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm,    arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "enter-nino.invalid-format")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an no date of birth is submitted" in {
        await(sessionStore.save(AgentSession(Some(personal), Some(servicePIR))))
        val requestWithForm = request.withFormUrlEncodedBody(
          "clientIdentifier" -> validNino.value,
          "dob.year"         -> "",
          "dob.month"        -> "",
          "dob.day"          -> ""
        )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm,    arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "error.irv-date-of-birth.required")
        checkHasAgentSignOutLink(result)
      }

      "redisplay page with errors when an invalid date of birth is submitted" in {
        await(sessionStore.save(AgentSession(Some(personal), Some(servicePIR))))
        val requestWithForm = request.withFormUrlEncodedBody(
          "clientIdentifier" -> validNino.value,
          "dob.year"         -> "9999",
          "dob.month"        -> "99",
          "dob.day"          -> "99"
        )
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm,    arn.value))

        status(result) shouldBe 200
        checkHtmlResultWithBodyMsgs(result, "identify-client.header", "enter-irv-date-of-birth.invalid-format")
        checkHasAgentSignOutLink(result)
      }

      "redirect to /agents/select-service if service is missing" in {
        await(sessionStore.save(AgentSession(Some(personal))))
        val requestWithForm = request.withFormUrlEncodedBody("clientIdentifier" -> validNino.value, "dob.year"         -> "1999",
          "dob.month"        -> "11",
          "dob.day"          -> "11")
        val result = submitIdentifyClient(authorisedAsValidAgent(requestWithForm,    arn.value))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.AgentsInvitationController.showSelectService().url)
      }
    }
  }

  "GET /agents/invitation-sent" should {
    val request = FakeRequest("GET", "/agents/invitation-sent")
    val invitationSent = controller.showInvitationSent()

    "return 200 for authorised Agent successfully created IRV invitation and redirected to Confirm Invitation Page (secureFlag = false) with no continue Url" in {
      await(sessionStore.save(
        AgentSession(Some(personal), Some(servicePIR), Some("ni"), Some(validNino.value), Some(dateOfBirth), clientTypeForInvitationSent = Some(personal))))

      givenAgentReference(arn, uid, personal)

      val result = invitationSent(authorisedAsValidAgent(request,    arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("invitation-sent-link.header"),
          htmlEscapedMessage("title.suffix.agents")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.header"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l2", "someurl"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l1.p.personal"))
      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.l4.personal"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.trackRequests.button"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.continueToASAccount.button"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent.startNewAuthRequest"))
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          s"$wireMockBaseUrlAsString${routes.ClientsMultiInvitationController.warmUp("personal", uid, "99-with-flake")}"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      checkInviteSentExitSurveyAgentSignOutLink(result)

      //check if we have cleared everything in cache except clientTypeForInvitationsSent
      await(sessionStore.hardGet) shouldBe AgentSession(clientTypeForInvitationSent = Some(personal))

      verifyAuthoriseAttempt()
    }
  }
}
