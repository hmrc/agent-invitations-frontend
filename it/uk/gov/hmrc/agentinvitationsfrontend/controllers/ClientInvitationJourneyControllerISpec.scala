package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.util.UUID

import play.api.Application
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.{BaseISpec, CallOps}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class ClientInvitationJourneyControllerISpec extends BaseISpec with StateAndBreadcrumbsMatchers with AuthBehaviours {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  override implicit lazy val app: Application = appBuilder(featureFlags)
    .overrides(new TestClientInvitationJourneyModule)
    .build()

  lazy val journeyState = app.injector.instanceOf[TestClientInvitationJourneyService]
  lazy val controller: ClientInvitationJourneyController = app.injector.instanceOf[ClientInvitationJourneyController]
  lazy val journeyIdKey = app.injector.instanceOf[ClientInvitationJourneyService].journeyKey

  import journeyState.model.State._

  val emptyBasket = Set.empty[AuthorisationRequest]

  def requestWithJourneyIdInCookie(method: String, path: String): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest(method, path).withSession(journeyIdKey -> UUID.randomUUID().toString)

  def requestWithJourneyIdInQuery(method: String, path: String): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest(method, CallOps.addParamsToUrl(path, journeyIdKey -> Some(UUID.randomUUID().toString)))

  "GET /invitations/:clientType/:uid/:agentName" when {
    trait Setup {
      val clientType = "personal"
      val agentName = "My-Agency"
      val endpointUrl: String = routes.ClientInvitationJourneyController.warmUp(clientType, uid, agentName).url

      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenGetAgencyNameClientStub(arn)
      journeyState.setEmpty()
    }

    "journey ID is already present in the session cookie, show the warmup page" should {
      "work when signed in" in new Setup {
        val reqAuthorisedWithJourneyId =
          authorisedAsAnyIndividualClient(requestWithJourneyIdInCookie("GET", endpointUrl))
        val result = controller.warmUp("personal", uid, "My-Agency")(reqAuthorisedWithJourneyId)
        checkWarmUpPageIsShown(result)
      }
      "work when not signed in" in new Setup {
        val reqWithJourneyId = requestWithJourneyIdInCookie("GET", endpointUrl)
        val result = controller.warmUp("personal", uid, "My-Agency")(reqWithJourneyId)
        checkWarmUpPageIsShown(result)
      }

      def checkWarmUpPageIsShown(result: Result) {
        status(result) shouldBe 200

        checkHtmlResultWithBodyText(
          result,
          htmlEscapedMessage("title.suffix.client"),
          htmlEscapedMessage("warm-up.header", "My Agency"),
          htmlEscapedMessage("warm-up.inset", "My Agency"))
        checkIncludesText(result, "<p>So we can confirm who you are")
      }
    }

    "journey ID is not already present in the session cookie, redirect to same page saving the journey ID in the session" should {
      "work when signed in" in new Setup {
        def request = authorisedAsAnyIndividualClient(FakeRequest("GET", endpointUrl))
        val result = controller.warmUp("personal", uid, "My-Agency")(request)
        checkRedirectedWithJourneyId(result, request, journeyIdKey)
      }

      "work when not signed in" in new Setup {
        def request = FakeRequest("GET", endpointUrl)
        val result = controller.warmUp("personal", uid, "My-Agency")(request)
        checkRedirectedWithJourneyId(result, request, journeyIdKey)
      }

      def checkRedirectedWithJourneyId(result: Result, request: Request[_], journeyIdKey: String): Unit = {
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(request.uri)

        val journeyId = result.session(request).get(journeyIdKey)
        journeyId shouldBe a[Some[String]]
        journeyId.get should not be empty
      }
    }
  }

  "POST /warm-up" when {
    "journey ID is not available or session expired" should {
      behave like anActionHandlingSessionExpiry(controller.submitWarmUp)
    }

    "journey ID is available in session cookie (e.g. already logged in)" should {
      val request = () => requestWithJourneyIdInCookie("GET", "/warm-up")
      behave like warmupSubmitAccept(request)
    }

    "journey ID is not in the session cookie but is on the query string (e.g. just been redirected from successful login)" should {
      val request = () => requestWithJourneyIdInQuery("GET", "/warm-up")
      behave like warmupSubmitAccept(request)
    }

    "user is authenticated as valid client" should {
      val request = () => requestWithJourneyIdInQuery("GET", "/warm-up")
      behave like aClientWithLowConfidenceLevelGetEndpoint(request(), controller.submitWarmUp)
    }

    def warmupSubmitAccept(request: () => FakeRequest[AnyContentAsEmpty.type]) = {

      "redirect to consent page if the invitation is found" in {
        givenAllInvitationIdsByStatus(uid, "Pending")
        journeyState.set(WarmUp(personal, uid, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsAnyIndividualClient(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showConsent().url)
      }

      "redirect to not found invitation if the invitation is not found" in {
        givenAllInvitationIdsByStatusReturnsEmpty(uid, "Pending")
        journeyState.set(WarmUp(personal, uid, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsAnyIndividualClient(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showNotFoundInvitation().url)
      }

      "redirect to /trust-not-claimed if client doesn't have the trust enrolment but invitation contains trust" in {
        givenAllInvitationIdsWithTrustByStatus(uid, "Pending")
        journeyState.set(WarmUp(business, uid, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsAnyOrganisationClient(request()))
        status(result) shouldBe 303

        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showTrustNotClaimed().url)
      }
    }

    "redirect to not authorised when an agent tries to respond to a clients invitation" in {
      givenAllInvitationIdsByStatus(uid, "Pending")
      journeyState.set(WarmUp(personal, uid, "My Agency", "my-agency"), Nil)
      val request = () => requestWithJourneyIdInCookie("GET", "/warm-up")

      val result = controller.submitWarmUp(authenticatedAnyClientWithAffinity(request()))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(
        routes.ClientInvitationJourneyController.incorrectlyAuthorisedAsAgent().url)
    }

    "throw an exception when a user with no affinity group tries to respond to a clients invitation" in {
      givenAllInvitationIdsByStatus(uid, "Pending")
      givenUnauthorisedWith("UnsupportedAffinityGroup")
      journeyState.set(WarmUp(personal, uid, "My Agency", "my-agency"), Nil)
      val request = () => requestWithJourneyIdInCookie("GET", "/warm-up")

      intercept[Exception] {
        await(controller.submitWarmUp(request()))
      }.getMessage shouldBe "UnsupportedAffinityGroup"
    }

    "redirect to gg log in with an appended journey ID on the continue url when there is no session" in {
      givenUnauthorisedWith("MissingBearerToken")
      journeyState.set(WarmUp(personal, uid, "My Agency", "my-agency"), Nil)
      val request = () => FakeRequest("GET", "/warm-up").withSession(journeyIdKey -> "foo")

      val result = controller.submitWarmUp(request())
      status(result) shouldBe 303

      val continueUrlEncoded =
        URLEncoder.encode("/warm-up?clientInvitationJourney=foo", StandardCharsets.UTF_8.toString())

      redirectLocation(result) shouldBe Some(
        s"/gg/sign-in?continue=$continueUrlEncoded&origin=agent-invitations-frontend")
    }
  }

  "GET /warm-up/to-decline" when {
    "journey ID is not available or session expired" should {
      behave like anActionHandlingSessionExpiry(controller.submitWarmUpConfirmDecline)
    }

    "journey ID is available in session cookie (e.g. already logged in)" should {
      val request = () => requestWithJourneyIdInCookie("GET", "/warm-up/to-decline")
      behave like warmupSubmitDecline(request)
    }

    "journey ID is on the query string (e.g. just been redirected from successful login)" should {
      val request = () => requestWithJourneyIdInQuery("GET", "/warm-up/to-decline")
      behave like warmupSubmitDecline(request)
    }

    def warmupSubmitDecline(request: () => FakeRequest[AnyContentAsEmpty.type]) = {
      "redirect to confirm decline" in {
        givenAllInvitationIdsByStatus(uid, "Pending")
        journeyState.set(WarmUp(personal, uid, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUpConfirmDecline(authorisedAsAnyIndividualClient(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showConfirmDecline().url)
      }

      "redirect to not found invitation" in {
        givenAllInvitationIdsByStatusReturnsEmpty(uid, "Pending")
        journeyState.set(WarmUp(personal, uid, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUpConfirmDecline(authorisedAsAnyIndividualClient(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showNotFoundInvitation().url)
      }

      "redirect to TrustNotClaimed if client doesn't have the HMRC-TERS-ORG enrolment" in {
        givenAllInvitationIdsWithTrustByStatus(uid, "Pending")
        journeyState.set(WarmUp(business, uid, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUpConfirmDecline(authorisedAsAnyOrganisationClient(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showTrustNotClaimed().url)
      }
    }
  }

  "GET /not-found" should {
    def request = requestWithJourneyIdInCookie("GET", "/warm-up/not-found")

    behave like anActionHandlingSessionExpiry(controller.showNotFoundInvitation)

    "display the not found invitation page" in {
      journeyState.set(NotFoundInvitation, Nil)

      val result = controller.showNotFoundInvitation(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-found-invitation.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-found-invitation.description.1"))
    }
  }
  "GET /consent" should {
    def request = requestWithJourneyIdInCookie("GET", "/warm-up/consent")

    behave like anActionHandlingSessionExpiry(controller.showConsent)

    "display the multi consent page" in {
      journeyState.set(
        MultiConsent(
          personal,
          uid,
          "My Agency",
          Seq(ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = true))),
        Nil)

      val result = controller.showConsent(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.multi.heading"))
    }
  }

  "POST /consent" should {
    def request = requestWithJourneyIdInCookie("POST", "/warm-up/consent")

    behave like anActionHandlingSessionExpiry(controller.submitConsent)

    "redirect to check answers when continuing" in {
      journeyState.set(
        MultiConsent(
          personal,
          uid,
          "My Agency",
          Seq(ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = false))),
        Nil)

      val result =
        controller.submitConsent(authorisedAsAnyIndividualClient(request.withFormUrlEncodedBody("accepted" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showCheckAnswers().url)
    }

    "user is authenticated as valid client" should {
      val request = () => requestWithJourneyIdInQuery("POST", "/warm-up/consent")
      behave like aClientWithLowConfidenceLevelPostEndpoint(request(), controller.submitConsent)
    }
  }

  "GET /consent-change" should {
    def request = requestWithJourneyIdInCookie("GET", "/consent/individual")

    behave like anActionHandlingSessionExpiry(controller.showConsentChange)

    "display the individual consent page" in {
      journeyState.set(
        SingleConsent(
          personal,
          uid,
          "My Agency",
          ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = true),
          Seq(
            ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = false),
            ClientConsent(invitationIdPIR, expiryDate, "afi", consent = false),
            ClientConsent(invitationIdVAT, expiryDate, "vat", consent = false)
          )
        ),
        Nil
      )

      val result = controller.showConsentChange(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.heading"))
    }

    "display the individual consent page when coming back to it from the CheckAnswers page" in {
      val consents = Seq(
        ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = false),
        ClientConsent(invitationIdPIR, expiryDate, "afi", consent = false),
        ClientConsent(invitationIdVAT, expiryDate, "vat", consent = false)
      )
      val currentState = CheckAnswers(
        personal,
        uid,
        "My Agency",
        consents
      )

      journeyState.set(
        currentState,
        breadcrumbs = List(
          currentState,
          SingleConsent(
            personal,
            uid,
            "My Agency",
            ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = true),
            consents
          )
        )
      )

      val result = controller.showConsentChange(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.heading"))
    }
  }

  "GET /warm-up/check-answers" should {
    def request = requestWithJourneyIdInCookie("GET", "/warm-up/check-answers")

    behave like anActionHandlingSessionExpiry(controller.showCheckAnswers)

    "display the check answers page" in {
      journeyState.set(
        CheckAnswers(
          personal,
          "uid",
          "My Agency",
          Seq(ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = false))),
        Nil)

      val result = controller.showCheckAnswers(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("check-answers.heading"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("check-answers.service.itsa"))
    }
  }

  "POST /warm-up/check-answers" should {
    def request = requestWithJourneyIdInCookie("POST", "/warm-up/check-answers")

    behave like anActionHandlingSessionExpiry(controller.submitCheckAnswers)

    "redirect to invitations accepted when all invitations are successfully accepted" in {
      givenInvitationByIdSuccess(invitationIdITSA, "ABCDEF123456789")
      givenInvitationByIdSuccess(invitationIdPIR, "AB123456A")
      givenInvitationByIdSuccess(invitationIdVAT, "101747696")
      givenAcceptInvitationSucceeds("ABCDEF123456789", invitationIdITSA, identifierITSA)
      givenAcceptInvitationSucceeds("AB123456A", invitationIdPIR, identifierPIR)
      givenAcceptInvitationSucceeds("101747696", invitationIdVAT, identifierVAT)
      journeyState.set(
        CheckAnswers(
          personal,
          uid,
          "My Agency",
          Seq(
            ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = true),
            ClientConsent(invitationIdPIR, expiryDate, "afi", consent = true),
            ClientConsent(invitationIdVAT, expiryDate, "vat", consent = true)
          )
        ),
        Nil
      )

      val result = controller.submitCheckAnswers(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showInvitationsAccepted().url)
    }
    "redirect to invitations rejected when all invitations are successfully rejected" in {
      givenInvitationByIdSuccess(invitationIdITSA, "ABCDEF123456789")
      givenInvitationByIdSuccess(invitationIdPIR, "AB123456A")
      givenInvitationByIdSuccess(invitationIdVAT, "101747696")
      givenRejectInvitationSucceeds("ABCDEF123456789", invitationIdITSA, identifierITSA)
      givenRejectInvitationSucceeds("AB123456A", invitationIdPIR, identifierPIR)
      givenRejectInvitationSucceeds("101747696", invitationIdVAT, identifierVAT)
      journeyState.set(
        CheckAnswers(
          personal,
          uid,
          "My Agency",
          Seq(
            ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = false),
            ClientConsent(invitationIdPIR, expiryDate, "afi", consent = false),
            ClientConsent(invitationIdVAT, expiryDate, "vat", consent = false)
          )
        ),
        Nil
      )

      val result = controller.submitCheckAnswers(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showInvitationsDeclined().url)
    }

    "redirect to some responses failed when some of the invitations are not successfully accepted" in {
      givenInvitationByIdSuccess(invitationIdITSA, "ABCDEF123456789")
      givenInvitationByIdSuccess(invitationIdPIR, "AB123456A")
      givenInvitationByIdSuccess(invitationIdVAT, "101747696")
      givenAcceptInvitationReturnsNotFound("ABCDEF123456789", invitationIdITSA, identifierITSA)
      givenAcceptInvitationSucceeds("AB123456A", invitationIdPIR, identifierPIR)
      givenAcceptInvitationSucceeds("101747696", invitationIdVAT, identifierVAT)
      journeyState.set(
        CheckAnswers(
          personal,
          uid,
          "My Agency",
          Seq(
            ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = true),
            ClientConsent(invitationIdPIR, expiryDate, "afi", consent = true),
            ClientConsent(invitationIdVAT, expiryDate, "vat", consent = true)
          )
        ),
        Nil
      )

      val result = controller.submitCheckAnswers(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showSomeResponsesFailed().url)
    }
    "redirect to all responses failed when all of the invitations are not successfully accepted" in {
      givenInvitationByIdSuccess(invitationIdITSA, "ABCDEF123456789")
      givenInvitationByIdSuccess(invitationIdPIR, "AB123456A")
      givenInvitationByIdSuccess(invitationIdVAT, "101747696")
      givenAcceptInvitationReturnsNotFound("ABCDEF123456789", invitationIdITSA, identifierITSA)
      givenAcceptInvitationReturnsNotFound("AB123456A", invitationIdPIR, identifierPIR)
      givenAcceptInvitationReturnsNotFound("101747696", invitationIdVAT, identifierVAT)
      journeyState.set(
        CheckAnswers(
          personal,
          uid,
          "My Agency",
          Seq(
            ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = true),
            ClientConsent(invitationIdPIR, expiryDate, "afi", consent = true),
            ClientConsent(invitationIdVAT, expiryDate, "vat", consent = true)
          )
        ),
        Nil
      )

      val result = controller.submitCheckAnswers(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showAllResponsesFailed().url)
    }
  }

  "GET /warm-up/confirm-decline" should {
    def request = requestWithJourneyIdInCookie("GET", "/warm-up/confirm-decline")

    behave like anActionHandlingSessionExpiry(controller.showConfirmDecline)

    "display the confirm decline page" in {
      journeyState.set(
        ConfirmDecline(
          personal,
          "uid",
          "My Agency",
          Seq(ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = false))),
        Nil)

      val result = controller.showConfirmDecline(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-decline.heading"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-decline.itsa.sub-header", "My Agency"))
    }
  }

  "POST /warm-up/confirm-decline" should {
    def request = requestWithJourneyIdInCookie("POST", "/warm-up/confirm-decline")

    behave like anActionHandlingSessionExpiry(controller.submitConfirmDecline)

    "redirect to invitation declined when yes is selected" in {
      givenInvitationByIdSuccess(invitationIdITSA, "ABCDEF123456789")
      givenRejectInvitationSucceeds("ABCDEF123456789", invitationIdITSA, identifierITSA)
      journeyState.set(
        ConfirmDecline(
          personal,
          "uid",
          "My Agency",
          Seq(ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = false))),
        Nil)

      val result = controller.submitConfirmDecline(
        authorisedAsAnyIndividualClient(request.withFormUrlEncodedBody("accepted" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showInvitationsDeclined().url)
    }

    "redirect to confirm terms when no is selected" in {
      journeyState.set(
        ConfirmDecline(
          personal,
          "uid",
          "My Agency",
          Seq(ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = false))),
        Nil)

      val result = controller.submitConfirmDecline(
        authorisedAsAnyIndividualClient(request.withFormUrlEncodedBody("accepted" -> "false")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showConsent().url)
    }

    "redirect to /all-responses-failed in-case the ACA is down to accept/reject" in {
      givenInvitationByIdSuccess(invitationIdTrust, validUtr.value)
      givenRejectInvitationReturnsWithStatus(validUtr.value, invitationIdTrust, identifierTrust, 404)

      journeyState.set(
        ConfirmDecline(
          personal,
          "uid",
          "My Agency",
          Seq(ClientConsent(invitationIdTrust, expiryDate, "trust", consent = false))),
        Nil)

      val result = controller.submitConfirmDecline(
        authorisedAsAnyIndividualClient(request.withFormUrlEncodedBody("accepted" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showAllResponsesFailed().url)
    }

    "redirect to /some-responses-failed in-case there is an issue in processing some consents only" in {
      givenInvitationByIdSuccess(invitationIdVAT, "101747696")
      givenInvitationByIdSuccess(invitationIdTrust, validUtr.value)
      givenRejectInvitationReturnsWithStatus(validUtr.value, invitationIdTrust, identifierTrust, 404)
      givenRejectInvitationSucceeds("101747696", invitationIdVAT, identifierVAT)

      journeyState.set(
        ConfirmDecline(
          personal,
          "uid",
          "My Agency",
          Seq(
            ClientConsent(invitationIdTrust, expiryDate, "trust", consent = false),
            ClientConsent(invitationIdVAT, expiryDate, "vat", consent = false))
        ),
        Nil
      )

      val result = controller.submitConfirmDecline(
        authorisedAsAnyIndividualClient(request.withFormUrlEncodedBody("accepted" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showSomeResponsesFailed().url)
    }
  }

  "GET /warm-up/accepted" should {
    def request = requestWithJourneyIdInCookie("GET", "/warm-up/accepted")

    behave like anActionHandlingSessionExpiry(controller.showInvitationsAccepted)

    "display the accepted page" in {
      journeyState
        .set(
          InvitationsAccepted("My Agency", Seq(ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = true))),
          Nil)

      val result = controller.showInvitationsAccepted(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete.multi.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete.multi.p1.itsa", "My Agency"))
    }
  }
  "GET /warm-up/rejected" should {
    def request = requestWithJourneyIdInCookie("GET", "/warm-up/rejected")

    behave like anActionHandlingSessionExpiry(controller.showInvitationsDeclined)

    "display the rejected page" in {
      journeyState
        .set(
          InvitationsDeclined("My Agency", Seq(ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = false))),
          Nil)

      val result = controller.showInvitationsDeclined(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-declined.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-declined.multi.itsa.p1", "My Agency"))
    }
  }
  "GET /warm-up/all-failed" should {
    def request = requestWithJourneyIdInCookie("GET", "/warm-up/all-failed")

    behave like anActionHandlingSessionExpiry(controller.showAllResponsesFailed)

    "display the all responses failed page" in {
      journeyState
        .set(AllResponsesFailed, Nil)

      val result = controller.showAllResponsesFailed(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("all-responses-failed.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("all-responses-failed.p1"))
    }
  }
  "GET /some-responses-failed" should {
    def request = requestWithJourneyIdInCookie("GET", "/warm-up/some-failed")

    behave like anActionHandlingSessionExpiry(controller.showSomeResponsesFailed)

    "display the some responses failed page" in {
      journeyState
        .set(
          SomeResponsesFailed(
            "My Agency",
            Seq(ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = true)),
            Seq(ClientConsent(invitationIdPIR, expiryDate, "afi", consent = true))),
          Nil
        )

      val result = controller.showSomeResponsesFailed(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("some-responses-failed.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("some-responses-failed.itsa"))
    }
  }

  "POST /some-responses-failed" should {
    def request = requestWithJourneyIdInCookie("POST", "/some-responses-failed")

    "redirect to complete page with only successfully processed consents" in {
      journeyState
        .set(
          SomeResponsesFailed(
            "My Agency",
            Seq(ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = true)),
            Seq(ClientConsent(invitationIdPIR, expiryDate, "afi", consent = true))),
          Nil
        )

      val result = controller.submitSomeResponsesFailed(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showInvitationsAccepted().url)

      journeyState.get.get._1 shouldBe
        InvitationsAccepted("My Agency", Seq(ClientConsent(invitationIdPIR, expiryDate, "afi", consent = true)))
    }
  }

  "GET /session-timeout" should {
    "display the session timeout/lost page" in {
      journeyState.set(MissingJourneyHistory, Nil)
      val result = controller.showMissingJourneyHistory(requestWithJourneyIdInCookie("GET", "/session-timeout"))
      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(result, "session-lost-client.header")
    }

    "redirect to itself with a new journey ID if the journey ID is missing" in {
      journeyState.set(MissingJourneyHistory, Nil)
      val result = controller.showMissingJourneyHistory(FakeRequest("GET", "/session-timeout"))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showMissingJourneyHistory().url)
    }
  }

  "GET /trust-not-claimed" should {
    def request = requestWithJourneyIdInCookie("GET", "/trust-not-claimed")
    "display the page as expected" in {
      journeyState.set(TrustNotClaimed, Nil)

      val result = controller.showTrustNotClaimed(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("trust-not-claimed.client.p1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("trust-not-claimed.client.p2"))
    }
  }

  "GET /not-authorised-as-client" should {
    "display the not authorised page" in {
      val result = controller.incorrectlyAuthorisedAsAgent(authorisedAsValidAgent(FakeRequest(), arn.value))
      status(result) shouldBe 403

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-authorised.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-authorised.description.p1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-authorised.description.p2"))
    }
  }

  "GET /cannot-confirm-identity" should {
    "display the cannot confirm identity page with technical issue content when the failure reason is technicalIssue" in {
      givenIVFailureReasonResponse(TechnicalIssue)
      val result = controller.showCannotConfirmIdentity(Some("valid-uuid"))(FakeRequest())
      status(result) shouldBe 403

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("technical-issues.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("technical-issues.p1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("technical-issues.p2"))
      checkHtmlResultWithBodyText(result,
        "Call the VAT online services helpline",
        "if you need help with Making Tax Digital for VAT.",
        "Call the HMRC Self Assessment online services helpline",
        "if you need help with Making Tax Digital for Income Tax.")

      checkResultContainsLink(result, "https://www.gov.uk/government/organisations/hm-revenue-customs/contact/vat-online-services-helpdesk", "Call the VAT online services helpline")
      checkResultContainsLink(result, "https://www.gov.uk/government/organisations/hm-revenue-customs/contact/self-assessment-online-services-helpdesk", "Call the HMRC Self Assessment online services helpline")
    }
  }

      Set(FailedMatching, FailedDirectorCheck, FailedIV, InsufficientEvidence).foreach { reason =>
        s"IV returns failed reason $reason " when {
          "display the default page" in {
            givenIVFailureReasonResponse(reason)
            val result = controller.showCannotConfirmIdentity(Some("valid-uuid"))(FakeRequest())
            status(result) shouldBe 403
            checkHtmlResultWithBodyMsgs(result,"cannot-confirm-identity.header",
              "cannot-confirm-identity.p1", "cannot-confirm-identity.p2")
            checkResultContainsLink(result,"/invitations/warm-up", "Try again", Some("button"))
          }
      }
    }

  Set(TimedOut, UserAborted).foreach { reason =>
    s"IV returns failed reason $reason" when {
      "display the signed out page" in {
        givenIVFailureReasonResponse(reason)
        val result = controller.showCannotConfirmIdentity(Some("valid-uuid"))(FakeRequest())
        status(result) shouldBe 403
        checkHtmlResultWithBodyMsgs(result, "signed-out.header",
        "signed-out.p1")
        checkResultContainsLink(result, "/invitations/warm-up", "Sign in again")
      }
    }
  }

  private def anActionHandlingSessionExpiry(action: Action[AnyContent]) =
    "redirect to /session-timeout if there is no journey ID/history available" when {
      "logged in" in {
        checkRedirectsToSessionExpiry(authorisedAsAnyIndividualClient(FakeRequest()))
      }

      "not logged in" in {
        checkRedirectsToSessionExpiry(FakeRequest())
      }

      def checkRedirectsToSessionExpiry(request: FakeRequest[AnyContentAsEmpty.type]) = {
        journeyState.setEmpty()
        val result = await(action(request))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showMissingJourneyHistory().url)
      }
    }
}
