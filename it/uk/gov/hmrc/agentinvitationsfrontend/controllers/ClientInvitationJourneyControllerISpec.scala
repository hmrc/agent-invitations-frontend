package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.util.UUID

import play.api.Application
import play.api.mvc.{Request, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.personal
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class ClientInvitationJourneyControllerISpec extends BaseISpec with StateAndBreadcrumbsMatchers {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  override implicit lazy val app: Application = appBuilder(featureFlags)
    .overrides(new TestClientInvitationJourneyModule)
    .build()

  lazy val journeyState = app.injector.instanceOf[TestClientInvitationJourneyService]
  lazy val controller: ClientInvitationJourneyController = app.injector.instanceOf[ClientInvitationJourneyController]
  lazy val journeyIdKey = app.injector.instanceOf[ClientInvitationJourneyService].journeyKey

  import journeyState.model.State._

  val emptyBasket = Set.empty[AuthorisationRequest]

  def requestWithJourneyId(method: String, path: String) =
    FakeRequest(method, path).withSession(journeyIdKey -> UUID.randomUUID().toString)

  "GET /invitations/:clientType/:uid/:agentName" when {
    trait Setup {
      val clientType = "personal"
      val agentName = "My-Agency"
      val endpointUrl: String = routes.ClientInvitationJourneyController.warmUp(clientType, uid, agentName).url

      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenGetAgencyNameClientStub(arn)
      journeyState.set(Root, Nil)
    }

    "journey ID is already present in the session cookie, show the warmup page" should {
      "work when signed in" in new Setup {
        def request = FakeRequest("GET", endpointUrl)
        val reqAuthorisedWithJourneyId = authorisedAsAnyIndividualClient(requestWithJourneyId("GET", endpointUrl))
        val result = controller.warmUp("personal", uid, "My-Agency")(reqAuthorisedWithJourneyId)
        checkWarmUpPageIsShown(result)
      }
      "work when not signed in" in new Setup {
        val reqWithJourneyId = requestWithJourneyId("GET", endpointUrl)
        val result = controller.warmUp("personal", uid, "My-Agency")(reqWithJourneyId)
        checkWarmUpPageIsShown(result)
      }

      def checkWarmUpPageIsShown(result: Result) {
        status(result) shouldBe 200

        checkHtmlResultWithBodyText(result, htmlEscapedMessage("warm-up.header", "My Agency"))
        checkIncludesText(result, "<p>So we can confirm who you are")
        checkHtmlResultWithBodyText(result, htmlEscapedMessage("warm-up.inset", "My Agency"))
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

  "GET /warm-up" should {
    def request = requestWithJourneyId("GET", "/warm-up")

    "redirect to consent page if the invitation is found" in {
      givenAllInvitationIdsByStatus(uid, "Pending")
      journeyState.set(WarmUp(personal, uid, "My Agency", "my-agency"), Nil)

      val result = controller.submitWarmUp(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showConsent().url)
    }
    "redirect to not found invitation if the invitation is not found" in {
      givenAllInvitationIdsByStatusReturnsEmpty(uid, "Pending")
      journeyState.set(WarmUp(personal, uid, "My Agency", "my-agency"), Nil)

      val result = controller.submitWarmUp(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showNotFoundInvitation().url)
    }
  }

  "GET /warm-up/to-decline" should {
    def request = requestWithJourneyId("GET", "/warm-up/to-decline")

    "redirect to confirm decline" in {
      givenAllInvitationIdsByStatus(uid, "Pending")
      journeyState.set(WarmUp(personal, uid, "My Agency", "my-agency"), Nil)

      val result = controller.submitWarmUpConfirmDecline(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showConfirmDecline().url)
    }
    "redirect to not found invitation" in {
      givenAllInvitationIdsByStatusReturnsEmpty(uid, "Pending")
      journeyState.set(WarmUp(personal, uid, "My Agency", "my-agency"), Nil)

      val result = controller.submitWarmUpConfirmDecline(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showNotFoundInvitation().url)
    }
  }

  "GET /warm-up/not-found" should {
    def request = requestWithJourneyId("GET", "/warm-up/not-found")

    "display the not found invitation page" in {
      journeyState.set(NotFoundInvitation, Nil)

      val result = controller.showNotFoundInvitation(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-found-invitation.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-found-invitation.description.1"))
    }
  }
  "GET /warm-up/consent" should {
    def request = requestWithJourneyId("GET", "/warm-up/consent")

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

  "GET /warm-up/incorrect-client-type" should {
    def request = requestWithJourneyId("GET", "/warm-up/incorrect-client-type")

    "display the incorrect client type page" in {
      journeyState.set(IncorrectClientType(personal), Nil)

      val result = controller.showIncorrectClientType(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("wrong-client-type.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("wrong-client-type.p2", "personal"))
    }
  }

  "POST /warm-up/consent" should {
    def request = requestWithJourneyId("POST", "/warm-up/consent")

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
  }

  "GET /consent/individual" should {
    def request = requestWithJourneyId("GET", "/consent/individual")

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

      val result = controller.showConsentIndividual(authorisedAsAnyIndividualClient(request))
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

      val result = controller.showConsentIndividual(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.heading"))
    }
  }

  "GET /warm-up/check-answers" should {
    def request = requestWithJourneyId("GET", "/warm-up/check-answers")

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
    def request = requestWithJourneyId("POST", "/warm-up/check-answers")

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
    def request = requestWithJourneyId("GET", "/warm-up/confirm-decline")

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
    def request = requestWithJourneyId("POST", "/warm-up/confirm-decline")

    "redirect to invitation declined when yes is selected" in {
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

    "redirect to confirm terms when yes is selected" in {
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
  }

  "GET /warm-up/accepted" should {
    def request = requestWithJourneyId("GET", "/warm-up/accepted")

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
    def request = requestWithJourneyId("GET", "/warm-up/rejected")

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
    def request = requestWithJourneyId("GET", "/warm-up/all-failed")

    "display the all responses failed page" in {
      journeyState
        .set(AllResponsesFailed, Nil)

      val result = controller.showAllResponsesFailed(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("all-responses-failed.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("all-responses-failed.p"))
    }
  }
  "GET /warm-up/some-failed" should {
    def request = requestWithJourneyId("GET", "/warm-up/some-failed")

    "display the some responses failed page" in {
      journeyState
        .set(
          SomeResponsesFailed("My Agency", Seq(ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = true))),
          Nil)

      val result = controller.showSomeResponsesFailed(authorisedAsAnyIndividualClient(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("some-responses-failed.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("some-responses-failed.itsa"))
    }
  }
}
