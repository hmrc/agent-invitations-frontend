package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.util.UUID

import org.joda.time.LocalDate
import org.scalatest.Assertion
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
  override implicit lazy val app: Application = appBuilder
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
          authorisedAsIndividualClientWithSomeSupportedEnrolments(requestWithJourneyIdInCookie("GET", endpointUrl))
        val result = controller.warmUp("personal", uid, "My-Agency")(reqAuthorisedWithJourneyId)
        checkWarmUpPageIsShown(result)
      }

      "work when not signed in" in new Setup {
        val reqWithJourneyId = requestWithJourneyIdInCookie("GET", endpointUrl)
        val result = controller.warmUp("personal", uid, "My-Agency")(reqWithJourneyId)
        checkWarmUpPageIsShown(result)
      }

      "remove spaces in the url" in new Setup {
        val reqAuthorisedWithJourneyId =
          authorisedAsIndividualClientWithSomeSupportedEnrolments(requestWithJourneyIdInCookie("GET", endpointUrl))
        val result = controller.warmUp("personal", uid, "My-Agency ")(reqAuthorisedWithJourneyId)
        checkWarmUpPageIsShown(result)
      }

      "show not-found content on the same page if the url is corrupted" in new Setup {
        val reqWithJourneyId = requestWithJourneyIdInCookie("GET", endpointUrl)
        val result = controller.warmUp("personal", uid, "wrong-agency-name")(reqWithJourneyId)
        status(result) shouldBe 200

        checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-found-invitation.header"))
        checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-found-invitation.description.1"))
        checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-found-invitation.description.2"))
        checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-found-invitation.description.3"))
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
        def request = authorisedAsIndividualClientWithSomeSupportedEnrolments(FakeRequest("GET", endpointUrl))
        val result = controller.warmUp("personal", uid, "My-Agency")(request)
        checkRedirectedWithJourneyId(result, request, journeyIdKey)
      }

      "work when not signed in" in new Setup {
        def request = FakeRequest("GET", endpointUrl)
        val result = controller.warmUp("personal", uid, "My-Agency")(request)
        checkRedirectedWithJourneyId(result, request, journeyIdKey)
      }

      def checkRedirectedWithJourneyId(result: Result, request: Request[_], journeyIdKey: String): Assertion = {
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(request.uri)

        val journeyId = result.session(request).get(journeyIdKey)
        journeyId shouldBe a[Some[_]]
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

    "user is authenticated as individual with NINO, but low confidence level" should {
      val request = () => requestWithJourneyIdInQuery("GET", "/warm-up")
      behave like anIndividualWithLowConfidenceLevelAndNinoGetEndpoint(request(), controller.submitWarmUp)
    }

    "user is authenticated as individual without NINO, and low confidence level" should {
      val request = () => requestWithJourneyIdInQuery("GET", "/warm-up")
      behave like anIndividualWithLowConfidenceLevelWithoutNinoGetEndpoint(request(), controller.submitWarmUp)
    }

    "user is authenticated as CGT individual and low confidence level" should {
      "not go through IV and redirect to consent page if the invitation is found" in {
        val request = () => requestWithJourneyIdInQuery("POST", "/warm-up")
        authorisedAsAnyCGTIndividualClientWithLowCL(request())

        givenGetSuspensionDetailsClientStub(arn, SuspensionDetails(suspensionStatus = false, None))
        givenAllInvitationIdsByStatus(uid, "Pending")
        journeyState.set(WarmUp(personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showConsent().url)
      }
    }

    def warmupSubmitAccept(request: () => FakeRequest[AnyContentAsEmpty.type]): Unit = {

      "redirect to consent page if the invitation is found" in {
        givenGetSuspensionDetailsClientStub(arn, SuspensionDetails(suspensionStatus = false, None))
        givenAllInvitationIdsByStatus(uid, "Pending")
        journeyState.set(WarmUp(personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showConsent().url)
      }

      "redirect to /respond/error/no-outstanding-requests if there are no invitations found " +
        "and the client has SOME supported MTD enrolments" in {
        givenAllInvitationIdsByStatusReturnsEmpty(uid)
        journeyState.set(WarmUp(personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showErrorNoOutstandingRequests().url)
      }

      "redirect to /respond/error/no-outstanding-requests if there are no invitations found " +
        "and the client has ALL supported MTD enrolments" in {
        givenAllInvitationIdsByStatusReturnsEmpty(uid)
        journeyState.set(WarmUp(personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithAllSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showErrorNoOutstandingRequests().url)
      }

      "redirect to /not-found if the client has no supported MTD enrolments" in {
        journeyState.set(WarmUp(personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithNoSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showNotFoundInvitation().url)
      }

      "redirect to /respond/error/no-outstanding-requests if the invitation has status of Accepted or Rejected " +
        "and the client has only SOME supported MTD enrolments" in {
        givenAllInvitationIdsByStatus(uid, "Accepted")
        journeyState.set(WarmUp(personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showErrorNoOutstandingRequests().url)
      }

      "redirect to /respond/error/already-responded if the most recent authorisation request has status of Accepted or Rejected " +
        "and the client has ALL supported MTD enrolments" in {
        givenAllInvitationIdsWithMixedStatus(uid, "Rejected")
        journeyState.set(WarmUp(personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithAllSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showErrorAlreadyRespondedToRequest().url)
      }

      "redirect to /respond/error/agent-cancelled-request if the most recent authorisation request has status of Cancelled " +
        "and the client has ALL supported MTD enrolments" in {
        givenAllInvitationIdsWithMixedStatus(uid, "Cancelled")
        journeyState.set(WarmUp(personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithAllSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showErrorAgentCancelledRequest().url)
      }

      "redirect to /respond/error/no-outstanding-requests if the most recent authorisation request has status of Cancelled " +
        "and the client has only SOME supported MTD enrolments" in {
        givenAllInvitationIdsWithMixedStatus(uid, "Cancelled")
        journeyState.set(WarmUp(personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showErrorNoOutstandingRequests().url)
      }

      "redirect to /respond/error/no-outstanding-requests if the most recent authorisation request has status of Expired " +
        "and the client has only SOME supported MTD enrolments" in {
        givenAllInvitationIdsWithMixedStatus(uid, "Expired")
        journeyState.set(WarmUp(personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showErrorNoOutstandingRequests().url)
      }

      "redirect to /respond/error/no-outstanding-requests if the invitation has mixed statuses, none of which are Pending" in {
        givenAllInvitationIdsWithMixedStatus(uid)
        journeyState.set(WarmUp(personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showErrorNoOutstandingRequests().url)
      }

      "redirect to suspended agent if the agent is suspended for all consent services" in {
        givenGetSuspensionDetailsClientStub(arn, SuspensionDetails(suspensionStatus = true, Some(Set("ITSA", "VATC"))))
        givenAllInvitationIdsByStatus(uid, "Pending")
        journeyState.set(WarmUp(personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showSuspendedAgent().url)
      }

      "redirect to /trust-not-claimed if client doesn't have the trust enrolment but invitation contains trust" in {
        givenAllInvitationIdsWithTrustByStatus(uid, "Pending")
        journeyState.set(WarmUp(business, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsAnyOrganisationClient(request()))
        status(result) shouldBe 303

        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showTrustNotClaimed().url)
      }
    }

    "redirect to /respond/error/cannot-view-request when an agent tries to respond to a clients invitation" in {
      givenAllInvitationIdsByStatus(uid, "Pending")
      journeyState.set(WarmUp(personal, uid, arn, "My Agency", "my-agency"), Nil)
      val request = () => FakeRequest("GET", "/warm-up").withSession(journeyIdKey -> "foo")

      val result = controller.submitWarmUp(authenticatedAnyClientWithAffinity(request()))
      status(result) shouldBe 303

      redirectLocation(result) shouldBe Some(
      "/invitations/respond/error/cannot-view-request"
      )
    }

    "throw an exception when a user with no affinity group tries to respond to a clients invitation" in {
      givenAllInvitationIdsByStatus(uid, "Pending")
      givenUnauthorisedWith("UnsupportedAffinityGroup")
      journeyState.set(WarmUp(personal, uid, arn, "My Agency", "my-agency"), Nil)
      val request = () => requestWithJourneyIdInCookie("GET", "/warm-up")

      intercept[Exception] {
        await(controller.submitWarmUp(request()))
      }.getMessage shouldBe "UnsupportedAffinityGroup"
    }

    "redirect to gg log in with an appended journey ID on the continue url when there is no session" in {
      givenUnauthorisedWith("MissingBearerToken")
      journeyState.set(WarmUp(personal, uid, arn, "My Agency", "my-agency"), Nil)
      val request = () => FakeRequest("GET", "/warm-up").withSession(journeyIdKey -> "foo")

      val result = controller.submitWarmUp(request())
      status(result) shouldBe 303

      val continueUrlEncoded =
        URLEncoder.encode("/warm-up?clientInvitationJourney=foo", StandardCharsets.UTF_8.toString())

      redirectLocation(result) shouldBe Some(
        s"/gg/sign-in?continue=$continueUrlEncoded&origin=agent-invitations-frontend")
    }
  }

  "GET /cannot-appoint" should {
    "display the agent suspended page" in {
      def request = requestWithJourneyIdInCookie("GET", "/cannot-appoint")
      journeyState.set(SuspendedAgent(personal, "uid", "name", Set("ITSA", "VATC"), Seq()), Nil)

      val result = controller.showSuspendedAgent(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyMsgs(
        result,
        "suspended-agent.all.header",
        "suspended-agent.p1.multi",
        "suspended-agent.p1.ITSA",
        "suspended-agent.p1.VATC")
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
        givenGetSuspensionDetailsClientStub(arn, SuspensionDetails(suspensionStatus = false, None))
        givenAllInvitationIdsByStatus(uid, "Pending")
        journeyState.set(WarmUp(personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUpConfirmDecline(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showConfirmDecline().url)
      }

      "redirect to /respond/error/no-outstanding-requests" in {
        givenAllInvitationIdsByStatusReturnsEmpty(uid)
        journeyState.set(WarmUp(personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUpConfirmDecline(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showErrorNoOutstandingRequests().url)
      }

      "redirect to TrustNotClaimed if client doesn't have the HMRC-TERS-ORG enrolment" in {
        givenAllInvitationIdsWithTrustByStatus(uid, "Pending")
        journeyState.set(WarmUp(business, uid, arn, "My Agency", "my-agency"), Nil)

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

      val result = controller.showNotFoundInvitation(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-found-invitation.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-found-invitation.description.1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-found-invitation.description.2"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-found-invitation.description.3"))

    }
  }

  "GET /respond/error/no-outstanding-requests" should {
    def request = requestWithJourneyIdInCookie("GET", "/respond/error/no-outstanding-requests")

    behave like anActionHandlingSessionExpiry(controller.showErrorNoOutstandingRequests)

    "display the no outstanding requests page" in {
      journeyState.set(NoOutstandingRequests, Nil)

      val result = controller.showErrorNoOutstandingRequests(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("no-outstanding-requests.heading"))
      checkIncludesText(result, "If you think this is wrong, contact the agent who sent you the request or <a target=\"_blank\" href=\"someAgentClientManagementFrontendExternalUrl#history\">view your request history</a>")

    }
  }

  "GET /action-needed" should {
    def request = requestWithJourneyIdInCookie("GET", "warm-up/action-needed")

    behave like anActionHandlingSessionExpiry(controller.showActionNeeded)

    "display the action required page" in {
      journeyState.set(ActionNeeded(personal), Nil)

      val result = controller.showActionNeeded(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("action-needed.header"))
    }
  }

  "GET /consent" should {
    def request = requestWithJourneyIdInCookie("GET", "/warm-up/consent")

    behave like anActionHandlingSessionExpiry(controller.showConsent)

    "display the multi consent page for itsa" in {
      journeyState.set(
        MultiConsent(
          personal,
          uid,
          "My Agency",
          Seq(ClientConsent(invitationIdITSA, LocalDate.now().plusDays(1), "itsa", consent = true))),
        Nil)

      val result = controller.showConsent(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.multi.heading"))
      checkHtmlResultWithBodyText(result, hasMessage("confirm-terms.multi.p1", "My Agency"))
      checkHtmlResultWithBodyText(result, hasMessage("confirm-terms.legend.single", "My Agency"))

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.itsa.heading"))
      checkHtmlResultWithBodyText(result, hasMessage("confirm-terms-multi.itsa.p1", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.itsa.list.item1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.itsa.list.item2"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.itsa.list.item3"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.itsa.list.item4"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.itsa.list.item5"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.itsa.list.item6"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.itsa.list.item7"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.itsa.list.item8"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.itsa.list.item9"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.itsa.list.item10"))
      checkIncludesText(result, "I consent to HMRC allowing My Agency to manage my Income Tax.")

    }

    "display the multi consent page for cgt personal" in {
      journeyState.set(
        MultiConsent(
          personal,
          uid,
          "My Agency",
          Seq(ClientConsent(invitationIdCgt, LocalDate.now().plusDays(1), "cgt", consent = true))),
        Nil)

      val result = controller.showConsent(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.multi.heading"))
      checkHtmlResultWithBodyText(result, hasMessage("confirm-terms.multi.p1", "My Agency"))
      checkHtmlResultWithBodyText(result, hasMessage("confirm-terms.legend.single", "My Agency"))

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.personal.heading"))
      checkHtmlResultWithBodyText(result, hasMessage("confirm-terms-multi.cgt.personal.p1", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.personal.p1.l1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.personal.p1.l2"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.personal.p1.l3"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.personal.p1.l4"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.personal.p1.l5"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.personal.p1.l6"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.personal.p1.l7"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.personal.p1.l8"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.personal.p1.l9"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.personal.p1.l10"))
      checkIncludesText(result, "I consent to HMRC allowing My Agency to manage my Capital Gains Tax on UK property account details.")
    }

    "display the correct legend message when there are multiple invitations" in {
      journeyState.set(
        MultiConsent(
          personal,
          uid,
          "My Agency",
          Seq(ClientConsent(invitationIdCgt, LocalDate.now().plusDays(1), "cgt", consent = true),
            ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = true))),
        Nil)

      val result = controller.showConsent(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.legend.multi", "My Agency"))
    }


    "display the multi consent page for cgt business" in {
      journeyState.set(
        MultiConsent(
          business,
          uid,
          "My Agency",
          Seq(ClientConsent(invitationIdCgt, LocalDate.now().plusDays(1), "cgt", consent = true))),
        Nil)

      val result = controller.showConsent(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.multi.heading"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.business.heading"))
      checkHtmlResultWithBodyText(result, hasMessage("confirm-terms-multi.cgt.business.p1", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.business.p1.l1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.business.p1.l2"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.business.p1.l3"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.business.p1.l4"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.business.p1.l5"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.business.p1.l6"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.business.p1.l7"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.business.p1.l8"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.business.p1.l9"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.cgt.business.p1.l10"))
      checkIncludesText(result, "I consent to HMRC allowing My Agency to manage a trust’s Capital Gains Tax on UK property account.")

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
        controller.submitConsent(authorisedAsIndividualClientWithSomeSupportedEnrolments(request.withFormUrlEncodedBody("accepted" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showCheckAnswers().url)
    }

    "user is authenticated as valid client" should {
      val request = () => requestWithJourneyIdInQuery("POST", "/warm-up/consent")
      behave like aClientWithLowConfidenceLevelPostEndpoint(request(), controller.submitConsent)
    }
  }

  "GET /change-consent" should {
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
            ClientConsent(invitationIdVAT, expiryDate, "vat", consent = false),
            ClientConsent(invitationIdCgt, expiryDate, "cgt", consent = false)
          )
        ),
        Nil
      )

      val result = controller.showConsentChange(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.heading"))
    }

    "display the individual consent page when coming back to it from the CheckAnswers page" in {
      val consents = Seq(
        ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = false),
        ClientConsent(invitationIdPIR, expiryDate, "afi", consent = false),
        ClientConsent(invitationIdVAT, expiryDate, "vat", consent = false),
        ClientConsent(invitationIdCgt, expiryDate, "cgt", consent = false)
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

      val result = controller.showConsentChange(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.heading"))
    }
  }

  "GET check-answers" should {
    def request = requestWithJourneyIdInCookie("GET", "/warm-up/check-answers")

    behave like anActionHandlingSessionExpiry(controller.showCheckAnswers)

    "display the check answers page" in {
      journeyState.set(
        CheckAnswers(
          personal,
          "uid",
          "My Agency",
          Seq(
            ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = false),
            ClientConsent(invitationIdCgt, expiryDate, "cgt", consent = false)
          )
        ),
        Nil
      )

      val result = controller.showCheckAnswers(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("check-answers.heading"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("check-answers.service.itsa"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("check-answers.service.cgt"))
    }
  }

  "POST /check-answers" should {
    def request = requestWithJourneyIdInCookie("POST", "/warm-up/check-answers")

    behave like anActionHandlingSessionExpiry(controller.submitCheckAnswers)

    "redirect to invitations accepted when all invitations are successfully accepted" in {
      givenInvitationByIdSuccess(invitationIdITSA, "ABCDEF123456789")
      givenInvitationByIdSuccess(invitationIdPIR, "AB123456A")
      givenInvitationByIdSuccess(invitationIdVAT, "101747696")
      givenInvitationByIdSuccess(invitationIdCgt, cgtRef.value, "HMRC-CGT-PD", "CGTPDRef")
      givenAcceptInvitationSucceeds("ABCDEF123456789", invitationIdITSA, identifierITSA)
      givenAcceptInvitationSucceeds("AB123456A", invitationIdPIR, identifierPIR)
      givenAcceptInvitationSucceeds("101747696", invitationIdVAT, identifierVAT)
      givenAcceptInvitationSucceeds(cgtRef.value, invitationIdCgt, "CGTPDRef")
      journeyState.set(
        CheckAnswers(
          personal,
          uid,
          "My Agency",
          Seq(
            ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = true),
            ClientConsent(invitationIdPIR, expiryDate, "afi", consent = true),
            ClientConsent(invitationIdVAT, expiryDate, "vat", consent = true),
            ClientConsent(invitationIdCgt, expiryDate, "cgt", consent = true)
          )
        ),
        Nil
      )

      val result = controller.submitCheckAnswers(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showInvitationsAccepted().url)
    }
    "redirect to invitations rejected when all invitations are successfully rejected" in {
      givenInvitationByIdSuccess(invitationIdITSA, "ABCDEF123456789")
      givenInvitationByIdSuccess(invitationIdPIR, "AB123456A")
      givenInvitationByIdSuccess(invitationIdVAT, "101747696")
      givenInvitationByIdSuccess(invitationIdCgt, cgtRef.value, "HMRC-CGT-PD", "CGTPDRef")
      givenRejectInvitationSucceeds("ABCDEF123456789", invitationIdITSA, identifierITSA)
      givenRejectInvitationSucceeds("AB123456A", invitationIdPIR, identifierPIR)
      givenRejectInvitationSucceeds("101747696", invitationIdVAT, identifierVAT)
      givenRejectInvitationSucceeds(cgtRef.value, invitationIdCgt, "CGTPDRef")
      journeyState.set(
        CheckAnswers(
          personal,
          uid,
          "My Agency",
          Seq(
            ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = false),
            ClientConsent(invitationIdPIR, expiryDate, "afi", consent = false),
            ClientConsent(invitationIdVAT, expiryDate, "vat", consent = false),
            ClientConsent(invitationIdCgt, expiryDate, "cgt", consent = false)
          )
        ),
        Nil
      )

      val result = controller.submitCheckAnswers(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
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

      val result = controller.submitCheckAnswers(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
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

      val result = controller.submitCheckAnswers(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showAllResponsesFailed().url)
    }
  }

  "GET /confirm-decline" should {
    def request = requestWithJourneyIdInCookie("GET", "/confirm-decline")

    behave like anActionHandlingSessionExpiry(controller.showConfirmDecline)

    "display the confirm decline page for single itsa consent" in {
      journeyState.set(
        ConfirmDecline(
          personal,
          "uid",
          "My Agency",
          Seq(ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = false))),
        Nil)

      val result = controller.showConfirmDecline(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-decline.heading"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-decline.itsa.sub-header", "My Agency"))
    }

    "display the confirm decline page for single cgt consent" in {
      journeyState.set(
        ConfirmDecline(
          personal,
          "uid",
          "My Agency",
          Seq(ClientConsent(invitationIdCgt, expiryDate, "cgt", consent = false))),
        Nil)

      val result = controller.showConfirmDecline(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-decline.heading"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-decline.cgt.sub-header", "My Agency"))
    }

    "display the confirm decline page for multi consent" in {
      journeyState.set(
        ConfirmDecline(
          personal,
          "uid",
          "My Agency",
          Seq(
            ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = false),
            ClientConsent(invitationIdCgt, expiryDate, "cgt", consent = false))
        ),
        Nil
      )

      val result = controller.showConfirmDecline(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-decline.sub-header", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-decline.itsa.service-name"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-decline.cgt.service-name"))
    }
  }

  "POST /confirm-decline" should {
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
        authorisedAsIndividualClientWithSomeSupportedEnrolments(request.withFormUrlEncodedBody("accepted" -> "true")))
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
        authorisedAsIndividualClientWithSomeSupportedEnrolments(request.withFormUrlEncodedBody("accepted" -> "false")))
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
        authorisedAsIndividualClientWithSomeSupportedEnrolments(request.withFormUrlEncodedBody("accepted" -> "true")))
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
        authorisedAsIndividualClientWithSomeSupportedEnrolments(request.withFormUrlEncodedBody("accepted" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showSomeResponsesFailed().url)
    }
  }

  "GET /invitations/accepted" should {
    def request = requestWithJourneyIdInCookie("GET", "/warm-up/accepted")

    behave like anActionHandlingSessionExpiry(controller.showInvitationsAccepted)

    "display the accepted page for multi consents" in {
      journeyState
        .set(
          InvitationsAccepted(
            "My Agency",
            Seq(
              ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = true),
              ClientConsent(invitationIdCgt, expiryDate, "cgt", consent = true)),
            personal),
          Nil
        )

      val result = controller.showInvitationsAccepted(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete.multi.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete.multi.p1.head", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete.multi.p1.itsa"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete.multi.p1.cgt"))

      checkInviteSentPageContainsSurveyLink(result, false)
    }

    "display the accepted page for single consent" in {
      journeyState
        .set(
          InvitationsAccepted(
            "My Agency",
            Seq(ClientConsent(invitationIdCgt, expiryDate, "cgt", consent = true)),
            business),
          Nil)

      val result = controller.showInvitationsAccepted(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete.cgt.p1", "My Agency"))
    }
  }
  "GET /declined" should {
    def request = requestWithJourneyIdInCookie("GET", "/declined")

    behave like anActionHandlingSessionExpiry(controller.showInvitationsDeclined)

    "display the rejected page for itsa" in {
      journeyState
        .set(
          InvitationsDeclined(
            "My Agency",
            Seq(ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = false)),
            personal),
          Nil)

      val result = controller.showInvitationsDeclined(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-declined.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-declined.multi.itsa.p1", "My Agency"))
    }

    "display the rejected page for cgt" in {
      journeyState
        .set(
          InvitationsDeclined(
            "My Agency",
            Seq(ClientConsent(invitationIdCgt, expiryDate, "cgt", consent = false)),
            personal),
          Nil)

      val result = controller.showInvitationsDeclined(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-declined.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-declined.multi.cgt.p1", "My Agency"))
    }
  }
  "GET /warm-up/all-failed" should {
    def request = requestWithJourneyIdInCookie("GET", "/warm-up/all-failed")

    behave like anActionHandlingSessionExpiry(controller.showAllResponsesFailed)

    "display the all responses failed page" in {
      journeyState
        .set(AllResponsesFailed, Nil)

      val result = controller.showAllResponsesFailed(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
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
            Seq(
              ClientConsent(invitationIdITSA, expiryDate, "itsa", consent = true),
              ClientConsent(invitationIdCgt, expiryDate, "cgt", consent = true)),
            Seq(ClientConsent(invitationIdPIR, expiryDate, "afi", consent = true)),
            personal
          ),
          Nil
        )

      val result = controller.showSomeResponsesFailed(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("some-responses-failed.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("some-responses-failed.p1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("some-responses-failed.li.itsa"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("some-responses-failed.li.cgt"))
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
            Seq(ClientConsent(invitationIdPIR, expiryDate, "afi", consent = true)),
            personal
          ),
          Nil
        )

      val result = controller.submitSomeResponsesFailed(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showInvitationsAccepted().url)

      journeyState.get.get._1 shouldBe
        InvitationsAccepted(
          "My Agency",
          Seq(ClientConsent(invitationIdPIR, expiryDate, "afi", consent = true)),
          personal)
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

  "GET /not-found" should {
    def request = requestWithJourneyIdInCookie("GET", "/not-found")
    "display the page as expected" in {
      journeyState.set(NotFoundInvitation, Nil)

      val result = controller.showNotFoundInvitation(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-found-invitation.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-found-invitation.description.1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-found-invitation.description.2"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-found-invitation.description.3"))
    }
  }

  "GET /action-needed" should {
    def request = requestWithJourneyIdInCookie("GET", "/action-needed")
    "display the page as expected for clientType=personal" in {
      journeyState.set(ActionNeeded(personal), Nil)

      val result = controller.showActionNeeded(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("action-needed.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("action-needed.summary", htmlEscapedMessage("action-needed.vat")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("action-needed.details.p1", htmlEscapedMessage("action-needed.vat.link-text")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("action-needed.summary", htmlEscapedMessage("action-needed.itsa")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("action-needed.details.p1", htmlEscapedMessage("action-needed.itsa.link-text")))
      checkResultContainsLink(result,"https://www.gov.uk/guidance/sign-up-for-making-tax-digital-for-vat", "sign up to Making Tax Digital for VAT (opens in a new tab)", newWin = true)
      checkResultContainsLink(result,"https://www.gov.uk/guidance/use-software-to-send-income-tax-updates", "sign up to the Making Tax Digital pilot for Income Tax (opens in a new tab)", newWin = true)
    }
    "display the page as expected for clientType=business" in {
      journeyState.set(ActionNeeded(business), Nil)

      val result = controller.showActionNeeded(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("action-needed.header"))
    checkHtmlResultWithBodyText(result, htmlEscapedMessage("action-needed.summary", htmlEscapedMessage("action-needed.vat")))
    checkHtmlResultWithBodyText(result, htmlEscapedMessage("action-needed.details.p1", htmlEscapedMessage("action-needed.vat.link-text")))
    checkHtmlResultWithBodyText(result, htmlEscapedMessage("action-needed.summary", htmlEscapedMessage("action-needed.ters")))
    checkHtmlResultWithBodyText(result, htmlEscapedMessage("action-needed.details.ters.p1"))
    checkHtmlResultWithBodyText(result, htmlEscapedMessage("action-needed.details.ters.p2"))
    checkResultContainsLink(result,"https://www.gov.uk/guidance/sign-up-for-making-tax-digital-for-vat", "sign up to Making Tax Digital for VAT (opens in a new tab)", newWin = true)
  }
  }

  "GET /respond/error/already-responded-to-request" should {
    def request = requestWithJourneyIdInCookie("GET", "/respond/error/already-responded-to-request")
    "display the page as expected" in {
      journeyState.set(AlreadyRespondedToRequest("1/1/2020"), Nil)

      val result = controller.showErrorAlreadyRespondedToRequest(authorisedAsIndividualClientWithAllSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("already-responded.header"))
      checkIncludesText(result,"You responded to this request on 1/1/2020.")
      checkIncludesText(result,"If you think this is wrong, contact the agent who sent you the request or <a target=\"_blank\" href=\"someAgentClientManagementFrontendExternalUrl#history\">view your request history")
    }
  }

  "GET /respond/error/agent-cancelled-request" should {
    def request = requestWithJourneyIdInCookie("GET", "/respond/error/agent-cancelled-request")
    "display the page as expected" in {
      journeyState.set(AgentCancelledRequest("d/M/yyyy"), Nil)

      val result = controller.showErrorAgentCancelledRequest(authorisedAsIndividualClientWithAllSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("agent-cancelled-request.header"))
      checkIncludesText(result, "This request was cancelled on d/M/yyyy. Ask your agent to send you another authorisation request link if you still want to authorise them.")
      checkResultContainsLink(result, "someAgentClientManagementFrontendExternalUrl#history","View your request history",None)

    }
  }

  "GET /respond/error/request-expired" should {
    def request = requestWithJourneyIdInCookie("GET", "/respond/error/request-expired")
    "display the page as expected" in {
      journeyState.set(RequestExpired("d/M/yyyy"), Nil)

      val result = controller.showErrorRequestExpired(authorisedAsIndividualClientWithAllSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("request-expired.header"))
      checkIncludesText(result, "This request expired on d/M/yyyy. Ask your agent to send you another authorisation request link if you still want to authorise them.")
      checkResultContainsLink(result, "someAgentClientManagementFrontendExternalUrl#history","View your request history",None)
    }
  }

  "GET /trust-not-claimed" should {
    def request = requestWithJourneyIdInCookie("GET", "/trust-not-claimed")
    "display the page as expected" in {
      journeyState.set(TrustNotClaimed, Nil)

      val result = controller.showTrustNotClaimed(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("trust-not-claimed.client.p1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("trust-not-claimed.client.p2"))
    }
  }

  "GET /respond/error/cannot-view-request" should {
    "display the error cannot view request page when current state is WarmUp" in {
      journeyState.set(WarmUp(personal, uid, arn, "My Agency", "my-agency"), Nil)
      val result = controller.showErrorCannotViewRequest(authorisedAsValidAgent(FakeRequest(), arn.value))

      status(result) shouldBe 403

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.cannot-view-request.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.cannot-view-request.p1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.cannot-view-request.p2"))
      checkHtmlResultWithBodyText(result , "If you are not an agent, sign in with the Government Gateway user ID that you use for your personal tax affairs.")
    }

    "display the not authorised as client view if the current state is not WarmUp" in {
      journeyState.set(TrustNotClaimed, Nil)
      val result = controller.showErrorCannotViewRequest(authorisedAsValidAgent(FakeRequest(), arn.value))

      status(result) shouldBe 403

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-authorised.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-authorised.description.p1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-authorised.description.p2"))
    }
  }

  "GET /sign-out-redirect" should {
    "clear current session and redirect to /warm-up when the journeyId is in session" in {
      val result = controller.signOutAndRedirect(authorisedAsValidAgent(FakeRequest()
        .withSession("clientInvitationJourney" -> "foo"),arn.value))

      status(result) shouldBe 303

      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.submitWarmUp().url)
    }

    "redirect to /gg/-sign-out when journeyId is not in session" in {
      val result = controller.signOutAndRedirect(authorisedAsValidAgent(FakeRequest(),arn.value))

      status(result) shouldBe 303

      redirectLocation(result) shouldBe Some("http://localhost:9025/gg/sign-out")
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
      val result = controller.showCannotConfirmIdentity(Some("valid-uuid"), Some("success-url"))(FakeRequest())
      status(result) shouldBe 403

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("technical-issues.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("technical-issues.p1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("technical-issues.p2"))
      checkHtmlResultWithBodyText(
        result,
        "Call the VAT online services helpline",
        "if you need help with Making Tax Digital for VAT.",
        "Call the HMRC Self Assessment online services helpline",
        "if you need help with Making Tax Digital for Income Tax."
      )

      checkResultContainsLink(
        result,
        "https://www.gov.uk/government/organisations/hm-revenue-customs/contact/vat-online-services-helpdesk",
        "Call the VAT online services helpline")
      checkResultContainsLink(
        result,
        "https://www.gov.uk/government/organisations/hm-revenue-customs/contact/self-assessment-online-services-helpdesk",
        "Call the HMRC Self Assessment online services helpline"
      )
    }
  }

  "GET /pdv-complete" should {

    val validationId = "1234567890"
    val targetUrl = "/targetUrl"
    val providerId = "41414"

    def request: FakeRequest[AnyContentAsEmpty.type] = requestWithJourneyIdInCookie("GET", "/pdv-complete")

    "redirect to targetUrl when validation and upsert succeed" in {

      givenIVUpsertSucceeded
      givenPdvValidationSuccess(validationId)
      val result = controller.pdvComplete(Some(targetUrl), Some(validationId))(
        authorisedAsIndividualWithCredentialRetrieval(request, providerId))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(targetUrl)
    }

    "show internal server error when validation succeeds but upsert failed" in {

      givenIVUpsertFailed
      givenPdvValidationSuccess(validationId)
      val result = controller.pdvComplete(Some(targetUrl), Some(validationId))(
        authorisedAsIndividualWithCredentialRetrieval(request, providerId))
      status(result) shouldBe 500
    }

    "show internal server error when no validationId provided" in {

      val result = controller.pdvComplete(Some("/targetUrl"), None)(
        authorisedAsIndividualWithCredentialRetrieval(request, providerId))
      status(result) shouldBe 500
    }

    "show internal server error when no targetUrl provided" in {

      val result = controller.pdvComplete(None, Some("1234567890"))(
        authorisedAsIndividualWithCredentialRetrieval(request, providerId))
      status(result) shouldBe 500
    }

    "show cannot-confirm-identity when validation failed" in {

      givenPdvValidationFailure(validationId)
      val result = controller.pdvComplete(Some(targetUrl), Some(validationId))(
        authorisedAsIndividualWithCredentialRetrieval(request, providerId))
      status(result) shouldBe 403
    }

    "show internal server error when validation data not found in PDV" in {

      givenPdvValidationNotFound(validationId)
      val result = controller.pdvComplete(Some(targetUrl), Some(validationId))(
        authorisedAsIndividualWithCredentialRetrieval(request, providerId))
      status(result) shouldBe 500
    }

    "show internal server error when there is no NINO in PDV response" in {

      givenPdvValidationSuccessNoNino(validationId)
      val result = controller.pdvComplete(Some(targetUrl), Some(validationId))(
        authorisedAsIndividualWithCredentialRetrieval(request, providerId))
      status(result) shouldBe 500
    }

  }

  Set(FailedMatching, FailedDirectorCheck, FailedIV, InsufficientEvidence).foreach { reason =>
    s"IV returns failed reason $reason " when {
      "display the default page" in {
        givenIVFailureReasonResponse(reason)
        val result = controller.showCannotConfirmIdentity(Some("valid-uuid"), Some("success-url"))(FakeRequest())
        status(result) shouldBe 403
        checkHtmlResultWithBodyMsgs(
          result,
          "cannot-confirm-identity.header",
          "cannot-confirm-identity.p1",
          "cannot-confirm-identity.p2")
        checkResultContainsLink(result, "/invitations/warm-up", "Try again", Some("button"), false, true )
      }
    }
  }

  Set(
    TechnicalIssue,
    FailedMatching,
    FailedDirectorCheck,
    FailedIV,
    InsufficientEvidence,
    TimedOut,
    UserAborted,
    LockedOut)
    .foreach { reason =>
      s"IV returns failed reason $reason" when {
        "display the signed out page" in {
          givenIVFailureReasonResponse(reason)
          val result = controller.showCannotConfirmIdentity(Some("valid-uuid"), Some("success-url"))(FakeRequest())
          val resultCode = status(result)
          reason match {
            case TechnicalIssue => {
              resultCode shouldBe 403
              checkHtmlResultWithBodyMsgs(
                result,
                "technical-issues.header",
                "technical-issues.p1",
                "technical-issues.p2")
              checkResultContainsLink(
                result,
                "https://www.gov.uk/government/organisations/hm-revenue-customs/contact/vat-online-services-helpdesk",
                "Call the VAT online services helpline")
              checkResultContainsLink(
                result,
                "https://www.gov.uk/government/organisations/hm-revenue-customs/contact/self-assessment-online-services-helpdesk",
                "Call the HMRC Self Assessment online services helpline"
              )
            }
            case FailedMatching | FailedDirectorCheck | FailedIV | InsufficientEvidence => {
              resultCode shouldBe 403
              checkHtmlResultWithBodyMsgs(
                result,
                "cannot-confirm-identity.header",
                "cannot-confirm-identity.p1",
                "cannot-confirm-identity.p2")
              checkResultContainsLink(result, "/invitations/warm-up", "Try again", Some("button"), false, true)
            }
            case UserAborted | TimedOut => resultCode shouldBe 303
            case LockedOut              => resultCode shouldBe 303
            case _                      => resultCode shouldBe 403
          }
        }
      }
    }

  private def anActionHandlingSessionExpiry(action: Action[AnyContent]) =
    "redirect to /session-timeout if there is no journey ID/history available" when {
      "logged in" in {
        checkRedirectsToSessionExpiry(authorisedAsIndividualClientWithSomeSupportedEnrolments(FakeRequest()))
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
