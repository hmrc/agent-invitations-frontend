package uk.gov.hmrc.agentinvitationsfrontend.controllers

import org.joda.time.LocalDate
import org.jsoup.Jsoup
import org.scalatest.Assertion
import play.api.Application
import play.api.mvc._
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{Business, Personal}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.{BaseISpec, CallOps, Css}
import uk.gov.hmrc.agentmtdidentifiers.model.{Service, SuspensionDetails}
import uk.gov.hmrc.http.{HeaderCarrier, SessionKeys}

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration

class ClientInvitationJourneyControllerISpec extends BaseISpec with StateAndBreadcrumbsMatchers with AuthBehaviours {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  override implicit lazy val app: Application = appBuilder
    .overrides(new TestClientInvitationJourneyModule)
    .build()

  lazy val journeyState = app.injector.instanceOf[TestClientInvitationJourneyService]
  lazy val controller: ClientInvitationJourneyController = app.injector.instanceOf[ClientInvitationJourneyController]
  lazy val journeyIdKey = app.injector.instanceOf[ClientInvitationJourneyService].journeyKey

  import journeyState.model.State._

  implicit val timeoutDuration: Duration = defaultAwaitTimeout.duration

  val emptyBasket = Set.empty[AuthorisationRequest]

  def requestWithJourneyIdInCookie(method: String, path: String): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest(method, path).withSession(journeyIdKey -> UUID.randomUUID().toString, SessionKeys.authToken -> "Bearer XYZ")

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

      "show new warmup prototype when personal client type" in new Setup {
        val reqAuthorisedWithJourneyId =
          authorisedAsIndividualClientWithSomeSupportedEnrolments(requestWithJourneyIdInCookie("GET", endpointUrl))
        val result = controller.warmUp("personal", uid, "My-Agency")(reqAuthorisedWithJourneyId)
        val agencyName = "My Agency"
        val htmlString = Helpers.contentAsString(result)
        val html = Jsoup.parse(htmlString)
        html.select(Css.H1).text() shouldBe "Authorise My Agency to deal with HMRC for you"
        html.select("#inset_text").text() shouldBe s"If you authorise ${agencyName}, this will cancel any consent you gave to someone else to act for you for the same service."
        val paragraphs = html.select(Css.paragraphs)
        paragraphs.get(0).text() shouldBe s"Use this service to allow ${agencyName} to manage your tax affairs."
        paragraphs.get(1).text() shouldBe "So we can confirm who you are, you will need to sign in with the Government Gateway user ID and password you use for your personal tax affairs or VAT."
        paragraphs.get(2).text() shouldBe "If you do not have a Government Gateway user ID for your personal tax affairs or VAT, you will be able to create a new one."
      }

      "show old warmup prototype when business client type" in new Setup {
        val reqAuthorisedWithJourneyId =
          authorisedAsIndividualClientWithSomeSupportedEnrolments(requestWithJourneyIdInCookie("GET", endpointUrl))
        val result = controller.warmUp("business", uid, "My-Agency")(reqAuthorisedWithJourneyId)
        val agencyName = "My Agency"
        val htmlString = Helpers.contentAsString(result)
        val html = Jsoup.parse(htmlString)
        html.select(Css.H1).text() shouldBe "Authorise My Agency to deal with HMRC for you"
        html.select("#inset_text").text() shouldBe s"If you authorise ${agencyName}, this will cancel any consent you gave to someone else to act for you for the same service."
        val paragraphs = html.select(Css.paragraphs)
        paragraphs.get(0).text() shouldBe s"Use this service to allow ${agencyName} to manage your tax affairs."
        paragraphs.get(1).text() shouldBe "So we can confirm who you are, you will need to sign in with the Government Gateway user ID and password you use for your business tax affairs."
        paragraphs.get(2).text() shouldBe "I do not want to authorise My Agency"
      }

      "work when signed in" in new Setup {
        val reqAuthorisedWithJourneyId =
          authorisedAsIndividualClientWithSomeSupportedEnrolments(requestWithJourneyIdInCookie("GET", endpointUrl))
        val result = controller.warmUp("personal", uid, "My-Agency")(reqAuthorisedWithJourneyId)
        val agencyName = "My Agency"
        val htmlString = Helpers.contentAsString(result)
        val html = Jsoup.parse(htmlString)
        html.select(Css.H1).text() shouldBe "Authorise My Agency to deal with HMRC for you"
        html.select("#inset_text").text() shouldBe s"If you authorise ${agencyName}, this will cancel any consent you gave to someone else to act for you for the same service."
        val paragraphs = html.select(Css.paragraphs)
        paragraphs.get(0).text() shouldBe s"Use this service to allow ${agencyName} to manage your tax affairs."
        paragraphs.get(1).text() shouldBe "So we can confirm who you are, you will need to sign in with the Government Gateway user ID and password you use for your personal tax affairs or VAT."
        paragraphs.get(2).text() shouldBe "If you do not have a Government Gateway user ID for your personal tax affairs or VAT, you will be able to create a new one."
      }

      "work when not signed in" in new Setup {
        val reqWithJourneyId = requestWithJourneyIdInCookie("GET", endpointUrl)
        val result = controller.warmUp("personal", uid, "My-Agency")(reqWithJourneyId)
        val agencyName = "My Agency"
        val htmlString = Helpers.contentAsString(result)
        val html = Jsoup.parse(htmlString)
        html.select(Css.H1).text() shouldBe "Authorise My Agency to deal with HMRC for you"
        html.select("#inset_text").text() shouldBe s"If you authorise ${agencyName}, this will cancel any consent you gave to someone else to act for you for the same service."
        val paragraphs = html.select(Css.paragraphs)
        paragraphs.get(0).text() shouldBe s"Use this service to allow ${agencyName} to manage your tax affairs."
        paragraphs.get(1).text() shouldBe "So we can confirm who you are, you will need to sign in with the Government Gateway user ID and password you use for your personal tax affairs or VAT."
        paragraphs.get(2).text() shouldBe "If you do not have a Government Gateway user ID for your personal tax affairs or VAT, you will be able to create a new one."
      }

      "remove spaces in the url" in new Setup {
        val reqAuthorisedWithJourneyId =
          authorisedAsIndividualClientWithSomeSupportedEnrolments(requestWithJourneyIdInCookie("GET", endpointUrl))
        val result = controller.warmUp("personal", uid, "My-Agency ")(reqAuthorisedWithJourneyId)
        val htmlString = Helpers.contentAsString(result)
        val html = Jsoup.parse(htmlString)
        val agencyName = "My Agency"
        html.select(Css.H1).text() shouldBe "Authorise My Agency to deal with HMRC for you"
        html.select("#inset_text").text() shouldBe s"If you authorise ${agencyName}, this will cancel any consent you gave to someone else to act for you for the same service."
        val paragraphs = html.select(Css.paragraphs)
        paragraphs.get(0).text() shouldBe s"Use this service to allow ${agencyName} to manage your tax affairs."
        paragraphs.get(1).text() shouldBe "So we can confirm who you are, you will need to sign in with the Government Gateway user ID and password you use for your personal tax affairs or VAT."
        paragraphs.get(2).text() shouldBe "If you do not have a Government Gateway user ID for your personal tax affairs or VAT, you will be able to create a new one."
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
    }

    "journey ID is not already present in the session cookie, redirect to same page saving the journey ID in the session" should {
      "work when signed in" in new Setup {
        def request = authorisedAsIndividualClientWithSomeSupportedEnrolments(FakeRequest("GET", endpointUrl))
        val result = controller.warmUp("personal", uid, "My-Agency")(request)
        checkRedirectedWithJourneyId(result.futureValue, request, journeyIdKey)
      }

      "work when not signed in" in new Setup {
        def request = FakeRequest("GET", endpointUrl)
        val result = controller.warmUp("personal", uid, "My-Agency")(request)
        checkRedirectedWithJourneyId(result.futureValue, request, journeyIdKey)
      }

      def checkRedirectedWithJourneyId(result: Result, request: Request[_], journeyIdKey: String): Assertion = {
        status(result) shouldBe 303
        redirectLocation(Future.successful(result)) shouldBe Some(request.uri)

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
      behave like anIndividualWithLowConfidenceLevelAndNinoGetEndpoint(request(), controller.submitWarmUp)
    }

    "user is authenticated as Organisation with ITSA enrolment and without NINO, and low confidence level" should {
      val request = () => requestWithJourneyIdInQuery("GET", "/warm-up")
      behave like anIndividualWithLowConfidenceLevelAndNinoGetEndpoint(request(), controller.submitWarmUp)
    }

    "user is authenticated as CGT individual and low confidence level" should {
      "not go through IV and redirect to consent page if the invitation is found" in {
        val request = () => requestWithJourneyIdInQuery("POST", "/warm-up")
        authorisedAsAnyCGTIndividualClientWithLowCL(request())

        givenGetSuspensionDetailsClientStub(arn, SuspensionDetails(suspensionStatus = false, None))
        givenAllInvitationIdsByStatus(uid, "Pending")
        journeyState.set(WarmUp(Personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showConsent().url)
      }
    }

    def warmupSubmitAccept(request: () => FakeRequest[AnyContentAsEmpty.type]): Unit = {

      "redirect to consent page if the invitation is found" in {
        givenGetSuspensionDetailsClientStub(arn, SuspensionDetails(suspensionStatus = false, None))
        givenAllInvitationIdsByStatus(uid, "Pending")
        journeyState.set(WarmUp(Personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showConsent().url)
      }

      "redirect to /respond/error/cannot-find-request if there are no invitations found " +
        "and the client has SOME supported MTD enrolments" in {
        givenAllInvitationIdsByStatusReturnsEmpty(uid)
        journeyState.set(WarmUp(Personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showErrorCannotFindRequest().url)
      }

      "redirect to /respond/error/no-outstanding-requests if there are no invitations found " +
        "and the client has ALL supported MTD enrolments" in {
        givenAllInvitationIdsByStatusReturnsEmpty(uid)
        journeyState.set(WarmUp(Personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithAllSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showErrorNoOutstandingRequests().url)
      }

      "redirect to /cannot-find-request if the client has no supported MTD enrolments" in {
        journeyState.set(WarmUp(Personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithNoSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showErrorCannotFindRequest().url)
      }

      "redirect to /respond/error/authorisation-request-already-responded if the invitation has status of Accepted or Rejected " +
        "and the client has only SOME supported MTD enrolments" in {
        givenAllInvitationIdsByStatus(uid, "Accepted")
        journeyState.set(WarmUp(Personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showErrorAuthorisationRequestUnsuccessful().url)
      }

      "redirect to /respond/error/already-responded if the most recent authorisation request has status of Accepted or Rejected " +
        "and the client has ALL supported MTD enrolments" in {
        givenAllInvitationIdsWithMixedStatus(uid, "Rejected")
        journeyState.set(WarmUp(Personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithAllSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showErrorAuthorisationRequestInvalid().url)
      }

      "redirect to /respond/error/agent-cancelled-request if the most recent authorisation request has status of Cancelled " +
        "and the client has ALL supported MTD enrolments" in {
        givenAllInvitationIdsWithMixedStatus(uid, "Cancelled")
        journeyState.set(WarmUp(Personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithAllSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showErrorAuthorisationRequestInvalid().url)
      }

      "redirect to /respond/error/authorisation-request-cancelled if the most recent authorisation request has status of Cancelled " +
        "and the client has only SOME supported MTD enrolments" in {
        givenAllInvitationIdsWithMixedStatus(uid, "Cancelled")
        journeyState.set(WarmUp(Personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showErrorAuthorisationRequestUnsuccessful().url)
      }

      "redirect to /respond/error/authorisation-request-expired if the most recent authorisation request has status of Expired " +
        "and the client has only SOME supported MTD enrolments" in {
        givenAllInvitationIdsWithMixedStatus(uid, "Expired")
        journeyState.set(WarmUp(Personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showErrorAuthorisationRequestUnsuccessful().url)
      }

      "redirect to /respond/error/authorisation-request-expired if the invitation has mixed statuses, none of which are Pending" in {
        givenAllInvitationIdsWithMixedStatus(uid, "Expired")
        journeyState.set(WarmUp(Personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showErrorAuthorisationRequestUnsuccessful().url)
      }

      "redirect to suspended agent if the agent is suspended for all consent services" in {
        givenGetSuspensionDetailsClientStub(arn, SuspensionDetails(suspensionStatus = true, Some(Set("ITSA", "VATC"))))
        givenAllInvitationIdsByStatus(uid, "Pending")
        journeyState.set(WarmUp(Personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showSuspendedAgent().url)
      }

      "redirect to /trust-not-claimed if client doesn't have the trust enrolment but invitation contains trust" in {
        givenAllInvitationIdsWithTrustByStatus(uid, "Pending")
        journeyState.set(WarmUp(Business, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUp(authorisedAsAnyOrganisationClient(request()))
        status(result) shouldBe 303

        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showTrustNotClaimed().url)
      }
    }

    "redirect to /respond/error/cannot-view-request when an agent tries to respond to a clients invitation" in {
      givenAllInvitationIdsByStatus(uid, "Pending")
      journeyState.set(WarmUp(Personal, uid, arn, "My Agency", "my-agency"), Nil)
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
      journeyState.set(WarmUp(Personal, uid, arn, "My Agency", "my-agency"), Nil)
      val request = () => requestWithJourneyIdInCookie("GET", "/warm-up")

      intercept[Exception] {
        await(controller.submitWarmUp(request()))
      }.getMessage shouldBe "UnsupportedAffinityGroup"
    }

    "redirect to /Government-Gateway-user-ID-needed when there is no session and client type is personal" in {
      givenUnauthorisedWith("MissingBearerToken")
      journeyState.set(WarmUp(Personal, uid, arn, "My Agency", "my-agency"), Nil)
      val request = () => FakeRequest("GET", "/warm-up").withSession(journeyIdKey -> "foo")

      val result = controller.submitWarmUp(request())
      status(result) shouldBe 303

      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showGGUserIdNeeded().url)
    }

    "redirect to /warm-up/session-required when there is no session and client type is business" in {
      givenUnauthorisedWith("MissingBearerToken")
      journeyState.set(WarmUp(Business, uid, arn, "My Agency", "my-agency"), Nil)
      val request = () => FakeRequest("GET", "/warm-up").withSession(journeyIdKey -> "foo")

      val result = controller.submitWarmUp(request())
      status(result) shouldBe 303

      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.submitWarmUpSessionRequired().url)
    }
  }

  "GET /Government-Gateway-user-ID-needed" should {
    "display the correct content" in {
      def request() = requestWithJourneyIdInCookie("GET", "/Government-Gateway-user-ID-needed")
    journeyState.set(GGUserIdNeeded(Personal, "uid", arn, "name"), Nil)
      val result = controller.showGGUserIdNeeded(request())

      status(result) shouldBe 200

      checkHtmlResultWithBodyMsgs(
        result,
        "gg-user-id-needed.h1",
        "gg-user-id-needed.p",
        "global.yes",
        "global.no")
    }
  }

  "POST /Government-Gateway-user-ID-needed" should {
    "redirect to /warm-up/session-required when 'Yes' was selected" in {
      def request() = requestWithJourneyIdInCookie("POST", "/Government-Gateway-user-ID-needed")

      journeyState.set(GGUserIdNeeded(Personal, "uid", arn, "name"), Nil)

      val result =
        controller.submitGGUserIdNeeded(authorisedAsIndividualClientWithSomeSupportedEnrolments(request.withFormUrlEncodedBody("accepted" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.submitWarmUpSessionRequired().url)
    }

    "redirect to /which-tax-service when 'No' was selected" in {
      def request() = requestWithJourneyIdInCookie("POST", "/Government-Gateway-user-ID-needed")

      journeyState.set(GGUserIdNeeded(Personal, "uid", arn, "name"), Nil)

      val result =
        controller.submitGGUserIdNeeded(authorisedAsIndividualClientWithSomeSupportedEnrolments(request.withFormUrlEncodedBody("accepted" -> "false")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showWhichTaxService().url)
    }
  }

  "GET /which-tax-service" should {
    "display the correct content" in {
      def request() = requestWithJourneyIdInCookie("GET", "/which-tax-service")
      journeyState.set(WhichTaxService(Personal, "uid", arn, "name"), Nil)
      val result = controller.showWhichTaxService(request())

      status(result) shouldBe 200

      checkHtmlResultWithBodyMsgs(
        result,
        "which-service.header",
        "global.yes",
        "global.no")
    }
  }

  "POST /which-tax-service" should {
    "redirect to /create-new-user-ID when 'Yes' was selected" in {
      def request() = requestWithJourneyIdInCookie("POST", "/which-tax-service")

      journeyState.set(WhichTaxService(Personal, "uid", arn, "name"), Nil)

      val result =
        controller.submitWhichTaxService(authorisedAsIndividualClientWithSomeSupportedEnrolments(request.withFormUrlEncodedBody("accepted" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showCreateNewUserId().url)
    }

    "redirect to /invitations/sign-up-to-tax-service when 'No' was selected" in {
      def request() = requestWithJourneyIdInCookie("POST", "/which-tax-service")

      journeyState.set(WhichTaxService(Personal, "uid", arn, "name"), Nil)

      val result =
        controller.submitWhichTaxService(authorisedAsIndividualClientWithSomeSupportedEnrolments(request.withFormUrlEncodedBody("accepted" -> "false")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showSignUpToTaxService().url)
    }
  }

  "GET /create-new-user-id" should {
    "display the correct content" in {
      def request() = requestWithJourneyIdInCookie("GET", "/create-new-user-id")
      journeyState.set(CreateNewUserId(Personal, "uid", arn, "name"), Nil)
      val result = controller.showCreateNewUserId(request())

      status(result) shouldBe 200

      checkHtmlResultWithBodyMsgs(
        result,
        "new-gg-id.header",
        "new-gg-id.li1",
        "new-gg-id.li2")
    }
  }

  "GET /sign-up-to-tax-service" should {
    "display the correct content" in {
      def request() = requestWithJourneyIdInCookie("GET", "/sign-up-to-tax-service")
      journeyState.set(SignUpToTaxService("uid"), Nil)
      val result = controller.showSignUpToTaxService(request())

      status(result) shouldBe 200

      checkHtmlResultWithBodyMsgs(
        result,
        "tax-service.header",
        "tax-service.l1",
        "tax-service.l2")
    }
  }

  "POST warm-up/session-required" should {
    "redirect to /consent when invitation found" in {

      def request() = requestWithJourneyIdInCookie("GET", "/warm-up/session-required")

      givenGetSuspensionDetailsClientStub(arn, SuspensionDetails(suspensionStatus = false, None))
      givenAllInvitationIdsByStatus(uid, "Pending")
      journeyState.set(WarmUpSessionRequired(Personal, uid, arn, "My Agency"), Nil)

      val result = controller.submitWarmUpSessionRequired(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showConsent().url)
    }
  }

  "GET /cannot-appoint" should {
    "display the agent suspended page" in {
      def request = requestWithJourneyIdInCookie("GET", "/cannot-appoint")
      journeyState.set(SuspendedAgent(Personal, "uid", "name", arn, Set("ITSA", "VATC"), Seq()), Nil)

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
        journeyState.set(WarmUp(Personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUpConfirmDecline(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showConfirmDecline().url)
      }

      "redirect to /respond/error/cannot-find-request" in {
        givenAllInvitationIdsByStatusReturnsEmpty(uid)
        journeyState.set(WarmUp(Personal, uid, arn, "My Agency", "my-agency"), Nil)

        val result = controller.submitWarmUpConfirmDecline(authorisedAsIndividualClientWithSomeSupportedEnrolments(request()))
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showErrorCannotFindRequest().url)
      }

      "redirect to TrustNotClaimed if client doesn't have the HMRC-TERS-ORG enrolment" in {
        givenAllInvitationIdsWithTrustByStatus(uid, "Pending")
        journeyState.set(WarmUp(Business, uid, arn, "My Agency", "my-agency"), Nil)

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

      val htmlString = Helpers.contentAsString(result)
      val html = Jsoup.parse(htmlString)
      val externalUrlLink = html.select("a[href='someAgentClientManagementFrontendExternalUrl#history']")
      externalUrlLink.text() shouldBe "view your request history"
      externalUrlLink.hasClass("govuk-link")
      html.select("p#no-outstanding").text() contains "If you think this is wrong, contact the agent who sent you the request or "

    }
  }

  "GET /action-needed" should {
    def request = requestWithJourneyIdInCookie("GET", "warm-up/action-needed")

    behave like anActionHandlingSessionExpiry(controller.showActionNeeded)

    "display the action required page" in {
      journeyState.set(ActionNeeded(Personal), Nil)

      val result = controller.showActionNeeded(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("action-needed.header"))
    }
  }

  "GET /consent" should {
    def request = requestWithJourneyIdInCookie("GET", "/warm-up/consent")

    behave like anActionHandlingSessionExpiry(controller.showConsent)

    "display the multi consent page for alt itsa" in {
      journeyState.set(
        MultiConsent(
          Personal,
          uid,
          "My Agency",
          arn,
          Seq(ClientConsent(invitationIdITSA, LocalDate.now().plusDays(1), "itsa", consent = true, isAltItsa = true))),
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
      checkIncludesText(result, "I consent to HMRC allowing My Agency to manage my Making Tax Digital for Income Tax.")

    }

    "display the multi consent page for itsa" in {
      journeyState.set(
        MultiConsent(
          Personal,
          uid,
          "My Agency",
          arn,
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
      checkIncludesText(result, "I consent to HMRC allowing My Agency to manage my Making Tax Digital for Income Tax.")

    }

    "display the multi consent page for cgt personal" in {
      journeyState.set(
        MultiConsent(
          Personal,
          uid,
          "My Agency",
          arn,
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
          Personal,
          uid,
          "My Agency",
          arn,
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
          Business,
          uid,
          "My Agency",
          arn,
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

    "display the multi consent page for trust - UTR" in {
      journeyState.set(
        MultiConsent(
          Business,
          uid,
          "My Agency",
          arn,
          Seq(ClientConsent(invitationIdTrust, LocalDate.now().plusDays(1), "trust", consent = true))),
        Nil)

      val result = controller.showConsent(authorisedAsOrganisationTrustClient(validUtr)(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.trust.heading"))
      checkHtmlResultWithBodyText(result, hasMessage("confirm-terms-multi.trust.p1", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.trust.list.item1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.trust.list.item2"))
      checkHtmlResultWithBodyText(result, hasMessage("confirm-terms-multi.trust.label", "My Agency"))
    }

    "display the multi consent page for trust - URN" in {
      journeyState.set(
        MultiConsent(
          Business,
          uid,
          "My Agency",
          arn,
          Seq(ClientConsent(invitationIdTrustNT, LocalDate.now().plusDays(1), "trustNT", consent = true))),
        Nil)

      val result = controller.showConsent(authorisedAsOrganisationTrustClient(validUrn)(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.trustNT.heading"))
      checkHtmlResultWithBodyText(result, hasMessage("confirm-terms-multi.trustNT.p1", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.trustNT.list.item1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-multi.trustNT.list.item2"))
      checkHtmlResultWithBodyText(result, hasMessage("confirm-terms-multi.trustNT.label", "My Agency"))
    }
  }

  "POST /consent" should {
    def request = requestWithJourneyIdInCookie("POST", "/warm-up/consent")

    behave like anActionHandlingSessionExpiry(controller.submitConsent)

    "redirect to check answers when continuing" in {
      journeyState.set(
        MultiConsent(
          Personal,
          uid,
          "My Agency",
          arn,
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
          Personal,
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
        Personal,
        uid,
        "My Agency",
        consents
      )

      journeyState.set(
        currentState,
        breadcrumbs = List(
          currentState,
          SingleConsent(
            Personal,
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
          Personal,
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
      givenInvitationByIdSuccess(invitationIdITSA, "ABCDEF123456789", suppliedClientId = Some(nino.value))
      givenInvitationByIdSuccess(invitationIdPIR, "AB123456A")
      givenInvitationByIdSuccess(invitationIdVAT, "101747696")
      givenInvitationByIdSuccess(invitationIdCgt, cgtRef.value, Service.CapitalGains, "CGTPDRef")
      givenAcceptInvitationSucceeds("ABCDEF123456789", invitationIdITSA, identifierITSA)
      givenAcceptInvitationSucceeds("AB123456A", invitationIdPIR, identifierPIR)
      givenAcceptInvitationSucceeds("101747696", invitationIdVAT, identifierVAT)
      givenAcceptInvitationSucceeds(cgtRef.value, invitationIdCgt, "CGTPDRef")
      journeyState.set(
        CheckAnswers(
          Personal,
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
      givenInvitationByIdSuccess(invitationIdITSA, "ABCDEF123456789", suppliedClientId = Some(nino.value))
      givenInvitationByIdSuccess(invitationIdPIR, "AB123456A")
      givenInvitationByIdSuccess(invitationIdVAT, "101747696")
      givenInvitationByIdSuccess(invitationIdCgt, cgtRef.value, Service.CapitalGains, "CGTPDRef")
      givenRejectInvitationSucceeds("ABCDEF123456789", invitationIdITSA, identifierITSA)
      givenRejectInvitationSucceeds("AB123456A", invitationIdPIR, identifierPIR)
      givenRejectInvitationSucceeds("101747696", invitationIdVAT, identifierVAT)
      givenRejectInvitationSucceeds(cgtRef.value, invitationIdCgt, "CGTPDRef")
      journeyState.set(
        CheckAnswers(
          Personal,
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
      givenInvitationByIdSuccess(invitationIdITSA, "ABCDEF123456789", suppliedClientId = Some(nino.value))
      givenInvitationByIdSuccess(invitationIdPIR, "AB123456A")
      givenInvitationByIdSuccess(invitationIdVAT, "101747696")
      givenAcceptInvitationReturnsNotFound("ABCDEF123456789", invitationIdITSA, identifierITSA)
      givenAcceptInvitationSucceeds("AB123456A", invitationIdPIR, identifierPIR)
      givenAcceptInvitationSucceeds("101747696", invitationIdVAT, identifierVAT)
      journeyState.set(
        CheckAnswers(
          Personal,
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
      givenInvitationByIdSuccess(invitationIdITSA, "ABCDEF123456789", suppliedClientId = Some(nino.value))
      givenInvitationByIdSuccess(invitationIdPIR, "AB123456A")
      givenInvitationByIdSuccess(invitationIdVAT, "101747696")
      givenAcceptInvitationReturnsNotFound("ABCDEF123456789", invitationIdITSA, identifierITSA)
      givenAcceptInvitationReturnsNotFound("AB123456A", invitationIdPIR, identifierPIR)
      givenAcceptInvitationReturnsNotFound("101747696", invitationIdVAT, identifierVAT)
      journeyState.set(
        CheckAnswers(
          Personal,
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
          Personal,
          "uid",
          "My Agency",
          arn,
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
          Personal,
          "uid",
          "My Agency",
          arn,
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
          Personal,
          "uid",
          "My Agency",
          arn,
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
      givenInvitationByIdSuccess(invitationIdITSA, "ABCDEF123456789", suppliedClientId = Some(nino.value))
      givenRejectInvitationSucceeds("ABCDEF123456789", invitationIdITSA, identifierITSA)
      journeyState.set(
        ConfirmDecline(
          Personal,
          "uid",
          "My Agency",
          arn,
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
          Personal,
          "uid",
          "My Agency",
          arn,
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
          Personal,
          "uid",
          "My Agency",
          arn,
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
          Personal,
          "uid",
          "My Agency",
          arn,
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
            Personal),
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
            Business),
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
            Personal),
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
            Personal),
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
            Personal
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
            Personal
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
          Personal)
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
      journeyState.set(ActionNeeded(Personal), Nil)

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
      journeyState.set(ActionNeeded(Business), Nil)

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

      val result = controller.showErrorAuthorisationRequestInvalid(authorisedAsIndividualClientWithAllSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("already-responded.header"))
      checkIncludesText(result,"You responded to this request on 1/1/2020.")
      checkIncludesText(result,"If you think this is wrong, contact the agent who sent you the request or <a href=\"someAgentClientManagementFrontendExternalUrl#history\">view your request history")
    }
  }

  "GET /respond/error/agent-cancelled-request" should {
    def request = requestWithJourneyIdInCookie("GET", "/respond/error/agent-cancelled-request")
    "display the page as expected" in {
      journeyState.set(AgentCancelledRequest("d/M/yyyy"), Nil)

      val result = controller.showErrorAuthorisationRequestInvalid(authorisedAsIndividualClientWithAllSupportedEnrolments(request))
      status(result) shouldBe 200

      val htmlString = Helpers.contentAsString(result)
      val html = Jsoup.parse(htmlString)
      val externalUrlLink = html.select("a[href='someAgentClientManagementFrontendExternalUrl#history']")
      externalUrlLink.text() shouldBe "View your request history"
      externalUrlLink.hasClass("govuk-link")
      html.select("p#no-outstanding").text() contains "If you think this is wrong, contact the agent who sent you the request or "
      html.select(Css.H1).text() shouldBe "Your agent cancelled this authorisation request"

      checkIncludesText(result, "This request was cancelled on d/M/yyyy. Ask your agent to send you another authorisation request link if you still want to authorise them.")

    }
  }

  "GET /respond/error/request-expired" should {
    def request = requestWithJourneyIdInCookie("GET", "/respond/error/request-expired")
    "display the page as expected" in {
      journeyState.set(RequestExpired("d/M/yyyy"), Nil)

      val result = controller.showErrorAuthorisationRequestInvalid(authorisedAsIndividualClientWithAllSupportedEnrolments(request))
      status(result) shouldBe 200

      val htmlString = Helpers.contentAsString(result)
      val html = Jsoup.parse(htmlString)

      html.select(Css.H1).get(0).text() shouldBe "This authorisation request has expired"
      html.select(Css.paragraphs).get(0).text() shouldBe "This request expired on d/M/yyyy. Ask your agent to send you another authorisation request link if you still want to authorise them."
      html.select("a[href=someAgentClientManagementFrontendExternalUrl#history]").text() shouldBe "View your request history"

    }
  }

  "GET /respond/error/authorisation-request-expired" should {
    def request = requestWithJourneyIdInCookie("GET", "/respond/error/authorisation-request-expired")
    "display the page as expected" in {
      journeyState.set(AuthorisationRequestExpired("d/M/yyyy", Personal), Nil)

      val result = controller.showErrorAuthorisationRequestUnsuccessful(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      val htmlString = Helpers.contentAsString(result)
      val html = Jsoup.parse(htmlString)

      html.select(Css.H1).get(0).text() shouldBe "This authorisation request has already expired"
      val paragraphs = html.select(Css.paragraphs)
      paragraphs.get(0).text() shouldBe "This request expired on d/M/yyyy. For details, view your history to check for any expired, cancelled or outstanding requests."
      paragraphs.get(0).select("a").text() shouldBe "view your history"
      paragraphs.get(0).select("a").attr("href") shouldBe "someAgentClientManagementFrontendExternalUrl#history"
      paragraphs.get(0).select("a").hasClass("govuk-link")
      paragraphs.get(1).text() shouldBe "If your agent has sent you a recent request, make sure you have signed up to the tax service you need."
      paragraphs.get(2).text() shouldBe "You could also check you have signed in with the correct Government Gateway user ID. It must be the same one you used to sign up to the tax service the authorisation request is for."
      paragraphs.get(3).select("a").text() shouldBe "Sign in with the Government Gateway user ID"
      paragraphs.get(3).select("span").text() shouldBe "you use for managing your personal tax affairs."
    }
  }

  "GET /respond/error/authorisation-request-cancelled" should {
    def request = requestWithJourneyIdInCookie("GET", "/respond/error/authorisation-request-cancelled")
    "display the page as expected" in {
      journeyState.set(AuthorisationRequestCancelled("d/M/yyyy", Personal), Nil)

      val result = controller.showErrorAuthorisationRequestUnsuccessful(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200
      val htmlString = Helpers.contentAsString(result)
      val html = Jsoup.parse(htmlString)

      checkIncludesText(result, "This authorisation request has been cancelled")
      checkIncludesText(result, "This request was cancelled by your agent on d/M/yyyy. For details, <a href=someAgentClientManagementFrontendExternalUrl#history>view your history</a> to check for any expired, cancelled or outstanding requests.")

      val paragraphs = html.select(Css.paragraphs)
      paragraphs.get(1).text() shouldBe "If your agent has sent you a recent request, make sure you have signed up to the tax service you need."
      paragraphs.get(2).text() shouldBe "You could also check you have signed in with the correct Government Gateway user ID. It must be the same one you used to sign up to the tax service the authorisation request is for."
      paragraphs.get(3).select("a").text() shouldBe "Sign in with the Government Gateway user ID"
      paragraphs.get(3).select("span").text() shouldBe "you use for managing your personal tax affairs."
    }
  }

  "GET /respond/error/authorisation-request-already-responded" should {
    def request = requestWithJourneyIdInCookie("GET", "/respond/error/authorisation-request-already-responded")
    "display the page as expected" in {
      journeyState.set(AuthorisationRequestAlreadyResponded("d/M/yyyy", Personal), Nil)

      val result = controller.showErrorAuthorisationRequestUnsuccessful(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      val htmlString = Helpers.contentAsString(result)
      val html = Jsoup.parse(htmlString)

      checkIncludesText(result, "This authorisation request has already been responded to")
      checkIncludesText(result, "This request has already been responded to on d/M/yyyy. For details, <a href=someAgentClientManagementFrontendExternalUrl#history>view your history</a> to check for any expired, cancelled or outstanding requests.")

      val paragraphs = html.select(Css.paragraphs)
      paragraphs.get(1).text() shouldBe "If your agent has sent you a recent request, make sure you have signed up to the tax service you need."
      paragraphs.get(2).text() shouldBe "You could also check you have signed in with the correct Government Gateway user ID. It must be the same one you used to sign up to the tax service the authorisation request is for."
      paragraphs.get(3).select("a").text() shouldBe "Sign in with the Government Gateway user ID"
      paragraphs.get(3).select("span").text() shouldBe "you use for managing your personal tax affairs."

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
      journeyState.set(WarmUp(Personal, uid, arn, "My Agency", "my-agency"), Nil)
      val result = controller.showErrorCannotViewRequest(authorisedAsAnyAgent(FakeRequest()))

      status(result) shouldBe 403

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.cannot-view-request.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.cannot-view-request.p1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.cannot-view-request.p2"))
      checkHtmlResultWithBodyText(result , "If you are not an agent, sign in with the Government Gateway user ID that you use for your personal tax affairs.")
    }

    "display the not authorised as client view if the current state is not WarmUp" in {
      journeyState.set(TrustNotClaimed, Nil)
      val result = controller.showErrorCannotViewRequest(authorisedAsAnyAgent(FakeRequest()))

      status(result) shouldBe 403

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-authorised.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-authorised.description.p1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-authorised.description.p2"))
    }
  }

  "GET /respond/error/cannot-find-request" should {
    def request = requestWithJourneyIdInCookie("GET", "/respond/error/cannot-find-request")
    "display the page as expected" in {
      journeyState.set(CannotFindRequest(Personal, "My Agency"), Nil)

      val result = controller.showErrorCannotFindRequest(authorisedAsIndividualClientWithSomeSupportedEnrolments(request))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, htmlEscapedMessage("cannot-find-request.header"))
      checkIncludesText(result,"We cannot find a request from My Agency.")
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("cannot-find-request.p2"))
      checkIncludesText(result,"You need to sign in with the correct Government Gateway user ID. " +
        "It is possible to have more than one, so make sure it is the same one you used to sign up to the tax service " +
        "the authorisation request is for. <a href=/invitations/sign-out-redirect>Try signing in with a different Government Gateway user ID</a> (the one that you use for managing your personal tax affairs).")
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("cannot-find-request.p4"))
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
        "if you need help with VAT.",
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

  Set(FailedMatching, FailedDirectorCheck, FailedIV, InsufficientEvidence).foreach { reason =>
    s"IV returns failed reason $reason " when {
      "display the default page" in {
        givenIVFailureReasonResponse(reason)
        val result = controller.showCannotConfirmIdentity(Some("valid-uuid"), Some("success-url"))(FakeRequest())
        status(result) shouldBe 403
        val html = Jsoup.parse(Helpers.contentAsString(result))
        html.select(Css.H1).text() shouldBe "We could not confirm your identity"
        val paragraphs = html.select(Css.paragraphs)
        paragraphs.get(0).text() shouldBe "The information you have entered does not match our records."
        paragraphs.get(1).text() shouldBe "If you need help with confirming your identity, use the ‘Get help with this page’ link."
        html.select("a#tryAgainButton").text() shouldBe "Try again"
        html.select("a#tryAgainButton").attr("href") shouldBe "/invitations/warm-up"
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

              val html = Jsoup.parse(Helpers.contentAsString(result))
              html.select(Css.H1).text() shouldBe "We could not confirm your identity"
              val paragraphs = html.select(Css.paragraphs)
              paragraphs.get(0).text() shouldBe "The information you have entered does not match our records."
              paragraphs.get(1).text() shouldBe "If you need help with confirming your identity, use the ‘Get help with this page’ link."
              html.select("a#tryAgainButton").text() shouldBe "Try again"
              html.select("a#tryAgainButton").attr("href") shouldBe "/invitations/warm-up"
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
        val result = action(request)
        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showMissingJourneyHistory().url)
      }
    }
}
