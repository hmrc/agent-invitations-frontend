package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import play.api.mvc.{Action, AnyContent, AnyContentAsEmpty}
import play.api.test.FakeRequest
import play.api.test.Helpers.redirectLocation
import uk.gov.hmrc.agentinvitationsfrontend.stubs.AuthStubs
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier

trait AuthBehaviours extends AuthStubs {
  self: BaseISpec =>

  def anAuthorisedAgentEndpoint(request: FakeRequest[AnyContentAsEmpty.type], action: Action[AnyContent])(
    implicit defaultAwaitTimeout: akka.util.Timeout): Unit = {

    "return 303 for an Agent with no enrolments and redirected to Login Page" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = await(action(authenticatedClient(request, "", Enrolment("", "", ""))))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("someSubscriptionExternalUrl")
      verifyAuthoriseAttempt()
    }

    "return 303 for no Agent and redirected to Login Page" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = await(action(authenticatedClient(request, "", Enrolment("OtherEnrolment", "Key", "Value"))))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("someSubscriptionExternalUrl")
      verifyAuthoriseAttempt()
    }

    "return 303 for not logged in user and redirected to Login Page" in {
      givenUnauthorisedWith("MissingBearerToken")
      val result = await(action(request))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/gg/sign-in?continue=%2Ftrack%2F&origin=agent-invitations-frontend")
      verifyAuthoriseAttempt()
    }
  }

  def anIndividualWithLowConfidenceLevelWithoutNinoGetEndpoint(
                                                            request: FakeRequest[AnyContentAsEmpty.type],
                                                            action: Action[AnyContent])(implicit defaultAwaitTimeout: akka.util.Timeout): Unit =

    "redirect to Personal Details Validation when confidence level is below 200 for an Individual without a NINO" in {

      implicit val hc: HeaderCarrier = HeaderCarrier()
      val result = await(action(authorisedAsIndividualClientWithSomeSupportedEnrolments(request, confidenceLevel = 50, hasNino = false)))

      // validationId will be added later by PDV on return
      val completionUrl: String =
        URLEncoder.encode(
          routes.ClientInvitationJourneyController.pdvComplete(targetUrl = Some(request.uri), validationId = None).url,
          StandardCharsets.UTF_8.toString)

      status(result) shouldBe 303
      redirectLocation(result).get should startWith("http://localhost:9968/start")
      redirectLocation(result).get should endWith(s"completionUrl=$completionUrl")
    }

  def anIndividualWithLowConfidenceLevelAndNinoGetEndpoint(
    request: FakeRequest[AnyContentAsEmpty.type],
    action: Action[AnyContent])(implicit defaultAwaitTimeout: akka.util.Timeout): Unit =

    "redirect to Identity Verification when confidence level is below 200 for an Individual with a NINO" in {

      implicit val hc: HeaderCarrier = HeaderCarrier()
      val result = await(action(authorisedAsIndividualClientWithSomeSupportedEnrolments(request, confidenceLevel = 50)))
      val failureUrl: String =
        URLEncoder.encode(
          routes.ClientInvitationJourneyController.showCannotConfirmIdentity(success = Some(request.uri)).url,
          StandardCharsets.UTF_8.toString)

      status(result) shouldBe 303
      redirectLocation(result).get should startWith("/mdtp/uplift?origin=aif&confidenceLevel=200")
      redirectLocation(result).get should endWith(s"failureURL=$failureUrl")
    }

  def aClientWithLowConfidenceLevelPostEndpoint(
    request: FakeRequest[AnyContentAsEmpty.type],
    action: Action[AnyContent])(implicit defaultAwaitTimeout: akka.util.Timeout): Unit =

    "redirect to cannot confirm identity when the confidence level is below 200 on a post request" in {
      implicit val hc: HeaderCarrier = HeaderCarrier()

      val result = await(action(authorisedAsIndividualClientWithSomeSupportedEnrolments(request, confidenceLevel = 50)))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showCannotConfirmIdentity().url)
    }

  def anOrganisationWithLowConfidenceWithoutNinoGetEndpoint(
                                                             request: FakeRequest[AnyContentAsEmpty.type],
                                                             action: Action[AnyContent])(implicit defaultAwaitTimeout: akka.util.Timeout): Unit =
    "redirect to Personal Details Validation when confidence level is below 200 for an Organisation with ITSA enrolment and without a NINO" in {
      implicit val hc: HeaderCarrier = HeaderCarrier()
      val result = await(action(authorisedAsITSAOrganisationClient(request, 50)))
      // validationId will be added later by PDV on return
      val completionUrl: String =
        URLEncoder.encode(
          routes.ClientInvitationJourneyController.pdvComplete(targetUrl = Some(request.uri), validationId = None).url,
          StandardCharsets.UTF_8.toString)

      status(result) shouldBe 303
      redirectLocation(result).get should startWith("http://localhost:9968/start")
      redirectLocation(result).get should endWith(s"completionUrl=$completionUrl")
    }
}
