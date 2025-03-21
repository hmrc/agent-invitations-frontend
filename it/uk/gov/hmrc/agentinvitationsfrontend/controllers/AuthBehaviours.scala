package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import play.api.mvc.{Action, AnyContent, AnyContentAsEmpty}
import play.api.test.FakeRequest
import play.api.test.Helpers.redirectLocation
import uk.gov.hmrc.agentinvitationsfrontend.stubs.AuthStubs
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.{HeaderCarrier, SessionKeys}

trait AuthBehaviours extends AuthStubs {
  self: BaseISpec =>

  def anAuthorisedAgentEndpoint(request: FakeRequest[AnyContentAsEmpty.type], action: Action[AnyContent])(implicit
    defaultAwaitTimeout: akka.util.Timeout
  ): Unit = {

    "return 303 for an Agent with no enrolments and redirected to agent subscription" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = action(authenticatedClient(request.withSession(SessionKeys.authToken -> "Bearer XYZ"), "", Enrolment("", "", "")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("someSubscriptionExternalUrl")
      verifyAuthoriseAttempt()
    }

    "return 303 for no Agent and redirected to Login Page" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = action(authenticatedClient(request, "", Enrolment("OtherEnrolment", "Key", "Value")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("someSubscriptionExternalUrl")
      verifyAuthoriseAttempt()
    }

    "return 303 for not logged in user and redirected to Login Page" in {
      givenUnauthorisedWith("MissingBearerToken")
      val result = action(request)
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(
        "http://localhost:9099/bas-gateway/sign-in?origin=agent-invitations-frontend&continue_url=http://localhost:9448/track/"
      )
    }
  }

  def anIndividualWithLowConfidenceLevelAndNinoGetEndpoint(request: FakeRequest[AnyContentAsEmpty.type], action: Action[AnyContent])(implicit
    defaultAwaitTimeout: akka.util.Timeout
  ): Unit =
    "redirect to Identity Verification when confidence level is below 250 for an Individual with a NINO" in {

      implicit val hc: HeaderCarrier = HeaderCarrier()
      val result = action(authorisedAsIndividualClientWithSomeSupportedEnrolments(request, confidenceLevel = 50))
      val failureUrl: String =
        URLEncoder.encode(
          routes.ClientInvitationJourneyController.showCannotConfirmIdentity(success = Some(request.uri)).url,
          StandardCharsets.UTF_8.toString
        )

      status(result) shouldBe 303
      redirectLocation(result).get should startWith("/mdtp/uplift?origin=aif&confidenceLevel=250")
      redirectLocation(result).get should endWith(s"failureURL=$failureUrl")
    }

  def anIndividualWithConfidenceLevel200AndNinoGetEndpoint(request: FakeRequest[AnyContentAsEmpty.type], action: Action[AnyContent])(implicit
    defaultAwaitTimeout: akka.util.Timeout
  ): Unit =
    "redirect to Identity Verification when confidence level is 200 for an Individual with a NINO" in {

      implicit val hc: HeaderCarrier = HeaderCarrier()
      val result = action(authorisedAsIndividualClientWithSomeSupportedEnrolments(request, confidenceLevel = 200))
      val failureUrl: String =
        URLEncoder.encode(
          routes.ClientInvitationJourneyController.showCannotConfirmIdentity(success = Some(request.uri)).url,
          StandardCharsets.UTF_8.toString
        )

      status(result) shouldBe 303
      redirectLocation(result).get should startWith("/mdtp/uplift?origin=aif&confidenceLevel=250")
      redirectLocation(result).get should endWith(s"failureURL=$failureUrl")
    }

  def aClientWithLowConfidenceLevelPostEndpoint(request: FakeRequest[AnyContentAsEmpty.type], action: Action[AnyContent])(implicit
    defaultAwaitTimeout: akka.util.Timeout
  ): Unit =
    "redirect to cannot confirm identity when the confidence level is below 250 on a post request" in {
      implicit val hc: HeaderCarrier = HeaderCarrier()

      val result = action(authorisedAsIndividualClientWithSomeSupportedEnrolments(request, confidenceLevel = 50))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showCannotConfirmIdentity().url)
    }
}
