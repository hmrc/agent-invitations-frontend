package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import play.api.mvc.{Action, AnyContent, AnyContentAsEmpty}
import play.api.test.FakeRequest
import play.api.test.Helpers.redirectLocation
import uk.gov.hmrc.agentinvitationsfrontend.stubs.AuthStubs
import uk.gov.hmrc.agentinvitationsfrontend.support.{BaseISpec, CallOps}
import uk.gov.hmrc.auth.core.AuthorisationException
import uk.gov.hmrc.http.HeaderCarrier

trait AuthBehaviours extends AuthStubs {
  self: BaseISpec =>

  def anAuthorisedAgentEndpoint(request: FakeRequest[AnyContentAsEmpty.type], action: Action[AnyContent])(
    implicit defaultAwaitTimeout: akka.util.Timeout) = {

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
      an[AuthorisationException] shouldBe thrownBy {
        await(action(request))
      }
      verifyAuthoriseAttempt()
    }
  }

  def anAuthorisedClientGetEndpoint(request: FakeRequest[AnyContentAsEmpty.type], action: Action[AnyContent])(
    implicit defaultAwaitTimeout: akka.util.Timeout) =
    "redirect to Identity Verification when confidence level is below 200" in {
      implicit val hc: HeaderCarrier = HeaderCarrier()
      val result = await(action(authorisedAsAnyIndividualClient(request, confidenceLevel = 50)))
      val failureUrl: String =
        URLEncoder.encode(
          routes.ClientInvitationJourneyController.showCannotConfirmIdentity().url,
          StandardCharsets.UTF_8.toString)

      status(result) shouldBe 303
      redirectLocation(result).get should startWith("/mdtp/uplift?origin=aif&confidenceLevel=200")
      redirectLocation(result).get should endWith(s"failureURL=$failureUrl")
    }

  def anAuthorisedClientPostEndpoint(request: FakeRequest[AnyContentAsEmpty.type], action: Action[AnyContent])(
    implicit defaultAwaitTimeout: akka.util.Timeout) =
    "redirect to cannot confirm identity when the confidence level is below 200 on a post request" in {
      implicit val hc: HeaderCarrier = HeaderCarrier()

      val result = await(action(authorisedAsAnyIndividualClient(request, confidenceLevel = 50)))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.showCannotConfirmIdentity().url)
    }
}
