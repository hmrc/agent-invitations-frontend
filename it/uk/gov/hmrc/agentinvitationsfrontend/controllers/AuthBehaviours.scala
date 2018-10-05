package uk.gov.hmrc.agentinvitationsfrontend.controllers

import play.api.mvc.{Action, AnyContent, AnyContentAsEmpty}
import play.api.test.FakeRequest
import play.api.test.Helpers.redirectLocation
import uk.gov.hmrc.agentinvitationsfrontend.stubs.AuthStubs
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.auth.core.AuthorisationException

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
      val result = await(action(authenticatedClient(request, "",Enrolment("OtherEnrolment", "Key", "Value"))))
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

}
