package uk.gov.hmrc.agentinvitationsfrontend.connectors

import play.api.mvc.Results._
import play.api.mvc.{AnyContentAsEmpty, Request, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.{Configuration, Environment}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{AuthActions, FeatureFlags}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.http.{HeaderCarrier, SessionKeys}
import uk.gov.hmrc.play.http.HeaderCarrierConverter

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AuthActionsISpec extends BaseISpec {

  val authConnector: AuthConnector = app.injector.instanceOf[AuthConnector]
  val acaConnector: AgentClientAuthorisationConnector = app.injector.instanceOf[AgentClientAuthorisationConnector]
  val config: Configuration = app.injector.instanceOf[Configuration]
  val env: Environment = app.injector.instanceOf[Environment]
  val externalUrls: ExternalUrls = app.injector.instanceOf[ExternalUrls]
  val featureFlags = app.injector.instanceOf[FeatureFlags]

  implicit val request: FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("GET", "/path-of-request").withSession(SessionKeys.authToken -> "Bearer XYZ")
  implicit def hc(implicit request: Request[_]): HeaderCarrier = HeaderCarrierConverter.fromRequestAndSession(request, request.session)

  object TestController
      extends AuthActions(externalUrls, env, config, authConnector, acaConnector, appConfig, featureFlags) {
    def testWithAuthorisedAsAgent: Result =
      await(super.withAuthorisedAsAgent { agent =>
        Future.successful(Ok(agent.arn.value))
      })
  }

  "withAuthorisedAsAgent" should {

    "call body with arn when valid agent" in {
      givenAuthorisedFor(
        "{}",
        s"""{
           |"authorisedEnrolments": [
           |  { "key":"HMRC-AS-AGENT", "identifiers": [
           |    { "key":"AgentReferenceNumber", "value": "fooArn" }
           |  ]}
           |]}""".stripMargin
      )
      givenNotSuspended()

      val result = TestController.testWithAuthorisedAsAgent
      status(result) shouldBe 200
      bodyOf(result) shouldBe "fooArn"
    }

    "redirect to /subscription when agent not enrolled for service" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = Future.successful(TestController.testWithAuthorisedAsAgent)
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("someSubscriptionExternalUrl")
    }

    "redirect to 'account limited' when agent is authenticated but suspended" in {
      givenAuthorisedFor(
      "{}",
      s"""{
         |"authorisedEnrolments": [
         |  { "key":"HMRC-AS-AGENT", "identifiers": [
         |    { "key":"AgentReferenceNumber", "value": "fooArn" }
         |  ]}
         |]}""".stripMargin
      )
      givenSuspended()

      val result = TestController.testWithAuthorisedAsAgent
      status(result) shouldBe 303
      val redirectUrl = redirectLocation(Future.successful(result)).get
      redirectUrl should include ("account-limited")
    }

    "throw Forbidden when expected agent's identifier missing" in {
      givenAuthorisedFor(
        "{}",
        s"""{
           |"authorisedEnrolments": [
           |  { "key":"HMRC-AS-AGENT", "identifiers": [
           |    { "key":"BAR", "value": "fooArn" }
           |  ]}
           |]}""".stripMargin
      )
      val result = TestController.testWithAuthorisedAsAgent
      status(result) shouldBe 403
    }
  }
}
