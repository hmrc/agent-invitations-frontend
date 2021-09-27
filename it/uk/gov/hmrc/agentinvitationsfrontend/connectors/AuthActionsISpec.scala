package uk.gov.hmrc.agentinvitationsfrontend.connectors

import play.api.mvc.Results._
import play.api.mvc.{AnyContentAsEmpty, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.test.Helpers
import play.api.{Configuration, Environment}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{AuthActionsImpl, FeatureFlags}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.http.{HeaderCarrier, SessionKeys}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AuthActionsISpec extends BaseISpec {

  val authConnector: AuthConnector = app.injector.instanceOf[AuthConnector]
  val config: Configuration = app.injector.instanceOf[Configuration]
  val env: Environment = app.injector.instanceOf[Environment]
  val externalUrls: ExternalUrls = app.injector.instanceOf[ExternalUrls]
  val featureFlags = app.injector.instanceOf[FeatureFlags]
  val pirRelationshipConnector = app.injector.instanceOf[PirRelationshipConnector]

  implicit val hc: HeaderCarrier = HeaderCarrier()
  implicit val request: FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("GET", "/path-of-request").withSession(SessionKeys.authToken -> "Bearer XYZ")

  object TestController
      extends AuthActionsImpl(externalUrls, env, config, authConnector, appConfig, featureFlags, pirRelationshipConnector) {
    def testWithAuthorisedAsAgent: Result =
      await(super.withAuthorisedAsAgent { agent =>
        Future.successful(Ok((agent.arn.value, agent.isWhitelisted).toString))
      })
  }

  "withAuthorisedAsAgent" should {

    "call body with arn and isWhitelisted flag when valid agent" in {
      givenAuthorisedFor(
        "{}",
        s"""{
           |"authorisedEnrolments": [
           |  { "key":"HMRC-AS-AGENT", "identifiers": [
           |    { "key":"AgentReferenceNumber", "value": "fooArn" }
           |  ]}
           |]}""".stripMargin
      )
      givenArnIsAllowlistedForIrv(Arn("fooArn"))
      val result = TestController.testWithAuthorisedAsAgent
      status(result) shouldBe 200
      bodyOf(result) shouldBe "(fooArn,true)"
    }

    "redirect to /subscription when agent not enrolled for service" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = Future.successful(TestController.testWithAuthorisedAsAgent)
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("someSubscriptionExternalUrl")
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
