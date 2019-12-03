package uk.gov.hmrc.agentinvitationsfrontend.connectors

import play.api.mvc.Results._
import play.api.mvc.{AnyContentAsEmpty, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.{Configuration, Environment}
import uk.gov.hmrc.agentinvitationsfrontend.config.{AppConfig, ExternalUrls}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{AuthActions, PasscodeVerification}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.http.{HeaderCarrier, SessionKeys}

import scala.concurrent.Future

class AuthActionsISpec extends BaseISpec {

  object TestController extends AuthActions {

    override def authConnector: AuthConnector = app.injector.instanceOf[AuthConnector]
    override def config: Configuration = app.injector.instanceOf[Configuration]
    override def env: Environment = app.injector.instanceOf[Environment]
    override def withVerifiedPasscode: PasscodeVerification = app.injector.instanceOf[PasscodeVerification]

    implicit override val appConfig: AppConfig = app.injector.instanceOf[AppConfig]

    implicit val hc: HeaderCarrier = HeaderCarrier()
    implicit val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/path-of-request").withSession(SessionKeys.authToken -> "Bearer XYZ")

    import scala.concurrent.ExecutionContext.Implicits.global

    def testWithAuthorisedAsAgent: Result =
      await(super.withAuthorisedAsAgent { agent =>
        Future.successful(Ok((agent.arn.value, agent.isWhitelisted).toString))
      })

    override def externalUrls: ExternalUrls = new ExternalUrls()
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
      val result = TestController.testWithAuthorisedAsAgent
      status(result) shouldBe 200
      bodyOf(result) shouldBe "(fooArn,true)"
    }

    "redirect to /subscription when agent not enrolled for service" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = await(TestController.testWithAuthorisedAsAgent)
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("fooSubscriptionUrl")
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
      val result = await(TestController.testWithAuthorisedAsAgent)
      status(result) shouldBe 403
    }
  }
}
