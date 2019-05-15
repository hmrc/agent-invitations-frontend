package uk.gov.hmrc.agentinvitationsfrontend.connectors

import play.api.{Configuration, Environment}
import play.api.mvc.{AnyContent, AnyContentAsEmpty, Request, Result}
import play.api.mvc.Results._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{AuthActions, PasscodeVerification, routes}
import uk.gov.hmrc.agentinvitationsfrontend.support.{BaseISpec, CallOps}
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisationException, InsufficientEnrolments}
import uk.gov.hmrc.http.{HeaderCarrier, SessionKeys}

import scala.concurrent.Future

class AuthActionsISpec extends BaseISpec {

  object TestController extends AuthActions {

    override def authConnector: AuthConnector = app.injector.instanceOf[AuthConnector]
    override def config: Configuration = app.injector.instanceOf[Configuration]
    override def env: Environment = app.injector.instanceOf[Environment]
    override def withVerifiedPasscode: PasscodeVerification = app.injector.instanceOf[PasscodeVerification]

    implicit val hc = HeaderCarrier()
    implicit val request = FakeRequest("GET", "/path-of-request").withSession(SessionKeys.authToken -> "Bearer XYZ")

    import scala.concurrent.ExecutionContext.Implicits.global

    def testWithAuthorisedAsAgent: Result =
      await(super.withAuthorisedAsAgent { agent =>
        Future.successful(Ok((agent.arn.value, agent.isWhitelisted).toString))
      })

    def testWithEnrolledAsClient(serviceName: String, identifierKey: String): Result =
      await(super.withEnrolledAsClient(serviceName, identifierKey) { maybeClientId =>
        Future.successful(Ok(maybeClientId.getOrElse("")))
      })

    override def externalUrls: ExternalUrls =
      new ExternalUrls("", "", "", "", "", "", "", "", "", "", "fooSubscriptionUrl", "", "", "")
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

    "throw AuthorisationException when user not logged in" in {
      givenUnauthorisedWith("MissingBearerToken")
      an[AuthorisationException] shouldBe thrownBy {
        TestController.testWithAuthorisedAsAgent
      }
    }

    "throw InsufficientEnrolments when agent not enrolled for service" in {
      givenAuthorisedFor(
        "{}",
        s"""{
           |"authorisedEnrolments": [
           |  { "key":"HMRC-MTD-IT", "identifiers": [
           |    { "key":"MTDITID", "value": "fooMtdItId" }
           |  ]}
           |]}""".stripMargin
      )
      val result = await(TestController.testWithAuthorisedAsAgent)
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("fooSubscriptionUrl")
    }

    "throw InsufficientEnrolments when expected agent's identifier missing" in {
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
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("fooSubscriptionUrl")
    }
  }

  "withEnrolledAsClient" should {
    class IndividualSetup(
      val confidenceLevel: Int,
      val serviceName: String = "HMRC-MTD-IT",
      val identifierKey: String = "MTDITID",
      val identifierValue: String = "fooMtdItId") {
      givenAuthorisedFor(
        "{}",
        s"""{
           |"affinityGroup": "Individual",
           |"authorisedEnrolments": [
           |  {
           |    "key":"$serviceName",
           |    "identifiers": [ { "key":"$identifierKey", "value": "$identifierValue" } ]
           |  }
           |],
           |"confidenceLevel": $confidenceLevel
           |}""".stripMargin
      )
    }

    "call body" when {
      "an individual with confidence level at 200" in new IndividualSetup(confidenceLevel = 200) {
        val result = TestController.testWithEnrolledAsClient(serviceName, identifierKey)
        status(result) shouldBe 200
        bodyOf(result) shouldBe identifierValue
      }
      "an individual with confidence level greater than 200" in new IndividualSetup(confidenceLevel = 300) {
        val result = TestController.testWithEnrolledAsClient(serviceName, identifierKey)
        status(result) shouldBe 200
        bodyOf(result) shouldBe identifierValue
      }
    }

    "throw InsufficientConfidenceLevel if individual has a confidence level below 200" when {
      "a GET request was made, redirect to IV uplift journey" in new IndividualSetup(confidenceLevel = 100) {
        val result = TestController.testWithEnrolledAsClient(serviceName, identifierKey)

        status(result) shouldBe 303

        val expectedRedirectUrl = CallOps.addParamsToUrl(
          url = "/mdtp/uplift?origin=aif",
          "confidenceLevel" -> Some("200"),
          "completionURL"   -> Some(TestController.request.path),
          "failureURL"      -> Some(routes.ClientInvitationJourneyController.notAuthorised().url)
        )

        redirectLocation(result).get shouldBe expectedRedirectUrl
      }

      "a non-GET request was made, redirect directly to /not-authorised" in new IndividualSetup(confidenceLevel = 100) {
        val request = TestController.request.copyFakeRequest(method = "POST")
        val result = await(TestController.withEnrolledAsClient(serviceName, identifierKey) { maybeClientId =>
          Future.successful(Ok(maybeClientId.getOrElse("")))
        }(request, TestController.hc, scala.concurrent.ExecutionContext.Implicits.global))

        status(result) shouldBe 303
        redirectLocation(result) shouldBe Some(routes.ClientInvitationJourneyController.notAuthorised().url)
      }
    }
  }
}
