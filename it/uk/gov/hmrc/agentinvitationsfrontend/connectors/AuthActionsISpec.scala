package uk.gov.hmrc.agentinvitationsfrontend.connectors

import play.api.mvc.Result
import play.api.mvc.Results._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{AuthActions, PasscodeVerification, routes}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisationException, InsufficientEnrolments}
import uk.gov.hmrc.http.{HeaderCarrier, SessionKeys}

import scala.concurrent.Future

class AuthActionsISpec extends BaseISpec {

  object TestController extends AuthActions {

    override def authConnector: AuthConnector = app.injector.instanceOf[AuthConnector]

    def withVerifiedPasscode: PasscodeVerification = app.injector.instanceOf[PasscodeVerification]

    implicit val hc = HeaderCarrier()
    implicit val request = FakeRequest().withSession(SessionKeys.authToken -> "Bearer XYZ")

    import scala.concurrent.ExecutionContext.Implicits.global

    def withAuthorisedAsAgent[A]: Result = {
      await(super.withAuthorisedAsAgent { arn => Future.successful(Ok(arn.value)) })
    }

    def withAuthorisedAsClient[A](serviceName: String, identifierKey: String): Result = {
      await(super.withAuthorisedAsClient(serviceName, identifierKey) { clientId => Future.successful(Ok(clientId)) })
    }
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
           |]}""".stripMargin)
      val result = TestController.withAuthorisedAsAgent
      status(result) shouldBe 200
      bodyOf(result) shouldBe "fooArn"
    }

    "throw AutorisationException when user not logged in" in {
      givenUnauthorisedWith("MissingBearerToken")
      an[AuthorisationException] shouldBe thrownBy {
        TestController.withAuthorisedAsAgent
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
           |]}""".stripMargin)
      an[InsufficientEnrolments] shouldBe thrownBy {
        TestController.withAuthorisedAsAgent
      }
    }

    "throw InsufficientEnrolments when expected agent's identifier missing" in {
      givenAuthorisedFor(
        "{}",
        s"""{
           |"authorisedEnrolments": [
           |  { "key":"HMRC-AS-AGENT", "identifiers": [
           |    { "key":"BAR", "value": "fooArn" }
           |  ]}
           |]}""".stripMargin)
      an[InsufficientEnrolments] shouldBe thrownBy {
        TestController.withAuthorisedAsAgent
      }
    }
  }

  "withAuthorisedAsClient" should {

    "call body with mtditid when valid mtd client" in {
      givenAuthorisedFor(
        "{}",
        s"""{
           |"authorisedEnrolments": [
           |  { "key":"HMRC-MTD-IT", "identifiers": [
           |    { "key":"MTDITID", "value": "fooMtdItId" }
           |  ]}
           |]}""".stripMargin)

      val result = TestController.withAuthorisedAsClient("HMRC-MTD-IT", "MTDITID")
      status(result) shouldBe 200
      bodyOf(result) shouldBe "fooMtdItId"
    }

    "call body with nino when valid afi client" in {
      givenAuthorisedFor(
        "{}",
        s"""{
           |"authorisedEnrolments": [
           |  { "key":"HMRC-NI", "identifiers": [
           |    { "key":"NINO", "value": "fooNINO" }
           |  ]}
           |]}""".stripMargin)

      val result = TestController.withAuthorisedAsClient("HMRC-NI", "NINO")
      status(result) shouldBe 200
      bodyOf(result) shouldBe "fooNINO"
    }

    "throw InsufficientEnrolments and redirect to not-sign-up page when client not enrolled for service" in {
      givenAuthorisedFor(
        "{}",
        s"""{
           |"authorisedEnrolments": [
           |  { "key":"HMRC-AS-AGENT", "identifiers": [
           |    { "key":"AgentReferenceNumber", "value": "fooArn" }
           |  ]}
           |]}""".stripMargin)
      val result = TestController.withAuthorisedAsClient("HMRC-MTD-IT", "MTDITID")
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notSignedUp().url
    }

    "throw InsufficientEnrolments and redirect to not-sign-up page when client not enrolled for service for AFI" in {
      givenAuthorisedFor(
        "{}",
        s"""{
           |"authorisedEnrolments": [
           |  { "key":"HMRC-AS-AGENT", "identifiers": [
           |    { "key":"AgentReferenceNumber", "value": "fooArn" }
           |  ]}
           |]}""".stripMargin)
      val result = TestController.withAuthorisedAsClient("HMRC-NI", "NINO")
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notSignedUp().url
    }

    "throw InsufficientEnrolments and redirect to not-sign-up page when expected client's identifier missing" in {
      givenAuthorisedFor(
        "{}",
        s"""{
           |"authorisedEnrolments": [
           |  { "key":"HMRC-MTD-IT", "identifiers": [
           |    { "key":"BAR", "value": "fooMtdItId" }
           |  ]}
           |]}""".stripMargin)
      val result = TestController.withAuthorisedAsClient("HMRC-MTD-IT", "MTDITID")
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notSignedUp().url
    }

    "throw InsufficientEnrolments and redirect to not-sign-up page when expected client's identifier missing for AFI" in {
      givenAuthorisedFor(
        "{}",
        s"""{
           |"authorisedEnrolments": [
           |  { "key":"HMRC-NI", "identifiers": [
           |    { "key":"BAR", "value": "fooMtdItId" }
           |  ]}
           |]}""".stripMargin)
      val result = TestController.withAuthorisedAsClient("HMRC-NI", "NINO")
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notSignedUp().url
    }

    "throw InsufficientConfidenceLevel and redirect to not-found page when expected client's is less than 200" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = TestController.withAuthorisedAsClient("HMRC-NI", "NINO")
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }
  }
}


