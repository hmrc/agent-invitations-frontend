package uk.gov.hmrc.agentinvitationsfrontend.connectors

import play.api.libs.json.Json
import play.api.mvc.Request
import play.api.mvc.Results._
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Injecting}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AuthActions
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.{HeaderCarrier, SessionKeys}
import uk.gov.hmrc.play.http.HeaderCarrierConverter

import scala.concurrent.{ExecutionContext, Future}

class AuthActionsIrvAllowlistISpec extends BaseISpec with Injecting {

  val authActions = inject[AuthActions]

  implicit def hc(implicit request: Request[_]): HeaderCarrier =  HeaderCarrierConverter.fromRequestAndSession(request, request.session)

  "withAuthorisedAsAgent" should {

      "redirect to sign in when the user does not have an active session" in {
        givenUnauthorisedWith("MissingBearerToken")

        implicit val request: Request[_] = FakeRequest()
        val result =
          authActions.withAuthorisedAsAgent(_ => Future.successful(NotImplemented))(request, hc, ExecutionContext.global)
        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some("http://localhost:9553/bas-gateway/sign-in?origin=agent-invitations-frontend&continue_url=http://localhost:9448/")
      }

      "redirect to agent subscription when the user does not have a HMRC-AS-AGENT enrolment" in {
        givenUnauthorisedForInsufficientEnrolments()

        implicit val request: Request[_] = FakeRequest().withSession(
          SessionKeys.authToken -> "Bearer XYZ")

        val result =
          authActions.withAuthorisedAsAgent(_ => Future.successful(NotImplemented))(request, hc, ExecutionContext.global)
        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some("someSubscriptionExternalUrl")
      }

      "return 403 Forbidden if the user is not signed in with Government Gateway" in {
        givenUnauthorisedWith("UnsupportedAuthProvider")

        implicit val request: Request[_] = FakeRequest().withSession(
          SessionKeys.authToken -> "Bearer XYZ")

        val result =
          authActions.withAuthorisedAsAgent(_ => Future.successful(NotImplemented))(request, hc, ExecutionContext.global)
        status(result) shouldBe FORBIDDEN
      }

      "invoke the callback body with the ARN" in {
        val authorise = Json.arr(
          Json.obj("enrolment"     -> "HMRC-AS-AGENT", "identifiers" -> Json.arr(), "state" -> "Activated"),
          Json.obj("authProviders" -> Json.arr("GovernmentGateway"))
        )
        val retrieved = Json.obj(
          "authorisedEnrolments" -> Json.arr(
            Json.obj(
              "key" -> "HMRC-AS-AGENT",
              "identifiers" -> Json.arr(
                Json.obj("key" -> "AgentReferenceNumber", "value" -> "TARN0000001")
              )
            )
          ))
        givenAuthorisedFor(
          Json.obj("authorise" -> authorise, "retrieve" -> Json.arr("authorisedEnrolments")).toString(),
          retrieved.toString())
        givenNotSuspended()

        implicit val request: Request[_] = FakeRequest().withSession(
          SessionKeys.authToken -> "Bearer XYZ")

        val result =
          authActions.withAuthorisedAsAgent { agent =>
            Future.successful(Ok(agent.arn.value))
          }(request, hc, ExecutionContext.global)

        status(result) shouldBe OK
        // `bodyOf(result)` results in some kind of implicit conversion shenanigans
        bodyOf(await(result)) shouldBe "TARN0000001"
      }

  }
}
