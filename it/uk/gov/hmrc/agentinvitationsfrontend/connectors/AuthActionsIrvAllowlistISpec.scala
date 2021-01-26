package uk.gov.hmrc.agentinvitationsfrontend.connectors

import play.api.libs.json.Json
import play.api.test.{FakeRequest, Injecting}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AuthActionsImpl
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import play.api.mvc.Results._
import uk.gov.hmrc.http.HeaderCarrier
import play.api.test.Helpers._
import uk.gov.hmrc.agentmtdidentifiers.model.Arn

import scala.concurrent.{ExecutionContext, Future}

class AuthActionsIrvAllowlistISpec extends BaseISpec with Injecting {
  override def extraConfig = Map("features.enable-irv-allowlist" -> true)

  val authActions = inject[AuthActionsImpl]

  "withAuthorisedAsAgent" when {
    "irv-allowlist is enabled" should {
      "redirect to sign in when the user does not have an active session" in {
        givenUnauthorisedWith("MissingBearerToken")

        val result =
          authActions.withAuthorisedAsAgent(_ => Future.successful(NotImplemented))(FakeRequest(), HeaderCarrier(), ExecutionContext.global)
        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some("/gg/sign-in?continue=%2F&origin=agent-invitations-frontend")
      }

      "redirect to agent subscription when the user does not have a HMRC-AS-AGENT enrolment" in {
        givenUnauthorisedForInsufficientEnrolments()

        val result =
          authActions.withAuthorisedAsAgent(_ => Future.successful(NotImplemented))(FakeRequest(), HeaderCarrier(), ExecutionContext.global)
        status(result) shouldBe SEE_OTHER
        redirectLocation(result) shouldBe Some("someSubscriptionExternalUrl")
      }

      "return 403 Forbidden if the user is not signed in with Government Gateway" in {
        givenUnauthorisedWith("UnsupportedAuthProvider")

        val result =
          authActions.withAuthorisedAsAgent(_ => Future.successful(NotImplemented))(FakeRequest(), HeaderCarrier(), ExecutionContext.global)
        status(result) shouldBe FORBIDDEN
      }

      "invoke the callback body with the ARN and isAllowed equal to true when the user is on the IRV allowlist" in {
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

        givenArnIsAllowlistedForIrv(Arn("TARN0000001"))

        val result =
          authActions.withAuthorisedAsAgent { agent =>
            Future.successful(Ok((agent.arn.value, agent.isWhitelisted).toString))
          }(FakeRequest(), HeaderCarrier(), ExecutionContext.global)

        status(result) shouldBe OK
        // `bodyOf(result)` results in some kind of implicit conversion shenanigans
        bodyOf(await(result)) shouldBe "(TARN0000001,true)"
      }

      "invoke the callback body with the ARN and isAllowed equal to false when the user is not on the IRV allowlist" in {
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

        givenArnIsNotAllowlistedForIrv(Arn("TARN0000001"))

        val result =
          authActions.withAuthorisedAsAgent { agent =>
            Future.successful(Ok((agent.arn.value, agent.isWhitelisted).toString()))
          }(FakeRequest(), HeaderCarrier(), ExecutionContext.global)

        status(result) shouldBe OK
        // `bodyOf(result)` results in some kind of implicit conversion shenanigans
        bodyOf(await(result)) shouldBe "(TARN0000001,false)"
      }
    }
  }
}
