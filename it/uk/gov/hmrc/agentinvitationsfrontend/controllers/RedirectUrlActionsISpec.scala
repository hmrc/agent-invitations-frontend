package uk.gov.hmrc.agentinvitationsfrontend.controllers
import play.api.test.FakeRequest
import play.api.mvc.Results.Ok
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.BadRequestException
import uk.gov.hmrc.play.bootstrap.binders.RedirectUrl

import scala.concurrent.Future

class RedirectUrlActionsISpec extends BaseISpec {

  val redirectUrlActions: RedirectUrlActions = app.injector.instanceOf[RedirectUrlActions]

  "RedirectUrlActions" should {

    "getRedirectUrl" should {
      "successfully extract a valid continue url" in {
        implicit val request = FakeRequest("GET", "/some/url?continue=/some/continue/url")

        val result: Option[RedirectUrl] = await(redirectUrlActions.getRedirectUrl)
        result shouldBe Some(RedirectUrl("/some/continue/url"))
      }
      "throw a bad request exception when the continue url is invalid" in {
        implicit val request = FakeRequest("GET", "/some/url?continue=foo")

        intercept[BadRequestException] {
          await(redirectUrlActions.getRedirectUrl)
        }
      }
      "return None when there is no continue url" in {
        implicit val request = FakeRequest("GET", "/some/url")

        val result: Option[RedirectUrl] = await(redirectUrlActions.getRedirectUrl)
        result shouldBe None
      }
    }

    "getErrorUrl" should {
      "successfully extract a valid error url" in {
        implicit val request =
          FakeRequest("GET", "/some/url?error=/some/error/url")

        val result: Option[RedirectUrl] = await(redirectUrlActions.getErrorUrl)
        result shouldBe Some(RedirectUrl("/some/error/url"))
      }
      "throw a BadRequest exception when the error url is invalid" in {
        implicit val request = FakeRequest("GET", "/some/url?error=foo")

        intercept[BadRequestException] {
          await(redirectUrlActions.getErrorUrl)
        }
      }
      "return None when there is no error url" in {
        implicit val request = FakeRequest("GET", "/some/url")

        val result: Option[RedirectUrl] = await(redirectUrlActions.getErrorUrl)
        result shouldBe None
      }
    }

    "maybeRedirectUrlOrBadRequest" should {
      "carry out the function block when the continue url has a whitelisted domain" in {
        withWhitelistedDomains
        implicit val request =
          FakeRequest("GET", "/some/url?continue=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fselect-client")

        val result =
          await(
            redirectUrlActions.maybeRedirectUrlOrBadRequest(
              Some(RedirectUrl("http://localhost:9996/tax-history/select-client")))(_ => Future(Ok("success"))))
        status(result) shouldBe 200
      }
      "carry out the function block when the continue url is relative" in {
        implicit val request =
          FakeRequest("GET", "/some/url?continue=%2Ffoo%2Fbar%2Fdah")

        val result =
          await(redirectUrlActions.maybeRedirectUrlOrBadRequest(Some(RedirectUrl("/foo/bar/dah")))(_ =>
            Future(Ok("success"))))
        status(result) shouldBe 200
      }
      "throw a Bad Request exception when the url domain is not whitelisted" in {
        withWhitelistedDomains
        implicit val request = FakeRequest("GET", "/some/url?continue=https://www.google.com")

        intercept[BadRequestException] {
          await(redirectUrlActions.maybeRedirectUrlOrBadRequest(Some(RedirectUrl("https://www.google.com")))(_ =>
            Future(Ok)))
        }
      }
      "carry out the function block when there is no continue url" in {
        implicit val request =
          FakeRequest("GET", "/some/url")

        val result =
          await(redirectUrlActions.maybeRedirectUrlOrBadRequest(None)(_ => Future(Ok("success"))))
        status(result) shouldBe 200
      }
    }
  }
}
