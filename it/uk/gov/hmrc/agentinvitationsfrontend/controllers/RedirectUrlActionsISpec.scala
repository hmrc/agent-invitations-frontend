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
    "extractErrorUrl" should {
      "successfully extract a valid error url" in {
        implicit val request =
          FakeRequest("GET", "/some/url?error=http%3A%2F%2Flocalhost%3A9999%2Fsome%2Furl")

        val result: Option[RedirectUrl] = await(redirectUrlActions.extractErrorUrl)
        result shouldBe Some(RedirectUrl("http://localhost:9999/some/url"))
      }
      "return None when the error url is invalid" in {
        implicit val request = FakeRequest("GET", "/some/url?error=foo")

        val result: Option[RedirectUrl] = await(redirectUrlActions.extractErrorUrl)
        result shouldBe None
      }
      "return None when there is no error url" in {
        implicit val request = FakeRequest("GET", "/some/url")

        val result: Option[RedirectUrl] = await(redirectUrlActions.extractErrorUrl)
        result shouldBe None
      }
    }

    "extractRedirectUrl" should {
      "successfully extract a valid continue url" in {
        implicit val request = FakeRequest("GET", "http:/some/url?continue=http%3A%2F%2Flocalhost%3A9999%2Fsome%2Furl")

        val result: Option[RedirectUrl] = await(redirectUrlActions.extractRedirectUrl)
        result shouldBe Some(RedirectUrl("http://localhost:9999/some/url"))
      }
      "return None when the continue url is invalid" in {
        implicit val request = FakeRequest("GET", "/some/url?continue=foo")

        val result: Option[RedirectUrl] = await(redirectUrlActions.extractRedirectUrl)
        result shouldBe None
      }
      "return None when there is no continue url" in {
        implicit val request = FakeRequest("GET", "/some/url")

        val result: Option[RedirectUrl] = await(redirectUrlActions.extractRedirectUrl)
        result shouldBe None
      }
    }

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
        implicit val request =
          FakeRequest("GET", "/some/url?continue=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fselect-client")

        val result =
          await(
            redirectUrlActions.maybeRedirectUrlOrBadRequest(
              Some(RedirectUrl("http://localhost:9996/tax-history/select-client")))(_ => Future(Ok("success"))))
        status(result) shouldBe 200
      }
      "throw a Bad Request exception when the url domain is not whitelisted" in {
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
