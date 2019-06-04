package uk.gov.hmrc.agentinvitationsfrontend.controllers
import play.api.test.FakeRequest
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.play.bootstrap.binders.RedirectUrl

class RedirectUrlActionsISpec extends BaseISpec {

  val redirectUrlActions: RedirectUrlActions = app.injector.instanceOf[RedirectUrlActions]

  "RedirectUrlActions" should {
    "extractErrorUrl" should {
      "successfully extract a valid error url" in {
        implicit val request =
          FakeRequest("GET", "/some/url?error=/some/error/url")

        val result: Option[RedirectUrl] = await(redirectUrlActions.extractErrorUrl)
        result shouldBe Some(RedirectUrl("/some/error/url"))
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
        implicit val request = FakeRequest("GET", "/some/url?continue=/some/continue/url")

        val result: Option[RedirectUrl] = await(redirectUrlActions.extractRedirectUrl)
        result shouldBe Some(RedirectUrl("/some/continue/url"))
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
      "return None when the continue url is invalid" in {
        implicit val request = FakeRequest("GET", "/some/url?continue=foo")

        val result: Option[RedirectUrl] = await(redirectUrlActions.getRedirectUrl)
        result shouldBe None
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
      "return None when the error url is invalid" in {
        implicit val request = FakeRequest("GET", "/some/url?error=foo")

        val result: Option[RedirectUrl] = await(redirectUrlActions.getErrorUrl)
        result shouldBe None
      }
      "return None when there is no error url" in {
        implicit val request = FakeRequest("GET", "/some/url")

        val result: Option[RedirectUrl] = await(redirectUrlActions.getErrorUrl)
        result shouldBe None
      }
    }
  }
}
