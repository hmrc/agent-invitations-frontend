package uk.gov.hmrc.agentinvitationsfrontend.controllers
import play.api.test.FakeRequest
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import play.api.test.Helpers._


class ClientErrorControllerISpec extends BaseISpec {

  lazy val controller: ClientErrorController = app.injector.instanceOf[ClientErrorController]

  "GET /wrong-account-type" should {

    "show wrong-account-type page for personal / individual" in {
      val result = controller.incorrectClientType(authorisedAsAnyIndividualClient(FakeRequest().withSession("clientType" -> "personal")))
      status(result) shouldBe 403
      checkHtmlResultWithBodyText(
        result,
        "There is a problem",
        "You signed in with the wrong type of Government Gateway account.",
        "To use this service, you need to sign in with the Government Gateway account you use for your personal tax affairs.",
        "You can create an account if you do not have one.",
        "Sign out and try again",
        companyAuthUrl+companyAuthSignOutPath
      )
    }

    "show wrong-account-type page for business / organisation" in {
      val result = controller.incorrectClientType(authorisedAsAnyIndividualClient(FakeRequest().withSession("clientType" -> "business")))
      status(result) shouldBe 403
      checkHtmlResultWithBodyText(
        result,
        "There is a problem",
        "You signed in with the wrong type of Government Gateway account.",
        "To use this service, you need to sign in with the Government Gateway account you use for your business tax affairs.",
        "You can create an account if you do not have one.",
        "Sign out and try again",
        companyAuthUrl+companyAuthSignOutPath

      )
    }

    "redirect to not-authorised page if client-type not found in session" in {
      val result = controller.incorrectClientType(authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.notAuthorised().url)
    }
  }

}
