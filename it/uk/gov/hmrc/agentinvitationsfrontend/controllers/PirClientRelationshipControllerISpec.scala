package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import play.api.mvc.{Action, AnyContent, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.personalincomerecord.PirClientRelationshipController
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.Arn

import scala.concurrent.Future

class PirClientRelationshipControllerISpec extends BaseISpec {

  lazy val controller: PirClientRelationshipController = app.injector.instanceOf[PirClientRelationshipController]
  val arn = Arn("TARN0000001")
  val clientId = "AA123456A"
  val afiService = "PERSONAL-INCOME-RECORD"

  "afiDeauthoriseAllStart" should {
    val afiDeauthoriseAllStart: Action[AnyContent] = controller.deauthoriseAllStart()

    "receive Some active relationships for given clientId" in {
      getActiveAfiRelationship(arn, afiService, clientId, false)

      val result = await(afiDeauthoriseAllStart(authorisedAsValidClientAFI(FakeRequest(), clientId)))

      status(result) shouldBe 200
    }

    "receive None and redirect if none relationships found for given clientId" in {
      val result = await(afiDeauthoriseAllStart(authorisedAsValidClientAFI(FakeRequest(), clientId)))

      status(result) shouldBe 303
    }

    "verify Unauthorized if user has insufficient enrolments" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = await(afiDeauthoriseAllStart(authenticatedClient(FakeRequest(), Enrolment("OtherEnrolment", "Key", "Value"))))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notSignedUp().url
      verifyAuthoriseAttempt()
    }

    "verify Unauthorized if user has insufficient confidence level" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = await(afiDeauthoriseAllStart(authenticatedClient(FakeRequest(), Enrolment("HMRC-NI", "NINO", clientId), "50")))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
      verifyAuthoriseAttempt()
    }
  }

  "submitAfiDeauthoriseAll" should {
    val submitAfiDeauthoriseAll: Action[AnyContent] = controller.submitDeauthoriseAll()

    "Happy path, client terminates all relationships" in {
      terminateAfiRelationshipsForClientId(afiService, clientId)
      val result = await(submitAfiDeauthoriseAll(authorisedAsValidClientAFI(
        FakeRequest().withFormUrlEncodedBody("confirmResponse" -> "true"), clientId)))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("clientEndsRelationshipEnded.title"))
      checkHasClientSignOutUrl(result)
    }

    "redirect to clientCancelled if confirmResponse is false" in {
      val result = await(submitAfiDeauthoriseAll(authorisedAsValidClientAFI(
        FakeRequest().withFormUrlEncodedBody("confirmResponse" -> "false"), clientId)))

      status(result) shouldBe 303
      redirectLocation(result).get shouldBe personalincomerecord.routes.PirClientRelationshipController.getClientDeclinedRelationshipTermination().url
    }

    "return 200 with errors for failing radioButton submit" in {
      val result = await(submitAfiDeauthoriseAll(authorisedAsValidClientAFI(FakeRequest(), clientId)))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("clientEndsRelationship.title"))
      checkHasClientSignOutUrl(result)
    }

    "verify Unauthorized if user has insufficient enrolments" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = await(submitAfiDeauthoriseAll(authenticatedClient(FakeRequest(), Enrolment("OtherEnrolment", "Key", "Value"))))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notSignedUp().url
      verifyAuthoriseAttempt()
    }

    "verify Unauthorized if user has insufficient confidence level" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = await(submitAfiDeauthoriseAll(authenticatedClient(FakeRequest(), Enrolment("HMRC-NI", "NINO", clientId), "50")))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
      verifyAuthoriseAttempt()
    }

    "failed to afiTerminateAllClientIdRelationships" in {
      failedTerminationAfiRelationshipsForClientId(afiService, clientId)
      val result = await(submitAfiDeauthoriseAll(authorisedAsValidClientAFI(
        FakeRequest().withFormUrlEncodedBody("confirmResponse" -> "true"), clientId)))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.terminate.500.title"))
    }
  }

  def checkHasClientSignOutUrl(result: Future[Result]) = {
    checkHtmlResultWithBodyText(result, htmlEscapedMessage("common.sign-out"))
    val continueUrl = URLEncoder.encode(s"$businessTaxAccountUrl/business-account", StandardCharsets.UTF_8.name())
    checkHtmlResultWithBodyText(result, s"$companyAuthUrl$companyAuthSignOutPath?continue=$continueUrl")
  }
}