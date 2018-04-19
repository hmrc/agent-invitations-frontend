package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import play.api.mvc.{Action, AnyContent, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.personalincomerecord.{PirClientRelationshipController, routes => routesPir}
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
      redirectLocation(result).get shouldBe routesPir.PirClientRelationshipController.getClientEndsRelationshipNoAgentPage.url
    }

    "verify Unauthorized if user has insufficient enrolments" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = await(afiDeauthoriseAllStart(authenticatedClient(FakeRequest(), Enrolment("OtherEnrolment", "Key", "Value"))))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notAuthorised().url
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
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("generic.title", htmlEscapedMessage("clientEndsRelationshipEnded.header"), htmlEscapedMessage("title.suffix.client")))
      checkHasClientSignOutUrl(result)
      checkHtmlResultWithBodyText(result, personalTaxAccountUrl)
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
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("clientEndsRelationship.title", htmlEscapedMessage("clientEndsRelationship.header"), htmlEscapedMessage("title.suffix.client")))
      checkHasClientSignOutUrl(result)
    }

    "verify Unauthorized if user has insufficient enrolments" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = await(submitAfiDeauthoriseAll(authenticatedClient(FakeRequest(), Enrolment("OtherEnrolment", "Key", "Value"))))
      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notAuthorised().url
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

  "getClientDeclinedRelationshipTermination" should {
    "show the client cancelled deauth page with correct content" in {
      val result = await(controller.getClientDeclinedRelationshipTermination(authorisedAsValidClientAFI(FakeRequest(), clientId)))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("generic.title", htmlEscapedMessage("clientCancelled.header"), htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("clientCancelled.p1"))
      checkHtmlResultWithBodyText(result, personalTaxAccountUrl)
    }
  }

  "getClientEndsRelationshipNoAgentPage" should {
    "show the client ends relationship but no agent page with correct content" in {
      val result = await(controller.getClientEndsRelationshipNoAgentPage(authorisedAsValidClientAFI(FakeRequest(), clientId)))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("generic.title", htmlEscapedMessage("clientEndsRelationshipNoAgent.header"), htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("clientEndsRelationshipNoAgent.p1"))
      checkHtmlResultWithBodyText(result, personalTaxAccountUrl)
    }
  }

  def checkHasClientSignOutUrl(result: Future[Result]) = {
    checkHtmlResultWithBodyText(result, htmlEscapedMessage("common.sign-out"))
    val continueUrl = URLEncoder.encode(s"$businessTaxAccountUrl/business-account", StandardCharsets.UTF_8.name())
    checkHtmlResultWithBodyText(result, s"$companyAuthUrl$companyAuthSignOutPath?continue=$continueUrl")
  }
}
