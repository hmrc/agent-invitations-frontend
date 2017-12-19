package uk.gov.hmrc.agentinvitationsfrontend.controllers

import play.api.mvc.{Action, AnyContent}
import play.api.test.FakeRequest
import uk.gov.hmrc.agentinvitationsfrontend.controllers.personalincomerecord.PirClientRelationshipController
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.auth.core.InsufficientEnrolments
import uk.gov.hmrc.http.NotFoundException
import play.api.test.Helpers._

class PirClientRelationshipControllerISpec extends BaseISpec {

  lazy val controller: PirClientRelationshipController = app.injector.instanceOf[PirClientRelationshipController]
  val arn = Arn("TARN0000001")
  val clientId = "AA123456A"
  val afiService = "PERSONAL-INCOME-RECORD"

  "afiDeauthoriseAllStart" should {
    val afiDeauthoriseAllStart: Action[AnyContent] = controller.afiDeauthoriseAllStart()

    "receive Some active relationships for given clientId" in {
      getActiveAfiRelationship(arn, afiService, clientId, false)

      val result = await(afiDeauthoriseAllStart(authorisedAsValidClientAFI(FakeRequest(), clientId)))

      status(result) shouldBe 200
    }

    "receive None and redirect if none relationships found for given clientId" in {
      val result = await(afiDeauthoriseAllStart(authorisedAsValidClientAFI(FakeRequest(), clientId)))

      status(result) shouldBe 303
    }

    "verify Unauthorized if user is not logged in" in {
      an[InsufficientEnrolments] shouldBe thrownBy {
        await(afiDeauthoriseAllStart(authenticated(FakeRequest(), Enrolment("OtherEnrolment", "Key", "Value"))))
      }
      verifyAuthoriseAttempt()
    }
  }

  "submitAfiDeauthoriseAll" should {
    val submitAfiDeauthoriseAll: Action[AnyContent] = controller.submitAfiDeauthoriseAll()

    "Happy path, client terminates all relationships" in {
      terminateAfiRelationshipsForClientId(afiService, clientId)
      val result = await(submitAfiDeauthoriseAll(authorisedAsValidClientAFI(
        FakeRequest().withFormUrlEncodedBody("confirmResponse" -> "true"), clientId)))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("clientEndsRelationshipEnded.title"))
    }

    "redirect to clientCancelled if confirmResponse is false" in {
      val result = await(submitAfiDeauthoriseAll(authorisedAsValidClientAFI(
        FakeRequest().withFormUrlEncodedBody("confirmResponse" -> "false"), clientId)))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/relationships/individual/manage-your-tax-agents/deauthorise-agent/decline")
    }

    "return 200 with errors for failing radioButton submit" in {
      val result = await(submitAfiDeauthoriseAll(authorisedAsValidClientAFI(FakeRequest(), clientId)))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("clientEndsRelationship.title"))
    }

    "verify Unauthorized if user is not logged in" in {
      an[InsufficientEnrolments] shouldBe thrownBy {
        await(submitAfiDeauthoriseAll(authenticated(FakeRequest(), Enrolment("OtherEnrolment", "Key", "Value"))))
      }
      verifyAuthoriseAttempt()
    }

    "afiTerminateAllClientIdRelationships returned NotFoundException" in {
      failedTerminationAfiRelationshipsForClientId(afiService, clientId)
      intercept[NotFoundException] {
        await(submitAfiDeauthoriseAll(authorisedAsValidClientAFI(
          FakeRequest().withFormUrlEncodedBody("confirmResponse" -> "true"), clientId)))
      }
      verifyTerminateAfiRelationshipsAttempt(afiService, clientId)
    }
  }
}
