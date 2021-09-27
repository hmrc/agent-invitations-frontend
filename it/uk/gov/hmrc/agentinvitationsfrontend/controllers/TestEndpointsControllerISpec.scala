package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.util.UUID

import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.testing.TestEndpointsController
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.SessionId

class TestEndpointsControllerISpec extends BaseISpec {

  lazy val controller: TestEndpointsController = app.injector.instanceOf[TestEndpointsController]
  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId(UUID.randomUUID().toString)))

  val clientId = "AA123456A"
  val afiService = "PERSONAL-INCOME-RECORD"
  val relationshipForm = Seq(
    "arn"      -> arn.value,
    "service"  -> afiService,
    "clientId" -> clientId
  )

  "getDeleteRelationship" should {
    val request = FakeRequest("GET", "/test-only/relationships/delete")
    val getDeleteRelationship = controller.getDeleteRelationship()
    "show delete_relationship page" in {
      val result = await(getDeleteRelationship(authorisedAsValidAgent(request, arn.value)))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Test Only: Delete a relationship")
    }
  }

  "submitDeleteRelationship" should {
    val request = FakeRequest("POST", "/test-only/relationships/delete")
    val submitDeleteRelationship = controller.submitDeleteRelationship()
    "delete an existing relationship" in {
      givenTestOnlyTerminateAfiRelationshipSucceeds(arn, afiService, clientId)

      val result = submitDeleteRelationship(authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(relationshipForm: _*))

      status(result) shouldBe 303
      redirectLocation(result).get shouldBe testing.routes.TestEndpointsController.getDeleteRelationship().url
    }

    "show not matched page as an error page if unable to delete an existing relationship" in {
      givenTerminateAfiRelationshipFails(arn, afiService, clientId)

      val result = submitDeleteRelationship(authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(relationshipForm: _*))

      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentInvitationJourneyController.showNotMatched().url
    }

    "return a Bad Request and reload the page if invalid form data" in {
      val result = await(submitDeleteRelationship(authorisedAsValidAgent(request, arn.value)))

      status(result) shouldBe 400
      checkHtmlResultWithBodyText(result, "Test Only: Delete a relationship")
    }
  }

  "getCreateRelationship" should {
    val request = FakeRequest("POST", "/test-only/relationships/create")
    val getCreateRelationship = controller.getCreateRelationship()
    "show create_relationship page" in {
      val result = await(getCreateRelationship(authorisedAsValidAgent(request, arn.value)))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Test Only: Create a relationship")
    }
  }

  "submitCreateRelationship" should {
    val request = FakeRequest("POST", "/test-only/relationships/create")
    val submitCreateRelationship = controller.submitCreateRelationship()
    "delete an existing relationship" in {
      givenTestOnlyCreateAfiRelationshipSucceeds(arn, afiService, clientId)

      val result = submitCreateRelationship(authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(relationshipForm: _*))

      status(result) shouldBe 303
      redirectLocation(result).get shouldBe testing.routes.TestEndpointsController.getCreateRelationship().url
    }

    "show not matched page as an error page if unable to delete an existing relationship" in {
      givenTestOnlyCreateAfiRelationshipFails(arn, afiService, clientId)

      val result = submitCreateRelationship(authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(relationshipForm: _*))

      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentInvitationJourneyController.showNotMatched().url
    }

    "return a Bad Request and reload the page if invalid form data" in {
      val result = await(submitCreateRelationship(authorisedAsValidAgent(request, arn.value)))

      status(result) shouldBe 400
      checkHtmlResultWithBodyText(result, "Test Only: Create a relationship")
    }
  }

  "getFastTrackForm" should {
    "show create fast-track page" in {
      val request = FakeRequest("GET", "/test-only/fast-track")
      val getFastTrackForm = controller.getFastTrackForm()
      val result = await(getFastTrackForm(authorisedAsValidAgent(request, arn.value)))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Test Only: Fast Track Invitation")
    }
  }
}
