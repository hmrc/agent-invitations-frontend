package uk.gov.hmrc.agentinvitationsfrontend.controllers

import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.testing.TestEndpointsController
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.Arn

class TestEndpointsControllerISpec extends BaseISpec {

  lazy val controller: TestEndpointsController = app.injector.instanceOf[TestEndpointsController]
  val arn = Arn("TARN0000001")
  val clientId = "AA123456A"
  val afiService = "PERSONAL-INCOME-RECORD"
  val relationshipForm = Seq(
    "arn" -> arn.value,
    "service" -> afiService,
    "clientId" -> clientId
  )

  "getDeleteRelationship" should {
    "show delete_relationship page" in {
      val result = await(controller.getDeleteRelationship().apply(FakeRequest()))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Test Only: Delete a relationship")
    }
  }

  "submitDeleteRelationship" should {
    "delete an existing relationship" in {
      deleteRelationship(arn, afiService, clientId)

      val result = await(controller.submitDeleteRelationship().apply(FakeRequest()
        .withFormUrlEncodedBody(relationshipForm: _*)))

      status(result) shouldBe 303
      redirectLocation(result).get shouldBe testing.routes.TestEndpointsController.getDeleteRelationship().url
    }

    "show not matched page as an error page if unable to delete an existing relationship" in {
      deleteRelationshipFailed(arn, afiService, clientId)

      val result = await(controller.submitDeleteRelationship().apply(FakeRequest()
        .withFormUrlEncodedBody(relationshipForm: _*)))

      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.notMatched().url
    }

    "return a Bad Request and reload the page if invalid form data" in {
      val result = await(controller.submitDeleteRelationship().apply(FakeRequest()))

      status(result) shouldBe 400
      checkHtmlResultWithBodyText(result, "Test Only: Delete a relationship")
    }
  }

  "getCreateRelationship" should {
    "show create_relationship page" in {
      val result = await(controller.getCreateRelationship().apply(FakeRequest()))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Test Only: Create a relationship")
    }
  }

  "submitCreateRelationship" should {
    "delete an existing relationship" in {
      createRelationship(arn, afiService, clientId)

      val result = await(controller.submitCreateRelationship().apply(FakeRequest()
        .withFormUrlEncodedBody(relationshipForm: _*)))

      status(result) shouldBe 303
      redirectLocation(result).get shouldBe testing.routes.TestEndpointsController.getCreateRelationship().url
    }

    "show not matched page as an error page if unable to delete an existing relationship" in {
      createRelationshipFailed(arn, afiService, clientId)

      val result = await(controller.submitCreateRelationship().apply(FakeRequest()
        .withFormUrlEncodedBody(relationshipForm: _*)))

      status(result) shouldBe 303
      redirectLocation(result).get shouldBe routes.AgentsInvitationController.notMatched().url
    }

    "return a Bad Request and reload the page if invalid form data" in {
      val result = await(controller.submitCreateRelationship().apply(FakeRequest()))

      status(result) shouldBe 400
      checkHtmlResultWithBodyText(result, "Test Only: Create a relationship")
    }
  }
}