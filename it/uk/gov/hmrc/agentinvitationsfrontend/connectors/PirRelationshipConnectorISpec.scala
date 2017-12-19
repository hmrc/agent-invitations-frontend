package uk.gov.hmrc.agentinvitationsfrontend.connectors

import uk.gov.hmrc.agentinvitationsfrontend.models.PirRelationship
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.http.{HeaderCarrier, NotFoundException}
import scala.concurrent.ExecutionContext.Implicits.global

class PirRelationshipConnectorISpec extends BaseISpec {

  implicit val hc = HeaderCarrier()
  val connector = app.injector.instanceOf[PirRelationshipConnector]
  val arn = Arn("TARN0000001")
  val clientId = "AA123456A"
  val afiService = "PERSONAL-INCOME-RECORD"

  "Get client relationships" should {
    "return existing ACTIVE relationships for specified clientId" in {
      getActiveAfiRelationship(arn, afiService, clientId, false)

      val result = await(connector.getAfiClientRelationships(afiService, clientId))

      result.isDefined shouldBe true
      result.get.isInstanceOf[List[PirRelationship]] shouldBe true
      result.get.head.clientId shouldBe clientId
    }
    "return NotFound Exception when ACTIVE relationship not found" in {
      getNotFoundForAfiRelationship(afiService, clientId)

      val result = await(connector.getAfiClientRelationships(afiService, clientId))

      result.isDefined shouldBe false
      result shouldBe None
    }
  }

  "afiTerminateAllClientIdRelationships" should {
    "return 200, terminating all active relationships for given clientId" in {
      terminateAfiRelationshipsForClientId(afiService, clientId)

      val result = await(connector.afiTerminateAllClientIdRelationships(afiService, clientId))

      result shouldBe 200
    }
    "return 404 when failing to find/terminate relationship " in {
      failedTerminationAfiRelationshipsForClientId(afiService, clientId)

      intercept[NotFoundException] {
        await(connector.afiTerminateAllClientIdRelationships(afiService, clientId))
      }
      verifyTerminateAfiRelationshipsAttempt(afiService, clientId)
    }
  }
}