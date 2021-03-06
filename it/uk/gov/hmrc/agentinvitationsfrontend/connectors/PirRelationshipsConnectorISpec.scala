package uk.gov.hmrc.agentinvitationsfrontend.connectors

import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.stubs.ACRStubs
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class PirRelationshipsConnectorISpec extends BaseISpec with ACRStubs {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  val connector = app.injector.instanceOf[PirRelationshipConnector]

  "GetInactiveIrvRelationships" should {
    "return a sequence of inactive IRV relationships" in {
      givenInactiveAfiRelationship(arn)
      val result = await(connector.getInactiveIrvRelationships)
      result(0).service shouldBe "PERSONAL-INCOME-RECORD"
      result(0).arn shouldBe arn
      result(0).dateTo shouldBe Some(LocalDate.parse("2015-09-21"))
      result(0).clientId shouldBe "AB123456A"
      result(1).service shouldBe "PERSONAL-INCOME-RECORD"
      result(1).arn shouldBe arn
      result(1).dateTo shouldBe Some(LocalDate.parse("2018-09-24"))
      result(1).clientId shouldBe "GZ753451B"
    }

    "return an empty sequence if no inactive relationships are found" in {
      givenInactiveAfiRelationshipNotFound
      val result = await(connector.getInactiveIrvRelationships)
      result shouldBe Seq.empty
    }
  }

  "checkIrvAllowed" should {
    "return true when agent-fi-relationship returns 204 No Content" in {
      val arn = Arn("TARN0000001")
      givenArnIsAllowlistedForIrv(arn)
      await(connector.checkIrvAllowed(arn)) shouldBe true
    }

    "return false when agent-fi-relationship returns 404 Not Found" in {
      val arn = Arn("TARN0000001")
      givenArnIsNotAllowlistedForIrv(arn)
      await(connector.checkIrvAllowed(arn)) shouldBe false
    }
  }
}
