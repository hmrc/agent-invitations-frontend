package uk.gov.hmrc.agentinvitationsfrontend.connectors

import java.time.LocalDate
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.stubs.ACRStubs
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.Service
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class PirRelationshipsConnectorISpec extends BaseISpec with ACRStubs {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  val connector = app.injector.instanceOf[PirRelationshipConnector]

  "GetInactiveIrvRelationships" should {
    "return a sequence of inactive IRV relationships" in {
      givenInactiveAfiRelationship(arn)
      val result = await(connector.getInactiveIrvRelationships)
      result(0).service shouldBe Service.PersonalIncomeRecord
      result(0).arn shouldBe arn
      result(0).dateTo shouldBe Some(LocalDate.parse("2015-09-21"))
      result(0).clientId shouldBe "AB123456A"
      result(1).service shouldBe Service.PersonalIncomeRecord
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
}
