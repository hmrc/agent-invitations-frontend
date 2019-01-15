package uk.gov.hmrc.agentinvitationsfrontend.connectors

import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.stubs.ACRStubs
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class RelationshipsConnectorISpec extends BaseISpec with ACRStubs {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  val connector = app.injector.instanceOf[RelationshipsConnector]

  "GetInactiveItsaRelationships" should {
    "return a sequence of inactive ITSA relationships" in {
      givenInactiveITSARelationships(arn)
      val result = await(connector.getInactiveItsaRelationships)
      result(0).serviceName shouldBe "HMRC-MTD-IT"
      result(0).arn shouldBe arn
      result(0).dateTo shouldBe Some(LocalDate.parse("2015-09-21"))
      result(0).clientId shouldBe "ABCDE1234567890"
      result(1).serviceName shouldBe "HMRC-MTD-IT"
      result(1).arn shouldBe arn
      result(1).dateTo shouldBe Some(LocalDate.parse("2015-09-24"))
      result(1).clientId shouldBe "JKKL80894713304"
    }

    "return an empty sequence if no inactive relationships are found" in {
      givenInactiveITSARelationshipsNotFound
      val result = await(connector.getInactiveItsaRelationships)
      result shouldBe Seq.empty
    }
  }

  "GetInactiveVatRelationships" should {
    "return a sequence of inactive VAT relationships" in {
      givenInactiveVATRelationships(arn)
      val result = await(connector.getInactiveVatRelationships)
      result(0).serviceName shouldBe "HMRC-MTD-VAT"
      result(0).arn shouldBe arn
      result(0).dateTo shouldBe Some(LocalDate.parse("2015-09-21"))
      result(0).clientId shouldBe "101747696"
      result(1).serviceName shouldBe "HMRC-MTD-VAT"
      result(1).arn shouldBe arn
      result(1).dateTo shouldBe Some(LocalDate.parse("2018-09-24"))
      result(1).clientId shouldBe "101747641"
    }
  }

  "return an empty sequence if no inactive relationships are found" in {
    givenInactiveVATRelationshipsNotFound
    val result = await(connector.getInactiveVatRelationships)
    result shouldBe Seq.empty
  }

}
