package uk.gov.hmrc.agentinvitationsfrontend.connectors

import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.stubs.ACRStubs
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class RelationshipsConnectorISpec extends BaseISpec with ACRStubs {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  val connector = app.injector.instanceOf[RelationshipsConnector]

  "GetInactiveRelationships" should {
    "return a sequence of all inactive relationships for the agent" in {
      givenInactiveRelationships(arn)
      val result = await(connector.getInactiveRelationships)
      result(0).service shouldBe "HMRC-MTD-IT"
      result(0).arn shouldBe arn
      result(0).dateTo shouldBe Some(LocalDate.parse("2015-09-21"))
      result(0).clientId shouldBe "ABCDE1234567890"

      result(1).service shouldBe "HMRC-MTD-VAT"
      result(1).arn shouldBe arn
      result(1).dateTo shouldBe Some(LocalDate.parse("2015-09-24"))
      result(1).clientId shouldBe "101747641"

      result(2).service shouldBe "HMRC-TERS-ORG"
      result(2).arn shouldBe arn
      result(2).dateTo shouldBe Some(LocalDate.parse("2015-09-21"))
      result(2).clientId shouldBe "4937455253"

      result(3).service shouldBe "HMRC-CGT-PD"
      result(3).arn shouldBe arn
      result(3).dateTo shouldBe Some(LocalDate.parse("2015-09-21"))
      result(3).clientId shouldBe "XMCGTP123456789"

    }

    "return an empty sequence if no inactive relationships are found" in {
      givenInactiveRelationshipsNotFound
      val result = await(connector.getInactiveRelationships)
      result shouldBe Seq.empty
    }
  }
}
