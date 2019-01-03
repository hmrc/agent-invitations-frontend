package uk.gov.hmrc.agentinvitationsfrontend.services
import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.models.{InactiveClient, ItsaTrackRelationship}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class TrackServiceISpec extends BaseISpec {

  val service: TrackService = app.injector.instanceOf[TrackService]

  implicit val headerCarrier = HeaderCarrier()

  override val arn = Arn("TARN0000001")
  override val mtdItId = MtdItId("ABCDE1234567890")
  val mtdItId2 = MtdItId("JKKL80894713304")

  val itsaRelationship1 =
    InactiveClient("HMRC-MTD-IT", "Boolean Ltd", validNino.value, "ni", Some(LocalDate.parse("2015-09-21")))
  val itsaRelationship2 =
    InactiveClient("HMRC-MTD-IT", "Boolean Ltd", validNino.value, "ni", Some(LocalDate.parse("2015-09-24")))
  val vatRelationship1 =
    InactiveClient("HMRC-MTD-VAT", "Gadgetron", validVrn.value, "vrn", Some(LocalDate.parse("2015-09-21")))
  val vatRelationship2 =
    InactiveClient("HMRC-MTD-VAT", "Gadgetron", validVrn9755.value, "vrn", Some(LocalDate.parse("2018-09-24")))
  val irvRelationship1 = InactiveClient(
    "PERSONAL-INCOME-RECORD",
    "Serena Williams",
    validNino.value,
    "ni",
    Some(LocalDate.parse("2015-09-21")))
  val irvRelationship2 =
    InactiveClient("PERSONAL-INCOME-RECORD", "Venus Williams", "GZ753451B", "ni", Some(LocalDate.parse("2018-09-24")))

  val inactiveRelationships =
    List(itsaRelationship1, itsaRelationship2, vatRelationship1, vatRelationship2, irvRelationship1, irvRelationship2)

  "getInactiveClients" should {
    "return list of relationships inactive for given agent" in {
      givenInactiveITSARelationships(arn)
      givenInactiveVATRelationships(arn)
      givenInactiveAfiRelationship(arn)
      givenNinoForMtdItId(mtdItId, validNino)
      givenNinoForMtdItId(mtdItId2, validNino)
      givenTradingName(validNino, "Boolean Ltd")
      givenCitizenDetailsAreKnownFor("AB123456A", "Serena", "Williams")
      givenCitizenDetailsAreKnownFor("GZ753451B", "Venus", "Williams")
      givenClientDetailsOnlyOrganisation(validVrn)
      givenClientDetailsOnlyOrganisation(validVrn9755)

      val result: Seq[InactiveClient] = await(service.getInactiveClients)

      result shouldBe inactiveRelationships
    }
  }

}
