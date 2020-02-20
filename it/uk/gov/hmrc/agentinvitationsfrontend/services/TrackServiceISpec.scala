package uk.gov.hmrc.agentinvitationsfrontend.services

import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.models.InactiveClient
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, MtdItId}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class TrackServiceISpec extends BaseISpec {

  val service: TrackService = app.injector.instanceOf[TrackService]

  implicit val headerCarrier = HeaderCarrier()

  override val arn = Arn("TARN0000001")
  override val mtdItId = MtdItId("ABCDE1234567890")
  val mtdItId2 = MtdItId("JKKL80894713304")

  val itsaRelationship = InactiveClient(
    Some("personal"),
    "HMRC-MTD-IT",
    validNino.value,
    "ni",
    Some(LocalDate.parse("2015-09-21")))

  val vatRelationship =
    InactiveClient(Some("personal"), "HMRC-MTD-VAT", "101747641", "vrn", Some(LocalDate.parse("2015-09-24")))

  val irvRelationship = InactiveClient(
    Some("personal"),
    "PERSONAL-INCOME-RECORD",
    validNino.value,
    "ni",
    Some(LocalDate.parse("2015-09-21")))

  val irvRelationship2 = InactiveClient(
    Some("personal"),
    "PERSONAL-INCOME-RECORD",
    "GZ753451B",
    "ni",
    Some(LocalDate.parse("2018-09-24")))

  val trustRelationship = InactiveClient(
    Some("personal"),
    "HMRC-TERS-ORG",
    validUtr.value,
    "utr",
    Some(LocalDate.parse("2015-09-21")))

  val cgtRelationship = InactiveClient(
    Some("personal"),
    "HMRC-CGT-PD",
    cgtRef.value,
    "CGTPDRef",
    Some(LocalDate.parse("2015-09-21")))

  val inactiveRelationships =
    List(itsaRelationship, vatRelationship, irvRelationship, irvRelationship2, trustRelationship, cgtRelationship)

  "getInactiveClients" should {
    "return list of relationships inactive for given agent" in {
      givenInactiveRelationships(arn)
      givenInactiveAfiRelationship(arn)
      givenNinoForMtdItId(mtdItId, validNino)
      givenNinoForMtdItId(mtdItId2, validNino)
      givenTradingName(validNino, "Boolean Ltd")
      givenCitizenDetailsAreKnownFor("AB123456A", "Serena", "Williams")
      givenCitizenDetailsAreKnownFor("GZ753451B", "Venus", "Williams")
      givenClientDetailsOnlyOrganisation(validVrn)
      givenClientDetailsOnlyOrganisation(validVrn9755)

      val result: Seq[InactiveClient] = await(service.getInactiveClients)
      result should contain theSameElementsAs(inactiveRelationships)
    }
  }

}
