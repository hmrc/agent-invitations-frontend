package uk.gov.hmrc.agentinvitationsfrontend.services
import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.models.{InactiveClient, ItsaTrackRelationship}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class RelationshipServiceISpec extends BaseISpec {

  val service: TrackService = app.injector.instanceOf[TrackService]

  implicit val headerCarrier = HeaderCarrier()

  val arn = Arn("TARN0000001")
  val mtdItId = MtdItId("ABCDE1234567890")
  val mtdItId2 = MtdItId("JKKL80894713304")
  private val validNino = Nino("AB123456A")
  private val validNinoSpace = Nino("AB 12 34 56 A")
  val serviceITSA = "HMRC-MTD-IT"
  val servicePIR = "PERSONAL-INCOME-RECORD"
  val validPostcode = "DH14EJ"
  val invitationIdITSA = InvitationId("ABERULMHCKKW3")
  val invitationIdPIR = InvitationId("B9SCS2T4NZBAX")

  val invitationIdVAT = InvitationId("CZTW1KY6RTAAT")
  val serviceVAT = "HMRC-MTD-VAT"
  val identifierVAT = "VRN"
  val validVrn = Vrn("101747696")
  val invalidVrn = Vrn("101747692")
  val validRegistrationDate = "2007-07-07"
  val validVrn9755 = Vrn("101747641")

  val itsaRelationship1 = InactiveClient("HMRC-MTD-IT", "Boolean Ltd", Some(LocalDate.parse("2015-09-21")))
  val itsaRelationship2 = InactiveClient("HMRC-MTD-IT", "Boolean Ltd", Some(LocalDate.parse("2018-09-24")))
  val vatRelationship1 = InactiveClient("HMRC-MTD-VAT", "Gadgetron", Some(LocalDate.parse("2015-09-21")))
  val vatRelationship2 = InactiveClient("HMRC-MTD-VAT", "Gadgetron", Some(LocalDate.parse("2018-09-24")))
  val irvRelationship1 = InactiveClient("PERSONAL-INCOME-RECORD", "Serena Williams", Some(LocalDate.parse("2015-09-21")))
  val irvRelationship2 = InactiveClient("PERSONAL-INCOME-RECORD", "Venus Williams", Some(LocalDate.parse("2018-09-24")))

  val inactiveRelationships = List(itsaRelationship1, itsaRelationship2, vatRelationship1, vatRelationship2, irvRelationship1, irvRelationship2)

  "getInactiveClients" should {
    "return list of relationships inactive for given agent" in {
      givenInactiveITSARelationships(arn)
      givenInactiveVATRelationships(arn)
      givenInactiveRelationshipsIrv(arn)
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
