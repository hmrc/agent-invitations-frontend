package uk.gov.hmrc.agentinvitationsfrontend.services

import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.models.{InactiveClient, TrackInformationSorted}
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

  "allResults" should {
    "match an invitation that has relationshipIsEnded = true with an invalid relationship, discarding the inactive relationship" in {
      givenASingleInvitationWithRelationshipEnded("123456789", "HMRC-MTD-VAT", "vrn", LocalDate.now().minusDays(20))
      givenInactiveAfiRelationshipNotFound
      givenASingleInactiveRelationship("HMRC-MTD-VAT", "123456789", LocalDate.now().minusDays(20).toString, LocalDate.now().minusDays(4).toString)

      val result: Seq[TrackInformationSorted] = await(service.allResults(Arn("TARN0000001"), true, 30))

      result.size shouldBe 1

      result shouldBe Seq(TrackInformationSorted(
        clientType = Some("personal"),
        service = "HMRC-MTD-VAT",
        clientId = "123456789",
        clientIdType = "vrn",
        clientName = Some("Dave"),
        status = "AcceptedThenCancelledByAgent",
        dateTime = Some(LocalDate.now().minusDays(4)),
        expiryDate = None, invitationId = Some("foo1"), isRelationshipEnded = true, relationshipEndedBy = Some("Agent")
      ))
    }

    "match an inactive relationship with the corresponding invitation and the agent and client create a new relationship" in {
      givenTwoInvitationsExistForSameClientWithOneDeAuthorised("123456789", "HMRC-MTD-VAT", "vrn", accepted1 = LocalDate.now().minusDays(15), accepted2 = LocalDate.now().minusDays(4))
      givenInactiveAfiRelationshipNotFound
      givenASingleInactiveRelationship("HMRC-MTD-VAT", "123456789", LocalDate.now().minusDays(15).toString, LocalDate.now().minusDays(5).toString)

      val result: Seq[TrackInformationSorted] = await(service.allResults(Arn("TARN0000001"), true, 30))

      result.size shouldBe 2

      result shouldBe Seq(
        TrackInformationSorted(
        clientType = Some("personal"),
        service = "HMRC-MTD-VAT",
        clientId = "123456789",
        clientIdType = "vrn",
        clientName = Some("Dave"),
        status = "Accepted",
        dateTime = Some(LocalDate.now().minusDays(4)),
        expiryDate = None,
        invitationId = Some("foo2"),
        isRelationshipEnded = false,
        relationshipEndedBy = None
      ),
        TrackInformationSorted(
        clientType = Some("personal"),
        service = "HMRC-MTD-VAT",
        clientId = "123456789",
        clientIdType = "vrn",
        clientName = Some("Dave"),
        status = "AcceptedThenCancelledByAgent",
        dateTime = Some(LocalDate.now().minusDays(5)),
        expiryDate = None,
          invitationId = Some("foo1"),
          isRelationshipEnded = true,
          relationshipEndedBy = Some("Agent")
      ))

    }

    "match an invalid relationship with the corresponding invitation when the relationship was de-authorised as a " +
      "consequence of another authorisation with another agent" in {
      givenASingleInvitationWithRelationshipStillActive("123456789", "HMRC-MTD-VAT", "vrn", LocalDate.now().minusDays(10) )
      givenInactiveAfiRelationshipNotFound
      givenASingleInactiveRelationship("HMRC-MTD-VAT", "123456789", LocalDate.now().minusDays(10).toString, LocalDate.now().minusDays(3).toString)

      val result: Seq[TrackInformationSorted] = await(service.allResults(Arn("TARN0000001"), true, 30))

      result.size shouldBe 1

      result shouldBe Seq(
        TrackInformationSorted(
          clientType = Some("personal"),
          service = "HMRC-MTD-VAT",
          clientId = "123456789",
          clientIdType = "vrn",
          clientName = Some("Dave"),
          status = "AcceptedThenCancelledByClient",
          dateTime = Some(LocalDate.now().minusDays(3)),
          expiryDate = None,
          invitationId = Some("foo1"),
          isRelationshipEnded = true,
          relationshipEndedBy = Some("Client")
        )
      )

    }

    "match invitations that have relationshipIsEnded = true with invalid relationships, discarding the invalid relationships" in {

      givenInactiveRelationships() // 4 inactive relationships
      givenInactiveAfiRelationshipNotFound
      givenGetInvitations() // 9 invitations
      givenNinoForMtdItId(mtdItId, validNino)

      val result: Seq[TrackInformationSorted] = await(service.allResults(Arn("TARN0000001"),true, 30))

      result.size shouldBe 9
    }

  }

}
