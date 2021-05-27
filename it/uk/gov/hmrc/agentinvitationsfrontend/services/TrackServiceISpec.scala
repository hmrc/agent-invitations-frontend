package uk.gov.hmrc.agentinvitationsfrontend.services

import org.joda.time.{DateTime, DateTimeZone, LocalDate}
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

  def nowMinus(days: Int) = DateTime.now(DateTimeZone.UTC).minusDays(days).withTimeAtStartOfDay()

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

  private def dummyTrackInformationSorted(clientId: String, status: String, isRelationshipEnded: Boolean, daysInPast: Int) = TrackInformationSorted(
    clientType = Some("personal"),
    service = "HMRC-MTD-VAT",
    clientId = clientId,
    clientIdType = "vrn",
    clientName = Some("Dave"),
    status = status,
    dateTime = Some(DateTime.now().minusDays(daysInPast).withTimeAtStartOfDay()),
    expiryDate = None, invitationId = Some("foo1"), isRelationshipEnded = isRelationshipEnded, relationshipEndedBy = Some("Agent")
  )

  "matchAndDiscard" should {
    "correctly match accepted with invalid and deauthorised" in {
      service.matchAndDiscard(Nil) shouldBe Nil

      val justAccepted = Seq(dummyTrackInformationSorted("123456789", "Accepted", false, 2))
      service.matchAndDiscard(justAccepted) shouldBe justAccepted

      val acceptedAndInvalid = Seq(
        dummyTrackInformationSorted("123456789", "Accepted", false, 2),
        dummyTrackInformationSorted("123456789", "InvalidRelationship", true, 1))
      service.matchAndDiscard(acceptedAndInvalid) shouldBe Seq(dummyTrackInformationSorted("123456789", "Accepted", true, 1))

      val acceptedAndInvalidAndAccepted = Seq(
        dummyTrackInformationSorted("123456789", "Accepted", false, 3),
        dummyTrackInformationSorted("123456789", "InvalidRelationship", true, 2),
        dummyTrackInformationSorted("123456789", "Accepted", false, 1))
      service.matchAndDiscard(acceptedAndInvalidAndAccepted) shouldBe Seq(
        dummyTrackInformationSorted("123456789", "Accepted", false, 1),
        dummyTrackInformationSorted("123456789", "Accepted", true, 2)
      )

      val acceptedAndDeauthed = Seq(
        dummyTrackInformationSorted("123456789", "Deauthorised", true, 1),
        dummyTrackInformationSorted("123456789", "InvalidRelationship", true, 1))
      service.matchAndDiscard(acceptedAndDeauthed) shouldBe Seq(
        dummyTrackInformationSorted("123456789", "Deauthorised", true, 1),
      )

      val acceptedAndDeauthedAndAccepted = Seq(
        dummyTrackInformationSorted("123456789", "Deauthorised", true, 2),
        dummyTrackInformationSorted("123456789", "InvalidRelationship", true, 2),
        dummyTrackInformationSorted("123456789", "Accepted", false, 1))
      service.matchAndDiscard(acceptedAndDeauthedAndAccepted) shouldBe Seq(
        dummyTrackInformationSorted("123456789", "Accepted", false, 1),
        dummyTrackInformationSorted("123456789", "Deauthorised", true, 2),
      )
    }
  }

  "allResults" should {
    "match an invitation that has relationshipIsEnded = true with an invalid relationship, discarding the inactive relationship" in {
      givenASingleInvitationWithRelationshipEnded("123456789", "HMRC-MTD-VAT", "vrn", DateTime.now().minusDays(20).withTimeAtStartOfDay())
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
        dateTime = Some(DateTime.now().minusDays(4).withTimeAtStartOfDay()),
        expiryDate = None, invitationId = Some("foo1"), isRelationshipEnded = true, relationshipEndedBy = Some("Agent")
      ))
    }

    "match an inactive relationship with the corresponding invitation and the agent and client create a new relationship" in {

      givenTwoInvitationsExistForSameClientWithOneDeAuthorised("123456789", "HMRC-MTD-VAT", "vrn", accepted1 = nowMinus(15), accepted2 = nowMinus(4))
      givenInactiveAfiRelationshipNotFound
      givenASingleInactiveRelationship("HMRC-MTD-VAT", "123456789", LocalDate.now().minusDays(15).toString, LocalDate.now().minusDays(5).toString)

      val result: Seq[TrackInformationSorted] = await(service.allResults(Arn("TARN0000001"), true, 30))

      result.size shouldBe 2

      result.map(s => (s.dateTime.map(_.toLocalDate.toString), s.status)) shouldBe Seq(
        (Some(nowMinus(4).toLocalDate.toString), "Accepted"),
        (Some(nowMinus(5).toLocalDate.toString), "AcceptedThenCancelledByAgent")
      )
    }

    "match a deauthorised relationship with the corresponding invitation and the agent and client create a new relationship" in {

      givenTwoInvitationsExistForSameClientOneWithDeauthedStatus("123456789", "HMRC-MTD-VAT", "vrn", accepted = nowMinus(15), deauthed = nowMinus(4))
      givenInactiveAfiRelationshipNotFound
      givenASingleInactiveRelationship("HMRC-MTD-VAT", "123456789", LocalDate.now().minusDays(15).toString, LocalDate.now().minusDays(5).toString)

      val result: Seq[TrackInformationSorted] = await(service.allResults(Arn("TARN0000001"), true, 30))

      result.size shouldBe 2

      result.map(s => (s.dateTime.map(_.toLocalDate.toString), s.status)) shouldBe Seq(
        (Some(nowMinus(4).toLocalDate.toString), "Accepted"),
        (Some(nowMinus(5).toLocalDate.toString), "AcceptedThenCancelledByAgent")
      )
    }

    "match an invalid relationship with the corresponding invitation when the relationship was de-authorised as a " +
      "consequence of another authorisation with another agent" in {
      givenASingleInvitationWithRelationshipStillActive("123456789", "HMRC-MTD-VAT", "vrn", DateTime.now().minusDays(10))
      givenInactiveAfiRelationshipNotFound
      givenASingleInactiveRelationship("HMRC-MTD-VAT", "123456789", LocalDate.now().minusDays(10).toString, LocalDate.now().minusDays(3).toString)

      val result: Seq[TrackInformationSorted] = await(service.allResults(Arn("TARN0000001"), true, 30))

      result.size shouldBe 1

      result.map(s => (s.dateTime.map(_.toLocalDate.toString), s.status)) shouldBe Seq(
        (Some(nowMinus(3).toLocalDate.toString), "AcceptedThenCancelledByAgent")
      )
    }

    "match invitations that have relationshipIsEnded = true with invalid relationships, discarding the invalid relationships" in {

      givenInactiveRelationships() // 4 inactive relationships
      givenInactiveAfiRelationshipNotFound
      givenGetInvitations() // 9 invitations
      givenNinoForMtdItId(mtdItId, validNino)

      val result: Seq[TrackInformationSorted] = await(service.allResults(Arn("TARN0000001"),true, 30))

      result.size shouldBe 9

      result.map(s => (s.clientId, s.service, s.status, s.dateTime.map(_.toLocalDate.toString))) shouldBe Seq(

        ("AB123456A", "HMRC-MTD-IT", "AcceptedThenCancelledByAgent", Some(nowMinus(0).toLocalDate.toString)),
        ("101747641", "HMRC-MTD-VAT", "AcceptedThenCancelledByClient", Some(nowMinus(1).toLocalDate.toString)),
        ("4937455253", "HMRC-TERS-ORG", "AcceptedThenCancelledByAgent", Some(nowMinus(3).toLocalDate.toString)),
        ("XMCGTP123456789", "HMRC-CGT-PD", "AcceptedThenCancelledByClient", Some(nowMinus(5).toLocalDate.toString)),
        ("AB123256B", "HMRC-MTD-IT", "Accepted", Some(nowMinus(25).toLocalDate.toString)),
        ("AB123456A", "HMRC-MTD-IT", "Cancelled", Some(nowMinus(30).toLocalDate.toString)),
        ("AB127456A", "HMRC-MTD-IT", "Pending", None),
        ("101747696", "HMRC-MTD-VAT", "Pending", None),
        ("AB129456B", "HMRC-MTD-IT", "Pending", None)
      )

    }

  }

}
