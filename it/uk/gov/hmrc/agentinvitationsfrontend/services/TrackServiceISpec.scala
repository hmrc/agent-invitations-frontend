package uk.gov.hmrc.agentinvitationsfrontend.services

import java.time.{LocalDateTime, LocalDate}
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.models.{InactiveClient, TrackInformationSorted}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, MtdItId, Service}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import java.time.{Instant, LocalDate, LocalDateTime, ZoneOffset}
import scala.concurrent.ExecutionContext.Implicits.global

class TrackServiceISpec extends BaseISpec {

  val service: TrackService = app.injector.instanceOf[TrackService]

  implicit val headerCarrier = HeaderCarrier()

  override val arn = Arn("TARN0000001")
  override val mtdItId = MtdItId("ABCDE1234567890")
  val mtdItId2 = MtdItId("JKKL80894713304")

  def nowMinus(days: Int) = Instant.now.atZone(ZoneOffset.UTC).toLocalDate.minusDays(days).atStartOfDay()

  val itsaRelationship = InactiveClient(
    Some("personal"),
    Some(Service.MtdIt),
    validNino.value,
    "ni",
    Some(LocalDate.parse("2015-09-21")))

  val vatRelationship =
    InactiveClient(Some("personal"), Some(Service.Vat), "101747641", "vrn", Some(LocalDate.parse("2015-09-24")))

  val irvRelationship = InactiveClient(
    Some("personal"),
    Some(Service.PersonalIncomeRecord),
    validNino.value,
    "ni",
    Some(LocalDate.parse("2015-09-21")))

  val irvRelationship2 = InactiveClient(
    Some("personal"),
    Some(Service.PersonalIncomeRecord),
    "GZ753451B",
    "ni",
    Some(LocalDate.parse("2018-09-24")))

  val trustRelationship = InactiveClient(
    Some("personal"),
    Some(Service.Trust),
    validUtr.value,
    "utr",
    Some(LocalDate.parse("2015-09-21")))

  val cgtRelationship = InactiveClient(
    Some("personal"),
    Some(Service.CapitalGains),
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
      givenCitizenDetailsAreKnownFor(Nino("AB123456A"), "Serena", "Williams")
      givenCitizenDetailsAreKnownFor(Nino("GZ753451B"), "Venus", "Williams")
      givenClientDetailsOnlyOrganisation(validVrn)
      givenClientDetailsOnlyOrganisation(validVrn9755)

      val result: Seq[InactiveClient] = await(service.getInactiveClients)
      result should contain theSameElementsAs(inactiveRelationships)
    }
  }

  private def dummyTrackInformationSorted(clientId: String, status: String, isRelationshipEnded: Boolean, daysInPast: Int) = TrackInformationSorted(
    clientType = Some("personal"),
    service = Some(Service.Vat),
    clientId = clientId,
    clientIdType = "vrn",
    clientName = Some("Dave"),
    status = status,
    dateTime = Some(Instant.now.atZone(ZoneOffset.UTC).toLocalDate.minusDays(daysInPast).atStartOfDay()),
    expiryDate = None, invitationId = Some("foo1"), isRelationshipEnded = isRelationshipEnded, relationshipEndedBy = Some("Agent"),
    lastUpdated = None
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
      val lastUpdated = Instant.now().atZone(ZoneOffset.UTC).toLocalDate.minusDays(20).atStartOfDay()
      givenASingleInvitationWithRelationshipEnded("123456789", Service.Vat, "vrn", lastUpdated)
      givenInactiveAfiRelationshipNotFound
      givenASingleInactiveRelationship(Service.Vat, "123456789", LocalDate.now().minusDays(20).toString, LocalDate.now().minusDays(4).toString)

      val result: Seq[TrackInformationSorted] = await(service.allResults(Arn("TARN0000001"), 30))

      result.size shouldBe 1

      result shouldBe Seq(TrackInformationSorted(
        clientType = Some("personal"),
        service = Some(Service.Vat),
        clientId = "123456789",
        clientIdType = "vrn",
        clientName = Some("Dave"),
        status = "AcceptedThenCancelledByAgent",
        dateTime = Some(Instant.now().atZone(ZoneOffset.UTC).toLocalDate.minusDays(4).atStartOfDay()),
        expiryDate = None, invitationId = Some("foo1"), isRelationshipEnded = true, relationshipEndedBy = Some("Agent"),
        lastUpdated = Some(LocalDateTime.parse(lastUpdated.toString))
      ))
    }

    "match an inactive relationship with the corresponding invitation and the agent and client create a new relationship" in {

      givenTwoInvitationsExistForSameClientWithOneDeAuthorised("123456789", Service.Vat, "vrn", accepted1 = nowMinus(15), accepted2 = nowMinus(4))
      givenInactiveAfiRelationshipNotFound
      givenASingleInactiveRelationship(Service.Vat, "123456789", LocalDate.now().minusDays(15).toString, LocalDate.now().minusDays(5).toString)

      val result: Seq[TrackInformationSorted] = await(service.allResults(Arn("TARN0000001"), 30))

      result.size shouldBe 2

      result.map(s => (s.dateTime.map(_.toLocalDate.toString), s.status)) shouldBe Seq(
        (Some(nowMinus(4).toLocalDate.toString), "Accepted"),
        (Some(nowMinus(5).toLocalDate.toString), "AcceptedThenCancelledByAgent")
      )
    }

    "match a deauthorised relationship with the corresponding invitation and the agent and client create a new relationship" in {

      givenTwoInvitationsExistForSameClientOneWithDeauthedStatus("123456789", Service.Vat, "vrn", accepted = nowMinus(15), deauthed = nowMinus(4))
      givenInactiveAfiRelationshipNotFound
      givenASingleInactiveRelationship(Service.Vat, "123456789", LocalDate.now().minusDays(15).toString, LocalDate.now().minusDays(5).toString)

      val result: Seq[TrackInformationSorted] = await(service.allResults(Arn("TARN0000001"), 30))

      result.size shouldBe 2

      result.map(s => (s.dateTime.map(_.toLocalDate.toString), s.status)) shouldBe Seq(
        (Some(nowMinus(4).toLocalDate.toString), "Accepted"),
        (Some(nowMinus(5).toLocalDate.toString), "AcceptedThenCancelledByAgent")
      )
    }

    "match an invalid relationship with the corresponding invitation when the relationship was de-authorised as a " +
      "consequence of another authorisation with another agent" in {
      givenASingleInvitationWithRelationshipStillActive("123456789", Service.Vat, "vrn", Instant.now().atZone(ZoneOffset.UTC).toLocalDate.minusDays(10).atStartOfDay())
      givenInactiveAfiRelationshipNotFound
      givenASingleInactiveRelationship(Service.Vat, "123456789", LocalDate.now().minusDays(10).toString, LocalDate.now().minusDays(3).toString)

      val result: Seq[TrackInformationSorted] = await(service.allResults(Arn("TARN0000001"), 30))

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

      val result: Seq[TrackInformationSorted] = await(service.allResults(Arn("TARN0000001"),30))

      result.size shouldBe 10

      result.map(s => (s.clientId, s.service, s.status, s.dateTime.map(_.toLocalDate.toString))) shouldBe Seq(

        ("AB123456A", Some(Service.MtdIt), "AcceptedThenCancelledByAgent", Some(nowMinus(0).toLocalDate.toString)),
        ("101747641", Some(Service.Vat), "AcceptedThenCancelledByClient", Some(nowMinus(1).toLocalDate.toString)),
        ("4937455253", Some(Service.Trust), "AcceptedThenCancelledByAgent", Some(nowMinus(3).toLocalDate.toString)),
        ("XMCGTP123456789", Some(Service.CapitalGains), "AcceptedThenCancelledByClient", Some(nowMinus(5).toLocalDate.toString)),
        ("AB123256B", Some(Service.MtdIt), "Accepted", Some(nowMinus(25).toLocalDate.toString)),
        ("AB123456A", Some(Service.MtdIt), "Cancelled", Some(nowMinus(30).toLocalDate.toString)),
        ("AB123456C", Some(Service.MtdIt), "Partialauth", Some(nowMinus(30).toLocalDate.toString)),
        ("AB127456A", Some(Service.MtdIt), "Pending", None),
        ("101747696", Some(Service.Vat), "Pending", None),
        ("AB129456B", Some(Service.MtdIt), "Pending", None)
      )

    }

  }

}
