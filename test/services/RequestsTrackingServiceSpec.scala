/*
 * Copyright 2019 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package services

import java.net.URL

import org.joda.time.{DateTime, LocalDate}
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito.{mock, when}
import uk.gov.hmrc.agentinvitationsfrontend.connectors._
import uk.gov.hmrc.agentinvitationsfrontend.models.{CustomerDetails, Individual, StoredInvitation, TrackedInvitation}
import uk.gov.hmrc.agentinvitationsfrontend.services.TrackService
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class RequestsTrackingServiceSpec extends UnitSpec {

  val invitationsConnector = mock(classOf[InvitationsConnector])
  val agentServicesAccountConnector = mock(classOf[AgentServicesAccountConnector])
  val citizenDetailsConnector = mock(classOf[CitizenDetailsConnector])
  val relationshipsConnector = mock(classOf[RelationshipsConnector])
  val pirRelationshipConnector = mock(classOf[PirRelationshipConnector])
  val tested = new TrackService(
    invitationsConnector,
    relationshipsConnector,
    pirRelationshipConnector,
    agentServicesAccountConnector,
    citizenDetailsConnector)

  val vrn = Vrn("101747696")
  val nino = Nino("AB123456A")
  val dateTime = DateTime.now.minusDays(10)

  implicit val now: LocalDate = LocalDate.now
  implicit val hc: HeaderCarrier = HeaderCarrier()

  "RequestsTrackingService" when {
    "resolving client name for VAT service" should {
      "return trading name first if available" in {
        when(
          agentServicesAccountConnector
            .getCustomerDetails(any(classOf[Vrn]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(
            Future.successful(
              CustomerDetails(
                Some("Aaa"),
                Some(Individual(Some("Bb1"), Some("Bb2"), Some("Bb3"), Some("Bb4"))),
                Some("Ccc"))))
        await(tested.getVatName(vrn)) shouldBe Some("Ccc")
      }

      "return business name if trading name not available" in {
        when(
          agentServicesAccountConnector
            .getCustomerDetails(any(classOf[Vrn]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(Future.successful(
            CustomerDetails(Some("Aaa"), Some(Individual(Some("Bb1"), Some("Bb2"), Some("Bb3"), Some("Bb4"))), None)))
        await(tested.getVatName(vrn)) shouldBe Some("Aaa")
      }

      "return full individual name if others not available" in {
        when(
          agentServicesAccountConnector
            .getCustomerDetails(any(classOf[Vrn]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(Future.successful(
            CustomerDetails(None, Some(Individual(Some("Bb1"), Some("Bb2"), Some("Bb3"), Some("Bb4"))), None)))
        await(tested.getVatName(vrn)) shouldBe Some("Bb1 Bb2 Bb3 Bb4")
      }

      "return partial individual name if others not available" in {
        when(
          agentServicesAccountConnector
            .getCustomerDetails(any(classOf[Vrn]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(
            Future.successful(CustomerDetails(None, Some(Individual(None, Some("Bb2"), None, Some("Bb4"))), None)))
        await(tested.getVatName(vrn)) shouldBe Some("Bb2 Bb4")
      }

      "return none if none available" in {
        when(
          agentServicesAccountConnector
            .getCustomerDetails(any(classOf[Vrn]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(Future.successful(CustomerDetails(None, None, None)))
        await(tested.getVatName(vrn)) shouldBe None
      }
    }

    "resolving client name for ITSA service" should {
      "return trading name if available" in {
        when(
          agentServicesAccountConnector
            .getTradingName(any(classOf[Nino]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(Future.successful(Some("Aaa")))
        await(tested.getItsaTradingName(nino)) shouldBe Some("Aaa")
      }

      "return individual name if trading name not available" in {
        when(
          citizenDetailsConnector
            .getCitizenDetails(any(classOf[Nino]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(Future.successful(Citizen(Some("String"), Some("Pearson"), Some(nino.value))))
        when(
          agentServicesAccountConnector
            .getTradingName(any(classOf[Nino]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(Future.successful(None))
        await(tested.getItsaTradingName(nino)) shouldBe Some("String Pearson")
      }

      "return none trading name not available" in {
        when(
          citizenDetailsConnector
            .getCitizenDetails(any(classOf[Nino]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(Future.successful(Citizen(None, None, None)))
        when(
          agentServicesAccountConnector
            .getTradingName(any(classOf[Nino]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(Future.successful(None))
        await(tested.getItsaTradingName(nino)) shouldBe None
      }
    }

    "resolving client name for PIR service" should {
      "return full citizen name if available" in {
        when(
          citizenDetailsConnector
            .getCitizenDetails(any(classOf[Nino]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(Future.successful(Citizen(Some("Aa1"), Some("Aa2"))))
        await(tested.getCitizenName(nino)) shouldBe Some("Aa1 Aa2")
      }

      "return partial citizen name if available" in {
        when(
          citizenDetailsConnector
            .getCitizenDetails(any(classOf[Nino]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(Future.successful(Citizen(Some("Aa1"), None)))
        await(tested.getCitizenName(nino)) shouldBe Some("Aa1")
      }

      "return none if citizen name not available" in {
        when(
          citizenDetailsConnector
            .getCitizenDetails(any(classOf[Nino]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(Future.successful(Citizen(None, None)))
        await(tested.getCitizenName(nino)) shouldBe None
      }
    }

    "resolving client name of an invitation" should {
      "return proper name according to the service name" in {
        when(
          agentServicesAccountConnector
            .getTradingName(any(classOf[Nino]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(Future.successful(Some("Aaa Itsa Trader")))
        when(
          agentServicesAccountConnector
            .getCustomerDetails(any(classOf[Vrn]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(
            Future.successful(
              CustomerDetails(
                Some("Aaa Vat Trader"),
                Some(Individual(Some("Bb1"), Some("Bb2"), Some("Bb3"), Some("Bb4"))),
                Some("Aaa Ltd."))))
        when(
          citizenDetailsConnector
            .getCitizenDetails(any(classOf[Nino]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(Future.successful(Citizen(Some("Aa1"), Some("Aa2"))))

        await(
          tested.getClientNameByService(
            TrackedInvitation(
              Some("personal"),
              "HMRC-MTD-IT",
              nino.value,
              "ni",
              None,
              "Pending",
              dateTime,
              now,
              "foo"))) shouldBe Some("Aaa Itsa Trader")

        await(
          tested.getClientNameByService(
            TrackedInvitation(
              Some("personal"),
              "HMRC-MTD-VAT",
              vrn.value,
              "vrn",
              None,
              "Accepted",
              dateTime,
              now,
              "foo"))) shouldBe Some("Aaa Ltd.")

        await(
          tested
            .getClientNameByService(
              TrackedInvitation(
                Some("personal"),
                "PERSONAL-INCOME-RECORD",
                nino.value,
                "ni",
                None,
                "Expired",
                dateTime,
                now,
                "foo"))) shouldBe Some("Aa1 Aa2")
      }
    }

    "adding the client name to the invitation" should {
      "return updated invitation if name available" in {
        when(
          agentServicesAccountConnector
            .getTradingName(any(classOf[Nino]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(Future.successful(Some("Aaa Itsa Trader")))
        when(
          agentServicesAccountConnector
            .getCustomerDetails(any(classOf[Vrn]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(Future.successful(
            CustomerDetails(None, Some(Individual(Some("A"), Some("B"), Some("C"), Some("D"))), Some("Aaa Ltd."))))
        when(
          citizenDetailsConnector
            .getCitizenDetails(any(classOf[Nino]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(Future.successful(Citizen(Some("Foo"), Some("Bar"))))

        await(
          tested.addClientName(
            TrackedInvitation(
              Some("personal"),
              "HMRC-MTD-IT",
              nino.value,
              "ni",
              None,
              "Pending",
              dateTime,
              now,
              "foo"))) shouldBe
          TrackedInvitation(
            Some("personal"),
            "HMRC-MTD-IT",
            nino.value,
            "ni",
            Some("Aaa Itsa Trader"),
            "Pending",
            dateTime,
            now,
            "foo")

        await(
          tested.addClientName(
            TrackedInvitation(
              Some("personal"),
              "HMRC-MTD-VAT",
              vrn.value,
              "vrn",
              None,
              "Accepted",
              dateTime,
              now,
              "foo"))) shouldBe
          TrackedInvitation(
            Some("personal"),
            "HMRC-MTD-VAT",
            vrn.value,
            "vrn",
            Some("Aaa Ltd."),
            "Accepted",
            dateTime,
            now,
            "foo")

        await(
          tested.addClientName(
            TrackedInvitation(
              Some("personal"),
              "PERSONAL-INCOME-RECORD",
              nino.value,
              "ni",
              None,
              "Rejected",
              dateTime,
              now,
              "foo"))) shouldBe
          TrackedInvitation(
            Some("personal"),
            "PERSONAL-INCOME-RECORD",
            nino.value,
            "ni",
            Some("Foo Bar"),
            "Rejected",
            dateTime,
            now,
            "foo")
      }

      "return unchanged invitation if name not available" in {
        when(
          agentServicesAccountConnector
            .getTradingName(any(classOf[Nino]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(Future.successful(None))
        when(
          agentServicesAccountConnector
            .getCustomerDetails(any(classOf[Vrn]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(Future.successful(CustomerDetails(None, None, None)))
        when(
          citizenDetailsConnector
            .getCitizenDetails(any(classOf[Nino]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(Future.successful(Citizen(None, None)))

        await(
          tested.addClientName(
            TrackedInvitation(
              Some("personal"),
              "HMRC-MTD-IT",
              nino.value,
              "ni",
              None,
              "Pending",
              dateTime,
              now,
              "foo"))) shouldBe
          TrackedInvitation(Some("personal"), "HMRC-MTD-IT", nino.value, "ni", None, "Pending", dateTime, now, "foo")

        await(
          tested.addClientName(
            TrackedInvitation(
              Some("personal"),
              "HMRC-MTD-VAT",
              vrn.value,
              "vrn",
              None,
              "Accepted",
              dateTime,
              now,
              "foo"))) shouldBe
          TrackedInvitation(Some("personal"), "HMRC-MTD-VAT", vrn.value, "vrn", None, "Accepted", dateTime, now, "foo")

        await(
          tested.addClientName(
            TrackedInvitation(
              Some("personal"),
              "PERSONAL-INCOME-RECORD",
              nino.value,
              "ni",
              None,
              "Rejected",
              dateTime,
              now,
              "foo"))) shouldBe
          TrackedInvitation(
            Some("personal"),
            "PERSONAL-INCOME-RECORD",
            nino.value,
            "ni",
            None,
            "Rejected",
            dateTime,
            now,
            "foo")
      }
    }

    "filter out PIR service if user not whitelisted" in {
      tested.whitelistedInvitation(false)(invitationForService("PERSONAL-INCOME-RECORD")) shouldBe false
      tested.whitelistedInvitation(false)(invitationForService("HMRC-MTD-IT")) shouldBe true
      tested.whitelistedInvitation(false)(invitationForService("HMRC-MTD-VAT")) shouldBe true
      tested.whitelistedInvitation(true)(invitationForService("PERSONAL-INCOME-RECORD")) shouldBe true
      tested.whitelistedInvitation(true)(invitationForService("HMRC-MTD-IT")) shouldBe true
      tested.whitelistedInvitation(true)(invitationForService("HMRC-MTD-VAT")) shouldBe true
    }

    "return empty tracked invitations when none supplied" in {
      when(
        invitationsConnector.getAllInvitations(any(classOf[Arn]), any(classOf[LocalDate]))(
          any(classOf[HeaderCarrier]),
          any(classOf[ExecutionContext]))).thenReturn(Future.successful(Seq()))
      val result = await(tested.getRecentAgentInvitations(Arn(""), true, 30))
      result shouldBe empty
    }

    "return tracked invitations updated with clients names" in {
      when(
        agentServicesAccountConnector
          .getTradingName(any(classOf[Nino]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
        .thenReturn(Future.successful(Some("Aaa Itsa Trader")))
      when(
        agentServicesAccountConnector
          .getCustomerDetails(any(classOf[Vrn]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
        .thenReturn(Future.successful(
          CustomerDetails(None, Some(Individual(Some("A"), Some("B"), Some("C"), Some("D"))), Some("Aaa Ltd."))))
      when(
        citizenDetailsConnector
          .getCitizenDetails(any(classOf[Nino]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
        .thenReturn(Future.successful(Citizen(Some("Foo"), Some("Bar"))))
      when(
        invitationsConnector.getAllInvitations(any(classOf[Arn]), any(classOf[LocalDate]))(
          any(classOf[HeaderCarrier]),
          any(classOf[ExecutionContext]))).thenReturn(
        Future.successful(
          Seq(
            invitationForService("HMRC-MTD-IT"),
            invitationForService("HMRC-MTD-VAT"),
            invitationForService("PERSONAL-INCOME-RECORD")
          )))
      val result = await(tested.getRecentAgentInvitations(Arn(""), true, 30))
      result.map(_.clientName).map(_.getOrElse("")) should contain theSameElementsAs Seq(
        "Aaa Itsa Trader",
        "Aaa Ltd.",
        "Foo Bar")
      result.map(_.status) should contain theSameElementsAs Seq("Pending", "Pending", "Pending")
    }

  }

  def invitationForService(service: String) =
    StoredInvitation(
      Arn(""),
      Some("personal"),
      service,
      if (service == "HMRC-MTD-VAT") vrn.value else nino.value,
      "Pending",
      dateTime.minusDays(10),
      dateTime,
      now.minusDays(1),
      "foo",
      new URL("http://foo/")
    )

}
