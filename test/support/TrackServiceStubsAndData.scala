/*
 * Copyright 2021 HM Revenue & Customs
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

package support

import java.net.URL

import org.joda.time.{DateTime, LocalDate}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{mock, when}
import uk.gov.hmrc.agentinvitationsfrontend.connectors._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services.TrackService
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, CgtRef, MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait TrackServiceStubsAndData {

  val acaConnector = mock(classOf[AgentClientAuthorisationConnector])
  val citizenDetailsConnector = mock(classOf[CitizenDetailsConnector])
  val relationshipsConnector = mock(classOf[RelationshipsConnector])
  val pirRelationshipConnector = mock(classOf[PirRelationshipConnector])
  val tested = new TrackService(relationshipsConnector, pirRelationshipConnector, acaConnector, citizenDetailsConnector)

  val vrn1 = Vrn("101747696")
  val vrn2 = Vrn("729899554")
  val nino1 = Nino("AB123456A")
  val nino2 = Nino("RS652949D")
  val cgtRef1 = CgtRef("XMCGTP704066305")

  val HMRCMTDIT = "HMRC-MTD-IT"
  val HMRCPIR = "PERSONAL-INCOME-RECORD"
  val HMRCMTDVAT = "HMRC-MTD-VAT"
  val TRUST = "HMRC-TERS-ORG"
  val HMRCCGTPD = "HMRC-CGT-PD"
  val HMRCNI = "HMRC-NI"

  val dateTime = DateTime.now.minusDays(10)

  implicit val now: LocalDate = LocalDate.now
  implicit val hc: HeaderCarrier = HeaderCarrier()

  def givenGetTradingName() =
    when(
      acaConnector
        .getTradingName(any(classOf[Nino]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
      .thenReturn(Future.successful(Some("Aaa Itsa Trader")))

  def givenGetCustomerDetails() =
    when(
      acaConnector
        .getCustomerDetails(any(classOf[Vrn]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
      .thenReturn(Future.successful(CustomerDetails(Some("Superior Ltd"), None, None)))

  def givenGetCgtSubscription() =
    when(
      acaConnector
        .getCgtSubscription(any(classOf[CgtRef]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
      .thenReturn(
        Future successful
          Some(
            CgtSubscription(
              "cgt",
              SubscriptionDetails(
                TypeOfPersonDetails("business", Right(OrganisationName("A Trust or an Estate"))),
                CgtAddressDetails(addressLine1 = "A Street", countryCode = "GB"))
            ))
      )

  def givenGetNinoForMtdit() =
    when(
      acaConnector
        .getNinoForMtdItId(any(classOf[MtdItId]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
      .thenReturn(
        Future successful
          Some(nino2)
      )

  def givenGetCitizenDetails() =
    when(
      citizenDetailsConnector
        .getCitizenDetails(any(classOf[Nino]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
      .thenReturn(Future.successful(Citizen(Some("John"), Some("Jones"))))

  def givenGetGivenInactiveRelationships(inactiveRelationships: InactiveTrackRelationship*) =
    when(
      relationshipsConnector
        .getInactiveRelationships(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
      .thenReturn(
        Future.successful(
          Seq(
            inactiveRelationships: _*
          )))

  def givenGetInactiveIrvRelationships(inactiveIrvRelationships: IrvTrackRelationship*) =
    when(pirRelationshipConnector.getInactiveIrvRelationships(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
      .thenReturn(
        Future.successful(
          Seq(
            inactiveIrvRelationships: _*
          )))

  def givenGetAllInvitations() =
    when(acaConnector.getAllInvitations(any(classOf[Arn]), any(classOf[LocalDate]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
      .thenReturn(Future.successful(Seq(
        StoredInvitation(
          arn = Arn(""),
          clientType = Some("personal"),
          service = HMRCMTDIT,
          clientId = nino1.value,
          detailsForEmail = None,
          status = "Pending",
          created = dateTime,
          lastUpdated = dateTime,
          expiryDate = now.plusDays(11),
          invitationId = "id1",
          isRelationshipEnded = false,
          relationshipEndedBy = None,
          selfUrl = new URL("http://id1")
        ),
        StoredInvitation(
          arn = Arn(""),
          clientType = Some("personal"),
          service = HMRCPIR,
          clientId = nino1.value,
          detailsForEmail = None,
          status = "Expired",
          created = dateTime.minusDays(20),
          lastUpdated = dateTime.plusDays(1),
          expiryDate = now.minusDays(9),
          invitationId = "id2",
          isRelationshipEnded = false,
          relationshipEndedBy = None,
          selfUrl = new URL("http://id2")
        ),
        StoredInvitation(
          arn = Arn(""),
          clientType = Some("personal"),
          service = HMRCPIR,
          clientId = nino1.value,
          detailsForEmail = None,
          status = "Cancelled",
          created = dateTime.plusDays(2),
          lastUpdated = dateTime.plusDays(7),
          expiryDate = now.plusDays(13),
          invitationId = "id3",
          isRelationshipEnded = false,
          relationshipEndedBy = None,
          selfUrl = new URL("http://id3")
        ),
        StoredInvitation(
          arn = Arn(""),
          clientType = Some("personal"),
          service = HMRCMTDIT,
          clientId = nino2.value,
          detailsForEmail = None,
          status = "Accepted",
          created = dateTime,
          lastUpdated = dateTime.plusDays(8),
          expiryDate = now.plusDays(11),
          invitationId = "id4",
          isRelationshipEnded = true,
          relationshipEndedBy = Some("Agent"),
          selfUrl = new URL("http://id4")
        ),
        StoredInvitation(
          arn = Arn(""),
          clientType = Some("personal"),
          service = HMRCMTDIT,
          clientId = nino2.value,
          detailsForEmail = None,
          status = "Accepted",
          created = dateTime.plusDays(9),
          lastUpdated = dateTime.plusDays(9),
          expiryDate = now.plusDays(20),
          invitationId = "id5",
          isRelationshipEnded = false,
          relationshipEndedBy = None,
          selfUrl = new URL("http://id5")
        ),
        StoredInvitation(
          arn = Arn(""),
          clientType = Some("business"),
          service = HMRCMTDVAT,
          clientId = vrn1.value,
          detailsForEmail = None,
          status = "Accepted",
          created = dateTime,
          lastUpdated = dateTime.plusDays(4),
          expiryDate = now.plusDays(11),
          invitationId = "id6",
          isRelationshipEnded = true,
          relationshipEndedBy = Some("Client"),
          selfUrl = new URL("http://id6")
        ),
        StoredInvitation(
          arn = Arn(""),
          clientType = Some("business"),
          service = HMRCMTDVAT,
          clientId = vrn2.value,
          detailsForEmail = None,
          status = "Pending",
          created = dateTime.plusDays(7),
          lastUpdated = dateTime.plusDays(7),
          expiryDate = now.plusDays(18),
          invitationId = "id7",
          isRelationshipEnded = false,
          relationshipEndedBy = None,
          selfUrl = new URL("http://id7")
        ),
        StoredInvitation(
          arn = Arn(""),
          clientType = Some("business"),
          service = HMRCCGTPD,
          clientId = cgtRef1.value,
          detailsForEmail = None,
          status = "Rejected",
          created = dateTime.minusDays(20),
          lastUpdated = dateTime,
          expiryDate = now.plusDays(1),
          invitationId = "id8",
          isRelationshipEnded = false,
          relationshipEndedBy = None,
          selfUrl = new URL("http://id8")
        ),
        StoredInvitation(
          arn = Arn(""),
          clientType = Some("business"),
          service = HMRCCGTPD,
          clientId = cgtRef1.value,
          detailsForEmail = None,
          status = "Accepted",
          created = dateTime.minusDays(30),
          lastUpdated = dateTime.minusDays(25),
          expiryDate = now.minusDays(19),
          invitationId = "id9",
          isRelationshipEnded = true,
          relationshipEndedBy = None,
          selfUrl = new URL("http://id9")
        )
      )))

  def givenGetAllInvitationsWithDetailsAvailable() =
    when(acaConnector.getAllInvitations(any(classOf[Arn]), any(classOf[LocalDate]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
      .thenReturn(Future.successful(Seq(StoredInvitation(
        arn = Arn(""),
        clientType = Some("business"),
        service = HMRCMTDVAT,
        clientId = vrn2.value,
        detailsForEmail = Some(DetailsForEmail("aphelion@mail.com", "Aphelion Ltd", "Perihelion")),
        status = "Pending",
        created = dateTime.plusDays(7),
        lastUpdated = dateTime.plusDays(7),
        expiryDate = now.plusDays(18),
        invitationId = "id7",
        isRelationshipEnded = false,
        relationshipEndedBy = None,
        selfUrl = new URL("http://id7")
      ))))

}
