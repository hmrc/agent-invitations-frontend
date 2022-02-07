/*
 * Copyright 2022 HM Revenue & Customs
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

import support.TrackServiceStubsAndData
import uk.gov.hmrc.agentinvitationsfrontend.models.{FilterFormStatus, InactiveTrackRelationship, PageInfo, TrackInformationSorted}
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import support.UnitSpec
import play.api.test.Helpers._

import scala.concurrent.ExecutionContext.Implicits.global

class TrackServiceSpecWithFilter extends UnitSpec with TrackServiceStubsAndData {

  "TrackService" should {

    "match an invalid relationship with an invitation, copying the end date of the relationship to the invitation " +
      "then discard the invalid relationship" in {

      givenGetTradingName()
      givenGetNinoForMtdit()
      givenGetInactiveIrvRelationships()
      givenGetGivenInactiveRelationships(
        InactiveTrackRelationship(Arn(""), "personal", nino2.value, HMRCMTDIT, Some(now.minusDays(2).toLocalDate)),
        InactiveTrackRelationship(Arn(""), "business", vrn1.value, HMRCMTDVAT, Some(now.minusDays(6).toLocalDate)),
        InactiveTrackRelationship(Arn(""), "business", cgtRef1.value, HMRCCGTPD, Some(now.minusDays(35).toLocalDate)) //won't count
      )
      givenGetAllInvitations()

      val result = await(tested
        .bindInvitationsAndRelationships(arn = Arn(""), showLastDays = 30, pageInfo = PageInfo(1, 10), filterByClient = None, filterByStatus = None))

      result shouldBe tested.TrackResultsPage(
        results = Seq(
          TrackInformationSorted(
            clientType = Some("business"),
            service = HMRCMTDVAT,
            clientId = vrn2.value,
            clientIdType = "vrn",
            clientName = Some("Superior Ltd"),
            status = "Pending",
            dateTime = None,
            expiryDate = Some(now.plusDays(18)),
            invitationId = Some("id7"),
            isRelationshipEnded = false,
            relationshipEndedBy = None,
            lastUpdated = Some(dateTime.plusDays(7))
          ),
          TrackInformationSorted(
            clientType = Some("personal"),
            service = HMRCMTDIT,
            clientId = nino1.value,
            clientIdType = "ni",
            clientName = Some("Aaa Itsa Trader"),
            status = "Pending",
            dateTime = None,
            expiryDate = Some(now.plusDays(11)),
            invitationId = Some("id1"),
            isRelationshipEnded = false,
            relationshipEndedBy = None,
            lastUpdated = Some(dateTime)
          ),
          TrackInformationSorted(
            clientType = Some("personal"),
            service = HMRCMTDIT,
            clientId = nino2.value,
            clientIdType = "ni",
            clientName = Some("Aaa Itsa Trader"),
            status = "Accepted",
            dateTime = Some(now.minusDays(1)),
            expiryDate = None,
            invitationId = Some("id5"),
            isRelationshipEnded = false,
            relationshipEndedBy = None,
            lastUpdated = Some(dateTime.plusDays(9))
          ),
          TrackInformationSorted(
            clientType = Some("personal"),
            service = HMRCMTDIT,
            clientId = nino2.value,
            clientIdType = "ni",
            clientName = Some("Aaa Itsa Trader"),
            status = "AcceptedThenCancelledByAgent",
            dateTime = Some(now.minusDays(2)),
            expiryDate = None,
            invitationId = Some("id4"),
            isRelationshipEnded = true,
            relationshipEndedBy = Some("Agent"),
            lastUpdated = Some(dateTime.plusDays(8))
          ),
          TrackInformationSorted(
            clientType = Some("personal"),
            service = HMRCPIR,
            clientId = nino1.value,
            clientIdType = "ni",
            clientName = Some("John Jones"),
            status = "Cancelled",
            dateTime = Some(now.minusDays(3)),
            expiryDate = None,
            invitationId = Some("id3"),
            isRelationshipEnded = false,
            relationshipEndedBy = None,
            lastUpdated = Some(dateTime.plusDays(7))
          ),
          TrackInformationSorted(
            clientType = Some("business"),
            service = HMRCMTDVAT,
            clientId = vrn1.value,
            clientIdType = "vrn",
            clientName = Some("Superior Ltd"),
            status = "AcceptedThenCancelledByClient",
            dateTime = Some(now.minusDays(6)),
            expiryDate = None,
            invitationId = Some("id6"),
            isRelationshipEnded = true,
            relationshipEndedBy = Some("Client"),
            lastUpdated = Some(dateTime.plusDays(4))
          ),
          TrackInformationSorted(
            clientType = Some("personal"),
            service = HMRCPIR,
            clientId = nino1.value,
            clientIdType = "ni",
            clientName = Some("John Jones"),
            status = "Expired",
            dateTime = None,
            expiryDate = Some(now.minusDays(9)),
            invitationId = Some("id2"),
            isRelationshipEnded = false,
            relationshipEndedBy = None,
            lastUpdated = Some(dateTime.plusDays(1))
          ),
          TrackInformationSorted(
            clientType = Some("business"),
            service = HMRCCGTPD,
            clientId = cgtRef1.value,
            clientIdType = "CGTPDRef",
            clientName = Some("A Trust or an Estate"),
            status = "Rejected",
            dateTime = Some(now.minusDays(10)),
            expiryDate = None,
            invitationId = Some("id8"),
            isRelationshipEnded = false,
            relationshipEndedBy = None,
            lastUpdated = Some(dateTime)
          ),
          TrackInformationSorted(
            clientType = Some("business"),
            service = HMRCCGTPD,
            clientId = cgtRef1.value,
            clientIdType = "CGTPDRef",
            clientName = Some("A Trust or an Estate"),
            status = "Accepted",
            dateTime = Some(now.minusDays(35)),
            expiryDate = None,
            invitationId = Some("id9"),
            isRelationshipEnded = true,
            relationshipEndedBy = None,
            lastUpdated = Some(dateTime.minusDays(25))
          )
        ),
        totalResults = 9,
        clientSet = Set("Superior Ltd", "Aaa Itsa Trader", "John Jones", "A Trust or an Estate")
      )

    }

    "filter results by clientName, returning only records that match the filter but include the full list of " +
      "clients in the config (to allow a new filter by name)" in {

      givenGetTradingName()
      givenGetCitizenDetails()
      givenGetCustomerDetails()
      givenGetCgtSubscription()
      givenGetNinoForMtdit()
      givenGetInactiveIrvRelationships()
      givenGetGivenInactiveRelationships(
        InactiveTrackRelationship(Arn(""), "personal", nino2.value, HMRCMTDIT, Some(now.minusDays(2).toLocalDate)),
        InactiveTrackRelationship(Arn(""), "business", vrn1.value, HMRCMTDVAT, Some(now.minusDays(6).toLocalDate)),
        InactiveTrackRelationship(Arn(""), "business", cgtRef1.value, HMRCCGTPD, Some(now.minusDays(35).toLocalDate)) //won't count
      )
      givenGetAllInvitations()

      val result = await(
        tested.bindInvitationsAndRelationships(
          arn = Arn(""),
          showLastDays = 30,
          pageInfo = PageInfo(1, 10),
          filterByClient = Some("John Jones"),
          filterByStatus = None))

      result shouldBe tested.TrackResultsPage(
        results = Seq(
          TrackInformationSorted(
            clientType = Some("personal"),
            service = HMRCPIR,
            clientId = nino1.value,
            clientIdType = "ni",
            clientName = Some("John Jones"),
            status = "Cancelled",
            dateTime = Some(now.minusDays(3)),
            expiryDate = None,
            invitationId = Some("id3"),
            isRelationshipEnded = false,
            relationshipEndedBy = None,
            lastUpdated = Some(dateTime.plusDays(7))
          ),
          TrackInformationSorted(
            clientType = Some("personal"),
            service = HMRCPIR,
            clientId = nino1.value,
            clientIdType = "ni",
            clientName = Some("John Jones"),
            status = "Expired",
            dateTime = None,
            expiryDate = Some(now.minusDays(9)),
            invitationId = Some("id2"),
            isRelationshipEnded = false,
            relationshipEndedBy = None,
            lastUpdated = Some(dateTime.plusDays(1))
          )
        ),
        totalResults = 2,
        clientSet = Set("Aaa Itsa Trader", "A Trust or an Estate", "John Jones", "Superior Ltd")
      )
    }

    "filter results by status, returning only records that match the filter but include the full list of " +
      "clients in the config (to allow a new filter by name)" in {

      givenGetTradingName()
      givenGetCitizenDetails()
      givenGetCustomerDetails()
      givenGetCgtSubscription()
      givenGetNinoForMtdit()
      givenGetInactiveIrvRelationships()
      givenGetGivenInactiveRelationships(
        InactiveTrackRelationship(Arn(""), "personal", nino2.value, HMRCMTDIT, Some(now.minusDays(2).toLocalDate)),
        InactiveTrackRelationship(Arn(""), "business", vrn1.value, HMRCMTDVAT, Some(now.minusDays(6).toLocalDate)),
        InactiveTrackRelationship(Arn(""), "business", cgtRef1.value, HMRCCGTPD, Some(now.minusDays(35).toLocalDate)) //won't count
      )
      givenGetAllInvitations()

      val result = await(
        tested.bindInvitationsAndRelationships(
          arn = Arn(""),
          showLastDays = 30,
          pageInfo = PageInfo(1, 10),
          filterByClient = None,
          filterByStatus = Some(FilterFormStatus.ClientNotYetResponded)
        ))

      result shouldBe tested.TrackResultsPage(
        results = Seq(
          TrackInformationSorted(
            clientType = Some("business"),
            service = HMRCMTDVAT,
            clientId = vrn2.value,
            clientIdType = "vrn",
            clientName = Some("Superior Ltd"),
            status = "Pending",
            dateTime = None,
            expiryDate = Some(now.plusDays(18)),
            invitationId = Some("id7"),
            isRelationshipEnded = false,
            relationshipEndedBy = None,
            lastUpdated = Some(dateTime.plusDays(7))
          ),
          TrackInformationSorted(
            clientType = Some("personal"),
            service = HMRCMTDIT,
            clientId = nino1.value,
            clientIdType = "ni",
            clientName = Some("Aaa Itsa Trader"),
            status = "Pending",
            dateTime = None,
            expiryDate = Some(now.plusDays(11)),
            invitationId = Some("id1"),
            isRelationshipEnded = false,
            relationshipEndedBy = None,
            lastUpdated = Some(dateTime)
          )
        ),
        totalResults = 2,
        clientSet = Set("Aaa Itsa Trader", "A Trust or an Estate", "John Jones", "Superior Ltd")
      )
    }

    "Use the company name stored with the invitation if it's available" in {

      givenGetTradingName()
      givenGetCitizenDetails()
      givenGetCustomerDetails()
      givenGetCgtSubscription()
      givenGetNinoForMtdit()
      givenGetInactiveIrvRelationships()
      givenGetGivenInactiveRelationships(
        InactiveTrackRelationship(Arn(""), "personal", nino2.value, HMRCMTDIT, Some(now.minusDays(2).toLocalDate)),
        InactiveTrackRelationship(Arn(""), "business", vrn1.value, HMRCMTDVAT, Some(now.minusDays(6).toLocalDate)),
        InactiveTrackRelationship(Arn(""), "business", cgtRef1.value, HMRCCGTPD, Some(now.minusDays(35).toLocalDate)) //won't count
      )
      givenGetAllInvitationsWithDetailsAvailable()

      val result = await(
        tested.bindInvitationsAndRelationships(
          arn = Arn(""),
          showLastDays = 30,
          pageInfo = PageInfo(1, 10),
          filterByClient = None,
          filterByStatus = None
        ))

      result shouldBe tested.TrackResultsPage(
        results = Seq(
          TrackInformationSorted(
            clientType = Some("business"),
            service = HMRCMTDVAT,
            clientId = vrn2.value,
            clientIdType = "vrn",
            clientName = Some("Perihelion"),
            status = "Pending",
            dateTime = None,
            expiryDate = Some(now.plusDays(18)),
            invitationId = Some("id7"),
            isRelationshipEnded = false,
            relationshipEndedBy = None,
            lastUpdated = Some(dateTime.plusDays(7))
          )
        ),
        totalResults = 1,
        clientSet = Set("Perihelion")
      )
    }

  }

}
