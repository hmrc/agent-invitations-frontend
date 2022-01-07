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

package models

import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.models.FilterFormStatus._
import uk.gov.hmrc.agentinvitationsfrontend.models.{FilterFormStatus, TrackInformationSorted}
import support.UnitSpec

class FilterFormStatusSpec extends UnitSpec {

  val now = LocalDate.now()

  "FilterFormStatus" should {

    "filter by ClientNotYetResponded" in {
      testFilter(ClientNotYetResponded, minimumRecord("Pending")) shouldBe true
      testFilter(AllStatuses, minimumRecord("Pending")) shouldBe true
    }

    "filter by ExpireInNext5Days" in {
      testFilter(ExpireInNext5Days, minimumRecord("Pending", None, Some(now.plusDays(2)))) shouldBe true
      testFilter(ExpireInNext5Days, minimumRecord("Pending", None, Some(now.plusDays(6)))) shouldBe false
      testFilter(AllStatuses, minimumRecord("Pending", None, Some(now.plusDays(6)))) shouldBe true
    }

    "filter by ActivityWithLast5Days" in {
      testFilter(ActivityWithinLast5Days, minimumRecord("Pending", Some(now.minusDays(4)), Some(now.plusDays(18)))) shouldBe true
      testFilter(ActivityWithinLast5Days, minimumRecord("Pending", Some(now.minusDays(6)), Some(now.plusDays(20)))) shouldBe false
      testFilter(AllStatuses, minimumRecord("Pending", Some(now.minusDays(6)), Some(now.plusDays(20)))) shouldBe true
    }

    "filter by AgentCancelledAuthorisation" in {
      testFilter(AgentCancelledAuthorisation, minimumRecord("AcceptedThenCancelledByAgent")) shouldBe true
      testFilter(AgentCancelledAuthorisation, minimumRecord("Accepted", isRelationshipEnded = true)) shouldBe true
      testFilter(AgentCancelledAuthorisation, minimumRecord("Accepted", isRelationshipEnded = true, relationshipEndedBy = Some("HMRC"))) shouldBe false
      testFilter(AgentCancelledAuthorisation, minimumRecord("Accepted", isRelationshipEnded = true, relationshipEndedBy = Some("Client"))) shouldBe false
      testFilter(AllStatuses, minimumRecord("Accepted", isRelationshipEnded = true, relationshipEndedBy = Some("Client"))) shouldBe true
    }

    "filter by ClientCancelledAuthorisation" in {
      testFilter(ClientCancelledAuthorisation, minimumRecord("AcceptedThenCancelledByClient")) shouldBe true
      testFilter(ClientCancelledAuthorisation, minimumRecord("Accepted", isRelationshipEnded = true)) shouldBe false
      testFilter(ClientCancelledAuthorisation, minimumRecord("Accepted", isRelationshipEnded = true, relationshipEndedBy = Some("HMRC"))) shouldBe false
      testFilter(AllStatuses, minimumRecord("Accepted", isRelationshipEnded = true, relationshipEndedBy = Some("HMRC"))) shouldBe true
    }

    "filter by HMRCCancelledAuthorisation" in {
      testFilter(HMRCCancelledAuthorisation, minimumRecord("AcceptedThenCancelledByHMRC")) shouldBe true
      testFilter(HMRCCancelledAuthorisation, minimumRecord("Accepted", isRelationshipEnded = true)) shouldBe false
      testFilter(AllStatuses, minimumRecord("AcceptedThenCancelledByHMRC")) shouldBe true

    }

    "filter by DeclinedByClient" in {
      testFilter(DeclinedByClient, minimumRecord("Rejected")) shouldBe true
      testFilter(AllStatuses, minimumRecord("Rejected")) shouldBe true
    }

    "filter by Expired" in {
      testFilter(Expired, minimumRecord("Expired")) shouldBe true
      testFilter(AllStatuses, minimumRecord("Expired")) shouldBe true
    }

  }
  def minimumRecord(
    status: String,
    date: Option[LocalDate] = None,
    expiryDate: Option[LocalDate] = None,
    isRelationshipEnded: Boolean = false,
    relationshipEndedBy: Option[String] = None): TrackInformationSorted =
    TrackInformationSorted(
      None,
      "service",
      "clientId",
      "clientIdType",
      None,
      status,
      date.map(_.toDateTimeAtStartOfDay),
      expiryDate.map(_.toDateTimeAtStartOfDay),
      None,
      isRelationshipEnded,
      relationshipEndedBy,
      lastUpdated = None
    )

  private def testFilter(filterFormStatus: FilterFormStatus, i: TrackInformationSorted): Boolean =
    filterFormStatus.filterForStatus(i)

}
