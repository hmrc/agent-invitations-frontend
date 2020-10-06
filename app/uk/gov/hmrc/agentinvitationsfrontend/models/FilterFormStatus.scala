/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.agentinvitationsfrontend.models

import org.joda.time.LocalDate

trait FilterFormStatus {
  val filterFunction: TrackInformationSorted => Boolean
}

object FilterFormStatus {

  case object AllStatuses extends FilterFormStatus {
    override val filterFunction = (i: TrackInformationSorted) => true
  }
  case object ExpireInNext5Days extends FilterFormStatus {
    override val filterFunction = (i: TrackInformationSorted) =>
      i.status == "Pending" && i.expiryDate.fold(false)(_.minusDays(5).isBefore(LocalDate.now()))
  }
  case object ActivityWithinLast5Days extends FilterFormStatus {
    override val filterFunction = (i: TrackInformationSorted) =>
      i.date.fold(false)(_.plusDays(5).isAfter(LocalDate.now()))
  }
  case object ClientNotYetResponded extends FilterFormStatus {
    override val filterFunction = (i: TrackInformationSorted) => i.status == "Pending"
  }
  case object AgentCancelledAuthorisation extends FilterFormStatus {
    override val filterFunction = (i: TrackInformationSorted) => i.status == "Cancelled"
  }
  case object DeclinedByClient extends FilterFormStatus {
    override val filterFunction = (i: TrackInformationSorted) => i.status == "Rejected"
  }
  case object AcceptedByClient extends FilterFormStatus {
    override val filterFunction = (i: TrackInformationSorted) => i.status == "Accepted"
  }
  case object Expired extends FilterFormStatus {
    override val filterFunction = (i: TrackInformationSorted) => i.status == "Expired"
  }

}
