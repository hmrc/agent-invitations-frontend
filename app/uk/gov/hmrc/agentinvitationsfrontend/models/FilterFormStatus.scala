/*
 * Copyright 2023 HM Revenue & Customs
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

import java.time.LocalDateTime

trait FilterFormStatus {
  val filterForStatus: TrackInformationSorted => Boolean
}

object FilterFormStatus {

  case object AllStatuses extends FilterFormStatus {
    override val filterForStatus: TrackInformationSorted => Boolean = (_: TrackInformationSorted) => true
  }
  case object ExpireInNext5Days extends FilterFormStatus {
    override val filterForStatus: TrackInformationSorted => Boolean = (i: TrackInformationSorted) =>
      i.status == "Pending" && i.expiryDate.fold(false)(_.minusDays(5).isBefore(LocalDateTime.now()))
  }
  case object ActivityWithinLast5Days extends FilterFormStatus {
    override val filterForStatus: TrackInformationSorted => Boolean = (i: TrackInformationSorted) =>
      i.dateTime.fold(false)(_.plusDays(5).isAfter(LocalDateTime.now()))
  }
  case object ClientNotYetResponded extends FilterFormStatus {
    override val filterForStatus: TrackInformationSorted => Boolean = (i: TrackInformationSorted) => i.status == "Pending"
  }
  case object AgentCancelledAuthorisation extends FilterFormStatus {
    override val filterForStatus: TrackInformationSorted => Boolean = (i: TrackInformationSorted) =>
      // need to temporarily (until 1/11?) include not client nor hmrc as only recently cancelled auth requests include the endedBy field
      (i.status ==
        "AcceptedThenCancelledByAgent") ||
        (i.status == "Accepted" && i.isRelationshipEnded && !(i.relationshipEndedBy
          .getOrElse("") == "Client" || i.relationshipEndedBy.getOrElse("") == "HMRC"))
  }
  case object DeclinedByClient extends FilterFormStatus {
    override val filterForStatus: TrackInformationSorted => Boolean = (i: TrackInformationSorted) => i.status == "Rejected"
  }
  case object AcceptedByClient extends FilterFormStatus {
    override val filterForStatus: TrackInformationSorted => Boolean = (i: TrackInformationSorted) => i.status == "Accepted" && !i.isRelationshipEnded
  }
  case object Expired extends FilterFormStatus {
    override val filterForStatus: TrackInformationSorted => Boolean = (i: TrackInformationSorted) => i.status == "Expired"
  }
  case object ClientCancelledAuthorisation extends FilterFormStatus {
    override val filterForStatus: TrackInformationSorted => Boolean = (i: TrackInformationSorted) => i.status == "AcceptedThenCancelledByClient"
  }
  case object HMRCCancelledAuthorisation extends FilterFormStatus {
    override val filterForStatus: TrackInformationSorted => Boolean = (i: TrackInformationSorted) => i.status == "AcceptedThenCancelledByHMRC"
  }

  def optionalToEnum: Option[String] => Option[FilterFormStatus] = {
    case Some(str) => Some(toEnum(str))
    case _         => None
  }

  def optionalFromEnum: Option[FilterFormStatus] => Option[String] = {
    case Some(status) => Some(fromEnum(status))
    case _            => None
  }

  def toEnum: String => FilterFormStatus = {
    case "AllStatuses"                  => AllStatuses
    case "ExpireInNext5Days"            => ExpireInNext5Days
    case "ActivityWithinLast5Days"      => ActivityWithinLast5Days
    case "ClientNotYetResponded"        => ClientNotYetResponded
    case "AgentCancelledAuthorisation"  => AgentCancelledAuthorisation
    case "DeclinedByClient"             => DeclinedByClient
    case "AcceptedByClient"             => AcceptedByClient
    case "Expired"                      => Expired
    case "ClientCancelledAuthorisation" => ClientCancelledAuthorisation
    case "HMRCCancelledAuthorisation"   => HMRCCancelledAuthorisation
    case e                              => throw new Exception(s"unexpected input $e")
  }

  def fromEnum: FilterFormStatus => String = {
    case AllStatuses                  => "AllStatuses"
    case ExpireInNext5Days            => "ExpireInNext5Days"
    case ActivityWithinLast5Days      => "ActivityWithinLast5Days"
    case ClientNotYetResponded        => "ClientNotYetResponded"
    case AgentCancelledAuthorisation  => "AgentCancelledAuthorisation"
    case DeclinedByClient             => "DeclinedByClient"
    case AcceptedByClient             => "AcceptedByClient"
    case Expired                      => "Expired"
    case ClientCancelledAuthorisation => "ClientCancelledAuthorisation"
    case HMRCCancelledAuthorisation   => "HMRCCancelledAuthorisation"
  }

  val statuses: Seq[FilterFormStatus] = List(
    AllStatuses,
    ExpireInNext5Days,
    ActivityWithinLast5Days,
    ClientNotYetResponded,
    AgentCancelledAuthorisation,
    DeclinedByClient,
    AcceptedByClient,
    Expired,
    ClientCancelledAuthorisation,
    HMRCCancelledAuthorisation
  )

}
