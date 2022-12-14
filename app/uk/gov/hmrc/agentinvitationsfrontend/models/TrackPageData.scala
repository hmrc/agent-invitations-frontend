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

package uk.gov.hmrc.agentinvitationsfrontend.models

import uk.gov.hmrc.agentmtdidentifiers.model.Service

import java.time.LocalDateTime

case class TrackInformationSorted(
  clientType: Option[String],
  service: Option[Service],
  clientId: String,
  clientIdType: String,
  clientName: Option[String],
  status: String,
  dateTime: Option[LocalDateTime],
  expiryDate: Option[LocalDateTime],
  invitationId: Option[String],
  isRelationshipEnded: Boolean,
  relationshipEndedBy: Option[String],
  lastUpdated: Option[LocalDateTime],
  isAltItsa: Boolean = false) {

  def sortDate: Option[LocalDateTime] =
    if (dateTime.isEmpty) expiryDate
    else if (expiryDate.isEmpty) dateTime
    else None
}

object TrackInformationSorted {

  implicit def dateOrdering: Ordering[LocalDateTime] = Ordering.fromLessThan(_ isAfter _)

  val orderingByDate: Ordering[TrackInformationSorted] = Ordering.by(_.sortDate)
}
