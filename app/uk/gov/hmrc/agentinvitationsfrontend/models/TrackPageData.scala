/*
 * Copyright 2018 HM Revenue & Customs
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
import play.api.libs.json.Json

case class TrackInformationSorted(
  service: String,
  clientName: Option[String],
  status: String,
  date: Option[LocalDate],
  expiryDate: Option[LocalDate],
  invitationId: Option[String]) {

  def effectiveStatus(implicit now: LocalDate): String =
    expiryDate match {
      case Some(expDate) if status == "Pending" && (now.isAfter(expDate) || now.isEqual(expDate)) => "Expired"
      case Some(_)                                                                                => status
      case None                                                                                   => status
    }

  def sortDate: Option[LocalDate] =
    if (date.isEmpty) expiryDate
    else if (expiryDate.isEmpty) date
    else None
}

object TrackInformationSorted {

  implicit val format = Json.format[TrackInformationSorted]

  implicit def dateOrdering: Ordering[LocalDate] = Ordering.fromLessThan(_ isAfter _)

  val orderingByDate: Ordering[TrackInformationSorted] = Ordering.by(_.sortDate)
}
