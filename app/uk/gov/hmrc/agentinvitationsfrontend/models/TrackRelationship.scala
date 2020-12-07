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

import java.time.LocalDateTime

import org.joda.time.LocalDate
import play.api.libs.functional.syntax._
import play.api.libs.json._
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import play.api.libs.json.JodaWrites._
import play.api.libs.json.JodaReads._

sealed trait TrackRelationship extends Product with Serializable {
  val arn: Arn
  val clientType: String
  val clientId: String
  val service: String
  val dateTo: Option[LocalDate]
}

case class InactiveTrackRelationship(arn: Arn, clientType: String, clientId: String, service: String, dateTo: Option[LocalDate])
    extends TrackRelationship

object InactiveTrackRelationship {
  implicit val format: OFormat[InactiveTrackRelationship] = Json.format[InactiveTrackRelationship]
}

case class IrvTrackRelationship(arn: Arn, dateTo: Option[LocalDate], clientId: String) extends TrackRelationship {
  val service = Services.HMRCPIR
  val clientType = "personal"
}

object IrvTrackRelationship {
  implicit val relationshipWrites = Json.writes[IrvTrackRelationship]

  implicit val reads: Reads[IrvTrackRelationship] =
    ((JsPath \ "arn").read[Arn] and
      (JsPath \ "endDate").readNullable[LocalDateTime].map(dateOpts => javaDateTimeToJodaDate(dateOpts)) and
      (JsPath \ "clientId").read[String])(IrvTrackRelationship.apply _)

  def javaDateTimeToJodaDate(javaTimeOpts: Option[LocalDateTime]): Option[LocalDate] =
    javaTimeOpts match {
      case Some(javaTime) => Some(LocalDate.parse(javaTime.toLocalDate.toString))
      case _              => Some(LocalDate.parse("9999-12-31"))
    }

}
