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

package uk.gov.hmrc.agentinvitationsfrontend.models

import java.time.LocalDateTime

import org.joda.time.LocalDate
import play.api.libs.functional.syntax._
import play.api.libs.json._
import uk.gov.hmrc.agentmtdidentifiers.model.Arn

sealed trait TrackRelationship extends Product with Serializable {
  val arn: Arn
  val serviceName: String
  val dateTo: Option[LocalDate]
  val clientId: String
}

case class ItsaInactiveTrackRelationship(arn: Arn, dateTo: Option[LocalDate], clientId: String)
    extends TrackRelationship {
  val serviceName = Services.HMRCMTDIT
}

object ItsaInactiveTrackRelationship {
  implicit val relationshipWrites = Json.writes[ItsaInactiveTrackRelationship]

  implicit val reads: Reads[ItsaInactiveTrackRelationship] =
    ((JsPath \ "arn").read[Arn] and
      (JsPath \ "dateTo").readNullable[LocalDate] and
      (JsPath \ "referenceNumber").read[String])(ItsaInactiveTrackRelationship.apply _)

}

trait Relationship {
  val arn: Arn
  val dateTo: Option[LocalDate]
  val dateFrom: Option[LocalDate]
}

case class ItsaRelationship(arn: Arn, dateTo: Option[LocalDate], dateFrom: Option[LocalDate]) extends Relationship

object ItsaRelationship {
  implicit val relationshipWrites = Json.writes[ItsaRelationship]

  implicit val reads: Reads[ItsaRelationship] = ((JsPath \ "arn").read[Arn] and
    (JsPath \ "dateTo").readNullable[LocalDate] and
    (JsPath \ "dateFrom").readNullable[LocalDate])(ItsaRelationship.apply _)
}

case class VatTrackRelationship(arn: Arn, dateTo: Option[LocalDate], clientId: String) extends TrackRelationship {
  val serviceName = Services.HMRCMTDVAT
}

object VatTrackRelationship {
  implicit val relationshipWrites = Json.writes[VatTrackRelationship]

  implicit val reads: Reads[VatTrackRelationship] =
    ((JsPath \ "arn").read[Arn] and
      (JsPath \ "dateTo").readNullable[LocalDate] and
      (JsPath \ "referenceNumber").read[String])(VatTrackRelationship.apply _)

}

case class IrvTrackRelationship(arn: Arn, dateTo: Option[LocalDate], clientId: String) extends TrackRelationship {
  val serviceName = Services.HMRCPIR
}

object IrvTrackRelationship {
  implicit val relationshipWrites = Json.writes[IrvTrackRelationship]

  implicit val reads: Reads[IrvTrackRelationship] =
    ((JsPath \ "arn").read[Arn] and
      (JsPath \ "endDate").readNullable[LocalDateTime].map(date => javaDateTimeToJodaDate(date.get)) and
      (JsPath \ "clientId").read[String])(IrvTrackRelationship.apply _)

  def javaDateTimeToJodaDate(javaTime: LocalDateTime): Option[LocalDate] =
    Some(LocalDate.parse(javaTime.toLocalDate.toString))

}
