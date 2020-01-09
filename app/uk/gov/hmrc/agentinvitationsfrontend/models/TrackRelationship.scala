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
import uk.gov.hmrc.http.controllers.RestFormats.localDateFormats

sealed trait TrackRelationship extends Product with Serializable {
  val arn: Arn
  val clientType: Option[String]
  val serviceName: String
  val dateTo: Option[LocalDate]
  val clientId: String
}

case class ItsaInactiveTrackRelationship(arn: Arn, dateTo: Option[LocalDate], clientId: String)
    extends TrackRelationship {
  val serviceName = Services.HMRCMTDIT
  val clientType = Some("personal")
}

object ItsaInactiveTrackRelationship {
  implicit val relationshipWrites = Json.writes[ItsaInactiveTrackRelationship]

  implicit val reads: Reads[ItsaInactiveTrackRelationship] =
    ((JsPath \ "arn").read[Arn] and
      (JsPath \ "dateTo").readNullable[LocalDate] and
      (JsPath \ "referenceNumber").read[String])(ItsaInactiveTrackRelationship.apply _)

}

case class VatTrackRelationship(arn: Arn, clientType: Option[String], dateTo: Option[LocalDate], clientId: String)
    extends TrackRelationship {
  val serviceName = Services.HMRCMTDVAT
}

object VatTrackRelationship {
  implicit val relationshipWrites = Json.writes[VatTrackRelationship]

  implicit val reads: Reads[VatTrackRelationship] =
    ((JsPath \ "arn").read[Arn] and
      (JsPath \ "clientType").readNullable[String] and
      (JsPath \ "dateTo").readNullable[LocalDate] and
      (JsPath \ "referenceNumber").read[String])(VatTrackRelationship.apply _)

}

case class TrustTrackRelationship(arn: Arn, dateTo: Option[LocalDate], clientId: String) extends TrackRelationship {
  val serviceName = Services.TRUST
  val clientType = Some("business")
}

object TrustTrackRelationship {
  implicit val relationshipWrites = Json.writes[TrustTrackRelationship]

  implicit val reads: Reads[TrustTrackRelationship] =
    ((JsPath \ "arn").read[Arn] and
      (JsPath \ "dateTo").readNullable[LocalDate] and
      (JsPath \ "referenceNumber").read[String])(TrustTrackRelationship.apply _)

}

case class CgtTrackRelationship(arn: Arn, clientType: Option[String], dateTo: Option[LocalDate], clientId: String)
    extends TrackRelationship {
  val serviceName = Services.HMRCCGTPD
}

object CgtTrackRelationship {
  implicit val relationshipWrites = Json.writes[CgtTrackRelationship]

  implicit val reads: Reads[CgtTrackRelationship] =
    ((JsPath \ "arn").read[Arn] and
      (JsPath \ "clientType").readNullable[String] and
      (JsPath \ "dateTo").readNullable[LocalDate] and
      (JsPath \ "referenceNumber").read[String])(CgtTrackRelationship.apply _)

}

case class IrvTrackRelationship(arn: Arn, dateTo: Option[LocalDate], clientId: String) extends TrackRelationship {
  val serviceName = Services.HMRCPIR
  val clientType = Some("personal")
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
