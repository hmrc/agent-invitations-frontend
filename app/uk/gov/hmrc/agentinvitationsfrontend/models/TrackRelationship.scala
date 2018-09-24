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
import play.api.libs.functional.syntax._
import play.api.libs.json._
import uk.gov.hmrc.agentmtdidentifiers.model.Arn

sealed trait TrackRelationship extends Product with Serializable {
  val arn: Arn
  val serviceName: String
  val dateTo: Option[LocalDate]
  val clientId: String
}

case class ItsaTrackRelationship(arn: Arn, dateTo: Option[LocalDate], clientId: String) extends TrackRelationship {
  val serviceName = Services.HMRCMTDIT
}

object ItsaTrackRelationship {
  implicit val relationshipWrites = Json.writes[ItsaTrackRelationship]

  implicit val reads: Reads[ItsaTrackRelationship] =
    ((JsPath \ "arn").read[Arn] and
      (JsPath \ "dateTo").readNullable[LocalDate] and
      (JsPath \ "referenceNumber").read[String])(ItsaTrackRelationship.apply _)

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
