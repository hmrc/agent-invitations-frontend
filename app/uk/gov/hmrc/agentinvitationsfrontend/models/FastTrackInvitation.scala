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

import play.api.libs.json.{JsPath, Json, Reads}
import uk.gov.hmrc.domain.TaxIdentifier
import play.api.libs.functional.syntax._

case class FastTrackInvitation (service: Option[String],
                                clientIdentifierType: Option[String],
                                clientIdentifier: Option[String],
                                knownFact: Option[String])

object FastTrackInvitation {
  implicit val format = Json.format[FastTrackInvitation]

  implicit val reads: Reads[FastTrackInvitation] = {
    (
        (JsPath \ "service").readNullable[String] and
        (JsPath \ "clientIdentifierType").readNullable[String] and
        (JsPath \ "clientIdentifier").readNullable[String] and
        (JsPath \ "knownFact").readNullable[String])(FastTrackInvitation.apply _)
  }
}

trait KnownFact {
  val value: String
}

case class Postcode(value: String) extends KnownFact
case class VatRegDate(value: String) extends KnownFact

object Postcode {
  implicit val format = Json.format[Postcode]
}

object VatRegDate {
  implicit val format = Json.format[VatRegDate]
}
