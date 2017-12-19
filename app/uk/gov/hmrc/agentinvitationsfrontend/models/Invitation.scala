/*
 * Copyright 2017 HM Revenue & Customs
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

import java.net.URL

import org.joda.time.{DateTime, LocalDate}
import play.api.libs.json.{JsPath, Reads}
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.domain.SimpleObjectReads
import play.api.libs.functional.syntax._

case class Invitation(
  id: String,
  arn: Arn,
  service: String,
  clientId: String,
  status: String,
  created: DateTime,
  lastUpdated: DateTime,
  expiryDate: LocalDate,
  selfUrl: URL,
  acceptUrl: Option[URL],
  rejectUrl: Option[URL])

object Invitation {
  import uk.gov.hmrc.http.controllers.RestFormats.dateTimeFormats

  def reads(readingFrom: URL): Reads[Invitation] = {
    implicit val urlReads = new SimpleObjectReads[URL]("href", s => new URL(readingFrom, s))
    (
      (JsPath \ "id").read[String] and
      (JsPath \ "arn").read[Arn] and
      (JsPath \ "service").read[String] and
      (JsPath \ "clientId").read[String] and
      (JsPath \ "status").read[String] and
      (JsPath \ "created").read[DateTime] and
      (JsPath \ "lastUpdated").read[DateTime] and
      (JsPath \ "expiryDate").read[LocalDate] and
      (JsPath \ "_links" \ "self").read[URL] and
      (JsPath \ "_links" \ "accept").readNullable[URL] and
      (JsPath \ "_links" \ "reject").readNullable[URL])(Invitation.apply _)
  }

}