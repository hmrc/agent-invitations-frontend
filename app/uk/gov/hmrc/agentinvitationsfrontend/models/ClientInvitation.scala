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

import org.joda.time.DateTime
import play.api.libs.json._
import uk.gov.hmrc.agentmtdidentifiers.model.Arn

case class ClientInvitation(
                             self: URL,
                             arn: Arn,
                             service: String,
                             status: String,
                             created: DateTime,
                             lastUpdated: DateTime,
                             acceptUrl: Option[URL],
                             rejectUrl: Option[URL]
                           )

object ClientInvitation {
  import uk.gov.hmrc.http.controllers.RestFormats.dateTimeFormats
  implicit val urlReads: Reads[URL] = JsPath.read[String].map(new URL(_))
  implicit object UrlWrites extends Writes[URL] {
    def writes(url: URL) = JsString(url.toString)
  }

  implicit val format = Json.format[ClientInvitation]
}