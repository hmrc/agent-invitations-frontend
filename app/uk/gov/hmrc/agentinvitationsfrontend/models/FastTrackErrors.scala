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

import play.api.libs.json.{JsPath, Json, OFormat, Reads}
import play.api.libs.functional.syntax._

case class FastTrackErrors(
  service: Option[String],
  clientIdentifier: Option[String],
  clientIdentifierType: Option[String],
  globalFormError: Option[String]) {

  def formErrorsMessages: String =
    Seq(service, clientIdentifier, clientIdentifierType, globalFormError).flatten.filter(_.nonEmpty).mkString(s" ")

}

object FastTrackErrors {

  implicit val reads: Reads[FastTrackErrors] = {
    ((JsPath \ "service").readNullable[Seq[String]] and
      (JsPath \ "clientIdentifier").readNullable[Seq[String]] and
      (JsPath \ "clientIdentifierType").readNullable[Seq[String]] and
      (JsPath \ "").readNullable[Seq[String]])(
      (serviceOpts, clientIdentifierTypeOpts, clientIdentifierOpts, globalFormErrorOpts) =>
        FastTrackErrors.apply(
          serviceOpts.getOrElse(Seq.empty).headOption,
          clientIdentifierOpts.getOrElse(Seq.empty).headOption,
          clientIdentifierTypeOpts.getOrElse(Seq.empty).headOption,
          globalFormErrorOpts.getOrElse(Seq.empty).headOption
      )
    )
  }
}
