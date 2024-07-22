/*
 * Copyright 2023 HM Revenue & Customs
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

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads}

case class FastTrackErrors(
  clientType: Option[String],
  service: Option[String],
  clientIdentifier: Option[String],
  clientIdentifierType: Option[String],
  globalFormError: Option[String]
) {

  def formErrorsMessages: String =
    Seq(clientType, service, clientIdentifier, clientIdentifierType, globalFormError).flatten
      .filter(_.nonEmpty)
      .mkString(s" ")

}

object FastTrackErrors {

  implicit val reads: Reads[FastTrackErrors] =
    ((JsPath \ "clientType").readNullable[Seq[String]] and
      (JsPath \ "service").readNullable[Seq[String]] and
      (JsPath \ "clientIdentifier").readNullable[Seq[String]] and
      (JsPath \ "clientIdentifierType").readNullable[Seq[String]] and
      (JsPath \ "").readNullable[Seq[String]])((clientTypeOpts, serviceOpts, clientIdentifierTypeOpts, clientIdentifierOpts, globalFormErrorOpts) =>
      FastTrackErrors.apply(
        clientTypeOpts.getOrElse(Seq.empty).headOption,
        serviceOpts.getOrElse(Seq.empty).headOption,
        clientIdentifierOpts.getOrElse(Seq.empty).headOption,
        clientIdentifierTypeOpts.getOrElse(Seq.empty).headOption,
        globalFormErrorOpts.getOrElse(Seq.empty).headOption
      )
    )
}
