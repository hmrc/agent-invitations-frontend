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

import play.api.libs.json._
import uk.gov.hmrc.play.bootstrap.binders.RedirectUrl._
import uk.gov.hmrc.play.bootstrap.binders.{RedirectUrl, UnsafePermitAll}

object RedirectUrlJsonFormat {

  private val redirectUrlWrites: Writes[RedirectUrl] =
    (__ \ "continueUrl").write[String].contramap(_.get(UnsafePermitAll).url)

  private val redirectUrlReads: Reads[RedirectUrl] =
    (__ \ "continueUrl").read[String].map(RedirectUrl.apply)

  implicit val redirectUrlFormat = Format[RedirectUrl](redirectUrlReads, redirectUrlWrites)
}
