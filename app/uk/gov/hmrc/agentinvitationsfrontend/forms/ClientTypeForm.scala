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

package uk.gov.hmrc.agentinvitationsfrontend.forms

import play.api.data.Forms._
import play.api.data._
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators.lowerCaseText

object ClientTypeForm {
  def form(emptyErrorMsg: String, clientTypes: Set[ClientType]): Form[String] =
    Form[String](
      single(
        "clientType" -> optional(lowerCaseText)
          .verifying(emptyErrorMsg, _.fold(false)(ClientType.isValid))
          .transform(_.getOrElse(""), (Some(_)): String => Option[String])
      )
    )

  lazy val authorisationForm: Form[String] = form("error.client-type.empty", ClientType.clientTypes.toSet)

  lazy val deAuthorisationForm: Form[String] =
    form("error.cancel-authorisation.client-type.empty", ClientType.clientTypes.toSet)

  def fastTrackForm(clientTypes: Set[ClientType]): Form[String] = form("error.fast-track.client-type.empty", clientTypes)

}
