/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators.lowerCaseText

object ClientTypeForm {
  def form(emptyErrorMsg: String, clientTypes: Set[String]): Form[String] =
    Form[String](
      single(
        "clientType" -> optional(lowerCaseText)
          .verifying(emptyErrorMsg, ct => clientTypes.contains(ct.getOrElse("")))
          .transform(_.getOrElse(""), (Some(_)): String => Option[String])
      )
    )

  val supportedClientTypes: Set[String] = Set("personal", "business", "trust")

  val supportedClientTypesFastTrack: Set[String] = Set("personal", "business")

  val supportedClientTypesForCgt: Set[String] = Set("personal", "trust")

  val supportedClientTypesForPpt: Set[String] = Set("personal", "trust")

  lazy val authorisationForm: Form[String] = form("error.client-type.empty", supportedClientTypes)

  lazy val deAuthorisationForm: Form[String] =
    form("error.cancel-authorisation.client-type.empty", supportedClientTypes)

  lazy val fastTrackForm: Form[String] = form("error.fast-track.client-type.empty", supportedClientTypesFastTrack)

  lazy val cgtClientTypeForm: Form[String] = form("error.fast-track.client-type.empty", supportedClientTypesForCgt)

  lazy val pptClientTypeForm: Form[String] = form("error.fast-track.client-type.empty", supportedClientTypesForPpt)
}
