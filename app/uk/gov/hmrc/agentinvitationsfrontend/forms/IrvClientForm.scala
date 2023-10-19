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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.DateFieldHelper
import uk.gov.hmrc.agentinvitationsfrontend.models.IrvClient
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.domain.Nino

object IrvClientForm {

  def form: Form[IrvClient] = Form(
    mapping(
      "clientIdentifier" -> uppercaseNormalizedText.verifying(validNino).transform[Nino](Nino(_), _.value),
      "dob"              -> DateFieldHelper.dateFieldsMapping("irv-date-of-birth")
    )(IrvClient.apply)(IrvClient.unapply)
  )
}
