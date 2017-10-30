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

package uk.gov.hmrc.agentinvitationsfrontend.form

import play.api.data.Form
import play.api.data.Forms._

case class PostCode(postcode: String)

object PostcodeForm {

  private def postcodeCheck(postcode: String) = postcode.matches("^[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}$|BFPO\\s?[0-9]{1,5}$")

  val postCodeForm: Form[PostCode] = {
    Form(mapping("postcode" -> text
        .verifying("enter-postcode.error-empty", _.nonEmpty)
        .verifying("enter-postcode.invalid-format", postcode => postcodeCheck(postcode))
    )(PostCode.apply)(PostCode.unapply))
  }
}
