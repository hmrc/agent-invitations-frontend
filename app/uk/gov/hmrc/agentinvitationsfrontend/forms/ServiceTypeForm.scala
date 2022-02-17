/*
 * Copyright 2022 HM Revenue & Customs
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

import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.supportedServices
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators.normalizedText

object ServiceTypeForm {

  /** Multiple choice service selection form */
  val form: Form[String] =
    Form[String](
      single(
        "serviceType" -> optional(text)
          .verifying("service.type.invalid", serviceOpt => supportedServices.contains(serviceOpt.getOrElse("")))
          .transform(_.getOrElse(""), (Some(_)): String => Option[String])
      )
    )

  /** Single select - returns String to be compatible with the above form
    * empty string denotes selecting "No"
    * */
  def selectSingleServiceForm(service: String, clientType: ClientType): Form[String] = {

    val UNDEF = "undefined"

    def serviceValidator(errorMessageKey: String): Constraint[String] = Constraint[String] { service: String =>
      if (service.isEmpty || supportedServices.contains(service))
        Valid
      else
        Invalid(ValidationError(errorMessageKey))
    }

    Form[String](
      single(
        "accepted" -> optional(normalizedText)
          .transform[String](
            maybeChoice =>
              maybeChoice.fold(UNDEF) {
                case "true"  => service
                case "false" => ""
                case _       => UNDEF
            },
            service => if (service == UNDEF) None else Some(service))
          .verifying(serviceValidator(s"select-single-service.$service.$clientType.error"))
      ))
  }
}
