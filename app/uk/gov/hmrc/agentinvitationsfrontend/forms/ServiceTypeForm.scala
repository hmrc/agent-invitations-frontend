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

import play.api.data.Form
import play.api.data.Forms._
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.supportedServices
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators.normalizedText
import uk.gov.hmrc.agentmtdidentifiers.model.Service

object ServiceTypeForm {

  /** Multiple choice service selection form */
  val form: Form[Service] =
    Form[Service](
      single(
        "serviceType" -> normalizedText
          .verifying("service.type.invalid", serviceId => supportedServices.exists(_.id == serviceId))
          .transform[Service](Service.forId, _.id)
      )
    )

  /** Single select - returns String to be compatible with the above form
    * empty string denotes selecting "No"
    * */
  def selectSingleServiceForm(service: Service, clientType: ClientType): Form[Option[Service]] =
    Form[Option[Service]](
      single(
        "accepted" -> normalizedText
          .verifying(s"select-single-service.${service.id}.$clientType.error", List("true", "false").contains(_))
          .transform[Option[Service]]({
            case "true"  => Some(service)
            case "false" => None
          }, _.fold("")(_.id))
      ))
}
