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

import play.api.data.{Form, Mapping}
import play.api.data.Forms._
import play.api.data.validation._
import play.api.libs.json.Json
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.domain.Nino._


case class AgentInvitation(service: String,
                           clientIdType: String,
                           clientId: String,
                           clientPostcode: String)

object AgentInvitation {
  implicit val format = Json.format[AgentInvitation]
}

case class AgentInvitationUserInput(nino: Nino, postcode: String)

object AgentInvitationsForm {

  private def postcodeCheck(postcode: String) = postcode.matches("^[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}$|BFPO\\s?[0-9]{1,5}$")

  val agentInvitationNinoForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "nino" -> text
        .verifying("enter-nino.error-empty", _.nonEmpty)
        .verifying("enter-nino.invalid-format", nino => isValid(nino)),
      "postcode" -> text
    )({(nino, postcode) => AgentInvitationUserInput(Nino(nino), postcode)})({user => Some((user.nino.value, user.postcode))}))
  }

  val agentInvitationPostCodeForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "nino" -> text,
      "postcode" -> text
        .verifying("enter-postcode.error-empty", _.nonEmpty)
        .verifying("enter-postcode.invalid-format", postcode => postcodeCheck(postcode))
    )({(nino, postcode) => AgentInvitationUserInput(Nino(nino), postcode)})({user => Some((user.nino.value, user.postcode))}))
  }

}

