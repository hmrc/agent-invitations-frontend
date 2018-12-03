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

import play.api.libs.json.{Json, OFormat}

case class ClientDetail(clientName: String, service: String, clientId: String)

object ClientDetail {
  implicit val format: OFormat[ClientDetail] = Json.format[ClientDetail]

}

case class AuthorisationRequest(clientType: String, clientDetails: Seq[ClientDetail])

object AuthorisationRequest {
  implicit val format: OFormat[AuthorisationRequest] = Json.format[AuthorisationRequest]
}
