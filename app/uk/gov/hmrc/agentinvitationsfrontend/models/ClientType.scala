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

package uk.gov.hmrc.agentinvitationsfrontend.models
import play.api.libs.json.{Format, JsError, JsResult, JsString, JsSuccess, JsValue}

sealed trait ClientType {
  override def toString = ClientType.fromEnum(this)
}

object ClientType {

  case object Personal extends ClientType
  case object Business extends ClientType
  case object Trust extends ClientType

  def toEnum: String => ClientType = {
    case "personal" => Personal
    case "business" => Business
    case "trust"    => Trust
    case alien      => throw new Exception(s"Client type $alien not supported")
  }

  def fromEnum: ClientType => String = {
    case ClientType.Personal => "personal"
    case ClientType.Business => "business"
    case ClientType.Trust    => "trust"
  }

  implicit val formats: Format[ClientType] = new Format[ClientType] {
    def writes(o: ClientType): JsValue = JsString(fromEnum(o))

    def reads(json: JsValue): JsResult[ClientType] = json match {
      case JsString(name) => JsSuccess(toEnum(name))
      case o              => JsError(s"Cannot parse ClientType from $o, must be JsString.")
    }
  }

  def clientTypeFor(clientType: Option[ClientType], service: String): Option[ClientType] =
    clientType.orElse(service match {
      case "HMRC-MTD-IT"            => Some(ClientType.Personal)
      case "PERSONAL-INCOME-RECORD" => Some(ClientType.Personal)
      case "HMRC-TERS-ORG"          => Some(ClientType.Trust)
      case "HMRC-TERSNT-ORG"        => Some(ClientType.Trust)
      case _                        => None
    })
}
