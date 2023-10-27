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

package uk.gov.hmrc.agentinvitationsfrontend.models
import play.api.libs.json.{Format, JsError, JsResult, JsString, JsSuccess, JsValue}
import uk.gov.hmrc.agentmtdidentifiers.model.Service

sealed trait ClientType {
  override def toString = ClientType.fromEnum(this)
}

object ClientType {

  case object Personal extends ClientType
  case object Business extends ClientType
  case object Trust extends ClientType

  val clientTypes: Seq[ClientType] = Seq(Personal, Business, Trust)

  def fromEnum: ClientType => String = {
    case ClientType.Personal => "personal"
    case ClientType.Business => "business"
    case ClientType.Trust    => "trust"
  }

  def toMaybeEnum(str: String): Option[ClientType] = clientTypes.find(fromEnum(_) == str)

  def toEnum(str: String): ClientType = toMaybeEnum(str).getOrElse(throw new Exception(s"Client type $str not supported"))

  def isValid(str: String): Boolean = toMaybeEnum(str).isDefined

  implicit val formats: Format[ClientType] = new Format[ClientType] {
    def writes(o: ClientType): JsValue = JsString(fromEnum(o))

    def reads(json: JsValue): JsResult[ClientType] = json match {
      case JsString(name) => JsSuccess(toEnum(name))
      case o              => JsError(s"Cannot parse ClientType from $o, must be JsString.")
    }
  }

  def clientTypeFor(clientType: Option[ClientType], service: Service): Option[ClientType] =
    clientType.orElse(service match {
      case Service.MtdIt                => Some(ClientType.Personal)
      case Service.PersonalIncomeRecord => Some(ClientType.Personal)
      case Service.Trust                => Some(ClientType.Trust)
      case Service.TrustNT              => Some(ClientType.Trust)
      case _                            => None
    })
}
