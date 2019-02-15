/*
 * Copyright 2019 HM Revenue & Customs
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
import play.api.libs.json.Format

sealed trait ClientType

object ClientType {

  case object personal extends ClientType
  case object business extends ClientType

  def toEnum: String => ClientType = {
    case "personal" => personal
    case "business" => business
    case alien      => throw new Exception(s"Client type $alien not supported")
  }

  def fromEnum: ClientType => String = {
    case ClientType.personal => "personal"
    case ClientType.business => "business"
  }

  def toEnum: String => ClientType = {
    case "personal" => Personal
    case "business" => Business
    case alien      => throw new Exception(s"Client type $alien not supported")
  }

  def fromEnum: ClientType => String = {
    case Personal => "personal"
    case Business => "business"
  }

  implicit val formats: Format[ClientType] = new EnumFormats[ClientType] {
    override val deserialize: String => ClientType = toEnum
  }.formats
}
