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
import play.api.libs.json._

trait EnumFormats[E] {

  val deserialize: String => E

  final val reads: Reads[E] = new Reads[E] {
    override def reads(json: JsValue): JsResult[E] = json match {
      case JsString(name) => JsSuccess(deserialize(name))
      case o              => JsError(s"Cannot parse Enum from $o, must be JsString.")
    }
  }

  final val writes: Writes[E] = new Writes[E] {
    override def writes(enum: E): JsValue = JsString(nameOf(enum))
  }

  final def formats: Format[E] = Format(reads, writes)

  final def nameOf(enum: E): String = {
    val className = enum.getClass.getName
    val lastDot = className.lastIndexOf('.')
    val typeName = {
      val s = if (lastDot < 0) className else className.substring(lastDot + 1)
      if (s.last == '$') s.init else s
    }
    val lastDollar = typeName.lastIndexOf('$')
    if (lastDollar < 0) typeName else typeName.substring(lastDollar + 1)
  }

}
