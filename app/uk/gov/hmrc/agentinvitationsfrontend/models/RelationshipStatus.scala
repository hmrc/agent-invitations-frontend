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

import play.api.libs.json._

sealed trait RelationshipStatus {
  val key: String
}

object RelationshipStatus {
  case object Active extends RelationshipStatus { val key = "ACTIVE" }
  case object Terminated extends RelationshipStatus { val key = "TERMINATED" }

  private implicit val statusWrites =  new Writes[RelationshipStatus] {
    override def writes(status: RelationshipStatus): JsValue = status match {
      case Active => JsString(Active.key)
      case Terminated => JsString(Terminated.key)
      case _ => throw new RuntimeException(s"Unable to parse the status to json: $status")
    }
  }

  private implicit val  statusReads = new Reads[RelationshipStatus] {
    override def reads(json: JsValue): JsResult[RelationshipStatus] = json match {
      case JsString(Active.key) => JsSuccess(Active)
      case JsString(Terminated.key) => JsSuccess(Terminated)
      case _ => throw new RuntimeException(s"Unable to parse the json to status: $json")
    }
  }

  implicit val relationshipStatusFormat = Format(statusReads, statusWrites)
}
