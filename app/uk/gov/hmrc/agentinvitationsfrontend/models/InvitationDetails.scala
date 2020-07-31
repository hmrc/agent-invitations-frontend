/*
 * Copyright 2020 HM Revenue & Customs
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

import org.joda.time.{DateTime, LocalDate}
import play.api.libs.json._
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId
import play.api.libs.json.JodaWrites._
import play.api.libs.json.JodaReads._

case class StatusChangeEvent(time: DateTime, status: InvitationStatus)

object StatusChangeEvent {
  implicit val statusChangeEventFormat = new Format[StatusChangeEvent] {
    override def reads(json: JsValue): JsResult[StatusChangeEvent] = {
      val time = new DateTime((json \ "time").as[Long])
      val status = InvitationStatus((json \ "status").as[String])
      JsSuccess(StatusChangeEvent(time, status))
    }

    override def writes(o: StatusChangeEvent): JsValue =
      Json.obj(
        "time"   -> o.time.getMillis,
        "status" -> o.status.toString
      )
  }
}

case class InvitationDetails(
  invitationId: InvitationId,
  expiryDate: LocalDate,
  status: InvitationStatus,
  isRelationshipEnded: Boolean,
  events: List[StatusChangeEvent]) {

  def firstEvent(): StatusChangeEvent =
    events.head

  def mostRecentEvent(): StatusChangeEvent =
    events.last

  def mostRecentStatus: InvitationStatus = mostRecentEvent().status
}

object InvitationDetails {
  implicit val format: Format[InvitationDetails] = Json.format[InvitationDetails]
}
