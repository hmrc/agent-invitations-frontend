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

import org.joda.time.DateTime
import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId}

import scala.collection.Seq

case class MultiAgentInvitation(clientType: String, invitationIds: Seq[InvitationId])

object MultiAgentInvitation {
  implicit val format = Json.format[MultiAgentInvitation]
}

case class MultiInvitationRecord(
  uid: String,
  arn: Arn,
  invitationIds: Seq[InvitationId],
  clientType: String,
  normalisedAgentName: String,
  createdDate: DateTime
)

object MultiInvitationRecord {

  import uk.gov.hmrc.http.controllers.RestFormats.dateTimeFormats

  implicit val formats: Format[MultiInvitationRecord] = Json.format[MultiInvitationRecord]
}
