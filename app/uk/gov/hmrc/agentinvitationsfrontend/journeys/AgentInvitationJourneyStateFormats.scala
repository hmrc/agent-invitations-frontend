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

package uk.gov.hmrc.agentinvitationsfrontend.journeys
import play.api.libs.json.{Json, _}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.State
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.States._

object AgentInvitationJourneyStateFormats extends JsonStateFormats[State] {

  val selectedClientType = Json.format[SelectedClientType]

  override val serializeStateProperties: PartialFunction[State, JsValue] = {
    case s: SelectedClientType => selectedClientType.writes(s)
  }

  override def deserializeState(stateName: String, properties: JsValue): JsResult[State] = stateName match {
    case "Start"                 => JsSuccess(Start)
    case "SelectClientType"      => JsSuccess(SelectClientType)
    case "SelectedClientType"    => selectedClientType.reads(properties)
    case "SelectPersonalService" => JsSuccess(SelectPersonalService)
    case "SelectBusinessService" => JsSuccess(SelectBusinessService)
    case _                       => JsError(s"Unknown state name $stateName")
  }

}
