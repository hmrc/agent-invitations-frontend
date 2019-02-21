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

  val SelectedClientTypeFormat = Json.format[ClientTypeSelected]
  val SelectPersonalServiceFormat = Json.format[SelectPersonalService]
  val SelectedPersonalServiceFormat = Json.format[PersonalServiceSelected]
  val SelectBusinessServiceFormat = Json.format[SelectBusinessService]
  val SelectedBusinessServiceFormat = Json.format[BusinessServiceSelected]
  val IdentifyClientFormat = Json.format[IdentifyClient]
  val ItsaIdentifiedClientFormat = Json.format[ItsaIdentifiedClient]
  val VatIdentifiedClientFormat = Json.format[VatIdentifiedClient]
  val IrvIdentifiedClientFormat = Json.format[IrvIdentifiedClient]
  val ConfirmClientFormat = Json.format[ConfirmClient]
  val ClientConfirmedFormat = Json.format[ClientConfirmed]

  override val serializeStateProperties: PartialFunction[State, JsValue] = {
    case s: ClientTypeSelected      => SelectedClientTypeFormat.writes(s)
    case s: SelectPersonalService   => SelectPersonalServiceFormat.writes(s)
    case s: PersonalServiceSelected => SelectedPersonalServiceFormat.writes(s)
    case s: SelectBusinessService   => SelectBusinessServiceFormat.writes(s)
    case s: BusinessServiceSelected => SelectedBusinessServiceFormat.writes(s)
    case s: IdentifyClient          => IdentifyClientFormat.writes(s)
    case s: ItsaIdentifiedClient    => ItsaIdentifiedClientFormat.writes(s)
    case s: VatIdentifiedClient     => VatIdentifiedClientFormat.writes(s)
    case s: IrvIdentifiedClient     => IrvIdentifiedClientFormat.writes(s)
    case s: ConfirmClient           => ConfirmClientFormat.writes(s)
    case s: ClientConfirmed         => ClientConfirmedFormat.writes(s)
  }

  override def deserializeState(stateName: String, properties: JsValue): JsResult[State] = stateName match {
    case "Start"                   => JsSuccess(Start)
    case "SelectClientType"        => JsSuccess(SelectClientType)
    case "ClientTypeSelected"      => SelectedClientTypeFormat.reads(properties)
    case "SelectPersonalService"   => SelectPersonalServiceFormat.reads(properties)
    case "PersonalServiceSelected" => SelectedPersonalServiceFormat.reads(properties)
    case "SelectBusinessService"   => SelectBusinessServiceFormat.reads(properties)
    case "BusinessServiceSelected" => SelectedBusinessServiceFormat.reads(properties)
    case "IdentifyClient"          => IdentifyClientFormat.reads(properties)
    case "ItsaIdentifiedClient"    => ItsaIdentifiedClientFormat.reads(properties)
    case "VatIdentifiedClient"     => VatIdentifiedClientFormat.reads(properties)
    case "IrvIdentifiedClient"     => IrvIdentifiedClientFormat.reads(properties)
    case "ConfirmClient"           => ConfirmClientFormat.reads(properties)
    case "ClientConfirmed"         => ClientConfirmedFormat.reads(properties)
    case _                         => JsError(s"Unknown state name $stateName")
  }

}
