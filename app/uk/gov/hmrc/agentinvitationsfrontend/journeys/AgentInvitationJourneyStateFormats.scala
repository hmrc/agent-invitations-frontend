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
  val IdentifyPersonalClientFormat = Json.format[IdentifyPersonalClient]
  val IdentifyBusinessClientFormat = Json.format[IdentifyBusinessClient]
  val ItsaIdentifiedClientFormat = Json.format[ItsaIdentifiedClient]
  val VatIdentifiedPersonalClientFormat = Json.format[VatIdentifiedPersonalClient]
  val VatIdentifiedBusinessClientFormat = Json.format[VatIdentifiedBusinessClient]
  val IrvIdentifiedClientFormat = Json.format[IrvIdentifiedClient]
  val ConfirmClientItsaFormat = Json.format[ConfirmClientItsa]
  val ConfirmClientIrvFormat = Json.format[ConfirmClientIrv]
  val ConfirmClientPersonalVatFormat = Json.format[ConfirmClientPersonalVat]
  val ConfirmClientBusinessVatFormat = Json.format[ConfirmClientBusinessVat]
  val ClientConfirmedPersonalFormat = Json.format[ClientConfirmedPersonal]
  val ClientConfirmedBusinessFormat = Json.format[ClientConfirmedBusiness]
  val ReviewAuthorisationsPersonalFormat = Json.format[ReviewAuthorisationsPersonal]
  val ReviewAuthorisationsBusinessFormat = Json.format[ReviewAuthorisationsBusiness]
  val InvitationSentPersonalFormat = Json.format[InvitationSentPersonal]
  val InvitationSentBusinessFormat = Json.format[InvitationSentBusiness]

  override val serializeStateProperties: PartialFunction[State, JsValue] = {
    case s: ClientTypeSelected           => SelectedClientTypeFormat.writes(s)
    case s: SelectPersonalService        => SelectPersonalServiceFormat.writes(s)
    case s: PersonalServiceSelected      => SelectedPersonalServiceFormat.writes(s)
    case s: SelectBusinessService        => SelectBusinessServiceFormat.writes(s)
    case s: BusinessServiceSelected      => SelectedBusinessServiceFormat.writes(s)
    case s: IdentifyPersonalClient       => IdentifyPersonalClientFormat.writes(s)
    case s: IdentifyBusinessClient       => IdentifyBusinessClientFormat.writes(s)
    case s: ItsaIdentifiedClient         => ItsaIdentifiedClientFormat.writes(s)
    case s: VatIdentifiedPersonalClient  => VatIdentifiedPersonalClientFormat.writes(s)
    case s: VatIdentifiedBusinessClient  => VatIdentifiedBusinessClientFormat.writes(s)
    case s: IrvIdentifiedClient          => IrvIdentifiedClientFormat.writes(s)
    case s: ConfirmClientItsa            => ConfirmClientItsaFormat.writes(s)
    case s: ConfirmClientIrv             => ConfirmClientIrvFormat.writes(s)
    case s: ConfirmClientPersonalVat     => ConfirmClientPersonalVatFormat.writes(s)
    case s: ConfirmClientBusinessVat     => ConfirmClientBusinessVatFormat.writes(s)
    case s: ClientConfirmedPersonal      => ClientConfirmedPersonalFormat.writes(s)
    case s: ClientConfirmedBusiness      => ClientConfirmedBusinessFormat.writes(s)
    case s: ReviewAuthorisationsPersonal => ReviewAuthorisationsPersonalFormat.writes(s)
    case s: ReviewAuthorisationsBusiness => ReviewAuthorisationsBusinessFormat.writes(s)
    case s: InvitationSentPersonal       => InvitationSentPersonalFormat.writes(s)
    case s: InvitationSentBusiness       => InvitationSentBusinessFormat.writes(s)
  }

  override def deserializeState(stateName: String, properties: JsValue): JsResult[State] = stateName match {
    case "Start"                          => JsSuccess(Start)
    case "SelectClientType"               => JsSuccess(SelectClientType)
    case "ClientTypeSelected"             => SelectedClientTypeFormat.reads(properties)
    case "SelectPersonalService"          => SelectPersonalServiceFormat.reads(properties)
    case "PersonalServiceSelected"        => SelectedPersonalServiceFormat.reads(properties)
    case "SelectBusinessService"          => SelectBusinessServiceFormat.reads(properties)
    case "BusinessServiceSelected"        => SelectedBusinessServiceFormat.reads(properties)
    case "IdentifyPersonalClient"         => IdentifyPersonalClientFormat.reads(properties)
    case "IdentifyBusinessClient"         => IdentifyBusinessClientFormat.reads(properties)
    case "ItsaIdentifiedClient"           => ItsaIdentifiedClientFormat.reads(properties)
    case "VatIdentifiedPersonalClient"    => VatIdentifiedPersonalClientFormat.reads(properties)
    case "VatIdentifiedBusinessClient"    => VatIdentifiedBusinessClientFormat.reads(properties)
    case "IrvIdentifiedClient"            => IrvIdentifiedClientFormat.reads(properties)
    case "ConfirmClientItsa"              => ConfirmClientItsaFormat.reads(properties)
    case "ConfirmClientIrv"               => ConfirmClientIrvFormat.reads(properties)
    case "ConfirmClientPersonalVat"       => ConfirmClientPersonalVatFormat.reads(properties)
    case "ConfirmClientBusinessVat"       => ConfirmClientBusinessVatFormat.reads(properties)
    case "ClientConfirmedPersonal"        => ClientConfirmedPersonalFormat.reads(properties)
    case "ClientConfirmedBusiness"        => ClientConfirmedBusinessFormat.reads(properties)
    case "ReviewAuthorisationsPersonal"   => ReviewAuthorisationsPersonalFormat.reads(properties)
    case "ReviewAuthorisationsBusiness"   => ReviewAuthorisationsBusinessFormat.reads(properties)
    case "AuthorisationsReviewedPersonal" => JsSuccess(AuthorisationsReviewedPersonal)
    case "AuthorisationsReviewedBusiness" => JsSuccess(AuthorisationsReviewedBusiness)
    case "InvitationSentPersonal"         => InvitationSentPersonalFormat.reads(properties)
    case "InvitationSentBusiness"         => InvitationSentBusinessFormat.reads(properties)
    case _                                => JsError(s"Unknown state name $stateName")
  }

}
