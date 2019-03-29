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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.State
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel.State._
import uk.gov.hmrc.play.fsm.JsonStateFormats

object AgentInvitationFastTrackJourneyStateFormats extends JsonStateFormats[State] {

  //Happy states
  val PrologueFormat = Json.format[Prologue]
  val CheckDetailsCompleteItsaFormat = Json.format[CheckDetailsCompleteItsa]
  val CheckDetailsCompleteIrvFormat = Json.format[CheckDetailsCompleteIrv]
  val CheckDetailsCompletePersonalVatFormat = Json.format[CheckDetailsCompletePersonalVat]
  val CheckDetailsCompleteBusinessVatFormat = Json.format[CheckDetailsCompleteBusinessVat]
  val CheckDetailsNoPostcodeFormat = Json.format[CheckDetailsNoPostcode]
  val CheckDetailsNoDobFormat = Json.format[CheckDetailsNoDob]
  val CheckDetailsNoVatRegDateFormat = Json.format[CheckDetailsNoVatRegDate]
  val CheckDetailsNoClientTypeVatFormat = Json.format[CheckDetailsNoClientTypeVat]
  val NoPostcodeFormat = Json.format[NoPostcode]
  val NoDobFormat = Json.format[NoDob]
  val NoVatRegDateFormat = Json.format[NoVatRegDate]
  val SelectClientTypeVatFormat = Json.format[SelectClientTypeVat]
  val IdentifyPersonalClientFormat = Json.format[IdentifyPersonalClient]
  val IdentifyBusinessClientFormat = Json.format[IdentifyBusinessClient]
  val InvitationSentPersonalFormat = Json.format[InvitationSentPersonal]
  val InvitationSentBusinessFormat = Json.format[InvitationSentBusiness]
  val PendingInvitationExistsFormat = Json.format[PendingInvitationExists]
  val ActiveAuthorisationExistsFormat = Json.format[ActiveAuthorisationExists]

  //Unhappy states
  val KnownFactNotMatchedFormat = Json.format[KnownFactNotMatched]
  val ClientNotSignedUpFormat = Json.format[ClientNotSignedUp]

  override val serializeStateProperties: PartialFunction[State, JsValue] = {
    case s: Prologue                        => PrologueFormat.writes(s)
    case s: CheckDetailsCompleteItsa        => CheckDetailsCompleteItsaFormat.writes(s)
    case s: CheckDetailsCompleteIrv         => CheckDetailsCompleteIrvFormat.writes(s)
    case s: CheckDetailsCompletePersonalVat => CheckDetailsCompletePersonalVatFormat.writes(s)
    case s: CheckDetailsCompleteBusinessVat => CheckDetailsCompleteBusinessVatFormat.writes(s)
    case s: CheckDetailsNoPostcode          => CheckDetailsNoPostcodeFormat.writes(s)
    case s: CheckDetailsNoDob               => CheckDetailsNoDobFormat.writes(s)
    case s: CheckDetailsNoVatRegDate        => CheckDetailsNoVatRegDateFormat.writes(s)
    case s: CheckDetailsNoClientTypeVat     => CheckDetailsNoClientTypeVatFormat.writes(s)
    case s: NoPostcode                      => NoPostcodeFormat.writes(s)
    case s: NoDob                           => NoDobFormat.writes(s)
    case s: NoVatRegDate                    => NoVatRegDateFormat.writes(s)
    case s: SelectClientTypeVat             => SelectClientTypeVatFormat.writes(s)
    case s: IdentifyPersonalClient          => IdentifyPersonalClientFormat.writes(s)
    case s: IdentifyBusinessClient          => IdentifyBusinessClientFormat.writes(s)
    case s: InvitationSentPersonal          => InvitationSentPersonalFormat.writes(s)
    case s: InvitationSentBusiness          => InvitationSentBusinessFormat.writes(s)
    case s: KnownFactNotMatched             => KnownFactNotMatchedFormat.writes(s)
    case s: ClientNotSignedUp               => ClientNotSignedUpFormat.writes(s)
    case s: PendingInvitationExists         => PendingInvitationExistsFormat.writes(s)
    case s: ActiveAuthorisationExists       => ActiveAuthorisationExistsFormat.writes(s)
  }

  override def deserializeState(stateName: String, properties: JsValue): JsResult[State] = stateName match {
    case "Prologue"                        => PrologueFormat.reads(properties)
    case "CheckDetailsCompleteItsa"        => CheckDetailsCompleteItsaFormat.reads(properties)
    case "CheckDetailsCompleteIrv"         => CheckDetailsCompleteIrvFormat.reads(properties)
    case "CheckDetailsCompletePersonalVat" => CheckDetailsCompletePersonalVatFormat.reads(properties)
    case "CheckDetailsCompleteBusinessVat" => CheckDetailsCompleteBusinessVatFormat.reads(properties)
    case "CheckDetailsNoPostcode"          => CheckDetailsNoPostcodeFormat.reads(properties)
    case "CheckDetailsNoDob"               => CheckDetailsNoDobFormat.reads(properties)
    case "CheckDetailsNoVatRegDate"        => CheckDetailsNoVatRegDateFormat.reads(properties)
    case "CheckDetailsNoClientTypeVat"     => CheckDetailsNoClientTypeVatFormat.reads(properties)
    case "NoPostcode"                      => NoPostcodeFormat.reads(properties)
    case "NoDob"                           => NoDobFormat.reads(properties)
    case "NoVatRegDate"                    => NoVatRegDateFormat.reads(properties)
    case "SelectClientTypeVat"             => SelectClientTypeVatFormat.reads(properties)
    case "IdentifyPersonalClient"          => IdentifyPersonalClientFormat.reads(properties)
    case "IdentifyBusinessClient"          => IdentifyBusinessClientFormat.reads(properties)
    case "InvitationSentPersonal"          => InvitationSentPersonalFormat.reads(properties)
    case "InvitationSentBusiness"          => InvitationSentBusinessFormat.reads(properties)
    case "KnownFactNotMatched"             => KnownFactNotMatchedFormat.reads(properties)
    case "ClientNotSignedUp"               => ClientNotSignedUpFormat.reads(properties)
    case "PendingInvitationExists"         => PendingInvitationExistsFormat.reads(properties)
    case "ActiveAuthorisationExists"       => ActiveAuthorisationExistsFormat.reads(properties)
    case _                                 => JsError(s"Unknown state name $stateName")
  }

}
