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

package uk.gov.hmrc.agentinvitationsfrontend.journeys

import play.api.libs.json.{Json, _}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationFastTrackJourneyModel._
import uk.gov.hmrc.play.fsm.JsonStateFormats

object AgentInvitationFastTrackJourneyStateFormats extends JsonStateFormats[State] {

  //Happy states
  val PrologueFormat = Json.format[Prologue]
  val CheckDetailsCompleteFormat = Json.format[CheckDetailsComplete]
  val CheckDetailsNoPostcodeFormat = Json.format[CheckDetailsNoPostcode]
  val CheckDetailsNoDobFormat = Json.format[CheckDetailsNoDob]
  val CheckDetailsNoVatRegDateFormat = Json.format[CheckDetailsNoVatRegDate]
  val CheckDetailsNoClientTypeVatFormat = Json.format[CheckDetailsNoClientTypeVat]
  val NoPostcodeFormat = Json.format[NoPostcode]
  val NoDobFormat = Json.format[NoDob]
  val NoVatRegDateFormat = Json.format[NoVatRegDate]
  val SelectClientTypeFormat = Json.format[SelectClientType]
  val IdentifyClientFormat = Json.format[IdentifyClient]
  val ConfirmClientTrustFormat = Json.format[ConfirmClientTrust]
  val InvitationSentFormat = Json.format[InvitationSent]
  val PendingInvitationExistsFormat = Json.format[PendingInvitationExists]
  val ActiveAuthorisationExistsFormat = Json.format[ActiveAuthorisationExists]
  val PartialAuthorisationExistsFormat = Json.format[PartialAuthorisationExists]
  val ClientNotRegisteredFormat = Json.format[ClientNotRegistered]
  val LegacyAuthorisationDetectedFormat = Json.format[LegacyAuthorisationDetected]

  //Unhappy states
  val ClientNotFoundFormat = Json.format[ClientNotFound]
  val ClientNotSignedUpFormat = Json.format[ClientNotSignedUp]
  val SuspendedAgentFormat = Json.format[SuspendedAgent]

  val ConfirmClientCgtFormat: OFormat[ConfirmClientCgt] = Json.format
  val ConfirmClientPptFormat: OFormat[ConfirmClientPpt] = Json.format
  val ConfirmCgtPostcodeFormat: OFormat[ConfirmPostcodeCgt] = Json.format
  val ConfirmCgtCountryCodeFormat: OFormat[ConfirmCountryCodeCgt] = Json.format
  val ConfirmPptRegDateFormat: OFormat[ConfirmRegDatePpt] = Json.format

  override val serializeStateProperties: PartialFunction[State, JsValue] = {
    case s: Prologue                    => PrologueFormat.writes(s)
    case s: CheckDetailsComplete        => CheckDetailsCompleteFormat.writes(s)
    case s: CheckDetailsNoPostcode      => CheckDetailsNoPostcodeFormat.writes(s)
    case s: CheckDetailsNoDob           => CheckDetailsNoDobFormat.writes(s)
    case s: CheckDetailsNoVatRegDate    => CheckDetailsNoVatRegDateFormat.writes(s)
    case s: CheckDetailsNoClientTypeVat => CheckDetailsNoClientTypeVatFormat.writes(s)
    case s: NoPostcode                  => NoPostcodeFormat.writes(s)
    case s: NoDob                       => NoDobFormat.writes(s)
    case s: NoVatRegDate                => NoVatRegDateFormat.writes(s)
    case s: SelectClientType            => SelectClientTypeFormat.writes(s)
    case s: IdentifyClient              => IdentifyClientFormat.writes(s)
    case s: ConfirmClientTrust          => ConfirmClientTrustFormat.writes(s)
    case s: InvitationSent              => InvitationSentFormat.writes(s)
    case s: ClientNotFound              => ClientNotFoundFormat.writes(s)
    case s: ClientNotSignedUp           => ClientNotSignedUpFormat.writes(s)
    case s: PendingInvitationExists     => PendingInvitationExistsFormat.writes(s)
    case s: ActiveAuthorisationExists   => ActiveAuthorisationExistsFormat.writes(s)
    case s: ConfirmClientCgt            => ConfirmClientCgtFormat.writes(s)
    case s: ConfirmClientPpt            => ConfirmClientPptFormat.writes(s)
    case s: ConfirmPostcodeCgt          => ConfirmCgtPostcodeFormat.writes(s)
    case s: ConfirmCountryCodeCgt       => ConfirmCgtCountryCodeFormat.writes(s)
    case s: ConfirmRegDatePpt           => ConfirmPptRegDateFormat.writes(s)
    case s: SuspendedAgent              => SuspendedAgentFormat.writes(s)
    case s: PartialAuthorisationExists  => PartialAuthorisationExistsFormat.writes(s)
    case s: ClientNotRegistered         => ClientNotRegisteredFormat.writes(s)
    case s: LegacyAuthorisationDetected => LegacyAuthorisationDetectedFormat.writes(s)
  }

  override def deserializeState(stateName: String, properties: JsValue): JsResult[State] = stateName match {
    case "Prologue"                     => PrologueFormat.reads(properties)
    case "CheckDetailsComplete"         => CheckDetailsCompleteFormat.reads(properties)
    case "CheckDetailsNoPostcode"       => CheckDetailsNoPostcodeFormat.reads(properties)
    case "CheckDetailsNoDob"            => CheckDetailsNoDobFormat.reads(properties)
    case "CheckDetailsNoVatRegDate"     => CheckDetailsNoVatRegDateFormat.reads(properties)
    case "CheckDetailsNoClientTypeVat"  => CheckDetailsNoClientTypeVatFormat.reads(properties)
    case "NoPostcode"                   => NoPostcodeFormat.reads(properties)
    case "NoDob"                        => NoDobFormat.reads(properties)
    case "NoVatRegDate"                 => NoVatRegDateFormat.reads(properties)
    case "SelectClientType"             => SelectClientTypeFormat.reads(properties)
    case "IdentifyClient"               => IdentifyClientFormat.reads(properties)
    case "InvitationSent"               => InvitationSentFormat.reads(properties)
    case "ConfirmClientTrust"           => ConfirmClientTrustFormat.reads(properties)
    case "ClientNotFound"               => ClientNotFoundFormat.reads(properties)
    case "TryAgainWithoutFastTrack"     => JsSuccess(TryAgainWithoutFastTrack)
    case "ClientNotSignedUp"            => ClientNotSignedUpFormat.reads(properties)
    case "PendingInvitationExists"      => PendingInvitationExistsFormat.reads(properties)
    case "ActiveAuthorisationExists"    => ActiveAuthorisationExistsFormat.reads(properties)
    case "ConfirmClientCgt"             => ConfirmClientCgtFormat.reads(properties)
    case "ConfirmClientPpt"             => ConfirmClientPptFormat.reads(properties)
    case "ConfirmPostcodeCgt"           => ConfirmCgtPostcodeFormat.reads(properties)
    case "ConfirmCountryCodeCgt"        => ConfirmCgtCountryCodeFormat.reads(properties)
    case "ConfirmRegDatePpt"            => ConfirmPptRegDateFormat.reads(properties)
    case "SuspendedAgent"               => SuspendedAgentFormat.reads(properties)
    case "PartialAuthorisationExists"   => PartialAuthorisationExistsFormat.reads(properties)
    case "AlreadyCopiedAcrossItsa"      => JsSuccess(AlreadyCopiedAcrossItsa)
    case "ClientNotRegistered"          => ClientNotRegisteredFormat.reads(properties)
    case "LegacyAuthorisationDetected"  => LegacyAuthorisationDetectedFormat.reads(properties)
    case "ClientInsolventFastTrack"     => JsSuccess(ClientInsolventFastTrack)
    case "CannotCreateFastTrackRequest" => JsSuccess(CannotCreateFastTrackRequest)
    case _                              => JsError(s"Unknown state name $stateName")
  }

}
