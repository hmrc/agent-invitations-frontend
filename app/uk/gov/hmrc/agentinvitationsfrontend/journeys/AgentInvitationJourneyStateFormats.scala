/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel._
import uk.gov.hmrc.play.fsm.JsonStateFormats

object AgentInvitationJourneyStateFormats extends JsonStateFormats[State] {

  //Happy states
  val SelectClientTypeFormat: OFormat[SelectClientType] = Json.format
  val SelectPersonalServiceFormat: OFormat[SelectPersonalService] = Json.format
  val SelectBusinessServiceFormat: OFormat[SelectBusinessService] = Json.format
  val SelectTrustServiceFormat: OFormat[SelectTrustService] = Json.format
  val IdentifyClientFormat: OFormat[IdentifyClient] = Json.format
  val ConfirmClientItsaFormat: OFormat[ConfirmClientItsa] = Json.format
  val ConfirmClientTrustFormat: OFormat[ConfirmClientTrust] = Json.format
  val ConfirmClientPersonalVatFormat: OFormat[ConfirmClientPersonalVat] = Json.format
  val ConfirmClientBusinessVatFormat: OFormat[ConfirmClientBusinessVat] = Json.format

  val ConfirmClientCgtFormat: OFormat[ConfirmClientCgt] = Json.format
  val ConfirmClientPptFormat: OFormat[ConfirmClientPpt] = Json.format
  val ConfirmCgtPostcodeFormat: OFormat[ConfirmPostcodeCgt] = Json.format
  val ConfirmCgtCountryCodeFormat: OFormat[ConfirmCountryCodeCgt] = Json.format

  val ReviewAuthorisationsFormat: OFormat[ReviewAuthorisations] = Json.format

  val DeleteAuthorisationRequestFormat: OFormat[DeleteAuthorisationRequest] = Json.format
  val InvitationSentPersonalFormat: OFormat[InvitationSentPersonal] = Json.format
  val InvitationSentBusinessFormat: OFormat[InvitationSentBusiness] = Json.format
  val InvitationSentTrustFormat: OFormat[InvitationSentTrust] = Json.format
  val PendingInvitationExistsFormat: OFormat[PendingInvitationExists] = Json.format
  val ActiveAuthorisationExistsFormat: OFormat[ActiveAuthorisationExists] = Json.format
  val PartialAuthorisationExistsFormat: OFormat[PartialAuthorisationExists] = Json.format
  val LegacyAuthorisationDetectedFormat: OFormat[LegacyAuthorisationDetected] = Json.format

  //Unhappy states
  val KnownFactNotMatchedFormat: OFormat[KnownFactNotMatched] = Json.format
  val CannotCreateRequestFormat: OFormat[CannotCreateRequest] = Json.format
  val SomeAuthorisationsFailedFormat: OFormat[SomeAuthorisationsFailed] = Json.format
  val AllAuthorisationsFailedFormat: OFormat[AllAuthorisationsFailed] = Json.format
  val ClientNotSignedUpFormat: OFormat[ClientNotSignedUp] = Json.format

  val TrustNotFoundFormat: OFormat[TrustNotFound] = Json.format
  val CgtRefNotFoundFormat: OFormat[CgtRefNotFound] = Json.format
  val PptRefNotFoundFormat: OFormat[PptRefNotFound] = Json.format
  val AgentSuspendedFormat: OFormat[AgentSuspended] = Json.format
  val ClientNotRegisteredFormat: OFormat[ClientNotRegistered] = Json.format

  override val serializeStateProperties: PartialFunction[State, JsValue] = {
    case s: SelectClientType            => SelectClientTypeFormat.writes(s)
    case s: SelectPersonalService       => SelectPersonalServiceFormat.writes(s)
    case s: SelectBusinessService       => SelectBusinessServiceFormat.writes(s)
    case s: SelectTrustService          => SelectTrustServiceFormat.writes(s)
    case s: IdentifyClient              => IdentifyClientFormat.writes(s)
    case s: ConfirmClientItsa           => ConfirmClientItsaFormat.writes(s)
    case s: ConfirmClientPersonalVat    => ConfirmClientPersonalVatFormat.writes(s)
    case s: ConfirmClientBusinessVat    => ConfirmClientBusinessVatFormat.writes(s)
    case s: ConfirmClientTrust          => ConfirmClientTrustFormat.writes(s)
    case s: ConfirmClientCgt            => ConfirmClientCgtFormat.writes(s)
    case s: ConfirmClientPpt            => ConfirmClientPptFormat.writes(s)
    case s: ConfirmPostcodeCgt          => ConfirmCgtPostcodeFormat.writes(s)
    case s: ConfirmCountryCodeCgt       => ConfirmCgtCountryCodeFormat.writes(s)
    case s: CgtRefNotFound              => CgtRefNotFoundFormat.writes(s)
    case s: PptRefNotFound              => PptRefNotFoundFormat.writes(s)
    case s: TrustNotFound               => TrustNotFoundFormat.writes(s)
    case s: ReviewAuthorisations        => ReviewAuthorisationsFormat.writes(s)
    case s: DeleteAuthorisationRequest  => DeleteAuthorisationRequestFormat.writes(s)
    case s: InvitationSentPersonal      => InvitationSentPersonalFormat.writes(s)
    case s: InvitationSentBusiness      => InvitationSentBusinessFormat.writes(s)
    case s: InvitationSentTrust         => InvitationSentTrustFormat.writes(s)
    case s: KnownFactNotMatched         => KnownFactNotMatchedFormat.writes(s)
    case s: CannotCreateRequest         => CannotCreateRequestFormat.writes(s)
    case s: SomeAuthorisationsFailed    => SomeAuthorisationsFailedFormat.writes(s)
    case s: AllAuthorisationsFailed     => AllAuthorisationsFailedFormat.writes(s)
    case s: ClientNotSignedUp           => ClientNotSignedUpFormat.writes(s)
    case s: PendingInvitationExists     => PendingInvitationExistsFormat.writes(s)
    case s: ActiveAuthorisationExists   => ActiveAuthorisationExistsFormat.writes(s)
    case s: AgentSuspended              => AgentSuspendedFormat.writes(s)
    case s: PartialAuthorisationExists  => PartialAuthorisationExistsFormat.writes(s)
    case s: ClientNotRegistered         => ClientNotRegisteredFormat.writes(s)
    case s: LegacyAuthorisationDetected => LegacyAuthorisationDetectedFormat.writes(s)

  }

  override def deserializeState(stateName: String, properties: JsValue): JsResult[State] = stateName match {
    case "SelectClientType"            => SelectClientTypeFormat.reads(properties)
    case "SelectPersonalService"       => SelectPersonalServiceFormat.reads(properties)
    case "SelectBusinessService"       => SelectBusinessServiceFormat.reads(properties)
    case "SelectTrustService"          => SelectTrustServiceFormat.reads(properties)
    case "IdentifyClient"              => IdentifyClientFormat.reads(properties)
    case "ConfirmClientTrust"          => ConfirmClientTrustFormat.reads(properties)
    case "ConfirmClientCgt"            => ConfirmClientCgtFormat.reads(properties)
    case "ConfirmClientPpt"            => ConfirmClientPptFormat.reads(properties)
    case "ConfirmPostcodeCgt"          => ConfirmCgtPostcodeFormat.reads(properties)
    case "ConfirmCountryCodeCgt"       => ConfirmCgtCountryCodeFormat.reads(properties)
    case "ConfirmClientItsa"           => ConfirmClientItsaFormat.reads(properties)
    case "ConfirmClientPersonalVat"    => ConfirmClientPersonalVatFormat.reads(properties)
    case "ConfirmClientBusinessVat"    => ConfirmClientBusinessVatFormat.reads(properties)
    case "ReviewAuthorisations"        => ReviewAuthorisationsFormat.reads(properties)
    case "DeleteAuthorisationRequest"  => DeleteAuthorisationRequestFormat.reads(properties)
    case "InvitationSentPersonal"      => InvitationSentPersonalFormat.reads(properties)
    case "InvitationSentBusiness"      => InvitationSentBusinessFormat.reads(properties)
    case "InvitationSentTrust"         => InvitationSentTrustFormat.reads(properties)
    case "KnownFactNotMatched"         => KnownFactNotMatchedFormat.reads(properties)
    case "TrustNotFound"               => TrustNotFoundFormat.reads(properties)
    case "CgtRefNotFound"              => CgtRefNotFoundFormat.reads(properties)
    case "PptRefNotFound"              => PptRefNotFoundFormat.reads(properties)
    case "CannotCreateRequest"         => CannotCreateRequestFormat.reads(properties)
    case "SomeAuthorisationsFailed"    => SomeAuthorisationsFailedFormat.reads(properties)
    case "AllAuthorisationsFailed"     => AllAuthorisationsFailedFormat.reads(properties)
    case "ClientNotSignedUp"           => ClientNotSignedUpFormat.reads(properties)
    case "PendingInvitationExists"     => PendingInvitationExistsFormat.reads(properties)
    case "ActiveAuthorisationExists"   => ActiveAuthorisationExistsFormat.reads(properties)
    case "AllAuthorisationsRemoved"    => JsSuccess(AllAuthorisationsRemoved)
    case "AlreadyCopiedAcrossItsa"     => JsSuccess(AlreadyCopiedAcrossItsa)
    case "AgentSuspended"              => AgentSuspendedFormat.reads(properties)
    case "PartialAuthorisationExists"  => PartialAuthorisationExistsFormat.reads(properties)
    case "ClientNotRegistered"         => ClientNotRegisteredFormat.reads(properties)
    case "LegacyAuthorisationDetected" => LegacyAuthorisationDetectedFormat.reads(properties)
    case _                             => JsError(s"Unknown state name $stateName")
  }

}
