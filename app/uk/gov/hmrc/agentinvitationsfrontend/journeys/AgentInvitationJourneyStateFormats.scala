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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.State.{DeleteAuthorisationRequestPersonal, PendingInvitationExists, _}
import uk.gov.hmrc.play.fsm.JsonStateFormats

object AgentInvitationJourneyStateFormats extends JsonStateFormats[State] {

  //Happy states
  val SelectClientTypeFormat: OFormat[SelectClientType] = Json.format
  val SelectPersonalServiceFormat: OFormat[SelectPersonalService] = Json.format
  val SelectTrustServiceFormat: OFormat[SelectTrustService] = Json.format
  val IdentifyPersonalClientFormat: OFormat[IdentifyPersonalClient] = Json.format
  val IdentifyTrustClientFormat: OFormat[IdentifyTrustClient] = Json.format
  val ConfirmClientItsaFormat: OFormat[ConfirmClientItsa] = Json.format
  val ConfirmClientTrustFormat: OFormat[ConfirmClientTrust] = Json.format
  val ConfirmClientPersonalVatFormat: OFormat[ConfirmClientPersonalVat] = Json.format
  val ConfirmClientBusinessVatFormat: OFormat[ConfirmClientBusinessVat] = Json.format

  val ConfirmClientPersonalGctFormat: OFormat[ConfirmClientPersonalCgt] = Json.format
  val ConfirmClientTrustGctFormat: OFormat[ConfirmClientTrustCgt] = Json.format
  val InvalidCgtAccountReferenceFormat: OFormat[InvalidCgtAccountReference] = Json.format

  val ReviewAuthorisationsPersonalFormat: OFormat[ReviewAuthorisationsPersonal] = Json.format
  val ReviewAuthorisationsTrustFormat: OFormat[ReviewAuthorisationsTrust] = Json.format

  val DeleteAuthorisationRequestPersonalFormat: OFormat[DeleteAuthorisationRequestPersonal] = Json.format
  val InvitationSentPersonalFormat: OFormat[InvitationSentPersonal] = Json.format
  val InvitationSentBusinessFormat: OFormat[InvitationSentBusiness] = Json.format
  val PendingInvitationExistsFormat: OFormat[PendingInvitationExists] = Json.format
  val ActiveAuthorisationExistsFormat: OFormat[ActiveAuthorisationExists] = Json.format

  //Unhappy states
  val KnownFactNotMatchedFormat: OFormat[KnownFactNotMatched] = Json.format
  val CannotCreateRequestFormat: OFormat[CannotCreateRequest] = Json.format
  val SomeAuthorisationsFailedFormat: OFormat[SomeAuthorisationsFailed] = Json.format
  val AllAuthorisationsFailedFormat: OFormat[AllAuthorisationsFailed] = Json.format
  val ClientNotSignedUpFormat: OFormat[ClientNotSignedUp] = Json.format

  override val serializeStateProperties: PartialFunction[State, JsValue] = {
    case s: SelectClientType                   => SelectClientTypeFormat.writes(s)
    case s: SelectPersonalService              => SelectPersonalServiceFormat.writes(s)
    case s: SelectTrustService                 => SelectTrustServiceFormat.writes(s)
    case s: IdentifyPersonalClient             => IdentifyPersonalClientFormat.writes(s)
    case s: IdentifyTrustClient                => IdentifyTrustClientFormat.writes(s)
    case s: ConfirmClientItsa                  => ConfirmClientItsaFormat.writes(s)
    case s: ConfirmClientPersonalVat           => ConfirmClientPersonalVatFormat.writes(s)
    case s: ConfirmClientBusinessVat           => ConfirmClientBusinessVatFormat.writes(s)
    case s: ConfirmClientTrust                 => ConfirmClientTrustFormat.writes(s)
    case s: ConfirmClientTrustCgt              => ConfirmClientTrustGctFormat.writes(s)
    case s: ConfirmClientPersonalCgt           => ConfirmClientPersonalGctFormat.writes(s)
    case s: InvalidCgtAccountReference         => InvalidCgtAccountReferenceFormat.writes(s)
    case s: ReviewAuthorisationsPersonal       => ReviewAuthorisationsPersonalFormat.writes(s)
    case s: ReviewAuthorisationsTrust          => ReviewAuthorisationsTrustFormat.writes(s)
    case s: DeleteAuthorisationRequestPersonal => DeleteAuthorisationRequestPersonalFormat.writes(s)
    case s: InvitationSentPersonal             => InvitationSentPersonalFormat.writes(s)
    case s: InvitationSentBusiness             => InvitationSentBusinessFormat.writes(s)
    case s: KnownFactNotMatched                => KnownFactNotMatchedFormat.writes(s)
    case s: CannotCreateRequest                => CannotCreateRequestFormat.writes(s)
    case s: SomeAuthorisationsFailed           => SomeAuthorisationsFailedFormat.writes(s)
    case s: AllAuthorisationsFailed            => AllAuthorisationsFailedFormat.writes(s)
    case s: ClientNotSignedUp                  => ClientNotSignedUpFormat.writes(s)
    case s: PendingInvitationExists            => PendingInvitationExistsFormat.writes(s)
    case s: ActiveAuthorisationExists          => ActiveAuthorisationExistsFormat.writes(s)
  }

  override def deserializeState(stateName: String, properties: JsValue): JsResult[State] = stateName match {
    case "SelectClientType"                   => SelectClientTypeFormat.reads(properties)
    case "SelectPersonalService"              => SelectPersonalServiceFormat.reads(properties)
    case "SelectBusinessService"              => JsSuccess(SelectBusinessService)
    case "SelectTrustService"                 => SelectTrustServiceFormat.reads(properties)
    case "IdentifyPersonalClient"             => IdentifyPersonalClientFormat.reads(properties)
    case "IdentifyBusinessClient"             => JsSuccess(IdentifyBusinessClient)
    case "IdentifyTrustClient"                => IdentifyTrustClientFormat.reads(properties)
    case "ConfirmClientTrust"                 => ConfirmClientTrustFormat.reads(properties)
    case "ConfirmClientPersonalCgt"           => ConfirmClientPersonalGctFormat.reads(properties)
    case "ConfirmClientTrustCgt"              => ConfirmClientTrustGctFormat.reads(properties)
    case "InvalidCgtAccountReference"         => InvalidCgtAccountReferenceFormat.reads(properties)
    case "ConfirmClientItsa"                  => ConfirmClientItsaFormat.reads(properties)
    case "ConfirmClientPersonalVat"           => ConfirmClientPersonalVatFormat.reads(properties)
    case "ConfirmClientBusinessVat"           => ConfirmClientBusinessVatFormat.reads(properties)
    case "ReviewAuthorisationsPersonal"       => ReviewAuthorisationsPersonalFormat.reads(properties)
    case "ReviewAuthorisationsTrust"          => ReviewAuthorisationsTrustFormat.reads(properties)
    case "DeleteAuthorisationRequestPersonal" => DeleteAuthorisationRequestPersonalFormat.reads(properties)
    case "InvitationSentPersonal"             => InvitationSentPersonalFormat.reads(properties)
    case "InvitationSentBusiness"             => InvitationSentBusinessFormat.reads(properties)
    case "KnownFactNotMatched"                => KnownFactNotMatchedFormat.reads(properties)
    case "TrustNotFound"                      => JsSuccess(TrustNotFound)
    case "CgtRefNotFound"                     => JsSuccess(CgtRefNotFound)
    case "CannotCreateRequest"                => CannotCreateRequestFormat.reads(properties)
    case "SomeAuthorisationsFailed"           => SomeAuthorisationsFailedFormat.reads(properties)
    case "AllAuthorisationsFailed"            => AllAuthorisationsFailedFormat.reads(properties)
    case "ClientNotSignedUp"                  => ClientNotSignedUpFormat.reads(properties)
    case "PendingInvitationExists"            => PendingInvitationExistsFormat.reads(properties)
    case "ActiveAuthorisationExists"          => ActiveAuthorisationExistsFormat.reads(properties)
    case "AllAuthorisationsRemoved"           => JsSuccess(AllAuthorisationsRemoved)
    case _                                    => JsError(s"Unknown state name $stateName")
  }

}
