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
  val SelectClientTypeFormat = Json.format[SelectClientType]
  val SelectPersonalServiceFormat = Json.format[SelectPersonalService]
  val IdentifyPersonalClientFormat = Json.format[IdentifyPersonalClient]
  val ConfirmClientItsaFormat = Json.format[ConfirmClientItsa]
  val ConfirmClientIrvFormat = Json.format[ConfirmClientIrv]
  val ConfirmClientTrustFormat = Json.format[ConfirmClientTrust]
  val ConfirmClientPersonalVatFormat = Json.format[ConfirmClientPersonalVat]
  val ConfirmClientBusinessVatFormat = Json.format[ConfirmClientBusinessVat]
  val ReviewAuthorisationsPersonalFormat = Json.format[ReviewAuthorisationsPersonal]
  val DeleteAuthorisationRequestPersonalFormat = Json.format[DeleteAuthorisationRequestPersonal]
  val InvitationSentPersonalFormat = Json.format[InvitationSentPersonal]
  val InvitationSentBusinessFormat = Json.format[InvitationSentBusiness]
  val PendingInvitationExistsFormat = Json.format[PendingInvitationExists]
  val ActiveAuthorisationExistsFormat = Json.format[ActiveAuthorisationExists]

  //Unhappy states
  val KnownFactNotMatchedFormat = Json.format[KnownFactNotMatched]
  val CannotCreateRequestFormat = Json.format[CannotCreateRequest]
  val SomeAuthorisationsFailedFormat = Json.format[SomeAuthorisationsFailed]
  val AllAuthorisationsFailedFormat = Json.format[AllAuthorisationsFailed]
  val ClientNotSignedUpFormat = Json.format[ClientNotSignedUp]

  override val serializeStateProperties: PartialFunction[State, JsValue] = {
    case s: SelectClientType                   => SelectClientTypeFormat.writes(s)
    case s: SelectPersonalService              => SelectPersonalServiceFormat.writes(s)
    case SelectTrustService                    => JsString(SelectTrustService.productPrefix)
    case s: IdentifyPersonalClient             => IdentifyPersonalClientFormat.writes(s)
    case IdentifyTrustClient                   => JsString(IdentifyTrustClient.productPrefix)
    case s: ConfirmClientItsa                  => ConfirmClientItsaFormat.writes(s)
    case s: ConfirmClientIrv                   => ConfirmClientIrvFormat.writes(s)
    case s: ConfirmClientPersonalVat           => ConfirmClientPersonalVatFormat.writes(s)
    case s: ConfirmClientBusinessVat           => ConfirmClientBusinessVatFormat.writes(s)
    case s: ConfirmClientTrust                 => ConfirmClientTrustFormat.writes(s)
    case s: ReviewAuthorisationsPersonal       => ReviewAuthorisationsPersonalFormat.writes(s)
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
    case "SelectTrustService"                 => JsSuccess(SelectTrustService)
    case "IdentifyPersonalClient"             => IdentifyPersonalClientFormat.reads(properties)
    case "IdentifyBusinessClient"             => JsSuccess(IdentifyBusinessClient)
    case "IdentifyTrustClient"                => JsSuccess(IdentifyTrustClient)
    case "ConfirmClientTrust"                 => ConfirmClientTrustFormat.reads(properties)
    case "ConfirmClientItsa"                  => ConfirmClientItsaFormat.reads(properties)
    case "ConfirmClientIrv"                   => ConfirmClientIrvFormat.reads(properties)
    case "ConfirmClientPersonalVat"           => ConfirmClientPersonalVatFormat.reads(properties)
    case "ConfirmClientBusinessVat"           => ConfirmClientBusinessVatFormat.reads(properties)
    case "ReviewAuthorisationsPersonal"       => ReviewAuthorisationsPersonalFormat.reads(properties)
    case "DeleteAuthorisationRequestPersonal" => DeleteAuthorisationRequestPersonalFormat.reads(properties)
    case "InvitationSentPersonal"             => InvitationSentPersonalFormat.reads(properties)
    case "InvitationSentBusiness"             => InvitationSentBusinessFormat.reads(properties)
    case "KnownFactNotMatched"                => KnownFactNotMatchedFormat.reads(properties)
    case "TrustNotFound"                      => JsSuccess(TrustNotFound)
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
