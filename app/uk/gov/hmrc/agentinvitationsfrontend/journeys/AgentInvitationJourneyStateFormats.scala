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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.States.{DeleteAuthorisationRequestPersonal, PendingInvitationExists, _}

object AgentInvitationJourneyStateFormats extends JsonStateFormats[State] {

  //Happy states
  val SelectClientTypeFormat = Json.format[SelectClientType]
  val SelectPersonalServiceFormat = Json.format[SelectPersonalService]
  val SelectBusinessServiceFormat = Json.format[SelectBusinessService]
  val IdentifyPersonalClientFormat = Json.format[IdentifyPersonalClient]
  val IdentifyBusinessClientFormat = Json.format[IdentifyBusinessClient]
  val ConfirmClientItsaFormat = Json.format[ConfirmClientItsa]
  val ConfirmClientIrvFormat = Json.format[ConfirmClientIrv]
  val ConfirmClientPersonalVatFormat = Json.format[ConfirmClientPersonalVat]
  val ConfirmClientBusinessVatFormat = Json.format[ConfirmClientBusinessVat]
  val ReviewAuthorisationsPersonalFormat = Json.format[ReviewAuthorisationsPersonal]
  val ReviewAuthorisationsBusinessFormat = Json.format[ReviewAuthorisationsBusiness]
  val DeleteAuthorisationRequestPersonalFormat = Json.format[DeleteAuthorisationRequestPersonal]
  val DeleteAuthorisationRequestBusinessFormat = Json.format[DeleteAuthorisationRequestBusiness]
  val InvitationSentPersonalFormat = Json.format[InvitationSentPersonal]
  val InvitationSentBusinessFormat = Json.format[InvitationSentBusiness]
  val PendingInvitationExistsFormat = Json.format[PendingInvitationExists]
  val ActiveRelationshipExistsFormat = Json.format[ActiveAuthorisationExists]

  //Unhappy states
  val KnownFactNotMatchedFormat = Json.format[KnownFactNotMatched]
  val SomeAuthorisationsFailedFormat = Json.format[SomeAuthorisationsFailed]
  val AllAuthorisationsFailedFormat = Json.format[AllAuthorisationsFailed]
  val ClientNotSignedUpFormat = Json.format[ClientNotSignedUp]

  override val serializeStateProperties: PartialFunction[State, JsValue] = {
    case s: SelectClientType                   => SelectClientTypeFormat.writes(s)
    case s: SelectPersonalService              => SelectPersonalServiceFormat.writes(s)
    case s: SelectBusinessService              => SelectBusinessServiceFormat.writes(s)
    case s: IdentifyPersonalClient             => IdentifyPersonalClientFormat.writes(s)
    case s: IdentifyBusinessClient             => IdentifyBusinessClientFormat.writes(s)
    case s: ConfirmClientItsa                  => ConfirmClientItsaFormat.writes(s)
    case s: ConfirmClientIrv                   => ConfirmClientIrvFormat.writes(s)
    case s: ConfirmClientPersonalVat           => ConfirmClientPersonalVatFormat.writes(s)
    case s: ConfirmClientBusinessVat           => ConfirmClientBusinessVatFormat.writes(s)
    case s: ReviewAuthorisationsPersonal       => ReviewAuthorisationsPersonalFormat.writes(s)
    case s: ReviewAuthorisationsBusiness       => ReviewAuthorisationsBusinessFormat.writes(s)
    case s: DeleteAuthorisationRequestPersonal => DeleteAuthorisationRequestPersonalFormat.writes(s)
    case s: DeleteAuthorisationRequestBusiness => DeleteAuthorisationRequestBusinessFormat.writes(s)
    case s: InvitationSentPersonal             => InvitationSentPersonalFormat.writes(s)
    case s: InvitationSentBusiness             => InvitationSentBusinessFormat.writes(s)
    case s: KnownFactNotMatched                => KnownFactNotMatchedFormat.writes(s)
    case s: SomeAuthorisationsFailed           => SomeAuthorisationsFailedFormat.writes(s)
    case s: AllAuthorisationsFailed            => AllAuthorisationsFailedFormat.writes(s)
    case s: ClientNotSignedUp                  => ClientNotSignedUpFormat.writes(s)
    case s: PendingInvitationExists            => PendingInvitationExistsFormat.writes(s)
    case s: ActiveAuthorisationExists          => ActiveRelationshipExistsFormat.writes(s)
  }

  override def deserializeState(stateName: String, properties: JsValue): JsResult[State] = stateName match {
    case "SelectClientType"                   => SelectClientTypeFormat.reads(properties)
    case "SelectPersonalService"              => SelectPersonalServiceFormat.reads(properties)
    case "SelectBusinessService"              => SelectBusinessServiceFormat.reads(properties)
    case "IdentifyPersonalClient"             => IdentifyPersonalClientFormat.reads(properties)
    case "IdentifyBusinessClient"             => IdentifyBusinessClientFormat.reads(properties)
    case "ConfirmClientItsa"                  => ConfirmClientItsaFormat.reads(properties)
    case "ConfirmClientIrv"                   => ConfirmClientIrvFormat.reads(properties)
    case "ConfirmClientPersonalVat"           => ConfirmClientPersonalVatFormat.reads(properties)
    case "ConfirmClientBusinessVat"           => ConfirmClientBusinessVatFormat.reads(properties)
    case "ReviewAuthorisationsPersonal"       => ReviewAuthorisationsPersonalFormat.reads(properties)
    case "ReviewAuthorisationsBusiness"       => ReviewAuthorisationsBusinessFormat.reads(properties)
    case "DeleteAuthorisationRequestPersonal" => DeleteAuthorisationRequestPersonalFormat.reads(properties)
    case "DeleteAuthorisationRequestBusiness" => DeleteAuthorisationRequestBusinessFormat.reads(properties)
    case "InvitationSentPersonal"             => InvitationSentPersonalFormat.reads(properties)
    case "InvitationSentBusiness"             => InvitationSentBusinessFormat.reads(properties)
    case "KnownFactNotMatched"                => KnownFactNotMatchedFormat.reads(properties)
    case "SomeAuthorisationsFailed"           => SomeAuthorisationsFailedFormat.reads(properties)
    case "AllAuthorisationsFailed"            => AllAuthorisationsFailedFormat.reads(properties)
    case "ClientNotSignedUp"                  => ClientNotSignedUpFormat.reads(properties)
    case "PendingInvitationExists"            => PendingInvitationExistsFormat.reads(properties)
    case "ActiveRelationshipExists"           => ActiveRelationshipExistsFormat.reads(properties)
    case _                                    => JsError(s"Unknown state name $stateName")
  }

}
