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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State._
import uk.gov.hmrc.play.fsm.JsonStateFormats

object ClientInvitationJourneyStateFormats extends JsonStateFormats[State] {

  //Happy states
  val WarmUpFormat = Json.format[WarmUp]
  val MultiConsentFormat = Json.format[MultiConsent]
  val SingleConsentFormat = Json.format[SingleConsent]
  val CheckAnswersFormat = Json.format[CheckAnswers]
  val InvitationsAcceptedFormat = Json.format[InvitationsAccepted]
  val InvitationsDeclinedFormat = Json.format[InvitationsDeclined]
  val SomeResponsesFailedFormat = Json.format[SomeResponsesFailed]
  val ConfirmDeclineFormat = Json.format[ConfirmDecline]
  val SuspendedAgentFormat = Json.format[SuspendedAgent]
  val ActionNeededFormat = Json.format[ActionNeeded]
  val AgentCancelledRequestFormat = Json.format[AgentCancelledRequest]
  val AlreadyRespondedToRequestFormat = Json.format[AlreadyRespondedToRequest]
  val RequestExpiredFormat = Json.format[RequestExpired]
  val CannotFindRequestFormat = Json.format[CannotFindRequest]
  val AuthorisationRequestExpiredFormat = Json.format[AuthorisationRequestExpired]
  val AuthorisationRequestCancelledFormat = Json.format[AuthorisationRequestCancelled]
  val AuthorisationRequestAlreadyRespondedFormat = Json.format[AuthorisationRequestAlreadyResponded]
  val GGUserIdNeededFormat = Json.format[GGUserIdNeeded]
  val WarmUpSessionRequiredFormat = Json.format[WarmUpSessionRequired]

  override val serializeStateProperties: PartialFunction[State, JsValue] = {
    case s: WarmUp                               => WarmUpFormat.writes(s)
    case s: MultiConsent                         => MultiConsentFormat.writes(s)
    case s: SingleConsent                        => SingleConsentFormat.writes(s)
    case s: CheckAnswers                         => CheckAnswersFormat.writes(s)
    case s: InvitationsAccepted                  => InvitationsAcceptedFormat.writes(s)
    case s: InvitationsDeclined                  => InvitationsDeclinedFormat.writes(s)
    case s: SomeResponsesFailed                  => SomeResponsesFailedFormat.writes(s)
    case s: ConfirmDecline                       => ConfirmDeclineFormat.writes(s)
    case s: SuspendedAgent                       => SuspendedAgentFormat.writes(s)
    case s: ActionNeeded                         => ActionNeededFormat.writes(s)
    case s: AgentCancelledRequest                => AgentCancelledRequestFormat.writes(s)
    case s: RequestExpired                       => RequestExpiredFormat.writes(s)
    case s: AlreadyRespondedToRequest            => AlreadyRespondedToRequestFormat.writes(s)
    case s: CannotFindRequest                    => CannotFindRequestFormat.writes(s)
    case s: AuthorisationRequestExpired          => AuthorisationRequestExpiredFormat.writes(s)
    case s: AuthorisationRequestCancelled        => AuthorisationRequestCancelledFormat.writes(s)
    case s: AuthorisationRequestAlreadyResponded => AuthorisationRequestAlreadyRespondedFormat.writes(s)
    case s: GGUserIdNeeded                       => GGUserIdNeededFormat.writes(s)
    case s: WarmUpSessionRequired                => WarmUpSessionRequiredFormat.writes(s)
  }

  override def deserializeState(stateName: String, properties: JsValue): JsResult[State] = stateName match {
    case "MissingJourneyHistory"                => JsSuccess(MissingJourneyHistory)
    case "WarmUp"                               => WarmUpFormat.reads(properties)
    case "NotFoundInvitation"                   => JsSuccess(NotFoundInvitation)
    case "NoOutstandingRequests"                => JsSuccess(NoOutstandingRequests)
    case "ActionNeeded"                         => ActionNeededFormat.reads(properties)
    case "AgentCancelledRequest"                => AgentCancelledRequestFormat.reads(properties)
    case "AlreadyRespondedToRequest"            => AlreadyRespondedToRequestFormat.reads(properties)
    case "CannotFindRequest"                    => CannotFindRequestFormat.reads(properties)
    case "RequestExpired"                       => RequestExpiredFormat.reads(properties)
    case "MultiConsent"                         => MultiConsentFormat.reads(properties)
    case "SingleConsent"                        => SingleConsentFormat.reads(properties)
    case "CheckAnswers"                         => CheckAnswersFormat.reads(properties)
    case "InvitationsAccepted"                  => InvitationsAcceptedFormat.reads(properties)
    case "InvitationsDeclined"                  => InvitationsDeclinedFormat.reads(properties)
    case "SomeResponsesFailed"                  => SomeResponsesFailedFormat.reads(properties)
    case "AllResponsesFailed"                   => JsSuccess(AllResponsesFailed)
    case "ConfirmDecline"                       => ConfirmDeclineFormat.reads(properties)
    case "TrustNotClaimed"                      => JsSuccess(TrustNotClaimed)
    case "SuspendedAgent"                       => SuspendedAgentFormat.reads(properties)
    case "AuthorisationRequestExpired"          => AuthorisationRequestExpiredFormat.reads(properties)
    case "AuthorisationRequestCancelled"        => AuthorisationRequestCancelledFormat.reads(properties)
    case "AuthorisationRequestAlreadyResponded" => AuthorisationRequestAlreadyRespondedFormat.reads(properties)
    case "GGUserIdNeeded"                       => GGUserIdNeededFormat.reads(properties)
    case "WarmUpSessionRequired"                => WarmUpSessionRequiredFormat.reads(properties)
  }

}
