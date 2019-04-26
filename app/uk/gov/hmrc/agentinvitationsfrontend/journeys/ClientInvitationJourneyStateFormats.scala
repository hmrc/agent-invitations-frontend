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
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State._
import uk.gov.hmrc.play.fsm.JsonStateFormats

object ClientInvitationJourneyStateFormats extends JsonStateFormats[State] {

  //Happy states
  val WarmUpFormat = Json.format[WarmUp]
  val ConsentFormat = Json.format[MultiConsent]
  val IncorrectClientTypeFormat = Json.format[IncorrectClientType]
  val MultiConsentFormat = Json.format[MultiConsent]
  val SingleConsentFormat = Json.format[SingleConsent]
  val CheckAnswersFormat = Json.format[CheckAnswers]
  val InvitationsAcceptedFormat = Json.format[InvitationsAccepted]
  val InvitationsDeclinedFormat = Json.format[InvitationsDeclined]
  val SomeResponsesFailedFormat = Json.format[SomeResponsesFailed]
  val ConfirmDeclineFormat = Json.format[ConfirmDecline]

  override val serializeStateProperties: PartialFunction[State, JsValue] = {
    case s: WarmUp              => WarmUpFormat.writes(s)
    case s: MultiConsent        => ConsentFormat.writes(s)
    case s: IncorrectClientType => IncorrectClientTypeFormat.writes(s)
    case s: MultiConsent        => MultiConsentFormat.writes(s)
    case s: SingleConsent       => SingleConsentFormat.writes(s)
    case s: CheckAnswers        => CheckAnswersFormat.writes(s)
    case s: InvitationsAccepted => InvitationsAcceptedFormat.writes(s)
    case s: InvitationsDeclined => InvitationsDeclinedFormat.writes(s)
    case s: SomeResponsesFailed => SomeResponsesFailedFormat.writes(s)
    case s: ConfirmDecline      => ConfirmDeclineFormat.writes(s)
  }

  override def deserializeState(stateName: String, properties: JsValue): JsResult[State] = stateName match {
    case "Root"                => JsSuccess(Root)
    case "WarmUp"              => WarmUpFormat.reads(properties)
    case "NotFoundInvitation"  => JsSuccess(NotFoundInvitation)
    case "Consent"             => ConsentFormat.reads(properties)
    case "IncorrectClientType" => IncorrectClientTypeFormat.reads(properties)
    case "MultiConsent"        => MultiConsentFormat.reads(properties)
    case "SingleConsent"       => SingleConsentFormat.reads(properties)
    case "CheckAnswers"        => CheckAnswersFormat.reads(properties)
    case "InvitationsAccepted" => InvitationsAcceptedFormat.reads(properties)
    case "InvitationsDeclined" => InvitationsDeclinedFormat.reads(properties)
    case "SomeResponsesFailed" => SomeResponsesFailedFormat.reads(properties)
    case "AllResponsesFailed"  => JsSuccess(AllResponsesFailed)
    case "ConfirmDecline"      => ConfirmDeclineFormat.reads(properties)
  }

}
