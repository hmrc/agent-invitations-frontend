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
import play.api.libs.json._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State._
import uk.gov.hmrc.play.fsm.JsonStateFormats

object AgentLedDeauthJourneyStateFormats extends JsonStateFormats[State] {

  val SelectServicePersonalFormats: OFormat[SelectServicePersonal] = Json.format
  val IdentifyClientPersonalFormats: OFormat[IdentifyClientPersonal] = Json.format
  val ConfirmClientsItsaFormats: OFormat[ConfirmClientItsa] = Json.format
  val ConfirmClientIrvFormats: OFormat[ConfirmClientIrv] = Json.format
  val ConfirmClientPersonalVatFormats: OFormat[ConfirmClientPersonalVat] = Json.format
  val ConfirmClientBusinessFormats: OFormat[ConfirmClientBusiness] = Json.format
  val NotSignedUpFormats: OFormat[NotSignedUp] = Json.format
  val ConfirmCancelFormats: OFormat[ConfirmCancel] = Json.format
  val NotAuthorisedFormats: OFormat[NotAuthorised] = Json.format
  val AuthorisationCancelledFormats: OFormat[AuthorisationCancelled] = Json.format

  override val serializeStateProperties: PartialFunction[State, JsValue] = {
    case s: SelectServicePersonal    => SelectServicePersonalFormats.writes(s)
    case s: IdentifyClientPersonal   => IdentifyClientPersonalFormats.writes(s)
    case s: ConfirmClientItsa        => ConfirmClientsItsaFormats.writes(s)
    case s: ConfirmClientIrv         => ConfirmClientIrvFormats.writes(s)
    case s: ConfirmClientPersonalVat => ConfirmClientPersonalVatFormats.writes(s)
    case s: ConfirmClientBusiness    => ConfirmClientBusinessFormats.writes(s)
    case s: NotSignedUp              => NotSignedUpFormats.writes(s)
    case s: ConfirmCancel            => ConfirmCancelFormats.writes(s)
    case s: NotAuthorised            => NotAuthorisedFormats.writes(s)
    case s: AuthorisationCancelled   => AuthorisationCancelledFormats.writes(s)

  }

  override def deserializeState(stateName: String, properties: JsValue): JsResult[State] = stateName match {
    case "SelectClientType"         => JsSuccess(SelectClientType)
    case "SelectServicePersonal"    => SelectServicePersonalFormats.reads(properties)
    case "SelectServiceBusiness"    => JsSuccess(SelectServiceBusiness)
    case "IdentifyClientPersonal"   => IdentifyClientPersonalFormats.reads(properties)
    case "IdentifyClientBusiness"   => JsSuccess(IdentifyClientBusiness)
    case "ConfirmClientItsa"        => ConfirmClientsItsaFormats.reads(properties)
    case "ConfirmClientIrv"         => ConfirmClientIrvFormats.reads(properties)
    case "ConfirmClientPersonalVat" => ConfirmClientPersonalVatFormats.reads(properties)
    case "ConfirmClientBusiness"    => ConfirmClientBusinessFormats.reads(properties)
    case "NotSignedUp"              => NotSignedUpFormats.reads(properties)
    case "KnownFactNotMatched"      => JsSuccess(KnownFactNotMatched)
    case "ConfirmCancel"            => ConfirmCancelFormats.reads(properties)
    case "NotAuthorised"            => NotAuthorisedFormats.reads(properties)
    case "AuthorisationCancelled"   => AuthorisationCancelledFormats.reads(properties)
    case "ResponseFailed"           => JsSuccess(ResponseFailed)
    case _                          => JsError(s"Unknown state name $stateName")
  }
}
