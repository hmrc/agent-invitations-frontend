/*
 * Copyright 2022 HM Revenue & Customs
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
import uk.gov.hmrc.agentinvitationsfrontend.models.TaxIdFormat._

object AgentLedDeauthJourneyStateFormats extends JsonStateFormats[State] {

  val SelectServiceFormats: OFormat[SelectService] = Json.format
  val IdentifyClientFormats: OFormat[IdentifyClient] = Json.format
  val ConfirmClientFormats: OFormat[ConfirmClient] = Json.format
  val ConfirmPostcodeCgtFormats: OFormat[ConfirmPostcodeCgt] = Json.format
  val ConfirmCountryCodeCgtFormats: OFormat[ConfirmCountryCodeCgt] = Json.format
  val NotSignedUpFormats: OFormat[NotSignedUp] = Json.format
  val ConfirmCancelFormats: OFormat[ConfirmCancel] = Json.format
  val NotAuthorisedFormats: OFormat[NotAuthorised] = Json.format
  val AuthorisationCancelledFormats: OFormat[AuthorisationCancelled] = Json.format
  val ResponseFailedFormats: OFormat[ResponseFailed] = Json.format
  val CgtRefNotFoundFormats: OFormat[CgtRefNotFound] = Json.format
  val PptRefNotFoundFormats: OFormat[PptRefNotFound] = Json.format

  override val serializeStateProperties: PartialFunction[State, JsValue] = {
    case s: SelectService          => SelectServiceFormats.writes(s)
    case s: IdentifyClient         => IdentifyClientFormats.writes(s)
    case s: ConfirmPostcodeCgt     => ConfirmPostcodeCgtFormats.writes(s)
    case s: ConfirmCountryCodeCgt  => ConfirmCountryCodeCgtFormats.writes(s)
    case s: ConfirmClient          => ConfirmClientFormats.writes(s)
    case s: CgtRefNotFound         => CgtRefNotFoundFormats.writes(s)
    case s: PptRefNotFound         => PptRefNotFoundFormats.writes(s)
    case s: NotSignedUp            => NotSignedUpFormats.writes(s)
    case s: ConfirmCancel          => ConfirmCancelFormats.writes(s)
    case s: NotAuthorised          => NotAuthorisedFormats.writes(s)
    case s: AuthorisationCancelled => AuthorisationCancelledFormats.writes(s)
    case s: ResponseFailed         => ResponseFailedFormats.writes(s)

  }

  override def deserializeState(stateName: String, properties: JsValue): JsResult[State] = stateName match {
    case "SelectClientType"       => JsSuccess(SelectClientType)
    case "SelectService"          => SelectServiceFormats.reads(properties)
    case "IdentifyClient"         => IdentifyClientFormats.reads(properties)
    case "ConfirmPostcodeCgt"     => ConfirmPostcodeCgtFormats.reads(properties)
    case "ConfirmCountryCodeCgt"  => ConfirmCountryCodeCgtFormats.reads(properties)
    case "ConfirmClient"          => ConfirmClientFormats.reads(properties)
    case "NotSignedUp"            => NotSignedUpFormats.reads(properties)
    case "KnownFactNotMatched"    => JsSuccess(KnownFactNotMatched)
    case "TrustNotFound"          => JsSuccess(TrustNotFound)
    case "CgtRefNotFound"         => CgtRefNotFoundFormats.reads(properties)
    case "PptRefNotFound"         => PptRefNotFoundFormats.reads(properties)
    case "ConfirmCancel"          => ConfirmCancelFormats.reads(properties)
    case "NotAuthorised"          => NotAuthorisedFormats.reads(properties)
    case "AuthorisationCancelled" => AuthorisationCancelledFormats.reads(properties)
    case "ResponseFailed"         => ResponseFailedFormats.reads(properties)
    case _                        => JsError(s"Unknown state name $stateName")
  }
}
