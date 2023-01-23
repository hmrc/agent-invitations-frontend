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

package uk.gov.hmrc.agentinvitationsfrontend.models

import play.api.libs.json._
import uk.gov.hmrc.http.BadRequestException

sealed trait IVResult {
  val value: String
}

case object Success extends IVResult { override val value = "Success" }
case object Incomplete extends IVResult { override val value = "Incomplete" }
case object PreconditionFailed extends IVResult { override val value = "PreconditionFailed" }
case object LockedOut extends IVResult { override val value = "LockedOut" }
case object InsufficientEvidence extends IVResult { override val value = "InsufficientEvidence" }
case object FailedMatching extends IVResult { override val value = "FailedMatching" }
case object TechnicalIssue extends IVResult { override val value = "TechnicalIssue" }
case object UserAborted extends IVResult { override val value = "UserAborted" }
case object TimedOut extends IVResult { override val value = "Timeout" }
case object FailedIV extends IVResult { override val value = "FailedIV" }
case object FailedDirectorCheck extends IVResult { override val value = "FailedDirectorCheck" }

object IVResult {
  def apply(str: String): IVResult = str match {
    case Success.value              => Success
    case Incomplete.value           => Incomplete
    case PreconditionFailed.value   => PreconditionFailed
    case LockedOut.value            => LockedOut
    case InsufficientEvidence.value => InsufficientEvidence
    case FailedMatching.value       => FailedMatching
    case TechnicalIssue.value       => TechnicalIssue
    case UserAborted.value          => UserAborted
    case TimedOut.value             => TimedOut
    case FailedIV.value             => FailedIV
    case FailedDirectorCheck.value  => FailedDirectorCheck
    case _                          => throw new BadRequestException("strange value for IVReason")
  }

  def unapply(reason: IVResult): Option[String] = reason match {
    case Success              => Some("Success")
    case Incomplete           => Some("Incomplete")
    case PreconditionFailed   => Some("PreconditionFailed")
    case LockedOut            => Some("LockedOut")
    case InsufficientEvidence => Some("InsufficientEvidence")
    case FailedMatching       => Some("FailedMatching")
    case TechnicalIssue       => Some("TechnicalIssue")
    case UserAborted          => Some("UserAborted")
    case TimedOut             => Some("Timeout")
    case FailedIV             => Some("FailedIV")
    case FailedDirectorCheck  => Some("FailedDirectorCheck")
  }

  implicit val reads: Reads[IVResult] = new Reads[IVResult] {
    override def reads(json: JsValue): JsResult[IVResult] =
      json match {
        case JsString(Success.value)              => JsSuccess(Success)
        case JsString(Incomplete.value)           => JsSuccess(Incomplete)
        case JsString(PreconditionFailed.value)   => JsSuccess(PreconditionFailed)
        case JsString(LockedOut.value)            => JsSuccess(LockedOut)
        case JsString(InsufficientEvidence.value) => JsSuccess(InsufficientEvidence)
        case JsString(FailedMatching.value)       => JsSuccess(FailedMatching)
        case JsString(TechnicalIssue.value)       => JsSuccess(TechnicalIssue)
        case JsString(UserAborted.value)          => JsSuccess(UserAborted)
        case JsString(TimedOut.value)             => JsSuccess(TimedOut)
        case JsString(FailedIV.value)             => JsSuccess(FailedIV)
        case JsString(FailedDirectorCheck.value)  => JsSuccess(FailedDirectorCheck)
        case invalid                              => JsError(s"invalid IVResult value found: $invalid")
      }
  }

  implicit val writes: Writes[IVResult] = new Writes[IVResult] {
    override def writes(o: IVResult): JsValue = JsString(o.value)
  }

}
