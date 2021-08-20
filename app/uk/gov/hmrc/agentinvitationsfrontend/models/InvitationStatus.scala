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

package uk.gov.hmrc.agentinvitationsfrontend.models

import play.api.libs.json._

sealed trait InvitationStatus {

  def toEither: Either[String, InvitationStatus] = this match {
    case Unknown(status) => Left(status)
    case status          => Right(status)
  }

  def leftMap[X](f: String => X) =
    toEither.left.map(f)

  override def toString = InvitationStatus.unapply(this).getOrElse("Unknown")
}

case object Pending extends InvitationStatus

case object Expired extends InvitationStatus

case object Rejected extends InvitationStatus

case object Accepted extends InvitationStatus

case object Cancelled extends InvitationStatus

case object Deauthorised extends InvitationStatus

case object Partialauth extends InvitationStatus

case class Unknown(attempted: String) extends InvitationStatus

object InvitationStatus {
  def unapply(status: InvitationStatus): Option[String] = status match {
    case Pending      => Some("Pending")
    case Rejected     => Some("Rejected")
    case Accepted     => Some("Accepted")
    case Cancelled    => Some("Cancelled")
    case Expired      => Some("Expired")
    case Deauthorised => Some("Deauthorised")
    case Partialauth  => Some("Partialauth")
    case _            => None
  }

  def apply(status: String): InvitationStatus = status.toLowerCase match {
    case "pending"      => Pending
    case "rejected"     => Rejected
    case "accepted"     => Accepted
    case "cancelled"    => Cancelled
    case "expired"      => Expired
    case "deauthorised" => Deauthorised
    case "Partialauth"  => Partialauth
    case _              => Unknown(status)
  }

  implicit val invitationStatusFormat = new Format[InvitationStatus] {
    override def reads(json: JsValue): JsResult[InvitationStatus] = apply(json.as[String]) match {
      case Unknown(value) => JsError(s"Status of [$value] is not a valid InvitationStatus")
      case value          => JsSuccess(value)
    }

    override def writes(o: InvitationStatus): JsValue =
      unapply(o).map(JsString).getOrElse(throw new IllegalArgumentException)
  }
}
