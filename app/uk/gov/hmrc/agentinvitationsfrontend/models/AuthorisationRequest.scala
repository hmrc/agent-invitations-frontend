/*
 * Copyright 2018 HM Revenue & Customs
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
import play.api.libs.json.{Json, OFormat}

import scala.util.Random

case class AuthorisationRequest(
  clientName: String,
  service: String,
  clientId: String,
  state: String = AuthorisationRequest.NEW,
  itemId: String = AuthorisationRequest.randomItemId)
    extends InvitationParams {

  def clientIdentifierType: String = service match {
    case Services.HMRCMTDIT  => "ni"
    case Services.HMRCMTDVAT => "vrn"
    case Services.HMRCPIR    => "ni"
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[AuthorisationRequest]

  override def equals(other: Any): Boolean = other match {
    case that: AuthorisationRequest =>
      (that canEqual this) &&
        service == that.service &&
        clientId == that.clientId
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(service, clientId)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object AuthorisationRequest {

  private val IdCharacterSet = "ABCDEFGHJKLMNOPRSTUWXYZ123456789"

  private def randomChar = IdCharacterSet(Random.nextInt(IdCharacterSet.length)).toString
  private def randomItemId = randomChar + randomChar + randomChar

  implicit val format: OFormat[AuthorisationRequest] = Json.format[AuthorisationRequest]

  val NEW: String = "New"
  val CREATED: String = "Created"
  val FAILED: String = "Failed"

  def eachHasBeenCreatedIn(requests: Set[AuthorisationRequest]): Boolean = requests.forall(_.state == CREATED)

  def noneHaveBeenCreatedIn(requests: Set[AuthorisationRequest]): Boolean = requests.forall(_.state == FAILED)
}
