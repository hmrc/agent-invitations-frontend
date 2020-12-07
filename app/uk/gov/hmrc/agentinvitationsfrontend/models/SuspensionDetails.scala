/*
 * Copyright 2020 HM Revenue & Customs
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

case class SuspensionDetails(suspensionStatus: Boolean, regimes: Option[Set[String]]) {

  //PERSONAL-INCOME-RECORD service has no enrolment / regime so cannot be suspended
  private val validSuspensionRegimes = Set("ITSA", "VATC", "TRS", "CGT")

  val suspendedRegimes: Set[String] =
    this.regimes.fold(Set.empty[String])(rs => if (rs.contains("ALL")) validSuspensionRegimes else rs)

  private val serviceToRegime: Map[String, String] =
    Map("HMRC-MTD-IT" -> "ITSA", "HMRC-MTD-VAT" -> "VATC", "HMRC-TERS-ORG" -> "TRS", "HMRC-CGT-PD" -> "CGT", "PERSONAL-INCOME-RECORD" -> "PIR")

  def isRegimeSuspended(service: String): Boolean = {
    val regime = serviceToRegime(service)
    suspendedRegimes.contains(regime)
  }

  def isAgentSuspended(s: Set[String]): Boolean = getSuspendedRegimes(s).nonEmpty

  def getSuspendedRegimes(services: Set[String]): Set[String] = {
    val r: Set[String] = services map (service => serviceToRegime(service))
    suspendedRegimes.intersect(r)
  }
}

case class SuspensionDetailsNotFound(message: String) extends Exception(message)

object SuspensionDetails {
  implicit val formats: OFormat[SuspensionDetails] = Json.format

}
