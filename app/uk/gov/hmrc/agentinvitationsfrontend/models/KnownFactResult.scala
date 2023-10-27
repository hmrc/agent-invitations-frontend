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

sealed trait KnownFactResult {
  def isOk: Boolean = this == KnownFactResult.Pass
}

object KnownFactResult {
  case object Pass extends KnownFactResult
  case class Fail(problem: KnownFactFailure) extends KnownFactResult

  sealed trait KnownFactFailure
  case object NotFound extends KnownFactFailure
  case object NotMatched extends KnownFactFailure
  case class HttpStatus(status: Int) extends KnownFactFailure
  case object VatClientInsolvent extends KnownFactFailure
  case object VatMigrationInProgress extends KnownFactFailure
}
