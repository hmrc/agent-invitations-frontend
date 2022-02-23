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

package support

import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags

case class TestFeatureFlags(
  override val showHmrcMtdIt: Boolean = false,
  override val showPersonalIncome: Boolean = false,
  override val showHmrcMtdVat: Boolean = false,
  override val showHmrcTrust: Boolean = false,
  override val showHmrcCgt: Boolean = false,
  override val showPlasticPackagingTax: Boolean = false,
  override val enableTrackCancelAuth: Boolean = false,
  override val showAgentLedDeAuth: Boolean = false,
  override val agentSuspensionEnabled: Boolean = false,
  override val acceptTrustURNIdentifier: Boolean = false,
) extends FeatureFlags

object TestFeatureFlags {
  def allDisabled: TestFeatureFlags = TestFeatureFlags()
  def allEnabled: TestFeatureFlags = TestFeatureFlags(true, true, true, true, true, true, true, true, true, true)
}
