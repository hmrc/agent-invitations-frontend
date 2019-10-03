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

package uk.gov.hmrc.agentinvitationsfrontend.controllers

import javax.inject.{Inject, Named, Singleton}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}

@Singleton
case class FeatureFlags @Inject()(
  @Named("features.show-hmrc-mtd-it") showHmrcMtdIt: Boolean = true,
  @Named("features.show-personal-income") showPersonalIncome: Boolean = true,
  @Named("features.show-hmrc-mtd-vat") showHmrcMtdVat: Boolean = true,
  @Named("features.show-hmrc-trust") showHmrcTrust: Boolean = false,
  @Named("features.show-hmrc-cgt") showHmrcCgt: Boolean = false,
  @Named("features.enable-track-cancel-auth-action") enableTrackCancelAuth: Boolean = false,
  @Named("features.show-agent-led-de-auth") showAgentLedDeAuth: Boolean = true) {}

object FeatureFlags {
  def apply(): FeatureFlags = new FeatureFlags()
}
