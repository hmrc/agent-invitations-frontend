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

package uk.gov.hmrc.agentinvitationsfrontend.controllers

import javax.inject.{Inject, Singleton}
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig

@Singleton
case class FeatureFlags @Inject()(appConfig: AppConfig) {

  val showHmrcMtdIt = appConfig.featuresMtdIt
  val showPersonalIncome = appConfig.featuresPersonalIncome
  val showHmrcMtdVat = appConfig.featuresMtdVat
  val showHmrcTrust = appConfig.featuresTrust
  val showHmrcCgt = appConfig.featuresCgt
  val enableTrackCancelAuth = appConfig.featuresEnableTrackCancelAction
  val showAgentLedDeAuth = appConfig.featuresAgentLedDeAuth
  val agentSuspensionEnabled = appConfig.featuresAgentSuspension

}
