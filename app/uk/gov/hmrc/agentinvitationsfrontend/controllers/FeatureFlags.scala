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

package uk.gov.hmrc.agentinvitationsfrontend.controllers

import javax.inject.{Inject, Named, Singleton}

@Singleton
class FeatureFlags @Inject()(@Named("features.show-hmrc-mtd-it") val showHmrcMtdIt: Boolean,
                             @Named("features.show-personal-income") val showPersonalIncome: Boolean,
                             @Named("features.show-hmrc-mtd-vat") val showHmrcMtdVat: Boolean,
                             @Named("features.show-kfc-mtd-it") val showKfcMtdIt: Boolean,
                             @Named("features.show-kfc-personal-income") val showKfcPersonalIncome: Boolean,
                             @Named("features.show-kfc-mtd-vat") val showKfcMtdVat: Boolean,
                             @Named("features.show-fast-track") val showFastTrack: Boolean)
