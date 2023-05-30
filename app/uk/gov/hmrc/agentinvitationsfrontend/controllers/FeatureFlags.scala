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

package uk.gov.hmrc.agentinvitationsfrontend.controllers

import com.google.inject.ImplementedBy
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentinvitationsfrontend.models.{ClientType, Services}
import uk.gov.hmrc.agentmtdidentifiers.model.Service

import javax.inject.{Inject, Singleton}

@ImplementedBy(classOf[ConfigFeatureFlags])
trait FeatureFlags {

  def isServiceEnabled(service: Service): Boolean
  def enabledServices: Set[Service] = Services.supportedServices.toSet.filter(isServiceEnabled)
  def enabledServicesFor(clientType: ClientType): Set[Service] =
    Services.supportedServicesFor(clientType).filter(isServiceEnabled)
}

@Singleton
case class ConfigFeatureFlags @Inject()(appConfig: AppConfig) extends FeatureFlags {

  private val showHmrcMtdIt = appConfig.featuresMtdIt
  private val showPersonalIncome = appConfig.featuresPersonalIncome
  private val showHmrcMtdVat = appConfig.featuresMtdVat
  private val showHmrcTrust = appConfig.featuresTrust
  private val showHmrcCgt = appConfig.featuresCgt
  private val showPlasticPackagingTax = appConfig.featuresPlasticPackagingTax

  def isServiceEnabled(service: Service): Boolean = service match {
    case Service.MtdIt                => showHmrcMtdIt
    case Service.PersonalIncomeRecord => showPersonalIncome
    case Service.Vat                  => showHmrcMtdVat
    case Service.Trust                => showHmrcTrust
    case Service.TrustNT              => showHmrcTrust
    case Service.CapitalGains         => showHmrcCgt
    case Service.Ppt                  => showPlasticPackagingTax
  }

}
