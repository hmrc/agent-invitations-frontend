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

import javax.inject.{Inject, Singleton}
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentinvitationsfrontend.models.{AuthorisedAgent, ClientType, Services}
import uk.gov.hmrc.agentmtdidentifiers.model.Service

@ImplementedBy(classOf[ConfigFeatureFlags])
trait FeatureFlags {
  val showHmrcMtdIt: Boolean
  val showPersonalIncome: Boolean
  val showHmrcMtdVat: Boolean
  val showHmrcTrust: Boolean
  val showHmrcCgt: Boolean
  val showPlasticPackagingTax: Boolean

  def isServiceEnabled(service: Service, agent: Option[AuthorisedAgent]): Boolean = service match {
    case Service.MtdIt                => showHmrcMtdIt
    case Service.PersonalIncomeRecord => showPersonalIncome
    case Service.Vat                  => showHmrcMtdVat
    case Service.Trust                => showHmrcTrust
    case Service.CapitalGains         => showHmrcCgt
    case Service.Ppt                  => showPlasticPackagingTax
  }

  def enabledServices(agent: Option[AuthorisedAgent]): Set[Service] = Services.supportedServices.toSet.filter(isServiceEnabled(_, agent))
  def enabledServicesFor(clientType: ClientType, agent: Option[AuthorisedAgent]): Set[Service] =
    Services.supportedServicesFor(clientType).filter(isServiceEnabled(_, agent))
}

@Singleton
case class ConfigFeatureFlags @Inject()(appConfig: AppConfig) extends FeatureFlags {

  val showHmrcMtdIt = appConfig.featuresMtdIt
  val showPersonalIncome = appConfig.featuresPersonalIncome
  val showHmrcMtdVat = appConfig.featuresMtdVat
  val showHmrcTrust = appConfig.featuresTrust
  val showHmrcCgt = appConfig.featuresCgt
  val showPlasticPackagingTax = appConfig.featuresPlasticPackagingTax

}
