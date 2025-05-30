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

package uk.gov.hmrc.agentinvitationsfrontend.config

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import javax.inject.{Inject, Singleton}

@Singleton
class ExternalUrls @Inject() (implicit appConfig: AppConfig) {

  val companyAuthUrl: String = appConfig.companyAuthFrontendExternalUrl
  val companyAuthSignOutPath: String = appConfig.companyAuthFrontendSignoutPath
  val companyAuthSignInPath: String = appConfig.companyAuthFrontendSigninPath
  val businessTaxAccountUrl: String = appConfig.btaExternalUrl
  val agentServicesAccountUrl = s"${appConfig.asaFrontendExternalUrl}/agent-services-account/home"
  val contactFrontendUrl: String = appConfig.contactFrontendExternalUrl
  val exitSurveyUrl: String = appConfig.feedbackSurveyUrl
  val agentOriginToken: String = appConfig.agentOriginToken
  val clientOriginToken: String = appConfig.clientOriginToken

  val subscriptionURL: String = appConfig.agentSubscriptionFrontendExternalUrl
  val agentClientManagementUrl: String = appConfig.acmExternalUrl
  val manageYourTaxAgentsUrl: String = appConfig.manageYourTaxAgentsUrl
  val agentInvitationsExternalUrl: String = appConfig.agentInvitationsFrontendExternalUrl
  val privacypolicyUrl: String = appConfig.privacyPolicyExternalUrl
  val agentMappingFrontendUrl = s"${appConfig.agentMappingFrontendExternalUrl}/agent-mapping/start"
  val timeout: Int = appConfig.timeoutDialogTimeoutSeconds
  val timeoutCountdown: Int = appConfig.timeoutDialogCountdownSeconds
  val guidanceUrlVatExisting = s"${appConfig.govUkGuidanceExternalUrl}/sign-up-for-making-tax-digital-for-vat"
  val guidanceUrlVatNew = s"${appConfig.govUkGuidanceExternalUrl}/sign-your-business-up-for-making-tax-digital-for-vat"
  val guidanceUrlSaExisting = s"${appConfig.govUkGuidanceExternalUrl}/agents-use-software-to-send-income-tax-updates"
  val guidanceUrlSaNew = s"${appConfig.govUkGuidanceExternalUrl}/use-software-to-send-income-tax-updates"
  val guidanceAuthoriseAgent: String =
    s"${appConfig.govUkGuidanceExternalUrl}/authorise-an-agent-to-deal-with-certain-tax-services-for-you"
  val guidanceUrlSaSignup = s"${appConfig.govUkExternalUrl}/register-for-self-assessment/self-employed"

  val companyAuthFrontendSignOutUrl = s"$companyAuthUrl$companyAuthSignOutPath"
  val companyAuthFrontendSignInUrl = s"$companyAuthUrl$companyAuthSignInPath"

  def contactFrontendServiceId(isAgent: Boolean): String = if (isAgent) agentOriginToken else clientOriginToken

  def signOutUrl(isAgent: Boolean, goToSurvey: Option[Boolean] = None): String = {
    val continueUrl = if (isAgent) {
      if (goToSurvey.getOrElse(false)) s"$exitSurveyUrl/$agentOriginToken"
      else agentServicesAccountUrl
    } else {
      s"$exitSurveyUrl/$clientOriginToken"
    }
    s"$companyAuthFrontendSignOutUrl?continue=${URLEncoder.encode(continueUrl, StandardCharsets.UTF_8.name())}"
  }

  val guidanceCgtUkProperty = "https://www.tax.service.gov.uk/capital-gains-tax-uk-property/start/report-pay-capital-gains-tax-uk-property"
  val guidanceVatSignup = "https://www.gov.uk/vat-record-keeping/sign-up-for-making-tax-digital-for-vat"
  val guidanceSa = "https://www.gov.uk/guidance/self-assessment-for-agents-online-service"
}
