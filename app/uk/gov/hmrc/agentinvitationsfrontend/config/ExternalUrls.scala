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

package uk.gov.hmrc.agentinvitationsfrontend.config

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import javax.inject.{Inject, Singleton}

@Singleton
class ExternalUrls @Inject()(implicit appConfig: AppConfig) {

  val companyAuthUrl = appConfig.companyAuthFrontendExternalUrl
  val companyAuthSignOutPath = appConfig.companyAuthFrontendSignoutPath
  val businessTaxAccountUrl = appConfig.btaExternalUrl
  val agentServicesAccountUrl = appConfig.asaFrontendExternalUrl
  val contactFrontendUrl = appConfig.contactFrontendExternalUrl
  val exitSurveyUrl = appConfig.feedbackSurveyFrontendExternalUrl
  val invitationExitSurvey = appConfig.feedbackSurveyOriginToken
  val agentOriginTokenIdentifier = appConfig.feedbackSurveyagentIdentifier
  val clientOriginTokenIdentifier = appConfig.feedbackSurveyClientIdentifier

  val pdvFrontendUrl = appConfig.pdvFrontendExternalUrl
  val subscriptionURL = appConfig.agentSubscriptionFrontendExternalUrl
  val agentClientManagementUrl = appConfig.acmExternalUrl
  val agentInvitationsExternalUrl = appConfig.agentInvitationsFrontendExternalUrl
  val privacypolicyUrl = appConfig.privacyPolicyExternalUrl
  val vatOnlineServiceHelplineUrl = appConfig.vatOnlineHelplineExternalUrl
  val saOnlineServiceHelplineUrl = appConfig.saOnlineHelplineFrontendExternalUrl
  val timeout = appConfig.timeoutDialogTimeoutSeconds
  val timeoutCountdown = appConfig.timeoutDialogCountdownSeconds

  val companyAuthFrontendSignOutUrl = s"$companyAuthUrl$companyAuthSignOutPath"

  private def contactFrontendServiceId(isAgent: Boolean) =
    if (isAgent) agentOriginTokenIdentifier else clientOriginTokenIdentifier

  def signOutUrl(isAgent: Boolean, goToSurvey: Option[Boolean]): String = {
    val continueUrl = if (isAgent) {
      if (goToSurvey.getOrElse(false)) s"$exitSurveyUrl$invitationExitSurvey$agentOriginTokenIdentifier"
      else s"$agentServicesAccountUrl/agent-services-account"
    } else {
      if (goToSurvey.getOrElse(false)) s"$exitSurveyUrl$invitationExitSurvey$clientOriginTokenIdentifier"
      else s"$businessTaxAccountUrl/business-account"
    }
    s"$companyAuthUrl$companyAuthSignOutPath?continue=${URLEncoder.encode(continueUrl, StandardCharsets.UTF_8.name())}"
  }

  def contactFrontendAjaxUrl(isAgent: Boolean): String =
    s"$contactFrontendUrl/contact/problem_reports_ajax?service=${contactFrontendServiceId(isAgent)}"

  def contactFrontendNonJsUrl(isAgent: Boolean): String =
    s"$contactFrontendUrl/contact/problem_reports_nonjs?service=${contactFrontendServiceId(isAgent)}"
}
