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

package uk.gov.hmrc.agentinvitationsfrontend.config

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import javax.inject.{Inject, Named, Singleton}

@Singleton
class ExternalUrls @Inject()(
  @Named("company-auth-frontend.external-url") val companyAuthUrl: String,
  @Named("company-auth-frontend.sign-out.path") val companyAuthSignOutPath: String,
  @Named("business-tax-account.external-url") val businessTaxAccountUrl: String,
  @Named("tax-account-router-frontend.account-url") val taxAccountRelativeUrl: String,
  @Named("agent-services-account-frontend.external-url") val agentServicesAccountUrl: String,
  @Named("contact-frontend.external-url") val contactFrontendUrl: String,
  @Named("feedback-survey-frontend.external-url") val exitSurveyUrl: String,
  @Named("survey.feedbackSurveyURNWithoutOriginToken") val invitationExitSurvey: String,
  @Named("survey.originTokenIdentifier.agent") val agentOriginTokenIdentifier: String,
  @Named("survey.originTokenIdentifier.client") val clientOriginTokenIdentifier: String,
  @Named("agent-subscription-frontend.external-url") val subscriptionURL: String,
  @Named("agent-client-management-frontend.external-url") val agentClientManagementUrl: String,
  @Named("privacy-policy.external-url") val privacypolicyUrl: String
) {

  val companyAuthFrontendSignOutUrl = s"$companyAuthUrl$companyAuthSignOutPath"

  private def contactFrontendServiceId(isAgent: Boolean) =
    if (isAgent) agentOriginTokenIdentifier else clientOriginTokenIdentifier

  def signOutUrl(isAgent: Boolean, goToSurvey: Option[Boolean]): String = {
    val continueUrl = isAgent match {
      case true => {
        if (goToSurvey.getOrElse(false) == true) s"$exitSurveyUrl$invitationExitSurvey$agentOriginTokenIdentifier"
        else s"$agentServicesAccountUrl/agent-services-account"
      }
      case false => {
        if (goToSurvey.getOrElse(false) == true) s"$exitSurveyUrl$invitationExitSurvey$clientOriginTokenIdentifier"
        else s"$businessTaxAccountUrl/business-account"
      }
    }
    s"$companyAuthUrl$companyAuthSignOutPath?continue=${URLEncoder.encode(continueUrl, StandardCharsets.UTF_8.name())}"
  }

  def contactFrontendAjaxUrl(isAgent: Boolean): String =
    s"$contactFrontendUrl/contact/problem_reports_ajax?service=${contactFrontendServiceId(isAgent)}"

  def contactFrontendNonJsUrl(isAgent: Boolean): String =
    s"$contactFrontendUrl/contact/problem_reports_nonjs?service=${contactFrontendServiceId(isAgent)}"
}
