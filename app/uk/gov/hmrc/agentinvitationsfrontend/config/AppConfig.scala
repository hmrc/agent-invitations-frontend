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

import javax.inject.{Inject, Singleton}
import play.api.i18n.Lang
import play.api.mvc.Call
import uk.gov.hmrc.agentinvitationsfrontend.controllers.routes
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.duration.{Duration, FiniteDuration}

@Singleton
class AppConfig @Inject() (servicesConfig: ServicesConfig) {

  val appName = "agent-invitations-frontend"
  val ssoRedirectUrl: String = "/government-gateway-registration-frontend?accountType=agent&origin=unknown"
  val signupClientUrl: String = "/sign-up-your-client-for-making-tax-digital-for-income-tax"
  val accountLimitedUrl: String = asaFrontendExternalUrl + "/agent-services-account/account-limited"

  private def baseUrl(serviceName: String) = servicesConfig.baseUrl(serviceName)

  private def getConfString(config: String) =
    servicesConfig.getConfString(config, throw new RuntimeException(s"config $config not found"))

  // BaseUrls
  val authBaseUrl: String = baseUrl("auth")
  val agentClientAuthorisationBaseUrl: String = baseUrl("agent-client-authorisation")
  val afiBaseUrl: String = baseUrl("agent-fi-relationship")
  val ssoBaseUrl: String = baseUrl("sso")
  val cidBaseUrl: String = baseUrl("citizen-details")
  val acrBaseUrl: String = baseUrl("agent-client-relationships")
  val ivFrontendBaseUrl: String = baseUrl("identity-verification-frontend")
  val ivBackendBaseUrl: String = baseUrl("identity-verification")
  val personalDetailsValidationBaseUrl: String = baseUrl("personal-details-validation")

  // Strings
  val authLoginCallbackUrl: String = servicesConfig.getString("authentication.login-callback.url")
  val companyAuthFrontendExternalUrl: String = getConfString("company-auth-frontend.external-url")
  val companyAuthFrontendSignoutPath: String = getConfString("company-auth-frontend.sign-out.path")
  val companyAuthFrontendSigninPath: String = getConfString("company-auth-frontend.sign-in.path")
  val contactFrontendExternalUrl: String = getConfString("contact-frontend.external-url")
  val btaExternalUrl: String = getConfString("business-tax-account.external-url")
  lazy val asaFrontendExternalUrl: String = getConfString("agent-services-account-frontend.external-url")
  val ggRegistrationFrontendExternalUrl: String = s"${getConfString("government-gateway-registration-frontend.external-url")}$ssoRedirectUrl"
  val basGatewayFrontendRegisterUrl: String = getConfString("bas-gateway-frontend.register.url")

  val ptaExternalUrl: String = getConfString("personal-tax-account.external-url")
  val agentInvitationsFrontendExternalUrl: String = getConfString("agent-invitations-frontend.external-url")
  val agentSubscriptionFrontendExternalUrl: String = getConfString("agent-subscription-frontend.external-url")
  val agentMappingExternalUrl: String = getConfString("agent-mapping-frontend.external-url")
  val privacyPolicyExternalUrl: String = getConfString("privacy-policy.external-url")
  val acmExternalUrl: String = getConfString("agent-client-management-frontend.external-url")

  val feedbackSurveyUrl: String = getConfString("feedback-frontend.external-url")
  val agentOriginToken = "INVITAGENT"
  val clientOriginToken = "INVITCLIENT"

  val countryListLocation: String = servicesConfig.getString("country.list.location")

  val invitationExpirationDuration: Duration = servicesConfig.getDuration("invitation.expiryDuration")
  val agentMappingFrontendExternalUrl: String = getConfString("agent-mapping-frontend.external-url")
  val govUkExternalUrl: String = s"${getConfString("gov-uk.external-url")}"
  val govUkGuidanceExternalUrl: String = s"$govUkExternalUrl/guidance"
  val govUkGuidanceSignupUrl: String = s"$govUkGuidanceExternalUrl$signupClientUrl"

  val suspensionCacheDuration: FiniteDuration = servicesConfig.getDuration("cache.suspensionDetails.duration") match {
    case fd: FiniteDuration => fd
    case _                  => throw new IllegalArgumentException(s"invalid suspension cache duration")
  }
  val suspensionCacheEnabled: Boolean = suspensionCacheDuration.toMillis != 0

  val languageMap: Map[String, Lang] = Map(
    "english" -> Lang("en"),
    "cymraeg" -> Lang("cy")
  )
  def routeToSwitchLanguage: String => Call =
    (lang: String) => routes.AgentInvitationsLanguageController.switchToLanguage(lang)

  // Ints
  val trackRequestsShowLastDays: Int = servicesConfig.getInt("track-requests-show-last-days")
  val trackRequestsPerPage: Int = servicesConfig.getInt("track-requests-per-page")
  val timeoutDialogTimeoutSeconds: Int = servicesConfig.getInt("timeoutDialog.timeout-seconds")
  val timeoutDialogCountdownSeconds: Int = servicesConfig.getInt("timeoutDialog.timeout-countdown-seconds")
  val mongoSessionExpireAfterSeconds: Int = servicesConfig.getInt("mongodb.session.expireAfterSeconds")
  val altItsaSignupDays: Int = servicesConfig.getInt("alt-itsa-signup-days")

  // Feature flags
  val featuresMtdIt: Boolean = servicesConfig.getBoolean("features.show-hmrc-mtd-it")
  val featuresPersonalIncome: Boolean = servicesConfig.getBoolean("features.show-personal-income")
  val featuresMtdVat: Boolean = servicesConfig.getBoolean("features.show-hmrc-mtd-vat")
  val featuresTrust: Boolean = servicesConfig.getBoolean("features.show-hmrc-trust")
  val featuresCgt: Boolean = servicesConfig.getBoolean("features.show-hmrc-cgt")
  val featuresPlasticPackagingTax: Boolean = servicesConfig.getBoolean("features.show-plastic-packaging-tax")
  val featuresAltItsa: Boolean = servicesConfig.getBoolean("features.enable-alt-itsa")
  val featuresCbc: Boolean = servicesConfig.getBoolean("features.show-cbc")
  val featuresPillar2: Boolean = servicesConfig.getBoolean("features.show-pillar2")
  val languageToggle: Boolean = servicesConfig.getBoolean("features.enable-welsh-toggle")
  val enableAcrfRedirects: Boolean = servicesConfig.getBoolean("features.enable-acrf-redirects")

  // ACRF routes
  val acrfBaseUrl: String = getConfString("agent-client-relationships-frontend.base-url")
  val acrfConnectorUrl: String = baseUrl("agent-client-relationships-frontend")
  val createAuthRequestUrl: String = acrfBaseUrl + getConfString("agent-client-relationships-frontend.create-auth-request-url")
  val cancelAuthRequestUrl: String = acrfBaseUrl + getConfString("agent-client-relationships-frontend.cancel-auth-request-url")
  val manageAuthRequestsUrl: String = acrfBaseUrl + getConfString("agent-client-relationships-frontend.manage-auth-requests-url")
  val fastTrackUrl: String = acrfConnectorUrl + getConfString("agent-client-relationships-frontend.fast-track-url")
  val manageYourTaxAgentsUrl: String = acrfConnectorUrl + getConfString("agent-client-relationships-frontend.manage-your-tax-agents-url")
}
