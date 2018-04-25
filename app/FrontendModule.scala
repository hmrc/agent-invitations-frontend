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

import java.net.URL
import javax.inject.{Inject, Provider, Singleton}

import com.google.inject.AbstractModule
import com.google.inject.name.{Named, Names}
import org.slf4j.MDC
import play.api.{Configuration, Environment, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{FrontendAuthConnector}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{FrontendPasscodeVerification, PasscodeVerification}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.otac.OtacAuthConnector
import uk.gov.hmrc.http._
import uk.gov.hmrc.play.audit.http.HttpAuditing
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http.ws.WSHttp

class FrontendModule(val environment: Environment, val configuration: Configuration) extends AbstractModule with ServicesConfig {

  override val runModeConfiguration: Configuration = configuration
  override protected def mode = environment.mode

  def configure(): Unit = {
    val appName = "agent-invitations-frontend"

    val loggerDateFormat: Option[String] = configuration.getString("logger.json.dateformat")
    Logger.info(s"Starting microservice : $appName : in mode : ${environment.mode}")
    MDC.put("appName", appName)
    loggerDateFormat.foreach(str => MDC.put("logger.json.dateformat", str))

    bindProperty("appName")

    bind(classOf[HttpGet]).to(classOf[HttpVerbs])
    bind(classOf[HttpPost]).to(classOf[HttpVerbs])
    bind(classOf[AuthConnector]).to(classOf[FrontendAuthConnector])
    bind(classOf[OtacAuthConnector]).to(classOf[FrontendAuthConnector])
    bind(classOf[PasscodeVerification]).to(classOf[FrontendPasscodeVerification])

    bindBaseUrl("auth")
    bindBaseUrl("agent-client-authorisation")
    bindBaseUrl("agent-fi-relationship")
    bindBaseUrl("authentication.login-callback.url")
    bindBaseUrl("agent-services-account")
    bindBaseUrl("cachable.session-cache")

    bindServiceProperty("company-auth-frontend.external-url")
    bindServiceProperty("company-auth-frontend.sign-out.path")
    bindServiceProperty("contact-frontend.external-url")

    bindServiceProperty("business-tax-account.external-url")
    bindServiceProperty("agent-services-account-frontend.external-url")
    bindServiceProperty("tax-account-router-frontend.account-url")
    bindServiceProperty("personal-tax-account.external-url")
    bindServiceProperty("agent-invitations-frontend.external-url")
    bindServiceProperty("agent-subscription-frontend.external-url")
    bindServiceProperty("cachable.session-cache.domain")

    bindServiceProperty("feedback-survey-frontend.external-url")
    bindProperty("survey.feedbackSurveyURNWithoutOriginToken")
    bindProperty("survey.originTokenIdentifier.agent")
    bindProperty("survey.originTokenIdentifier.client")

    bindBooleanProperty("features.show-hmrc-mtd-it")
    bindBooleanProperty("features.show-personal-income")
    bindBooleanProperty("features.show-hmrc-mtd-vat")
    bindBooleanProperty("features.show-kfc-mtd-it")
    bindBooleanProperty("features.show-kfc-personal-income")
    bindBooleanProperty("features.show-kfc-mtd-vat")
  }

  private def bindBaseUrl(serviceName: String) =
    bind(classOf[URL]).annotatedWith(Names.named(s"$serviceName-baseUrl")).toProvider(new BaseUrlProvider(serviceName))

  private class BaseUrlProvider(serviceName: String) extends Provider[URL] {
    override lazy val get = new URL(baseUrl(serviceName))
  }

  private def bindProperty(propertyName: String) =
    bind(classOf[String]).annotatedWith(Names.named(propertyName)).toProvider(new PropertyProvider(propertyName))

  private class PropertyProvider(confKey: String) extends Provider[String] {
    override lazy val get = configuration.getString(confKey)
      .getOrElse(throw new IllegalStateException(s"No value found for configuration property $confKey"))
  }

  private def bindServiceProperty(propertyName: String) =
    bind(classOf[String]).annotatedWith(Names.named(s"$propertyName")).toProvider(new ServicePropertyProvider(propertyName))

  private class ServicePropertyProvider(propertyName: String) extends Provider[String] {
    override lazy val get = getConfString(propertyName, throw new RuntimeException(s"No configuration value found for '$propertyName'"))
  }

  private def bindBooleanProperty(propertyName: String) =
    bind(classOf[Boolean]).annotatedWith(Names.named(propertyName)).toProvider(new BooleanPropertyProvider(propertyName))

  private class BooleanPropertyProvider(confKey: String) extends Provider[Boolean] {
    override lazy val get: Boolean = configuration.getBoolean(confKey)
      .getOrElse(throw new IllegalStateException(s"No value found for configuration property $confKey"))
  }
}

@Singleton
class HttpVerbs @Inject() (val auditConnector: AuditConnector, @Named("appName") val appName: String)
  extends HttpGet with HttpPost with HttpPut with HttpPatch with HttpDelete with WSHttp
  with HttpAuditing {
  override val hooks = Seq(AuditingHook)
}