/*
 * Copyright 2017 HM Revenue & Customs
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

import java.net.{InetSocketAddress, URL}
import java.util.concurrent.TimeUnit._
import javax.inject.{Inject, Provider, Singleton}

import com.codahale.metrics.{MetricFilter, SharedMetricRegistries}
import com.codahale.metrics.graphite.{Graphite, GraphiteReporter}
import com.google.inject.AbstractModule
import com.google.inject.name.{Named, Names}
import org.slf4j.MDC
import play.api.inject.ApplicationLifecycle
import play.api.{Configuration, Environment, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{FrontendAuditConnector, FrontendAuthConnector}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.http._
import uk.gov.hmrc.play.audit.http.HttpAuditing
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.config.inject.ServicesConfig
import uk.gov.hmrc.play.http.ws.WSHttp

import scala.concurrent.{ExecutionContext, Future}

class FrontendModule(val environment: Environment, val configuration: Configuration) extends AbstractModule with ServicesConfig {

  override val runModeConfiguration: Configuration = configuration

  def configure(): Unit = {
    val appName = configuration.getString("appName").getOrElse(throw new Exception("Missing 'appName' config property"))

    val loggerDateFormat: Option[String] = configuration.getString("logger.json.dateformat")
    Logger.info(s"Starting microservice : $appName : in mode : ${environment.mode}")
    MDC.put("appName", appName)
    loggerDateFormat.foreach(str => MDC.put("logger.json.dateformat", str))

    bindProperty("appName")

    bind(classOf[HttpGet]).to(classOf[HttpVerbs])
    bind(classOf[HttpPost]).to(classOf[HttpVerbs])
    bind(classOf[AuthConnector]).to(classOf[FrontendAuthConnector])
    bind(classOf[AuditConnector]).to(classOf[FrontendAuditConnector])

    bindBaseUrl("auth")
    bindBaseUrl("agent-client-authorisation")
    bindServiceProperty("company-auth.login-url")
    bindServiceProperty("agent-invitations-frontend.start-url")
    bindServiceProperty("agent-invitations-frontend.external-url")
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
    bind(classOf[String]).annotatedWith(Names.named(propertyName)).toProvider(new ServicePropertyProvider(propertyName))

  private class ServicePropertyProvider(confKey: String) extends Provider[String] {
    override lazy val get = getConfString(confKey, throw new IllegalStateException(s"No value found for configuration property $confKey"))
  }

}

@Singleton
class HttpVerbs @Inject() (val auditConnector: AuditConnector, @Named("appName") val appName: String)
  extends HttpGet with HttpPost with HttpPut with HttpPatch with HttpDelete with WSHttp
  with HttpAuditing {
  override val hooks = Seq(AuditingHook)
}