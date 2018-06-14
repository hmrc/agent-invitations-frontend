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

package uk.gov.hmrc.agentinvitationsfrontend.connectors

import java.net.URL

import com.codahale.metrics.MetricRegistry
import com.google.inject.name.Named
import com.kenshoo.play.metrics.Metrics
import javax.inject.{Inject, Singleton}
import play.api.Logger
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier, HttpGet}
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

import scala.concurrent.Future

@Singleton
class SsoConnector @Inject()(http: HttpGet, @Named("sso-baseUrl") baseUrl: URL, metrics: Metrics)
    extends HttpAPIMonitor {
  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  def validateExternalDomain(domain: String)(implicit hc: HeaderCarrier): Future[Boolean] =
    monitor(s"ConsumedAPI-SSO-validateExternalDomain-GET") {
      val url = new URL(baseUrl, s"/sso/validate/domain/$domain")
      http
        .GET(url.toString)
        .map(_ => true)
        .recover {
          case _: BadRequestException => false
          case e: Exception =>
            Logger(getClass).error(s"Unable to validate domain $domain", e)
            false
        }
    }
}
