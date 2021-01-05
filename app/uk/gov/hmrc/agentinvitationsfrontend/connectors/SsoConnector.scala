/*
 * Copyright 2021 HM Revenue & Customs
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

import com.codahale.metrics.MetricRegistry
import com.kenshoo.play.metrics.Metrics
import javax.inject.{Inject, Singleton}
import play.api.http.Status.OK
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SsoConnector @Inject()(http: HttpClient)(implicit appConfig: AppConfig, metrics: Metrics) extends HttpAPIMonitor {
  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  val url = s"${appConfig.ssoBaseUrl}/sso/domains"

  def getWhitelistedDomains()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Set[String]] =
    monitor("ConsumedAPI-SSO-getExternalDomains-GET") {
      http.GET[HttpResponse](url).map { r =>
        r.status match {
          case OK =>
            (r.json \ "externalDomains").as[Set[String]] ++ (r.json \ "internalDomains").as[Set[String]]
          case s => throw new Exception(s"retrieval of whitelisted domains failed, status: $s")
        }
      }
    }
}
