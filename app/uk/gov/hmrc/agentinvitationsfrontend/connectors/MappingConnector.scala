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
import play.api.Logging
import play.api.http.Status._
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HttpClient, _}

import java.net.URL
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class MappingConnector @Inject()(http: HttpClient)(implicit val appConfig: AppConfig, metrics: Metrics)
    extends HttpAPIMonitor with Logging {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  val baseUrl: URL = new URL(appConfig.agentMappingBaseUrl)

  private[connectors] def getMappingUrl(arn: Arn): URL =
    new URL(baseUrl, s"/agent-mapping/mappings/$arn")

  private val originHeader = Seq("Origin" -> "agent-invitations-frontend")

  def getMapping(arn: Arn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[LegacyAgentId] =
    monitor(s"ConsumedAPI-Get-Agent-Mapping-GET") {
      val url = getMappingUrl(arn)
      http.GET[HttpResponse](url.toString, originHeader) map { r =>
        r.status match {
          case OK => r.json.as[LegacyAgentId]
          case status: Int =>
            logger.warn(s"unexpected status from agent-client-mapping when getting invitation, status: $status")
            throw new RuntimeException(s"error during getInvitation, status: $status, arn: $arn")

        }
      }
    }


}
