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
import javax.inject.{Inject, Named, Singleton}

import com.codahale.metrics.MetricRegistry
import com.kenshoo.play.metrics.Metrics
import org.joda.time.DateTime
import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.models.PirRelationship
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.http._
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class PirRelationshipConnector @Inject()(
                                          @Named("agent-fi-relationship-baseUrl") baseUrl: URL,
                                          http: HttpGet with HttpPost with HttpPut with HttpDelete,
                                          metrics: Metrics) extends HttpAPIMonitor {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  def getClientRelationships(service: String, clientId: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[List[PirRelationship]]] = {
    getRelationshipList(deauthServiceClientIdUrl(service, clientId))
  }

  def terminateAllClientIdRelationships(service: String, clientId: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Int] = {
    terminateAllClientRelationships(deauthServiceClientIdUrl(service, clientId))
  }

  def getRelationshipList(location: String)(implicit hc: HeaderCarrier): Future[Option[List[PirRelationship]]] = {
    monitor(s"ConsumedAPI-Get-AfiRelationship-GET") {
      val url = craftUrl(location)
      http.GET[Option[List[PirRelationship]]](url.toString)
    }
  }

  def terminateAllClientRelationships(location: String)(implicit hc: HeaderCarrier): Future[Int] = {
    monitor(s"ConsumedAPI-Get-AfiRelationship-GET") {
      val url = craftUrl(location)
      http.DELETE[HttpResponse](url.toString).map(_.status)
        .recover {
          case _: Upstream5xxResponse => 500
        }
    }
  }

  def createRelationship(arn: Arn, service: String, clientId: String)(implicit hc: HeaderCarrier): Future[Int] = {
    val ISO_LOCAL_DATE_TIME_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS"
    val url = craftUrl(createAndDeleteRelationshipUrl(arn, service, clientId))
    val body = Json.obj("startDate" -> DateTime.now().toString(ISO_LOCAL_DATE_TIME_FORMAT))
    http.PUT[JsObject, HttpResponse](url.toString, body).map(_.status)
      .recover {
        case _: Upstream5xxResponse => 500
      }
  }

  def deleteRelationship(arn: Arn, service: String, clientId: String)(implicit hc: HeaderCarrier): Future[Int] = {
    val url = craftUrl(createAndDeleteRelationshipUrl(arn, service, clientId))
    http.DELETE[HttpResponse](url.toString).map(_.status)
      .recover {
        case _: Upstream5xxResponse => 500
      }
  }

  private def createAndDeleteRelationshipUrl(arn: Arn, service: String, clientId: String) =
    s"/agent-fi-relationship/relationships/agent/${arn.value}/service/$service/client/$clientId"

  private def deauthServiceClientIdUrl(service: String, clientId: String): String =
    s"/agent-fi-relationship/relationships/service/$service/clientId/$clientId"

  private def craftUrl(location: String) = new URL(baseUrl, location)
}