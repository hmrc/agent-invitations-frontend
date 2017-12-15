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

package uk.gov.hmrc.agentinvitationsfrontend.connectors


import java.net.URL
import javax.inject.{Inject, Named, Singleton}

import com.codahale.metrics.MetricRegistry
import com.kenshoo.play.metrics.Metrics
import play.api.libs.json.Reads
import play.mvc.Result
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.models.Relationship
import uk.gov.hmrc.http._
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import uk.gov.hmrc.http.{HttpResponse, HeaderCarrier}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AfiRelationshipConnector @Inject()(
                                          @Named("agent-fi-relationship-baseUrl") baseUrl: URL,
                                          http: HttpGet with HttpPost with HttpPut with HttpDelete,
                                          metrics: Metrics) extends HttpAPIMonitor {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  def getAfiClientRelationships(service: String, clientId: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[List[Relationship]] = {
    getAfiRelationshipList(afiDeauthServiceClientIdUrl(service, clientId))
  }

  def afiTerminateAllClientIdRelationships(service: String, clientId: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Result] = {
    afiTerminateAllClientRelationships(afiDeauthServiceClientIdUrl(service, clientId))
  }

  def getAfiRelationshipList(location: String)(implicit hc: HeaderCarrier): Future[List[Relationship]] = {
    monitor(s"ConsumedAPI-Get-AfiRelationship-GET") {
      val url = invitationUrl(location)
      implicit val readsRelationship: Reads[Relationship] = Relationship.reads(url)
      http.GET[List[Relationship]](url.toString)
    }
  }

  def afiTerminateAllClientRelationships(location: String)(implicit hc: HeaderCarrier): Future[Result] = {
    monitor(s"ConsumedAPI-Get-AfiRelationship-GET") {
      val url = invitationUrl(location)
      http.DELETE[Result](url.toString).map(response => response)
    }
  }

  private def afiDeauthServiceClientIdUrl(service: String, clientId: String): String =
    s"/agent-fi-relationship/relationships/service/$service/clientId/$clientId"

  private def invitationUrl(location: String) = new URL(baseUrl, location)
}
