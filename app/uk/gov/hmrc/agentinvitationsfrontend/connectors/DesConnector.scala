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
import com.kenshoo.play.metrics.Metrics
import javax.inject.{Inject, Named, Singleton}
import play.api.libs.json._
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.UriPathEncoding.encodePathSegment
import uk.gov.hmrc.agentmtdidentifiers.model.MtdItId
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http._
import uk.gov.hmrc.http.logging.Authorization

import scala.concurrent.{ExecutionContext, Future}

case class NinoBusinessDetails(nino: Nino)

object NinoBusinessDetails {
  implicit val reads = Json.reads[NinoBusinessDetails]
}

@Singleton
class DesConnector @Inject()(
  @Named("des-baseUrl") baseUrl: URL,
  @Named("des.authorizationToken") authorizationToken: String,
  @Named("des.environment") environment: String,
  httpGet: HttpGet,
  metrics: Metrics)
    extends HttpAPIMonitor {
  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  def getNinoFor(mtdbsa: MtdItId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Nino] = {
    val url = new URL(baseUrl, s"/registration/business-details/mtdbsa/${encodePathSegment(mtdbsa.value)}")

    getWithDesHeaders[NinoBusinessDetails]("GetRegistrationBusinessDetailsByMtdbsa", url).map(_.nino)
  }

  private def getWithDesHeaders[A: HttpReads](apiName: String, url: URL)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[A] = {
    val desHeaderCarrier = hc.copy(
      authorization = Some(Authorization(s"Bearer $authorizationToken")),
      extraHeaders = hc.extraHeaders :+ "Environment" -> environment)
    monitor(s"ConsumedAPI-DES-$apiName-GET") {
      httpGet.GET[A](url.toString)(implicitly[HttpReads[A]], desHeaderCarrier, ec)
    }
  }
}
