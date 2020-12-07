/*
 * Copyright 2020 HM Revenue & Customs
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
import play.api.Logging
import play.api.http.Status._
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentinvitationsfrontend.models.{IVResult, NinoClStoreEntry}
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HttpClient, _}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class IdentityVerificationConnector @Inject()(http: HttpClient)(implicit val appConfig: AppConfig, metrics: Metrics)
    extends HttpAPIMonitor with Logging {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  private[connectors] def getIVResultUrl(journeyId: String) =
    s"${appConfig.ivFrontendBaseUrl}/mdtp/journey/journeyId/$journeyId"

  private[connectors] def updateEntryUrl(credId: String) =
    s"${appConfig.ivBackendBaseUrl}/identity-verification/nino/$credId"

  def getIVResult(journeyId: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[IVResult]] =
    monitor("ConsumedAPI-Client-Get-IVResult-GET") {
      http
        .GET[HttpResponse](getIVResultUrl(journeyId).toString)
        .map { response =>
          response.status match {
            case OK =>
              val result = (response.json \ "result").as[IVResult]
              logger.warn(s"identity verification returned result $result for journeyId $journeyId")
              Some(result)
            case NOT_FOUND => None
          }
        }
    }

  /** Call identity-verification to update NINO store
    * @return The HTTP status code of the PUT response
    *
    * */
  def updateEntry(entry: NinoClStoreEntry, credId: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Int] =
    monitor("ConsumedAPI-Client-updateNinoOnAuthRecord-PUT") {
      http
        .PUT[NinoClStoreEntry, HttpResponse](updateEntryUrl(credId).toString, entry)
        .map(_.status)
    }
}
