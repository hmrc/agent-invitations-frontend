/*
 * Copyright 2019 HM Revenue & Customs
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
import play.api.Logger
import play.api.http.Status
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.models.{IVResult, NinoClStoreEntry}
import uk.gov.hmrc.http._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class IdentityVerificationConnector @Inject()(
  @Named("identity-verification-frontend-baseUrl") ivFrontendBaseUrl: URL,
  @Named("identity-verification-baseUrl") ivBaseUrl: URL,
  http: HttpGet with HttpPut,
  metrics: Metrics)
    extends HttpAPIMonitor {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  private[connectors] def getIVResultUrl(journeyId: String): URL =
    new URL(ivFrontendBaseUrl, s"/mdtp/journey/journeyId/$journeyId")

  private[connectors] def updateEntryUrl(credId: String): URL =
    new URL(ivBaseUrl, s"/nino/$credId")

  def getIVResult(journeyId: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[IVResult]] =
    monitor("ConsumedAPI-Client-Get-IVResult-GET") {
      http
        .GET[HttpResponse](getIVResultUrl(journeyId).toString)
        .map { response =>
          response.status match {
            case 200 => {
              val result = (response.json \ "result").as[IVResult]
              Logger.warn(s"identity verification returned result $result for journeyId $journeyId")
              Some(result)
            }
          }
        }
        .recover {
          case e: NotFoundException => {
            Logger.warn(s"identity verification did not recognise the journeyId $journeyId $e")
            None
          }
        }
    }

  /** Call identity-verification to update NINO store
    * @return The HTTP status code of the PUT response
    *
    * */
  def updateEntry(entry: NinoClStoreEntry, credId: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Int] =
    monitor("ConsumedAPI-Client-updateEntry-PUT") {
      http
        .PUT[NinoClStoreEntry, HttpResponse](updateEntryUrl(credId).toString, entry)
        .map(_.status)
        .recover {
          case e: Upstream5xxResponse => {
            Logger.error(s"identity-verification did not update entry ${e.getMessage}")
            Status.INTERNAL_SERVER_ERROR
          }
        }
    }
}
