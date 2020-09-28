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
import play.api.http.Status._
import play.api.libs.json.{JsPath, Reads}
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HttpClient, _}

import scala.concurrent.{ExecutionContext, Future}

case class Citizen(firstName: Option[String], lastName: Option[String], nino: Option[String] = None) {
  lazy val name: Option[String] = {
    val n = Seq(firstName, lastName).collect({ case Some(x) => x }).mkString(" ")
    if (n.isEmpty) None else Some(n)
  }
}

object Citizen {
  implicit val reads: Reads[Citizen] = {
    val current = JsPath \ "name" \ "current"
    for {
      fn <- (current \ "firstName").readNullable[String]
      ln <- (current \ "lastName").readNullable[String]
      n  <- (JsPath \ "ids" \ "nino").readNullable[String]
    } yield Citizen(fn, ln, n)
  }
}

@Singleton
class CitizenDetailsConnector @Inject()(http: HttpClient)(implicit val appConfig: AppConfig, metrics: Metrics)
    extends HttpAPIMonitor {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  def getCitizenDetails(nino: Nino)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Citizen] =
    monitor(s"ConsumedAPI-CitizenDetails-GET") {
      val url = s"${appConfig.cidBaseUrl}/citizen-details/nino/${nino.value}"
      http.GET[HttpResponse](url).map { r =>
        r.status match {
          case OK        => r.json.as[Citizen]
          case NOT_FOUND => Citizen(None, None, None)
          case s         => throw new RuntimeException(s"unexpected error during getCitizenDetails, status: $s")
        }
      }
    }
}
