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
import play.api.libs.json.{JsPath, Reads}
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http._

import scala.concurrent.{ExecutionContext, Future}

case class Citizen(firstName: Option[String], lastName: Option[String], nino: Option[String]) {
  val name: Option[String] = (firstName, lastName) match {
    case (Some(f), Some(l)) => Some(s"$f $l")
    case _                  => None
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
class CitizenDetailsConnector @Inject()(
  @Named("citizen-details-baseUrl") baseUrl: URL,
  http: HttpGet with HttpDelete,
  metrics: Metrics)
    extends HttpAPIMonitor {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  def getCitizenDetails(nino: Nino)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Citizen] =
    monitor(s"ConsumedAPI-CitizenDetails-GET") {
      val url = new URL(baseUrl, s"/citizen-details/nino/${nino.value}")
      http.GET[Citizen](url.toString).recover {
        case _: NotFoundException => Citizen(None, None, None)
      }
    }
}
