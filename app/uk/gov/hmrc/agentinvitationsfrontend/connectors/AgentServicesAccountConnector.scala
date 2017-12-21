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
import play.api.libs.json.{JsPath, Reads}
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.http.{HeaderCarrier, HttpGet, NotFoundException}
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

import scala.concurrent.Future

case class AgencyName(name: Option[String])

case class AgencyNameNotFound() extends Exception

object AgencyName {
  implicit val nameReads: Reads[AgencyName] =
    (JsPath \ "agencyName").readNullable[String].map(AgencyName(_))
}

@Singleton
class AgentServicesAccountConnector @Inject() (
                                                @Named("agent-services-account-baseUrl") baseUrl: URL,
                                                http: HttpGet,
                                                metrics: Metrics) extends HttpAPIMonitor {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  def getAgencyName(arn: String)(implicit hc: HeaderCarrier): Future[Option[String]] =
    monitor(s"ConsumedAPI-Get-AgencyName-GET") {
      http.GET[AgencyName](new URL(baseUrl, s"/agent-services-account/client/agency-name/$arn").toString).map(_.name)
    } recoverWith {
      case _: NotFoundException => Future failed AgencyNameNotFound()
    }
}
