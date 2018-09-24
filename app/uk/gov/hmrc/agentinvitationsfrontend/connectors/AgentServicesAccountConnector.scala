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
import play.api.Logger
import play.api.libs.json.{JsObject, JsPath, Reads}
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ContinueUrlActions
import uk.gov.hmrc.agentinvitationsfrontend.models.CustomerDetails
import uk.gov.hmrc.agentmtdidentifiers.model.{MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.{HeaderCarrier, HttpGet, NotFoundException}

import scala.concurrent.{ExecutionContext, Future}

case class AgencyName(name: Option[String])

case class AgencyNameNotFound() extends Exception

object AgencyName {
  implicit val nameReads: Reads[AgencyName] =
    (JsPath \ "agencyName").readNullable[String].map(AgencyName(_))
}

@Singleton
class AgentServicesAccountConnector @Inject()(
  @Named("agent-services-account-baseUrl") baseUrl: URL,
  http: HttpGet,
  continueUrlActions: ContinueUrlActions,
  metrics: Metrics)
    extends HttpAPIMonitor {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  def getAgencyName(arn: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    monitor(s"ConsumedAPI-Get-AgencyName-GET") {
      http.GET[AgencyName](new URL(baseUrl, s"/agent-services-account/client/agency-name/$arn").toString).map(_.name)
    } recoverWith {
      case _: NotFoundException => Future failed AgencyNameNotFound()
    }

  def getTradingName(nino: Nino)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    monitor(s"ConsumedAPI-Get-TradingName-POST") {
      http
        .GET[JsObject](craftUrl(getTradingNameWithNino(nino)).toString)
        .map(obj => (obj \ "tradingName").asOpt[String])
    }.recover {
      case _: NotFoundException => None
    }

  def getCustomerDetails(vrn: Vrn)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[CustomerDetails] =
    monitor(s"ConsumedAPI-Get-VatOrgName-POST") {
      http
        .GET[CustomerDetails](craftUrl(getCustomerDetailsWithVrn(vrn)).toString)
    }.recover {
      case _: NotFoundException => CustomerDetails(None, None, None)
    }

  def getNinoForMtdItId(mtdItId: MtdItId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Nino]] = {
    monitor(s"ConsumedAPI-Get-NinoForMtdItId-GET") {
      http.GET[JsObject](craftUrl(getNinoForMtdItIdUrl(mtdItId)).toString)
        .map(obj => (obj \ "nino").asOpt[Nino])
    }.recover {
      case e => {
        Logger(getClass).error(s"Unable to translate MtdItId: ${e.getMessage}")
        None
      }
    }
  }

  private def craftUrl(location: String) = new URL(baseUrl, location)

  private def getTradingNameWithNino(nino: Nino): String =
    s"/agent-services-account/client/trading-name/nino/${nino.value}"

  private def getCustomerDetailsWithVrn(vrn: Vrn): String =
    s"/agent-services-account/client/vat-customer-details/vrn/${vrn.value}"

  private def getNinoForMtdItIdUrl(mtdItId: MtdItId): String =
    s"/agent-services-account/client/mtdItId/${mtdItId.value}"
}
