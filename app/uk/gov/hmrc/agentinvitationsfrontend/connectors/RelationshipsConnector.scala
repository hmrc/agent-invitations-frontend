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
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class RelationshipsConnector @Inject()(
  @Named("agent-client-relationships-baseUrl") baseUrl: URL,
  http: HttpGet with HttpDelete,
  metrics: Metrics)
    extends HttpAPIMonitor {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  val getInactiveItsaRelationshipUrl: URL =
    new URL(baseUrl, "/agent-client-relationships/relationships/inactive/service/HMRC-MTD-IT")

  val getInactiveVatRelationshipUrl: URL =
    new URL(baseUrl, "/agent-client-relationships/relationships/inactive/service/HMRC-MTD-VAT")

  def deleteRelationshipItsaUrl(arn: Arn, nino: Nino): URL =
    new URL(baseUrl, s"/agent-client-relationships/agent/${arn.value}/service/HMRC-MTD-IT/client/NI/${nino.value}")

  def deleteRelationshipVatUrl(arn: Arn, vrn: Vrn): URL =
    new URL(baseUrl, s"/agent-client-relationships/agent/${arn.value}/service/HMRC-MTD-VAT/client/VRN/${vrn.value}")

  def getInactiveItsaRelationships(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Seq[ItsaTrackRelationship]] =
    monitor("ConsumedApi-Get-InactiveItsaRelationships-GET") {
      http
        .GET[Seq[ItsaTrackRelationship]](getInactiveItsaRelationshipUrl.toString)
        .recover {
          case _: NotFoundException =>
            Logger(getClass).warn("No inactive relationships were found for ITSA")
            Seq.empty
        }
    }

  def getInactiveVatRelationships(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Seq[VatTrackRelationship]] =
    monitor("ConsumedApi-Get-InactiveVatRelationships-GET") {
      http
        .GET[Seq[VatTrackRelationship]](getInactiveVatRelationshipUrl.toString)
        .recover {
          case _: NotFoundException =>
            Logger(getClass).warn("No inactive relationships were found for VAT")
            Seq.empty
        }
    }

  def deleteRelationshipItsa(arn: Arn, nino: Nino)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] =
    monitor("ConsumedAPI-DELETE-ItsaRelationship-DELETE") {
      http.DELETE(deleteRelationshipItsaUrl(arn, nino).toString).map(_ => Some(true))
    }.recover {
      case _: NotFoundException => Some(false)
      case _                    => None
    }

  def deleteRelationshipVat(arn: Arn, vrn: Vrn)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] =
    monitor("ConsumedAPI-DELETE-VatRelationship-DELETE") {
      http.DELETE(deleteRelationshipVatUrl(arn, vrn).toString).map(_ => Some(true))
    }.recover {
      case _: NotFoundException => {
        Some(false)
      }
      case _ => {
        None
      }
    }
}
