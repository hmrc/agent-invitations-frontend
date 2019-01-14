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
import org.joda.time.DateTime
import play.api.Logger
import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.models.IrvTrackRelationship
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class PirRelationshipConnector @Inject()(
  @Named("agent-fi-relationship-baseUrl") baseUrl: URL,
  http: HttpGet with HttpPost with HttpPut with HttpDelete,
  metrics: Metrics)
    extends HttpAPIMonitor {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  def createRelationship(arn: Arn, service: String, clientId: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Int] =
    monitor(s"ConsumedAPI-Put-TestOnlyRelationship-PUT") {
      val ISO_LOCAL_DATE_TIME_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS"
      val url = craftUrl(createAndDeleteRelationshipUrl(arn, service, clientId))
      val body = Json.obj("startDate" -> DateTime.now().toString(ISO_LOCAL_DATE_TIME_FORMAT))
      http
        .PUT[JsObject, HttpResponse](url.toString, body)
        .map(_.status)
        .recover {
          case _: Upstream5xxResponse => 500
        }
    }

  def deleteRelationship(arn: Arn, service: String, clientId: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] =
    monitor(s"ConsumedAPI-Delete-TestOnlyRelationship-DELETE") {
      val url = craftUrl(createAndDeleteRelationshipUrl(arn, service, clientId))
      http
        .DELETE[HttpResponse](url.toString)
        .map(_ => Some(true))
        .recover {
          case _: NotFoundException   => Some(false)
          case _: Upstream5xxResponse => None
        }
    }

  val getInactiveIrvRelationshipUrl: URL =
    new URL(baseUrl, "/agent-fi-relationship/relationships/inactive")

  def getInactiveIrvRelationships(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Seq[IrvTrackRelationship]] =
    monitor("ConsumedApi-Get-InactiveIrvRelationships-GET") {
      http
        .GET[Seq[IrvTrackRelationship]](getInactiveIrvRelationshipUrl.toString)
        .recover {
          case _: NotFoundException =>
            Logger(getClass).warn("No inactive relationships were found for IRV")
            Seq.empty
        }
    }

  def getActiveIrvRelationshipUrl(arn: Arn, clientId: Nino): URL =
    new URL(
      baseUrl,
      s"/agent-fi-relationship/relationships/PERSONAL-INCOME-RECORD/agent/${arn.value}/client/${clientId.value}")

  def getPirRelationshipForAgent(arn: Arn, clientId: Nino)(implicit hc: HeaderCarrier, ec: ExecutionContext) =
    monitor("ConsumedApi-Get-ActiveIrvRelationships-GET") {
      http
        .GET[Seq[IrvTrackRelationship]](getActiveIrvRelationshipUrl(arn, clientId).toString)
        .map(_.headOption)
        .recover {
          case _: NotFoundException =>
            Logger(getClass).warn("No active relationships were found for IRV")
            None
        }
    }

  private def createAndDeleteRelationshipUrl(arn: Arn, service: String, clientId: String) =
    s"/agent-fi-relationship/relationships/agent/${arn.value}/service/$service/client/$clientId"

  private def craftUrl(location: String) = new URL(baseUrl, location)

  /* TEST ONLY Connector method for create relationship. This method should not be used in production code */
  def testOnlyCreateRelationship(arn: Arn, service: String, clientId: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Int] = {
    val url = new URL(
      baseUrl,
      s"/agent-fi-relationship/test-only/relationships/agent/${arn.value}/service/$service/client/$clientId")
    val ISO_LOCAL_DATE_TIME_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS"
    val body = Json.obj("startDate" -> DateTime.now().toString(ISO_LOCAL_DATE_TIME_FORMAT))
    http
      .PUT[JsObject, HttpResponse](url.toString, body)
      .map(_.status)
      .recover {
        case _: Upstream5xxResponse => 500
      }
  }

  /* TEST ONLY Connector method for delete relationship. This method should not be used in production code */
  def testOnlyDeleteRelationship(arn: Arn, service: String, clientId: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] = {
    val url = new URL(
      baseUrl,
      s"/agent-fi-relationship/test-only/relationships/agent/${arn.value}/service/$service/client/$clientId")
    http
      .DELETE[HttpResponse](url.toString)
      .map(_ => Some(true))
      .recover {
        case _: NotFoundException   => Some(false)
        case _: Upstream5xxResponse => None
      }
  }
}
