/*
 * Copyright 2023 HM Revenue & Customs
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

import javax.inject.{Inject, Singleton}
import play.api.http.Status._
import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentinvitationsfrontend.models.IrvTrackRelationship
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Service}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HttpClient, _}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class PirRelationshipConnector @Inject()(http: HttpClient)(implicit appConfig: AppConfig, metrics: Metrics) extends HttpAPIMonitor {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  val baseUrl = new URL(appConfig.afiBaseUrl)

  val ISO_LOCAL_DATE_TIME_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS"

  def createRelationship(arn: Arn, service: String, clientId: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Int] =
    monitor(s"ConsumedAPI-Put-TestOnlyRelationship-PUT") {
      val url = craftUrl(createAndDeleteRelationshipUrl(arn, service, clientId))
      val body = Json.obj("startDate" -> LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME))
      http
        .PUT[JsObject, HttpResponse](url.toString, body)
        .map(_.status)
    }

  def deleteRelationship(arn: Arn, service: Service, clientId: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Boolean]] =
    monitor(s"ConsumedAPI-Delete-TestOnlyRelationship-DELETE") {
      val url = craftUrl(createAndDeleteRelationshipUrl(arn, service.id, clientId))
      http.DELETE[HttpResponse](url.toString).map { r =>
        r.status match {
          case OK                => Some(true)
          case NOT_FOUND         => Some(false)
          case s if s / 100 == 5 => None
        }
      }
    }

  val getInactiveIrvRelationshipUrl: URL =
    new URL(baseUrl, "/agent-fi-relationship/relationships/inactive")

  def getInactiveIrvRelationships(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Seq[IrvTrackRelationship]] =
    monitor("ConsumedApi-Get-InactiveIrvRelationships-GET") {
      http
        .GET[HttpResponse](getInactiveIrvRelationshipUrl.toString)
        .map { r =>
          r.status match {
            case OK        => r.json.as[Seq[IrvTrackRelationship]]
            case NOT_FOUND => Seq.empty
          }
        }
    }

  def getActiveIrvRelationshipUrl(arn: Arn, clientId: Nino): URL =
    new URL(baseUrl, s"/agent-fi-relationship/relationships/PERSONAL-INCOME-RECORD/agent/${arn.value}/client/${clientId.value}")

  def getPirRelationshipForAgent(arn: Arn, clientId: Nino)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[IrvTrackRelationship]] =
    monitor("ConsumedApi-Get-ActiveIrvRelationships-GET") {
      http
        .GET[HttpResponse](getActiveIrvRelationshipUrl(arn, clientId).toString)
        .map { r =>
          r.status match {
            case OK        => r.json.as[Seq[IrvTrackRelationship]].headOption
            case NOT_FOUND => None
          }
        }
    }

  private def createAndDeleteRelationshipUrl(arn: Arn, service: String, clientId: String) =
    s"/agent-fi-relationship/relationships/agent/${arn.value}/service/$service/client/$clientId"

  private def craftUrl(location: String) = new URL(baseUrl, location)

  /* TEST ONLY Connector method for create relationship. This method should not be used in production code */
  def testOnlyCreateRelationship(arn: Arn, service: String, clientId: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Int] = {
    val url = new URL(baseUrl, s"/agent-fi-relationship/test-only/relationships/agent/${arn.value}/service/$service/client/$clientId")
    val body = Json.obj("startDate" -> LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME))
    http
      .PUT[JsObject, HttpResponse](url.toString, body)
      .map(_.status)
  }

  /* TEST ONLY Connector method for delete relationship. This method should not be used in production code */
  def testOnlyDeleteRelationship(arn: Arn, service: String, clientId: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] = {
    val url = new URL(baseUrl, s"/agent-fi-relationship/test-only/relationships/agent/${arn.value}/service/$service/client/$clientId")
    http.DELETE[HttpResponse](url.toString).map { r =>
      r.status match {
        case OK                => Some(true)
        case NOT_FOUND         => Some(false)
        case s if s / 100 == 5 => None
      }
    }
  }

  def checkIrvAllowed(arn: Arn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] = {
    val url = new URL(baseUrl, s"/agent-fi-relationship/${arn.value}/irv-allowed")
    http
      .GET[HttpResponse](url.toString)
      .map {
        case HttpResponse(NO_CONTENT, _, _) => true
        case HttpResponse(NOT_FOUND, _, _)  => false
      }
  }
}
