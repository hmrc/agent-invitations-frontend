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

import com.codahale.metrics.MetricRegistry
import com.kenshoo.play.metrics.Metrics
import play.api.Logging
import play.api.http.Status._
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.TaxIdentifier
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HttpClient, _}

import java.net.URL
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class RelationshipsConnector @Inject()(http: HttpClient, featureFlags: FeatureFlags)(implicit appConfig: AppConfig, metrics: Metrics)
    extends HttpAPIMonitor with Logging {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  val baseUrl = new URL(appConfig.acrBaseUrl)

  /* TODO [Service onboarding]
     another pattern match which could be centralised if it weren't for annoying variations that prevent it.
     Maybe we should relax the requirement for an exact match in agent-client-relationships
     (so utr/UTR/SAUTR is treated as the same, likewise ni/NI/NINO etc.)
     And decide once and for all WHICH is the variation we use */
  private def serviceIdentifierType(service: Service): String = service match {
    case Service.MtdIt => "NI" // TODO - any good reason why we are using "NI" instead of "NINO"?
    case service       => service.supportedSuppliedClientIdType.enrolmentId
  }

  def isServiceEnabled(service: Service): Boolean = featureFlags.isServiceEnabled(service)

  private val inactiveRelationshipUrl: String = s"$baseUrl/agent-client-relationships/agent/relationships/inactive"

  private def getRelationshipUrlFor(service: Service, arn: Arn, identifier: TaxIdentifier): String =
    new URL(
      baseUrl,
      s"/agent-client-relationships/agent/${arn.value}/service" +
        s"/${service.id}/client/${serviceIdentifierType(service)}/${identifier.value}").toString

  private def hasMappedLegacyRelationshipUrlFor(arn: Arn, nino: String): String =
    s"$baseUrl/agent-client-relationships/agent/${arn.value}/client/$nino/legacy-mapped-relationship"

  def getInactiveRelationships(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Seq[InactiveTrackRelationship]] =
    monitor(s"ConsumedApi-Get-InactiveRelationships-GET") {
      http
        .GET[HttpResponse](inactiveRelationshipUrl)
        .map { r =>
          r.status match {
            case OK        => r.json.as[Seq[InactiveTrackRelationship]]
            case NOT_FOUND => Seq.empty
            case other     => throw new RuntimeException(s"unexpected $other error when calling 'getInactiveRelationships'")
          }
        }
    }

  def getLegacySaRelationshipStatusFor(arn: Arn, nino: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[LegacySaRelationshipResult] =
    monitor(s"ConsumedApi-Get-getLegacyRelationshipStatusFor-GET") {
      http
        .GET[HttpResponse](hasMappedLegacyRelationshipUrlFor(arn, nino))
        .map { r =>
          r.status match {
            case NO_CONTENT => LegacySaRelationshipFoundAndMapped
            case OK         => LegacySaRelationshipFoundNotMapped
            case NOT_FOUND  => LegacySaRelationshipNotFound
            case other      => throw new RuntimeException(s"unexpected $other error when calling 'getLegacySaRelationshipStatusFor'")
          }
        }
    }

  def deleteRelationshipForService(service: Service, arn: Arn, identifier: TaxIdentifier)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] = {
    require(service != Service.PersonalIncomeRecord) // Note that we need to go a different route for the PIR service
    if (isServiceEnabled(service)) {
      monitor(s"ConsumedAPI-DELETE-$service-Relationship-DELETE") {
        val url = getRelationshipUrlFor(service, arn, identifier)
        http.DELETE[HttpResponse](url).map { r =>
          r.status match {
            case NO_CONTENT => Some(true)
            case NOT_FOUND  => Some(false)
            case _          => None
          }
        }
      }
    } else {
      logger.warn(s"$service is disabled - cannot delete relationship")
      Future successful None
    }
  }

  def checkRelationship(service: Service, arn: Arn, identifier: TaxIdentifier)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    if (isServiceEnabled(service)) {
      monitor("ConsumedApi-Get-CheckRelationship-GET") {
        val url = getRelationshipUrlFor(service, arn, identifier)
        http
          .GET[HttpResponse](url)
          .map { r =>
            r.status match {
              case OK        => true
              case NOT_FOUND => false
              case _ =>
                throw UpstreamErrorResponse(
                  s"Could not check whether a $service relationship exists for $arn: upstream status code ${r.status}",
                  r.status)
            }
          }
      }
    } else {
      logger.warn(s"$service is disabled - cannot check relationships")
      Future successful false
    }
}
