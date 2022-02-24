/*
 * Copyright 2022 HM Revenue & Customs
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
import play.api.Logging
import play.api.http.Status._
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HttpClient, _}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class RelationshipsConnector @Inject()(http: HttpClient, featureFlags: FeatureFlags)(implicit appConfig: AppConfig, metrics: Metrics)
    extends HttpAPIMonitor with Logging {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  val baseUrl = new URL(appConfig.acrBaseUrl)

  // TODO when Service becomes a sealed trait, move these to that class
  private val serviceShortNames: Map[Service, String] = Map(
    Service.MtdIt        -> "Itsa",
    Service.Vat          -> "VAT",
    Service.Trust        -> "Trust",
    Service.TrustNT      -> "TrustNT",
    Service.CapitalGains -> "Cgt",
    Service.Ppt          -> "Ppt"
  )

  private val serviceIdentifierTypes: Map[Service, String] = Map(
    Service.MtdIt        -> "NI",
    Service.Vat          -> "VRN",
    Service.Trust        -> "SAUTR",
    Service.TrustNT      -> "URN",
    Service.CapitalGains -> "CGTPDRef",
    Service.Ppt          -> "EtmpRegistrationNumber"
  )

  def isServiceEnabled(service: Service): Boolean = service match {
    case Service.MtdIt                => featureFlags.showHmrcMtdIt
    case Service.Vat                  => featureFlags.showHmrcMtdVat
    case Service.Trust                => featureFlags.showHmrcTrust
    case Service.TrustNT              => featureFlags.showHmrcTrust
    case Service.CapitalGains         => featureFlags.showHmrcCgt
    case Service.Ppt                  => featureFlags.showPlasticPackagingTax
    case Service.PersonalIncomeRecord => featureFlags.showPersonalIncome
  }

  private val inactiveRelationshipUrl: String = s"$baseUrl/agent-client-relationships/agent/relationships/inactive"

  private def getRelationshipUrlFor(service: Service, arn: Arn, identifier: TaxIdentifier): String =
    new URL(
      baseUrl,
      s"/agent-client-relationships/agent/${arn.value}/service" +
        s"/${service.id}/client/${serviceIdentifierTypes(service)}/${identifier.value}").toString

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

  private def deleteRelationshipForService(service: Service, arn: Arn, identifier: TaxIdentifier)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] =
    if (isServiceEnabled(service)) {
      monitor(s"ConsumedAPI-DELETE-${serviceShortNames(service)}Relationship-DELETE") {
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
      logger.warn(s"${serviceShortNames(service)} is disabled - cannot delete relationship")
      Future successful None
    }

  def deleteRelationshipItsa(arn: Arn, nino: Nino)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Boolean]] =
    deleteRelationshipForService(Service.MtdIt, arn, nino)

  def deleteRelationshipVat(arn: Arn, vrn: Vrn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Boolean]] =
    deleteRelationshipForService(Service.Vat, arn, vrn)

  def deleteRelationshipTrust(arn: Arn, utr: Utr)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Boolean]] =
    deleteRelationshipForService(Service.Trust, arn, utr)

  def deleteRelationshipTrustNT(arn: Arn, urn: Urn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Boolean]] =
    deleteRelationshipForService(Service.TrustNT, arn, urn)

  def deleteRelationshipCgt(arn: Arn, ref: CgtRef)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Boolean]] =
    deleteRelationshipForService(Service.CapitalGains, arn, ref)

  def deleteRelationshipPpt(arn: Arn, ref: PptRef)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Boolean]] =
    deleteRelationshipForService(Service.Ppt, arn, ref)

  private def checkRelationship(service: Service, arn: Arn, identifier: TaxIdentifier)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Boolean] =
    if (isServiceEnabled(service)) {
      monitor("ConsumedApi-Get-CheckItsaRelationship-GET") {
        val url = getRelationshipUrlFor(service, arn, identifier)
        http
          .GET[HttpResponse](url)
          .map { r =>
            r.status match {
              case OK        => true
              case NOT_FOUND => false
            }
          }
      }
    } else {
      logger.warn(s"${serviceShortNames(service)} is disabled - cannot check relationships")
      Future successful false
    }

  def checkItsaRelationship(arn: Arn, nino: Nino)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    checkRelationship(Service.MtdIt, arn, nino)

  def checkVatRelationship(arn: Arn, vrn: Vrn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    checkRelationship(Service.Vat, arn, vrn)

  def checkTrustRelationship(arn: Arn, utr: Utr)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    checkRelationship(Service.Trust, arn, utr)

  def checkTrustNTRelationship(arn: Arn, urn: Urn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    checkRelationship(Service.TrustNT, arn, urn)

  def checkCgtRelationship(arn: Arn, ref: CgtRef)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    checkRelationship(Service.CapitalGains, arn, ref)

  def checkPptRelationship(arn: Arn, ref: PptRef)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    checkRelationship(Service.Ppt, arn, ref)

}
