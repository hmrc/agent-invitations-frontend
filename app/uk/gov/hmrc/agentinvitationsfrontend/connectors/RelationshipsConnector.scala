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

import java.net.URL

import com.codahale.metrics.MetricRegistry
import com.kenshoo.play.metrics.Metrics
import javax.inject.{Inject, Singleton}
import play.api.Logger
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, CgtRef, Utr, Vrn}
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.http._
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class RelationshipsConnector @Inject()(http: HttpClient, featureFlags: FeatureFlags)(
  implicit appConfig: AppConfig,
  metrics: Metrics)
    extends HttpAPIMonitor {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  val baseUrl = new URL(appConfig.acrBaseUrl)

  // TODO when Service becomes a sealed trait, move these to that class
  private val serviceShortNames = Map(
    "HMRC-MTD-IT"   -> "Itsa",
    "HMRC-MTD-VAT"  -> "VAT",
    "HMRC-TERS-ORG" -> "Trust",
    "HMRC-CGT-PD"   -> "Cgt"
  )

  private val serviceIdentifierTypes = Map(
    "HMRC-MTD-IT"   -> "NI",
    "HMRC-MTD-VAT"  -> "VRN",
    "HMRC-TERS-ORG" -> "SAUTR",
    "HMRC-CGT-PD"   -> "CGTPDRef"
  )

  def isServiceEnabled(service: String): Boolean = service match {
    case "HMRC-MTD-IT"            => featureFlags.showHmrcMtdIt
    case "HMRC-MTD-VAT"           => featureFlags.showHmrcMtdVat
    case "HMRC-TERS-ORG"          => featureFlags.showHmrcTrust
    case "HMRC-CGT-PD"            => featureFlags.showHmrcCgt
    case "PERSONAL-INCOME-RECORD" => featureFlags.showPersonalIncome
    case _                        => false // unknown service
  }

  private val inactiveRelationshipUrl: String = s"$baseUrl/agent-client-relationships/agent/relationships/inactive"

  private def getRelationshipUrlFor(service: String, arn: Arn, identifier: TaxIdentifier): String =
    new URL(
      baseUrl,
      s"/agent-client-relationships/agent/${arn.value}/service" +
        s"/$service/client/${serviceIdentifierTypes(service)}/${identifier.value}").toString

  def getInactiveRelationships(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext,
    evidence: HttpReads[Seq[InactiveTrackRelationship]]): Future[Seq[InactiveTrackRelationship]] =
    monitor(s"ConsumedApi-Get-InactiveRelationships-GET") {
      http
        .GET[Seq[InactiveTrackRelationship]](inactiveRelationshipUrl)
        .recover {
          case _: NotFoundException =>
            Seq.empty
        }
    }

  private def deleteRelationshipForService(service: String, arn: Arn, identifier: TaxIdentifier)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] =
    if (isServiceEnabled(service)) {
      monitor(s"ConsumedAPI-DELETE-${serviceShortNames(service)}Relationship-DELETE") {
        val url = getRelationshipUrlFor(service, arn, identifier)

        http.DELETE[HttpResponse](url).map(_ => Some(true))
      }.recover {
        case _: NotFoundException => Some(false)
        case _                    => None
      }
    } else {
      Logger(getClass).warn(s"${serviceShortNames(service)} is disabled - cannot delete relationship")
      Future successful None
    }

  def deleteRelationshipItsa(arn: Arn, nino: Nino)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] =
    deleteRelationshipForService("HMRC-MTD-IT", arn, nino)

  def deleteRelationshipVat(arn: Arn, vrn: Vrn)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] =
    deleteRelationshipForService("HMRC-MTD-VAT", arn, vrn)

  def deleteRelationshipTrust(arn: Arn, utr: Utr)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] =
    deleteRelationshipForService("HMRC-TERS-ORG", arn, utr)

  def deleteRelationshipCgt(arn: Arn, ref: CgtRef)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] =
    deleteRelationshipForService("HMRC-CGT-PD", arn, ref)

  private def checkRelationship(service: String, arn: Arn, identifier: TaxIdentifier)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Boolean] =
    if (isServiceEnabled(service)) {
      monitor("ConsumedApi-Get-CheckItsaRelationship-GET") {
        val url = getRelationshipUrlFor(service, arn, identifier)

        http
          .GET[HttpResponse](url)
          .map(_ => true)
          .recover {
            case _: NotFoundException =>
              Logger(getClass).warn(
                s"No relationships were found for this agent and client for ${serviceShortNames(service)}")
              false
          }
      }
    } else {
      Logger.warn(s"${serviceShortNames(service)} is disabled - cannot check relationships")
      Future successful false
    }

  def checkItsaRelationship(arn: Arn, nino: Nino)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    checkRelationship("HMRC-MTD-IT", arn, nino)

  def checkVatRelationship(arn: Arn, vrn: Vrn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    checkRelationship("HMRC-MTD-VAT", arn, vrn)

  def checkTrustRelationship(arn: Arn, utr: Utr)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    checkRelationship("HMRC-TERS-ORG", arn, utr)

  def checkCgtRelationship(arn: Arn, ref: CgtRef)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    checkRelationship("HMRC-CGT-PD", arn, ref)

}
