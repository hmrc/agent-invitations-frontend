/*
 * Copyright 2021 HM Revenue & Customs
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
import org.joda.time.format.ISODateTimeFormat
import org.joda.time.{DateTime, LocalDate}
import play.api.Logging
import play.api.http.Status._
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads}
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.UriPathEncoding.encodePathSegment
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.{Nino, SimpleObjectReads}
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.controllers.RestFormats.{dateTimeFormats, localDateFormats}
import uk.gov.hmrc.http.{HttpClient, _}

import java.net.URL
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class MappingConnector @Inject()(http: HttpClient)(implicit val appConfig: AppConfig, metrics: Metrics)
    extends HttpAPIMonitor with Logging {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  val baseUrl: URL = new URL(appConfig.agentMappingUrl)

  import Reads._

  private val dateFormatter = ISODateTimeFormat.date()

  private[connectors] def getMappingUrl(arn: Arn): URL =
    new URL(baseUrl, s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent")

  private def mappingUrl(location: String) = new URL(baseUrl, location)

  private val originHeader = Seq("Origin" -> "agent-invitations-frontend")

  def getMapping(arn: Arn, agentInvitation: AgentInvitation)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    monitor(s"ConsumedAPI-Get-Agent-Mapping-GET") {
      val url = mappingUrl(location)
      http.GET[HttpResponse](createInvitationUrl(arn).toString, originHeader) map { r =>
        r.status match {
          case CREATED => r.header("location")
          case status: Int =>
            logger.warn(s"unexpected status from agent-mapping when creating invitation, status: $status")
            None
        }
      }
    }

//  def getInvitation(location: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[StoredInvitation] =
//    monitor(s"ConsumedAPI-Get-Invitation-GET") {
//      val url = invitationUrl(location)
//      http.GET[HttpResponse](url.toString).map { r =>
//        r.status match {
//          case OK => r.json.as[StoredInvitation]
//          case status: Int =>
//            logger.warn(s"unexpected status from agent-client-authorisation when getting invitation, status: $status")
//            throw new RuntimeException(s"error during getInvitation, status: $status, url: $location")
//        }
//      }
//    }




  object Reads {

    implicit val detailsForEmailReads = Json.reads[DetailsForEmail]

    implicit val reads: Reads[StoredInvitation] = {

      implicit val urlReads: SimpleObjectReads[URL] = new SimpleObjectReads[URL]("href", s => new URL(baseUrl, s))

      ((JsPath \ "arn").read[Arn] and
        (JsPath \ "clientType").readNullable[String] and
        (JsPath \ "service").read[String] and
        (JsPath \ "clientId").read[String] and
        (JsPath \ "clientIdType").read[String] and
        (JsPath \ "suppliedClientId").read[String] and
        (JsPath \ "suppliedClientIdType").read[String] and
        (JsPath \ "detailsForEmail").readNullable[DetailsForEmail] and
        (JsPath \ "status").read[String] and
        (JsPath \ "created").read[DateTime] and
        (JsPath \ "lastUpdated").read[DateTime] and
        (JsPath \ "expiryDate").read[LocalDate] and
        (JsPath \ "invitationId").read[String] and
        (JsPath \ "isRelationshipEnded").read[Boolean] and
        (JsPath \ "relationshipEndedBy").readNullable[String] and
        (JsPath \ "_links" \ "self").read[URL])(
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) => StoredInvitation.apply(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
      )
    }
  }

}
