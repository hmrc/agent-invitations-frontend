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
import org.joda.time.format.ISODateTimeFormat
import org.joda.time.{DateTime, LocalDate}
import play.api.libs.json.JsObject
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.UriPathEncoding.encodePathSegment
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentInvitation, StoredInvitation}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class InvitationsConnector @Inject()(
  @Named("agent-client-authorisation-baseUrl") baseUrl: URL,
  http: HttpGet with HttpPost with HttpPut,
  metrics: Metrics)
    extends HttpAPIMonitor {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  import Reads._

  private val dateFormatter = ISODateTimeFormat.date()

  private[connectors] def createInvitationUrl(arn: Arn): URL =
    new URL(baseUrl, s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent")

  private[connectors] def getAgencyInvitationsUrl(arn: Arn, createdOnOrAfter: LocalDate): URL =
    new URL(
      baseUrl,
      s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent?createdOnOrAfter=${dateFormatter
        .print(createdOnOrAfter)}"
    )

  private[connectors] def acceptITSAInvitationUrl(mtdItId: MtdItId, invitationId: InvitationId): URL =
    new URL(
      baseUrl,
      s"/agent-client-authorisation/clients/MTDITID/${mtdItId.value}/invitations/received/${invitationId.value}/accept")

  private[connectors] def rejectITSAInvitationUrl(mtdItId: MtdItId, invitationId: InvitationId) =
    new URL(
      baseUrl,
      s"/agent-client-authorisation/clients/MTDITID/${mtdItId.value}/invitations/received/${invitationId.value}/reject")

  private def invitationUrl(location: String) = new URL(baseUrl, location)

  def createInvitation(arn: Arn, agentInvitation: AgentInvitation)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[String]] =
    monitor(s"ConsumedAPI-Agent-Create-Invitation-POST") {
      http.POST[AgentInvitation, HttpResponse](createInvitationUrl(arn).toString, agentInvitation) map { r =>
        r.header("location")
      }
    }

  def getInvitation(location: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[StoredInvitation] =
    monitor(s"ConsumedAPI-Get-Invitation-GET") {
      val url = invitationUrl(location)
      http.GET[StoredInvitation](url.toString)
    }

  def getAllInvitations(arn: Arn, createdOnOrAfter: LocalDate)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Seq[StoredInvitation]] =
    monitor(s"ConsumedAPI-Get-AllInvitations-GET") {
      val url = getAgencyInvitationsUrl(arn, createdOnOrAfter)
      http
        .GET[JsObject](url.toString)
        .map(obj => (obj \ "_embedded" \ "invitations").as[Seq[StoredInvitation]])
    }

  def acceptITSAInvitation(mtdItId: MtdItId, invitationId: InvitationId)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Int] =
    monitor(s"ConsumedAPI-Accept-Invitation-PUT") {
      http.PUT[Boolean, HttpResponse](acceptITSAInvitationUrl(mtdItId, invitationId).toString, false).map(_.status)
    }

  def rejectITSAInvitation(mtdItId: MtdItId, invitationId: InvitationId)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Int] =
    monitor(s"ConsumedAPI-Reject-Invitation-PUT") {
      http.PUT[Boolean, HttpResponse](rejectITSAInvitationUrl(mtdItId, invitationId).toString, false).map(_.status)
    }

  private[connectors] def acceptAFIInvitationUrl(nino: Nino, invitationId: InvitationId): URL =
    new URL(
      baseUrl,
      s"/agent-client-authorisation/clients/NI/${nino.value}/invitations/received/${invitationId.value}/accept")

  private[connectors] def rejectAFIInvitationUrl(nino: Nino, invitationId: InvitationId) =
    new URL(
      baseUrl,
      s"/agent-client-authorisation/clients/NI/${nino.value}/invitations/received/${invitationId.value}/reject")

  def acceptAFIInvitation(nino: Nino, invitationId: InvitationId)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Int] =
    monitor(s"ConsumedAPI-Accept-Invitation-PUT") {
      http.PUT[Boolean, HttpResponse](acceptAFIInvitationUrl(nino, invitationId).toString, false).map(_.status)
    }

  def rejectAFIInvitation(nino: Nino, invitationId: InvitationId)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Int] =
    monitor(s"ConsumedAPI-Reject-Invitation-PUT") {
      http.PUT[Boolean, HttpResponse](rejectAFIInvitationUrl(nino, invitationId).toString, false).map(_.status)
    }

  private[connectors] def acceptVATInvitationUrl(vrn: Vrn, invitationId: InvitationId): URL =
    new URL(
      baseUrl,
      s"/agent-client-authorisation/clients/VRN/${vrn.value}/invitations/received/${invitationId.value}/accept")

  private[connectors] def rejectVATInvitationUrl(vrn: Vrn, invitationId: InvitationId) =
    new URL(
      baseUrl,
      s"/agent-client-authorisation/clients/VRN/${vrn.value}/invitations/received/${invitationId.value}/reject")

  private[connectors] def checkVatRegisteredClientUrl(vrn: Vrn, registrationDate: LocalDate) =
    new URL(
      baseUrl,
      s"/agent-client-authorisation/known-facts/organisations/vat/${vrn.value}/registration-date/${registrationDate.toString}")

  private[connectors] def checkCitizenRecordUrl(nino: Nino, dob: LocalDate) =
    new URL(baseUrl, s"/agent-client-authorisation/known-facts/individuals/${nino.value}/dob/${dob.toString}")

  private[connectors] def checkPostcodeUrl(nino: Nino, postcode: String) =
    new URL(baseUrl, s"/agent-client-authorisation/known-facts/individuals/nino/${nino.value}/sa/postcode/$postcode")

  def acceptVATInvitation(vrn: Vrn, invitationId: InvitationId)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Int] =
    monitor(s"ConsumedAPI-Accept-Invitation-PUT") {
      http.PUT[Boolean, HttpResponse](acceptVATInvitationUrl(vrn, invitationId).toString, false).map(_.status)
    }

  def rejectVATInvitation(vrn: Vrn, invitationId: InvitationId)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Int] =
    monitor(s"ConsumedAPI-Reject-Invitation-PUT") {
      http.PUT[Boolean, HttpResponse](rejectVATInvitationUrl(vrn, invitationId).toString, false).map(_.status)
    }

  def checkPostcodeForClient(nino: Nino, postcode: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] =
    monitor(s"ConsumedAPI-CheckPostcode-GET") {
      http.GET[HttpResponse](checkPostcodeUrl(nino, postcode).toString).map(_ => Some(true))
    }.recover {
      case notMatched: Upstream4xxResponse if notMatched.message.contains("POSTCODE_DOES_NOT_MATCH")         => Some(false)
      case notEnrolled: Upstream4xxResponse if notEnrolled.message.contains("CLIENT_REGISTRATION_NOT_FOUND") => None
    }

  def checkVatRegisteredClient(vrn: Vrn, registrationDateKnownFact: LocalDate)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] =
    monitor(s"ConsumedAPI-CheckVatRegDate-GET") {
      http.GET[HttpResponse](checkVatRegisteredClientUrl(vrn, registrationDateKnownFact).toString).map(_ => Some(true))
    }.recover {
      case ex: Upstream4xxResponse if ex.upstreamResponseCode == 403 => Some(false)
      case _: NotFoundException                                      => None
    }

  def checkCitizenRecord(nino: Nino, dob: LocalDate)(
    implicit headerCarrier: HeaderCarrier,
    executionContext: ExecutionContext): Future[Option[Boolean]] =
    monitor(s"ConsumedAPI-CheckCitizenRecord-GET") {
      http.GET[HttpResponse](checkCitizenRecordUrl(nino, dob).toString).map(_ => Some(true))
    }.recover {
      case ex: Upstream4xxResponse if ex.upstreamResponseCode == 403 => Some(false)
      case _: NotFoundException                                      => None
    }

  object Reads {

    import play.api.libs.functional.syntax._
    import play.api.libs.json.{JsPath, Reads}
    import uk.gov.hmrc.domain.SimpleObjectReads
    import uk.gov.hmrc.http.controllers.RestFormats.dateTimeFormats

    implicit val reads: Reads[StoredInvitation] = {

      implicit val urlReads: SimpleObjectReads[URL] = new SimpleObjectReads[URL]("href", s => new URL(baseUrl, s))

      ((JsPath \ "arn").read[Arn] and
        (JsPath \ "service").read[String] and
        (JsPath \ "clientId").read[String] and
        (JsPath \ "clientIdType").read[String] and
        (JsPath \ "suppliedClientId").read[String] and
        (JsPath \ "suppliedClientIdType").read[String] and
        (JsPath \ "status").read[String] and
        (JsPath \ "created").read[DateTime] and
        (JsPath \ "lastUpdated").read[DateTime] and
        (JsPath \ "expiryDate").read[LocalDate] and
        (JsPath \ "_links" \ "self").read[URL])(
        (a, b, c, d, e, f, g, h, i, j, k) => StoredInvitation.apply(a, b, c, d, e, f, g, h, i, j, k)
      )
    }
  }

}
