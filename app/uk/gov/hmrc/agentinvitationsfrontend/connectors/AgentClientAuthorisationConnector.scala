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
import uk.gov.hmrc.agentinvitationsfrontend.models.VatKnownFactCheckResult._
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
class AgentClientAuthorisationConnector @Inject()(http: HttpClient)(implicit val appConfig: AppConfig, metrics: Metrics)
    extends HttpAPIMonitor with Logging {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  val baseUrl: URL = new URL(appConfig.agentClientAuthorisationBaseUrl)

  import Reads._

  private val dateFormatter = ISODateTimeFormat.date()

  private[connectors] def createInvitationUrl(arn: Arn): URL =
    new URL(baseUrl, s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent")

  private[connectors] def createAgentLinkUrl(arn: Arn, clientType: String): URL =
    new URL(baseUrl, s"/agent-client-authorisation/agencies/references/arn/${encodePathSegment(arn.value)}/clientType/$clientType")

  private[connectors] def getAgentReferenceRecordUrl(uid: String): URL =
    new URL(baseUrl, s"/agent-client-authorisation/agencies/references/uid/$uid")

  private[connectors] def getAgentReferenceRecordUrl(arn: Arn): URL =
    new URL(baseUrl, s"/agent-client-authorisation/agencies/references/arn/${arn.value}")

  private[connectors] def getAgencyInvitationsUrl(arn: Arn, createdOnOrAfter: LocalDate): URL =
    new URL(
      baseUrl,
      s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent?createdOnOrAfter=${dateFormatter
        .print(createdOnOrAfter)}"
    )

  private[connectors] def getAllPendingInvitationsForClientUrl(arn: Arn, clientId: String, service: String): URL =
    new URL(
      baseUrl,
      s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent?status=Pending&clientId=$clientId&service=$service"
    )

  private[connectors] def getAcceptedInvitationsForClientUrl(arn: Arn, clientId: String, service: String): URL =
    new URL(
      baseUrl,
      s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent?clientId=$clientId&service=$service"
    )

  private[connectors] def getAltItsaInvitationsForClientUrl(arn: Arn, clientId: String): URL =
    new URL(
      baseUrl,
      s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent?status=PartialAuth&clientId=$clientId&service=HMRC-MTD-IT"
    )

  private[connectors] def getAgentInvitationUrl(invitationId: InvitationId): URL =
    new URL(baseUrl, s"/agent-client-authorisation/invitations/${invitationId.value}")

  private[connectors] def acceptITSAInvitationUrl(mtdItId: MtdItId, invitationId: InvitationId): URL =
    new URL(baseUrl, s"/agent-client-authorisation/clients/MTDITID/${mtdItId.value}/invitations/received/${invitationId.value}/accept")

  private[connectors] def acceptAltITSAInvitationUrl(nino: Nino, invitationId: InvitationId): URL =
    new URL(baseUrl, s"/agent-client-authorisation/clients/NI/${nino.value}/invitations/received/${invitationId.value}/accept")

  private[connectors] def rejectITSAInvitationUrl(mtdItId: MtdItId, invitationId: InvitationId) =
    new URL(baseUrl, s"/agent-client-authorisation/clients/MTDITID/${mtdItId.value}/invitations/received/${invitationId.value}/reject")

  private[connectors] def rejectAltITSAInvitationUrl(nino: Nino, invitationId: InvitationId) =
    new URL(baseUrl, s"/agent-client-authorisation/clients/NI/${nino.value}/invitations/received/${invitationId.value}/reject")

  private[connectors] def acceptAFIInvitationUrl(nino: Nino, invitationId: InvitationId): URL =
    new URL(baseUrl, s"/agent-client-authorisation/clients/NI/${nino.value}/invitations/received/${invitationId.value}/accept")

  private[connectors] def rejectAFIInvitationUrl(nino: Nino, invitationId: InvitationId) =
    new URL(baseUrl, s"/agent-client-authorisation/clients/NI/${nino.value}/invitations/received/${invitationId.value}/reject")

  private[connectors] def acceptVATInvitationUrl(vrn: Vrn, invitationId: InvitationId): URL =
    new URL(baseUrl, s"/agent-client-authorisation/clients/VRN/${vrn.value}/invitations/received/${invitationId.value}/accept")

  private[connectors] def rejectVATInvitationUrl(vrn: Vrn, invitationId: InvitationId) =
    new URL(baseUrl, s"/agent-client-authorisation/clients/VRN/${vrn.value}/invitations/received/${invitationId.value}/reject")

  private[connectors] def cancelInvitationUrl(arn: Arn, invitationId: InvitationId) =
    new URL(baseUrl, s"/agent-client-authorisation/agencies/${arn.value}/invitations/sent/${invitationId.value}/cancel")

  private[connectors] def setRelationshipEndedUrl(invitationId: InvitationId, endedBySource: String = "Agent") =
    new URL(s"$baseUrl/agent-client-authorisation/invitations/${invitationId.value}/relationship-ended?endedBy=$endedBySource")

  private[connectors] def checkVatRegisteredClientUrl(vrn: Vrn, registrationDate: LocalDate) =
    new URL(baseUrl, s"/agent-client-authorisation/known-facts/organisations/vat/${vrn.value}/registration-date/${registrationDate.toString}")

  private[connectors] def checkCitizenRecordUrl(nino: Nino, dob: LocalDate) =
    new URL(baseUrl, s"/agent-client-authorisation/known-facts/individuals/${nino.value}/dob/${dob.toString}")

  private[connectors] def checkPostcodeUrl(nino: Nino, postcode: String) =
    new URL(baseUrl, s"/agent-client-authorisation/known-facts/individuals/nino/${nino.value}/sa/postcode/$postcode")

  private[connectors] def getAllInvitationDetailsUrl(uid: String) =
    new URL(baseUrl, s"/agent-client-authorisation/clients/invitations/uid/$uid")

  private[connectors] def putAltItsaAuthorisationUrl(arn: Arn) =
    new URL(baseUrl, s"/agent-client-authorisation/agent/alt-itsa/update/${arn.value}")

  private def invitationUrl(location: String) = new URL(baseUrl, location)

  private val originHeader = Seq("Origin" -> "agent-invitations-frontend")

  def createInvitation(arn: Arn, agentInvitation: AgentInvitation)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    monitor(s"ConsumedAPI-Agent-Create-Invitation-POST") {
      http.POST[AgentInvitation, HttpResponse](createInvitationUrl(arn).toString, agentInvitation, originHeader) map { r =>
        r.status match {
          case CREATED => r.header("location")
          case status: Int =>
            logger.warn(s"unexpected status from agent-client-authorisation when creating invitation, status: $status")
            None
        }
      }
    }

  def createAgentLink(arn: Arn, clientType: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    monitor(s"ConsumedAPI-Agent-Create-AgentLink-POST") {
      http.POST[Boolean, HttpResponse](createAgentLinkUrl(arn, clientType).toString, false) map { r =>
        r.status match {
          case CREATED => r.header("location")
          case status: Int =>
            logger.warn(s"unexpected status from agent-client-authorisation when creating agent link, status: $status")
            None
        }
      }
    }

  def getAgentReferenceRecord(uid: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[AgentReferenceRecord]] =
    monitor("ConsumedAPI-Client-Get-AgentReferenceRecordByUid-GET") {
      http.GET[HttpResponse](getAgentReferenceRecordUrl(uid).toString).map { r =>
        r.status match {
          case OK => r.json.asOpt[AgentReferenceRecord]
          case status: Int =>
            logger.warn(s"unexpected status from agent-client-authorisation when getting agency reference record, status: $status")
            None
        }
      }
    }

  def getAgentReferenceRecord(arn: Arn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[SimplifiedAgentReferenceRecord] =
    monitor("ConsumedAPI-Client-Get-AgentReferenceRecordByArn-GET") {
      http.GET[HttpResponse](getAgentReferenceRecordUrl(arn).toString).map { r =>
        r.status match {
          case OK => r.json.as[SimplifiedAgentReferenceRecord]
          case status: Int =>
            logger.warn(s"unexpected status from agent-client-authorisation when getting agency reference record, status: $status")
            throw new RuntimeException("Agent reference record not found")
        }
      }
    }

  def getInvitation(location: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[StoredInvitation] =
    monitor(s"ConsumedAPI-Get-Invitation-GET") {
      val url = invitationUrl(location)
      http.GET[HttpResponse](url.toString).map { r =>
        r.status match {
          case OK => r.json.as[StoredInvitation]
          case status: Int =>
            logger.warn(s"unexpected status from agent-client-authorisation when getting invitation, status: $status")
            throw new RuntimeException(s"error during getInvitation, status: $status, url: $location")
        }
      }
    }

  def getInvitation(invitationId: InvitationId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[StoredInvitation]] =
    monitor(s"ConsumedAPI-Get-AgentInvitation-GET") {
      http.GET[HttpResponse](getAgentInvitationUrl(invitationId).toString).map { r =>
        r.status match {
          case OK => r.json.asOpt[StoredInvitation]
          case status: Int =>
            logger.warn(s"unexpected status from agent-client-authorisation when getting invitation, status: $status")
            None
        }
      }
    }

  def getAllInvitations(arn: Arn, createdOnOrAfter: LocalDate)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Seq[StoredInvitation]] =
    monitor(s"ConsumedAPI-Get-AllInvitations-GET") {
      val url = getAgencyInvitationsUrl(arn, createdOnOrAfter)
      http.GET[HttpResponse](url.toString).map { r =>
        r.status match {
          case OK => (r.json \ "_embedded" \ "invitations").as[Seq[StoredInvitation]]
          case status: Int =>
            logger.warn(s"unexpected status from agent-client-authorisation when getAllInvitations, status: $status")
            Seq.empty
        }
      }
    }

  def getAllPendingInvitationsForClient(arn: Arn, clientId: String, service: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Seq[StoredInvitation]] =
    monitor(s"ConsumedAPI-Get-AllInvitations-GET") {
      val url = getAllPendingInvitationsForClientUrl(arn, clientId, service)
      http.GET[HttpResponse](url.toString).map { r =>
        r.status match {
          case OK => (r.json \ "_embedded" \ "invitations").as[Seq[StoredInvitation]]
          case status: Int =>
            logger.warn(s"unexpected status from agent-client-authorisation when getAllPendingInvitationsForClient, status: $status")
            Seq.empty
        }
      }
    }

  def getPartialAuthorisationsForClient(arn: Arn, clientId: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Seq[StoredInvitation]] =
    monitor(s"ConsumedAPI-Get-PartialAuthorisation-GET") {
      val url = getAltItsaInvitationsForClientUrl(arn, clientId)
      http.GET[HttpResponse](url.toString).map { r =>
        r.status match {
          case OK => (r.json \ "_embedded" \ "invitations").as[Seq[StoredInvitation]]
          case status: Int =>
            logger.warn(s"unexpected status from agent-client-authorisation when getPartialAuthorisationsForClient, status: $status")
            Seq.empty
        }
      }
    }

  def getInvitationsForClient(arn: Arn, clientId: String, service: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Seq[StoredInvitation]] =
    monitor(s"ConsumedAPI-Get-MostRecentAcceptedInvitation-GET") {
      val url = getAcceptedInvitationsForClientUrl(arn, clientId, service)
      http.GET[HttpResponse](url.toString).map { r =>
        r.status match {
          case OK => (r.json \ "_embedded" \ "invitations").as[Seq[StoredInvitation]]
          case status: Int =>
            logger.warn(s"unexpected status from agent-client-authorisation when getInvitationsForClient, status: $status")
            Seq.empty
        }
      }
    }

  def acceptITSAInvitation(mtdItId: MtdItId, invitationId: InvitationId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    monitor(s"ConsumedAPI-Accept-Invitation-PUT") {
      http
        .PUT[Boolean, HttpResponse](acceptITSAInvitationUrl(mtdItId, invitationId).toString, false)
        .map(_.status == 204)
    }.recover {
      case e =>
        logger.error(s"Create ITSA Relationship Failed: ${e.getMessage}")
        false
    }

  def acceptAltITSAInvitation(nino: Nino, invitationId: InvitationId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    monitor(s"ConsumedAPI-Accept-Invitation-PUT") {
      http
        .PUT[Boolean, HttpResponse](acceptAltITSAInvitationUrl(nino, invitationId).toString, false)
        .map(_.status == 204)
    }.recover {
      case e =>
        logger.error(s"Create Alt-ITSA Relationship Failed: ${e.getMessage}")
        false
    }

  def rejectITSAInvitation(mtdItId: MtdItId, invitationId: InvitationId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    monitor(s"ConsumedAPI-Reject-Invitation-PUT") {
      http
        .PUT[Boolean, HttpResponse](rejectITSAInvitationUrl(mtdItId, invitationId).toString, false)
        .map(_.status == 204)
    }.recover {
      case e =>
        logger.error(s"Reject ITSA Invitation Failed: ${e.getMessage}")
        false
    }

  def rejectAltITSAInvitation(nino: Nino, invitationId: InvitationId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    monitor(s"ConsumedAPI-Reject-Invitation-PUT") {
      http
        .PUT[Boolean, HttpResponse](rejectAltITSAInvitationUrl(nino, invitationId).toString, false)
        .map(_.status == 204)
    }.recover {
      case e =>
        logger.error(s"Reject Alt-ITSA Invitation Failed: ${e.getMessage}")
        false
    }

  def acceptAFIInvitation(nino: Nino, invitationId: InvitationId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    monitor(s"ConsumedAPI-Accept-Invitation-PUT") {
      http.PUT[Boolean, HttpResponse](acceptAFIInvitationUrl(nino, invitationId).toString, false).map(_.status == 204)
    }.recover {
      case e =>
        logger.error(s"Create IRV Relationship Failed: ${e.getMessage}")
        false
    }

  def rejectAFIInvitation(nino: Nino, invitationId: InvitationId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    monitor(s"ConsumedAPI-Reject-Invitation-PUT") {
      http.PUT[Boolean, HttpResponse](rejectAFIInvitationUrl(nino, invitationId).toString, false).map(_.status == 204)
    }.recover {
      case e =>
        logger.error(s"Reject IRV Invitation Failed: ${e.getMessage}")
        false
    }

  def acceptVATInvitation(vrn: Vrn, invitationId: InvitationId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    monitor(s"ConsumedAPI-Accept-Invitation-PUT") {
      http.PUT[Boolean, HttpResponse](acceptVATInvitationUrl(vrn, invitationId).toString, false).map(_.status == 204)
    }.recover {
      case e =>
        logger.error(s"Create VAT Relationship Failed: ${e.getMessage}")
        false
    }

  def rejectVATInvitation(vrn: Vrn, invitationId: InvitationId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    monitor(s"ConsumedAPI-Reject-Invitation-PUT") {
      http.PUT[Boolean, HttpResponse](rejectVATInvitationUrl(vrn, invitationId).toString, false).map(_.status == 204)
    }.recover {
      case e =>
        logger.error(s"Reject VAT Invitation Failed: ${e.getMessage}")
        false
    }

  def acceptTrustInvitation(trustTaxIdentifier: TrustTaxIdentifier, invitationId: InvitationId)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Boolean] =
    monitor(s"ConsumedAPI-Accept-Invitation-PUT") {
      val identifier = trustTaxIdentifier match {
        case Utr(_) => "UTR"
        case Urn(_) => "URN"
      }
      val url = new URL(
        baseUrl,
        s"/agent-client-authorisation/clients/$identifier/${trustTaxIdentifier.value}/invitations/received/${invitationId.value}/accept").toString
      http.PUT[Boolean, HttpResponse](url, false).map(_.status == 204)
    }.recover {
      case e =>
        logger.error(s"Create Trust Relationship Failed: ${e.getMessage}")
        false
    }

  def rejectTrustInvitation(trustTaxIdentifier: TrustTaxIdentifier, invitationId: InvitationId)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Boolean] =
    monitor(s"ConsumedAPI-Reject-Invitation-PUT") {
      val identifier = trustTaxIdentifier match {
        case Utr(_) => "UTR"
        case Urn(_) => "URN"
      }
      val url = new URL(
        baseUrl,
        s"/agent-client-authorisation/clients/$identifier/${trustTaxIdentifier.value}/invitations/received/${invitationId.value}/reject").toString
      http.PUT[Boolean, HttpResponse](url, false).map(_.status == 204)
    }.recover {
      case e =>
        logger.error(s"Reject Trust Invitation Failed: ${e.getMessage}")
        false
    }

  def acceptCgtInvitation(cgtRef: CgtRef, invitationId: InvitationId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    monitor(s"ConsumedAPI-Accept-Invitation-PUT") {
      val url =
        new URL(baseUrl, s"/agent-client-authorisation/clients/CGTPDRef/${cgtRef.value}/invitations/received/${invitationId.value}/accept").toString
      http.PUT[Boolean, HttpResponse](url, false).map(_.status == 204)
    }.recover {
      case e =>
        logger.error(s"Create Cgt Relationship Failed: ${e.getMessage}")
        false
    }

  def rejectCgtInvitation(cgtRef: CgtRef, invitationId: InvitationId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    monitor(s"ConsumedAPI-Reject-Invitation-PUT") {
      val url =
        new URL(baseUrl, s"/agent-client-authorisation/clients/CGTPDRef/${cgtRef.value}/invitations/received/${invitationId.value}/reject").toString
      http.PUT[Boolean, HttpResponse](url, false).map(_.status == 204)
    }.recover {
      case e =>
        logger.error(s"Reject CGT Invitation Failed: ${e.getMessage}")
        false
    }

  def acceptPptInvitation(pptRef: PptRef, invitationId: InvitationId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    monitor(s"ConsumedAPI-Accept-Invitation-PUT") {
      val url =
        new URL(
          baseUrl,
          s"/agent-client-authorisation/clients/EtmpRegistrationNumber/${pptRef.value}/invitations/received/${invitationId.value}/accept").toString
      http.PUT[Boolean, HttpResponse](url, false).map(_.status == 204)
    }.recover {
      case e =>
        logger.error(s"Create PPT Relationship Failed: ${e.getMessage}")
        false
    }

  def rejectPptInvitation(pptRef: PptRef, invitationId: InvitationId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    monitor(s"ConsumedAPI-Reject-Invitation-PUT") {
      val url =
        new URL(
          baseUrl,
          s"/agent-client-authorisation/clients/EtmpRegistrationNumber/${pptRef.value}/invitations/received/${invitationId.value}/reject").toString
      http.PUT[Boolean, HttpResponse](url, false).map(_.status == 204)
    }.recover {
      case e =>
        logger.error(s"Reject PPT Invitation Failed: ${e.getMessage}")
        false
    }

  def cancelInvitation(arn: Arn, invitationId: InvitationId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Boolean]] =
    monitor("ConsumedApi-Cancel-Invitation-PUT") {
      http.PUT[String, HttpResponse](cancelInvitationUrl(arn, invitationId).toString, "").map { r =>
        r.status match {
          case NO_CONTENT => Some(true)
          case NOT_FOUND  => Some(false)
          case _          => None
        }
      }
    }

  def setRelationshipEnded(invitationId: InvitationId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Boolean]] =
    monitor("ConsumedApi-Set-Relationship-Ended-PUT") {
      http.PUT[String, HttpResponse](setRelationshipEndedUrl(invitationId).toString, "").map { r =>
        r.status match {
          case NO_CONTENT => Some(true)
          case NOT_FOUND  => Some(false)
          case _          => None
        }
      }
    }

  def checkPostcodeForClient(nino: Nino, postcode: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Boolean]] =
    monitor(s"ConsumedAPI-CheckPostcode-GET") {
      http.GET[HttpResponse](checkPostcodeUrl(nino, postcode).toString).map { r =>
        r.status match {
          case NO_CONTENT                                            => Some(true)
          case _ if r.body.contains("POSTCODE_DOES_NOT_MATCH")       => Some(false)
          case _ if r.body.contains("CLIENT_REGISTRATION_NOT_FOUND") => None
          case s if s / 100 == 5 =>
            throw new RuntimeException(s"unexpected error during postcode match check, error: ${r.body}")
        }
      }
    }

  def checkVatRegisteredClient(vrn: Vrn, registrationDateKnownFact: LocalDate)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[VatKnownFactCheckResult] =
    monitor(s"ConsumedAPI-CheckVatRegDate-GET") {
      http.GET[HttpResponse](checkVatRegisteredClientUrl(vrn, registrationDateKnownFact).toString).map { r =>
        r.status match {
          case NO_CONTENT                                                           => VatKnownFactCheckOk
          case NOT_FOUND                                                            => VatDetailsNotFound
          case FORBIDDEN if r.body.contains("VAT_REGISTRATION_DATE_DOES_NOT_MATCH") => VatKnownFactNotMatched
          case FORBIDDEN if r.body.contains("VAT_RECORD_CLIENT_INSOLVENT_TRUE")     => VatRecordClientInsolvent
          case LOCKED                                                               => VatRecordMigrationInProgress
          case s if s / 100 == 5 =>
            throw new RuntimeException(s"unexpected error during vat registration date match check, error: ${r.body}")
        }
      }
    }

  def checkCitizenRecord(nino: Nino, dob: LocalDate)(
    implicit headerCarrier: HeaderCarrier,
    executionContext: ExecutionContext): Future[Option[Boolean]] =
    monitor(s"ConsumedAPI-CheckCitizenRecord-GET") {
      http.GET[HttpResponse](checkCitizenRecordUrl(nino, dob).toString).map { r =>
        r.status match {
          case NO_CONTENT => Some(true)
          case FORBIDDEN  => Some(false)
          case NOT_FOUND  => None
        }
      }

    }

  def getAllClientInvitationDetailsForAgent(uid: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Seq[InvitationDetails]] =
    monitor(s"ConsumedAPI-Get-AllInvitations-GET") {
      val url = getAllInvitationDetailsUrl(uid)
      http
        .GET[HttpResponse](url.toString)
        .map { r =>
          r.status match {
            case OK => r.json.as[Seq[InvitationDetails]]
            case _  => Seq.empty
          }
        }
    }

  def getTrustName(trustTaxIdentifier: String)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[TrustResponse] = {
    val url = new URL(baseUrl, s"/agent-client-authorisation/known-facts/organisations/trust/$trustTaxIdentifier").toString

    monitor(s"ConsumedAPI-Get-Trust-KnownFacts-GET") {
      http.GET[HttpResponse](url).map { response =>
        response.status match {
          case OK    => response.json.as[TrustResponse]
          case other => throw new RuntimeException(s"unexpected $other error when calling 'getTrustName'")
        }
      }
    }
  }

  def getCgtSubscription(cgtRef: CgtRef)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Option[CgtSubscription]] = {
    val url = new URL(baseUrl, s"/agent-client-authorisation/cgt/subscriptions/${cgtRef.value}").toString
    monitor(s"ConsumedAPI-CGTSubscription-GET") {
      http
        .GET[HttpResponse](url)
        .map { response =>
          response.status match {
            case OK => Some(response.json.as[CgtSubscription])
            case NOT_FOUND =>
              logger.warn(s"CGT Subscription not found for given cgtRef: ${cgtRef.value}")
              None
            case BAD_REQUEST =>
              logger.warn(s"BadRequest response when getting CgtSubscription for given cgtRef: ${cgtRef.value}")
              None
          }
        }
    }
  }

  def getPptSubscription(pptRef: PptRef)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Option[PptSubscription]] = {
    val url = new URL(baseUrl, s"/agent-client-authorisation/ppt/subscriptions/${pptRef.value}").toString
    monitor(s"ConsumedAPI-PPTSubscription-GET") {
      http
        .GET[HttpResponse](url)
        .map { response =>
          response.status match {
            case OK => Some(response.json.as[PptSubscription])
            case NOT_FOUND =>
              logger.warn(s"PPT Subscription not found for given pptRef: ${pptRef.value}")
              None
            case BAD_REQUEST =>
              logger.warn(s"BadRequest response when getting PptSubscription for given pptRef: ${pptRef.value}")
              None
          }
        }
    }
  }

  def getPptCustomerName(pptRef: PptRef)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] = {
    val url = new URL(baseUrl, s"/agent-client-authorisation/client/ppt-customer-name/pptref/${pptRef.value}").toString
    monitor(s"ConsumedAPI-GetPPTCustomerName-GET") {
      http
        .GET[HttpResponse](url)
        .map { response =>
          response.status match {
            case OK if (response.json \ "customerName").isDefined =>
              val customerNameField = (response.json \ "customerName")
              if (customerNameField.isEmpty) logger.warn(s"Malformed JSON received when getting customer name for pptRef: ${pptRef.value}")
              customerNameField.asOpt[String]
            case NOT_FOUND =>
              logger.warn(s"PPT Subscription not found for pptRef: ${pptRef.value}")
              None
            case BAD_REQUEST =>
              logger.warn(s"BadRequest response when getting customer name for pptRef: ${pptRef.value}")
              None
          }
        }
    }
  }

  def checkKnownFactPPT(pptClient: PptClient)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Boolean] = {
    val url = new URL(baseUrl, s"/agent-client-authorisation/known-facts/ppt/${pptClient.pptRef.value}/${pptClient.registrationDate}").toString
    monitor(s"ConsumedAPI-Get-PPT-KnownFacts-GET") {
      http
        .GET[HttpResponse](url)
        .map { response =>
          response.status match {
            case OK | NO_CONTENT => true
            case NOT_FOUND | FORBIDDEN =>
              logger.warn(s"PPT known fact check failed for pptRef: ${pptClient.pptRef.value} and registration date: ${pptClient.registrationDate}")
              false
            case BAD_REQUEST =>
              logger.warn(
                s"BadRequest response for PPT known fact check for pptRef: ${pptClient.pptRef.value} and registration date: ${pptClient.registrationDate}")
              false
          }
        }
    }
  }

  def getAgencyName(arn: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    monitor(s"ConsumedAPI-Get-AgencyName-GET") {
      http.GET[HttpResponse](s"$baseUrl/agent-client-authorisation/client/agency-name/$arn").map { r =>
        r.status match {
          case OK        => (r.json \ "agencyName").asOpt[String]
          case NOT_FOUND => throw AgencyNameNotFound()
        }
      }
    }

  def getAgencyEmail()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[String] =
    monitor("ConsumerAPI-Get-AgencyEmail-GET") {
      http
        .GET[HttpResponse](s"$baseUrl/agent-client-authorisation/agent/agency-email")
        .map(response =>
          response.status match {
            case OK         => Json.parse(response.body).as[AgencyEmail].email
            case NO_CONTENT => throw AgencyEmailNotFound("No email found in the record for this agent")
            case NOT_FOUND  => throw AgencyEmailNotFound("No record found for this agent")
        })
    }

  def getAgencySuspensionDetails()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[SuspensionDetails] =
    monitor("ConsumerAPI-Get-AgencySuspensionDetails-GET") {
      http
        .GET[HttpResponse](s"$baseUrl/agent-client-authorisation/agent/suspension-details")
        .map(response =>
          response.status match {
            case OK         => Json.parse(response.body).as[SuspensionDetails]
            case NO_CONTENT => SuspensionDetails(suspensionStatus = false, None)
            case NOT_FOUND  => throw SuspensionDetailsNotFound("No record found for this agent")
        })
    }

  def getSuspensionDetails(arn: Arn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[SuspensionDetails] =
    monitor(s"ConsumedAPI-Get-AgencySuspensionDetails-GET") {
      http
        .GET[HttpResponse](s"$baseUrl/agent-client-authorisation/client/suspension-details/${arn.value}")
        .map(response =>
          response.status match {
            case OK         => Json.parse(response.body).as[SuspensionDetails]
            case NO_CONTENT => SuspensionDetails(suspensionStatus = false, None)
            case NOT_FOUND  => throw SuspensionDetailsNotFound("No record found for this agent")
        })
    }

  def getTradingName(nino: Nino)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    monitor(s"ConsumedAPI-Get-TradingName-POST") {
      http
        .GET[HttpResponse](s"$baseUrl/agent-client-authorisation/client/trading-name/nino/${nino.value}")
        .map { r =>
          r.status match {
            case OK        => (r.json \ "tradingName").asOpt[String]
            case NOT_FOUND => None
          }
        }
    }

  def getCustomerDetails(vrn: Vrn)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[CustomerDetails] =
    monitor(s"ConsumedAPI-Get-VatOrgName-POST") {
      http
        .GET[HttpResponse](s"$baseUrl/agent-client-authorisation/client/vat-customer-details/vrn/${vrn.value}")
        .map { r =>
          r.status match {
            case OK        => r.json.as[CustomerDetails]
            case NOT_FOUND => CustomerDetails(None, None, None)
          }
        }
    }

  def getNinoForMtdItId(mtdItId: MtdItId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Nino]] =
    monitor(s"ConsumedAPI-Get-NinoForMtdItId-GET") {
      http
        .GET[HttpResponse](s"$baseUrl/agent-client-authorisation/client/mtdItId/${mtdItId.value}")
        .map { r =>
          r.status match {
            case OK => (r.json \ "nino").asOpt[Nino]
            case s =>
              logger.error(s"Unable to translate MtdItId, status: $s")
              None
          }
        }
    }

  def updateAltItsaAuthorisation(arn: Arn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    monitor(s"ConsumedAPI-updateAltItsaAuthorisation-PUT") {
      {
        http
          .PUT[Boolean, HttpResponse](putAltItsaAuthorisationUrl(arn).toString, false)
          .map(_.status == 204)
      }.recover {
        case e =>
          logger.error(s"update AlT-ITSA Relationship Failed: ${e.getMessage}")
          false
      }
    }

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
