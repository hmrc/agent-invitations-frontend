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
import com.github.blemale.scaffeine.Scaffeine
import com.kenshoo.play.metrics.Metrics
import play.api.Logging
import play.api.http.Status._
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsObject, JsPath, Json, Reads}
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.UriPathEncoding.encodePathSegment
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.models.KnownFactResult._
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.{Nino, SimpleObjectReads, TaxIdentifier}
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HttpClient, _}

import java.net.URL
import java.time.{LocalDate, LocalDateTime}
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AgentClientAuthorisationConnector @Inject()(http: HttpClient)(implicit val appConfig: AppConfig, metrics: Metrics)
    extends HttpAPIMonitor with Logging {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  private val suspensionCache: com.github.blemale.scaffeine.Cache[Arn, SuspensionDetails] =
    Scaffeine()
      .recordStats()
      .expireAfterWrite(appConfig.suspensionCacheDuration)
      .maximumSize(100000)
      .build[Arn, SuspensionDetails]()

  val baseUrl: URL = new URL(appConfig.agentClientAuthorisationBaseUrl)

  import Reads._

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
      s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent?createdOnOrAfter=${createdOnOrAfter.toString}"
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

  private[connectors] def cancelInvitationUrl(arn: Arn, invitationId: InvitationId) =
    new URL(baseUrl, s"/agent-client-authorisation/agencies/${arn.value}/invitations/sent/${invitationId.value}/cancel")

  private[connectors] def setRelationshipEndedUrl =
    new URL(s"$baseUrl/agent-client-authorisation/invitations/set-relationship-ended")

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

  def respondToInvitation(service: Service, taxId: TaxIdentifier, invitationId: InvitationId, accepted: Boolean)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Boolean] = {
    val taxIdType = (service, taxId) match {
      /* TODO [Service onboarding]
         another pattern match that could be centralised. But we still have several anomalies which make usage vary...
         e.g. alt-ITSA (MtdIt with Nino instead of MtdItId)
         also in some places we use "NI" and elsewhere "NINO" and "ni" :(
         also sometimes we use "UTR", other times "SAUTR", the upper/lowercase is inconsistent... Sort this out */
      case (Service.MtdIt, MtdItId(_))                => "MTDITID"
      case (Service.MtdIt, Nino(_))                   => "NI" // alt-ITSA
      case (Service.PersonalIncomeRecord, Nino(_))    => "NI"
      case (Service.Vat, Vrn(_))                      => "VRN"
      case (Service.Trust | Service.TrustNT, Utr(_))  => "UTR"
      case (Service.Trust | Service.TrustNT, Urn(_))  => "URN"
      case (Service.CapitalGains, CgtRef(_))          => "CGTPDRef"
      case (Service.Ppt, PptRef(_))                   => "EtmpRegistrationNumber"
      case (Service.Cbc | Service.CbcNonUk, CbcId(_)) => "cbcId"
      case (Service.Pillar2, PlrId(_))                => "PLRID"
    } // Todo above, reduce number of cases to the 'special' ones only
    val isAltItsa = (service, taxId) match {
      case (Service.MtdIt, Nino(_)) => true
      case _                        => false
    }
    val acceptReject: String = if (accepted) "accept" else "reject"
    val url = new URL(
      baseUrl,
      s"/agent-client-authorisation/clients/$taxIdType/${taxId.value}/invitations/received/${invitationId.value}/$acceptReject").toString
    monitor(s"ConsumedAPI-${acceptReject.capitalize}-Invitation-PUT") {
      http.PUT[Boolean, HttpResponse](url, false).map(_.status == 204)
    }.recover {
      case e =>
        val ifAltItsa = if (isAltItsa) "(alt-ITSA)" else ""
        logger.error(s"${acceptReject.capitalize} $service $ifAltItsa invitation failed: ${e.getMessage}")
        false
    }
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

  def setRelationshipEnded(arn: Arn, clientId: String, service: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Boolean]] =
    monitor("ConsumedApi-Set-Relationship-Ended-PUT") {
      val requestBody = SetRelationshipEndedPayload(arn, clientId, service, Some("Agent"))
      http.PUT[SetRelationshipEndedPayload, HttpResponse](setRelationshipEndedUrl, requestBody).map { r =>
        r.status match {
          case NO_CONTENT => Some(true)
          case NOT_FOUND  => Some(false)
          case _          => None
        }
      }
    }

  def checkPostcodeForClient(nino: Nino, postcode: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[KnownFactResult] =
    monitor(s"ConsumedAPI-CheckPostcode-GET") {
      http.GET[HttpResponse](checkPostcodeUrl(nino, postcode).toString).map { r =>
        r.status match {
          case NO_CONTENT                                            => Pass
          case _ if r.body.contains("POSTCODE_DOES_NOT_MATCH")       => Fail(NotMatched)
          case _ if r.body.contains("CLIENT_REGISTRATION_NOT_FOUND") => Fail(NotFound)
          case s if s / 100 == 5 =>
            logger.error(s"unexpected error during postcode match check, error: ${r.body}")
            Fail(HttpStatus(s))
        }
      }
    }

  def checkVatRegisteredClient(vrn: Vrn, registrationDateKnownFact: LocalDate)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[KnownFactResult] =
    monitor(s"ConsumedAPI-CheckVatRegDate-GET") {
      http.GET[HttpResponse](checkVatRegisteredClientUrl(vrn, registrationDateKnownFact).toString).map { r =>
        r.status match {
          case NO_CONTENT                                                           => Pass
          case NOT_FOUND                                                            => Fail(NotFound)
          case FORBIDDEN if r.body.contains("VAT_REGISTRATION_DATE_DOES_NOT_MATCH") => Fail(NotMatched)
          case FORBIDDEN if r.body.contains("VAT_RECORD_CLIENT_INSOLVENT_TRUE")     => Fail(VatClientInsolvent)
          case LOCKED                                                               => Fail(VatMigrationInProgress)
          case s if s / 100 == 5 =>
            throw new RuntimeException(s"unexpected error during vat registration date match check, error: ${r.body}")
        }
      }
    }

  def checkCitizenRecord(nino: Nino, dob: LocalDate)(
    implicit headerCarrier: HeaderCarrier,
    executionContext: ExecutionContext): Future[KnownFactResult] =
    monitor(s"ConsumedAPI-CheckCitizenRecord-GET") {
      http.GET[HttpResponse](checkCitizenRecordUrl(nino, dob).toString).map { r =>
        r.status match {
          case NO_CONTENT => Pass
          case FORBIDDEN  => Fail(NotMatched)
          case NOT_FOUND  => Fail(NotFound)
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

  def checkKnownFactPPT(pptClient: PptClient)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[KnownFactResult] = {
    val url = new URL(baseUrl, s"/agent-client-authorisation/known-facts/ppt/${pptClient.pptRef.value}/${pptClient.registrationDate}").toString
    monitor(s"ConsumedAPI-Get-PPT-KnownFacts-GET") {
      http
        .GET[HttpResponse](url)
        .map { response =>
          response.status match {
            case OK | NO_CONTENT => Pass
            case NOT_FOUND | FORBIDDEN =>
              logger.warn(s"PPT known fact check failed for pptRef: ${pptClient.pptRef.value} and registration date: ${pptClient.registrationDate}")
              Fail(NotMatched)
            case status =>
              logger.warn(
                s"$status response for PPT known fact check for pptRef: ${pptClient.pptRef.value} and registration date: ${pptClient.registrationDate}")
              Fail(HttpStatus(status))
          }
        }
    }
  }

  def checkCbcEmailKnownFact(cbcId: CbcId, email: String)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[KnownFactResult] = {
    val url = new URL(baseUrl, s"/agent-client-authorisation/known-facts/cbc/${cbcId.value}").toString
    monitor(s"ConsumedAPI-Get-CBC-KnownFacts-POST") {
      http
        .POST[JsObject, HttpResponse](url, Json.obj("email" -> email))
        .map { response =>
          response.status match {
            case OK | NO_CONTENT => Pass
            case NOT_FOUND | FORBIDDEN =>
              logger.warn(s"CBC known fact check failed for cbcId: ${cbcId.value}")
              Fail(NotMatched)
            case status =>
              logger.warn(s"Unexpected response status $status for CBC known fact check for cbcId: ${cbcId.value}")
              Fail(HttpStatus(status))
          }
        }
    }
  }

  def checkPillar2KnownFact(plrId: PlrId, registrationDate: LocalDate)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[KnownFactResult] = {
    val url = new URL(baseUrl, s"/agent-client-authorisation/known-facts/pillar2/${plrId.value}").toString
    monitor(s"ConsumedAPI-Get-Pillar2-KnownFacts-POST") {
      http
        .POST[JsObject, HttpResponse](url, Json.obj("registrationDate" -> registrationDate.toString))
        .map { response =>
          response.status match {
            case OK | NO_CONTENT => Pass
            case NOT_FOUND | FORBIDDEN =>
              logger.warn(s"Pillar2 known fact check failed for plrId: ${plrId.value}")
              Fail(NotMatched)
            case status =>
              logger.warn(s"Unexpected response status $status for Pillar2 known fact check for cbcId: ${plrId.value}")
              Fail(HttpStatus(status))
          }
        }
    }
  }

  def getPillar2Subscription(plrId: PlrId)(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Option[Pillar2Subscription]] = {
    val url = new URL(baseUrl, s"/agent-client-authorisation/pillar2/subscriptions/${plrId.value}").toString
    monitor(s"ConsumedAPI-Pillar2Subscription-GET") {
      http
        .GET[HttpResponse](url)
        .map { response =>
          response.status match {
            case OK => response.json.asOpt[Pillar2Subscription]
            case NOT_FOUND =>
              logger.warn(s"PPT Subscription not found for plrId: ${plrId.value}")
              None
            case x =>
              logger.warn(s"$x response when getting PptSubscription for plrId: ${plrId.value}")
              None
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

  // This call is cached as we are doing this check on almost every page
  def getSuspensionDetails(arn: Arn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[SuspensionDetails] =
    suspensionCache.getIfPresent(arn).map(Future.successful).getOrElse {
      monitor(s"ConsumedAPI-Get-AgencySuspensionDetails-GET") {
        http
          .GET[HttpResponse](s"$baseUrl/agent-client-authorisation/client/suspension-details/${arn.value}")
          .map { response =>
            val suspensionDetails = response.status match {
              case OK         => Json.parse(response.body).as[SuspensionDetails]
              case NO_CONTENT => SuspensionDetails(suspensionStatus = false, None)
              case NOT_FOUND  => throw SuspensionDetailsNotFound("No record found for this agent")
            }
            if (appConfig.suspensionCacheEnabled) suspensionCache.put(arn, suspensionDetails)
            suspensionDetails
          }
      }
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

  def getCbcSubscription(cbcId: CbcId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[SimpleCbcSubscription]] = {
    val url = new URL(baseUrl, s"/agent-client-authorisation/cbc/subscriptions/${cbcId.value}").toString

    monitor(s"ConsumedAPI-Get-CBC-subscription-GET") {
      http.GET[HttpResponse](url).map { response =>
        response.status match {
          case OK => Some(response.json.as[SimpleCbcSubscription])
          case NOT_FOUND =>
            logger.warn(s"CBC Subscription not found for cbcId: ${cbcId.value}")
            None
          case status =>
            logger.warn(s"getCbcName: unexpected response $status for cbcId ${cbcId.value}")
            None
        }
      }
    }
  }

  object Reads {

    implicit val detailsForEmailReads = Json.reads[DetailsForEmail]

    implicit val reads: Reads[StoredInvitation] = {

      implicit val urlReads: SimpleObjectReads[URL] = new SimpleObjectReads[URL]("href", s => new URL(baseUrl, s))

      implicit val localDateTimeFormat = MongoLocalDateTimeFormat.localDateTimeFormat

      ((JsPath \ "arn").read[Arn] and
        (JsPath \ "clientType").readNullable[String] and
        (JsPath \ "service").read[Service] and
        (JsPath \ "clientId").read[String] and
        (JsPath \ "clientIdType").read[String] and
        (JsPath \ "suppliedClientId").read[String] and
        (JsPath \ "suppliedClientIdType").read[String] and
        (JsPath \ "detailsForEmail").readNullable[DetailsForEmail] and
        (JsPath \ "status").read[String] and
        (JsPath \ "created").read[LocalDateTime] and
        (JsPath \ "lastUpdated").read[LocalDateTime] and
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
