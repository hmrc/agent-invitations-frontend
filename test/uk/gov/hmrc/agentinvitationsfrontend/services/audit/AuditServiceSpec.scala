/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.agentinvitationsfrontend.services.audit

import org.mockito.Mockito.verify
import org.mockito.captor.ArgCaptor
import org.mockito.scalatest.IdiomaticMockito
import org.scalatestplus.play.PlaySpec
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.{Authorization, HeaderCarrier, RequestId, SessionId}
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.DataEvent

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class AuditServiceSpec extends PlaySpec with IdiomaticMockito {

  trait Setup {
    val mockConnector = mock[AuditConnector]
    val service = new AuditService(mockConnector)

    val hc = HeaderCarrier(
      authorization = Some(Authorization("test bearer token")),
      sessionId = Some(SessionId("test session id")),
      requestId = Some(RequestId("test request id")))

    val arn: String = "HX2345"
    val agencyName: String = "someAgency"
    val vrnType: String = "vrn"
    val ninoType: String = "ni"
    val nino: String = "WM123456C"
    val mtdItId: String = "mtdItId"
    val vatRegistrationNumber: String = "123456782"
    val result: String = "Success"
    val personalIncomeRecordServiceId = "PERSONAL-INCOME-RECORD"
    val mtdItServiceId = "HMRC-MTD-IT"
    val mtdVatServiceId = "HMRC-MTD-VAT"
    val clientType = "personal"
  }

  "auditEvent" should {
    "send an AgentClientAuthorisationRequestCreated Event" in new Setup {

      mockConnector.sendEvent(any[DataEvent])(any[HeaderCarrier], any[ExecutionContext]) returns Future.successful(AuditResult.Success)

      await(
        service.sendAgentInvitationSubmitted(
          arn = arn,
          invitationId = "1",
          clientIdType = ninoType,
          clientType = clientType,
          clientId = nino,
          serviceId = personalIncomeRecordServiceId,
          uid = "uid",
          result = result
        )(hc, FakeRequest("GET", "/path"), global))

        val captor = ArgCaptor[DataEvent]
        verify(mockConnector).sendEvent(captor.capture)(any[HeaderCarrier], any[ExecutionContext])
        val sentEvent = captor.value

        sentEvent.auditType mustBe "AgentClientAuthorisationRequestCreated"
        sentEvent.auditSource mustBe "agent-invitations-frontend"
        sentEvent.detail("factCheck") mustBe "Success"
        sentEvent.detail("invitationId") mustBe "1"
        sentEvent.detail("agentReferenceNumber") mustBe "HX2345"
        sentEvent.detail("clientType") mustBe "personal"
        sentEvent.detail("clientIdType") mustBe "ni"
        sentEvent.detail("clientId") mustBe "WM123456C"
        sentEvent.detail("service") mustBe "PERSONAL-INCOME-RECORD"

        sentEvent.tags("transactionName") mustBe "Agent client service authorisation request created"
        sentEvent.tags("path") mustBe "/path"
        sentEvent.tags("X-Session-ID") mustBe "test session id"
        sentEvent.tags("X-Request-ID") mustBe "test request id"
    }

    "send an AgentClientInvitationResponse Event for ITSA" in new Setup {
      mockConnector.sendEvent(any[DataEvent])(any[HeaderCarrier], any[ExecutionContext]) returns Future.successful(AuditResult.Success)

      await(
        service.sendAgentInvitationResponse(
          arn = arn,
          invitationId = "1",
          isAccepted = true,
          clientIdType = ninoType,
          clientId = mtdItId,
          service = mtdItServiceId,
          agencyName = agencyName
        )(
          hc,
          FakeRequest("GET", "/path"),
          concurrent.ExecutionContext.Implicits.global))

        val captor = ArgCaptor[DataEvent]
        verify(mockConnector).sendEvent(captor.capture)(any[HeaderCarrier], any[ExecutionContext])
        val sentEvent = captor.value

        sentEvent.auditType mustBe "AgentClientInvitationResponse"
        sentEvent.auditSource mustBe "agent-invitations-frontend"
        sentEvent.detail("clientResponse") mustBe "Accepted"
        sentEvent.detail("invitationId") mustBe "1"
        sentEvent.detail("agentReferenceNumber") mustBe "HX2345"
        sentEvent.detail("agencyName") mustBe "someAgency"
        sentEvent.detail("clientIdType") mustBe "ni"
        sentEvent.detail("clientId") mustBe "mtdItId"
        sentEvent.detail("service") mustBe "HMRC-MTD-IT"

        sentEvent.tags("transactionName") mustBe "agent-client-invitation-response"
        sentEvent.tags("path") mustBe "/path"
        sentEvent.tags("X-Session-ID") mustBe "test session id"
        sentEvent.tags("X-Request-ID") mustBe "test request id"
    }

    "send an AgentClientInvitationResponse Event for PIR" in new Setup {
      mockConnector.sendEvent(any[DataEvent])(any[HeaderCarrier], any[ExecutionContext]) returns Future.successful(AuditResult.Success)

      await(
        service
          .sendAgentInvitationResponse(
            arn = arn,
            invitationId = "2",
            isAccepted = true,
            clientIdType = ninoType,
            clientId = nino,
            service = personalIncomeRecordServiceId,
            agencyName = agencyName
          )(
            hc,
            FakeRequest("GET", "/path"),
            concurrent.ExecutionContext.Implicits.global))

        val captor = ArgCaptor[DataEvent]
        verify(mockConnector).sendEvent(captor.capture)(any[HeaderCarrier], any[ExecutionContext])
        val sentEvent = captor.value

        sentEvent.auditType mustBe "AgentClientInvitationResponse"
        sentEvent.auditSource mustBe "agent-invitations-frontend"
        sentEvent.detail("clientResponse") mustBe "Accepted"
        sentEvent.detail("invitationId") mustBe "2"
        sentEvent.detail("agentReferenceNumber") mustBe "HX2345"
        sentEvent.detail("agencyName") mustBe "someAgency"
        sentEvent.detail("clientIdType") mustBe "ni"
        sentEvent.detail("clientId") mustBe "WM123456C"
        sentEvent.detail("service") mustBe "PERSONAL-INCOME-RECORD"

        sentEvent.tags("transactionName") mustBe "agent-client-invitation-response"
        sentEvent.tags("path") mustBe "/path"
        sentEvent.tags("X-Session-ID") mustBe "test session id"
        sentEvent.tags("X-Request-ID") mustBe "test request id"
    }

    "send an AgentClientInvitationResponse Event for VAT" in new Setup {
      mockConnector.sendEvent(any[DataEvent])(any[HeaderCarrier], any[ExecutionContext]) returns Future.successful(AuditResult.Success)

      await(
        service.sendAgentInvitationResponse(
          arn = arn,
          invitationId = "3",
          isAccepted = true,
          clientIdType = vrnType,
          clientId = vatRegistrationNumber,
          service = mtdVatServiceId,
          agencyName = agencyName
        )(hc,
          FakeRequest("GET", "/path"),
          concurrent.ExecutionContext.Implicits.global))

        val captor = ArgCaptor[DataEvent]
        verify(mockConnector).sendEvent(captor.capture)(any[HeaderCarrier], any[ExecutionContext])
        val sentEvent = captor.value

        sentEvent.auditType mustBe "AgentClientInvitationResponse"
        sentEvent.auditSource mustBe "agent-invitations-frontend"
        sentEvent.detail("clientResponse") mustBe "Accepted"
        sentEvent.detail("invitationId") mustBe "3"
        sentEvent.detail("agentReferenceNumber") mustBe "HX2345"
        sentEvent.detail("agencyName") mustBe "someAgency"
        sentEvent.detail("clientIdType") mustBe "vrn"
        sentEvent.detail("clientId") mustBe "123456782"
        sentEvent.detail("service") mustBe "HMRC-MTD-VAT"

        sentEvent.tags("transactionName") mustBe "agent-client-invitation-response"
        sentEvent.tags("path") mustBe "/path"
        sentEvent.tags("X-Session-ID") mustBe "test session id"
        sentEvent.tags("X-Request-ID") mustBe "test request id"
      }

  }

}
