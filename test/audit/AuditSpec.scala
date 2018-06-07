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

package audit

import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalatest.concurrent.Eventually
import org.scalatest.mockito.MockitoSugar
import play.api.test.FakeRequest
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.models.UserInputNinoAndPostcode
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, MtdItId}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.{Authorization, RequestId, SessionId}
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.DataEvent
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext

class AuditSpec extends UnitSpec with MockitoSugar with Eventually {

  "auditEvent" should {

    "send an AgentClientAuthorisationRequestCreated Event" in {
      val mockConnector = mock[AuditConnector]
      val service = new AuditService(mockConnector)

      val hc = HeaderCarrier(
        authorization = Some(Authorization("dummy bearer token")),
        sessionId = Some(SessionId("dummy session id")),
        requestId = Some(RequestId("dummy request id")))

      val arn: Arn = Arn("HX2345")
      val agentInvitaitonUserInput: UserInputNinoAndPostcode = UserInputNinoAndPostcode("serviceName", Some(Nino("WM123456C")), Some("AA1 1AA"))
      val invitationId: String = "1"
      val result: String = "Success"

      await(service.sendAgentInvitationSubmitted(
        arn,
        invitationId,
        agentInvitaitonUserInput.service,
        agentInvitaitonUserInput.clientIdentifierType,
        agentInvitaitonUserInput.clientIdentifier,
        result)(
        hc,
        FakeRequest("GET", "/path")))

      eventually {
        val captor = ArgumentCaptor.forClass(classOf[DataEvent])
        verify(mockConnector).sendEvent(captor.capture())(any[HeaderCarrier], any[ExecutionContext])
        val sentEvent = captor.getValue.asInstanceOf[DataEvent]

        sentEvent.auditType shouldBe "AgentClientAuthorisationRequestCreated"
        sentEvent.auditSource shouldBe "agent-invitations-frontend"
        sentEvent.detail("factCheck") shouldBe "Success"
        sentEvent.detail("invitationId") shouldBe "1"
        sentEvent.detail("agentReferenceNumber") shouldBe "HX2345"
        sentEvent.detail("clientIdType") shouldBe "ni"
        sentEvent.detail("clientId") shouldBe "WM123456C"
        sentEvent.detail("service") shouldBe "serviceName"

        sentEvent.tags.contains("Authorization") shouldBe false
        sentEvent.detail("Authorization") shouldBe "dummy bearer token"

        sentEvent.tags("transactionName") shouldBe "Agent client service authorisation request created"
        sentEvent.tags("path") shouldBe "/path"
        sentEvent.tags("X-Session-ID") shouldBe "dummy session id"
        sentEvent.tags("X-Request-ID") shouldBe "dummy request id"
      }
    }

    "send an AgentClientInvitationResponse Event for ITSA" in {
      val mockConnector = mock[AuditConnector]
      val service = new AuditService(mockConnector)

      val hc = HeaderCarrier(
        authorization = Some(Authorization("dummy bearer token")),
        sessionId = Some(SessionId("dummy session id")),
        requestId = Some(RequestId("dummy request id")))

      val arn: Arn = Arn("HX2345")
      val agencyName: String = "someAgency"
      val clientResponse = "accepted"
      val invitationId: String = "1"
      val clientIdType: String = "ni"
      val mtdItId: MtdItId = MtdItId("mtdItId")
      val serviceName: String = "HMRC-MTD-IT"

      await(service.sendAgentInvitationResponse(
        invitationId,
        arn,
        clientResponse,
        clientIdType,
        mtdItId.value,
        serviceName,
        agencyName)(
        hc,
        FakeRequest("GET", "/path")))

      eventually {
        val captor = ArgumentCaptor.forClass(classOf[DataEvent])
        verify(mockConnector).sendEvent(captor.capture())(any[HeaderCarrier], any[ExecutionContext])
        val sentEvent = captor.getValue.asInstanceOf[DataEvent]

        sentEvent.auditType shouldBe "AgentClientInvitationResponse"
        sentEvent.auditSource shouldBe "agent-invitations-frontend"
        sentEvent.detail("clientResponse") shouldBe "accepted"
        sentEvent.detail("invitationId") shouldBe "1"
        sentEvent.detail("agentReferenceNumber") shouldBe "HX2345"
        sentEvent.detail("agencyName") shouldBe "someAgency"
        sentEvent.detail("clientIdType") shouldBe "ni"
        sentEvent.detail("clientId") shouldBe "mtdItId"
        sentEvent.detail("service") shouldBe "HMRC-MTD-IT"

        sentEvent.tags.contains("Authorization") shouldBe false
        sentEvent.detail("Authorization") shouldBe "dummy bearer token"

        sentEvent.tags("transactionName") shouldBe "agent-client-invitation-response"
        sentEvent.tags("path") shouldBe "/path"
        sentEvent.tags("X-Session-ID") shouldBe "dummy session id"
        sentEvent.tags("X-Request-ID") shouldBe "dummy request id"
      }
    }

    "send an AgentClientInvitationResponse Event for PIR" in {
      val mockConnector = mock[AuditConnector]
      val service = new AuditService(mockConnector)

      val hc = HeaderCarrier(
        authorization = Some(Authorization("dummy bearer token")),
        sessionId = Some(SessionId("dummy session id")),
        requestId = Some(RequestId("dummy request id")))

      val arn: Arn = Arn("HX2345")
      val agencyName: String = "someAgency"
      val clientResponse = "accepted"
      val invitationId: String = "2"
      val clientIdType: String = "ni"
      val nino: String = "nino"
      val serviceName: String = "PERSONAL-INCOME-RECORD"

      await(service.sendAgentInvitationResponse(
        invitationId,
        arn,
        clientResponse,
        clientIdType,
        nino,
        serviceName,
        agencyName)(
        hc,
        FakeRequest("GET", "/path")))

      eventually {
        val captor = ArgumentCaptor.forClass(classOf[DataEvent])
        verify(mockConnector).sendEvent(captor.capture())(any[HeaderCarrier], any[ExecutionContext])
        val sentEvent = captor.getValue.asInstanceOf[DataEvent]

        sentEvent.auditType shouldBe "AgentClientInvitationResponse"
        sentEvent.auditSource shouldBe "agent-invitations-frontend"
        sentEvent.detail("clientResponse") shouldBe "accepted"
        sentEvent.detail("invitationId") shouldBe "2"
        sentEvent.detail("agentReferenceNumber") shouldBe "HX2345"
        sentEvent.detail("agencyName") shouldBe "someAgency"
        sentEvent.detail("clientIdType") shouldBe "ni"
        sentEvent.detail("clientId") shouldBe "nino"
        sentEvent.detail("service") shouldBe "PERSONAL-INCOME-RECORD"

        sentEvent.tags.contains("Authorization") shouldBe false
        sentEvent.detail("Authorization") shouldBe "dummy bearer token"

        sentEvent.tags("transactionName") shouldBe "agent-client-invitation-response"
        sentEvent.tags("path") shouldBe "/path"
        sentEvent.tags("X-Session-ID") shouldBe "dummy session id"
        sentEvent.tags("X-Request-ID") shouldBe "dummy request id"
      }
    }

    "send an AgentClientInvitationResponse Event for VAT" in {
      val mockConnector = mock[AuditConnector]
      val service = new AuditService(mockConnector)

      val hc = HeaderCarrier(
        authorization = Some(Authorization("dummy bearer token")),
        sessionId = Some(SessionId("dummy session id")),
        requestId = Some(RequestId("dummy request id")))

      val arn: Arn = Arn("HX2345")
      val agencyName: String = "someAgency"
      val clientResponse = "accepted"
      val invitationId: String = "3"
      val clientIdType: String = "vrn"
      val vatRegistrationNumber: String = "vat"
      val serviceName: String = "HMRC-MTD-VAT"

      await(service.sendAgentInvitationResponse(
        invitationId,
        arn,
        clientResponse,
        clientIdType,
        vatRegistrationNumber,
        serviceName,
        agencyName)(
        hc,
        FakeRequest("GET", "/path")))

      eventually {
        val captor = ArgumentCaptor.forClass(classOf[DataEvent])
        verify(mockConnector).sendEvent(captor.capture())(any[HeaderCarrier], any[ExecutionContext])
        val sentEvent = captor.getValue.asInstanceOf[DataEvent]

        sentEvent.auditType shouldBe "AgentClientInvitationResponse"
        sentEvent.auditSource shouldBe "agent-invitations-frontend"
        sentEvent.detail("clientResponse") shouldBe "accepted"
        sentEvent.detail("invitationId") shouldBe "3"
        sentEvent.detail("agentReferenceNumber") shouldBe "HX2345"
        sentEvent.detail("agencyName") shouldBe "someAgency"
        sentEvent.detail("clientIdType") shouldBe "vrn"
        sentEvent.detail("clientId") shouldBe "vat"
        sentEvent.detail("service") shouldBe "HMRC-MTD-VAT"

        sentEvent.tags.contains("Authorization") shouldBe false
        sentEvent.detail("Authorization") shouldBe "dummy bearer token"

        sentEvent.tags("transactionName") shouldBe "agent-client-invitation-response"
        sentEvent.tags("path") shouldBe "/path"
        sentEvent.tags("X-Session-ID") shouldBe "dummy session id"
        sentEvent.tags("X-Request-ID") shouldBe "dummy request id"
      }
    }

  }

}
