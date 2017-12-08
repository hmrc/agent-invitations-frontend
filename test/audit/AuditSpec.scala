/*
 * Copyright 2017 HM Revenue & Customs
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
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentInvitationUserInput
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

    "send an AgentClientInvitationSubmitted Event" in {
      val mockConnector = mock[AuditConnector]
      val service = new AuditService(mockConnector)

      val hc = HeaderCarrier(
        authorization = Some(Authorization("dummy bearer token")),
        sessionId = Some(SessionId("dummy session id")),
        requestId = Some(RequestId("dummy request id")))

      val arn: Arn = Arn("HX2345")
      val agentInvitaitonUserInput: AgentInvitationUserInput = AgentInvitationUserInput(Nino("WM123456C"), Some("serviceName"), "AA1 1AA")
      val invitationId: String = "1"
      val result: String = "Success"

      await(service.sendAgentInvitationSubmitted(
        arn,
        invitationId,
        agentInvitaitonUserInput,
        result)(
        hc,
        FakeRequest("GET", "/path")))

      eventually {
        val captor = ArgumentCaptor.forClass(classOf[DataEvent])
        verify(mockConnector).sendEvent(captor.capture())(any[HeaderCarrier], any[ExecutionContext])
        val sentEvent = captor.getValue.asInstanceOf[DataEvent]

        sentEvent.auditType shouldBe "AgentClientInvitationSubmitted"
        sentEvent.auditSource shouldBe "agent-invitations-frontend"
        sentEvent.detail("result") shouldBe "Success"
        sentEvent.detail("invitationId") shouldBe "1"
        sentEvent.detail("agentReferenceNumber") shouldBe "HX2345"
        sentEvent.detail("regimeId") shouldBe "WM123456C"
        sentEvent.detail("regime") shouldBe "serviceName"

        sentEvent.tags.contains("Authorization") shouldBe false
        sentEvent.detail("Authorization") shouldBe "dummy bearer token"

        sentEvent.tags("transactionName") shouldBe "agent-client-invitation-submitted"
        sentEvent.tags("path") shouldBe "/path"
        sentEvent.tags("X-Session-ID") shouldBe "dummy session id"
        sentEvent.tags("X-Request-ID") shouldBe "dummy request id"
      }
    }

    "send an AgentClientInvitationResponse Event" in {
      val mockConnector = mock[AuditConnector]
      val service = new AuditService(mockConnector)

      val hc = HeaderCarrier(
        authorization = Some(Authorization("dummy bearer token")),
        sessionId = Some(SessionId("dummy session id")),
        requestId = Some(RequestId("dummy request id")))

      val arn: Arn = Arn("HX2345")
      val inviteStatus = "accepted"
      val invitationId: String = "1"
      val mtdItId: MtdItId = MtdItId("mtdItId")

      await(service.sendAgentInvitationResponse(
        invitationId,
        arn,
        inviteStatus,
        mtdItId)(
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
        sentEvent.detail("regimeId") shouldBe "mtdItId"
        sentEvent.detail("regime") shouldBe "HMRC-MTD-IT"

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
