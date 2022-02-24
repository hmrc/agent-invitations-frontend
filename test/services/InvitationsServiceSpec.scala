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

package services

import org.mockito.ArgumentMatchers._
import org.mockito.Mockito.{mock, when}
import play.api.test.Helpers._
import support.UnitSpec
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.connectors._
import uk.gov.hmrc.agentinvitationsfrontend.services.InvitationsService
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Service, Vrn}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class InvitationsServiceSpec extends UnitSpec {

  val acaConnector = mock(classOf[AgentClientAuthorisationConnector])
  val cidConnector = mock(classOf[CitizenDetailsConnector])
  val auditService = mock(classOf[AuditService])
  val relationshipConnector = mock(classOf[RelationshipsConnector])

  val tested = new InvitationsService(acaConnector, cidConnector, relationshipConnector, auditService)

  val vrn = Vrn("101747696")
  val arn = Arn("TARN6169111")

  implicit val hc: HeaderCarrier = HeaderCarrier()

  "InvitationsService" when {

    "setRelationshipEndedForActiveInvitation" should {
      "return Some(true) when it succeeds" in {

        when(
          acaConnector.setRelationshipEnded(any(classOf[Arn]), any(classOf[String]), any(classOf[String]))(
            any(classOf[HeaderCarrier]),
            any(classOf[ExecutionContext])))
          .thenReturn(
            Future successful Some(true)
          )
        await(tested.setRelationshipEnded(arn, vrn.value, Service.Vat)) shouldBe Some(true)
      }
      "return Some(false) when it fails" in {

        when(
          acaConnector.setRelationshipEnded(any(classOf[Arn]), any(classOf[String]), any(classOf[String]))(
            any(classOf[HeaderCarrier]),
            any(classOf[ExecutionContext])))
          .thenReturn(
            Future successful Some(false)
          )
        await(tested.setRelationshipEnded(arn, vrn.value, Service.Vat)) shouldBe Some(false)
      }
    }
  }
}
