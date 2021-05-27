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

package services

import org.joda.time.{DateTime, LocalDate}
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito.{mock, when}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.connectors._
import uk.gov.hmrc.agentinvitationsfrontend.models.StoredInvitation
import uk.gov.hmrc.agentinvitationsfrontend.services.InvitationsService
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, Vrn}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import java.net.URL
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
  val created = nowMinus(15)
  val expiry = LocalDate.now.plusDays(5)

  val storedInvitation: Int => StoredInvitation = (t: Int) =>
    StoredInvitation.apply(
      arn,
      Some("personal"),
      vatService,
      vrn.value,
      None,
      "Accepted",
      created,
      nowMinus(t),
      expiry,
      s"foo$t",
      false,
      None,
      new URL("http://www.someurl.com")
  )

  private def nowMinus(days: Int) =
    DateTime.now.minusDays(days)

  val vatService = "HMRC-MTD-VAT"
  implicit val hc: HeaderCarrier = HeaderCarrier()

  "InvitationsService" when {
    "getActiveInvitationsFor" should {
      "return the most recent invitation" in {
        when(
          acaConnector.getAcceptedInvitationsForClient(any(classOf[Arn]), any(classOf[String]), any(classOf[String]))(
            any(classOf[HeaderCarrier]),
            any(classOf[ExecutionContext])))
          .thenReturn(
            Future successful Seq(
              storedInvitation(2),
              storedInvitation(7),
              storedInvitation(0),
              storedInvitation(3)
            )
          )
        await(tested.getActiveInvitationFor(arn, vrn.value, vatService)) shouldBe Some(InvitationId("foo0"))
      }
    }
    "setRelationshipEndedForActiveInvitation" should {
      "return Some(true) when it succeeds" in {
        when(
          acaConnector.getAcceptedInvitationsForClient(any(classOf[Arn]), any(classOf[String]), any(classOf[String]))(
            any(classOf[HeaderCarrier]),
            any(classOf[ExecutionContext])))
          .thenReturn(
            Future successful Seq(
              storedInvitation(2),
              storedInvitation(7),
              storedInvitation(0),
              storedInvitation(3)
            )
          )
        when(acaConnector.setRelationshipEnded(any(classOf[InvitationId]))(any(classOf[HeaderCarrier]), any(classOf[ExecutionContext])))
          .thenReturn(
            Future successful Some(true)
          )

        await(tested.setRelationshipEnded(arn, vrn.value, vatService)) shouldBe Some(true)
      }
    }
  }

}
