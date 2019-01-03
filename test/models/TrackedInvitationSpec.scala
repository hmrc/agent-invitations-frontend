/*
 * Copyright 2019 HM Revenue & Customs
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

package models

import java.net.URL

import org.joda.time.{DateTime, LocalDate}
import uk.gov.hmrc.agentinvitationsfrontend.models.{StoredInvitation, TrackedInvitation}
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.play.test.UnitSpec

class TrackedInvitationSpec extends UnitSpec {

  implicit val now: LocalDate = LocalDate.now

  "TrackedInvitation" when {
    "converted from the stored one" should {
      "have status Expired if already expired" in {

        val invitation = exampleInvitation
          .copy(status = "Pending", expiryDate = now.minusDays(1))
        val tracked = TrackedInvitation.fromStored(invitation)
        tracked.status shouldBe "Pending"
        tracked.effectiveStatus shouldBe "Expired"
        tracked.expiryDate shouldBe now.minusDays(1)
      }

      "have status Expired if expired today" in {

        val invitation = exampleInvitation
          .copy(status = "Pending", created = DateTime.now, expiryDate = now)
        val tracked = TrackedInvitation.fromStored(invitation)
        tracked.status shouldBe "Pending"
        tracked.effectiveStatus shouldBe "Expired"
        tracked.expiryDate shouldBe now
      }

      "have status Pending if not yet expired" in {

        val invitation = exampleInvitation
          .copy(status = "Pending", created = DateTime.now, expiryDate = now.plusDays(1))
        val tracked = TrackedInvitation.fromStored(invitation)
        tracked.status shouldBe "Pending"
        tracked.effectiveStatus shouldBe "Pending"
        tracked.expiryDate shouldBe now.plusDays(1)
      }

      "have status Pending if expires tomorrow" in {

        val invitation = exampleInvitation
          .copy(status = "Pending", created = DateTime.now, expiryDate = now.plusDays(1))
        val tracked = TrackedInvitation.fromStored(invitation)
        tracked.status shouldBe "Pending"
        tracked.effectiveStatus shouldBe "Pending"
        tracked.expiryDate shouldBe now.plusDays(1)
      }

      "have status Accepted" in {
        val dateTime = DateTime.now.minusDays(5)
        val invitation = exampleInvitation
          .copy(status = "Accepted", lastUpdated = dateTime)
        val tracked = TrackedInvitation.fromStored(invitation)
        tracked.status shouldBe "Accepted"
        tracked.effectiveStatus shouldBe "Accepted"
        tracked.lastUpdated shouldBe dateTime
      }

      "have clientId of clientIdType if ni" in {

        val invitation = exampleInvitation
          .copy(clientId = "foo", clientIdType = "ni", suppliedClientId = "bar", suppliedClientIdType = "barType")
        val tracked = TrackedInvitation.fromStored(invitation)
        tracked.clientId shouldBe "foo"
        tracked.clientIdType shouldBe "ni"
      }

      "have clientId of clientIdType if vrn" in {

        val invitation = exampleInvitation
          .copy(clientId = "bla", clientIdType = "vrn", suppliedClientId = "car", suppliedClientIdType = "ni")
        val tracked = TrackedInvitation.fromStored(invitation)
        tracked.clientId shouldBe "bla"
        tracked.clientIdType shouldBe "vrn"
      }

      "have clientId of suppliedIdType if clientIdType not preferable" in {

        val invitation = exampleInvitation
          .copy(clientId = "foo", clientIdType = "MTDITID", suppliedClientId = "bar", suppliedClientIdType = "barType")
        val tracked = TrackedInvitation.fromStored(invitation)
        tracked.clientId shouldBe "bar"
        tracked.clientIdType shouldBe "barType"
      }

      "not have clientName" in {

        val invitation = exampleInvitation
        val tracked = TrackedInvitation.fromStored(invitation)
        tracked.clientName shouldBe None
      }
    }
  }

  val exampleInvitation = StoredInvitation(
    Arn("TARN0000001"),
    None,
    "",
    "",
    "",
    "",
    "",
    "",
    DateTime.now.minusDays(20),
    DateTime.now.minusDays(20),
    now.plusDays(10),
    "",
    new URL(s"http://localhost:9432/agent-client-authorisation/agencies/TARN0000001/invitations/sent/foo1")
  )
}
