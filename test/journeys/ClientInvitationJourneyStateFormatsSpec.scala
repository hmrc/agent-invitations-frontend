/*
 * Copyright 2020 HM Revenue & Customs
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

package journeys

import org.joda.time.LocalDate
import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.ClientInvitationJourneyStateFormats
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientConsent
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.personal
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId}
import uk.gov.hmrc.play.test.UnitSpec

class ClientInvitationJourneyStateFormatsSpec extends UnitSpec {

  implicit val formats: Format[State] = ClientInvitationJourneyStateFormats.formats

  "ClientInvitationJourneyStateFormats" should {
    "serialize and deserialize state" when {

      val jsonConsents = """"consents":[
                           |{
                           |"invitationId": {
                           |  "value": "A1BEOZEO7MNO6"
                           |  },
                           |"expiryDate": "2010-01-01",
                           |"serviceKey": "itsa",
                           |"consent": true,
                           |"processed": false
                           |}, {
                           |"invitationId": {
                           |  "value": "B1BEOZEO7MNO6"
                           |  },
                           |"expiryDate": "2010-02-02",
                           |"serviceKey": "afi",
                           |"consent": true,
                           |"processed": false
                           |}
                           |]"""

      "WarmUp" in {
        val state = WarmUp(personal, "uid", Arn("TARN0000001"), "agent name", "agent-name")
        val json = Json.parse(
          """{"state":"WarmUp","properties":{"clientType": "personal", "uid": "uid", "arn": "TARN0000001", "agentName": "agent name", "normalisedAgentName":"agent-name"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "NotFoundInvitation" in {
        val state = NotFoundInvitation
        val json = Json.parse("""{"state":"NotFoundInvitation"}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "ActionRequired" in {
        val state = ActionRequired(personal)
        val json = Json.parse("""{"state":"ActionRequired", "properties": {"clientType": "personal"}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "InvitationAlreadyResponded" in {
        val state = InvitationAlreadyResponded
        val json = Json.parse("""{"state":"InvitationAlreadyResponded"}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "AllRequestsCancelled" in {
        val state = AllRequestsCancelled
        val json = Json.parse("""{"state":"AllRequestsCancelled"}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "AllRequestsExpired" in {
        val state = AllRequestsExpired
        val json = Json.parse("""{"state":"AllRequestsExpired"}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "CannotViewRequest" in {
        val state = CannotViewRequest
        val json = Json.parse("""{"state":"CannotViewRequest"}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "SuspendedAgent" in {
        val state = SuspendedAgent(personal, "uid", "agent name", Set("ITSA", "VATC"), Seq())
        val json = Json.parse(
          """{"state":"SuspendedAgent","properties":{"clientType": "personal", "uid": "uid",  "agentName": "agent name", "suspendedRegimes": ["ITSA", "VATC"], "nonSuspendedConsents": []}}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "MultiConsent" in {
        val state = MultiConsent(
          personal,
          "uid",
          "agent name",
          Seq(
            ClientConsent(
              InvitationId("A1BEOZEO7MNO6"),
              LocalDate.parse("2010-01-01"),
              "itsa",
              consent = true
            ),
            ClientConsent(
              InvitationId("B1BEOZEO7MNO6"),
              LocalDate.parse("2010-02-02"),
              "afi",
              consent = true
            )
          )
        )
        val json = Json.parse(s"""{"state":"MultiConsent",
                                 |"properties":{"clientType": "personal", 
                                 |"uid": "uid", 
                                 |"agentName": "agent name", $jsonConsents}}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "SingleConsent" in {
        val state = SingleConsent(
          personal,
          "uid",
          "agent name",
          ClientConsent(
            InvitationId("B1BEOZEO7MNO6"),
            LocalDate.parse("2010-02-02"),
            "afi",
            consent = true
          ),
          Seq(
            ClientConsent(
              InvitationId("A1BEOZEO7MNO6"),
              LocalDate.parse("2010-01-01"),
              "itsa",
              consent = true
            ),
            ClientConsent(
              InvitationId("B1BEOZEO7MNO6"),
              LocalDate.parse("2010-02-02"),
              "afi",
              consent = true
            )
          )
        )
        val json = Json.parse(
          s"""{"state":"SingleConsent","properties":{"clientType": "personal", "uid": "uid", "agentName": "agent name", "consent": {"invitationId": {"value": "B1BEOZEO7MNO6"}, "expiryDate": "2010-02-02", "serviceKey": "afi", "consent": true, "processed": false}, $jsonConsents}}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "CheckAnswers" in {
        val state = CheckAnswers(
          personal,
          "uid",
          "agent name",
          Seq(
            ClientConsent(
              InvitationId("A1BEOZEO7MNO6"),
              LocalDate.parse("2010-01-01"),
              "itsa",
              consent = true
            ),
            ClientConsent(
              InvitationId("B1BEOZEO7MNO6"),
              LocalDate.parse("2010-02-02"),
              "afi",
              consent = true
            )
          )
        )
        val json = Json.parse(
          s"""{"state":"CheckAnswers","properties":{"clientType": "personal", "uid": "uid", "agentName": "agent name", $jsonConsents}}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "InvitationsAccepted" in {
        val state = InvitationsAccepted(
          "agent name",
          Seq(
            ClientConsent(
              InvitationId("A1BEOZEO7MNO6"),
              LocalDate.parse("2010-01-01"),
              "itsa",
              consent = true
            ),
            ClientConsent(
              InvitationId("B1BEOZEO7MNO6"),
              LocalDate.parse("2010-02-02"),
              "afi",
              consent = true
            )
          ),
          personal
        )
        val json = Json.parse(
          s"""{"state":"InvitationsAccepted","properties":{"agentName": "agent name", $jsonConsents, "clientType": "personal"}}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "InvitationsDeclined" in {
        val state = InvitationsDeclined(
          "agent name",
          Seq(
            ClientConsent(
              InvitationId("A1BEOZEO7MNO6"),
              LocalDate.parse("2010-01-01"),
              "itsa",
              consent = true
            ),
            ClientConsent(
              InvitationId("B1BEOZEO7MNO6"),
              LocalDate.parse("2010-02-02"),
              "afi",
              consent = true
            )
          ),
          personal
        )
        val json = Json.parse(s"""{"state":"InvitationsDeclined",
                                 |"properties":{
                                 |"agentName": "agent name",
                                 |$jsonConsents,
                                 |"clientType": "personal"}}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "AllResponsesFailed" in {
        val state = AllResponsesFailed
        val json = Json.parse("""{"state":"AllResponsesFailed"}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "SomeResponsesFailed" in {
        val state = SomeResponsesFailed(
          "agent name",
          Seq(
            ClientConsent(
              InvitationId("A1BEOZEO7MNO6"),
              LocalDate.parse("2010-01-01"),
              "itsa",
              consent = true
            )
          ),
          Seq(
            ClientConsent(
              InvitationId("B1BEOZEO7MNO6"),
              LocalDate.parse("2010-02-02"),
              "afi",
              consent = true,
              processed = true
            )),
          personal
        )
        val json =
          Json.parse(s"""{"state":"SomeResponsesFailed","properties":{"agentName": "agent name", 
               "failedConsents": [{
                        |  "invitationId": {
                        |    "value": "A1BEOZEO7MNO6"
                        |  },
                        |  "expiryDate": "2010-01-01",
                        |  "serviceKey": "itsa",
                        |  "consent": true,
                        |  "processed": false
                        |}],
                        |"successfulConsents": [{
                        |  "invitationId": {
                        |    "value": "B1BEOZEO7MNO6"
                        |  },
                        |  "expiryDate": "2010-02-02",
                        |  "serviceKey": "afi",
                        |  "consent": true,
                        |  "processed": true
                        |}],
                        |"clientType": "personal"}}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }
      "ConfirmDecline" in {
        val state = ConfirmDecline(
          personal,
          "uid",
          "agent name",
          Seq(
            ClientConsent(
              InvitationId("A1BEOZEO7MNO6"),
              LocalDate.parse("2010-01-01"),
              "itsa",
              consent = true
            ),
            ClientConsent(
              InvitationId("B1BEOZEO7MNO6"),
              LocalDate.parse("2010-02-02"),
              "afi",
              consent = true
            )
          )
        )
        val json = Json.parse(
          s"""{"state":"ConfirmDecline","properties":{"clientType": "personal", "uid": "uid", "agentName": "agent name", $jsonConsents}}""".stripMargin)

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

      "TrustNotClaimed" in {
        val state = TrustNotClaimed
        val json = Json.parse("""{"state":"TrustNotClaimed"}""")

        Json.toJson(state) shouldBe json
        json.as[State] shouldBe state
      }

    }
  }
}
