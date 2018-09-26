package uk.gov.hmrc.agentinvitationsfrontend.controllers

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

import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.stubs.CitizenDetailsStub
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId
import org.jsoup.Jsoup

class AgentsRequestTrackingControllerISpec extends BaseISpec with AuthBehaviours with CitizenDetailsStub {

  lazy val controller: AgentsRequestTrackingController = app.injector.instanceOf[AgentsRequestTrackingController]

  val arn = Arn("TARN0000001")
  val mtdItId = MtdItId("ABCDEF123456789")
  private val validNino = Nino("AB123456A")
  private val validNinoSpace = Nino("AB 12 34 56 A")
  val serviceITSA = "HMRC-MTD-IT"
  val servicePIR = "PERSONAL-INCOME-RECORD"
  val validPostcode = "DH14EJ"
  val invitationIdITSA = InvitationId("ABERULMHCKKW3")
  val invitationIdPIR = InvitationId("B9SCS2T4NZBAX")

  val invitationIdVAT = InvitationId("CZTW1KY6RTAAT")
  val serviceVAT = "HMRC-MTD-VAT"
  val identifierVAT = "VRN"
  val validVrn = Vrn("101747696")
  val invalidVrn = Vrn("101747692")
  val validRegistrationDate = "2007-07-07"
  val validVrn9755 = Vrn("101747641")

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  "GET /track/" should {

    val request = FakeRequest("GET", "/track/")
    val showTrackRequests = controller.showTrackRequests

    "render a page listing non-empty invitations with client's names resolved" in {
      givenAllInvitationsStub(arn)
      givenInactiveITSARelationships(arn)
      givenInactiveVATRelationships(arn)
      givenInactiveRelationshipsIrv(arn)
      givenNinoForMtdItId(MtdItId("JKKL80894713304"), Nino("AB123456A"))
      givenNinoForMtdItId(MtdItId("ABCDE1234567890"), Nino("AB123456A"))
      givenTradingName(Nino("AB123456A"), "FooBar Ltd.")
      givenCitizenDetailsAreKnownFor("AB123456B", "John", "Smith")
      givenCitizenDetailsAreKnownFor("GZ753451B", "Cosmo", "Kramer")
      givenCitizenDetailsAreKnownFor("AB123456A", "Rodney", "Jones")
      givenClientDetails(Vrn("101747696"))
      givenClientDetails(Vrn("101747641"))
      val result = showTrackRequests(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Accepted",
        "Pending",
        "Declined",
        "Expired",
        "Cancelled",
        "You cancelled your authorisation",
        "FooBar Ltd.",
        "John Smith",
        "Cosmo Kramer",
        "GDT",
        "11 September 2018",
        "21 September 2015",
        "24 September 2018",
        "01 January 2099",
        htmlEscapedMessage("recent-invitations.description", 30)
      )
      checkHtmlResultWithBodyMsgs(
        result,
        "recent-invitations.header",
        "recent-invitations.table-row-header.clientName",
        "recent-invitations.table-row-header.service",
        "recent-invitations.table-row-header.status",
        "recent-invitations.invitation.service.HMRC-MTD-IT",
        "recent-invitations.invitation.service.HMRC-MTD-VAT",
        "recent-invitations.invitation.service.PERSONAL-INCOME-RECORD"
      )


      val parseHtml = Jsoup.parse(contentAsString(result))
      parseHtml.getElementsByAttributeValue("class", "row-0").toString should include("FooBar Ltd.")
      parseHtml.getElementsByAttributeValue("class", "row-0").toString should include("Report their income or expenses through software")
      parseHtml.getElementsByAttributeValue("class", "row-3").toString should include("GDT")
      parseHtml.getElementsByAttributeValue("class", "row-3").toString should include("Report their VAT returns through software")
      parseHtml.getElementsByAttributeValue("class", "row-7").toString should include("John Smith")
      parseHtml.getElementsByAttributeValue("class", "row-7").toString should include("View their PAYE income record")
      parseHtml.getElementsByAttributeValue("class", "row-23").toString should include("Rodney Jones")
      parseHtml.getElementsByAttributeValue("class", "row-23").toString should include("View their PAYE income record")

    }

    "render a page listing non-empty invitations without client's names" in {
      givenAllInvitationsStub(arn)
      givenInactiveITSARelationships(arn)
      givenInactiveVATRelationships(arn)
      givenTradingNameNotFound(Nino("AB123456A"))
      givenCitizenDetailsReturns404For("AB123456B")
      givenClientDetailsNotFound(Vrn("101747696"))
      val result = showTrackRequests(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Accepted",
        "Pending",
        "Declined",
        "Expired",
        "Cancelled",
        "You cancelled your authorisation",
        "11 September 2018",
        "01 January 2099",
        htmlEscapedMessage("recent-invitations.description", 30)
      )
      checkHtmlResultWithBodyMsgs(
        result,
        "recent-invitations.header",
        "recent-invitations.table-row-header.clientName",
        "recent-invitations.table-row-header.service",
        "recent-invitations.table-row-header.status",
        "recent-invitations.invitation.service.HMRC-MTD-IT",
        "recent-invitations.invitation.service.HMRC-MTD-VAT",
        "recent-invitations.invitation.service.PERSONAL-INCOME-RECORD"
      )
    }

    "render a page listing empty invitations" in {
      givenAllInvitationsEmptyStub(arn)
      val result = showTrackRequests(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("recent-invitations.description", 30))
      checkHtmlResultWithBodyMsgs(
        result,
        "recent-invitations.header",
        "recent-invitations.empty",
        "recent-invitations.empty.continue")
    }

    behave like anAuthorisedAgentEndpoint(request, showTrackRequests)
  }
}
