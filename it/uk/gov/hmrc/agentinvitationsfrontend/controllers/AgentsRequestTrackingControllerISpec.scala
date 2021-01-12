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

import java.util.UUID

import org.joda.time.{DateTimeZone, LocalDate}
import org.jsoup.Jsoup
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.forms.FilterTrackRequestsForm
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.personal
import uk.gov.hmrc.agentinvitationsfrontend.models.FilterFormStatus.ClientNotYetResponded
import uk.gov.hmrc.agentinvitationsfrontend.models.FilterTrackRequests
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

class AgentsRequestTrackingControllerISpec extends BaseISpec with AuthBehaviours {

  lazy val controller: AgentsRequestTrackingController = app.injector.instanceOf[AgentsRequestTrackingController]
  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId(UUID.randomUUID().toString)))

  "GET /track/" should {

    val request = FakeRequest("GET", "/track/")
    val showTrackRequestsPageOne = controller.showTrackRequests(1, None, None)
    val showTrackRequestsPageTwo = controller.showTrackRequests(2, None, None)
    val showTrackRequestsPageThree = controller.showTrackRequests(3, None, None)

    "render a page with filter form listing non-empty invitations with client's names resolved" in {
      givenGetInvitations(arn) // 18 invitations
      givenInactiveRelationships(arn)
      givenInactiveAfiRelationship(arn)  // 2 relationships
      givenNinoForMtdItId(MtdItId("JKKL80894713304"), Nino("AB123456A"))
      givenNinoForMtdItId(MtdItId("ABCDE1234567890"), Nino("AB123456A"))
      givenTradingName(Nino("AB123456A"), "FooBar Ltd.")
      givenCitizenDetailsAreKnownFor("AB123456B", "John", "Smith")
      givenCitizenDetailsAreKnownFor("GZ753451B", "Cosmo", "Kramer")
      givenCitizenDetailsAreKnownFor("AB123456A", "Rodney", "Jones")
      givenClientDetails(Vrn("101747696"))
      givenClientDetails(Vrn("101747641"))

      givenTrustClientReturns(validUtr, 200, Json.toJson(trustResponse).toString())
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription()).toString())

      val resultPageOne = showTrackRequestsPageOne(authorisedAsValidAgent(request, arn.value))
      status(resultPageOne) shouldBe 200
      checkHtmlResultWithBodyText(
        resultPageOne,
        "Accepted by client",
        "Client has not yet responded",
        "Declined by client",
        "FooBar Ltd.",
        "John Smith",
        "GDT",
        "11 September 2018",
        "01 January 2099",
        "Resend request to client",
        "Cancel this request",
        "Start new request",
        "Cancel your authorisation",
        htmlEscapedMessage("recent-invitations.description", 30)
      )

      checkHtmlResultWithBodyMsgs(
        resultPageOne,
        "recent-invitations.header",
        "recent-invitations.filter-client.label",
        "recent-invitations.filter-status.label",
        "recent-invitations.filter.filter.button",
        "recent-invitations.filter.clear.button",
        "recent-invitations.table-row-header.clientName",
        "recent-invitations.table-row-header.service",
        "recent-invitations.table-row-header.status",
        "recent-invitations.table-row-header.actions",
        "recent-invitations.invitation.service.HMRC-MTD-IT",
        "recent-invitations.invitation.service.HMRC-MTD-VAT",
        "recent-invitations.invitation.service.PERSONAL-INCOME-RECORD"
      )

      val parseHtml = Jsoup.parse(contentAsString(resultPageOne))

      parseHtml.getElementsByAttributeValue("id", "row-0").toString should include("FooBar Ltd.")
      parseHtml.getElementsByAttributeValue("id", "row-0").toString should include("Manage their Income Tax")

      val resultPageTwo = showTrackRequestsPageTwo(authorisedAsValidAgent(request, arn.value))
      status(resultPageTwo) shouldBe 200

      checkHtmlResultWithBodyText(
        resultPageTwo,
        "Request expired as client did not respond in time",
        "You cancelled this request"
        )

      val parseHtmlPageTwo = Jsoup.parse(contentAsString(resultPageTwo))

      parseHtmlPageTwo.getElementsByAttributeValue("id", "row-1").toString should include("cancelled this request")
      parseHtmlPageTwo.getElementsByAttributeValue("id", "row-1").toString should include("fastTrackInvitationCreate")

      val resultPageThree = showTrackRequestsPageThree(authorisedAsValidAgent(request, arn.value))
      status(resultPageThree) shouldBe 200

      checkHtmlResultWithBodyText(
        resultPageThree,
        "21 September 2015",
        cgtSubscription().name
        )

      checkHtmlResultWithBodyMsgs(
        resultPageThree,
        "recent-invitations.invitation.service.HMRC-CGT-PD",
        "recent-invitations.invitation.service.HMRC-TERS-ORG")
    }

    "render a page listing non-empty invitations without client's names" in {
      givenGetInvitations(arn)
      givenInactiveRelationships(arn)
      givenInactiveAfiRelationship(arn)
      givenNinoForMtdItId(MtdItId("JKKL80894713304"), Nino("AB123456A"))
      givenNinoForMtdItId(MtdItId("ABCDE1234567890"), Nino("AB123456A"))
      givenTradingNameNotFound(Nino("AB123456A"))
      givenCitizenDetailsReturns404For("AB123456B")
      givenClientDetailsNotFound(Vrn("101747696"))
      givenClientDetails(Vrn("101747641"))
      givenCitizenDetailsAreKnownFor("AB123456B", "John", "Smith")
      givenCitizenDetailsAreKnownFor("GZ753451B", "Cosmo", "Kramer")
      givenCitizenDetailsAreKnownFor("AB123456A", "Rodney", "Jones")
      givenTrustClientReturns(validUtr, 200, Json.toJson(trustResponse).toString())

      val result = showTrackRequestsPageOne(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(
        result,
        "Accepted by client",
        "Client has not yet responded",
        "Declined by client",
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

      val result2 = showTrackRequestsPageTwo(authorisedAsValidAgent(request, arn.value))
      status(result2) shouldBe 200

      checkHtmlResultWithBodyText(
        result2,
        "Request expired as client did not respond in time",
        "You cancelled this request")

    }

    "render a page listing empty invitations" in {
      givenGetInvitationsReturnsEmpty(arn)
      givenInactiveRelationshipsNotFound
      givenInactiveAfiRelationshipNotFound
      val result = showTrackRequestsPageOne(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("recent-invitations.description", 30))
      checkHtmlResultWithBodyMsgs(
        result,
        "recent-invitations.header",
        "recent-invitations.empty",
        "recent-invitations.empty.continue")
    }

    "accept valid request with filter by client query param" in {

      val request = FakeRequest("GET", "/track?page=1&client=FooBar+Ltd.")
      val showTrackRequestsPageOne = controller.showTrackRequests(1, Some("FooBar Ltd."), None)

        givenGetInvitations(arn) // 18 invitations
        givenInactiveRelationships(arn)
        givenInactiveAfiRelationship(arn) // 2 relationships
        givenNinoForMtdItId(MtdItId("JKKL80894713304"), Nino("AB123456A"))
        givenNinoForMtdItId(MtdItId("ABCDE1234567890"), Nino("AB123456A"))
        givenTradingName(Nino("AB123456A"), "FooBar Ltd.")
        givenCitizenDetailsAreKnownFor("AB123456B", "John", "Smith")
        givenCitizenDetailsAreKnownFor("GZ753451B", "Cosmo", "Kramer")
        givenCitizenDetailsAreKnownFor("AB123456A", "Rodney", "Jones")
        givenClientDetails(Vrn("101747696"))
        givenClientDetails(Vrn("101747641"))

        givenTrustClientReturns(validUtr, 200, Json.toJson(trustResponse).toString())
        givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription()).toString())

        val resultPageOne = showTrackRequestsPageOne(authorisedAsValidAgent(request, arn.value))
        status(resultPageOne) shouldBe 200

      val parseHtml = Jsoup.parse(contentAsString(resultPageOne))

      parseHtml.getElementsByAttributeValue("id", "row-0").toString should include("FooBar Ltd.")
      parseHtml.getElementsByAttributeValue("id", "row-1").toString should include("FooBar Ltd.")
      parseHtml.getElementsByAttributeValue("id", "row-2").toString should include("FooBar Ltd.")
      parseHtml.getElementsByAttributeValue("id", "row-3").toString should include("FooBar Ltd.")
      parseHtml.getElementsByAttributeValue("id", "row-4").toString should include("FooBar Ltd.")
      parseHtml.getElementsByAttributeValue("id", "row-5").toString should include("FooBar Ltd.")
      parseHtml.getElementsByAttributeValue("id", "row-6").toString should include("FooBar Ltd.")
    }

    "accept valid request with filter by client and status query params" in {

      val request = FakeRequest("GET", "/track?page=1&client=FooBar+Ltd.&status=ClientNotYetResponded")
      val showTrackRequestsPageOne = controller.showTrackRequests(1, Some("FooBar Ltd."), Some(ClientNotYetResponded))

      givenGetInvitations(arn) // 18 invitations
      givenInactiveRelationships(arn)
      givenInactiveAfiRelationship(arn) // 2 relationships
      givenNinoForMtdItId(MtdItId("JKKL80894713304"), Nino("AB123456A"))
      givenNinoForMtdItId(MtdItId("ABCDE1234567890"), Nino("AB123456A"))
      givenTradingName(Nino("AB123456A"), "FooBar Ltd.")
      givenCitizenDetailsAreKnownFor("AB123456B", "John", "Smith")
      givenCitizenDetailsAreKnownFor("GZ753451B", "Cosmo", "Kramer")
      givenCitizenDetailsAreKnownFor("AB123456A", "Rodney", "Jones")
      givenClientDetails(Vrn("101747696"))
      givenClientDetails(Vrn("101747641"))

      givenTrustClientReturns(validUtr, 200, Json.toJson(trustResponse).toString())
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription()).toString())

      val resultPageOne = showTrackRequestsPageOne(authorisedAsValidAgent(request, arn.value))
      status(resultPageOne) shouldBe 200

      val parseHtml = Jsoup.parse(contentAsString(resultPageOne))

      parseHtml.getElementsByAttributeValue("id", "row-0").toString should include("FooBar Ltd.")
      parseHtml.getElementsByAttributeValue("id", "row-1").toString should include("FooBar Ltd.")

    }

    behave like anAuthorisedAgentEndpoint(request, showTrackRequestsPageOne)
  }

  "POST /track" should {

    "accept form with valid inputs" in {

      val request = FakeRequest("POST", "/track")
      val postTrack = controller.submitFilterTrackRequests

      givenGetInvitations(arn) // 18 invitations
      givenInactiveRelationships(arn)
      givenInactiveAfiRelationship(arn) // 2 relationships
      givenNinoForMtdItId(MtdItId("JKKL80894713304"), Nino("AB123456A"))
      givenNinoForMtdItId(MtdItId("ABCDE1234567890"), Nino("AB123456A"))
      givenTradingName(Nino("AB123456A"), "FooBar Ltd.")
      givenCitizenDetailsAreKnownFor("AB123456B", "John", "Smith")
      givenCitizenDetailsAreKnownFor("GZ753451B", "Cosmo", "Kramer")
      givenCitizenDetailsAreKnownFor("AB123456A", "Rodney", "Jones")
      givenClientDetails(Vrn("101747696"))
      givenClientDetails(Vrn("101747641"))

      givenTrustClientReturns(validUtr, 200, Json.toJson(trustResponse).toString())
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription()).toString())

      val formData = FilterTrackRequestsForm.form(Set("")).fill(FilterTrackRequests(Some("FooBar Ltd."),Some(ClientNotYetResponded)))
      val formDataWithButton = (formData.data + ("filter" -> "filter")).toSeq
      val result = postTrack(authorisedAsValidAgent(request.withFormUrlEncodedBody(formDataWithButton: _*), arn.value))

      status(result) shouldBe 303

      redirectLocation(result) shouldBe Some(routes.AgentsRequestTrackingController.showTrackRequests(1, Some("FooBar Ltd."), Some(ClientNotYetResponded)).url)

    }

    "have a clear button to clear the filter" in {

      val request = FakeRequest("POST", "/track")
      val postTrack = controller.submitFilterTrackRequests

      givenGetInvitations(arn) // 18 invitations
      givenInactiveRelationships(arn)
      givenInactiveAfiRelationship(arn) // 2 relationships
      givenNinoForMtdItId(MtdItId("JKKL80894713304"), Nino("AB123456A"))
      givenNinoForMtdItId(MtdItId("ABCDE1234567890"), Nino("AB123456A"))
      givenTradingName(Nino("AB123456A"), "FooBar Ltd.")
      givenCitizenDetailsAreKnownFor("AB123456B", "John", "Smith")
      givenCitizenDetailsAreKnownFor("GZ753451B", "Cosmo", "Kramer")
      givenCitizenDetailsAreKnownFor("AB123456A", "Rodney", "Jones")
      givenClientDetails(Vrn("101747696"))
      givenClientDetails(Vrn("101747641"))

      givenTrustClientReturns(validUtr, 200, Json.toJson(trustResponse).toString())
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription()).toString())

      val formData = FilterTrackRequestsForm.form(Set("")).bind(Map("filter" -> "clear"))
      val formDataWithButton = formData.data.toSeq
      val result = postTrack(authorisedAsValidAgent(request.withFormUrlEncodedBody(formDataWithButton: _*), arn.value))

      status(result) shouldBe 303

      redirectLocation(result) shouldBe Some(routes.AgentsRequestTrackingController.showTrackRequests(1, None, None).url)

    }

    "show form with error when client name submitted is not a recognised clientName or the status is not valid" in {

      val request = FakeRequest("POST", "/track")
      val postTrack = controller.submitFilterTrackRequests

      givenGetInvitations(arn) // 18 invitations
      givenInactiveRelationships(arn)
      givenInactiveAfiRelationship(arn) // 2 relationships
      givenNinoForMtdItId(MtdItId("JKKL80894713304"), Nino("AB123456A"))
      givenNinoForMtdItId(MtdItId("ABCDE1234567890"), Nino("AB123456A"))
      givenTradingName(Nino("AB123456A"), "FooBar Ltd.")
      givenCitizenDetailsAreKnownFor("AB123456B", "John", "Smith")
      givenCitizenDetailsAreKnownFor("GZ753451B", "Cosmo", "Kramer")
      givenCitizenDetailsAreKnownFor("AB123456A", "Rodney", "Jones")
      givenClientDetails(Vrn("101747696"))
      givenClientDetails(Vrn("101747641"))

      givenTrustClientReturns(validUtr, 200, Json.toJson(trustResponse).toString())
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription()).toString())

      val formData = FilterTrackRequestsForm.form(Set("")).bind(Map("client" -> "Not a name we know", "status" -> "Bad status" ))
      val formDataWithButton = (formData.data + ("filter" -> "filter")).toSeq
      val result = postTrack(authorisedAsValidAgent(request.withFormUrlEncodedBody(formDataWithButton: _*), arn.value))

      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result,
      "There is a problem",
        "You must select a name from the list",
        "You must select a status from the list"
      )
    }
  }

  "POST /resend-link" should {

    val request = FakeRequest("POST", "/resend-link/")
    val postResendLink = controller.submitToResendLink

    "return 303 and redirect to show resend link page" in {
      givenAgentReference(arn, "uid", personal)
      givenGetAgencyEmailAgentStub
      val expirationDate = LocalDate.now(DateTimeZone.UTC).plusDays(21)
      val formData =
        controller.trackInformationForm.fill(TrackResendForm("HMRC-MTD-IT", Some(personal), expirationDate.toString))
      val result =
        postResendLink(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsRequestTrackingController.showResendLink().url)
    }

    "return 400 BadRequest when form data contains errors in service" in {
      val expirationDate: String = LocalDate.now(DateTimeZone.UTC).plusDays(5).toString
      val formData = controller.trackInformationForm.fill(TrackResendForm("foo", Some(personal), expirationDate))
      val result =
        postResendLink(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }

    "return 400 BadRequest when form data contains errors in clientType" in {
      val dataForm = controller.trackInformationForm.bind(
        Map("service" -> "HMRC-MTD-IT", "clientType" -> "foo", "expiryDate" -> "2019-01-01"))
      val result =
        postResendLink(authorisedAsValidAgent(request.withFormUrlEncodedBody(dataForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }

    "return 400 BadRequest when form data contains errors in expiryDate" in {
      val formData = controller.trackInformationForm.fill(TrackResendForm("HMRC-MTD-IT", Some(personal), "foo"))
      val result =
        postResendLink(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }
  }

  "GET /resend-link" should {

    val request = FakeRequest("GET", "/resend-link/")
    val getResendLink = controller.showResendLink

    "return 200" in {
      val result =
        getResendLink(authorisedAsValidAgent(request.withSession(
          "agentLink" -> "/agent/",
          "clientType" -> "personal",
        "expiryDate" -> "2017-05-05",
        "service" -> "HMRC-MTD-IT",
        "agencyEmail" -> "abc@xyz.com"), arn.value))

      checkHtmlResultWithBodyMsgs(result,
        "invitation-sent.link-text",
        "invitation-sent.select-link",
        "invitation-sent.client-warning",
        "invitation-sent.next-steps.heading",
        "invitation-sent.next-steps.p",
        "invitation-sent.next-steps.link-text.track",
        "invitation-sent.next-steps.link-text.new",
        "invitation-sent.next-steps.link-text.asa")

      checkHtmlResultWithBodyText(result, hasMessage("invitation-sent.client-respond", "5 May 2017", "abc@xyz.com"),
        "Clients who still need help can follow a <a href=https://www.gov.uk/guidance/authorise-an-agent-to-deal-with-certain-tax-services-for-you target=\"_blank\">step-by-step guide to authorising a tax agent (opens in a new tab)</a>")
    }
  }

  "POST /confirm-cancel" should {

    val request = FakeRequest("POST", "/confirm-cancel/")
    val postToConfirmCancel = controller.submitToConfirmCancel

    "return 303 redirect to confirm cancel page when form is correct" in {
      val formData =
        controller.cancelRequestForm.fill(CancelRequestForm(invitationIdITSA.value, "HMRC-MTD-IT", "personal", "Johnny Gogo"))
      val result =
        postToConfirmCancel(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track/confirm-cancel")
    }

    "return 400 BadRequest when form data contains errors in invitationId" in {
      val formData = controller.cancelRequestForm.fill(CancelRequestForm("foo", "HMRC-MTD-IT", "personal", "Johnny Gogo"))
      val result =
        postToConfirmCancel(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }

    "return 400 BadRequest when form data contains errors in service" in {
      val formData = controller.cancelRequestForm.fill(CancelRequestForm(invitationIdITSA.value, "foo", "personal", "Johnny Gogo"))
      val result =
        postToConfirmCancel(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }

  }

  "GET /track/confirm-cancel" should {
    val request = FakeRequest("GET", "/track/confirm-cancel")
    val showConfirmCancel = controller.showConfirmCancel

    "render a confirm cancel page" in {
      val result = showConfirmCancel(
        authorisedAsValidAgent(
          request.withSession("invitationId" -> invitationIdITSA.value, "service" -> "HMRC-MTD-IT"),
          arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Are you sure you want to cancel this authorisation request?",
        "If you cancel this request, you will not be able to manage their Income Tax.",
        "Yes",
        "No"
      )
    }

  }

  "POST /track/confirm-cancel" should {
    val request = FakeRequest("POST", "/track/confirm-cancel")
    val postConfirmCancel = controller.submitConfirmCancel

    "when yes is selected on confirm cancel page, cancel the invitation and send to invitation cancelled page" in {
      givenCancelInvitationReturns(arn, invitationIdITSA, 204)
      val result = postConfirmCancel(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancel" -> "true")
            .withSession("invitationId" -> invitationIdITSA.value, "service" -> "HMRC-MTD-IT", "clientName" -> "Joe Volcano"),
          arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track/request-cancelled")
    }

    "redirect to track page when there is no invitationId in the session" in {
      givenCancelInvitationReturns(arn, invitationIdITSA, 204)
      val result = postConfirmCancel(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancel" -> "true")
            .withSession("clientName" -> "Joe Volcano"),
          arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track")
    }

    "redirect to track page when no service in the session" in {
      givenCancelInvitationReturns(arn, invitationIdITSA, 204)
      val result = postConfirmCancel(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancel" -> "true")
            .withSession("clientName" -> "Joe Volcano", "invitationId" -> invitationIdITSA.value),
          arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track")
    }

    "NotFound when yes is selected on confirm cancel page, but cancellation fails because invitation is not found" in {
      givenCancelInvitationReturns(arn, invitationIdITSA, 404)
      val result = postConfirmCancel(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancel" -> "true")
            .withSession("invitationId" -> invitationIdITSA.value, "service" -> "HMRC-MTD-IT", "clientName" -> "Joe Volcano"),
          arn.value))

      status(result) shouldBe NOT_FOUND
    }

    "Forbidden when yes is selected on confirm cancel page, but cancellation fails because inivtation status is invalid" in {
      givenCancelInvitationReturns(arn, invitationIdITSA, 403)
      val result = postConfirmCancel(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancel" -> "true")
            .withSession("invitationId" -> invitationIdITSA.value, "service" -> "HMRC-MTD-IT", "clientName" -> "Joe Volcano"),
          arn.value))

      status(result) shouldBe FORBIDDEN
    }

    "when no is selected on confirm cancel page, go back to track authorisations page" in {
      val result = postConfirmCancel(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancel" -> "false")
            .withSession("invitationId" -> invitationIdITSA.value, "clientName" -> "Joe Volcano"),
          arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track")
    }

    "when neither option is selected on confirm cancel page show an error" in {
      val result = postConfirmCancel(
        authorisedAsValidAgent(
          request
            .withSession("invitationId" -> invitationIdITSA.value, "service" -> "HMRC-MTD-IT", "clientName" -> "Joe Volcano"),
          arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Select yes if you want to cancel this authorisation request")
    }
  }

  "GET /request-cancelled/" should {
    val request = FakeRequest("POST", "/track/request-cancelled")
    val showRequestCancelled = controller.showRequestCancelled
    "render a request cancelled page" in {
      val result = showRequestCancelled(
        authorisedAsValidAgent(
          request
            .withSession(
              "invitationId" -> invitationIdITSA.value,
              "clientName"   -> "Joe Volcano",
              "service"      -> "HMRC-MTD-IT"),
          arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Authorisation request cancelled",
        "You have cancelled your authorisation request to manage their Income Tax.",
        "Joe Volcano can no longer respond to this request.",
        hasMessage("request-cancelled.p2", "/invitations/agents/client-type")
      )
    }
    "render a request cancelled page without the client's name during an IRV journey" in {
      val result = showRequestCancelled(
        authorisedAsValidAgent(
          request
            .withSession(
              "invitationId" -> invitationIdPIR.value,
              "clientName"   -> "Voe Jolcano",
              "service"      -> "PERSONAL-INCOME-RECORD"),
          arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Authorisation request cancelled",
        "You have cancelled your authorisation request to view their PAYE income record.",
        "Your client can no longer respond to this request."
      )
    }
  }

  "POST /confirm-cancel-authorisation" should {
    val request = FakeRequest("POST", "/confirm-cancel-authorisation")
    val postToConfirmCancelAuth = controller.submitToCancelAuthorisationConfirm

    "return 303 redirect to confirm cancel authorisation page when form is correct" in {
      val formData =
        controller.cancelAuthorisationForm.fill(CancelAuthorisationForm(serviceITSA, validNino.value, "personal", "Sylvia Plath", "ALFE93Y9KAELF"))
      val result = postToConfirmCancelAuth(
        authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track/cancel-authorisation")
    }

    "return 400 BadRequest when form data contains errors in service" in {
      val formData =
        controller.cancelAuthorisationForm.fill(CancelAuthorisationForm("foo", validNino.value, "personal", "Sylvia Plath", "ALFE93Y9KAELF"))
      val result = postToConfirmCancelAuth(
        authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }

    "return 400 BadRequest when form data contains errors in clientId" in {
      val formData =
        controller.cancelAuthorisationForm.fill(CancelAuthorisationForm(serviceITSA, "foo", "personal", "Sylvia Plath", "ALFE93Y9KAELF"))
      val result = postToConfirmCancelAuth(
        authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }
  }

  "GET /track/cancel-authorisation" should {
    val request = FakeRequest("GET", "/track/cancel-authorisation")
    val showConfirmCancelAuth = controller.showCancelAuthorisationConfirm

    "render a confirm cancel authorisation page" in {
      val result =
        showConfirmCancelAuth(authorisedAsValidAgent(request.withSession("service" -> serviceITSA), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Are you sure you want to cancel this client’s authorisation?",
        "You will no longer be able to manage their Income Tax.",
        "You will not be able to undo this action.",
        "Yes",
        "No"
      )
    }
  }

  "POST /track/cancel-authorisation" should {
    val request = FakeRequest("POST", "/track/cancel-authorisation")
    val postConfirmCancelAuth = controller.submitCancelAuthorisationConfirm

    "when yes is selected on confirm cancel authorisation page, cancel the authorisation and redirect to authorisation cancelled page for ITSA" in {
      givenCancelledAuthorisationItsa(arn, validNino, 204)
      givenSetRelationshipEndedReturns(invitationIdITSA, 204)
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
            .withSession("service" -> serviceITSA, "clientId" -> validNino.value, "clientName" -> "Joe Volcano", "invitationId" -> invitationIdITSA.value),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track/cancelled")
    }

    "when yes is selected on confirm cancel authorisation page, cancel the authorisation and redirect to authorisation cancelled page for IRV" in {
      givenTerminateAfiRelationshipSucceeds(arn, "PERSONAL-INCOME-RECORD", validNino.value)
      givenSetRelationshipEndedReturns(invitationIdPIR, 204)
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
            .withSession("service" -> servicePIR, "clientId" -> validNino.value, "clientName" -> "Joe Volcano", "invitationId" -> invitationIdPIR.value),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track/cancelled")
    }

    "when yes is selected on confirm cancel authorisation page, cancel the authorisation and redirect to authorisation cancelled page for VAT" in {
      givenCancelledAuthorisationVat(arn, validVrn, 204)
      givenSetRelationshipEndedReturns(invitationIdVAT, 204)
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
            .withSession("service" -> serviceVAT, "clientId" -> validVrn.value, "clientName" -> "Joe Volcano", "invitationId" -> invitationIdVAT.value),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track/cancelled")
    }

    "when yes is selected on confirm cancel authorisation page, but service is session is not supported throw error" in {
      givenCancelledAuthorisationVat(arn, validVrn, 204)
      givenSetRelationshipEndedReturns(invitationIdVAT, 204)
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
            .withSession("service" -> "foo", "clientId" -> validVrn.value, "clientName" -> "Joe Volcano", "invitationId" -> invitationIdVAT.value),
          arn.value
        ))

      an[Exception] shouldBe thrownBy {
        await(result)
      }
    }

    "mark the relationship as ended in ACA and go to showAuthorisationCancelledPage even if relationship service returns 404" in {
      givenCancelledAuthorisationItsa(arn, validNino, 404)
      givenSetRelationshipEndedReturns(invitationIdITSA, 204)
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
            .withSession("service" -> serviceITSA, "clientId" -> validNino.value, "clientName" -> "Joe Volcano", "invitationId" -> invitationIdITSA.value),
          arn.value
        ))

      status(result) shouldBe 303
    }

    "go to problem page when yes is selected on confirm cancel authorisation page, but relationship deletion fails for some other reason" in {
      givenCancelledAuthorisationItsa(arn, validNino, 403)
      givenSetRelationshipEndedReturns(invitationIdITSA, 204)
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
            .withSession("service" -> serviceITSA, "clientId" -> validNino.value, "clientName" -> "Joe Volcano", "invitationId" -> invitationIdITSA.value),
          arn.value
        ))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Sorry, there is a problem with the service",
        "The authorisation was not cancelled. Please try again.",
        "Try again")
    }

    "when no is selected on confirm cancel authorisation page, go back to track authorisations page" in {
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "false")
            .withSession("service" -> serviceITSA, "clientId" -> validNino.value, "clientName" -> "Joe Volcano"),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track")
    }

    "when neither option is selected on confirm cancel page show an error" in {
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withSession("service" -> serviceITSA, "clientId" -> validNino.value, "clientName" -> "Joe Volcano"),
          arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Select yes if you want to cancel this authorisation request")
    }
  }

  "GET /track/cancelled" should {

    val request = FakeRequest("GET", "/track/cancel-authorisation")
    val showAuthCancelled = controller.showAuthorisationCancelled

    "render a authorisation cancelled page for ITSA" in {
      val result = showAuthCancelled(
        authorisedAsValidAgent(
          request
            .withSession(
              "invitationId" -> invitationIdITSA.value,
              "clientId"     -> validNino.value,
              "clientName"   -> "Buttercup Powerpuff",
              "service"      -> "HMRC-MTD-IT"),
          arn.value
        ))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Authorisation cancelled",
        "What this means",
        "You are no longer authorised by Buttercup Powerpuff to manage their Income Tax.",
        "Return to track your recent authorisation requests"
      )
    }

    "render a authorisation cancelled page for IRV" in {
      val result = showAuthCancelled(
        authorisedAsValidAgent(
          request
            .withSession(
              "invitationId" -> invitationIdPIR.value,
              "clientId"     -> validNino.value,
              "clientName"   -> "Bubbles Powerpuff",
              "service"      -> "PERSONAL-INCOME-RECORD"),
          arn.value
        ))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Authorisation cancelled",
        "What this means",
        "You are no longer authorised by Bubbles Powerpuff to view their PAYE income record.",
        "Return to track your recent authorisation requests"
      )
    }

    "render a authorisation cancelled page for VAT" in {
      val result = showAuthCancelled(
        authorisedAsValidAgent(
          request
            .withSession(
              "invitationId" -> invitationIdVAT.value,
              "clientId"     -> validVrn.value,
              "clientName"   -> "Blossom Powerpuff",
              "service"      -> "HMRC-MTD-VAT"),
          arn.value
        ))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Authorisation cancelled",
        "What this means",
        "You are no longer authorised by Blossom Powerpuff to manage their " +
          "VAT.",
        "Return to track your recent authorisation requests"
      )
    }
  }
}
