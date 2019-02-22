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

import org.joda.time.{DateTimeZone, LocalDate}
import org.jsoup.Jsoup
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

class AgentsRequestTrackingControllerISpec extends BaseISpec with AuthBehaviours {

  lazy val controller: AgentsRequestTrackingController = app.injector.instanceOf[AgentsRequestTrackingController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  "GET /track/" should {

    val request = FakeRequest("GET", "/track/")
    val showTrackRequests = controller.showTrackRequests

    "render a page listing non-empty invitations with client's names resolved" in {
      givenGetInvitations(arn)
      givenInactiveITSARelationships(arn)
      givenInactiveVATRelationships(arn)
      givenInactiveAfiRelationship(arn)
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
        "Accepted by client",
        "Client has not yet responded",
        "Declined by client",
        "Request expired as client did not respond in time",
        "You cancelled this request",
        "You cancelled your authorisation",
        "FooBar Ltd.",
        "John Smith",
        "Cosmo Kramer",
        "GDT",
        "11 September 2018",
        "21 September 2015",
        "24 September 2018",
        "01 January 2099",
        "Resend request to client",
        "Cancel this request",
        "Start new request",
        "Cancel your authorisation",
        htmlEscapedMessage("recent-invitations.description", 30)
      )
      checkHtmlResultWithBodyMsgs(
        result,
        "recent-invitations.header",
        "recent-invitations.table-row-header.clientName",
        "recent-invitations.table-row-header.service",
        "recent-invitations.table-row-header.status",
        "recent-invitations.table-row-header.actions",
        "recent-invitations.invitation.service.HMRC-MTD-IT",
        "recent-invitations.invitation.service.HMRC-MTD-VAT",
        "recent-invitations.invitation.service.PERSONAL-INCOME-RECORD"
      )

      val parseHtml = Jsoup.parse(contentAsString(result))
      println(parseHtml)
      parseHtml.getElementsByAttributeValue("id","row-0").toString should include("FooBar Ltd.")
      parseHtml.getElementsByAttributeValue("id","row-0").toString should include(
        "Report their income and expenses through software")
      parseHtml.getElementsByAttributeValue("id","row-3").toString should include("GDT")
      parseHtml.getElementsByAttributeValue("id","row-3").toString should include(
        "Report their VAT returns through software")
      parseHtml.getElementsByAttributeValue("id","row-3").toString should include("resendRequest")
      parseHtml.getElementsByAttributeValue("id","row-7").toString should include("John Smith")
      parseHtml.getElementsByAttributeValue("id","row-7").toString should include("View their PAYE income record")
      parseHtml.getElementsByAttributeValue("id","row-23").toString should include("Rodney Jones")
      parseHtml.getElementsByAttributeValue("id","row-23").toString should include("View their PAYE income record")

      parseHtml.getElementsByAttributeValue("id","row-8").toString should include("Declined")
      parseHtml.getElementsByAttributeValue("id","row-8").toString should include("fastTrackInvitationCreate")
      parseHtml.getElementsByAttributeValue("id","row-11").toString should include("cancelled this request")
      parseHtml.getElementsByAttributeValue("id","row-11").toString should include("fastTrackInvitationCreate")
      parseHtml.getElementsByAttributeValue("id","row-3").toString should include("cancelled your authorisation")
      parseHtml.getElementsByAttributeValue("id","row-3").toString should include("fastTrackInvitationCreate")

    }

    "render a page listing non-empty invitations without client's names" in {
      givenGetInvitations(arn)
      givenInactiveITSARelationships(arn)
      givenInactiveVATRelationships(arn)
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
      val result = showTrackRequests(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Accepted by client",
        "Client has not yet responded",
        "Declined by client",
        "Request expired as client did not respond in time",
        "You cancelled this request",
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
      givenGetInvitationsReturnsEmpty(arn)
      givenInactiveITSARelationshipsNotFound
      givenInactiveVATRelationshipsNotFound
      givenInactiveAfiRelationshipNotFound
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

  "POST /resend-link" should {

    val request = FakeRequest("POST", "/resend-link/")
    val postResendLink = controller.submitToResendLink

    "return 200 and go to resend link page" in {
      givenAgentReference(arn, "uid", "personal")
      val expirationDate: String = LocalDate.now(DateTimeZone.UTC).plusDays(14).toString
      val formData =
        controller.trackInformationForm.fill(TrackResendForm("HMRC-MTD-IT", Some("personal"), expirationDate))
      val result =
        postResendLink(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Resend this link to your client",
        "What you need to do next",
        "Copy this link and email it to your client.",
        "individual or sole trader tax affairs",
        "Track your recent authorisation requests",
        "Return to your agent services account",
        "Start new authorisation request"
      )
    }

    "return 400 BadRequest when form data contains errors in service" in {
      val expirationDate: String = LocalDate.now(DateTimeZone.UTC).plusDays(5).toString
      val formData = controller.trackInformationForm.fill(TrackResendForm("foo", Some("personal"), expirationDate))
      val result =
        postResendLink(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }

    "return 400 BadRequest when form data contains errors in invitationId" in {
      val expirationDate: String = LocalDate.now(DateTimeZone.UTC).plusDays(5).toString
      val formData = controller.trackInformationForm.fill(TrackResendForm("HMRC-MTD-IT", Some("foo"), expirationDate))
      val result =
        postResendLink(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }

    "return 400 BadRequest when form data contains errors in expiryDate" in {
      val expirationDate: String = LocalDate.now(DateTimeZone.UTC).plusDays(5).toString
      val formData = controller.trackInformationForm.fill(TrackResendForm("HMRC-MTD-IT", Some("personal"), "foo"))
      val result =
        postResendLink(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }
  }

  "POST /confirm-cancel" should {

    val request = FakeRequest("POST", "/confirm-cancel/")
    val postToConfirmCancel = controller.submitToConfirmCancel

    "return 303 redirect to confirm cancel page when form is correct" in {
      val formData =
        controller.cancelRequestForm.fill(CancelRequestForm(invitationIdITSA.value, "HMRC-MTD-IT", "Johnny Gogo"))
      val result =
        postToConfirmCancel(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track/confirm-cancel")
    }

    "return 400 BadRequest when form data contains errors in invitationId" in {
      val formData = controller.cancelRequestForm.fill(CancelRequestForm("foo", "HMRC-MTD-IT", "Johnny Gogo"))
      val result =
        postToConfirmCancel(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }

    "return 400 BadRequest when form data contains errors in service" in {
      val formData = controller.cancelRequestForm.fill(CancelRequestForm(invitationIdITSA.value, "foo", "Johnny Gogo"))
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
        "If you cancel this request, you will not be able to report their income and expenses through software.",
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
            .withSession("invitationId" -> invitationIdITSA.value, "clientName" -> "Joe Volcano"),
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

    "NotFound when yes is selected on confirm cancel page, but cancellation fails because invitation is not found" in {
      givenCancelInvitationReturns(arn, invitationIdITSA, 404)
      val result = postConfirmCancel(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancel" -> "true")
            .withSession("invitationId" -> invitationIdITSA.value, "clientName" -> "Joe Volcano"),
          arn.value))

      status(result) shouldBe NOT_FOUND
    }

    "Forbidden when yes is selected on confirm cancel page, but cancellation fails because inivtation status is invalid" in {
      givenCancelInvitationReturns(arn, invitationIdITSA, 403)
      val result = postConfirmCancel(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancel" -> "true")
            .withSession("invitationId" -> invitationIdITSA.value, "clientName" -> "Joe Volcano"),
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
            .withSession("invitationId" -> invitationIdITSA.value, "clientName" -> "Joe Volcano"),
          arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "This field is required")
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
        "You have cancelled your authorisation request to report their income and expenses through software.",
        "Joe Volcano will not be able to respond to this request.",
        "Made a mistake?",
        hasMessage("request-cancelled.p2", "/invitations/agents/client-type")
      )
    }
  }

  "POST /confirm-cancel-authorisation" should {
    val request = FakeRequest("POST", "/confirm-cancel-authorisation")
    val postToConfirmCancelAuth = controller.submitToCancelAuthorisationConfirm

    "return 303 redirect to confirm cancel authorisation page when form is correct" in {
      val formData =
        controller.cancelAuthorisationForm.fill(CancelAuthorisationForm(serviceITSA, validNino.value, "Sylvia Plath"))
      val result = postToConfirmCancelAuth(
        authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track/cancel-authorisation")
    }

    "return 400 BadRequest when form data contains errors in service" in {
      val formData =
        controller.cancelAuthorisationForm.fill(CancelAuthorisationForm("foo", validNino.value, "Sylvia Plath"))
      val result = postToConfirmCancelAuth(
        authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }

    "return 400 BadRequest when form data contains errors in clientId" in {
      val formData =
        controller.cancelAuthorisationForm.fill(CancelAuthorisationForm(serviceITSA, "foo", "Sylvia Plath"))
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
        "Are you sure you want to cancel this client's authorisation?",
        "You will no longer be able to report their income and expenses through software.",
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
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
            .withSession("service" -> serviceITSA, "clientId" -> validNino.value, "clientName" -> "Joe Volcano"),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track/cancelled")
    }

    "when yes is selected on confirm cancel authorisation page, cancel the authorisation and redirect to authorisation cancelled page for IRV" in {
      givenTerminateAfiRelationshipSucceeds(arn, "PERSONAL-INCOME-RECORD", validNino.value)
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
            .withSession("service" -> servicePIR, "clientId" -> validNino.value, "clientName" -> "Joe Volcano"),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track/cancelled")
    }

    "when yes is selected on confirm cancel authorisation page, cancel the authorisation and redirect to authorisation cancelled page for VAT" in {
      givenCancelledAuthorisationVat(arn, validVrn, 204)
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
            .withSession("service" -> serviceVAT, "clientId" -> validVrn.value, "clientName" -> "Joe Volcano"),
          arn.value
        ))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track/cancelled")
    }

    "when yes is selected on confirm cancel authorisation page, but service is session is not supported throw error" in {
      givenCancelledAuthorisationVat(arn, validVrn, 204)
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
            .withSession("service" -> "foo", "clientId" -> validVrn.value, "clientName" -> "Joe Volcano"),
          arn.value
        ))

      an[Exception] shouldBe thrownBy {
        await(result)
      }
    }

    "return not found when yes is selected on confirm cancel authorisation page, but cancellation fails because relationship is not found" in {
      givenCancelledAuthorisationItsa(arn, validNino, 404)
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
            .withSession("service" -> serviceITSA, "clientId" -> validNino.value, "clientName" -> "Joe Volcano"),
          arn.value
        ))

      status(result) shouldBe 404
    }

    "go to problem page when yes is selected on confirm cancel authorisation page, but relationship deletion fails for some other reason" in {
      givenCancelledAuthorisationItsa(arn, validNino, 403)
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
            .withSession("service" -> serviceITSA, "clientId" -> validNino.value, "clientName" -> "Joe Volcano"),
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
      checkHtmlResultWithBodyText(result, "This field is required")
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
        "You are no longer authorised by Buttercup Powerpuff to report their income and expenses through software.",
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
        "You are no longer authorised by Blossom Powerpuff to report their VAT returns through software.",
        "Return to track your recent authorisation requests"
      )
    }
  }
}
