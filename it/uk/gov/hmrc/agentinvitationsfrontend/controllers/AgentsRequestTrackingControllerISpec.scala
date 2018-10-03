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
      parseHtml.getElementsByAttributeValue("class", "row-0").toString should include("FooBar Ltd.")
      parseHtml.getElementsByAttributeValue("class", "row-0").toString should include("Report their income or expenses through software")
      parseHtml.getElementsByAttributeValue("class", "row-3").toString should include("GDT")
      parseHtml.getElementsByAttributeValue("class", "row-3").toString should include("Report their VAT returns through software")
      parseHtml.getElementsByAttributeValue("class", "row-3").toString should include("resendRequest")
      parseHtml.getElementsByAttributeValue("class", "row-7").toString should include("John Smith")
      parseHtml.getElementsByAttributeValue("class", "row-7").toString should include("View their PAYE income record")
      parseHtml.getElementsByAttributeValue("class", "row-23").toString should include("Rodney Jones")
      parseHtml.getElementsByAttributeValue("class", "row-23").toString should include("View their PAYE income record")

      parseHtml.getElementsByAttributeValue("class", "row-15").toString should include("expired")
      parseHtml.getElementsByAttributeValue("class", "row-15").toString should include("fastTrackInvitationCreate")
      parseHtml.getElementsByAttributeValue("class", "row-8").toString should include("Declined")
      parseHtml.getElementsByAttributeValue("class", "row-8").toString should include("fastTrackInvitationCreate")
      parseHtml.getElementsByAttributeValue("class", "row-11").toString should include("cancelled this request")
      parseHtml.getElementsByAttributeValue("class", "row-11").toString should include("fastTrackInvitationCreate")
      parseHtml.getElementsByAttributeValue("class", "row-3").toString should include("cancelled your authorisation")
      parseHtml.getElementsByAttributeValue("class", "row-3").toString should include("fastTrackInvitationCreate")

    }

    "render a page listing non-empty invitations without client's names" in {
      givenAllInvitationsStub(arn)
      givenInactiveITSARelationships(arn)
      givenInactiveVATRelationships(arn)
      givenInactiveRelationshipsIrv(arn)
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
      givenAllInvitationsEmptyStub(arn)
      givenInactiveITSARelationshipsNotFound
      givenInactiveVATRelationshipsNotFound
      givenInactiveRelationshipsIrvNotFound
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
      val expirationDate: String = LocalDate.now(DateTimeZone.UTC).plusDays(5).toString
      val formData = controller.trackInformationForm.fill(TrackResendForm("HMRC-MTD-IT", invitationIdITSA.value, expirationDate))
      val result = postResendLink(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Resend this link to your client",
        "What you need to do next",
        "The link expires 5 days from now.",
        "Track your recent authorisation requests",
        "Return to your agent services account",
        "Start new authorisation request")
    }

    "return 400 BadRequest when form data contains errors in service" in {
      val expirationDate: String = LocalDate.now(DateTimeZone.UTC).plusDays(5).toString
      val formData = controller.trackInformationForm.fill(TrackResendForm("foo", invitationIdITSA.value, expirationDate))
      val result = postResendLink(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }

    "return 400 BadRequest when form data contains errors in invitationId" in {
      val expirationDate: String = LocalDate.now(DateTimeZone.UTC).plusDays(5).toString
      val formData = controller.trackInformationForm.fill(TrackResendForm("HMRC-MTD-IT", "foo", expirationDate))
      val result = postResendLink(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }

    "return 400 BadRequest when form data contains errors in expiryDate" in {
      val expirationDate: String = LocalDate.now(DateTimeZone.UTC).plusDays(5).toString
      val formData = controller.trackInformationForm.fill(TrackResendForm("HMRC-MTD-IT", invitationIdITSA.value, "foo"))
      val result = postResendLink(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }
  }

  "POST /confirm-cancel" should {

    val request = FakeRequest("POST", "/confirm-cancel/")
    val postToConfirmCancel = controller.submitToConfirmCancel

    "return 303 redirect to confirm cancel page when form is correct" in {
      val formData = controller.cancelRequestForm.fill(CancelRequestForm(invitationIdITSA.value, "HMRC-MTD-IT", "Johnny Gogo"))
      val result = postToConfirmCancel(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track/confirm-cancel")
    }

    "return 400 BadRequest when form data contains errors in invitationId" in {
      val formData = controller.cancelRequestForm.fill(CancelRequestForm("foo", "HMRC-MTD-IT", "Johnny Gogo"))
      val result = postToConfirmCancel(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }

    "return 400 BadRequest when form data contains errors in service" in {
      val formData = controller.cancelRequestForm.fill(CancelRequestForm(invitationIdITSA.value, "foo", "Johnny Gogo"))
      val result = postToConfirmCancel(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }

  }

  "GET /track/confirm-cancel" should {
    val request = FakeRequest("GET", "/track/confirm-cancel")
    val showConfirmCancel = controller.getConfirmCancel

    "render a confirm cancel page" in {
      val result = showConfirmCancel(authorisedAsValidAgent(request.withSession("invitationId" -> invitationIdITSA.value), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Are you sure you want to cancel this authorisation request?",
        "If you cancel this request, you will not be able to report their income or expenses through software.",
      "Yes", "No")
    }

  }

  "POST /track/confirm-cancel" should {
    val request = FakeRequest("POST", "/track/confirm-cancel")
    val postConfirmCancel = controller.submitConfirmCancel

    "when yes is selected on confirm cancel page, cancel the invitation and send to invitation cancelled page" in {
      cancelInvitationStub(arn, invitationIdITSA, 204)
      val result = postConfirmCancel(authorisedAsValidAgent(request.withFormUrlEncodedBody("confirmCancel" -> "true")
        .withSession("invitationId" -> invitationIdITSA.value, "clientName" -> "Joe Volcano"), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Authorisation request cancelled",
        "You have cancelled your authorisation request to report their income or expenses through software.",
        "Joe Volcano will not be able to respond to this request.", "Made a mistake?")
    }

    "NotFound when yes is selected on confirm cancel page, but cancellation fails because invitation is not found" in {
      cancelInvitationStub(arn, invitationIdITSA, 404)
      val result = postConfirmCancel(authorisedAsValidAgent(request.withFormUrlEncodedBody("confirmCancel" -> "true")
        .withSession("invitationId" -> invitationIdITSA.value, "clientName" -> "Joe Volcano"), arn.value))

      status(result) shouldBe NOT_FOUND
    }

    "Forbidden when yes is selected on confirm cancel page, but cancellation fails because inivtation status is invalid" in {
      cancelInvitationStub(arn, invitationIdITSA, 403)
      val result = postConfirmCancel(authorisedAsValidAgent(request.withFormUrlEncodedBody("confirmCancel" -> "true")
        .withSession("invitationId" -> invitationIdITSA.value, "clientName" -> "Joe Volcano"), arn.value))

      status(result) shouldBe FORBIDDEN
    }

    "when no is selected on confirm cancel page, go back to track authorisations page" in {
      val result = postConfirmCancel(authorisedAsValidAgent(request.withFormUrlEncodedBody("confirmCancel" -> "false")
        .withSession("invitationId" -> invitationIdITSA.value, "clientName" -> "Joe Volcano"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track")
    }

    "when neither option is selected on confirm cancel page show an error" in {
      val result = postConfirmCancel(authorisedAsValidAgent(request
        .withSession("invitationId" -> invitationIdITSA.value, "clientName" -> "Joe Volcano"), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "This field is required")
    }

  }

  "POST /confirm-cancel-authorisation" should {
    val request = FakeRequest("POST", "/confirm-cancel-authorisation")
    val postToConfirmCancelAuth = controller.submitToCancelAuthorisationConfirm

    "return 303 redirect to confirm cancel authorisation page when form is correct" in {
      val formData = controller.cancelAuthorisationForm.fill(CancelAuthorisationForm(serviceITSA, validNino.value, "Sylvia Plath"))
      val result = postToConfirmCancelAuth(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track/cancel-authorisation")
    }

    "return 400 BadRequest when form data contains errors in service" in {
      val formData = controller.cancelAuthorisationForm.fill(CancelAuthorisationForm("foo", validNino.value, "Sylvia Plath"))
      val result = postToConfirmCancelAuth(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }

    "return 400 BadRequest when form data contains errors in clientId" in {
      val formData = controller.cancelAuthorisationForm.fill(CancelAuthorisationForm(serviceITSA, "foo", "Sylvia Plath"))
      val result = postToConfirmCancelAuth(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }
  }

  "GET /track/cancel-authorisation" should {
    val request = FakeRequest("GET", "/track/cancel-authorisation")
    val showConfirmCancelAuth = controller.showCancelAuthorisationConfirm

    "render a confirm cancel authorisation page" in {
      val result = showConfirmCancelAuth(authorisedAsValidAgent(request.withSession("service" -> serviceITSA), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Are you sure you want to cancel this client's authorisation?",
        "You will no longer be able to report their income or expenses through software.", "You will not be able to undo this action.",
        "Yes", "No")
    }
  }

  "POST /track/cancel-authorisation" should {
    val request = FakeRequest("POST", "/track/cancel-authorisation")
    val postConfirmCancelAuth = controller.submitCancelAuthorisationConfirm

    "when yes is selected on confirm cancel authorisation page, cancel the authorisation and send to authorisation cancelled page for ITSA" in {
      givenCancelledAuthorisationItsa(arn, validNino, 204)
      val result = postConfirmCancelAuth(authorisedAsValidAgent(request.withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
        .withSession("service" -> serviceITSA, "clientId" -> validNino.value, "clientName" -> "Joe Volcano"), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Authorisation cancelled",
        "What this means",
        "You are no longer authorised by Joe Volcano to report their income or expenses through software.",
        "Return to track your recent authorisation requests")
    }

    "when yes is selected on confirm cancel authorisation page, cancel the authorisation and send to authorisation cancelled page for IRV" in {
      deleteRelationship(arn, "PERSONAL-INCOME-RECORD", validNino.value)
      val result = postConfirmCancelAuth(authorisedAsValidAgent(request.withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
        .withSession("service" -> servicePIR, "clientId" -> validNino.value, "clientName" -> "Joe Volcano"), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Authorisation cancelled",
        "What this means",
        "You are no longer authorised by Joe Volcano to view their PAYE income record.",
        "Return to track your recent authorisation requests")
    }

    "when yes is selected on confirm cancel authorisation page, cancel the authorisation and send to authorisation cancelled page for VAT" in {
      givenCancelledAuthorisationVat(arn, validVrn, 204)
      val result = postConfirmCancelAuth(authorisedAsValidAgent(request.withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
        .withSession("service" -> serviceVAT, "clientId" -> validVrn.value, "clientName" -> "Joe Volcano"), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Authorisation cancelled",
        "What this means",
        "You are no longer authorised by Joe Volcano to report their VAT returns through software.",
        "Return to track your recent authorisation requests")
    }

    "when yes is selected on confirm cancel authorisation page, but service is session is not supported throw error" in {
      givenCancelledAuthorisationVat(arn, validVrn, 204)
      val result = postConfirmCancelAuth(authorisedAsValidAgent(request.withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
        .withSession("service" -> "foo", "clientId" -> validVrn.value, "clientName" -> "Joe Volcano"), arn.value))

      an[Exception] shouldBe thrownBy {
        await(result)
      }
    }



  "NotFound when yes is selected on confirm cancel authorisation page, but cancellation fails because relationship is not found" in {
      givenCancelledAuthorisationItsa(arn, validNino, 404)
      val result = postConfirmCancelAuth(authorisedAsValidAgent(request.withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
        .withSession("service" -> serviceITSA, "clientId" -> validNino.value, "clientName" -> "Joe Volcano"), arn.value))

      status(result) shouldBe NOT_FOUND
    }

    "Forbidden when yes is selected on confirm cancel authorisation page, but relationship deletion fails for some other reason" in {
      givenCancelledAuthorisationItsa(arn, validNino, 403)
      val result = postConfirmCancelAuth(authorisedAsValidAgent(request.withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
        .withSession("service" -> serviceITSA, "clientId" -> validNino.value, "clientName" -> "Joe Volcano"), arn.value))

      status(result) shouldBe FORBIDDEN
    }

    "when no is selected on confirm cancel authorisation page, go back to track authorisations page" in {
      val result = postConfirmCancelAuth(authorisedAsValidAgent(request.withFormUrlEncodedBody("confirmCancelAuthorisation" -> "false")
        .withSession("service" -> serviceITSA, "clientId" -> validNino.value, "clientName" -> "Joe Volcano"), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track")
    }

    "when neither option is selected on confirm cancel page show an error" in {
      val result = postConfirmCancelAuth(authorisedAsValidAgent(request
        .withSession("service" -> serviceITSA, "clientId" -> validNino.value, "clientName" -> "Joe Volcano"), arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "This field is required")
    }
  }
}

