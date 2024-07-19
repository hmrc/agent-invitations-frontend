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

import java.time.{LocalDate, LocalDateTime}
import org.jsoup.Jsoup
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.test.Helpers
import uk.gov.hmrc.agentinvitationsfrontend.forms.FilterTrackRequestsForm
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.Personal
import uk.gov.hmrc.agentinvitationsfrontend.models.FilterFormStatus.AcceptedByClient
import uk.gov.hmrc.agentinvitationsfrontend.models.FilterTrackRequests
import uk.gov.hmrc.agentinvitationsfrontend.support.{BaseISpec, Css}
import uk.gov.hmrc.agentmtdidentifiers.model.{MtdItId, Service, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.SessionId

import java.time.format.DateTimeFormatter
import java.util.UUID

class AgentsRequestTrackingControllerISpec extends BaseISpec with AuthBehaviours {

  lazy val controller: AgentsRequestTrackingController = app.injector.instanceOf[AgentsRequestTrackingController]
  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId(UUID.randomUUID().toString)))

  val fmt: DateTimeFormatter = DateTimeFormatter.ofPattern("d MMMM yyyy")
  private def displayDate(dt: LocalDate): String =
    dt.format(fmt)

  def nowMinus(d: Int): LocalDateTime = LocalDateTime.now().minusDays(d)

  "GET /track/" should {

    val request = FakeRequest("GET", "/track/")
    val showTrackRequestsPageOne = controller.showTrackRequests(1, None, None)
    val showTrackRequestsPageTwo = controller.showTrackRequests(2, None, None)

    "render a page with filter form listing non-empty invitations with client's names" in {

      givenGetInvitationsTrack() // 12 invitations
      givenInactiveRelationships() // 4 relationships
      given2InactiveAfiRelationships(nowMinus(3), nowMinus(8)) // 2 relationship
      givenNinoForMtdItId(MtdItId("ABCDE1234567890"), Nino("AB123456A"))
      givenPutAltItsaAuth(arn)

      val resultPageOne = showTrackRequestsPageOne(authorisedAsValidAgent(request, arn.value))
      status(resultPageOne) shouldBe 200
      checkHtmlResultWithBodyText(
        resultPageOne,
        "Aaa Itsa Trader",
        "D Trust",
        "Excel Ltd",
        "Property Dev",
        "Accepted by client. They later cancelled their authorisation",
        "You cancelled your authorisation",
        "Client has not yet responded",
        s"${displayDate(LocalDate.now().minusDays(10))}",
        s"${displayDate(LocalDate.now().minusDays(1))}",
        s"${displayDate(LocalDate.now().minusDays(3))}",
        s"${displayDate(LocalDate.now().minusDays(5))}",
        s"${displayDate(LocalDate.now())}",
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

      val parseHtml = Jsoup.parse(Helpers.contentAsString(resultPageOne))

      parseHtml.getElementsByAttributeValue("id", "row-0").toString should include("Ddd Itsa Trader")
      parseHtml.getElementsByAttributeValue("id", "row-0").toString should include("Manage their Making Tax Digital for Income Tax")

      val resultPageTwo = showTrackRequestsPageTwo(authorisedAsValidAgent(request, arn.value))
      status(resultPageTwo) shouldBe 200

      checkHtmlResultWithBodyText(
        resultPageTwo,
        "Accepted",
        "You cancelled this request"
      )

      val parseHtmlPageTwo = Jsoup.parse(Helpers.contentAsString(resultPageTwo))

      parseHtmlPageTwo.getElementsByAttributeValue("id", "row-1").toString should include("Bbb Itsa Trader")
      parseHtmlPageTwo.getElementsByAttributeValue("id", "row-1").toString should include("Manage their Making Tax Digital for Income Tax")

    }

    "render a page listing empty invitations" in {
      givenGetInvitationsReturnsEmpty(arn)
      givenInactiveRelationshipsNotFound
      givenInactiveAfiRelationshipNotFound
      givenPutAltItsaAuth(arn)
      val result = showTrackRequestsPageOne(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("recent-invitations.description", 30))
      checkHtmlResultWithBodyMsgs(result, "recent-invitations.header", "recent-invitations.empty", "recent-invitations.empty.continue")
    }

    "accept valid request with filter by client query param" in {

      val request = FakeRequest("GET", "/track?page=1&client=Ddd+Itsa+Trader")
      val showTrackRequestsPageOne = controller.showTrackRequests(1, Some("Ddd Itsa Trader"), None)

      givenGetInvitationsTrack() // 12 invitations
      givenInactiveRelationships() // 4 relationships
      given2InactiveAfiRelationships(nowMinus(3), nowMinus(8)) // 2 relationship
      givenNinoForMtdItId(MtdItId("ABCDE1234567890"), Nino("AB123456A"))
      givenPutAltItsaAuth(arn)

      val resultPageOne = showTrackRequestsPageOne(authorisedAsValidAgent(request, arn.value))
      status(resultPageOne) shouldBe 200

      val parseHtml = Jsoup.parse(Helpers.contentAsString(resultPageOne))

      parseHtml.getElementsByAttributeValue("id", "row-0").toString should include("Ddd Itsa Trader")
      parseHtml.getElementsByAttributeValue("id", "row-1").toString should include("Ddd Itsa Trader")
      parseHtml.getElementsByAttributeValue("id", "row-2").toString should include("Ddd Itsa Trader")
    }

    "accept valid request with filter by client and status query params" in {

      val request = FakeRequest("GET", "/track?page=1&client=Ddd+Itsa+Trader&status=Accepted")
      val showTrackRequestsPageOne = controller.showTrackRequests(1, Some("Ddd Itsa Trader"), Some(AcceptedByClient))

      givenGetInvitationsTrack() // 12 invitations
      givenInactiveRelationships() // 4 relationships
      given2InactiveAfiRelationships(nowMinus(3), nowMinus(8)) // 2 relationship
      givenNinoForMtdItId(MtdItId("ABCDE1234567890"), Nino("AB123456A"))
      givenPutAltItsaAuth(arn)

      val resultPageOne = showTrackRequestsPageOne(authorisedAsValidAgent(request, arn.value))
      status(resultPageOne) shouldBe 200

      val parseHtml = Jsoup.parse(Helpers.contentAsString(resultPageOne))

      parseHtml.getElementsByAttributeValue("id", "row-0").toString should include("Ddd Itsa Trader")
    }

    "render Country by country non UK pending invitations" in {

      givenGetCbcInvitations()
      givenInactiveCbcRelationships()
      givenInactiveAfiRelationshipNotFound
      givenPutAltItsaAuth(arn)

      val resultPageOne = showTrackRequestsPageOne(authorisedAsValidAgent(request, arn.value))
      status(resultPageOne) shouldBe 200
      checkHtmlResultWithBodyText(
        resultPageOne,
        "Cbc UK Company",
        "Cbc Non-UK Company",
        "You cancelled your authorisation",
        s"${displayDate(LocalDate.now().minusDays(10))}",
        s"${displayDate(LocalDate.now().minusDays(10))}",
        "Start new request",
        htmlEscapedMessage("recent-invitations.description", 30)
      )

    }

    behave like anAuthorisedAgentEndpoint(request, showTrackRequestsPageOne)
  }

  "POST /track" should {

    "accept form with valid inputs" in {

      val request = FakeRequest("POST", "/track")
      val postTrack = controller.submitFilterTrackRequests

      givenGetInvitationsTrack() // 12 invitations
      givenInactiveRelationships() // 4 relationships
      given2InactiveAfiRelationships(nowMinus(3), nowMinus(8)) // 2 relationship
      givenNinoForMtdItId(MtdItId("ABCDE1234567890"), Nino("AB123456A"))

      val formData = FilterTrackRequestsForm.form(Set("")).fill(FilterTrackRequests(Some("Ddd Itsa Trader"), Some(AcceptedByClient)))
      val formDataWithButton = (formData.data + ("filter" -> "filter")).toSeq
      val result = postTrack(authorisedAsValidAgent(request.withFormUrlEncodedBody(formDataWithButton: _*), arn.value))

      status(result) shouldBe 303

      redirectLocation(result) shouldBe Some(
        routes.AgentsRequestTrackingController.showTrackRequests(1, Some("Ddd Itsa Trader"), Some(AcceptedByClient)).url
      )
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
      givenCitizenDetailsAreKnownFor(Nino("AB123456B"), "John", "Smith")
      givenCitizenDetailsAreKnownFor(Nino("GZ753451B"), "Cosmo", "Kramer")
      givenCitizenDetailsAreKnownFor(Nino("AB123456A"), "Rodney", "Jones")
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
      givenCitizenDetailsAreKnownFor(Nino("AB123456B"), "John", "Smith")
      givenCitizenDetailsAreKnownFor(Nino("GZ753451B"), "Cosmo", "Kramer")
      givenCitizenDetailsAreKnownFor(Nino("AB123456A"), "Rodney", "Jones")
      givenClientDetails(Vrn("101747696"))
      givenClientDetails(Vrn("101747641"))

      givenTrustClientReturns(validUtr, 200, Json.toJson(trustResponse).toString())
      givenGetCgtSubscriptionReturns(cgtRef, 200, Json.toJson(cgtSubscription()).toString())

      val formData = FilterTrackRequestsForm.form(Set("")).bind(Map("client" -> "Not a name we know", "status" -> "Bad status"))
      val formDataWithButton = (formData.data + ("filter" -> "filter")).toSeq
      val result = postTrack(authorisedAsValidAgent(request.withFormUrlEncodedBody(formDataWithButton: _*), arn.value))

      status(result) shouldBe 200
      val htmlString = Helpers.contentAsString(result)
      val html = Jsoup.parse(htmlString)

      html.title() shouldBe "Manage your recent authorisation requests - Ask a client to authorise you - GOV.UK"
      html.select(Css.ERROR_SUMMARY_TITLE).text() shouldBe "There is a problem"
      html.select(Css.errorSummaryLinkWithHref("#client")).text() shouldBe "You must select a name from the list"
      html.select(Css.errorSummaryLinkWithHref("#status")).text() shouldBe "You must select a status from the list"

      html.select(Css.errorSummaryForField("client")).text() shouldBe "You must select a name from the list"
      html.select(Css.errorSummaryForField("status")).text() shouldBe "You must select a status from the list"
    }
  }

  "POST /resend-link" should {

    val request = FakeRequest("POST", "/resend-link/")
    val postResendLink = controller.submitToResendLink

    "return 303 and redirect to show resend link page" in {
      givenAgentReference(arn, "uid", Personal)
      givenGetAgencyEmailAgentStub
      val expirationDate = LocalDate.now.plusDays(21)
      val formData =
        controller.trackInformationForm.fill(TrackResendForm("HMRC-MTD-IT", Some(Personal), expirationDate.toString))
      val result =
        postResendLink(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsRequestTrackingController.showResendLink.url)
    }

    "return 400 BadRequest when form data contains errors in service" in {
      val expirationDate: String = LocalDate.now.plusDays(5).toString
      val formData = controller.trackInformationForm.fill(TrackResendForm("foo", Some(Personal), expirationDate))
      val result =
        postResendLink(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }

    "return 400 BadRequest when form data contains errors in clientType" in {
      val dataForm = controller.trackInformationForm.bind(Map("service" -> "HMRC-MTD-IT", "clientType" -> "foo", "expiryDate" -> "2019-01-01"))
      val result =
        postResendLink(authorisedAsValidAgent(request.withFormUrlEncodedBody(dataForm.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }

    "return 400 BadRequest when form data contains errors in expiryDate" in {
      val formData = controller.trackInformationForm.fill(TrackResendForm("HMRC-MTD-IT", Some(Personal), "foo"))
      val result =
        postResendLink(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }
  }

  "GET /resend-link" should {

    val request = FakeRequest("GET", "/resend-link/")
    val getResendLink = controller.showResendLink

    "return 200 and expected content for default (non altItsa) scenario" in {
      val result =
        getResendLink(
          authorisedAsValidAgent(
            request.withSession(
              "agentLink"   -> "/agent/",
              "clientType"  -> "personal",
              "expiryDate"  -> "2017-05-05",
              "service"     -> "HMRC-MTD-IT",
              "agencyEmail" -> "abc@xyz.com",
              "isAltItsa"   -> "false"
            ),
            arn.value
          )
        )

      val htmlString = Helpers.contentAsString(result)
      val html = Jsoup.parse(htmlString)
      html.title() shouldBe "Resend this link to your client - Ask a client to authorise you - GOV.UK"
      html.select(Css.H1).text() shouldBe "Resend this link to your client"
      html.select("p#altItsa-list-hint").size() shouldBe 0
      val listItems = html.select("ol li")
      listItems.size() shouldBe 3
    }

    "return expected content for isAltItsa" in {

      val result = getResendLink(
        authorisedAsValidAgent(
          request.withSession(
            "agentLink"   -> "/agent/",
            "clientType"  -> "personal",
            "expiryDate"  -> "2017-05-05",
            "service"     -> "HMRC-MTD-IT",
            "agencyEmail" -> "abc@xyz.com",
            "isAltItsa"   -> "true"
          ),
          arn.value
        )
      )

      val htmlString = Helpers.contentAsString(result)
      val html = Jsoup.parse(htmlString)

      html.title() shouldBe "What you need to do next - Ask a client to authorise you - GOV.UK"
      html.select(Css.H1).text() shouldBe "What you need to do next"
      html.select("p#altItsa-list-hint").text() shouldBe "You must follow all four steps."

      val listItems = html.select("ol li")
      listItems.size() shouldBe 4
      listItems.get(0).text() matches "Copy this authorisation request link and send it to your client: http://localhost:\\d+/agent/"
      listItems.get(0).select("strong").text() matches "http://localhost:\\d+/agent/"
      listItems.get(1).text() shouldBe "Tell them to select this link. They will then be asked to sign in using their Government Gateway user ID."
      listItems
        .get(2)
        .select("p")
        .text() shouldBe "Ask your client to respond by 5 May 2017 or your authorisation request link will expire. We will email you at abc@xyz.com to update you on the status of this request."
      listItems
        .get(2)
        .select(".govuk-warning-text strong")
        .text() shouldBe "To give you authorisation, clients will need to use the same Government Gateway user ID they used to sign up for this service."
      listItems
        .get(3)
        .select("a")
        .attr("href")
        .shouldBe("https://www.gov.uk/guidance/sign-up-your-client-for-making-tax-digital-for-income-tax")
      listItems.get(3).text() shouldBe "Sign up your client for Making Tax Digital for Income Tax (opens in new tab)."

      html.select("h2#further-help-heading").get(0).text() shouldBe "Further help"
      html.select("p#further-help-text").text() shouldBe "Get help to complete another task, including:"

    }

    "return 200 when service is not present in session" in {
      val service = ""

      val result = getResendLink(
        authorisedAsValidAgent(
          request.withSession(
            "agentLink"   -> "/agent/",
            "clientType"  -> "personal",
            "expiryDate"  -> "2017-05-05",
            "service"     -> service,
            "agencyEmail" -> "abc@xyz.com"
          ),
          arn.value
        )
      )

      status(result) shouldBe OK
      val htmlString = Helpers.contentAsString(result)
      val html = Jsoup.parse(htmlString)
      html.title() shouldBe "Resend this link to your client - Ask a client to authorise you - GOV.UK"
      html.select(Css.H1).text() shouldBe "Resend this link to your client"

      val olLis = html.select("ol.govuk-list li")
      olLis.size() shouldBe 3
      olLis.get(0).text() matches "Copy this authorisation request link and send it to your client: http://localhost:\\d+/agent/"
      olLis.get(0).select("strong").text() matches "http://localhost:\\d+/agent/"
      olLis.get(1).text() shouldBe "Tell them to select this link. They will then be asked to sign in using their Government Gateway user ID."
      olLis
        .get(2)
        .select("p")
        .text() shouldBe "Ask your client to respond by 5 May 2017 or your authorisation request link will expire. We will email you at abc@xyz.com to update you on the status of this request."
      olLis
        .get(2)
        .select(".govuk-warning-text strong")
        .text() shouldBe "To give you authorisation, clients will need to use the same Government Gateway user ID they used to sign up for this service."
      html.select("h2#further-help-heading").get(0).text() shouldBe "Further help"
      html.select("p#further-help-text").text() shouldBe "Get help to complete another task, including:"

    }

    "redirect to /track when agentLink is not present in session" in {

      val result = getResendLink(
        authorisedAsValidAgent(
          request.withSession("clientType" -> "personal", "expiryDate" -> "2017-05-05", "service" -> "HMRC-MTD-IT", "agencyEmail" -> "abc@xyz.com"),
          arn.value
        )
      )
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsRequestTrackingController.showTrackRequests(1, None, None).url
    }

    "redirect to /track when expiryDate is not present in session" in {

      val result = getResendLink(
        authorisedAsValidAgent(
          request.withSession("clientType" -> "personal", "agentLink" -> "/agent/", "service" -> "HMRC-MTD-IT", "agencyEmail" -> "abc@xyz.com"),
          arn.value
        )
      )
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsRequestTrackingController.showTrackRequests(1, None, None).url
    }

    "return 200 when altItsa is not present in session" in {

      val result = getResendLink(
        authorisedAsValidAgent(
          request.withSession(
            "clientType"  -> "personal",
            "agentLink"   -> "/agent/",
            "service"     -> "HMRC-MTD-IT",
            "expiryDate"  -> "2017-05-05",
            "agencyEmail" -> "abc@xyz.com"
          ),
          arn.value
        )
      )
      status(result) shouldBe OK
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
        authorisedAsValidAgent(request.withSession("invitationId" -> invitationIdITSA.value, "service" -> "HMRC-MTD-IT"), arn.value)
      )

      status(result) shouldBe 200

      val htmlString = Helpers.contentAsString(result)
      val html = Jsoup.parse(htmlString)

      html.title() shouldBe "Are you sure you want to cancel this authorisation request? - Ask a client to authorise you - GOV.UK"
      html.select(Css.H1).text() shouldBe "Are you sure you want to cancel this authorisation request?"
      html
        .select("div#confirmCancel-hint")
        .text()
        .shouldBe("If you cancel this request, you will not be able to manage their Making Tax Digital for Income Tax.")
      html.select("label[for=confirmCancel]").text() shouldBe "Yes"
      html.select("label[for=confirmCancel-2]").text() shouldBe "No"
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
          arn.value
        )
      )

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track/request-cancelled")
    }

    "redirect to manage page when there is no invitationId in the session" in {
      givenCancelInvitationReturns(arn, invitationIdITSA, 204)
      val result = postConfirmCancel(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancel" -> "true")
            .withSession("clientName" -> "Joe Volcano"),
          arn.value
        )
      )

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track")
    }

    "redirect to manage page when no service in the session" in {
      givenCancelInvitationReturns(arn, invitationIdITSA, 204)
      val result = postConfirmCancel(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancel" -> "true")
            .withSession("clientName" -> "Joe Volcano", "invitationId" -> invitationIdITSA.value),
          arn.value
        )
      )

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
          arn.value
        )
      )

      status(result) shouldBe NOT_FOUND
    }

    "Forbidden when yes is selected on confirm cancel page, but cancellation fails because inivtation status is invalid" in {
      givenCancelInvitationReturns(arn, invitationIdITSA, 403)
      val result = postConfirmCancel(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancel" -> "true")
            .withSession("invitationId" -> invitationIdITSA.value, "service" -> "HMRC-MTD-IT", "clientName" -> "Joe Volcano"),
          arn.value
        )
      )

      status(result) shouldBe FORBIDDEN
    }

    "when no is selected on confirm cancel page, go back to manage authorisations page" in {
      val result = postConfirmCancel(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancel" -> "false")
            .withSession("invitationId" -> invitationIdITSA.value, "clientName" -> "Joe Volcano"),
          arn.value
        )
      )

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track")
    }

    "when neither option is selected on confirm cancel page show an error" in {
      val result = postConfirmCancel(
        authorisedAsValidAgent(
          request
            .withSession("invitationId" -> invitationIdITSA.value, "service" -> "HMRC-MTD-IT", "clientName" -> "Joe Volcano"),
          arn.value
        )
      )

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
            .withSession("invitationId" -> invitationIdITSA.value, "clientName" -> "Joe Volcano", "service" -> "HMRC-MTD-IT"),
          arn.value
        )
      )

      status(result) shouldBe 200

      val htmlString = Helpers.contentAsString(result)
      val html = Jsoup.parse(htmlString)

      html.title() shouldBe "Authorisation request cancelled - Ask a client to authorise you - GOV.UK"
      html.select(Css.H1).text() shouldBe "Authorisation request cancelled"
      html.select(".govuk-panel__title").text() shouldBe "Authorisation request cancelled"
      html
        .select("p#request-cancelled-text")
        .text() shouldBe "You have cancelled your authorisation request to manage their Making Tax Digital for Income Tax."
      html.select("p#request-cancelled-for-client").text() shouldBe "Joe Volcano can no longer respond to this request."
      html
        .select("p#cancelled-by-mistake-link-text")
        .text() shouldBe "If you cancelled your authorisation request by mistake, you will need to start a new authorisation request."
      html.select("p#cancelled-by-mistake-link-text a").attr("href") shouldBe "/invitations/agents/client-type"

      val trackLink = html.select("a[href='/invitations/track']")
      trackLink.text() shouldBe "Manage your recent authorisation requests"
      trackLink.hasClass("govuk-link")

    }

    "render a request cancelled page without the client's name during an IRV journey" in {
      val result = showRequestCancelled(
        authorisedAsValidAgent(
          request
            .withSession("invitationId" -> invitationIdPIR.value, "clientName" -> "Voe Jolcano", "service" -> "PERSONAL-INCOME-RECORD"),
          arn.value
        )
      )

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Authorisation request cancelled",
        "You have cancelled your authorisation request to view their Income record.",
        "Your client can no longer respond to this request."
      )
    }
  }

  "POST /confirm-cancel-authorisation" should {
    val request = FakeRequest("POST", "/confirm-cancel-authorisation")
    val postToConfirmCancelAuth = controller.submitToCancelAuthorisationConfirm

    "return 303 redirect to confirm cancel authorisation page when form is correct" in {
      val formData =
        controller.cancelAuthorisationForm.fill(
          CancelAuthorisationForm(serviceITSA, validNino.value, "personal", "Sylvia Plath", "ALFE93Y9KAELF", "Partialauth")
        )
      val result = postToConfirmCancelAuth(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track/cancel-authorisation")
    }

    "return 400 BadRequest when form data contains errors in service" in {
      val formData =
        controller.cancelAuthorisationForm.fill(
          CancelAuthorisationForm("foo", validNino.value, "personal", "Sylvia Plath", "ALFE93Y9KAELF", "Partialauth")
        )
      val result = postToConfirmCancelAuth(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }

    "return 400 BadRequest when form data contains errors in clientId" in {
      val formData =
        controller.cancelAuthorisationForm.fill(
          CancelAuthorisationForm(serviceITSA, "foo", "personal", "Sylvia Plath", "ALFE93Y9KAELF", "Partialauth")
        )
      val result = postToConfirmCancelAuth(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }

    "return 400 BadRequest when form data contains errors in status" in {
      val formData =
        controller.cancelAuthorisationForm.fill(
          CancelAuthorisationForm(serviceITSA, validNino.value, "personal", "Sylvia Plath", "ALFE93Y9KAELF", "foo")
        )
      val result = postToConfirmCancelAuth(authorisedAsValidAgent(request.withFormUrlEncodedBody(formData.data.toSeq: _*), arn.value))

      status(result) shouldBe 400
    }
  }

  "GET /track/cancel-authorisation" should {
    val request = FakeRequest("GET", "/track/cancel-authorisation")
    val showConfirmCancelAuth = controller.showCancelAuthorisationConfirm

    "render a confirm cancel authorisation page" in {
      val result = showConfirmCancelAuth(
        authorisedAsValidAgent(request.withSession("service" -> serviceITSA), arn.value)
      )

      status(result) shouldBe 200
      val htmlString = Helpers.contentAsString(result)
      val html = Jsoup.parse(htmlString)

      html.title() shouldBe "Are you sure you want to cancel this client’s authorisation? - Ask a client to authorise you - GOV.UK"
      html.select(Css.H1).text() shouldBe "Are you sure you want to cancel this client’s authorisation?"

      html
        .select("div.govuk-inset-text")
        .text() shouldBe "Cancelling your authorisation to manage a client’s Making Tax Digital for Income Tax does not automatically cancel any authorisation for Self Assessment."
      html
        .select("#confirmCancelAuthorisation-hint")
        .text() shouldBe "You will no longer be able to manage their Making Tax Digital for Income Tax. You will not be able to undo this action."
      html.select("label[for=confirmCancelAuthorisation]").text() shouldBe "Yes"
      html.select("label[for=confirmCancelAuthorisation-2]").text() shouldBe "No"
      html.select("button.govuk-button#continue").text() shouldBe "Continue"
    }

    "redirect to /track when no service is in the session" in {
      val result = showConfirmCancelAuth(
        authorisedAsValidAgent(request, arn.value)
      )
      status(result) shouldBe 303

      redirectLocation(result) shouldBe Some("/invitations/track")
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
            .withSession(
              "service"      -> serviceITSA,
              "clientId"     -> validNino.value,
              "clientName"   -> "Joe Volcano",
              "invitationId" -> invitationIdITSA.value,
              "status"       -> "Accepted"
            ),
          arn.value
        )
      )

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track/cancelled")
    }

    "when yes is selected on confirm cancel authorisation page, cancel the authorisation and redirect to authorisation cancelled page for alt-ITSA" in {
      givenSetRelationshipEndedReturns(arn, validNino.value, Service.MtdIt, 204)
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
            .withSession(
              "service"      -> serviceITSA,
              "clientId"     -> validNino.value,
              "clientName"   -> "Joe Volcano",
              "invitationId" -> invitationIdITSA.value,
              "status"       -> "Partialauth"
            ),
          arn.value
        )
      )

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track/cancelled")
    }

    "when yes is selected on confirm cancel authorisation page, cancel the authorisation and redirect to authorisation cancelled page for IRV" in {
      givenTerminateAfiRelationshipSucceeds(arn, "PERSONAL-INCOME-RECORD", validNino.value)
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
            .withSession(
              "service"      -> servicePIR,
              "clientId"     -> validNino.value,
              "clientName"   -> "Joe Volcano",
              "invitationId" -> invitationIdPIR.value
            ),
          arn.value
        )
      )

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track/cancelled")
    }

    "when yes is selected on confirm cancel authorisation page, cancel the authorisation and redirect to authorisation cancelled page for VAT" in {
      givenCancelledAuthorisationVat(arn, validVrn, 204)
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
            .withSession(
              "service"      -> serviceVAT,
              "clientId"     -> validVrn.value,
              "clientName"   -> "Joe Volcano",
              "invitationId" -> invitationIdVAT.value
            ),
          arn.value
        )
      )

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track/cancelled")
    }

    "when yes is selected on confirm cancel authorisation page, but service in session is not supported throw error" in {
      givenCancelledAuthorisationVat(arn, validVrn, 204)
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
            .withSession("service" -> "foo", "clientId" -> validVrn.value, "clientName" -> "Joe Volcano", "invitationId" -> invitationIdVAT.value),
          arn.value
        )
      )

      an[Exception] shouldBe thrownBy {
        await(result)
      }
    }

    "mark the relationship as ended in ACA and go to showAuthorisationCancelledPage even if relationship service returns 404" in {
      givenCancelledAuthorisationItsa(arn, validNino, 404)
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
            .withSession(
              "service"      -> serviceITSA,
              "clientId"     -> validNino.value,
              "clientName"   -> "Joe Volcano",
              "invitationId" -> invitationIdITSA.value,
              "status"       -> "Accepted"
            ),
          arn.value
        )
      )

      status(result) shouldBe 303
    }

    "go to problem page when yes is selected on confirm cancel authorisation page, but relationship deletion fails for some other reason" in {
      givenCancelledAuthorisationItsa(arn, validNino, 403)
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
            .withSession(
              "service"      -> serviceITSA,
              "clientId"     -> validNino.value,
              "clientName"   -> "Joe Volcano",
              "invitationId" -> invitationIdITSA.value
            ),
          arn.value
        )
      )

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Sorry, there is a problem with the service",
        "The authorisation was not cancelled. Please try again.",
        "Try again"
      )
    }

    "go to problem page when yes is selected on confirm cancel authorisation page for alt-itsa, but set relationship ended fails" in {
      givenSetRelationshipEndedReturns(arn, validNino.value, Service.MtdIt, 500)
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "true")
            .withSession(
              "service"      -> serviceITSA,
              "clientId"     -> validNino.value,
              "clientName"   -> "Joe Volcano",
              "invitationId" -> invitationIdITSA.value,
              "status"       -> "Partialauth"
            ),
          arn.value
        )
      )

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Sorry, there is a problem with the service",
        "The authorisation was not cancelled. Please try again.",
        "Try again"
      )
    }

    "when no is selected on confirm cancel authorisation page, go back to manage authorisations page" in {
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withFormUrlEncodedBody("confirmCancelAuthorisation" -> "false")
            .withSession("service" -> serviceITSA, "clientId" -> validNino.value, "clientName" -> "Joe Volcano"),
          arn.value
        )
      )

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/track")
    }

    "when neither option is selected on confirm cancel page show an error" in {
      val result = postConfirmCancelAuth(
        authorisedAsValidAgent(
          request
            .withSession("service" -> serviceITSA, "clientId" -> validNino.value, "clientName" -> "Joe Volcano"),
          arn.value
        )
      )

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
              "service"      -> "HMRC-MTD-IT"
            ),
          arn.value
        )
      )

      status(result) shouldBe 200
      val htmlString = Helpers.contentAsString(result)
      val html = Jsoup.parse(htmlString)
      checkHtmlResultWithBodyText(
        result,
        "Authorisation cancelled",
        "What this means",
        "You are no longer authorised by Buttercup Powerpuff to manage their Making Tax Digital for Income Tax.",
        "Check or cancel an authorisation for Self Assessment",
        "You could still have an authorisation in place for this client’s Self Assessment. To check or cancel an authorisation, you will need to:",
        "sign out of this agent services account",
        "follow the instructions for deleting the client from your client list"
      )

      html
        .select("a[href='https://www.gov.uk/guidance/self-assessment-for-agents-online-service']")
        .text() shouldBe "sign in to your HMRC online services for agents account"
      html.select("a[href='/invitations/track']").text() shouldBe "Return to manage your recent authorisation requests"
    }

    "render a authorisation cancelled page for IRV" in {
      val result = showAuthCancelled(
        authorisedAsValidAgent(
          request
            .withSession(
              "invitationId" -> invitationIdPIR.value,
              "clientId"     -> validNino.value,
              "clientName"   -> "Bubbles Powerpuff",
              "service"      -> "PERSONAL-INCOME-RECORD"
            ),
          arn.value
        )
      )

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Authorisation cancelled",
        "What this means",
        "You are no longer authorised by Bubbles Powerpuff to view their Income record.",
        "Return to manage your recent authorisation requests"
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
              "service"      -> "HMRC-MTD-VAT"
            ),
          arn.value
        )
      )

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Authorisation cancelled",
        "What this means",
        "You are no longer authorised by Blossom Powerpuff to manage their " +
          "VAT.",
        "Return to manage your recent authorisation requests"
      )
    }
  }
}
