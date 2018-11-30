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

import play.api.mvc.{Action, AnyContent, Cookie}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.audit.AgentInvitationEvent.AgentClientInvitationResponse
import uk.gov.hmrc.agentinvitationsfrontend.connectors.AgencyNameNotFound
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ClientsInvitationController.confirmAuthorisationForm
import uk.gov.hmrc.agentinvitationsfrontend.support.{BaseISpec, TestDataCommonSupport}
import uk.gov.hmrc.auth.core.AuthorisationException

class ClientsInvitationsIRVControllerISpec extends BaseISpec {

  lazy val controller: ClientsInvitationController = app.injector.instanceOf[ClientsInvitationController]

  "GET /:invitationId (landing page)" should {

    "show the landing page even if the user is not authenticated" in {
      val result = controller.start(invitationIdPIR)(FakeRequest())
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(
        result,
        hasMessage(
          "generic.title",
          htmlEscapedMessage("landing-page.afi.header"),
          htmlEscapedMessage("title.suffix.client")))
      await(bodyOf(result)) should not include htmlEscapedMessage("common.sign-out")
    }

    "show the landing page with AFI content variant if the invitation ID prefix is 'B'" in {
      val result = controller.start(invitationIdPIR)(FakeRequest())
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(
        result,
        hasMessage(
          "generic.title",
          htmlEscapedMessage("landing-page.afi.header"),
          htmlEscapedMessage("title.suffix.client"),
          htmlEscapedMessage("landing-page.reminder"),
          htmlEscapedMessage("landing-page.radio1"))
        )
    }

    "show a signout url on the landing page if the user is authenticated" in {
      val result = controller.start(invitationIdPIR)(FakeRequest().withCookies(Cookie("mdtp", "authToken=Bearer+")))
      status(result) shouldBe OK
      checkHasClientSignOutUrl(result)
    }
  }

  "POST /:invitationId (making a choice on the landing page)" should {
    val submitStart: Action[AnyContent] = controller.submitStart(invitationIdPIR)

    "redirect to /accept-tax-agent-invitation/consent/:invitationId when yes is selected" in {
      val serviceForm = confirmAuthorisationForm.fill(ConfirmAuthForm(Some("yes")))
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Pending")
      val result =
        submitStart(FakeRequest().withSession("agencyName" -> "My Agency")
          .withFormUrlEncodedBody(serviceForm.data.toSeq: _*))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController
        .getConfirmTerms(invitationIdPIR)
        .url
    }

    "redirect to confirm-decline page when no is selected" in {
      val serviceForm = confirmAuthorisationForm.fill(ConfirmAuthForm(Some("no")))
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Pending")
      val result =
        submitStart(FakeRequest().withSession("agencyName" -> "My Agency")
          .withFormUrlEncodedBody(serviceForm.data.toSeq: _*))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController
        .getConfirmDecline(invitationIdPIR)
        .url

    }

    "redirect to decide-later page when I want to decide later is selected" in {
      val serviceForm = confirmAuthorisationForm.fill(ConfirmAuthForm(Some("maybe")))
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Pending")
      val result =
        submitStart(FakeRequest().withSession("agencyName" -> "My Agency")
          .withFormUrlEncodedBody(serviceForm.data.toSeq: _*))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController
        .getDecideLater(invitationIdPIR)
        .url

    }

    "refresh the page with errors when no radio button is selected" in {
      val serviceForm = confirmAuthorisationForm.fill(ConfirmAuthForm(Some("")))
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Pending")
      val result =
        submitStart(FakeRequest().withSession("agencyName" -> "My Agency")
          .withFormUrlEncodedBody(serviceForm.data.toSeq: _*))
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.summary.heading"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.confirmAuthorisation.invalid"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("landing-page.afi.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("landing-page.radio1"))
    }

    "throw an error when the radio button selection is invalid" in {
      val serviceForm = confirmAuthorisationForm.fill(ConfirmAuthForm(Some("foo")))
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Pending")

      an[Exception] should be thrownBy {
        await(submitStart(FakeRequest().withSession("agencyName" -> "My Agency")
          .withFormUrlEncodedBody(serviceForm.data.toSeq: _*)))
      }
    }
  }

  "GET /accept-tax-agent-invitation/consent/:invitationId (confirm terms page)" should {

    val getConfirmTermsAFI: Action[AnyContent] = controller.getConfirmTerms(invitationIdPIR)

    "show the confirm terms page for AFI" in {
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Pending")
      givenGetAgencyNameClientStub(arn)
      val req = authorisedAsValidClientAFI(FakeRequest().withSession("agencyName" -> "My Agency"), nino)
      val result = getConfirmTermsAFI(req)

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("confirm-terms.heading", htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.heading"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.subheading1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.afi.bullet1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.afi.bullet2"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.afi.bullet3"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.afi.bullet4"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.subheading1.p2"))
      checkHasClientSignOutUrl(result)
    }

    "show the invitation expired page when invitation has expired" in {
      getExpiredInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR)
      givenGetAgencyNameClientStub(arn)
      val reqAFI = authorisedAsValidClientAFI(FakeRequest().withSession("agencyName"   -> "My Agency"), nino)
      val resultAFI = getConfirmTermsAFI(reqAFI)

      status(resultAFI) shouldBe SEE_OTHER

      redirectLocation(resultAFI) shouldBe Some(routes.ClientsInvitationController.invitationExpired().url)
    }

    "show the request-cancelled page when the invitation has already been cancelled" in {
      getCancelledInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR)
      givenGetAgencyNameClientStub(arn)
      val reqAFI = authorisedAsValidClientAFI(FakeRequest().withSession("agencyName" -> "My Agency"), nino)
      val resultAFI = getConfirmTermsAFI(reqAFI)

      status(resultAFI) shouldBe SEE_OTHER

      redirectLocation(resultAFI) shouldBe Some(routes.ClientsInvitationController.requestCancelled().url)
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Pending")
      givenAgencyNameNotFoundClientStub(arn)

      val result = getConfirmTermsAFI(authorisedAsValidClientAFI(FakeRequest(), nino))

      an[AgencyNameNotFound] should be thrownBy await(result)
    }

    "redirect to /client/not-authorised if an authenticated user does not have the HMRC-NI Enrolment" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = controller.getConfirmTerms(invitationIdPIR)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Individual",
          Enrolment("OtherEnrolment", "OtherValue", nino)))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notAuthorised().url
    }

    "redirect to /client/not-found if an authenticated user does not have the Confidence Level 200" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = controller.getConfirmTerms(invitationIdPIR)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Individual",
          Enrolment("HMRC-NI", "NINO", nino),
          "50"))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }
  }

  "POST /accept-tax-agent-invitation/consent/:invitationId (clicking confirm on the confirm terms page)" should {
    val submitConfirmTermsAFI: Action[AnyContent] = controller.submitConfirmTerms(invitationIdPIR)

    "redirect to complete page when YES was selected" in {
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Pending")
      acceptInvitationStub(nino, invitationIdPIR, identifierPIR)
      givenGetAgencyNameClientStub(arn)

      val reqAFI = authorisedAsValidClientAFI(FakeRequest(), nino)
        .withFormUrlEncodedBody("confirmTerms" -> "true")
        .withSession("agencyName" -> "My Agency")
      val resultAFI = submitConfirmTermsAFI(reqAFI)

      verifyAgentInvitationResponseEvent(invitationIdPIR, arn.value, "Accepted", "ni", nino, servicePIR, "My Agency")

      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultAFI).get shouldBe routes.ClientsInvitationController.getCompletePage(invitationIdPIR).url
    }

    "call agent-client-authorisation to accept the invitation and create the relationship in ETMP when YES was selected" in {
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Pending")
      acceptInvitationStub(nino, invitationIdPIR, identifierPIR)
      givenGetAgencyNameClientStub(arn)

      val reqAFI = authorisedAsValidClientAFI(FakeRequest(), nino)
        .withFormUrlEncodedBody("confirmTerms" -> "true")
        .withSession("agencyName" -> "My Agency")
      await(submitConfirmTermsAFI(reqAFI).withSession("agencyName" -> "My Agency"))

      verifyAcceptInvitationAttempt(nino, invitationIdPIR, identifierPIR)
    }

    "redirect to confirm-decline when NO was selected" in {
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Pending")
      givenGetAgencyNameClientStub(arn)

      val reqAFI = authorisedAsValidClientAFI(FakeRequest(), nino)
        .withFormUrlEncodedBody("confirmTerms" -> "false")
        .withSession("agencyName" -> "My Agency")
      val resultAFI = submitConfirmTermsAFI(reqAFI)

      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultAFI).get shouldBe routes.ClientsInvitationController.getConfirmDecline(invitationIdPIR).url
    }

    "reshow the page when the radio buttons were not selected with an error message" in {
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Pending")
      acceptInvitationStub(nino, invitationIdPIR, identifierPIR)
      givenGetAgencyNameClientStub(arn)

      val reqAFI = authorisedAsValidClientAFI(FakeRequest().withSession("agencyName" -> "My Agency"), nino)
        .withFormUrlEncodedBody("confirmTerms"                                  -> "")
      val resultAFI = submitConfirmTermsAFI(reqAFI).withSession("agencyName"    -> "My Agency")

      status(resultAFI) shouldBe OK
      checkHtmlResultWithBodyText(resultAFI, htmlEscapedMessage("confirm-terms.afi.bullet1"))
      checkHtmlResultWithBodyText(resultAFI, htmlEscapedMessage("confirm-terms.afi.bullet2"))
      checkHtmlResultWithBodyText(resultAFI, htmlEscapedMessage("confirm-terms.afi.bullet3"))
      checkHtmlResultWithBodyText(resultAFI, htmlEscapedMessage("confirm-terms.afi.bullet4"))
      checkHasClientSignOutUrl(resultAFI)
    }

    "redirect to /incorrect/ if authenticated user has HMRC-NI enrolment but with a different clientId" in {
      val reqAFI = authorisedAsValidClientAFI(FakeRequest(), nino)
        .withFormUrlEncodedBody("confirmTerms" -> "true")
        .withSession("agencyName" -> "My Agency")
      getInvitationNoPermissionStub(nino, invitationIdPIR, identifierPIR)
      acceptInvitationNoPermissionStub(nino, invitationIdPIR, identifierPIR)
      givenGetAgencyNameClientStub(arn)
      val resultAFI = submitConfirmTermsAFI(reqAFI)

      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultAFI).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
    }

    "redirect to invitationAlreadyResponded when an invitation is returned that is already actioned" in {
      val reqAFI = authorisedAsValidClientAFI(FakeRequest(), nino)
        .withFormUrlEncodedBody("confirmTerms" -> "true")
        .withSession("agencyName" -> "My Agency")
      alreadyActionedGetInvitationStub(nino, invitationIdPIR, identifierPIR)
      alreadyActionedAcceptInvitationStub(nino, invitationIdPIR, identifierPIR)
      givenGetAgencyNameClientStub(arn)
      val resultAFI = submitConfirmTermsAFI(reqAFI)

      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultAFI) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
    }

    "redirect to notFoundInvitation where no such invitation" in {
      val resultAFI = submitConfirmTermsAFI(
        authorisedAsValidClientAFI(
          FakeRequest()
            .withFormUrlEncodedBody("confirmTerms" -> "true"),
          nino).withSession("agencyName" -> "My Agency"))
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultAFI).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to /client/not-authorised if an authenticated user does not have the HMRC-NI Enrolment" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = controller.submitConfirmTerms(invitationIdPIR)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Other Affinity Group",
          Enrolment("OtherEnrolment", "OtherValue", nino)))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notAuthorised().url
    }

    "redirect to /client/not-found if an authenticated user does not have the Confidence Level 200" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = controller.submitConfirmTerms(invitationIdPIR)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Individual",
          Enrolment("HMRC-NI", "NINO", nino),
          "50"))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Pending")
      givenAgencyNameNotFoundClientStub(arn)

      val result = submitConfirmTermsAFI(authorisedAsValidClientAFI(FakeRequest(), nino))

      an[AgencyNameNotFound] should be thrownBy await(result)
    }

  }

  "GET /accept-tax-agent-invitation/accepted/:invitationId (complete page)" should {

    val getCompletePageAFI: Action[AnyContent] = controller.getCompletePage(invitationIdPIR)

    "show the complete page for AFI" in {
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Accepted")
      givenGetAgencyNameClientStub(arn)

      val result =
        getCompletePageAFI(authorisedAsValidClientAFI(FakeRequest().withSession("agencyName" -> "My Agency"), nino))

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("client-complete.header"),
          htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("My Agency"))
      checkHtmlResultWithBodyText(result, hasMessage("client-complete.afi.p1", "My Agency"))
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("client-complete.sub-header"),
        htmlEscapedMessage("client-complete.whatHappensNext.l1"),
        htmlEscapedMessage("client-complete.whatHappensNext.l2"),
        htmlEscapedMessage("client-complete.button.mta"),
        "someAgentClientManagementFrontendExternalUrl"
      )
      checkExitSurveyAfterInviteResponseSignOutUrl(result)
    }

    "return exception when agency name retrieval fails for AFI" in {
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Accepted")
      val result = getCompletePageAFI(authorisedAsValidClientAFI(FakeRequest(), nino))
      an[AgencyNameNotFound] should be thrownBy await(result)
    }

    "redirect to /client/not-authorised if an authenticated user does not have the HMRC-NI Enrolment" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = controller.getCompletePage(invitationIdPIR)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Other Affinity Group",
          Enrolment("OtherEnrolment", "OtherValue", nino)))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notAuthorised().url
    }

    "redirect to /client/not-found if an authenticated user does not have the Confidence Level 200" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = controller.getCompletePage(invitationIdPIR)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Individual",
          Enrolment("HMRC-NI", "NINO", nino),
          "50"))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }
  }

  "GET /accept-tax-agent-invitation/confirm-decline/:invitationId (confirm decline page)" should {

    val getConfirmInvitationAFI: Action[AnyContent] = controller.getConfirmDecline(invitationIdPIR)

    "show the confirm invitation page for AFI" in {
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, "NI", "Pending")
      givenGetAgencyNameClientStub(arn)
      val result = getConfirmInvitationAFI(
        authorisedAsValidClientAFI(FakeRequest().withSession("agencyName" -> "My Agency"), nino))

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("confirm-decline.heading", htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-decline.heading", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-decline.afi.sub-header", "My Agency"))
      checkHasClientSignOutUrl(result)
    }

    "return 303 for not logged in user and redirected to Login Page for AFI" in {
      givenUnauthorisedWith("MissingBearerToken")
      an[AuthorisationException] shouldBe thrownBy {
        await(getConfirmInvitationAFI(FakeRequest().withSession("agencyName" -> "My Agency")))
      }
      verifyAuthoriseAttempt()
    }

    "redirect to /client/not-authorised if an authenticated user does not have the HMRC-NI Enrolment" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = controller.getConfirmDecline(invitationIdPIR)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Other Affinity Group",
          Enrolment("OtherEnrolment", "OtherValue", nino)))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notAuthorised().url
    }

    "redirect to /client/not-found if an authenticated user does not have the Confidence Level 200" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = controller.getConfirmDecline(invitationIdPIR)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Individual",
          Enrolment("HMRC-NI", "NINO", nino),
          "50"))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }

    "redirect to /not-found/ if authenticated user has HMRC-NI enrolment but the invitationId they supplied does not exist" in {
      notFoundGetInvitationStub(nino, invitationIdPIR, identifierPIR)
      val resultAFI = getConfirmInvitationAFI(
        authorisedAsValidClientAFI(FakeRequest().withSession("agencyName" -> "My Agency"), nino))
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultAFI).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to invitationAlreadyResponded when an invitation is returned that is already actioned" in {
      getAlreadyAcceptedInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR)
      val resultAFI = getConfirmInvitationAFI(
        authorisedAsValidClientAFI(FakeRequest().withSession("agencyName" -> "My Agency"), nino))

      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultAFI) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to /incorrect/ if authenticated user has HMRC-NI enrolment but with a different nino" in {
      incorrectGetInvitationStub(nino, invitationIdPIR, identifierPIR)

      val resultAFI = getConfirmInvitationAFI(
        authorisedAsValidClientAFI(FakeRequest().withSession("agencyName" -> "My Agency"), nino))

      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultAFI).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to notFoundInvitation when invitationId missing from session" in {
      val resultAFI = getConfirmInvitationAFI(authorisedAsValidClientAFI(FakeRequest(), nino))

      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultAFI) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }
  }

  "POST /accept-tax-agent-invitation/confirm-decline/:invitationId (confirm decline page)" should {
    val submitConfirmInvitationAFI: Action[AnyContent] = controller.submitConfirmDecline(invitationIdPIR)

    "reshow the page when neither yes nor no choices were selected with an error message" in {
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Pending")
      givenGetAgencyNameClientStub(arn)
      val resultAFI = submitConfirmInvitationAFI(
        authorisedAsValidClientAFI(FakeRequest().withSession("agencyName" -> "My Agency"), nino))

      status(resultAFI) shouldBe OK
      checkHtmlResultWithBodyText(
        resultAFI,
        htmlEscapedMessage("confirm-decline.heading", htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(resultAFI, htmlEscapedMessage("confirm-decline.heading", "My Agency"))
      checkHtmlResultWithBodyText(resultAFI, htmlEscapedMessage("error.confirmDecline.invalid"))
    }

    "redirect to declined page when YES was selected" in {
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Pending")
      givenGetAgencyNameClientStub(arn)

      val reqAFI = authorisedAsValidClientAFI(FakeRequest().withSession("agencyName" -> "My Agency"), nino)
        .withFormUrlEncodedBody("confirmDecline" -> "true")
      val resultAFI = submitConfirmInvitationAFI(reqAFI)

      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultAFI).get shouldBe routes.ClientsInvitationController.getInvitationDeclined(invitationIdPIR).url
    }

    "redirect to confirm terms when NO was selected" in {
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Pending")
      givenGetAgencyNameClientStub(arn)

      val reqAFI = authorisedAsValidClientAFI(FakeRequest().withSession("agencyName" -> "My Agency"), nino)
        .withFormUrlEncodedBody("confirmDecline" -> "false")
      val resultAFI = controller.submitConfirmDecline(invitationIdPIR).apply(reqAFI)

      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultAFI) shouldBe Some(
        routes.ClientsInvitationController.getConfirmTerms(invitationIdPIR).url)
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Pending")

      val resultAFI = submitConfirmInvitationAFI(authorisedAsValidClientAFI(FakeRequest().withSession(), nino))

      an[AgencyNameNotFound] should be thrownBy await(resultAFI)
    }

    "redirect to /client/not-authorised if an authenticated user does not have the HMRC-NI Enrolment" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = controller.submitConfirmDecline(invitationIdPIR)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Other Affinity Group",
          Enrolment("OtherEnrolment", "OtherValue", nino)))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notAuthorised().url
    }

    "redirect to /client/not-found if an authenticated user does not have the Confidence Level 200" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = controller.submitConfirmDecline(invitationIdPIR)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Individual",
          Enrolment("HMRC-NI", "NINO", nino),
          "50"))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }
  }

  "GET /reject-tax-agent-invitation/declined/:invitationId" should {
    val getInvitationDeclinedAFI = controller.getInvitationDeclined(invitationIdPIR)

    "show invitation_declined page for an authenticated client with a valid invitation for AFI" in {

      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Pending")
      rejectInvitationStub(nino, invitationIdPIR, identifierPIR)
      givenGetAgencyNameClientStub(arn)

      val result = getInvitationDeclinedAFI(
        authorisedAsValidClientAFI(FakeRequest().withSession("agencyName" -> "My Agency"), nino))

      status(result) shouldBe OK

      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("invitation-declined.header"),
          htmlEscapedMessage("title.suffix.client")),
        htmlEscapedMessage("invitation-declined-afi.p1", "My Agency"),
        htmlEscapedMessage("invitation-declined-afi.button"),
        s"""href="$taxAccountRelativeUrl""""
      )

      checkExitSurveyAfterInviteResponseSignOutUrl(result)
      verifyAgentInvitationResponseEvent(invitationIdPIR, arn.value, "Declined", "ni", nino, servicePIR, "My Agency")
    }

    "redirect to invitationAlreadyResponded when declined a invitation that is already actioned" in {
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Declined")
      alreadyActionedRejectInvitationStub(nino, invitationIdPIR, identifierPIR)
      givenGetAgencyNameClientStub(arn)

      val resultAFI = getInvitationDeclinedAFI(
        authorisedAsValidClientAFI(FakeRequest().withSession("agencyName" -> "My Agency"), nino))

      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultAFI) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
    }

    "redirect to notFoundInvitation when invitation does not exist" in {
      notFoundGetInvitationStub(nino, invitationIdPIR, identifierPIR)
      val resultAFI = getInvitationDeclinedAFI(
        authorisedAsValidClientAFI(FakeRequest().withSession("agencyName" -> "My Agency"), nino))

      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultAFI) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to /incorrect/ if authenticated user has HMRC-NI enrolment but with a different id" in {
      incorrectGetInvitationStub(nino, invitationIdPIR, identifierPIR)
      val resultAFI = getInvitationDeclinedAFI(
        authorisedAsValidClientAFI(FakeRequest().withSession("agencyName" -> "My Agency"), nino))

      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultAFI).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Pending")
      givenAgencyNameNotFoundClientStub(arn)

      val resultAFI = getInvitationDeclinedAFI(
        authorisedAsValidClientAFI(FakeRequest().withSession("agencyName" -> "My Agency"), nino))

      an[AgencyNameNotFound] should be thrownBy await(resultAFI)
    }

    "redirect to /client/not-authorised if an authenticated user does not have the HMRC-NI Enrolment" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = controller.getInvitationDeclined(invitationIdPIR)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Other Affinity Group",
          Enrolment("OtherEnrolment", "OtherValue", nino)))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notAuthorised().url
    }

    "redirect to /client/not-found if an authenticated user does not have the Confidence Level 200" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = controller.getInvitationDeclined(invitationIdPIR)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Individual",
          Enrolment("HMRC-NI", "NINO", nino),
          "50"))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }
  }

  "GET /decide-later/:invitationId" should {
    val getDecideLaterPIR = controller.getDecideLater(invitationIdPIR)

    "show the decide later page with PIR content even when user is not authenticated" in {
      getInvitationStub(arn, nino, invitationIdPIR, servicePIR, identifierPIR, "Pending")
      givenGetAgencyNameClientStub(arn)
      val result = getDecideLaterPIR(FakeRequest())
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("decide-later.header", htmlEscapedMessage("title.suffix.client")))
      await(bodyOf(result)) should not include htmlEscapedMessage("common.sign-out")
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("decide-later.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("decide-later.afi.p1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("decide-later.subheader1"))
    }

    "show a signout url on the landing page if the user is authenticated" in {
      val result = getDecideLaterPIR(FakeRequest().withCookies(Cookie("mdtp", "authToken=Bearer+")))
      status(result) shouldBe OK
      checkHasClientSignOutUrl(result)
    }
  }
}
