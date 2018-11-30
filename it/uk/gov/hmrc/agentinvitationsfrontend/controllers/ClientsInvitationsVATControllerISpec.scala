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

import play.api.mvc.{Action, AnyContent, Cookie, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.audit.AgentInvitationEvent.AgentClientInvitationResponse
import uk.gov.hmrc.agentinvitationsfrontend.connectors.AgencyNameNotFound
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ClientsInvitationController.confirmAuthorisationForm
import uk.gov.hmrc.agentinvitationsfrontend.support.{BaseISpec, TestDataCommonSupport}
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.auth.core.AuthorisationException

class ClientsInvitationsVATControllerISpec extends BaseISpec {

  lazy val controller: ClientsInvitationController = app.injector.instanceOf[ClientsInvitationController]

  "GET /:invitationId (landing page)" should {
    "show the landing page even if the user is not authenticated" in {
      val result = controller.start(invitationIdVAT)(FakeRequest())
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(
        result,
        hasMessage(
          "generic.title",
          htmlEscapedMessage("landing-page.vat.header"),
          htmlEscapedMessage("title.suffix.client")))
      await(bodyOf(result)) should not include htmlEscapedMessage("common.sign-out")
    }

    "show the landing page with VAT content variant if the invitation ID prefix is 'C'" in {
      val result = controller.start(invitationIdVAT)(FakeRequest())
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(
        result,
        hasMessage(
          "generic.title",
          htmlEscapedMessage("landing-page.vat.header"),
          htmlEscapedMessage("title.suffix.client"),
          htmlEscapedMessage("landing-page.reminder"),
          htmlEscapedMessage("landing-page.radio1")))
    }



    "show a signout url on the landing page if the user is authenticated" in {
      val result = controller.start(invitationIdVAT)(FakeRequest().withCookies(Cookie("mdtp", "authToken=Bearer+")))
      status(result) shouldBe OK
      checkHasClientSignOutUrl(result)
    }
  }

  "POST /:invitationId (making a choice on the landing page)" should {
    val submitStart: Action[AnyContent] = controller.submitStart(invitationIdVAT)

    "redirect to /accept-tax-agent-invitation/consent/:invitationId when yes is selected" in {
      val serviceForm = confirmAuthorisationForm.fill(ConfirmAuthForm(Some("yes")))
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      val result =
        submitStart(FakeRequest().withSession("agencyName" -> "My Agency")
          .withFormUrlEncodedBody(serviceForm.data.toSeq: _*))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController
        .getConfirmTerms(invitationIdVAT)
        .url
    }

    "redirect to confirm-decline page when no is selected" in {
      val serviceForm = confirmAuthorisationForm.fill(ConfirmAuthForm(Some("no")))
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      val result =
        submitStart(FakeRequest().withSession("agencyName" -> "My Agency")
          .withFormUrlEncodedBody(serviceForm.data.toSeq: _*))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController
        .getConfirmDecline(invitationIdVAT)
        .url
    }

    "redirect to decide-later page when I dont know is selected" in {
      val serviceForm = confirmAuthorisationForm.fill(ConfirmAuthForm(Some("maybe")))
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      val result =
        submitStart(FakeRequest().withSession("agencyName" -> "My Agency")
          .withFormUrlEncodedBody(serviceForm.data.toSeq: _*))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController
        .getDecideLater(invitationIdVAT)
        .url
    }

    "refresh the page with errors when no radio button is selected" in {
      val serviceForm = confirmAuthorisationForm.fill(ConfirmAuthForm(Some("")))
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      val result =
        submitStart(FakeRequest().withSession("agencyName" -> "My Agency")
          .withFormUrlEncodedBody(serviceForm.data.toSeq: _*))
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.summary.heading"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.confirmAuthorisation.invalid"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("landing-page.vat.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("landing-page.radio1"))
      htmlEscapedMessage("landing-page.reminder")
    }

    "throw an error when the radio button selection is invalid" in {
      val serviceForm = confirmAuthorisationForm.fill(ConfirmAuthForm(Some("foo")))
      getInvitationStub(arn, nino, invitationIdVAT, serviceVAT, identifierVAT, "Pending")

      an[Exception] should be thrownBy {
        await(submitStart(FakeRequest().withSession("agencyName" -> "My Agency")
          .withFormUrlEncodedBody(serviceForm.data.toSeq: _*)))
      }
    }
  }

  "GET /accept-tax-agent-invitation/consent/:invitationId (confirm terms page)" should {

    val getConfirmTermsVAT: Action[AnyContent] = controller.getConfirmTerms(invitationIdVAT)

    "show the confirm terms page for VAT" in {
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      givenGetAgencyNameClientStub(arn)
      val req = authorisedAsValidClientVAT(FakeRequest().withSession("agencyName" -> "My Agency"), validVrn.value)
      val result = getConfirmTermsVAT(req)

      status(result) shouldBe OK

      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("confirm-terms.heading", htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.heading"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.subheading1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.vat.bullet1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.vat.bullet2"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.vat.bullet3"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.vat.p1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.subheading1.p2"))
      checkHasClientSignOutUrl(result)
    }

    "show the invitation expired page when invitation has expired" in {
      getExpiredInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT)
      givenGetAgencyNameClientStub(arn)
      val reqVAT = authorisedAsValidClientVAT(FakeRequest().withSession("agencyName"   -> "My Agency"), validVrn.value)
      val resultVAT = getConfirmTermsVAT(reqVAT)

      status(resultVAT) shouldBe SEE_OTHER

      redirectLocation(resultVAT) shouldBe Some(routes.ClientsInvitationController.invitationExpired().url)
    }

    "show the request-cancelled page when the invitation has already been cancelled" in {
      getCancelledInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT)
      givenGetAgencyNameClientStub(arn)
      val reqVAT = authorisedAsValidClientVAT(FakeRequest().withSession("agencyName" -> "My Agency"), validVrn.value)
      val resultVAT = getConfirmTermsVAT(reqVAT)

      status(resultVAT) shouldBe SEE_OTHER

      redirectLocation(resultVAT) shouldBe Some(routes.ClientsInvitationController.requestCancelled().url)
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      givenAgencyNameNotFoundClientStub(arn)

      val result = getConfirmTermsVAT(authorisedAsValidClientVAT(FakeRequest(), validVrn.value))

      an[AgencyNameNotFound] should be thrownBy await(result)
    }

    "redirect to /client/not-signed-up if an authenticated user does not have the HMRC-MTD-VAT Enrolment" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = controller.getConfirmTerms(invitationIdITSA)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Other Affinity Group",
          Enrolment("OtherEnrolment", "OtherValue", validVrn.value)))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notSignedUp().url
    }

    "redirect to /client/not-found if an authenticated user does not have the Confidence Level 200" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = controller.getConfirmTerms(invitationIdVAT)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Organisation",
          Enrolment("HMRC-MTD-VAT", "VRN", validVrn.value),
          "50"))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }
  }

  "POST /accept-tax-agent-invitation/consent/:invitationId (clicking confirm on the confirm terms page)" should {
    val submitConfirmTermsVAT: Action[AnyContent] = controller.submitConfirmTerms(invitationIdVAT)

    "redirect to complete page when YES was selected" in {
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      acceptInvitationStub(validVrn.value, invitationIdVAT, identifierVAT)
      givenGetAgencyNameClientStub(arn)

      val reqVAT = authorisedAsValidClientVAT(FakeRequest(), validVrn.value)
        .withFormUrlEncodedBody("confirmTerms" -> "true")
        .withSession("agencyName" -> "My Agency")
      val resultVAT = submitConfirmTermsVAT(reqVAT)

      verifyAgentInvitationResponseEvent(
        invitationIdVAT,
        arn.value,
        "Accepted",
        "vrn",
        validVrn.value,
        serviceVAT,
        "My Agency")

      status(resultVAT) shouldBe SEE_OTHER
      redirectLocation(resultVAT).get shouldBe routes.ClientsInvitationController.getCompletePage(invitationIdVAT).url
    }

    "call agent-client-authorisation to accept the invitation and create the relationship in ETMP when YES was selected" in {
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      acceptInvitationStub(validVrn.value, invitationIdVAT, identifierVAT)
      givenGetAgencyNameClientStub(arn)

      val reqVAT = authorisedAsValidClientVAT(FakeRequest(), validVrn.value)
        .withFormUrlEncodedBody("confirmTerms" -> "true")
        .withSession("agencyName" -> "My Agency")
      await(submitConfirmTermsVAT(reqVAT))

      verifyAcceptInvitationAttempt(validVrn.value, invitationIdVAT, identifierVAT)
    }

    "redirect to confirm-decline when NO was selected" in {
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      givenGetAgencyNameClientStub(arn)

      val reqVAT = authorisedAsValidClientVAT(FakeRequest(), validVrn.value)
        .withFormUrlEncodedBody("confirmTerms" -> "false")
        .withSession("agencyName" -> "My Agency")

      val resultVAT = submitConfirmTermsVAT(reqVAT)

      status(resultVAT) shouldBe SEE_OTHER
      redirectLocation(resultVAT).get shouldBe routes.ClientsInvitationController.getConfirmDecline(invitationIdVAT).url
    }

    "reshow the page when the radio buttons were not selected with an error message" in {
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      acceptInvitationStub(validVrn.value, invitationIdVAT, identifierVAT)
      givenGetAgencyNameClientStub(arn)

      val reqVAT = authorisedAsValidClientVAT(FakeRequest().withSession("agencyName" -> "My Agency"), validVrn.value)
        .withFormUrlEncodedBody("confirmTerms" -> "")
      val resultVAT = submitConfirmTermsVAT(reqVAT).withSession("agencyName" -> "My Agency")

      status(resultVAT) shouldBe OK
      checkHtmlResultWithBodyText(resultVAT, htmlEscapedMessage("confirm-terms.heading"))
      checkHtmlResultWithBodyText(resultVAT, htmlEscapedMessage("confirm-terms.vat.bullet1"))
      checkHtmlResultWithBodyText(resultVAT, htmlEscapedMessage("confirm-terms.vat.bullet2"))
      checkHtmlResultWithBodyText(resultVAT, htmlEscapedMessage("confirm-terms.vat.bullet3"))
      checkHasClientSignOutUrl(resultVAT)
    }

    "redirect to /incorrect/ if authenticated user has HMRC-MTD-VAT enrolment but with a different clientId" in {
      val reqVAT = authorisedAsValidClientVAT(FakeRequest(), validVrn.value)
        .withFormUrlEncodedBody("confirmTerms" -> "true")
        .withSession("agencyName" -> "My Agency")
      getInvitationNoPermissionStub(validVrn.value, invitationIdVAT, identifierVAT)
      acceptInvitationNoPermissionStub(validVrn.value, invitationIdVAT, identifierVAT)
      givenGetAgencyNameClientStub(arn)
      val resultVAT = submitConfirmTermsVAT(reqVAT)

      status(resultVAT) shouldBe SEE_OTHER
      redirectLocation(resultVAT).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
    }

    "redirect to invitationAlreadyResponded when an invitation is returned that is already actioned" in {
      val reqVAT = authorisedAsValidClientVAT(FakeRequest(), validVrn.value)
        .withFormUrlEncodedBody("confirmTerms" -> "true")
        .withSession("agencyName" -> "My Agency")
      alreadyActionedGetInvitationStub(validVrn.value, invitationIdVAT, identifierVAT)
      alreadyActionedAcceptInvitationStub(validVrn.value, invitationIdVAT, identifierVAT)
      givenGetAgencyNameClientStub(arn)
      val resultVAT= submitConfirmTermsVAT(reqVAT)

      status(resultVAT) shouldBe SEE_OTHER
      redirectLocation(resultVAT) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
    }

    "redirect to notFoundInvitation where no such invitation" in {
      val resultVAT = submitConfirmTermsVAT(
        authorisedAsValidClientVAT(
          FakeRequest()
            .withFormUrlEncodedBody("confirmTerms" -> "true"),
          validVrn.value).withSession("agencyName" -> "My Agency"))
      status(resultVAT) shouldBe SEE_OTHER
      redirectLocation(resultVAT).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to /client/not-signed-up if an authenticated user does not have the HMRC-MTD-VAT Enrolment" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = controller.submitConfirmTerms(invitationIdITSA)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Other Affinity Group",
          Enrolment("OtherEnrolment", "OtherValue", validVrn.value)))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notSignedUp().url
    }

    "redirect to /client/not-found if an authenticated user does not have the Confidence Level 200" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = controller.submitConfirmTerms(invitationIdVAT)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Organisation",
          Enrolment("HMRC-MTD-VAT", "VRN", validVrn.value),
          "50"))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      givenAgencyNameNotFoundClientStub(arn)

      val result = submitConfirmTermsVAT(authorisedAsValidClientVAT(FakeRequest(), validVrn.value))

      an[AgencyNameNotFound] should be thrownBy await(result)
    }

  }

  "GET /accept-tax-agent-invitation/accepted/:invitationId (complete page)" should {

    val getCompletePageVAT: Action[AnyContent] = controller.getCompletePage(invitationIdVAT)

    "show the complete page for VAT" in {
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Accepted")
      givenGetAgencyNameClientStub(arn)

      val result = getCompletePageVAT(
        authorisedAsValidClientVAT(FakeRequest().withSession("agencyName" -> "My Agency"), validVrn.value))

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("client-complete.header"),
          htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("My Agency"))
      checkHtmlResultWithBodyText(result, hasMessage("client-complete.vat.p1", "My Agency"))
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

    "return exception when agency name retrieval fails for VAT" in {
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Accepted")
      val result = getCompletePageVAT(authorisedAsValidClientVAT(FakeRequest(), validVrn.value))
      an[AgencyNameNotFound] should be thrownBy await(result)
    }

    "redirect to /client/not-signed-up if an authenticated user does not have the HMRC-MTD-VAT Enrolment" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = controller.getCompletePage(invitationIdITSA)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Other Affinity Group",
          Enrolment("OtherEnrolment", "OtherValue", validVrn.value)))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notSignedUp().url
    }

    "redirect to /client/not-found if an authenticated user does not have the Confidence Level 200" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = controller.getCompletePage(invitationIdVAT)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Organisation",
          Enrolment("HMRC-MTD-VAT", "VRN", validVrn.value),
          "50"))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }
  }

  "GET /accept-tax-agent-invitation/confirm-decline/:invitationId (confirm decline page)" should {

    val getConfirmInvitationVAT: Action[AnyContent] = controller.getConfirmDecline(invitationIdVAT)

    "show the confirm invitation page for VAT" in {
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      givenGetAgencyNameClientStub(arn)
      val result = getConfirmInvitationVAT(
        authorisedAsValidClientVAT(FakeRequest().withSession("agencyName" -> "My Agency"), validVrn.value))

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("confirm-decline.heading", htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-decline.heading", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-decline.vat.sub-header", "My Agency"))
      checkHasClientSignOutUrl(result)
    }

    "return 303 for not logged in user and redirected to Login Page for VAT" in {
      givenUnauthorisedWith("MissingBearerToken")
      an[AuthorisationException] shouldBe thrownBy {
        await(getConfirmInvitationVAT(FakeRequest().withSession("agencyName" -> "My Agency")))
      }
      verifyAuthoriseAttempt()
    }

    "redirect to /client/not-found if an authenticated user does not have the Confidence Level 200" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = controller.getConfirmDecline(invitationIdVAT)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Organisation",
          Enrolment("HMRC-MTD-VAT", "VRN", validVrn.value),
          "50"))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }

    "redirect to /not-found/ if authenticated user has HMRC-MTD-VAT enrolment but the invitationId they supplied does not exist" in {
      notFoundGetInvitationStub(validVrn.value, invitationIdVAT, identifierVAT)
      val resultVAT = getConfirmInvitationVAT(
        authorisedAsValidClientVAT(FakeRequest().withSession("agencyName" -> "My Agency"), validVrn.value))
      status(resultVAT) shouldBe SEE_OTHER
      redirectLocation(resultVAT).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to invitationAlreadyResponded when an invitation is returned that is already actioned" in {
      getAlreadyAcceptedInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT)
      val resultVAT = getConfirmInvitationVAT(
        authorisedAsValidClientVAT(FakeRequest().withSession("agencyName" -> "My Agency"), validVrn.value))

      status(resultVAT) shouldBe SEE_OTHER
      redirectLocation(resultVAT) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to /incorrect/ if authenticated user has HMRC-MTD-VAT but with a different validVrn.value" in {
      incorrectGetInvitationStub(validVrn.value, invitationIdVAT, identifierVAT)
      val resultVAT = getConfirmInvitationVAT(
        authorisedAsValidClientVAT(FakeRequest().withSession("agencyName" -> "My Agency"), validVrn.value))

      status(resultVAT) shouldBe SEE_OTHER
      redirectLocation(resultVAT).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to notFoundInvitation when invitationId missing from session" in {
      val resultVAT = getConfirmInvitationVAT(authorisedAsValidClientVAT(FakeRequest(), validVrn.value))

      status(resultVAT) shouldBe SEE_OTHER
      redirectLocation(resultVAT) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }
  }

  "POST /accept-tax-agent-invitation/confirm-decline/:invitationId (confirm decline page)" should {
    val submitConfirmInvitationVAT: Action[AnyContent] = controller.submitConfirmDecline(invitationIdVAT)

    "reshow the page when neither yes nor no choices were selected with an error message" in {
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      givenGetAgencyNameClientStub(arn)
      val resultVAT = submitConfirmInvitationVAT(
        authorisedAsValidClientVAT(FakeRequest().withSession("agencyName" -> "My Agency"), validVrn.value))


      status(resultVAT) shouldBe OK
      checkHtmlResultWithBodyText(
        resultVAT,
        htmlEscapedMessage("confirm-decline.heading", htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(resultVAT, htmlEscapedMessage("confirm-decline.heading", "My Agency"))
      checkHtmlResultWithBodyText(resultVAT, htmlEscapedMessage("error.confirmDecline.invalid"))
    }

    "redirect to declined page when YES was selected" in {
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      givenGetAgencyNameClientStub(arn)

      val reqVAT = authorisedAsValidClientVAT(FakeRequest().withSession("agencyName" -> "My Agency"), validVrn.value)
        .withFormUrlEncodedBody("confirmDecline" -> "true")

      val resultVAT = submitConfirmInvitationVAT(reqVAT)

      status(resultVAT) shouldBe SEE_OTHER
      redirectLocation(resultVAT).get shouldBe routes.ClientsInvitationController.getInvitationDeclined(invitationIdVAT).url
    }

    "redirect to confirm terms when NO was selected" in {
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      givenGetAgencyNameClientStub(arn)

      val reqVAT = authorisedAsValidClientVAT(FakeRequest().withSession("agencyName" -> "My Agency"), validVrn.value)
        .withFormUrlEncodedBody("confirmDecline" -> "false")

      val resultVAT = controller.submitConfirmDecline(invitationIdVAT).apply(reqVAT)

      status(resultVAT) shouldBe SEE_OTHER
      redirectLocation(resultVAT) shouldBe Some(
        routes.ClientsInvitationController.getConfirmTerms(invitationIdVAT).url)
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")

      val resultVAT =
        submitConfirmInvitationVAT(authorisedAsValidClientVAT(FakeRequest().withSession(), validVrn.value))

      an[AgencyNameNotFound] should be thrownBy await(resultVAT)
    }

    "redirect to /client/not-signed-up if an authenticated user does not have the HMRC-MTD-VAT Enrolment" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = controller.getConfirmTerms(invitationIdITSA)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Other Affinity Group",
          Enrolment("OtherEnrolment", "OtherValue", validVrn.value)))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notSignedUp().url
    }

    "redirect to /client/not-found if an authenticated user does not have the Confidence Level 200" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = controller.submitConfirmDecline(invitationIdVAT)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Organisation",
          Enrolment("HMRC-MTD-VAT", "VRN", validVrn.value),
          "50"))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }
  }

  "GET /reject-tax-agent-invitation/declined/:invitationId" should {
    val getInvitationDeclinedVAT = controller.getInvitationDeclined(invitationIdVAT)

    "show invitation_declined page for an authenticated client with a valid invitation for VAT" in {
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      rejectInvitationStub(validVrn.value, invitationIdVAT, identifierVAT)
      givenGetAgencyNameClientStub(arn)

      val result = getInvitationDeclinedVAT(
        authorisedAsValidClientVAT(FakeRequest().withSession("agencyName" -> "My Agency"), validVrn.value))

      status(result) shouldBe OK

      checkExitSurveyAfterInviteResponseSignOutUrl(result)
      verifyAgentInvitationResponseEvent(
        invitationIdVAT,
        arn.value,
        "Declined",
        "vrn",
        validVrn.value,
        serviceVAT,
        "My Agency")
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-declined.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-declined-vat.p1", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-declined-vat.button"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-declined.sub-header"))
    }

    "redirect to invitationAlreadyResponded when declined a invitation that is already actioned" in {
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Declined")
      alreadyActionedRejectInvitationStub(validVrn.value, invitationIdVAT, identifierVAT)
      givenGetAgencyNameClientStub(arn)

      val resultVAT = getInvitationDeclinedVAT(
        authorisedAsValidClientVAT(FakeRequest().withSession("agencyName" -> "My Agency"), validVrn.value))

      status(resultVAT) shouldBe SEE_OTHER
      redirectLocation(resultVAT) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
    }

    "redirect to notFoundInvitation when invitation does not exist" in {
      notFoundGetInvitationStub(validVrn.value, invitationIdVAT, identifierVAT)
      val resultVAT = getInvitationDeclinedVAT(
        authorisedAsValidClientVAT(FakeRequest().withSession("agencyName" -> "My Agency"), validVrn.value))

      status(resultVAT) shouldBe SEE_OTHER
      redirectLocation(resultVAT) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to /incorrect/ if authenticated user has HMRC-MTD-VAT enrolment but with a different id" in {
      incorrectGetInvitationStub(validVrn.value, invitationIdVAT, identifierVAT)
      val resultVAT = getInvitationDeclinedVAT(
        authorisedAsValidClientVAT(FakeRequest().withSession("agencyName" -> "My Agency"), validVrn.value))

      status(resultVAT) shouldBe SEE_OTHER
      redirectLocation(resultVAT).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      givenAgencyNameNotFoundClientStub(arn)

      val resultVAT = getInvitationDeclinedVAT(
        authorisedAsValidClientVAT(FakeRequest().withSession("agencyName" -> "My Agency"), validVrn.value))

      an[AgencyNameNotFound] should be thrownBy await(resultVAT)
    }

    "redirect to /client/not-found if an authenticated user does not have the Confidence Level 200" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = controller.getInvitationDeclined(invitationIdVAT)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"), "Organisation",
          Enrolment("HMRC-MTD-VAT", "VRN", validVrn.value),
          "50"))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }
  }

  "GET /not-sign-up/" should {

    "show not-sign-up page with VAT content" in {
      val result = controller.notSignedUp(
        FakeRequest()
          .withSession("clientService" -> "HMRC-MTD-VAT"))
      status(result) shouldBe FORBIDDEN
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("not-found-invitation.header"),
          htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-signed-up-vat.description"))
      await(bodyOf(result)) should not include htmlEscapedMessage("common.sign-out")
    }
  }

  "GET /decide-later/:invitationId" should {
    val getDecideLaterVAT = controller.getDecideLater(invitationIdVAT)

    "show the decide later page with VAT content even when user is not authenticated" in {
      getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
      givenGetAgencyNameClientStub(arn)
      val result = getDecideLaterVAT(FakeRequest())
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("decide-later.header", htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("decide-later.header"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("decide-later.vat.p1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("decide-later.subheader1"))
      await(bodyOf(result)) should not include htmlEscapedMessage("common.sign-out")
    }

    "show a signout url on the landing page if the user is authenticated" in {
      val result = getDecideLaterVAT(FakeRequest().withCookies(Cookie("mdtp", "authToken=Bearer+")))
      status(result) shouldBe OK
      checkHasClientSignOutUrl(result)
    }
  }
}
