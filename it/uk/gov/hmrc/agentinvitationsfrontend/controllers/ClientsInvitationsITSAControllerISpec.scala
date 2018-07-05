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
import uk.gov.hmrc.agentinvitationsfrontend.support.{BaseISpec, TestDataCommonSupport}
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.auth.core.AuthorisationException

class ClientsInvitationsITSAControllerISpec extends BaseISpec with TestDataCommonSupport {

  lazy val controller: ClientsInvitationController = app.injector.instanceOf[ClientsInvitationController]

  "GET /:invitationId (landing page)" should {

    "show the landing page even if the user is not authenticated" in {
      val result = controller.start(invitationIdITSA)(FakeRequest())
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(
        result,
        hasMessage(
          "generic.title",
          htmlEscapedMessage("landing-page.header"),
          htmlEscapedMessage("title.suffix.client")))
      await(bodyOf(result)) should not include htmlEscapedMessage("common.sign-out")
    }

    "show the landing page with ITSA content variant if the invitation ID prefix is 'A'" in {
      val result = controller.start(invitationIdITSA)(FakeRequest())
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(
        result,
        hasMessage(
          "generic.title",
          htmlEscapedMessage("landing-page.header"),
          htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("landing-page.service.itsa.p1"))
    }
  }

  "POST /:invitationId (clicking accept on the landing page)" should {
    val submitStart: Action[AnyContent] = controller.submitStart(invitationIdITSA)

    "redirect to /accept-tax-agent-invitation/consent/:invitationId" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Pending")
      val result =
        submitStart(authorisedAsValidClientITSA(FakeRequest().withSession("agencyName" -> "My Agency"), mtdItId.value))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController
        .getConfirmTerms(invitationIdITSA)
        .url
    }
  }

  "GET /accept-tax-agent-invitation/consent/:invitationId (confirm terms page)" should {

    val getConfirmTermsITSA: Action[AnyContent] = controller.getConfirmTerms(invitationIdITSA)

    "show the confirm terms page for ITSA" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Pending")
      givenGetAgencyNameStub(arn)
      val reqITSA = authorisedAsValidClientITSA(FakeRequest().withSession("agencyName" -> "My Agency"), mtdItId.value)
      val resultITSA = getConfirmTermsITSA(reqITSA)

      status(resultITSA) shouldBe OK
      checkHtmlResultWithBodyText(
        resultITSA,
        htmlEscapedMessage("confirm-terms.itsa.title", htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("confirm-terms.heading"))
      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("confirm-terms.subheading1"))
      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("confirm-terms.itsa.bullet1"))
      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("confirm-terms.itsa.bullet2"))
      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("confirm-terms.itsa.p1"))
      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("confirm-terms.subheading1.p2"))
      checkHasClientSignOutUrl(resultITSA)
    }

    "show the invitation expired page when invitation has expired" in {
      getExpiredInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      givenGetAgencyNameStub(arn)
      val reqITSA = authorisedAsValidClientITSA(FakeRequest().withSession("agencyName" -> "My Agency"), mtdItId.value)
      val resultITSA = getConfirmTermsITSA(reqITSA)

      status(resultITSA) shouldBe SEE_OTHER

      redirectLocation(resultITSA) shouldBe Some(routes.ClientsInvitationController.invitationExpired().url)
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Pending")
      givenAgencyNameNotFoundStub(arn)

      val result = getConfirmTermsITSA(authorisedAsValidClientITSA(FakeRequest(), mtdItId.value))

      an[AgencyNameNotFound] should be thrownBy await(result)
    }

    "redirect to /client/not-signed-up if an authenticated user does not have the HMRC-MTD-IT Enrolment" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = controller.getConfirmTerms(invitationIdITSA)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"),
          Enrolment("OtherEnrolment", "OtherValue", mtdItId.value)))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notSignedUp().url
    }

    "redirect to /client/not-found if an authenticated user does not have the Confidence Level 200" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = controller.getConfirmTerms(invitationIdITSA)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"),
          Enrolment("HMRC-MTD-IT", "MTD-IT-ID", mtdItId.value),
          "50"))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }
  }

  "POST /accept-tax-agent-invitation/consent/:invitationId (clicking confirm on the confirm terms page)" should {
    val submitConfirmTermsITSA: Action[AnyContent] = controller.submitConfirmTerms(invitationIdITSA)

    "redirect to complete page when YES was selected" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Pending")
      acceptInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      givenGetAgencyNameStub(arn)

      val reqITSA = authorisedAsValidClientITSA(FakeRequest(), mtdItId.value)
        .withFormUrlEncodedBody("confirmTerms" -> "true")
        .withSession("agencyName" -> "My Agency")

      val resultITSA = submitConfirmTermsITSA(reqITSA)

      verifyAgentInvitationResponseEvent(
        invitationIdITSA,
        arn.value,
        "Accepted",
        "ni",
        mtdItId.value,
        serviceITSA,
        "My Agency")

      status(resultITSA) shouldBe SEE_OTHER

      redirectLocation(resultITSA).get shouldBe routes.ClientsInvitationController.getCompletePage(invitationIdITSA).url
    }

    "call agent-client-authorisation to accept the invitation and create the relationship in ETMP when YES was selected" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Pending")
      acceptInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      givenGetAgencyNameStub(arn)

      val reqITSA = authorisedAsValidClientITSA(FakeRequest(), mtdItId.value)
        .withFormUrlEncodedBody("confirmTerms" -> "true")
        .withSession("agencyName" -> "My Agency")

      await(submitConfirmTermsITSA(reqITSA))

      verifyAcceptInvitationAttempt(mtdItId.value, invitationIdITSA, identifierITSA)
    }

    "redirect to confirm-decline when NO was selected" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Pending")
      givenGetAgencyNameStub(arn)

      val reqITSA = authorisedAsValidClientITSA(FakeRequest(), mtdItId.value)
        .withFormUrlEncodedBody("confirmTerms" -> "false")
        .withSession("agencyName" -> "My Agency")

      val resultITSA = submitConfirmTermsITSA(reqITSA)

      status(resultITSA) shouldBe SEE_OTHER

      redirectLocation(resultITSA).get shouldBe routes.ClientsInvitationController.getConfirmDecline(invitationIdITSA).url
    }

    "reshow the page when the radio buttons were not selected with an error message" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Pending")
      acceptInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      givenGetAgencyNameStub(arn)

      val reqITSA = authorisedAsValidClientITSA(FakeRequest().withSession("agencyName" -> "My Agency"), mtdItId.value)
        .withFormUrlEncodedBody("confirmTerms" -> "")
      val resultITSA = submitConfirmTermsITSA(reqITSA).withSession("agencyName" -> "My Agency")

      status(resultITSA) shouldBe OK

      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("confirm-terms.heading"))
      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("confirm-terms.subheading1"))
      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("confirm-terms.itsa.bullet1"))
      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("confirm-terms.itsa.bullet2"))
      checkHasClientSignOutUrl(resultITSA)
    }

    "redirect to /incorrect/ if authenticated user has HMRC-MTD-IT or HMRC-NI enrolment but with a different clientId" in {
      val reqITSA = authorisedAsValidClientITSA(FakeRequest(), mtdItId.value)
        .withFormUrlEncodedBody("confirmTerms" -> "true")
        .withSession("agencyName" -> "My Agency")
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Pending")
      acceptInvitationNoPermissionStub(mtdItId.value, invitationIdITSA, identifierITSA)
      givenGetAgencyNameStub(arn)
      val resultITSA = submitConfirmTermsITSA(reqITSA)

      status(resultITSA) shouldBe SEE_OTHER

      redirectLocation(resultITSA).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
    }

    "redirect to invitationAlreadyResponded when an invitation is returned that is already actioned" in {
      val reqITSA = authorisedAsValidClientITSA(FakeRequest(), mtdItId.value)
        .withFormUrlEncodedBody("confirmTerms" -> "true")
        .withSession("agencyName" -> "My Agency")
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Pending")
      alreadyActionedAcceptInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      givenGetAgencyNameStub(arn)
      val resultITSA = submitConfirmTermsITSA(reqITSA)

      status(resultITSA) shouldBe SEE_OTHER
      redirectLocation(resultITSA) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
    }

    "redirect to notFoundInvitation where no such invitation" in {
      val resultITSA = submitConfirmTermsITSA(
        authorisedAsValidClientITSA(
          FakeRequest()
            .withFormUrlEncodedBody("confirmTerms" -> "true"),
          mtdItId.value).withSession("agencyName" -> "My Agency"))

      status(resultITSA) shouldBe SEE_OTHER
      redirectLocation(resultITSA).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to /client/not-signed-up if an authenticated user does not have the HMRC-MTD-IT Enrolment" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = controller.submitConfirmTerms(invitationIdITSA)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"),
          Enrolment("OtherEnrolment", "OtherValue", mtdItId.value)))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notSignedUp().url
    }

    "redirect to /client/not-found if an authenticated user does not have the Confidence Level 200" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = controller.submitConfirmTerms(invitationIdITSA)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"),
          Enrolment("HMRC-MTD-IT", "MTD-IT-ID", mtdItId.value),
          "50"))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Pending")
      givenAgencyNameNotFoundStub(arn)

      val result = submitConfirmTermsITSA(authorisedAsValidClientITSA(FakeRequest(), mtdItId.value))

      an[AgencyNameNotFound] should be thrownBy await(result)
    }

  }

  "GET /accept-tax-agent-invitation/accepted/:invitationId (complete page)" should {

    val getCompletePageITSA: Action[AnyContent] = controller.getCompletePage(invitationIdITSA)

    "show the complete page for ITSA" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Accepted")
      givenGetAgencyNameStub(arn)
      val result = getCompletePageITSA(
        authorisedAsValidClientITSA(FakeRequest().withSession("agencyName" -> "My Agency"), mtdItId.value))
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("client-complete.header"),
          htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("My Agency"))
      checkHtmlResultWithBodyText(result, hasMessage("client-complete.itsa.p1", "My Agency"))
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("client-complete.sub-header"),
        htmlEscapedMessage("client-complete.whatHappensNext.p1"),
        htmlEscapedMessage("client-complete.whatHappensNext.l1"),
        htmlEscapedMessage("client-complete.whatHappensNext.l2"),
        htmlEscapedMessage("client-complete.button.mta"),
        "someAgentClientManagementFrontendExternalUrl"
      )
      checkExitSurveyAfterInviteResponseSignOutUrl(result)

    }

    "return exception when agency name retrieval fails for ITSA" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Accepted")
      givenAgencyNameNotFoundStub(arn)

      val result = getCompletePageITSA(authorisedAsValidClientITSA(FakeRequest(), mtdItId.value))

      an[AgencyNameNotFound] should be thrownBy await(result)
    }

    "redirect to /client/not-signed-up if an authenticated user does not have the HMRC-MTD-IT Enrolment" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = controller.getCompletePage(invitationIdITSA)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"),
          Enrolment("OtherEnrolment", "OtherValue", mtdItId.value)))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notSignedUp().url
    }

    "redirect to /client/not-found if an authenticated user does not have the Confidence Level 200" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = controller.getCompletePage(invitationIdITSA)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"),
          Enrolment("HMRC-MTD-IT", "MTDITID", mtdItId.value),
          "50"))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }
  }

  "GET /accept-tax-agent-invitation/confirm-decline/:invitationId (confirm decline page)" should {

    val getConfirmInvitationITSA: Action[AnyContent] = controller.getConfirmDecline(invitationIdITSA)

    "show the confirm invitation page for ITSA" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Pending")
      givenGetAgencyNameStub(arn)
      val result = getConfirmInvitationITSA(
        authorisedAsValidClientITSA(FakeRequest().withSession("agencyName" -> "My Agency"), mtdItId.value))
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("confirm-invitation.title", htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-invitation.heading", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-invitation.itsa.sub-header", "My Agency"))
      checkHasClientSignOutUrl(result)
    }

    "return 303 for not logged in user and redirected to Login Page for ITSA" in {
      givenUnauthorisedWith("MissingBearerToken")
      an[AuthorisationException] shouldBe thrownBy {
        await(getConfirmInvitationITSA(FakeRequest().withSession("agencyName" -> "My Agency")))
      }
      verifyAuthoriseAttempt()
    }

    "redirect to /client/not-signed-up if an authenticated user does not have the HMRC-MTD-IT Enrolment" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = controller.getConfirmDecline(invitationIdITSA)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"),
          Enrolment("OtherEnrolment", "OtherValue", mtdItId.value)))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notSignedUp().url
    }

    "redirect to /client/not-found if an authenticated user does not have the Confidence Level 200" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = controller.getConfirmDecline(invitationIdITSA)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"),
          Enrolment("HMRC-MTD-IT", "MTDITID", mtdItId.value),
          "50"))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }

    "redirect to /not-found/ if authenticated user has HMRC-MTD-IT enrolment but the invitationId they supplied does not exist" in {
      notFoundGetInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      val resultITSA = getConfirmInvitationITSA(
        authorisedAsValidClientITSA(FakeRequest().withSession("agencyName" -> "My Agency"), mtdItId.value))
      status(resultITSA) shouldBe SEE_OTHER
      redirectLocation(resultITSA).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to invitationAlreadyResponded when an invitation is returned that is already actioned" in {
      getAlreadyAcceptedInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      val resultITSA = getConfirmInvitationITSA(
        authorisedAsValidClientITSA(FakeRequest().withSession("agencyName" -> "My Agency"), mtdItId.value))

      status(resultITSA) shouldBe SEE_OTHER
      redirectLocation(resultITSA) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to /incorrect/ if authenticated user has HMRC-MTD-IT enrolment but with a different mtdItId.value" in {
      incorrectGetInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      val resultITSA = getConfirmInvitationITSA(
        authorisedAsValidClientITSA(FakeRequest().withSession("agencyName" -> "My Agency"), mtdItId.value))

      status(resultITSA) shouldBe SEE_OTHER
      redirectLocation(resultITSA).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to notFoundInvitation when invitationId missing from session" in {
      val resultITSA = getConfirmInvitationITSA(authorisedAsValidClientITSA(FakeRequest(), mtdItId.value))

      status(resultITSA) shouldBe SEE_OTHER
      redirectLocation(resultITSA) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }
  }

  "POST /accept-tax-agent-invitation/confirm-decline/:invitationId (confirm decline page)" should {
    val submitConfirmInvitationITSA: Action[AnyContent] = controller.submitConfirmDecline(invitationIdITSA)

    "reshow the page when neither yes nor no choices were selected with an error message" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Pending")
      givenGetAgencyNameStub(arn)
      val resultITSA = submitConfirmInvitationITSA(
        authorisedAsValidClientITSA(FakeRequest().withSession("agencyName" -> "My Agency"), mtdItId.value))

      status(resultITSA) shouldBe OK
      checkHtmlResultWithBodyText(
        resultITSA,
        htmlEscapedMessage("confirm-invitation.title", htmlEscapedMessage("title.suffix.client")))
      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("confirm-invitation.heading", "My Agency"))
      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("error.confirmInvite.invalid"))
    }

    "redirect to declined page when YES was selected" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Pending")
      givenGetAgencyNameStub(arn)

      val reqITSA = authorisedAsValidClientITSA(FakeRequest().withSession("agencyName" -> "My Agency"), mtdItId.value)
        .withFormUrlEncodedBody("confirmInvite" -> "true")
      val resultITSA = submitConfirmInvitationITSA(reqITSA)

      status(resultITSA) shouldBe SEE_OTHER
      redirectLocation(resultITSA).get shouldBe routes.ClientsInvitationController.getInvitationDeclined(invitationIdITSA).url
    }

    "redirect to confirm terms when NO was selected" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Pending")
      givenGetAgencyNameStub(arn)

      val reqITSA = authorisedAsValidClientITSA(FakeRequest().withSession("agencyName" -> "My Agency"), mtdItId.value)
        .withFormUrlEncodedBody("confirmInvite" -> "false")
      val resultITSA = controller.submitConfirmDecline(invitationIdITSA).apply(reqITSA)

      status(resultITSA) shouldBe SEE_OTHER
      redirectLocation(resultITSA) shouldBe Some(
        routes.ClientsInvitationController.getConfirmTerms(invitationIdITSA).url)
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Pending")

      val resultITSA =
        submitConfirmInvitationITSA(authorisedAsValidClientITSA(FakeRequest().withSession(), mtdItId.value))

      an[AgencyNameNotFound] should be thrownBy await(resultITSA)
    }

    "redirect to /client/not-signed-up if an authenticated user does not have the HMRC-MTD-IT Enrolment" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = controller.submitConfirmDecline(invitationIdITSA)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"),
          Enrolment("OtherEnrolment", "OtherValue", mtdItId.value)))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notSignedUp().url
    }

    "redirect to /client/not-found if an authenticated user does not have the Confidence Level 200" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = controller.submitConfirmDecline(invitationIdITSA)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"),
          Enrolment("HMRC-MTD-IT", "MTDITID", mtdItId.value),
          "50"))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }
  }

  "GET /reject-tax-agent-invitation/declined/:invitationId" should {
    val getInvitationDeclinedITSA = controller.getInvitationDeclined(invitationIdITSA)

    "show invitation_declined page for an authenticated client with a valid invitation for ITSA" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Pending")
      rejectInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      givenGetAgencyNameStub(arn)

      val result = getInvitationDeclinedITSA(
        authorisedAsValidClientITSA(FakeRequest().withSession("agencyName" -> "My Agency"), mtdItId.value))

      status(result) shouldBe OK

      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage(
          "generic.title",
          htmlEscapedMessage("invitation-declined.header"),
          htmlEscapedMessage("title.suffix.client")),
        htmlEscapedMessage("invitation-declined-itsa.p1", "My Agency"),
        htmlEscapedMessage("invitation-declined-itsa.button"),
        s"""href="$taxAccountRelativeUrl""""
      )

      checkExitSurveyAfterInviteResponseSignOutUrl(result)
      verifyAgentInvitationResponseEvent(
        invitationIdITSA,
        arn.value,
        "Declined",
        "ni",
        mtdItId.value,
        serviceITSA,
        "My Agency")
    }

    "redirect to invitationAlreadyResponded when declined a invitation that is already actioned" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Declined")
      alreadyActionedRejectInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      givenGetAgencyNameStub(arn)

      val resultITSA = getInvitationDeclinedITSA(
        authorisedAsValidClientITSA(FakeRequest().withSession("agencyName" -> "My Agency"), mtdItId.value))

      status(resultITSA) shouldBe SEE_OTHER
      redirectLocation(resultITSA) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
    }

    "redirect to notFoundInvitation when invitation does not exist" in {
      notFoundGetInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      val resultITSA = getInvitationDeclinedITSA(
        authorisedAsValidClientITSA(FakeRequest().withSession("agencyName" -> "My Agency"), mtdItId.value))

      status(resultITSA) shouldBe SEE_OTHER
      redirectLocation(resultITSA) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to /incorrect/ if authenticated user has HMRC-MTD-IT enrolment but with a different id" in {
      incorrectGetInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      val resultITSA = getInvitationDeclinedITSA(
        authorisedAsValidClientITSA(FakeRequest().withSession("agencyName" -> "My Agency"), mtdItId.value))

      status(resultITSA) shouldBe SEE_OTHER
      redirectLocation(resultITSA).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Pending")
      givenAgencyNameNotFoundStub(arn)

      val resultITSA = getInvitationDeclinedITSA(
        authorisedAsValidClientITSA(FakeRequest().withSession("agencyName" -> "My Agency"), mtdItId.value))

      an[AgencyNameNotFound] should be thrownBy await(resultITSA)
    }

    "redirect to /client/not-signed-up if an authenticated user does not have the HMRC-MTD-IT Enrolment" in {
      givenUnauthorisedForInsufficientEnrolments()
      val result = controller.getInvitationDeclined(invitationIdITSA)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"),
          Enrolment("OtherEnrolment", "OtherValue", mtdItId.value)))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notSignedUp().url
    }

    "redirect to /client/not-found if an authenticated user does not have the Confidence Level 200" in {
      givenUnauthorisedForInsufficientConfidenceLevel()
      val result = controller.getInvitationDeclined(invitationIdITSA)(
        authenticatedClient(
          FakeRequest().withSession("agencyName" -> "My Agency"),
          Enrolment("HMRC-MTD-IT", "MTDITID", mtdItId.value),
          "50"))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }
  }
}
