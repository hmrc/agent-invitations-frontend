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

import play.api.mvc.{Action, AnyContent}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.audit.AgentInvitationEvent.AgentClientInvitationResponse
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.auth.core.AuthorisationException
import uk.gov.hmrc.http.NotFoundException

class ClientsInvitationControllerISpec extends BaseISpec {

  lazy val controller: ClientsInvitationController = app.injector.instanceOf[ClientsInvitationController]
  val arn = Arn("TARN0000001")
  val mtdItId = MtdItId("ABCDEF123456789")
  val invitationIdITSA = InvitationId("ABERULMHCKKW3")
  val invitationIdAFI = InvitationId("BT5YMLY6GG2L6")
  val invalidInvitationIdCRC5 = InvitationId("ABERULMHCKKW1")
  val nino = "AB123456A"

  "GET /:invitationId (landing page)" should {
    "show the landing page even if the user is not authenticated" in {
      val result = controller.start(invitationIdITSA)(FakeRequest())
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("landing-page.title"))
    }

    "show the landing page with ITSA content variant if the invitation ID prefix is 'A'" in {
      val itsaInvId = InvitationId("ATSF4OW9CCRD2")
      val result = controller.start(itsaInvId)(FakeRequest())
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("landing-page.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("landing-page.service.itsa.p1"))
    }

    "show the landing page with AFI content variant if the invitation ID prefix is 'B'" in {
      val afiPrefixInvId = InvitationId("BTSF4OW9CCRBO")
      val result = controller.start(afiPrefixInvId)(FakeRequest())
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("landing-page.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("landing-page.service.afi.p1"))
    }

    "redirect to notFoundInvitation when the invitation ID prefix is not a known service" in {
      val strangePrefixInvId = InvitationId("CTSF4OW9CCRPT")
      val result = controller.start(strangePrefixInvId)(FakeRequest())
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
    }

    /*"redirect to notFoundInvitation when invitationId fails regex" in {
     /* val result = controller.start(InvitationId("someInvitationID"))(FakeRequest())
     // status(result) shouldBe BAD_REQUEST
      //global.error.400.message
     // redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation.url
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("global.error.400.message"))*/


    }

    "redirect to notFoundInvitation when CRC5 does not match invitationID CRC5" in {
      val result = controller.start(invalidInvitationIdCRC5)(FakeRequest())
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation.url
    }*/
  }

  "POST / (clicking accept on the landing page)" should {
    val submitStart: Action[AnyContent] = controller.submitStart(invitationIdITSA)

    "redirect to /accept-tax-agent-invitation/2" in {
      val result = submitStart(FakeRequest().withSession("invitationId" -> invitationIdITSA.value))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.getConfirmInvitation(invitationIdITSA).url
    }
  }

  "GET /reject-tax-agent-invitation/1" should {
    val getInvitationDeclined = controller.getInvitationDeclined(invitationIdITSA)

    "show invitation_declined page for an authenticated client with a valid invitation" in {

      getInvitationStub(arn, mtdItId.value, invitationIdITSA)
      rejectInvitationStub(mtdItId.value, invitationIdITSA)
      givenGetAgencyNameStub(arn)

      val result = getInvitationDeclined(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-declined.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-declined.p1", "My Agency"))
      verifyAgentInvitationResponseEvent(invitationIdITSA, arn.value, "Declined", mtdItId.value)
    }

    "redirect to invitationAlreadyResponded when declined a invitation that is already actioned" in {

      getInvitationStub(arn, mtdItId.value, invitationIdITSA)
      alreadyActionedRejectInvitationStub(mtdItId.value, invitationIdITSA)
      givenGetAgencyNameStub(arn)

      val result = getInvitationDeclined(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
      verifyAgentInvitationResponseEvent(invitationIdITSA, arn.value, "Declined", mtdItId.value)
    }

    "redirect to notFoundInvitation when invitation does not exist" in {

      notFoundGetInvitationStub(mtdItId.value, invitationIdITSA)
      val result = getInvitationDeclined(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to /incorrect/ if authenticated user has HMRC-MTD-IT enrolment but with a different mtdItId.value" in {

      incorrectGetInvitationStub(mtdItId.value, invitationIdITSA)
      val result = getInvitationDeclined(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to notFound when invitationId missing from session" in {
      val result = getInvitationDeclined(authorisedAsValidClient(FakeRequest(), mtdItId.value))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA)
      givenAgencyNameNotFoundStub(arn)

      val result = getInvitationDeclined(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))

      an[NotFoundException] should be thrownBy await(result)
    }
  }

  "GET /accept-tax-agent-invitation/2 (confirm invitation page)" should {

    val getConfirmInvitationITSA: Action[AnyContent] = controller.getConfirmInvitation(invitationIdITSA)
    val getConfirmInvitationAFI: Action[AnyContent] = controller.getConfirmInvitation(invitationIdAFI)

    "show the confirm invitation page for ITSA" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA)
      givenGetAgencyNameStub(arn)
      val result = getConfirmInvitationITSA(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))
      verifyAgentInvitationResponseEvent(invitationIdITSA, arn.value, "Accepted", mtdItId.value)

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-invitation.title", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-invitation.itsa.sub-header", "My Agency"))
    }

    "show the confirm invitation page for AFI" in {
      getInvitationStub(arn, mtdItId.value, invitationIdAFI)
      givenGetAgencyNameStub(arn)
      val result = getConfirmInvitationAFI(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), mtdItId.value))
      verifyAgentInvitationResponseEvent(invitationIdAFI, arn.value, "Accepted", mtdItId.value)

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-invitation.title", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-invitation.afi.sub-header", "My Agency"))
    }

    "return 303 for not logged in user and redirected to Login Page for ITSA" in {
      givenUnauthorisedWith("MissingBearerToken")
      an[AuthorisationException] shouldBe thrownBy {
        await(getConfirmInvitationITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value)))
      }
      verifyAuthoriseAttempt()
    }

    "return 303 for not logged in user and redirected to Login Page for AFI" in {
      givenUnauthorisedWith("MissingBearerToken")
      an[AuthorisationException] shouldBe thrownBy {
        await(getConfirmInvitationAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value)))
      }
      verifyAuthoriseAttempt()
    }

    "redirect to /client/not-signed-up if an authenticated user does not have the HMRC-MTD-IT Enrolment" in {
      val resultITSA = getConfirmInvitationITSA(authorisedAsValidAgent(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))
      val resultAFI = getConfirmInvitationITSA(authorisedAsValidAgent(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), mtdItId.value))
      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultITSA).get shouldBe routes.ClientsInvitationController.notSignedUp().url
      redirectLocation(resultAFI).get shouldBe routes.ClientsInvitationController.notSignedUp().url
    }

    "redirect to /not-found/ if authenticated user has HMRC-MTD-IT enrolment but the invitationId they supplied does not exist" in {
      notFoundGetInvitationStub(mtdItId.value,invitationIdITSA)
      notFoundGetInvitationStub(mtdItId.value,invitationIdAFI)
      val resultITSA = getConfirmInvitationITSA(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))
      val resultAFI = getConfirmInvitationAFI(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), mtdItId.value))
      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultITSA).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
      redirectLocation(resultAFI).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to invitationAlreadyResponded when an invitation is returned that is already actioned" in {
      getAlreadyAcceptedInvitationStub(arn, mtdItId.value, invitationIdITSA)
      getAlreadyAcceptedInvitationStub(arn, mtdItId.value, invitationIdAFI)
      val resultITSA = getConfirmInvitationITSA(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))
      val resultAFI = getConfirmInvitationAFI(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), mtdItId.value))

      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultITSA) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
      redirectLocation(resultAFI) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to /incorrect/ if authenticated user has HMRC-MTD-IT enrolment but with a different mtdItId.value" in {
      incorrectGetInvitationStub(mtdItId.value, invitationIdITSA)
      incorrectGetInvitationStub(mtdItId.value, invitationIdAFI)
      val resultITSA = getConfirmInvitationITSA(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))
      val resultAFI = getConfirmInvitationITSA(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), mtdItId.value))

      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultITSA).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
      redirectLocation(resultAFI).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to notFoundInvitation when invitationId missing from session" in {
      val resultITSA = getConfirmInvitationITSA(authorisedAsValidClient(FakeRequest(), mtdItId.value))
      val resultAFI = getConfirmInvitationAFI(authorisedAsValidClient(FakeRequest(), mtdItId.value))

      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultITSA) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
      redirectLocation(resultAFI) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }
  }

  "POST /accept-tax-agent-invitation/2 (clicking continue on the confirm invitation page)" should {
    val submitConfirmInvitation: Action[AnyContent] = controller.submitConfirmInvitation(invitationIdITSA)

    "reshow the page when neither yes nor no choices were selected with an error message" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA)
      givenGetAgencyNameStub(arn)
      val result = submitConfirmInvitation(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-invitation.title", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.confirmInvite.invalid"))
    }

    "redirect to confirm terms page when yes was selected" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA)
      givenGetAgencyNameStub(arn)

      val req = authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value).withFormUrlEncodedBody("confirmInvite" -> "true")
      val result = controller.submitConfirmInvitation(invitationIdITSA).apply(req)

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.getConfirmTerms(invitationIdITSA).url
    }

    "redirect to invitation declined when no is selected" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA)
      givenGetAgencyNameStub(arn)

      val req = authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value).withFormUrlEncodedBody("confirmInvite" -> "false")
      val result = controller.submitConfirmInvitation(invitationIdITSA).apply(req)

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.getInvitationDeclined(invitationIdITSA).url)
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA)
      givenAgencyNameNotFoundStub(arn)

      val result = submitConfirmInvitation(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))

      an[NotFoundException] should be thrownBy await(result)
    }
  }

  "GET /accept-tax-agent-invitation/3 (confirm terms page)" should {

    val getConfirmTermsITSA: Action[AnyContent] = controller.getConfirmTerms(invitationIdITSA)
    val getConfirmTermsAFI: Action[AnyContent] = controller.getConfirmTerms(invitationIdAFI)

    "show the confirm terms page for ITSA" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA)
      givenGetAgencyNameStub(arn)
      val req = authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value)
      val result = getConfirmTermsITSA(req)

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-itsa-terms.alert", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-itsa.checkbox", "My Agency"))
    }

    "show the confirm terms page for AFI" in {
      getInvitationStub(arn, mtdItId.value, invitationIdAFI)
      givenGetAgencyNameStub(arn)
      val req = authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), mtdItId.value)
      val result = getConfirmTermsAFI(req)

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-afi-terms.alert", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-afi.checkbox", "My Agency"))
    }

    "show the invitation expired page when invitation has expired" in {
      getExpiredInvitationStub(arn, mtdItId.value, invitationIdITSA)
      getExpiredInvitationStub(arn, mtdItId.value, invitationIdAFI)
      givenGetAgencyNameStub(arn)
      val reqITSA = authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value)
      val reqAFI = authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), mtdItId.value)
      val resultITSA = getConfirmTermsITSA(reqITSA)
      val resultAFI = getConfirmTermsITSA(reqAFI)

      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER

      redirectLocation(resultITSA) shouldBe Some(routes.ClientsInvitationController.invitationExpired.url)
      redirectLocation(resultAFI) shouldBe Some(routes.ClientsInvitationController.invitationExpired.url)
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA)
      givenAgencyNameNotFoundStub(arn)

      val result = getConfirmTermsITSA(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))

      an[NotFoundException] should be thrownBy await(result)
    }
  }

  "POST /accept-tax-agent-invitation/3 (clicking confirm on the confirm terms page)" should {
    val submitConfirmTermsITSA: Action[AnyContent] = controller.submitConfirmTerms(invitationIdITSA)

    "redirect to complete page when the checkbox was checked" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA)
      acceptInvitationStub(mtdItId.value, invitationIdITSA)
      givenGetAgencyNameStub(arn)

      val req = authorisedAsValidClient(FakeRequest(), mtdItId.value).withFormUrlEncodedBody("confirmTerms" -> "true")
      val result = submitConfirmTermsITSA(req)

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.getCompletePage(invitationIdITSA).url
    }

    "call agent-client-authorisation to accept the invitation and create the relationship in ETMP when the checkbox was checked" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA)
      acceptInvitationStub(mtdItId.value, invitationIdITSA)
      givenGetAgencyNameStub(arn)

      val req = authorisedAsValidClient(FakeRequest(), mtdItId.value).withFormUrlEncodedBody("confirmTerms" -> "true")
      await(submitConfirmTermsITSA(req))

      verifyAcceptInvitationAttempt(mtdItId.value, invitationIdITSA)
    }

    "reshow the page when the checkbox was not checked with an error message" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA)
      acceptInvitationStub(mtdItId.value, invitationIdITSA)
      givenGetAgencyNameStub(arn)

      val req = authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value).withFormUrlEncodedBody("confirmTerms" -> "")
      val result = submitConfirmTermsITSA(req)

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.confirmTerms.invalid"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-itsa.checkbox", "My Agency"))
    }

    "redirect to /incorrect/ if authenticated user has HMRC-MTD-IT enrolment but with a different mtdItId.value" in {
      val req = authorisedAsValidClient(FakeRequest(), mtdItId.value).withFormUrlEncodedBody("confirmTerms" -> "true")
      getInvitationStub(arn, mtdItId.value, invitationIdITSA)
      acceptInvitationNoPermissionStub(mtdItId.value, invitationIdITSA)
      val result = submitConfirmTermsITSA(req)

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
    }

    "redirect to invitationAlreadyResponded when an invitation is returned that is already actioned" in {
      val req = authorisedAsValidClient(FakeRequest(), mtdItId.value).withFormUrlEncodedBody("confirmTerms" -> "true")
      getInvitationStub(arn, mtdItId.value, invitationIdITSA)
      alreadyActionedAcceptInvitationStub(mtdItId.value, invitationIdITSA)
      val result = submitConfirmTermsITSA(req)

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
    }

    "redirect to notFoundInvitation where no such invitation" in {

      val result = submitConfirmTermsITSA(authorisedAsValidClient(FakeRequest().withFormUrlEncodedBody("confirmTerms" -> "true"), mtdItId.value))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

  }

  "GET /accept-tax-agent-invitation/4 (complete page)" should {

    val getCompletePage: Action[AnyContent] = controller.getCompletePage(invitationIdITSA)

    "show the complete page" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA)
      givenGetAgencyNameStub(arn)

      val result = getCompletePage(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete.title1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete.title2", "My Agency"))
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA)
      givenAgencyNameNotFoundStub(arn)

      val result = getCompletePage(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))

      an[NotFoundException] should be thrownBy await(result)
    }
  }

  "GET /not-sign-up/" should {
    "show not sign up page if user does not have a valid enrolment" in {
      val result = controller.notSignedUp(FakeRequest())
      status(result) shouldBe FORBIDDEN
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-problem.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-signed-up.description"))
    }
  }

  "GET /incorrect/" should {
    "show incorrect page if user accidentally attempted to respond to another client's invitation" in {
      val result = controller.incorrectInvitation(FakeRequest())
      status(result) shouldBe FORBIDDEN
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-problem.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("incorrect-invitation.description"))
    }
  }

  "GET /not-found/" should {
    "show not found page if user responds to an invitation that does not exist" in {
      val result = controller.notFoundInvitation(FakeRequest())
      status(result) shouldBe NOT_FOUND
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-problem.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("not-found-invitation.description"))
    }
  }

  "GET /already-responded/" should {
    "show already responded page if user responds to an invitation that does not have a status Pending" in {
      val result = controller.invitationAlreadyResponded(FakeRequest())
      status(result) shouldBe FORBIDDEN
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-problem.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-already-responded.description"))
    }
  }

  def verifyAgentInvitationResponseEvent(invitationId: InvitationId, arn: String, clientResponse: String, clientId: String): Unit = {
    verifyAuditRequestSent(1, AgentClientInvitationResponse,
      detail = Map(
        "invitationId" -> invitationId.value,
        "agentReferenceNumber" -> arn,
        "regimeId" -> mtdItId.value,
        "regime" -> "HMRC-MTD-IT",
        "clientResponse" -> clientResponse
      ),
      tags = Map(
        "transactionName" -> "agent-client-invitation-response",
        "path" -> "/"
      )
    )
  }
}
