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
  val serviceITSA = "HMRC-MTD-IT"
  val serviceNI = "HMRC-NI"
  val identifierITSA = "MTDITID"
  val identifierAFI = "NI"
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
  }

  "POST / (clicking accept on the landing page)" should {
    val submitStart: Action[AnyContent] = controller.submitStart(invitationIdITSA)

    "redirect to /accept-tax-agent-invitation/2" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      val result = submitStart(authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))
      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.getConfirmInvitation(invitationIdITSA).url
    }
  }

  "GET /reject-tax-agent-invitation/1" should {
    val getInvitationDeclinedITSA = controller.getInvitationDeclined(invitationIdITSA)
    val getInvitationDeclinedAFI = controller.getInvitationDeclined(invitationIdAFI)

    "show invitation_declined page for an authenticated client with a valid invitation for ITSA" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      rejectInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      givenGetAgencyNameStub(arn)

      val result = getInvitationDeclinedITSA(authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-declined.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-declined-itsa.p1", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-declined-itsa.button"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      verifyAgentInvitationResponseEvent(invitationIdITSA, arn.value, "Declined", mtdItId.value, serviceITSA, "My Agency")
    }

    "show invitation_declined page for an authenticated client with a valid invitation for AFI" in {

      getInvitationStub(arn, nino, invitationIdAFI, serviceNI, identifierAFI)
      rejectInvitationStub(nino, invitationIdAFI, identifierAFI)
      givenGetAgencyNameStub(arn)

      val result = getInvitationDeclinedAFI(authorisedAsValidClientAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), nino))

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-declined.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-declined-afi.p1", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-declined-afi.button"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
      verifyAgentInvitationResponseEvent(invitationIdAFI, arn.value, "Declined", nino, serviceNI, "My Agency")
    }

    "redirect to invitationAlreadyResponded when declined a invitation that is already actioned" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      getInvitationStub(arn, nino, invitationIdAFI, serviceNI, identifierAFI)
      alreadyActionedRejectInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      alreadyActionedRejectInvitationStub(nino, invitationIdAFI, identifierAFI)
      givenGetAgencyNameStub(arn)

      val resultITSA = getInvitationDeclinedITSA(authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))
      val resultAFI = getInvitationDeclinedAFI(authorisedAsValidClientAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), nino))

      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultITSA) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
      redirectLocation(resultAFI) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
      verifyAgentInvitationResponseEvent(invitationIdITSA, arn.value, "Declined", mtdItId.value, serviceITSA, "My Agency")
      verifyAgentInvitationResponseEvent(invitationIdAFI, arn.value, "Declined", nino, serviceNI, "My Agency")
    }

    "redirect to notFoundInvitation when invitation does not exist" in {
      notFoundGetInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      notFoundGetInvitationStub(nino, invitationIdAFI, identifierAFI)
      val resultITSA = getInvitationDeclinedITSA(authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))
      val resultAFI = getInvitationDeclinedAFI(authorisedAsValidClientAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), nino))

      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultITSA) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
      redirectLocation(resultAFI) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to /incorrect/ if authenticated user has HMRC-MTD-IT / HMRC-NI enrolment but with a different id" in {
      incorrectGetInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      incorrectGetInvitationStub(nino, invitationIdAFI, identifierAFI)
      val resultITSA = getInvitationDeclinedITSA(authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))
      val resultAFI = getInvitationDeclinedAFI(authorisedAsValidClientAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), nino))

      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultITSA).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
      redirectLocation(resultAFI).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to notFound when invitationId missing from session" in {
      val resultITSA = getInvitationDeclinedITSA(authorisedAsValidClientITSA(FakeRequest(), mtdItId.value))
      val resultAFI = getInvitationDeclinedAFI(authorisedAsValidClientAFI(FakeRequest(), nino))
      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultITSA) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
      redirectLocation(resultAFI) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      getInvitationStub(arn, nino, invitationIdAFI, serviceNI, identifierAFI)
      givenAgencyNameNotFoundStub(arn)

      val resultITSA = getInvitationDeclinedITSA(authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))
      val resultAFI = getInvitationDeclinedAFI(authorisedAsValidClientAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), nino))

      an[NotFoundException] should be thrownBy await(resultITSA)
      an[NotFoundException] should be thrownBy await(resultAFI)
    }
  }

  "GET /accept-tax-agent-invitation/2 (confirm invitation page)" should {

    val getConfirmInvitationITSA: Action[AnyContent] = controller.getConfirmInvitation(invitationIdITSA)
    val getConfirmInvitationAFI: Action[AnyContent] = controller.getConfirmInvitation(invitationIdAFI)

    "show the confirm invitation page for ITSA" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      givenGetAgencyNameStub(arn)
      val result = getConfirmInvitationITSA(authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))
      verifyAgentInvitationResponseEvent(invitationIdITSA, arn.value, "Accepted", mtdItId.value, serviceITSA, "My Agency")
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-invitation.title", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-invitation.itsa.sub-header", "My Agency"))
    }

    "show the confirm invitation page for AFI" in {
      getInvitationStub(arn, nino, invitationIdAFI, serviceNI, "NI")
      givenGetAgencyNameStub(arn)
      val result = getConfirmInvitationAFI(authorisedAsValidClientAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), nino))
      verifyAgentInvitationResponseEvent(invitationIdAFI, arn.value, "Accepted", nino, serviceNI, "My Agency")

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
      notFoundGetInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      notFoundGetInvitationStub(nino, invitationIdAFI, identifierAFI)
      val resultITSA = getConfirmInvitationITSA(authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))
      val resultAFI = getConfirmInvitationAFI(authorisedAsValidClientAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), nino))
      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultITSA).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
      redirectLocation(resultAFI).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to invitationAlreadyResponded when an invitation is returned that is already actioned" in {
      getAlreadyAcceptedInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      getAlreadyAcceptedInvitationStub(arn, nino, invitationIdAFI, serviceNI, identifierAFI)
      val resultITSA = getConfirmInvitationITSA(authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))
      val resultAFI = getConfirmInvitationAFI(authorisedAsValidClientAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), nino))

      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultITSA) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
      redirectLocation(resultAFI) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to /incorrect/ if authenticated user has HMRC-MTD-IT enrolment but with a different mtdItId.value" in {
      incorrectGetInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      incorrectGetInvitationStub(nino, invitationIdAFI, identifierAFI)
      val resultITSA = getConfirmInvitationITSA(authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))
      val resultAFI = getConfirmInvitationITSA(authorisedAsValidClientAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), nino))

      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultITSA).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
      redirectLocation(resultAFI).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

    "redirect to notFoundInvitation when invitationId missing from session" in {
      val resultITSA = getConfirmInvitationITSA(authorisedAsValidClientITSA(FakeRequest(), mtdItId.value))
      val resultAFI = getConfirmInvitationAFI(authorisedAsValidClientAFI(FakeRequest(), nino))

      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultITSA) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
      redirectLocation(resultAFI) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }
  }

  "POST /accept-tax-agent-invitation/2 (clicking continue on the confirm invitation page)" should {
    val submitConfirmInvitationITSA: Action[AnyContent] = controller.submitConfirmInvitation(invitationIdITSA)
    val submitConfirmInvitationAFI: Action[AnyContent] = controller.submitConfirmInvitation(invitationIdAFI)

    "reshow the page when neither yes nor no choices were selected with an error message" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      getInvitationStub(arn, nino, invitationIdAFI, serviceNI, identifierAFI)
      givenGetAgencyNameStub(arn)
      val resultITSA = submitConfirmInvitationITSA(authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))
      val resultAFI = submitConfirmInvitationAFI(authorisedAsValidClientAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), nino))

      status(resultITSA) shouldBe OK
      status(resultAFI) shouldBe OK
      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("confirm-invitation.title", "My Agency"))
      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("error.confirmInvite.invalid"))
      checkHtmlResultWithBodyText(resultAFI, htmlEscapedMessage("confirm-invitation.title", "My Agency"))
      checkHtmlResultWithBodyText(resultAFI, htmlEscapedMessage("error.confirmInvite.invalid"))
    }

    "redirect to confirm terms page when yes was selected" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      getInvitationStub(arn, nino, invitationIdAFI, serviceNI, identifierAFI)
      givenGetAgencyNameStub(arn)

      val reqITSA = authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value).withFormUrlEncodedBody("confirmInvite" -> "true")
      val reqAFI = authorisedAsValidClientAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), nino).withFormUrlEncodedBody("confirmInvite" -> "true")
      val resultITSA = submitConfirmInvitationITSA(reqITSA)
      val resultAFI = submitConfirmInvitationAFI(reqAFI)

      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultITSA).get shouldBe routes.ClientsInvitationController.getConfirmTerms(invitationIdITSA).url
      redirectLocation(resultAFI).get shouldBe routes.ClientsInvitationController.getConfirmTerms(invitationIdAFI).url
    }

    "redirect to invitation declined when no is selected" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      getInvitationStub(arn, nino, invitationIdAFI, serviceNI, identifierAFI)
      givenGetAgencyNameStub(arn)

      val reqITSA = authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value).withFormUrlEncodedBody("confirmInvite" -> "false")
      val reqAFI = authorisedAsValidClientAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), nino).withFormUrlEncodedBody("confirmInvite" -> "false")
      val resultITSA = controller.submitConfirmInvitation(invitationIdITSA).apply(reqITSA)
      val resultAFI = controller.submitConfirmInvitation(invitationIdAFI).apply(reqAFI)

      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultITSA) shouldBe Some(routes.ClientsInvitationController.getInvitationDeclined(invitationIdITSA).url)
      redirectLocation(resultAFI) shouldBe Some(routes.ClientsInvitationController.getInvitationDeclined(invitationIdAFI).url)
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      getInvitationStub(arn, nino, invitationIdAFI, serviceNI, identifierAFI)
      givenAgencyNameNotFoundStub(arn)

      val resultITSA = submitConfirmInvitationITSA(authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))
      val resultAFI = submitConfirmInvitationAFI(authorisedAsValidClientAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), nino))

      an[NotFoundException] should be thrownBy await(resultITSA)
      an[NotFoundException] should be thrownBy await(resultAFI)
    }
  }

  "GET /accept-tax-agent-invitation/3 (confirm terms page)" should {

    val getConfirmTermsITSA: Action[AnyContent] = controller.getConfirmTerms(invitationIdITSA)
    val getConfirmTermsAFI: Action[AnyContent] = controller.getConfirmTerms(invitationIdAFI)

    "show the confirm terms page for ITSA" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      getInvitationStub(arn, nino, invitationIdAFI, serviceNI, identifierAFI)
      givenGetAgencyNameStub(arn)
      val reqITSA = authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value)
      val reqAFI = authorisedAsValidClientAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), nino)
      val resultITSA = getConfirmTermsITSA(reqITSA)
      val resultAFI = getConfirmTermsAFI(reqAFI)

      status(resultITSA) shouldBe OK
      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("confirm-terms.title"))
      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("confirm-itsa-terms.alert", "My Agency"))
      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("confirm-terms-itsa.checkbox", "My Agency"))
    }

    "show the confirm terms page for AFI" in {
      getInvitationStub(arn, nino, invitationIdAFI, serviceNI, "NI")
      givenGetAgencyNameStub(arn)
      val req = authorisedAsValidClientAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), nino)
      val result = getConfirmTermsAFI(req)

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-afi-terms.alert", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms-afi.checkbox", "My Agency"))
    }

    "show the invitation expired page when invitation has expired" in {
      getExpiredInvitationStub(arn, mtdItId.value, invitationIdITSA)
      getExpiredInvitationStub(arn, nino, invitationIdAFI)
      givenGetAgencyNameStub(arn)
      val reqITSA = authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value)
      val reqAFI = authorisedAsValidClientAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), nino)
      val resultITSA = getConfirmTermsITSA(reqITSA)
      val resultAFI = getConfirmTermsITSA(reqAFI)

      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER

      redirectLocation(resultITSA) shouldBe Some(routes.ClientsInvitationController.invitationExpired.url)
      redirectLocation(resultAFI) shouldBe Some(routes.ClientsInvitationController.invitationExpired.url)
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      givenAgencyNameNotFoundStub(arn)

      val result = getConfirmTermsITSA(authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))

      an[NotFoundException] should be thrownBy await(result)
    }
  }

  "POST /accept-tax-agent-invitation/3 (clicking confirm on the confirm terms page)" should {
    val submitConfirmTermsITSA: Action[AnyContent] = controller.submitConfirmTerms(invitationIdITSA)
    val submitConfirmTermsAFI: Action[AnyContent] = controller.submitConfirmTerms(invitationIdAFI)

    "redirect to complete page when the checkbox was checked" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      getInvitationStub(arn, nino, invitationIdAFI, serviceNI, identifierAFI)
      acceptInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      acceptInvitationStub(nino, invitationIdAFI, identifierAFI)
      givenGetAgencyNameStub(arn)

      val reqITSA = authorisedAsValidClientITSA(FakeRequest(), mtdItId.value).withFormUrlEncodedBody("confirmTerms" -> "true")
      val reqAFI = authorisedAsValidClientAFI(FakeRequest(), nino).withFormUrlEncodedBody("confirmTerms" -> "true")
      val resultITSA = submitConfirmTermsITSA(reqITSA)
      val resultAFI = submitConfirmTermsAFI(reqAFI)

      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultITSA).get shouldBe routes.ClientsInvitationController.getCompletePage(invitationIdITSA).url
      redirectLocation(resultAFI).get shouldBe routes.ClientsInvitationController.getCompletePage(invitationIdAFI).url
    }

    "call agent-client-authorisation to accept the invitation and create the relationship in ETMP when the checkbox was checked" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      getInvitationStub(arn, nino, invitationIdAFI, serviceNI, identifierAFI)
      acceptInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      acceptInvitationStub(nino, invitationIdAFI, identifierAFI)
      givenGetAgencyNameStub(arn)

      val reqITSA = authorisedAsValidClientITSA(FakeRequest(), mtdItId.value).withFormUrlEncodedBody("confirmTerms" -> "true")
      val reqAFI = authorisedAsValidClientAFI(FakeRequest(), nino).withFormUrlEncodedBody("confirmTerms" -> "true")
      await(submitConfirmTermsITSA(reqITSA))
      await(submitConfirmTermsAFI(reqAFI))

      verifyAcceptInvitationAttempt(mtdItId.value, invitationIdITSA, identifierITSA)
      verifyAcceptInvitationAttempt(nino, invitationIdAFI, identifierAFI)
    }

    "reshow the page when the checkbox was not checked with an error message" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      getInvitationStub(arn, nino, invitationIdAFI, serviceNI, identifierAFI)
      acceptInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      acceptInvitationStub(nino, invitationIdAFI, identifierAFI)
      givenGetAgencyNameStub(arn)

      val reqITSA = authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value).withFormUrlEncodedBody("confirmTerms" -> "")
      val reqAFI = authorisedAsValidClientAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), nino).withFormUrlEncodedBody("confirmTerms" -> "")
      val resultITSA = submitConfirmTermsITSA(reqITSA)
      val resultAFI = submitConfirmTermsAFI(reqAFI)

      status(resultITSA) shouldBe OK
      status(resultAFI) shouldBe OK
      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("confirm-terms.title"))
      checkHtmlResultWithBodyText(resultAFI, htmlEscapedMessage("confirm-terms.title"))
      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("error.confirmTerms.invalid"))
      checkHtmlResultWithBodyText(resultAFI, htmlEscapedMessage("error.confirmTerms.invalid"))
      checkHtmlResultWithBodyText(resultITSA, htmlEscapedMessage("confirm-terms-itsa.checkbox", "My Agency"))
      checkHtmlResultWithBodyText(resultAFI, htmlEscapedMessage("confirm-terms-afi.checkbox", "My Agency"))
    }

    "redirect to /incorrect/ if authenticated user has HMRC-MTD-IT or HMRC-NI enrolment but with a different clientId" in {
      val reqITSA = authorisedAsValidClientITSA(FakeRequest(), mtdItId.value).withFormUrlEncodedBody("confirmTerms" -> "true")
      val reqAFI = authorisedAsValidClientAFI(FakeRequest(), nino).withFormUrlEncodedBody("confirmTerms" -> "true")
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      getInvitationStub(arn, nino, invitationIdAFI, serviceNI, identifierAFI)
      acceptInvitationNoPermissionStub(mtdItId.value, invitationIdITSA, identifierITSA)
      acceptInvitationNoPermissionStub(nino, invitationIdAFI, identifierAFI)
      val resultITSA = submitConfirmTermsITSA(reqITSA)
      val resultAFI = submitConfirmTermsAFI(reqAFI)

      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultITSA).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
      redirectLocation(resultAFI).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
    }

    "redirect to invitationAlreadyResponded when an invitation is returned that is already actioned" in {
      val reqITSA = authorisedAsValidClientITSA(FakeRequest(), mtdItId.value).withFormUrlEncodedBody("confirmTerms" -> "true")
      val reqAFI = authorisedAsValidClientAFI(FakeRequest(), nino).withFormUrlEncodedBody("confirmTerms" -> "true")
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      getInvitationStub(arn, nino, invitationIdAFI, serviceNI, identifierAFI)
      alreadyActionedAcceptInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      alreadyActionedAcceptInvitationStub(nino, invitationIdAFI, identifierAFI)
      val resultITSA = submitConfirmTermsITSA(reqITSA)
      val resultAFI = submitConfirmTermsITSA(reqAFI)

      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultITSA) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
      redirectLocation(resultAFI) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
    }

    "redirect to notFoundInvitation where no such invitation" in {
      val resultITSA = submitConfirmTermsITSA(authorisedAsValidClientITSA(FakeRequest().withFormUrlEncodedBody("confirmTerms" -> "true"), mtdItId.value))
      val resultAFI = submitConfirmTermsAFI(authorisedAsValidClientAFI(FakeRequest().withFormUrlEncodedBody("confirmTerms" -> "true"), nino))

      status(resultITSA) shouldBe SEE_OTHER
      status(resultAFI) shouldBe SEE_OTHER
      redirectLocation(resultITSA).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
      redirectLocation(resultAFI).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
      verifyAuditRequestNotSent(AgentClientInvitationResponse)
    }

  }

  "GET /accept-tax-agent-invitation/4 (complete page)" should {

    val getCompletePageITSA: Action[AnyContent] = controller.getCompletePage(invitationIdITSA)
    val getCompletePageAFI: Action[AnyContent] = controller.getCompletePage(invitationIdAFI)

    "show the complete page for ITSA" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      givenGetAgencyNameStub(arn)
      val result = getCompletePageITSA(authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete.title1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete.title2", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete-itsa.title3"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete-itsa.p1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete-itsa.button"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
    }

    "return exception when agency name retrieval fails for ITSA" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA)
      givenAgencyNameNotFoundStub(arn)

      val result = getCompletePageITSA(authorisedAsValidClientITSA(FakeRequest().withSession("invitationId" -> invitationIdITSA.value), mtdItId.value))

      an[NotFoundException] should be thrownBy await(result)
    }

  "show the complete page for AFI" in {
      getInvitationStub(arn, nino, invitationIdAFI, serviceNI, identifierAFI)
      givenGetAgencyNameStub(arn)

      val result = getCompletePageAFI(authorisedAsValidClientAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), nino))

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete.title1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete.title2", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete-afi.title3"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete-afi.button"))
      checkHtmlResultWithBodyText(result, wireMockBaseUrlAsString)
    }

    "return exception when agency name retrieval fails for AFI" in {
      getInvitationStub(arn, nino, invitationIdAFI, serviceNI, identifierAFI)
      givenAgencyNameNotFoundStub(arn)
      val result = getCompletePageAFI(authorisedAsValidClientAFI(FakeRequest().withSession("invitationId" -> invitationIdAFI.value), nino))
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

  def verifyAgentInvitationResponseEvent(invitationId: InvitationId, arn: String, clientResponse: String, clientId: String, service: String,  agencyName: String): Unit = {
    verifyAuditRequestSent(1, AgentClientInvitationResponse,
      detail = Map(
        "invitationId" -> invitationId.value,
        "agentReferenceNumber" -> arn,
        "agencyName" -> agencyName,
        "regimeId" -> clientId,
        "regime" -> service,
        "clientResponse" -> clientResponse
      ),
      tags = Map(
        "transactionName" -> "agent-client-invitation-response",
        "path" -> "/"
      )
    )
  }
}
