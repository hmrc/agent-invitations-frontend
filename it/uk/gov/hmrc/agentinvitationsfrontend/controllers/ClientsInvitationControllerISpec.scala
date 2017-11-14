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
import uk.gov.hmrc.agentinvitationsfrontend.audit.AgentInvitationEvent
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, MtdItId}
import uk.gov.hmrc.http.NotFoundException

class ClientsInvitationControllerISpec extends BaseISpec {

  lazy val controller: ClientsInvitationController = app.injector.instanceOf[ClientsInvitationController]
  val arn = Arn("TARN0000001")
  val mtdItId = MtdItId("ABCDEF123456789")

  "GET /:invitationId (landing page)" should {
    "show the landing page even if the user is not authenticated" in {
      val result = controller.start("someInvitationID")(FakeRequest())
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("landing-page.title"))
    }

    "session contains invitation ID when loading the landing page even if the user is not authenticated" in {
      implicit val request = FakeRequest()
      val result = controller.start("someInvitationID")(request)

      await(result).session.get("invitationId") shouldBe Some("someInvitationID")
    }
  }

  "POST / (clicking accept on the landing page)" should {

    val submitStart: Action[AnyContent] = controller.submitStart

    "redirect to /accept-tax-agent-invitation/2" in {
      getInvitationStub(arn, mtdItId, "1")
      val result = submitStart(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> "1"), mtdItId.value))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.getConfirmInvitation().url
      verifyAgentInvitationResponseEvent("1", arn.value, "Accepted", mtdItId.value)
    }

    "redirect to /client/not-signed-up if an authenticated user does not have the HMRC-MTD-IT Enrolment" in {
      val result = submitStart(authorisedAsValidAgent(FakeRequest(), ""))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notSignedUp().url
      verifyAuditRequestNotSent(AgentInvitationEvent.AgentClientInvitationResponse)
    }

    "redirect to /not-found/ if authenticated user has HMRC-MTD-IT enrolment but the invitationId they supplied does not exist" in {
      notFoundGetInvitationStub(mtdItId, "1")
      val result = submitStart(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> "1"), mtdItId.value))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.notFoundInvitation().url
      verifyAuditRequestNotSent(AgentInvitationEvent.AgentClientInvitationResponse)
    }

    "redirect to /incorrect/ if authenticated user has HMRC-MTD-IT enrolment but with a different MTDITID" in {
      incorrectGetInvitationStub(mtdItId, "1")
      val result = submitStart(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> "1"), mtdItId.value))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
      verifyAuditRequestNotSent(AgentInvitationEvent.AgentClientInvitationResponse)
    }
  }

  "GET /reject-tax-agent-invitation/1" should {
    val getInvitationDeclined = controller.getInvitationDeclined

    "show invitation_declined page for an authenticated client with a valid invitation" in {
      getInvitationStub(arn, mtdItId, "1")
      rejectInvitationStub(mtdItId, "1")
      givenGetAgencyNameStub(arn)

      val result = getInvitationDeclined(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> "1"), mtdItId.value))

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-declined.title"))
      verifyAgentInvitationResponseEvent("1", arn.value, "Declined", mtdItId.value)
    }

    "redirect to invitationAlreadyResponded when declined a invitation that is already actioned" in {
      getInvitationStub(arn, mtdItId, "1")
      alreadyActionedRejectInvitationStub(mtdItId, "1")
      givenGetAgencyNameStub(arn)

      val result = getInvitationDeclined(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> "1"), mtdItId.value))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.invitationAlreadyResponded().url)
      verifyAgentInvitationResponseEvent("1", arn.value, "Declined", mtdItId.value)
    }

    "redirect to notFoundInvitation when invitation does not exist" in {
      notFoundGetInvitationStub(mtdItId, "1")

      val result = getInvitationDeclined(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> "1"), mtdItId.value))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
      verifyAuditRequestNotSent(AgentInvitationEvent.AgentClientInvitationResponse)
    }

    "redirect to incorrectInvitation when invitationId missing from session" in {
      val result = getInvitationDeclined(authorisedAsValidClient(FakeRequest(), mtdItId.value))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.incorrectInvitation().url)
      verifyAuditRequestNotSent(AgentInvitationEvent.AgentClientInvitationResponse)
    }
  }

  "GET /accept-tax-agent-invitation/2 (confirm invitation page)" should {

    val getConfirmInvitation: Action[AnyContent] = controller.getConfirmInvitation

    "show the confirm invitation page" in {
      getInvitationStub(arn, mtdItId, "1")
      givenGetAgencyNameStub(arn)
      val result = getConfirmInvitation(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> "1"), mtdItId.value))

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-invitation.title", "My Agency"))
    }
  }

  "POST /accept-tax-agent-invitation/2 (clicking continue on the confirm invitation page)" should {
    val submitConfirmInvitation: Action[AnyContent] = controller.submitConfirmInvitation

    "reshow the page when neither yes nor no choices were selected with an error message" in {
      getInvitationStub(arn, mtdItId, "1")
      givenGetAgencyNameStub(arn)
      val result = submitConfirmInvitation(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> "1"), mtdItId.value))

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-invitation.title", "My Agency"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.confirmInvite.invalid"))
    }

    "redirect to confirm terms page when yes was selected" in {
      getInvitationStub(arn, mtdItId, "1")
      givenGetAgencyNameStub(arn)

      val req = authorisedAsValidClient(FakeRequest().withSession("invitationId" -> "1"), mtdItId.value).withFormUrlEncodedBody("confirmInvite" -> "true")
      val result = controller.submitConfirmInvitation().apply(req)

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.getConfirmTerms().url
    }

    "redirect to invitation declined when no is selected" in {
      getInvitationStub(arn, mtdItId, "1")
      givenGetAgencyNameStub(arn)

      val req = authorisedAsValidClient(FakeRequest().withSession("invitationId" -> "1"), mtdItId.value).withFormUrlEncodedBody("confirmInvite" -> "false")
      val result = controller.submitConfirmInvitation().apply(req)

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.getInvitationDeclined().url)
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, mtdItId, "1")
      givenAgencyNameNotFoundStub(arn)

      val result = submitConfirmInvitation(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> "1"), mtdItId.value))

      an[NotFoundException] should be thrownBy await(result)
    }
  }

  "GET /accept-tax-agent-invitation/3 (confirm terms page)" should {

    val getConfirmTerms: Action[AnyContent] = controller.getConfirmTerms

    "show the confirm terms page" in {
      getInvitationStub(arn, mtdItId, "1")
      givenGetAgencyNameStub(arn)
      val req = authorisedAsValidClient(FakeRequest().withSession("invitationId" -> "1"), mtdItId.value)
      val result = getConfirmTerms(req)

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.checkbox", "My Agency"))
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, mtdItId, "1")
      givenAgencyNameNotFoundStub(arn)

      val result = getConfirmTerms(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> "1"), mtdItId.value))

      an[NotFoundException] should be thrownBy await(result)
    }
  }

  "POST /accept-tax-agent-invitation/3 (clicking confirm on the confirm terms page)" should {
    def withSessionData[A](req: FakeRequest[A], key: String, value: String): FakeRequest[A] = {
      req.withSession((req.session + (key -> value)).data.toSeq: _*)
    }

    val submitConfirmTerms: Action[AnyContent] = controller.submitConfirmTerms

    "redirect to complete page when the checkbox was checked" in {
      getInvitationStub(arn, mtdItId, "someInvitationId")
      acceptInvitationStub(mtdItId, "someInvitationId")
      givenGetAgencyNameStub(arn)

      val req = authorisedAsValidClient(FakeRequest(), mtdItId.value).withFormUrlEncodedBody("confirmTerms" -> "true")
      val reqWithSession = withSessionData(req, "invitationId", "someInvitationId")
      val result = submitConfirmTerms(reqWithSession)

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.getCompletePage().url
    }

    "call agent-client-authorisation to accept the invitation and create the relationship in ETMP when the checkbox was checked" in {
      getInvitationStub(arn, mtdItId, "someInvitationId")
      acceptInvitationStub(mtdItId, "someInvitationId")
      givenGetAgencyNameStub(arn)

      val req = authorisedAsValidClient(FakeRequest(), mtdItId.value).withFormUrlEncodedBody("confirmTerms" -> "true")
      val reqWithSession = withSessionData(req, "invitationId", "someInvitationId")
      await(submitConfirmTerms(reqWithSession))

      verifyAcceptInvitationAttempt(mtdItId, "someInvitationId")
    }

    "reshow the page when the checkbox was not checked with an error message" in {
      getInvitationStub(arn, mtdItId, "1")
      acceptInvitationStub(mtdItId, "someInvitationId")
      givenGetAgencyNameStub(arn)

      val req = authorisedAsValidClient(FakeRequest().withSession("invitationId" -> "1"), mtdItId.value).withFormUrlEncodedBody("confirmTerms" -> "")
      val result = submitConfirmTerms(req)

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("error.confirmTerms.invalid"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("confirm-terms.checkbox", "My Agency"))
    }

    "redirect to /incorrect/ if invitation id is not available in the session" in {
      incorrectGetInvitationStub(mtdItId, "1")

      val result = submitConfirmTerms(authorisedAsValidClient(FakeRequest().withFormUrlEncodedBody("confirmTerms" -> "true"), mtdItId.value))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.ClientsInvitationController.incorrectInvitation().url
      verifyAuditRequestNotSent(AgentInvitationEvent.AgentClientInvitationResponse)
    }

  }

  "GET /accept-tax-agent-invitation/4 (complete page)" should {

    val getCompletePage: Action[AnyContent] = controller.getCompletePage

    "show the complete page" in {
      getInvitationStub(arn, mtdItId, "1")
      givenGetAgencyNameStub(arn)

      val result = getCompletePage(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> "1"), mtdItId.value))

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete.title1"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("client-complete.title2", "My Agency"))
    }

    "return exception when agency name retrieval fails" in {
      getInvitationStub(arn, mtdItId, "1")
      givenAgencyNameNotFoundStub(arn)

      val result = getCompletePage(authorisedAsValidClient(FakeRequest().withSession("invitationId" -> "1"), mtdItId.value))

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

  def verifyAgentInvitationResponseEvent(invitationId: String, arn: String, clientResponse: String, mtdItId: String): Unit = {
    verifyAuditRequestSent(1, AgentInvitationEvent.AgentClientInvitationResponse,
      detail = Map(
        "invitationId" -> invitationId,
        "agentReferenceNumber" -> arn,
        "regimeId" -> mtdItId,
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
