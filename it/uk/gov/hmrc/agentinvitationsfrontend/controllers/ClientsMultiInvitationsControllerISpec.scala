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

import play.api.mvc.Cookie
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.connectors.AgencyNameNotFound
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ClientsInvitationController.confirmDeclineForm
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId

class ClientsMultiInvitationsControllerISpec extends BaseISpec {

  lazy val controller: ClientsMultiInvitationController = app.injector.instanceOf[ClientsMultiInvitationController]

  "GET /:clientType/:uid/:agentName  (warm up page)" should {

    "show the warm up page even if the user is not authenticated for personal taxes" in {
      getAgentReferenceRecordStub(arn, uid)
      givenGetAgencyNameClientStub(arn)

      val result = controller.warmUp("personal", uid, normalisedAgentName)(FakeRequest())
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result,
        "Appoint My Agency to deal with HMRC for you",
        "We need your approval for each individual or sole trader tax service that My Agency wants to deal with for you.",
        "I do not want to appoint My Agency")
      await(bodyOf(result)) should not include htmlEscapedMessage("common.sign-out")
    }

    "show the warm up page even if the user is not authenticated for business taxes" in {
      getAgentReferenceRecordStub(arn, uid)
      givenGetAgencyNameClientStub(arn)

      val result = controller.warmUp("business", uid, normalisedAgentName)(FakeRequest())
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result,
        "Appoint My Agency to deal with HMRC for you",
        "We need your approval for each business tax service that My Agency wants to deal with for you.",
        "I do not want to appoint My Agency")
      await(bodyOf(result)) should not include htmlEscapedMessage("common.sign-out")
    }

    "show a signout url on the landing page if the user is authenticated" in {
      getAgentReferenceRecordStub(arn, uid)
      givenGetAgencyNameClientStub(arn)

      val result = controller.warmUp("personal", uid, normalisedAgentName)(FakeRequest().withCookies(Cookie("mdtp", "authToken=Bearer+")))
      status(result) shouldBe OK
      checkHasClientSignOutUrl(result)
    }

    "redirect to not found if there is no agent reference record found" in {
      getNotFoundAgentReferenceRecordStub(uid)

      val result = controller.warmUp("personal", uid, normalisedAgentName)(FakeRequest())
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
    }

    "redirect to not found if the normalised agent name does not match database version" in {
      getNotFoundAgentReferenceRecordStub(uid)

      val result = controller.warmUp("personal", uid, "other-agent-name")(FakeRequest())
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
    }

    "throw an AgencyNameNotFound exception if agencyName is not found" in {
      getAgentReferenceRecordStub(arn, uid)
      givenGetAgencyNameNotFoundClientStub(arn)

      an[AgencyNameNotFound] shouldBe thrownBy {
        await(controller.warmUp("personal", uid, normalisedAgentName)(FakeRequest()))
      }
    }
  }

  "GET /accept-tax-agent-invitation/confirm-decline/:clientType/:uid (multi confirm decline)" should {
    "show the confirm decline page for personal" in {
      authorisedAsAnyClient(FakeRequest())
      getAgentReferenceRecordStub(arn, uid)
      getAllPendingInvitationIdsStub(uid)
      givenGetAgencyNameClientStub(arn)

      val result = controller.getMultiConfirmDecline("personal", uid)(FakeRequest())

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, "Are you sure you want to decline this request?")
      checkHtmlResultWithBodyText(result, "My Agency will not be able to:")
      checkHtmlResultWithBodyText(result, "report your income and expenses through software")
      checkHtmlResultWithBodyText(result, "report your employer PAYE updates through software")
      checkHtmlResultWithBodyText(result, "report your VAT returns through software")
    }

    "redirect to notFound if there are no invitationIds found" in {
      authorisedAsAnyClient(FakeRequest())
      getAgentReferenceRecordStub(arn, uid)
      getAllPendingInvitationIdsEmptyStub(uid)
      givenGetAgencyNameClientStub(arn)


      val result = controller.getMultiConfirmDecline("personal", uid)(FakeRequest())

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
    }

    "throw an AgencyNameNotFound exception if agencyName is not found" in {
      authorisedAsAnyClient(FakeRequest())
      getAgentReferenceRecordStub(arn, uid)
      getAllPendingInvitationIdsEmptyStub(uid)
      givenGetAgencyNameNotFoundClientStub(arn)

      an[AgencyNameNotFound] shouldBe thrownBy {
        await(controller.getMultiConfirmDecline("personal", uid)(FakeRequest()))
      }
    }

    "throw an exception if the agency record is not found" in {
      authorisedAsAnyClient(FakeRequest())
      getNotFoundAgentReferenceRecordStub(uid)
      getAllPendingInvitationIdsStub(uid)
      givenGetAgencyNameClientStub(arn)

      an[Exception] shouldBe thrownBy {
        await(controller.getMultiConfirmDecline("personal", uid)(FakeRequest()))
      }
    }

  }

  "POST /accept-tax-agent-invitation/confirm-decline/:clientType/:uid (multi confirm decline)" should {

    "redirect to multi invitations declined if YES is selected and invitations are successfully declined" in {
      authorisedAsAnyClient(FakeRequest())
      getAgentReferenceRecordStub(arn, uid)
      getAllPendingInvitationIdsStub(uid)
      givenGetAgencyNameClientStub(arn)
      getInvitationStub(arn, "ABCDEF123456789", InvitationId("AG1UGUKTPNJ7W"), "HMRC-MTD-IT", "MTDITID", "Pending")
      getInvitationStub(arn, "AB123456A", InvitationId("B9SCS2T4NZBAX"), "PERSONAL-INCOME-RECORD", "NI", "Pending")
      getInvitationStub(arn, "101747696", InvitationId("CZTW1KY6RTAAT"), "HMRC-MTD-VAT", "VRN", "Pending")
      rejectInvitationStub("ABCDEF123456789", InvitationId("AG1UGUKTPNJ7W"), "MTDITID")
      rejectInvitationStub("AB123456A", InvitationId("B9SCS2T4NZBAX"), "NI")
      rejectInvitationStub("101747696", InvitationId("CZTW1KY6RTAAT"), "VRN")

      val confirmForm = confirmDeclineForm.fill(ConfirmForm(Some(true)))

      val result = controller.submitMultiConfirmDecline("personal", uid)(FakeRequest().withFormUrlEncodedBody(confirmForm.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsMultiInvitationController.getMultiInvitationsDeclined(uid).url)
    }

    "redirect to consent page if NO is selected" in {}

    "redisplay form with errors is no radio button is selected" in {
      authorisedAsAnyClient(FakeRequest())
      getAgentReferenceRecordStub(arn, uid)
      getAllPendingInvitationIdsStub(uid)
      givenGetAgencyNameClientStub(arn)

      val result = controller.submitMultiConfirmDecline("personal", uid)(FakeRequest())

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "This field is required")
    }

    "redirect to multi invitations declined via failure cases" in {
      authorisedAsAnyClientFalse(FakeRequest())
      getAgentReferenceRecordStub(arn, uid)
      getAllPendingInvitationIdsFalseStub(uid)
      givenGetAgencyNameClientStub(arn)
      getInvitationStub(arn, "ABCDEF123456789", InvitationId("AG1UGUKTPNJ7W"), "HMRC-MTD-IT", "MTDITID", "Pending")
      getInvitationStub(arn, "AB123456A", InvitationId("B9SCS2T4NZBAX"), "PERSONAL-INCOME-RECORD", "NI", "Pending")
      getInvitationStub(arn, "101747696", InvitationId("CZTW1KY6RTAAT"), "HMRC-MTD-VAT", "VRN", "Pending")
      rejectInvitationStub("ABCDEF123456789", InvitationId("AG1UGUKTPNJ7W"), "MTDITID")
      rejectInvitationStub("AB123456A", InvitationId("B9SCS2T4NZBAX"), "NI")
      rejectInvitationStub("101747696", InvitationId("CZTW1KY6RTAAT"), "VRN")

      val confirmForm = confirmDeclineForm.fill(ConfirmForm(Some(true)))

      val result = controller.submitMultiConfirmDecline("personal", uid)(FakeRequest().withFormUrlEncodedBody(confirmForm.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsMultiInvitationController.getMultiInvitationsDeclined(uid).url)
    }

    "throw an exception if the service is not supported" in {}
  }

  "GET /reject-tax-agent-invitation/declined/:uid (multi invitations declined)" should {

    "show the multi invitations declined page for personal" in {
      authorisedAsAnyClient(FakeRequest())
      getAgentReferenceRecordStub(arn, uid)
      getAllPendingInvitationIdsStub(uid)
      givenGetAgencyNameClientStub(arn)

      val result = controller.getMultiInvitationsDeclined(uid)(FakeRequest())
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result,
        "You have declined this request",
        "You have not given permission to My Agency to:",
        "report your income and expenses through software",
        "report your employer PAYE updates through software",
        "report your VAT returns through software")
    }

    "redirect to notFound if there are no invitationIds found" in {
      authorisedAsAnyClient(FakeRequest())
      getAgentReferenceRecordStub(arn, uid)
      getAllPendingInvitationIdsEmptyStub(uid)
      givenGetAgencyNameClientStub(arn)

      val result = controller.getMultiInvitationsDeclined(uid)(FakeRequest())

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
    }

    "Throw an AgencyNameNotFound exception if agencyName is not found" in {
      authorisedAsAnyClient(FakeRequest())
      getAgentReferenceRecordStub(arn, uid)
      getAllPendingInvitationIdsEmptyStub(uid)
      givenGetAgencyNameNotFoundClientStub(arn)

      an[AgencyNameNotFound] shouldBe thrownBy {
        await(controller.getMultiInvitationsDeclined(uid)(FakeRequest()))
      }
    }
  }

}
