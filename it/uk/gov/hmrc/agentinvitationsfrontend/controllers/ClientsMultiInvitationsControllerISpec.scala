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

import org.joda.time.LocalDate
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.connectors.AgencyNameNotFound
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ClientsInvitationController.confirmDeclineForm
import uk.gov.hmrc.agentinvitationsfrontend.models.{ConfirmedTerms, Consent, MultiInvitationsCacheItem}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier}
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class ClientsMultiInvitationsControllerISpec extends BaseISpec {

  lazy val controller: ClientsMultiInvitationController = app.injector.instanceOf[ClientsMultiInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  val expiryDate = LocalDate.now().plusDays(7)

  "GET /:clientType/:uid/:agentName  (warm up page)" should {

    "show the warm up page even if the user is not authenticated for personal taxes" in {
      givenAgentReferenceRecordStub(arn, uid)
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
      givenAgentReferenceRecordStub(arn, uid)
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
      givenAgentReferenceRecordStub(arn, uid)
      givenGetAgencyNameClientStub(arn)

      val result = controller.warmUp("personal", uid, normalisedAgentName)(authorisedAsAnyClient(FakeRequest()))
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
      givenAgentReferenceRecordStub(arn, uid)
      givenGetAgencyNameNotFoundClientStub(arn)

      an[AgencyNameNotFound] shouldBe thrownBy {
        await(controller.warmUp("personal", uid, normalisedAgentName)(FakeRequest()))
      }
    }
  }

  "GET /accept-tax-agent-invitation/consent/:clientType/:uid (multi consent)" should {
    "show the multi consent page with separate consent for each service" in {
      givenAllInvitationIdsStubByStatus(uid, "Pending")
      givenAgentReferenceRecordStub(arn, uid)
      givenGetAgencyNameClientStub(arn)

      val result = controller.getMultiConfirmTerms("personal", uid)(authorisedAsAnyClient(FakeRequest()))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(result, "We need your consent to share information",
        "Report my income and expenses through software",
        "5 March 2018",
        "View your PAYE income record",
        "1 November 2018",
        "Report my VAT returns through software",
        "25 December 2018"
      )

    }

    "redirect to notFound if no invitations are found" in {
      getAllInvitationIdsEmptyByStatusStub(uid, "Pending")

      val result = controller.getMultiConfirmTerms("personal", uid)(authorisedAsAnyClient(FakeRequest()))
      status(result) shouldBe 303

      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
    }

  }

  "POST /accept-tax-agent-invitation/consent/:clientType/:uid (multi consent)" should {
    "redirect to check answers page with consent choices" in {
      await(testMultiInvitationsCache
        .save(MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"),expiryDate, "itsa", consent = false),
          Consent(InvitationId("B9SCS2T4NZBAX"),expiryDate, "afi", consent = false),
          Consent(InvitationId("CZTW1KY6RTAAT"),expiryDate, "vat", consent = false)), Some("My agency Name"))))

      val confirmTermsForm = ClientsMultiInvitationController.confirmTermsMultiForm.fill(ConfirmedTerms(itsaConsent = true, afiConsent = true, vatConsent = true))

      val result = controller.submitMultiConfirmTerms("personal", uid)(
        authorisedAsAnyClient(FakeRequest()).withFormUrlEncodedBody(confirmTermsForm.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsMultiInvitationController.showCheckAnswers("personal", uid).url)
    }

    "redirect to check answers page and update cache correctly when answers have been changed using individual consent page for itsa" in {
      await(testMultiInvitationsCache
        .save(MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"),expiryDate, "itsa", consent = false),
          Consent(InvitationId("B9SCS2T4NZBAX"),expiryDate, "afi", consent = false),
          Consent(InvitationId("CZTW1KY6RTAAT"),expiryDate, "vat", consent = false)), Some("My agency Name"))))

      val confirmTermsForm = ClientsMultiInvitationController.confirmTermsMultiForm.fill(ConfirmedTerms(itsaConsent = true, afiConsent = true, vatConsent = true))

      val result = controller.submitMultiConfirmTerms("personal", uid)(
        authorisedAsAnyClient(FakeRequest()).withFormUrlEncodedBody(confirmTermsForm.data.toSeq: _*)
          .withSession("itsaChoice" -> "false", "afiChoice" -> "false", "vatChoice" -> "false", "whichConsent" -> "itsa"))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsMultiInvitationController.showCheckAnswers("personal", uid).url)

      await(testMultiInvitationsCache.fetch()) shouldBe Some(MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"),expiryDate, "itsa", consent = true),
        Consent(InvitationId("B9SCS2T4NZBAX"),expiryDate, "afi", consent = false),
        Consent(InvitationId("CZTW1KY6RTAAT"),expiryDate, "vat", consent = false)), Some("My agency Name")))
    }

    "redirect to check answers page and update cache correctly when answers have been changed using individual consent page for afi" in {
      await(testMultiInvitationsCache
        .save(MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"),expiryDate, "itsa", consent = false),
          Consent(InvitationId("B9SCS2T4NZBAX"),expiryDate, "afi", consent = false),
          Consent(InvitationId("CZTW1KY6RTAAT"),expiryDate, "vat", consent = false)), Some("My agency Name"))))

      val confirmTermsForm = ClientsMultiInvitationController.confirmTermsMultiForm.fill(ConfirmedTerms(itsaConsent = true, afiConsent = true, vatConsent = true))

      val result = controller.submitMultiConfirmTerms("personal", uid)(
        authorisedAsAnyClient(FakeRequest()).withFormUrlEncodedBody(confirmTermsForm.data.toSeq: _*)
          .withSession("itsaChoice" -> "false", "afiChoice" -> "false", "vatChoice" -> "false", "whichConsent" -> "afi"))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsMultiInvitationController.showCheckAnswers("personal", uid).url)

      await(testMultiInvitationsCache.fetch()) shouldBe Some(MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"),expiryDate, "itsa", consent = false),
        Consent(InvitationId("B9SCS2T4NZBAX"),expiryDate, "afi", consent = true),
        Consent(InvitationId("CZTW1KY6RTAAT"),expiryDate, "vat", consent = false)), Some("My agency Name")))
    }

    "redirect to check answers page and update cache correctly when answers have been changed using individual consent page for vat" in {
      await(testMultiInvitationsCache
        .save(MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"),expiryDate, "itsa", consent = false),
          Consent(InvitationId("B9SCS2T4NZBAX"),expiryDate, "afi", consent = false),
          Consent(InvitationId("CZTW1KY6RTAAT"),expiryDate, "vat", consent = false)), Some("My agency Name"))))

      val confirmTermsForm = ClientsMultiInvitationController.confirmTermsMultiForm.fill(ConfirmedTerms(itsaConsent = true, afiConsent = true, vatConsent = true))

      val result = controller.submitMultiConfirmTerms("personal", uid)(
        authorisedAsAnyClient(FakeRequest()).withFormUrlEncodedBody(confirmTermsForm.data.toSeq: _*)
          .withSession("itsaChoice" -> "false", "afiChoice" -> "false", "vatChoice" -> "false", "whichConsent" -> "vat"))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsMultiInvitationController.showCheckAnswers("personal", uid).url)

      await(testMultiInvitationsCache.fetch()) shouldBe Some(MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"),expiryDate, "itsa", consent = false),
        Consent(InvitationId("B9SCS2T4NZBAX"),expiryDate, "afi", consent = false),
        Consent(InvitationId("CZTW1KY6RTAAT"),expiryDate, "vat", consent = true)), Some("My agency Name")))
    }

  }


  "GET /accept-tax-agent-invitation/confirm-decline/:clientType/:uid (multi confirm decline)" should {
    "show the confirm decline page for personal" in {
      givenAgentReferenceRecordStub(arn, uid)
      givenAllInvitationIdsStubByStatus(uid, "Pending")
      givenGetAgencyNameClientStub(arn)

      val result = controller.getMultiConfirmDecline("personal", uid)(authorisedAsAnyClient(FakeRequest()))

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, "Are you sure you want to decline this request?")
      checkHtmlResultWithBodyText(result, "My Agency will not be able to:")
      checkHtmlResultWithBodyText(result, "report your income and expenses through software")
      checkHtmlResultWithBodyText(result, "report your employer PAYE updates through software")
      checkHtmlResultWithBodyText(result, "report your VAT returns through software")
    }

    "redirect to notFound if there are no invitationIds found" in {
      givenAgentReferenceRecordStub(arn, uid)
      getAllInvitationIdsEmptyByStatusStub(uid, "Pending")
      givenGetAgencyNameClientStub(arn)

      val result = controller.getMultiConfirmDecline("personal", uid)(authorisedAsAnyClient(FakeRequest()))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
    }

    "throw an AgencyNameNotFound exception if agencyName is not found" in {
      givenAgentReferenceRecordStub(arn, uid)
      givenAllInvitationIdsStubByStatus(uid, "Pending")
      givenGetAgencyNameNotFoundClientStub(arn)

      an[AgencyNameNotFound] shouldBe thrownBy {
        await(controller.getMultiConfirmDecline("personal", uid)(authorisedAsAnyClient(FakeRequest())))
      }
    }

    "throw an exception if the agency record is not found" in {
      getNotFoundAgentReferenceRecordStub(uid)
      givenAllInvitationIdsStubByStatus(uid, "Pending")
      givenGetAgencyNameClientStub(arn)

      an[Exception] shouldBe thrownBy {
        await(controller.getMultiConfirmDecline("personal", uid)(authorisedAsAnyClient(FakeRequest())))
      }
    }

  }

  "POST /accept-tax-agent-invitation/confirm-decline/:clientType/:uid (multi confirm decline)" should {

    "redirect to multi invitations declined if YES is selected and invitations are successfully declined" in {
      await(testMultiInvitationsCache.save(MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"),expiryDate, "itsa", true)), Some("my agency name"))))

      getInvitationByIdStub(InvitationId("AG1UGUKTPNJ7W"), "ABCDEF123456789")
      getInvitationByIdStub(InvitationId("B9SCS2T4NZBAX"), "AB123456A")
      getInvitationByIdStub(InvitationId("CZTW1KY6RTAAT"), "101747696")

      rejectInvitationStub("ABCDEF123456789", InvitationId("AG1UGUKTPNJ7W"), "MTDITID")
      rejectInvitationStub("AB123456A", InvitationId("B9SCS2T4NZBAX"), "NI")
      rejectInvitationStub("101747696", InvitationId("CZTW1KY6RTAAT"), "VRN")

      val confirmForm = confirmDeclineForm.fill(ConfirmForm(Some(true)))

      val result = controller.submitMultiConfirmDecline("personal", uid)(authorisedAsAnyClient(FakeRequest()).withFormUrlEncodedBody(confirmForm.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsMultiInvitationController.getMultiInvitationsDeclined(uid).url)
    }

    "redirect to consent page if NO is selected" in {
      await(testMultiInvitationsCache.save(MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"),expiryDate, "itsa", true)), Some("my agency name"))))

      val confirmForm = confirmDeclineForm.fill(ConfirmForm(Some(false)))

      val result = controller.submitMultiConfirmDecline("personal", uid)(authorisedAsAnyClient(FakeRequest()).withFormUrlEncodedBody(confirmForm.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsMultiInvitationController.getMultiConfirmTerms("personal", uid).url)

    }

    "redisplay form with errors if no radio button is selected" in {
      givenAgentReferenceRecordStub(arn, uid)
      givenAllInvitationIdsStubByStatus(uid, "Pending")
      givenGetAgencyNameClientStub(arn)

      val result = controller.submitMultiConfirmDecline("personal", uid)(authorisedAsAnyClient(FakeRequest()))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "This field is required")
    }

    "redirect to multi invitations declined via failure cases" in {
      givenAgentReferenceRecordStub(arn, uid)
      getAllPendingInvitationIdsFalseStub(uid)
      givenGetAgencyNameClientStub(arn)

      getInvitationByIdStub(InvitationId("AG1UGUKTPNJ7W"), "ABCDEF123456789")
      getInvitationByIdStub(InvitationId("B9SCS2T4NZBAX"), "AB123456A")
      getInvitationByIdStub(InvitationId("CZTW1KY6RTAAT"), "101747696")

      rejectInvitationStub("ABCDEF123456789", InvitationId("AG1UGUKTPNJ7W"), "MTDITID")
      rejectInvitationStub("AB123456A", InvitationId("B9SCS2T4NZBAX"), "NI")
      rejectInvitationStub("101747696", InvitationId("CZTW1KY6RTAAT"), "VRN")

      val confirmForm = confirmDeclineForm.fill(ConfirmForm(Some(true)))

      val result = controller.submitMultiConfirmDecline("personal", uid)(authorisedAsAnyClientFalse(FakeRequest()).withFormUrlEncodedBody(confirmForm.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsMultiInvitationController.getMultiInvitationsDeclined(uid).url)
    }

    "throw a Bad Request Exception if there is nothing in the cache" in {
      await(testMultiInvitationsCache.clear())

      val result = controller.submitMultiConfirmDecline("personal", uid)(authorisedAsAnyClient(FakeRequest()))

      an[BadRequestException] shouldBe thrownBy {
        await(result)
      }
    }

    "throw an Exception if there is no agency name in the cache" in {
      await(testMultiInvitationsCache.save(MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"),expiryDate, "itsa", true)), None)))

      val result = controller.submitMultiConfirmDecline("personal", uid)(authorisedAsAnyClientFalse(FakeRequest()))

      an[Exception] shouldBe thrownBy {
        await(result)
      }

    }
  }

  "GET /accept-tax-agent-invitation/check-answers" should {
    "show the check answers page" in {
      await(testMultiInvitationsCache.save(MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"),expiryDate, "itsa", consent = true),
        Consent(InvitationId("B9SCS2T4NZBAX"),expiryDate, "afi", consent = false),
        Consent(InvitationId("CZTW1KY6RTAAT"),expiryDate, "vat", consent = true)), Some("My agency Name"))))

      val result = controller.showCheckAnswers(personal.get, uid)(authorisedAsAnyClient(FakeRequest()))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Check your answers before sending your response",
        "Your consent details",
        "Here are the details of your response to My agency Name",
        "View your PAYE income record",
        "Report your income and expenses through software",
        "Report your VAT returns through software",
        "Confirm and send response",
        "Yes",
        "No"
      )
    }

    "throw a Bad Request Exception if there is nothing in the cache" in {
      await(testMultiInvitationsCache.clear())

      val result = controller.showCheckAnswers(personal.get, uid)(authorisedAsAnyClient(FakeRequest()))

      an[BadRequestException] shouldBe thrownBy {
        await(result)
      }
    }

    "throw an Exception if tehre is no agency name in the cache" in {
      await(testMultiInvitationsCache.save(MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"),expiryDate, "itsa", consent = true),
        Consent(InvitationId("B9SCS2T4NZBAX"),expiryDate, "afi", consent = false),
        Consent(InvitationId("CZTW1KY6RTAAT"),expiryDate, "vat", consent = true)), None)))

      val result = controller.showCheckAnswers(personal.get, uid)(authorisedAsAnyClient(FakeRequest()))

      an[Exception] shouldBe thrownBy {
        await(result)
      }
    }
  }

  "GET /accept-tax-agent-invitation/consent/:clientType/:uid/:givenServiceKey  (multi confirm terms individual)" should {

    "show the multi confirm terms page for only itsa service" in {
      await(testMultiInvitationsCache.save(MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"),expiryDate, "itsa", consent = true),
        Consent(InvitationId("B9SCS2T4NZBAX"),expiryDate, "afi", consent = false),
        Consent(InvitationId("CZTW1KY6RTAAT"),expiryDate, "vat", consent = true)), Some("My Agency Name"))))

      val result = controller.getMultiConfirmTermsIndividual(personal.get, uid, "itsa")(authorisedAsAnyClient(FakeRequest()))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "We need your consent to share information",
        "Report my income and expenses through software",
        "I consent to HMRC sharing the following information with My Agency Name, so this agent can report my income and expenses through software:",
        "my income and expenses information"
      )
      checkHtmlResultWithNotBodyText(result,
        "View your PAYE income record",
        "Report my VAT returns through software")
    }

    "show the multi confirm terms page for only itsa service when coming from check answers" in {
      await(testMultiInvitationsCache.save(MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"),expiryDate, "itsa", consent = true),
        Consent(InvitationId("B9SCS2T4NZBAX"),expiryDate, "afi", consent = false),
        Consent(InvitationId("CZTW1KY6RTAAT"),expiryDate, "vat", consent = true)), Some("My Agency Name"))))

      val result = controller.getMultiConfirmTermsIndividual(personal.get, uid, "itsa")(authorisedAsAnyClient(FakeRequest()
        .withHeaders("Referer" -> s"someBaseUrl${routes.ClientsMultiInvitationController.showCheckAnswers("personal", uid).url}")))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Change your consent",
        "Report my income and expenses through software",
        "I consent to HMRC sharing the following information with My Agency Name, so this agent can report my income and expenses through software:",
        "my income and expenses information"
      )
      checkHtmlResultWithNotBodyText(result,
        "View your PAYE income record",
        "Report my VAT returns through software")
    }

    "show the multi confirm terms page for only irv service" in {
      await(testMultiInvitationsCache.save(MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"),expiryDate, "itsa", consent = true),
        Consent(InvitationId("B9SCS2T4NZBAX"),expiryDate, "afi", consent = false),
        Consent(InvitationId("CZTW1KY6RTAAT"),expiryDate, "vat", consent = true)), Some("My Agency Name"))))

      val result = controller.getMultiConfirmTermsIndividual(personal.get, uid, "afi")(authorisedAsAnyClient(FakeRequest()))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result,
        "View your PAYE income record",
        "I consent to HMRC sharing the following information with My Agency Name, so this agent can view my income record:",
        "who I have worked for in the past"
      )
      checkHtmlResultWithNotBodyText(result,
        "Report my income and expenses through software",
        "Report my VAT returns through software")
    }

    "show the multi confirm terms page for only vat service" in {
      await(testMultiInvitationsCache.save(MultiInvitationsCacheItem(Seq(Consent(InvitationId("AG1UGUKTPNJ7W"),expiryDate, "itsa", consent = true),
        Consent(InvitationId("B9SCS2T4NZBAX"),expiryDate, "afi", consent = false),
        Consent(InvitationId("CZTW1KY6RTAAT"),expiryDate, "vat", consent = true)), Some("My Agency Name"))))

      val result = controller.getMultiConfirmTermsIndividual(personal.get, uid, "vat")(authorisedAsAnyClient(FakeRequest()))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result,
        "Report my VAT returns through software",
        "I consent to HMRC sharing the following information with My Agency Name, so this agent can report my VAT returns through software:",
        "my VAT registration details, such as business name and contact details"
      )
      checkHtmlResultWithNotBodyText(result,
        "View your PAYE income record",
        "Report my income and expenses through software")
    }

    "return Invalid Journey State when there is nothing in the cache" in {
      await(testMultiInvitationsCache.clear())
      val result = controller.getMultiConfirmTermsIndividual(personal.get, uid, "itsa")(authorisedAsAnyClient(FakeRequest()))

      an[BadRequestException] shouldBe thrownBy {
        await(result)
      }
    }

    "return Invalid Journey State if the chosen consent is empty" in {
      await(testMultiInvitationsCache.save(MultiInvitationsCacheItem(Seq(Consent(InvitationId("B9SCS2T4NZBAX"),expiryDate, "afi", consent = false),
        Consent(InvitationId("CZTW1KY6RTAAT"),expiryDate, "vat", consent = true)), Some("My Agency Name"))))

      val result = controller.getMultiConfirmTermsIndividual(personal.get, uid, "itsa")(authorisedAsAnyClient(FakeRequest()))

      an[BadRequestException] shouldBe thrownBy {
        await(result)
      }
    }
  }

  "GET /reject-tax-agent-invitation/declined/uid/:uid (multi invitations declined)" should {

    "show the multi invitations declined page for personal" in {
      givenAgentReferenceRecordStub(arn, uid)
      givenAllInvitationIdsStubByStatus(uid, "Rejected")
      givenGetAgencyNameClientStub(arn)

      val result = controller.getMultiInvitationsDeclined(uid)(authorisedAsAnyClient(FakeRequest()))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result,
        "You have declined this request",
        "You have not given permission to My Agency to:",
        "report your income and expenses through software",
        "report your employer PAYE updates through software",
        "report your VAT returns through software")
    }

    "redirect to notFound if there are no invitationIds found" in {
      givenAgentReferenceRecordStub(arn, uid)
      getAllInvitationIdsEmptyByStatusStub(uid, "Rejected")
      givenGetAgencyNameClientStub(arn)

      val result = controller.getMultiInvitationsDeclined(uid)(authorisedAsAnyClient(FakeRequest()))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
    }

    "Throw an AgencyNameNotFound exception if agencyName is not found" in {
      givenAgentReferenceRecordStub(arn, uid)
      givenAllInvitationIdsStubByStatus(uid, "Rejected")
      givenGetAgencyNameNotFoundClientStub(arn)

      an[AgencyNameNotFound] shouldBe thrownBy {
        await(controller.getMultiInvitationsDeclined(uid)(authorisedAsAnyClient(FakeRequest())))
      }
    }
  }
}
