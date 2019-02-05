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
import uk.gov.hmrc.agentinvitationsfrontend.models.{ClientConsent, ClientConsentsJourneyState, ConfirmedTerms}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId
import uk.gov.hmrc.http.logging.SessionId
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier}

import scala.concurrent.ExecutionContext.Implicits.global

class ClientsMultiInvitationsControllerISpec extends BaseISpec {

  lazy val controller: ClientsMultiInvitationController = app.injector.instanceOf[ClientsMultiInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))

  val expiryDate = LocalDate.now().plusDays(7)

  "GET /:clientType/:uid/:agentName  (warm up page)" should {

    "show the warm up page even if the user is not authenticated for personal taxes" in {
      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenGetAgencyNameClientStub(arn)

      val result = controller.warmUp("personal", uid, normalisedAgentName)(FakeRequest())
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(
        result,
        "Appoint My Agency to deal with HMRC for you",
        "We need your approval for each individual or sole trader tax service that My Agency wants to deal with for you.",
        "I do not want to appoint My Agency"
      )
      await(bodyOf(result)) should not include htmlEscapedMessage("common.sign-out")
    }

    "show the warm up page even if the user is not authenticated for business taxes" in {
      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenGetAgencyNameClientStub(arn)

      val result = controller.warmUp("business", uid, normalisedAgentName)(FakeRequest())
      status(result) shouldBe OK
      checkHtmlResultWithBodyText(
        result,
        "Appoint My Agency to deal with HMRC for you",
        "We need your approval for each business tax service that My Agency wants to deal with for you.",
        "I do not want to appoint My Agency"
      )
      await(bodyOf(result)) should not include htmlEscapedMessage("common.sign-out")
    }

    "show a signout url on the landing page if the user is authenticated" in {
      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenGetAgencyNameClientStub(arn)

      val result =
        controller.warmUp("personal", uid, normalisedAgentName)(authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe OK
      checkHasClientSignOutUrl(result)
    }

    "redirect to not found if there is no agent reference record found" in {
      givenAgentReferenceRecordNotFoundForUid(uid)

      val result = controller.warmUp("personal", uid, normalisedAgentName)(FakeRequest())
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
    }

    "redirect to not found if the normalised agent name does not match database version" in {
      givenAgentReferenceRecordNotFoundForUid(uid)

      val result = controller.warmUp("personal", uid, "other-agent-name")(FakeRequest())
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
    }

    "throw an AgencyNameNotFound exception if agencyName is not found" in {
      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenGetAgencyNameNotFoundClientStub(arn)

      an[AgencyNameNotFound] shouldBe thrownBy {
        await(controller.warmUp("personal", uid, normalisedAgentName)(FakeRequest()))
      }
    }
  }

  "GET /accept-tax-agent-invitation/consent/:clientType/:uid (multi consent)" should {
    "show the multi consent page with separate consent for each service for personal" in {
      givenAllInvitationIdsByStatus(uid, "Pending")
      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenGetAgencyNameClientStub(arn)

      val result = controller.getMultiConfirmTerms("personal", uid)(authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(
        result,
        "We need your consent to share information",
        "Report my income and expenses through software",
        "5 March 9999",
        "View your PAYE income record",
        "1 November 9999",
        "Submit my VAT Returns through software",
        "25 December 9999"
      )

    }

    "show the multi consent page with separate consent for each service for business" in {
      givenAllInvitationIdsByStatus(uid, "Pending")
      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenGetAgencyNameClientStub(arn)

      val result = controller.getMultiConfirmTerms("business", uid)(authorisedAsAnyOrganisationClient(FakeRequest()))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(
        result,
        "We need your consent to share information",
        "Report my income and expenses through software",
        "5 March 9999",
        "View your PAYE income record",
        "1 November 9999",
        "Submit my VAT Returns through software",
        "25 December 9999"
      )

    }

    "show the multi consents page but don't show services in which the invitation expiry date has passed" in {
      givenAllInvitationIdsByStatusReturnsSomeExpired(uid, "Pending")
      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenGetAgencyNameClientStub(arn)

      val result = controller.getMultiConfirmTerms("personal", uid)(authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 200

      checkHtmlResultWithBodyText(
        result,
        "We need your consent to share information",
        "Report my income and expenses through software",
        "1 November 9999")
      checkHtmlResultWithNotBodyText(
        result,
        "View your PAYE income record",
        "5 March 9999",
        "Report my VAT Returns through software",
        "25 December 2000")
    }

    "redirect to not found invitation if there are no invitations found" in {
      givenAllInvitationIdsByStatusReturnsEmpty(uid, "Pending")
      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenGetAgencyNameClientStub(arn)

      val result = controller.getMultiConfirmTerms("personal", uid)(authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
    }

    "redirect to notFound if an exception is thrown by an external service" in {
      givenAllInvitationIdsByStatusReturnsNotFound(uid, "Pending")

      val result = controller.getMultiConfirmTerms("personal", uid)(authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 303

      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
    }

    "redirect to wrong-account-type if signed-in as business for personal invitation" in {
      givenAllInvitationIdsByStatusReturnsNotFound(uid, "Pending")

      val result = controller.getMultiConfirmTerms("personal", uid)(authorisedAsAnyOrganisationClient(FakeRequest()))
      status(result) shouldBe 303

      redirectLocation(result) shouldBe Some(routes.ClientErrorController.incorrectClientType().url)
    }

    "redirect to wrong-account-type if signed-in as personal for business invitation" in {
      givenAllInvitationIdsByStatusReturnsNotFound(uid, "Pending")

      val result = controller.getMultiConfirmTerms("business", uid)(authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 303

      redirectLocation(result) shouldBe Some(routes.ClientErrorController.incorrectClientType().url)
    }

  }

  "POST /accept-tax-agent-invitation/consent/:clientType/:uid (multi consent)" should {
    "redirect to check answers page with consent choices" in {
      await(
        testClientConsentsJourneyStateCache
          .save(ClientConsentsJourneyState(
            Seq(
              ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false),
              ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false),
              ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false)
            ),
            Some("My agency Name")
          )))

      val confirmTermsForm = ClientsMultiInvitationController.confirmTermsMultiForm.fill(
        ConfirmedTerms(itsaConsent = true, afiConsent = true, vatConsent = true))

      val result = controller.submitMultiConfirmTerms("personal", uid)(
        authorisedAsAnyIndividualClient(FakeRequest()).withFormUrlEncodedBody(confirmTermsForm.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(
        routes.ClientsMultiInvitationController.showCheckAnswers("personal", uid).url)
    }

    "redirect to check answers page and update cache correctly when answers have been changed using individual consent page for itsa" in {
      await(
        testClientConsentsJourneyStateCache
          .save(ClientConsentsJourneyState(
            Seq(
              ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false),
              ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false),
              ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false)
            ),
            Some("My agency Name")
          )))

      val confirmTermsForm = ClientsMultiInvitationController.confirmTermsMultiForm.fill(
        ConfirmedTerms(itsaConsent = true, afiConsent = true, vatConsent = true))

      val result = controller.submitMultiConfirmTerms("personal", uid)(authorisedAsAnyIndividualClient(FakeRequest())
        .withFormUrlEncodedBody(confirmTermsForm.data.toSeq: _*)
        .withSession("itsaChoice" -> "false", "afiChoice" -> "false", "vatChoice" -> "false", "whichConsent" -> "itsa"))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(
        routes.ClientsMultiInvitationController.showCheckAnswers("personal", uid).url)

      await(testClientConsentsJourneyStateCache.fetch) shouldBe Some(
        ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = true),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false)
          ),
          Some("My agency Name")
        ))
    }

    "redirect to check answers page and update cache correctly when answers have been changed using individual consent page for afi" in {
      await(
        testClientConsentsJourneyStateCache
          .save(ClientConsentsJourneyState(
            Seq(
              ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false),
              ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false),
              ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false)
            ),
            Some("My agency Name")
          )))

      val confirmTermsForm = ClientsMultiInvitationController.confirmTermsMultiForm.fill(
        ConfirmedTerms(itsaConsent = true, afiConsent = true, vatConsent = true))

      val result = controller.submitMultiConfirmTerms("personal", uid)(authorisedAsAnyIndividualClient(FakeRequest())
        .withFormUrlEncodedBody(confirmTermsForm.data.toSeq: _*)
        .withSession("itsaChoice" -> "false", "afiChoice" -> "false", "vatChoice" -> "false", "whichConsent" -> "afi"))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(
        routes.ClientsMultiInvitationController.showCheckAnswers("personal", uid).url)

      await(testClientConsentsJourneyStateCache.fetch) shouldBe Some(
        ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false)
          ),
          Some("My agency Name")
        ))
    }

    "redirect to check answers page and update cache correctly when answers have been changed using individual consent page for vat" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false)
          ),
          Some("My agency Name")
        )))

      val confirmTermsForm = ClientsMultiInvitationController.confirmTermsMultiForm.fill(
        ConfirmedTerms(itsaConsent = true, afiConsent = true, vatConsent = true))

      val result = controller.submitMultiConfirmTerms("personal", uid)(authorisedAsAnyIndividualClient(FakeRequest())
        .withFormUrlEncodedBody(confirmTermsForm.data.toSeq: _*)
        .withSession("itsaChoice" -> "false", "afiChoice" -> "false", "vatChoice" -> "false", "whichConsent" -> "vat"))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(
        routes.ClientsMultiInvitationController.showCheckAnswers("personal", uid).url)

      await(testClientConsentsJourneyStateCache.fetch) shouldBe Some(
        ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true)
          ),
          Some("My agency Name")
        ))
    }

    "redirect to wrong-account-type page when submitting confirm terms" in {
      await(
    testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
      Seq(
        ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false),
        ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false),
        ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false)
      ),
      Some("My agency Name")
    )))

      val confirmTermsForm = ClientsMultiInvitationController.confirmTermsMultiForm.fill(
    ConfirmedTerms(itsaConsent = true, afiConsent = true, vatConsent = true))

      val result = controller.submitMultiConfirmTerms("personal", uid)(
    authorisedAsAnyOrganisationClient(FakeRequest())
      .withFormUrlEncodedBody(confirmTermsForm.data.toSeq: _*)
      .withSession("itsaChoice" -> "false", "afiChoice" -> "false", "vatChoice" -> "false", "whichConsent" -> "vat"))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientErrorController.incorrectClientType().url)

      await(testClientConsentsJourneyStateCache.fetch) shouldBe Some(
    ClientConsentsJourneyState(
      Seq(
        ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false),
        ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false),
        ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false)
      ),
      Some("My agency Name")
    ))
    }

  }

  "GET /accept-tax-agent-invitation/confirm-decline/:clientType/:uid (multi confirm decline)" should {
    "show the confirm decline page" in {
      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenAllInvitationIdsByStatus(uid, "Pending")
      givenGetAgencyNameClientStub(arn)

      val result = controller.getMultiConfirmDecline("personal", uid)(authorisedAsAnyIndividualClient(FakeRequest()))

      status(result) shouldBe OK
      checkHtmlResultWithBodyText(result, "Are you sure you want to decline this request?")
      checkHtmlResultWithBodyText(result, "My Agency will not be able to:")
      checkHtmlResultWithBodyText(result, "report your income and expenses through software")
      checkHtmlResultWithBodyText(result, "report your employer PAYE updates through software")
      checkHtmlResultWithBodyText(result, "report your VAT returns through software")
    }

    "redirect to notFound if there are no invitationIds found" in {
      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenAllInvitationIdsByStatusReturnsNotFound(uid, "Pending")
      givenGetAgencyNameClientStub(arn)

      val result = controller.getMultiConfirmDecline("personal", uid)(authorisedAsAnyIndividualClient(FakeRequest()))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
    }

    "throw an AgencyNameNotFound exception if agencyName is not found" in {
      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenAllInvitationIdsByStatus(uid, "Pending")
      givenGetAgencyNameNotFoundClientStub(arn)

      an[AgencyNameNotFound] shouldBe thrownBy {
        await(controller.getMultiConfirmDecline("personal", uid)(authorisedAsAnyIndividualClient(FakeRequest())))
      }
    }

    "throw an exception if the agency record is not found" in {
      givenAgentReferenceRecordNotFoundForUid(uid)
      givenAllInvitationIdsByStatus(uid, "Pending")
      givenGetAgencyNameClientStub(arn)

      an[Exception] shouldBe thrownBy {
        await(controller.getMultiConfirmDecline("personal", uid)(authorisedAsAnyIndividualClient(FakeRequest())))
      }
    }

  }

  "POST /accept-tax-agent-invitation/confirm-decline/:clientType/:uid (multi confirm decline)" should {

    "redirect to multi invitations declined if YES is selected and invitations are successfully declined" in {
      await(
        testClientConsentsJourneyStateCache.save(
          ClientConsentsJourneyState(
            Seq(ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", true)),
            Some("my agency name"))))

      givenInvitationByIdSuccess(InvitationId("AG1UGUKTPNJ7W"), "ABCDEF123456789")
      givenInvitationByIdSuccess(InvitationId("B9SCS2T4NZBAX"), "AB123456A")
      givenInvitationByIdSuccess(InvitationId("CZTW1KY6RTAAT"), "101747696")

      givenRejectInvitationSucceeds("ABCDEF123456789", InvitationId("AG1UGUKTPNJ7W"), "MTDITID")
      givenRejectInvitationSucceeds("AB123456A", InvitationId("B9SCS2T4NZBAX"), "NI")
      givenRejectInvitationSucceeds("101747696", InvitationId("CZTW1KY6RTAAT"), "VRN")

      val confirmForm = confirmDeclineForm.fill(ConfirmForm(Some(true)))

      val result = controller.submitMultiConfirmDecline("personal", uid)(
        authorisedAsAnyIndividualClient(FakeRequest()).withFormUrlEncodedBody(confirmForm.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(
        routes.ClientsMultiInvitationController.getMultiInvitationsDeclined(uid).url)
    }

    "redirect to consent page if NO is selected" in {
      await(
        testClientConsentsJourneyStateCache.save(
          ClientConsentsJourneyState(
            Seq(ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", true)),
            Some("my agency name"))))

      val confirmForm = confirmDeclineForm.fill(ConfirmForm(Some(false)))

      val result = controller.submitMultiConfirmDecline("personal", uid)(
        authorisedAsAnyIndividualClient(FakeRequest()).withFormUrlEncodedBody(confirmForm.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(
        routes.ClientsMultiInvitationController.getMultiConfirmTerms("personal", uid).url)

    }

    "redisplay form with errors if no radio button is selected" in {
      await(
        testClientConsentsJourneyStateCache.save(
          ClientConsentsJourneyState(
            Seq(ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", true)),
            Some("my agency name"))))
      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenAllInvitationIdsByStatus(uid, "Pending")
      givenGetAgencyNameClientStub(arn)

      val result = controller.submitMultiConfirmDecline("personal", uid)(authorisedAsAnyIndividualClient(FakeRequest()))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, "Select yes if you want to decline this request")
    }

    "redirect to wrong-account-type when submitting to decline invitations" in {
      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenAllInvitationIdsByStatus(uid, "Pending")
      givenGetAgencyNameClientStub(arn)

      val result =
        controller.submitMultiConfirmDecline("personal", uid)(authorisedAsAnyOrganisationClient(FakeRequest()))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientErrorController.incorrectClientType().url)
    }

    "redirect to multi invitations declined via failure cases" in {
      await(
        testClientConsentsJourneyStateCache.save(
          ClientConsentsJourneyState(
            Seq(ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", true)),
            Some("my agency name"))))

      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenAllPendingInvitationIdsReturnsFakePending(uid)
      givenGetAgencyNameClientStub(arn)

      givenInvitationByIdSuccess(InvitationId("AG1UGUKTPNJ7W"), "ABCDEF123456789")
      givenInvitationByIdSuccess(InvitationId("B9SCS2T4NZBAX"), "AB123456A")
      givenInvitationByIdSuccess(InvitationId("CZTW1KY6RTAAT"), "101747696")

      givenRejectInvitationSucceeds("ABCDEF123456789", InvitationId("AG1UGUKTPNJ7W"), "MTDITID")
      givenRejectInvitationSucceeds("AB123456A", InvitationId("B9SCS2T4NZBAX"), "NI")
      givenRejectInvitationSucceeds("101747696", InvitationId("CZTW1KY6RTAAT"), "VRN")

      val confirmForm = confirmDeclineForm.fill(ConfirmForm(Some(true)))

      val result = controller.submitMultiConfirmDecline("personal", uid)(
        authorisedAsAnyClientFalse(FakeRequest()).withFormUrlEncodedBody(confirmForm.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(
        routes.ClientsMultiInvitationController.getMultiInvitationsDeclined(uid).url)
    }

    "throw a Illegal State Exception if there is nothing in the cache" in {

      val result = controller.submitMultiConfirmDecline("personal", uid)(authorisedAsAnyIndividualClient(FakeRequest()))

      an[IllegalStateException] shouldBe thrownBy {
        await(result)
      }
    }

    "throw an Exception if there is no agency name in the cache" in {
      await(testClientConsentsJourneyStateCache.save(
        ClientConsentsJourneyState(Seq(ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", true)), None)))

      val result = controller.submitMultiConfirmDecline("personal", uid)(authorisedAsAnyClientFalse(FakeRequest()))

      an[Exception] shouldBe thrownBy {
        await(result)
      }

    }
  }

  "GET /accept-tax-agent-invitation/check-answers" should {
    "show the check answers page" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = true),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true)
          ),
          Some("My agency Name")
        )))

      val result = controller.showCheckAnswers(personal.get, uid)(authorisedAsAnyIndividualClient(FakeRequest()))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Check your answers before sending your response",
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

    "throw a Illegal State Exception if there is nothing in the cache" in {
      await(testClientConsentsJourneyStateCache.clear())

      val result = controller.showCheckAnswers(personal.get, uid)(authorisedAsAnyIndividualClient(FakeRequest()))

      an[IllegalStateException] shouldBe thrownBy {
        await(result)
      }
    }

    "throw an Exception if tehre is no agency name in the cache" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = true),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true)
          ),
          None
        )))

      val result = controller.showCheckAnswers(personal.get, uid)(authorisedAsAnyIndividualClient(FakeRequest()))

      an[Exception] shouldBe thrownBy {
        await(result)
      }
    }
  }

  "GET /accept-tax-agent-invitation/consent/:clientType/:uid/:givenServiceKey  (multi confirm terms individual)" should {

    "show the multi confirm terms page for only itsa service" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = true),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true)
          ),
          Some("My Agency Name")
        )))

      val result = controller.getMultiConfirmTermsIndividual(personal.get, uid, "itsa")(
        authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "We need your consent to share information",
        "Report my income and expenses through software",
        "I consent to HMRC sharing the following information with My Agency Name, so this agent can report my income and expenses through software:",
        "my income and expenses information"
      )
      checkHtmlResultWithNotBodyText(result, "View your PAYE income record", "Submit my VAT returns through software")
    }

    "show the multi confirm terms page for only itsa service when coming from check answers" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = true),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true)
          ),
          Some("My Agency Name")
        )))

      val result = controller.getMultiConfirmTermsIndividual(personal.get, uid, "itsa")(
        authorisedAsAnyIndividualClient(FakeRequest()
          .withHeaders(
            "Referer" -> s"someBaseUrl${routes.ClientsMultiInvitationController.showCheckAnswers("personal", uid).url}")))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Change your consent",
        "Report my income and expenses through software",
        "I consent to HMRC sharing the following information with My Agency Name, so this agent can report my income and expenses through software:",
        "my income and expenses information"
      )
      checkHtmlResultWithNotBodyText(result, "View your PAYE income record", "Submit my VAT returns through software")
    }

    "show the multi confirm terms page for only irv service" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = true),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true)
          ),
          Some("My Agency Name")
        )))

      val result = controller.getMultiConfirmTermsIndividual(personal.get, uid, "afi")(
        authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "View your PAYE income record",
        "I consent to HMRC sharing the following information with My Agency Name, so this agent can view my PAYE income record:",
        "who I have worked for in the past"
      )
      checkHtmlResultWithNotBodyText(
        result,
        "Report my income and expenses through software",
        "Submit my VAT Returns through software")
    }

    "show the multi confirm terms page for only vat service" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = true),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true)
          ),
          Some("My Agency Name")
        )))

      val result = controller.getMultiConfirmTermsIndividual(personal.get, uid, "vat")(
        authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Submit my VAT Returns through software",
        "I consent to HMRC sharing the following information with My Agency Name, so this agent can submit my VAT Returns through software:",
        "my VAT registration details, such as business name and contact details"
      )
      checkHtmlResultWithNotBodyText(
        result,
        "View your PAYE income record",
        "Report my income and expenses through software")
    }

    "return Invalid Journey State when there is nothing in the cache" in {
      await(testClientConsentsJourneyStateCache.clear())
      val result = controller.getMultiConfirmTermsIndividual(personal.get, uid, "itsa")(
        authorisedAsAnyIndividualClient(FakeRequest()))

      an[IllegalStateException] shouldBe thrownBy {
        await(result)
      }
    }

    "return Invalid Journey State if the chosen consent is empty" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true)
          ),
          Some("My Agency Name")
        )))

      val result = controller.getMultiConfirmTermsIndividual(personal.get, uid, "itsa")(
        authorisedAsAnyIndividualClient(FakeRequest()))

      an[BadRequestException] shouldBe thrownBy {
        await(result)
      }
    }
  }

  "GET /reject-tax-agent-invitation/declined/uid/:uid (multi invitations declined)" should {

    "show the multi invitations declined page for personal" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = true),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true)
          ),
          Some("My Agency Name")
        )))

      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenAllInvitationIdsByStatus(uid, "Rejected")
      givenGetAgencyNameClientStub(arn)

      val result = controller.getMultiInvitationsDeclined(uid)(authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "You declined this request",
        "You have not given permission to My Agency to:",
        "report your income and expenses through software",
        "report your employer PAYE updates through software",
        "report your VAT returns through software"
      )
    }

    "show the multi invitations declined page for multi ITSA Invitation" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = true),
            ClientConsent(InvitationId("AG1UGUKTPNJ7Z"), expiryDate, "itsa", consent = true),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true)
          ),
          Some("My Agency Name")
        )))

      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenAllInvitationIdsByStatusReturnsSomeDuplicated(uid, "Rejected")
      givenGetAgencyNameClientStub(arn)

      val result = controller.getMultiInvitationsDeclined(uid)(authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "You declined this request",
        "You have not given permission to My Agency to:",
        "report your income and expenses through software",
        "report your VAT returns through software"
      )
    }

    "show the multi invitations declined page when there is just one invitation" in {
      await(
        testClientConsentsJourneyStateCache.save(
          ClientConsentsJourneyState(
            Seq(ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = true)),
            Some("My Agency Name"))))

      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenAllInvitationIdsByStatus(uid, "Rejected")
      givenGetAgencyNameClientStub(arn)

      val result = controller.getMultiInvitationsDeclined(uid)(authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "You declined this request",
        "You have not given permission to My Agency to report your income and expenses through software")
      checkHtmlResultWithNotBodyText(result, "You have not given permission to {0} to:")
    }

    "throw a Illegal State Exception if there is nothing in the cache" in {
      await(testClientConsentsJourneyStateCache.clear())

      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenAllInvitationIdsByStatus(uid, "Rejected")
      givenGetAgencyNameClientStub(arn)

      val result = controller.getMultiInvitationsDeclined(uid)(authorisedAsAnyIndividualClient(FakeRequest()))

      an[IllegalStateException] shouldBe thrownBy {
        await(result)
      }

    }

    "redirect to notFound if there are no invitationIds found" in {
      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenAllInvitationIdsByStatusReturnsNotFound(uid, "Rejected")
      givenGetAgencyNameClientStub(arn)

      val result = controller.getMultiInvitationsDeclined(uid)(authorisedAsAnyIndividualClient(FakeRequest()))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsInvitationController.notFoundInvitation().url)
    }

    "redirect to wrong-account-type if signed-in as business for personal invitation" in {
      givenAllInvitationIdsByStatusReturnsNotFound(uid, "Pending")

      val result = controller.getMultiConfirmDecline("personal", uid)(authorisedAsAnyOrganisationClient(FakeRequest()))
      status(result) shouldBe 303

      redirectLocation(result) shouldBe Some(routes.ClientErrorController.incorrectClientType().url)
    }

    "redirect to wrong-account-type if signed-in as personal for business invitation" in {
      givenAllInvitationIdsByStatusReturnsNotFound(uid, "Pending")

      val result = controller.getMultiConfirmDecline("business", uid)(authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 303

      redirectLocation(result) shouldBe Some(routes.ClientErrorController.incorrectClientType().url)
    }

    "Throw an AgencyNameNotFound exception if agencyName is not found" in {
      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenAllInvitationIdsByStatus(uid, "Rejected")
      givenGetAgencyNameNotFoundClientStub(arn)

      an[AgencyNameNotFound] shouldBe thrownBy {
        await(controller.getMultiInvitationsDeclined(uid)(authorisedAsAnyIndividualClient(FakeRequest())))
      }
    }
  }

  "POST /accept-tax-agent-invitation/submit-answers/:clientType/:uid" should {
    "redirect to accepted invitation page for accepting all invitations" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = true),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true)
          ),
          Some("My Agency Name")
        )))

      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenAllInvitationIdsByStatus(uid, "Pending")
      givenInvitationByIdSuccess(InvitationId("AG1UGUKTPNJ7W"), "ABCDEF123456789")
      givenInvitationByIdSuccess(InvitationId("B9SCS2T4NZBAX"), "AB123456A")
      givenInvitationByIdSuccess(InvitationId("CZTW1KY6RTAAT"), "101747696")
      givenAcceptInvitationSucceeds("ABCDEF123456789", InvitationId("AG1UGUKTPNJ7W"), identifierITSA)
      givenAcceptInvitationSucceeds("AB123456A", InvitationId("B9SCS2T4NZBAX"), identifierPIR)
      givenAcceptInvitationSucceeds("101747696", InvitationId("CZTW1KY6RTAAT"), identifierVAT)

      val result = controller.submitAnswers(uid)(authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsMultiInvitationController.invitationAccepted().url)
    }

    "redirect to accepted invitation page for accepting some invitations" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = true),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true)
          ),
          Some("My Agency Name")
        )))

      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenAllInvitationIdsByStatus(uid, "Pending")
      givenInvitationByIdSuccess(InvitationId("AG1UGUKTPNJ7W"), "ABCDEF123456789")
      givenInvitationByIdSuccess(InvitationId("B9SCS2T4NZBAX"), "AB123456A")
      givenInvitationByIdSuccess(InvitationId("CZTW1KY6RTAAT"), "101747696")
      givenAcceptInvitationSucceeds("ABCDEF123456789", InvitationId("AG1UGUKTPNJ7W"), identifierITSA)
      givenRejectInvitationSucceeds("AB123456A", InvitationId("B9SCS2T4NZBAX"), identifierPIR)
      givenAcceptInvitationSucceeds("101747696", InvitationId("CZTW1KY6RTAAT"), identifierVAT)

      val result = controller.submitAnswers(uid)(authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsMultiInvitationController.invitationAccepted.url)
    }

    "redirect to declined invitation page for rejecting all invitations" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = false),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false)
          ),
          Some("My Agency Name")
        )))

      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenAllInvitationIdsByStatus(uid, "Pending")
      givenInvitationByIdSuccess(InvitationId("AG1UGUKTPNJ7W"), "ABCDEF123456789")
      givenInvitationByIdSuccess(InvitationId("B9SCS2T4NZBAX"), "AB123456A")
      givenInvitationByIdSuccess(InvitationId("CZTW1KY6RTAAT"), "101747696")
      givenRejectInvitationSucceeds("ABCDEF123456789", InvitationId("AG1UGUKTPNJ7W"), identifierITSA)
      givenRejectInvitationSucceeds("AB123456A", InvitationId("B9SCS2T4NZBAX"), identifierPIR)
      givenRejectInvitationSucceeds("101747696", InvitationId("CZTW1KY6RTAAT"), identifierVAT)

      val result = controller.submitAnswers(uid)(authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(
        routes.ClientsMultiInvitationController.getMultiInvitationsDeclined(uid).url)
    }

    "redirect to showSomeResponsesFailed when some of acceptance has failed" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = true),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true)
          ),
          Some("My Agency Name")
        )))

      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenAllInvitationIdsByStatus(uid, "Pending")
      givenInvitationByIdSuccess(InvitationId("AG1UGUKTPNJ7W"), "ABCDEF123456789")
      givenInvitationByIdSuccess(InvitationId("B9SCS2T4NZBAX"), "AB123456A")
      givenInvitationByIdSuccess(InvitationId("CZTW1KY6RTAAT"), "101747696")
      givenAcceptInvitationReturnsNotFound("ABCDEF123456789", InvitationId("AG1UGUKTPNJ7W"), identifierITSA)
      givenAcceptInvitationSucceeds("AB123456A", InvitationId("B9SCS2T4NZBAX"), identifierPIR)
      givenAcceptInvitationReturnsNotFound("101747696", InvitationId("CZTW1KY6RTAAT"), identifierVAT)

      val result = controller.submitAnswers(uid)(authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsMultiInvitationController.showSomeResponsesFailed().url)
    }

    "redirect to allResponsesFailed when all of the acceptance has failed" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = true),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true)
          ),
          Some("My Agency Name")
        )))

      givenAgentReferenceRecordExistsForUid(arn, uid)
      givenAllInvitationIdsByStatus(uid, "Pending")
      givenInvitationByIdSuccess(InvitationId("AG1UGUKTPNJ7W"), "ABCDEF123456789")
      givenInvitationByIdSuccess(InvitationId("B9SCS2T4NZBAX"), "AB123456A")
      givenInvitationByIdSuccess(InvitationId("CZTW1KY6RTAAT"), "101747696")
      givenAcceptInvitationReturnsNotFound("ABCDEF123456789", InvitationId("AG1UGUKTPNJ7W"), identifierITSA)
      givenAcceptInvitationReturnsNotFound("AB123456A", InvitationId("B9SCS2T4NZBAX"), identifierPIR)
      givenAcceptInvitationReturnsNotFound("101747696", InvitationId("CZTW1KY6RTAAT"), identifierVAT)

      val result = controller.submitAnswers(uid)(authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.ClientsMultiInvitationController.showAllResponsesFailed().url)
    }

    "throw an exception when the cache is empty" in {
      await(testClientConsentsJourneyStateCache.clear())

      val result = controller.submitAnswers(uid)(authorisedAsAnyIndividualClient(FakeRequest()))
      an[IllegalStateException] shouldBe thrownBy {
        await(result)
      }

    }
  }

  "GET /accept-tax-agent-invitation/accepted/:clientType/:uid" should {
    "show accepted invitation page when accepting all invitations" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = true, processed = true),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true, processed = true),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = true, processed = true)
          ),
          Some("My Agency Name")
        )))

      val result = controller.invitationAccepted(authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Approval complete",
        "My Agency Name can now deal with HMRC for you",
        "reporting your income and expenses through software",
        "viewing your income record",
        "reporting your VAT returns through software"
      )
      checkHtmlResultWithNotBodyText(result, "Made a mistake?")
    }

    "show accepted invitation page when some invitations are accepted and some are rejected" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false, processed = true),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true, processed = true),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false, processed = true)
          ),
          Some("My Agency Name")
        )))

      val result = controller.invitationAccepted(authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Approval complete",
        "My Agency Name can now deal with HMRC for you",
        "My Agency Name is now confirmed as your authorised tax agent for viewing your income record.",
        "Made a mistake?",
        "You did not appoint My Agency Name for 2 services. Contact the person who sent you the request if you declined it by mistake."
      )
      checkHtmlResultWithNotBodyText(
        result,
        "reporting your income and expenses through software",
        "reporting your VAT returns through software")
    }

    "show different content for made a mistake if only one service is declined" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true, processed = true),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false, processed = true)
          ),
          Some("My Agency Name")
        )))

      val result = controller.invitationAccepted(authorisedAsAnyIndividualClient(FakeRequest()))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Approval complete",
        "My Agency Name can now deal with HMRC for you",
        "My Agency Name is now confirmed as your authorised tax agent for viewing your income record.",
        "Made a mistake?",
        "You did not appoint My Agency Name for 1 service. Contact the person who sent you the request if you declined it by mistake."
      )
      checkHtmlResultWithNotBodyText(
        result,
        "reporting your income and expenses through software",
        "reporting your VAT returns through software")
    }

    "throw an Illegal State Exception when the cache is empty" in {
      await(testClientConsentsJourneyStateCache.clear())

      val result = controller.invitationAccepted(authorisedAsAnyIndividualClient(FakeRequest()))
      an[IllegalStateException] shouldBe thrownBy {
        await(result)
      }
    }

    "thrown an AgencyNameNotFound exception when there is no agency name in the cache" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false)
          ),
          None
        )))

      val result = controller.invitationAccepted(authorisedAsAnyIndividualClient(FakeRequest()))

      an[Exception] shouldBe thrownBy {
        await(result)
      }
    }
  }

  "GET /some-responses-failed (some responses failed)" should {
    "show the some responses failed page when some responses have failed" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true, processed = true),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false)
          ),
          Some("My Agency Name")
        )))

      val result = controller.showSomeResponsesFailed(authorisedAsAnyIndividualClient(FakeRequest()))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Sorry, some of your responses could not be saved",
        "We could not save your response for the following services:",
        "Report your income and expenses through software",
        "Report your VAT Returns through software",
        "You can view which services you appointed My Agency Name to deal with."
      )
    }

    "throw a bad request exception if there are only successful invitations being passed through" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false, processed = true),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true, processed = true),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false, processed = true)
          ),
          Some("My Agency Name")
        )))

      val result = controller.showSomeResponsesFailed(authorisedAsAnyIndividualClient(FakeRequest()))

      an[BadRequestException] shouldBe thrownBy {
        await(result)
      }
    }

    "throw a bad request exception if there are only unsuccessful invitations being passed through" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false)
          ),
          Some("My Agency Name")
        )))

      val result = controller.showSomeResponsesFailed(authorisedAsAnyIndividualClient(FakeRequest()))

      an[BadRequestException] shouldBe thrownBy {
        await(result)
      }
    }

    "throw an exception when there is no agency name in the cache" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true, processed = true),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false)
          ),
          None
        )))

      val result = controller.showSomeResponsesFailed(authorisedAsAnyIndividualClient(FakeRequest()))

      an[Exception] shouldBe thrownBy {
        await(result)
      }
    }
  }

  "GET /all-responses-failed (all responses failed)" should {
    "show the all responses failed page when all responses have failed" in {
      await(
        testClientConsentsJourneyStateCache.save(ClientConsentsJourneyState(
          Seq(
            ClientConsent(InvitationId("AG1UGUKTPNJ7W"), expiryDate, "itsa", consent = false),
            ClientConsent(InvitationId("B9SCS2T4NZBAX"), expiryDate, "afi", consent = true),
            ClientConsent(InvitationId("CZTW1KY6RTAAT"), expiryDate, "vat", consent = false)
          ),
          Some("My Agency Name")
        )))

      val result = controller.showAllResponsesFailed(authorisedAsAnyIndividualClient(FakeRequest()))

      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Sorry, there is a problem with the service",
        "We could not save your responses. Please try again in 24 hours.")
    }
  }

}
