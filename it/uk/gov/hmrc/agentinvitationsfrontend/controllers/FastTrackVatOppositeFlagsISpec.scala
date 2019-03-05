package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.util.UUID

import org.joda.time.LocalDate
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsFastTrackInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.forms.ClientTypeForm
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentFastTrackRequest, AgentSession}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

class FastTrackVatOppositeFlagsISpec extends BaseISpec {

  import scala.concurrent.duration._
  override implicit val defaultTimeout = 35 seconds

  override protected def appBuilder: GuiceApplicationBuilder =
    new GuiceApplicationBuilder()
      .configure(
        "microservice.services.auth.port"                                     -> wireMockPort,
        "microservice.services.agent-client-authorisation.port"               -> wireMockPort,
        "microservice.services.agent-client-relationships.port"               -> wireMockPort,
        "microservice.services.agent-services-account.port"                   -> wireMockPort,
        "microservice.services.company-auth.login-url"                        -> wireMockHost,
        "microservice.services.company-auth.port"                             -> wireMockPort,
        "microservice.services.des.port"                                      -> wireMockPort,
        "microservice.services.agent-fi-relationship.port"                    -> wireMockPort,
        "microservice.services.citizen-details.host"                          -> wireMockHost,
        "microservice.services.citizen-details.port"                          -> wireMockPort,
        "microservice.services.agent-invitations-frontend.external-url"       -> wireMockBaseUrlAsString,
        "microservice.services.agent-services-account-frontend.external-url"  -> wireMockBaseUrlAsString,
        "microservice.services.company-auth-frontend.external-url"            -> companyAuthUrl,
        "microservice.services.company-auth-frontend.sign-out.path"           -> companyAuthSignOutPath,
        "microservice.services.business-tax-account.external-url"             -> businessTaxAccountUrl,
        "microservice.services.tax-account-router-frontend.account-url"       -> taxAccountRelativeUrl,
        "microservice.services.personal-tax-account.external-url"             -> personalTaxAccountUrl,
        "auditing.enabled"                                                    -> true,
        "auditing.consumer.baseUri.host"                                      -> wireMockHost,
        "auditing.consumer.baseUri.port"                                      -> wireMockPort,
        "features.show-hmrc-mtd-it"                                           -> true,
        "features.show-personal-income"                                       -> true,
        "features.show-hmrc-mtd-vat"                                          -> true,
        "features.show-kfc-mtd-it"                                            -> false,
        "features.show-kfc-personal-income"                                   -> false,
        "features.show-kfc-mtd-vat"                                           -> false,
        "features.enable-fast-track"                                          -> true,
        "features.redirect-to-confirm-personal-income"                        -> true,
        "features.redirect-to-confirm-mtd-it"                                 -> false,
        "features.redirect-to-confirm-mtd-vat"                                -> false,
        "microservice.services.agent-subscription-frontend.external-url"      -> "someSubscriptionExternalUrl",
        "microservice.services.agent-client-management-frontend.external-url" -> "someAgentClientManagementFrontendExternalUrl",
        "mongodb.uri" -> s"$mongoUri"
      )

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  lazy val fastTrackController: AgentsFastTrackInvitationController = app.injector.instanceOf[AgentsFastTrackInvitationController]
  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId(UUID.randomUUID().toString)))

  val agentSession = AgentSession(Some(business), Some(serviceVAT), Some("vrn"), Some(validVrn.value), Some(validRegistrationDate), fromFastTrack = fromFastTrack)

  "POST /agents/fast-track" should {
    val request = FakeRequest(
      "POST",
      "/agents/fast-track?continue=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fselect-client&error=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fnot-authorised"
    )
    val fastTrack = fastTrackController.agentFastTrack()

    "return 303 check-details if service calling fast-track is correct for VAT" in {
      val formData =
        AgentFastTrackRequest(Some(business), serviceVAT, "vrn", validVrn.value, Some(validRegistrationDate))
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result).get shouldBe routes.AgentsFastTrackInvitationController.showCheckDetails().url
    }

    "return 303 check-details if service calling fast-track does not contain vat-reg-date for VAT" in {
      val formData =
        AgentFastTrackRequest(Some(business), serviceVAT, "vrn", validVrn.value, None)
      val fastTrackFormData = agentFastTrackForm.fill(formData)

      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsFastTrackInvitationController.showCheckDetails().url)
    }

    "return 303 check-details if service calling fast-track contains invalid vat-reg-date for VAT" in {
      val formData = AgentFastTrackRequest(Some(business), serviceVAT, "vrn", validVrn.value, Some("invalid_reg_date"))
      val fastTrackFormData = agentFastTrackForm.fill(formData)

      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsFastTrackInvitationController.showCheckDetails().url)
    }

    "return 303 check-details if service calling fast-track does not contain client type" in {
      val formData =
        AgentFastTrackRequest(None, serviceVAT, "vrn", validVrn.value, Some(validRegistrationDate))
      val fastTrackFormData = agentFastTrackForm.fill(formData)

      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentsFastTrackInvitationController.showCheckDetails().url)
    }

    "return 303 and redirect to error url if service calling fast-track for VAT contains invalid vrn" in {
      val formData =  AgentFastTrackRequest(Some(business), serviceVAT, "", "INVALID_VRN", Some(validRegistrationDate))
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe
        Some(
          "http://localhost:9996/tax-history/not-authorised?issue=UNSUPPORTED_CLIENT_ID_TYPE INVALID_CLIENT_ID_RECEIVED:INVALID_VRN")
    }

    "return 303 and redirect to error url if service calling fast-track for VAT does not contain vrn" in {
      val formData = AgentFastTrackRequest(Some(business), serviceVAT)
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe
        Some(
          "http://localhost:9996/tax-history/not-authorised?issue=UNSUPPORTED_CLIENT_ID_TYPE INVALID_CLIENT_ID_RECEIVED:NOTHING")
    }

    "return 303 and redirect to error url if there is no service but all other fields are valid for VAT" in {
      val formData =
        AgentFastTrackRequest(Some(business), "", "vrn", validVrn.value, Some(validRegistrationDate))
      val fastTrackFormData = agentFastTrackForm.fill(formData)
      val result = fastTrack(
        authorisedAsValidAgent(request, arn.value)
          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

      status(result) shouldBe SEE_OTHER
      redirectLocation(result) shouldBe
        Some("http://localhost:9996/tax-history/not-authorised?issue=UNSUPPORTED_SERVICE")
    }

//    "return 303 and redirect to error url if there is no client-type but all other fields are valid for VAT" in {
//      val formData =
//        CurrentInvitationInput(None, serviceVAT, "vrn", validVrn.value, Some(validRegistrationDate), fromFastTrack)
//      val fastTrackFormData = agentFastTrackForm.fill(formData)
//      val result = fastTrack(
//        authorisedAsValidAgent(request, arn.value)
//          .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))
//
//      status(result) shouldBe SEE_OTHER
//      redirectLocation(result) shouldBe
//        Some("http://localhost:9996/tax-history/not-authorised?issue=UNSUPPORTED_CLIENT_TYPE")
//    }
  }

  "GET /agents/check-details" should {

    val request = FakeRequest()
    "display the check details page when known fact is not required and provided for VAT" in {
      await(sessionStore.save(agentSession))
      val result = await(fastTrackController.showCheckDetails(authorisedAsValidAgent(request,    arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("report a client's VAT returns through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("VAT registration number"))
      checkHtmlResultWithBodyText(result, validVrn.value)
    }

    "display alternate check details page when known fact is not required and not provided for VAT" in {
      await(sessionStore.save(agentSession.copy(knownFact = None, fromFastTrack = fromFastTrack)))
      val result = await(fastTrackController.showCheckDetails(authorisedAsValidAgent(request,    arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("report a client's VAT returns through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("VAT registration number"))
      checkHtmlResultWithBodyText(result, validVrn.value)
    }

    "display alternate check details page when client-type is required and not provided for VAT" in {
      await(sessionStore.save(agentSession.copy(clientType = None)))
      val result = await(fastTrackController.showCheckDetails(authorisedAsValidAgent(request,    arn.value)))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("Check your client's details before you continue"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("report a client's VAT returns through software"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("VAT registration number"))
      checkHtmlResultWithBodyText(result, validVrn.value)
    }
  }

  "POST /agents/check-details" should {
    val request = FakeRequest()

    "redirect to confirm_invitation when YES is selected for VAT service" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)
      givenInvitationCreationSucceeds(
        arn,
        Some(business),
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        "VRN")
      givenAgentReference(arn, "BBBBBBBB", business)
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 200)
      givenAgentReferenceRecordExistsForArn(arn, "uid")

      await(sessionStore.save(agentSession.copy(clientType = Some(business))))

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request,    arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "render confirm_invitations when client type is provided, known fact is not required and YES is selected for VAT service" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)
      givenInvitationCreationSucceeds(
        arn,
        Some(personal),
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        "VRN")
      givenAgentReference(arn, "BBBBBBBB", personal)
      givenAgentReferenceRecordExistsForArn(arn, "uid")

      await(sessionStore.save(agentSession.copy(clientType = Some(personal), fromFastTrack = fromFastTrack)))

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request,    arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "redirect to client-type when client type is not provided and YES is selected for VAT service" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)
      givenInvitationCreationSucceeds(
        arn,
        Some(business),
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        "VRN")
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 200)

      await(sessionStore.save(agentSession.copy(clientType = None, fromFastTrack = fromFastTrack)))

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request,    arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/client-type")
    }

    "redirect to client-type when client type and known fact are not provided and YES is selected for VAT service" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)

      await(sessionStore.save(agentSession.copy(clientType = None, knownFact = None, fromFastTrack = fromFastTrack)))

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request,    arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/client-type")
    }

    "redirect to client-type when not provided and then submit and redirect to complete" in {
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)
      givenInvitationCreationSucceeds(
        arn,
        Some(personal),
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        "VRN")
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 200)
      givenAgentReference(arn, "BBBBBBBB", personal)
      givenAgentReferenceRecordExistsForArn(arn, "uid")

      await(sessionStore.save(agentSession.copy(clientType = None, fromFastTrack = fromFastTrack)))

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request,    arn.value).withFormUrlEncodedBody("checkDetails" -> "true")))

      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/client-type")

      val serviceForm = ClientTypeForm.form.fill(personal)

      val result2 = await(
        controller.submitClientType(
          authorisedAsValidAgent(request.withFormUrlEncodedBody(serviceForm.data.toSeq: _*),    arn.value)))
      status(result2) shouldBe 303
      redirectLocation(result2) shouldBe Some("/invitations/agents/invitation-sent")
    }

    "redirect to identify-client when NO is selected for VAT service" in {
      givenInvitationCreationSucceeds(
        arn,
        Some(business),
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        "VRN")
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 200)

      await(sessionStore.save(agentSession.copy(fromFastTrack = fromFastTrack)))

      val result = await(
        fastTrackController.submitCheckDetails(
          authorisedAsValidAgent(request,    arn.value).withFormUrlEncodedBody("checkDetails" -> "false")))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/identify-client")
    }
  }

  "GET /agents/more-details" should {
    val request = FakeRequest()
    "display the known fact page when known fact is required and provided for VAT" in {

      await(sessionStore.save(agentSession.copy(clientType = Some(business), fromFastTrack = fromFastTrack, knownFact = None)))
      val result = await(fastTrackController.showKnownFact(authorisedAsValidAgent(request,    arn.value)))
      checkHtmlResultWithBodyText(result, "What is your client's VAT registration date?")
      checkHtmlResultWithBodyText(
        result,
        htmlEscapedMessage("This will help us match their details against information we hold."))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("For example, 31 8 2015"))
    }
  }

  "POST /agents/more-details" should {
    val request = FakeRequest("POST", "/agents/more-details")
    "redirect to invitation sent when client details are valid and match for VAT" in {
      givenInvitationCreationSucceeds(
        arn,
        Some(business),
        validVrn.value,
        invitationIdVAT,
        validVrn.value,
        "vrn",
        serviceVAT,
        identifierVAT)
      givenAgentReference(arn, "BBBBBBBB", business)
      givenVatRegisteredClientReturns(validVrn, LocalDate.parse(Some(validRegistrationDate).get), 204)
      givenGetAllPendingInvitationsReturnsEmpty(arn, validVrn.value, serviceVAT)
      givenCheckRelationshipVatWithStatus(arn, validVrn.value, 404)
      givenAgentReferenceRecordExistsForArn(arn, "uid")

      val requestWithForm = request.withFormUrlEncodedBody(
        "knownFact.year"       -> "2007",
        "knownFact.month"      -> "07",
        "knownFact.day"        -> "07"
      )

      await(sessionStore.save(agentSession))

      val result = await(fastTrackController.submitKnownFact(authorisedAsValidAgent(requestWithForm,    arn.value)))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
    }
  }

}
