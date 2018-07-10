package uk.gov.hmrc.agentinvitationsfrontend.controllers

import play.api.mvc.{Action, AnyContent, AnyContentAsEmpty}
import play.api.test.FakeRequest
import play.api.test.Helpers.redirectLocation
import uk.gov.hmrc.agentinvitationsfrontend.models.{Confirmation, CurrentInvitationInput, UserInputNinoAndPostcode}
import uk.gov.hmrc.agentinvitationsfrontend.support.{BaseISpec, TestDataCommonSupport}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController._

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationsITSAControllerJourneyISpec extends BaseISpec with AuthBehaviours with TestDataCommonSupport {

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session12345")))


//  "GET /confirm-client" should {
//    val request = FakeRequest("GET", "/agents/confirm-client")
//    val showConfirmClient = controller.showConfirmClient()
//
//    "return 200 and show client name" in {
//      testFastTrackCache.save(
//        CurrentInvitationInput(
//          Some(serviceITSA),
//          Some("ni"),
//          Some(validNino.value),
//          Some(validPostcode),
//          None,
//          fromFastTrack))
//      givenTradingName(validNino, "64 Bit")
//      val result = showConfirmClient(authorisedAsValidAgent(request, arn.value))
//      status(result) shouldBe 200
//      checkHtmlResultWithBodyText(result, "64 Bit")
//      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")
//      checkHtmlResultWithBodyMsgs(result, "confirm-client.yes")
//      checkHtmlResultWithBodyMsgs(result, "confirm-client.no")
//    }
//
//    "return 200 and no client name was found" in {
//      testFastTrackCache.save(
//        CurrentInvitationInput(
//          Some(serviceITSA),
//          Some("ni"),
//          Some(validNino.value),
//          Some(validPostcode),
//          None,
//          fromFastTrack))
//      givenTradingNameMissing(validNino)
//      val result = showConfirmClient(authorisedAsValidAgent(request, arn.value))
//      status(result) shouldBe 200
//      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")
//      checkHtmlResultWithBodyMsgs(result, "confirm-client.yes")
//      checkHtmlResultWithBodyMsgs(result, "confirm-client.no")
//    }
//
//    behaveLikeMissingCacheScenarios(showConfirmClient, request)
//  }
//
//  "POST /confirm-client" should {
//    val request = FakeRequest("POST", "/agents/confirm-client")
//    val submitConfirmClient = controller.submitConfirmClient()
//
//    "redirect to invitation-sent" in {
//      testFastTrackCache.save(
//        CurrentInvitationInput(
//          Some(serviceITSA),
//          Some("ni"),
//          Some(validNino.value),
//          Some(validPostcode),
//          None,
//          fromFastTrack))
//      createInvitationStubWithKnownFacts(
//        arn,
//        mtdItId.value,
//        invitationIdITSA,
//        validNino.value,
//        serviceITSA,
//        "NI",
//        Some(validPostcode))
//      givenTradingName(validNino, "64 Bit")
//      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, "NI", "Pending")
//      val choice = agentConfirmClientForm.fill(Confirmation(true))
//      val result =
//        submitConfirmClient(authorisedAsValidAgent(request, arn.value).withFormUrlEncodedBody(choice.data.toSeq: _*))
//      redirectLocation(result) shouldBe Some("/invitations/agents/invitation-sent")
//      status(result) shouldBe 303
//    }
//
//    "return 200 for not selecting an option" in {
//      testFastTrackCache.save(
//        CurrentInvitationInput(
//          Some(serviceITSA),
//          Some("ni"),
//          Some(validNino.value),
//          Some(validPostcode),
//          None,
//          fromFastTrack))
//      givenTradingName(validNino, "64 Bit")
//      val result = submitConfirmClient(authorisedAsValidAgent(request, arn.value))
//      status(result) shouldBe 200
//      givenTradingName(validNino, "64 Bit")
//      checkHtmlResultWithBodyMsgs(result, "error.confirm-client.required")
//      checkHtmlResultWithBodyMsgs(result, "confirm-client.header")
//      checkHtmlResultWithBodyMsgs(result, "confirm-client.yes")
//      checkHtmlResultWithBodyMsgs(result, "confirm-client.no")
//    }
//
//    behaveLikeMissingCacheScenarios(submitConfirmClient, request)
//  }
//
//  def behaveLikeMissingCacheScenarios(action: Action[AnyContent], request: FakeRequest[AnyContentAsEmpty.type]) = {
//    "return to identify-client no client identifier found in cache" in {
//      testFastTrackCache.save(CurrentInvitationInput(Some(serviceITSA), None, None, None, None, fromFastTrack))
//      val result = action(authorisedAsValidAgent(request, arn.value))
//      status(result) shouldBe 303
//      redirectLocation(result) shouldBe Some("/invitations/agents/identify-client")
//    }
//
//    "return to select-service for no cache" in {
//      val result = action(authorisedAsValidAgent(request, arn.value))
//      status(result) shouldBe 303
//      redirectLocation(result) shouldBe Some("/invitations/agents/select-service")
//    }
//  }
}
