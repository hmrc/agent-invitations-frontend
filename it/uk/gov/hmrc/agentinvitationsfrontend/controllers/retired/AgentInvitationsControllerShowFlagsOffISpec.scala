package uk.gov.hmrc.agentinvitationsfrontend.controllers.retired

import java.util.UUID

import play.api.Application
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.retired.AgentsFastTrackInvitationController.agentFastTrackForm
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentFastTrackRequest
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

class AgentInvitationsControllerShowFlagsOffISpec extends BaseISpec {

  override implicit lazy val app: Application = appBuilder(oppositeFeatureFlags)
    .build()

  lazy val fastTrackController: AgentsFastTrackInvitationController =
    app.injector.instanceOf[AgentsFastTrackInvitationController]
  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId(UUID.randomUUID().toString)))

  "Show Feature Flags are switched off" should {
    val request = FakeRequest("POST", "/agents/fast-track")
    val fastTrack = fastTrackController.agentFastTrack()

    "through fast-track, return 400 and prevent agents" when {

      "creating an ITSA invitation" in {
        val formData =
          AgentFastTrackRequest(Some(personal), serviceITSA, "ni", nino, Some(validPostcode))
        val fastTrackFormData = agentFastTrackForm.fill(formData)
        val result = fastTrack(
          authorisedAsValidAgent(request, arn.value)
            .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

        status(result) shouldBe BAD_REQUEST
      }

      "creating an IRV invitation" in {
        val formData = AgentFastTrackRequest(Some(personal), servicePIR, "ni", nino, Some(validPostcode))
        val fastTrackFormData = agentFastTrackForm.fill(formData)
        val result = fastTrack(
          authorisedAsValidAgent(request, arn.value)
            .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

        status(result) shouldBe BAD_REQUEST
      }

      "creating an VAT invitation" in {
        val formData =
          AgentFastTrackRequest(Some(business), serviceVAT, "vrn", validVrn.value, Some(validRegistrationDate))
        val fastTrackFormData = agentFastTrackForm.fill(formData)
        val result = fastTrack(
          authorisedAsValidAgent(request, arn.value)
            .withFormUrlEncodedBody(fastTrackFormData.data.toSeq: _*))

        status(result) shouldBe BAD_REQUEST
      }
    }
  }
}
