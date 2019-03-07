package uk.gov.hmrc.agentinvitationsfrontend.controllers.journeys

import org.joda.time.LocalDate
import play.api.Application
import play.api.test.FakeRequest
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{business, personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.Vrn
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class AgentInvitationFastTrackJourneyControllerISpec extends BaseISpec with StateAndBreadcrumbsMatchers {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  override implicit lazy val app: Application = appBuilder
    .overrides(new TestAgentInvitationFastTrackJourneyModule)
    .build()

  lazy val journeyState = app.injector.instanceOf[TestAgentInvitationFastTrackJourneyService]
  lazy val controller: AgentInvitationFastTrackJourneyController =
    app.injector.instanceOf[AgentInvitationFastTrackJourneyController]

  import journeyState.model.State
  import journeyState.model.States._

  val availableServices = Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)
  val emptyBasket = Set.empty[AuthorisationRequest]

  "POST /agents/fast-track" should {
    val request = FakeRequest("POST", "/agents/fast-track")

    "redirect to check-details if all values in request a`re valid" in {

      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "personal",
            "service"              -> "HMRC-MTD-IT",
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "AB123456A",
            "knownFact"            -> "BN32TN"),
          arn.value
        ))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(routes.AgentInvitationFastTrackJourneyController.showCheckDetails().url)
      journeyState.get shouldBe None
    }
  }
}
