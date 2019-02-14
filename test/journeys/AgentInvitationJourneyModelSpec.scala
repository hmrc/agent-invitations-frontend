package journeys
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global
import AgentInvitationJourneyModel.State
import AgentInvitationJourneyModel.States._
import AgentInvitationJourneyModel.Transitions._
import uk.gov.hmrc.http.HeaderCarrier

class AgentInvitationJourneyModelSpec extends UnitSpec {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  case class JourneyAt(initialState: State) extends AgentInvitationJourneyService with TestStorage[State] {
    await(save(initialState))
  }

  "AgentInvitationJourneyService" when {
    "at any state" should {
      "startJourney and go to Start" in {
        await(JourneyAt(UnknownState) apply startJourney) shouldBe Right(Start)
        await(JourneyAt(Start) apply startJourney) shouldBe Right(Start)
      }
    }
  }

}
