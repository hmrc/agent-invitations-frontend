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

    "redirect to check-details if all values in request are valid with no continue url" in {

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
      journeyState.get shouldBe Some(
        CheckDetails(AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN32TN")), None),
        List(Prologue(None)))
    }

    "redirect to check-details if all values in request are valid with a continue and error url query parameters" in {

      val request = FakeRequest(
        "POST",
        "/agents/fast-track?continue=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fselect-client&error=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fnot-authorised"
      )
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
      journeyState.get shouldBe Some(
        CheckDetails(
          AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN32TN")),
          Some("http://localhost:9996/tax-history/select-client")),
        List(Prologue(Some("http://localhost:9996/tax-history/not-authorised")), Prologue(None))
      )
    }

    "redirect to the error url with appended error reason if all values in request are valid with a continue and error url query parameters" in {

      val request = FakeRequest(
        "POST",
        "/agents/fast-track?continue=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fselect-client&error=http%3A%2F%2Flocalhost%3A9996%2Ftax-history%2Fnot-authorised"
      )
      val result = controller.agentFastTrack(
        authorisedAsValidAgent(
          request.withFormUrlEncodedBody(
            "clientType"           -> "personal",
            "service"              -> "foo",
            "clientIdentifierType" -> "ni",
            "clientIdentifier"     -> "AB123456A",
            "knownFact"            -> "BN32TN"),
          arn.value
        ))
      status(result) shouldBe 303
      redirectLocation(result) shouldBe Some(
        "http://localhost:9996/tax-history/not-authorised?issue=UNSUPPORTED_SERVICE")
      journeyState.get shouldBe Some(
        Prologue(Some("http://localhost:9996/tax-history/not-authorised")),
        List(Prologue(None))
      )
    }
  }

  "GET /agents/fast-track/check-details" should {
    val request = FakeRequest("GET", "/agents/fast-track/check-details")
    "show the check-details page" in {
      journeyState.set(
        CheckDetails(AgentFastTrackRequest(Some(personal), HMRCMTDIT, "ni", "AB123456A", Some("BN32TN")), None),
        List(Prologue(None)))

      val result = controller.showCheckDetails(authorisedAsValidAgent(request, arn.value))

      status(result) shouldBe 200
      checkHtmlResultWithBodyMsgs(
        result,
        "check-details.heading",
        "check-details.p.HMRC-MTD-IT",
        "check-details.client-type.personal")
    }
  }
}
