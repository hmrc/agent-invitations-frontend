package uk.gov.hmrc.agentinvitationsfrontend.services

import uk.gov.hmrc.agentinvitationsfrontend.models.CurrentInvitationInput
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class FastTrackKeyStoreCacheSpec extends BaseISpec {

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("session1234356")))

  val currentInvitationInput = CurrentInvitationInput(
    Some("service"),
    Some("clientIdentifierType"),
    Some("clientIdentifier"),
    Some("knownFact"))

  "FastTrackKeyStoreCache" should {
    "store and fetch saved fast-track invitation from keystore" in {
      await(testFastTrackCache.save(currentInvitationInput))
      val result = await(testFastTrackCache.fetch())

      result shouldBe Some(currentInvitationInput)
    }

    "return nothing if no fast-track invitation was stored in keystore" in {
      val result = await(testFastTrackCache.fetch())
      result shouldBe None
    }
  }
}
