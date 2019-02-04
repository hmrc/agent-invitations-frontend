package uk.gov.hmrc.agentinvitationsfrontend.services

import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentMultiAuthorisationJourneyState, AuthorisationRequest, Invitation}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class AgentMultiAuthorisationJourneyStateKeyStoreCacheSpec extends BaseISpec {

  private implicit val hc = HeaderCarrier(sessionId = Some(SessionId("sessionId123456")))

  "fetch" should {
    "return an optional journey state when there is something in the cache" in {
      val cache = new AgentMultiAuthorisationJourneyStateKeyStoreCache(new TestSessionCache())

      val authRequests = Set(AuthorisationRequest("name of client", Invitation(Some("personal"), "HMRC-MTD-IT", "AB123456A", Some("BN114AW"))))
      val journeyState = AgentMultiAuthorisationJourneyState("personal", authRequests)

      await(cache.save(journeyState))

      await(cache.fetch) shouldBe Some(journeyState)
    }

    "return None when there is nothing in the cache" in {
      val cache = new AgentMultiAuthorisationJourneyStateKeyStoreCache(new TestSessionCache())

      await(cache.fetch) shouldBe None
    }
  }

  "fetchAndClear" should {
    "return an optional journey state when there is something in the cache and generate an empty cache" in {
      val cache = new AgentMultiAuthorisationJourneyStateKeyStoreCache(new TestSessionCache())

      val authRequests = Set(AuthorisationRequest("name of client", Invitation(Some("personal"), "HMRC-MTD-IT", "AB123456A", Some("BN114AW"))))
      val journeyState = AgentMultiAuthorisationJourneyState("personal", authRequests)

      await(cache.save(journeyState))

      await(cache.fetchAndClear) shouldBe Some(journeyState)

      await(cache.fetch) shouldBe Some(AgentMultiAuthorisationJourneyState("", Set.empty))
    }

    "return None when there is nothing in the cache" in {
      val cache = new AgentMultiAuthorisationJourneyStateKeyStoreCache(new TestSessionCache())

      await(cache.fetchAndClear) shouldBe None
    }
  }

  "save" should {
    "return the entry you are saving and overwrite a previous cache entry with the new one" in {
      val cache = new AgentMultiAuthorisationJourneyStateKeyStoreCache(new TestSessionCache())

      val authRequests1 = Set(AuthorisationRequest("name of client", Invitation(Some("personal"), "HMRC-MTD-IT", "AB123456A", Some("BN114AW"))))
      val journeyState1 = AgentMultiAuthorisationJourneyState("personal", authRequests1)
      val authRequests2 = Set(AuthorisationRequest("name of client", Invitation(Some("business"), "HMRC-MTD-VAT", "111111", Some("2012-01-02"))))
      val journeyState2 = AgentMultiAuthorisationJourneyState("business", authRequests2)

      await(cache.save(journeyState1)) shouldBe journeyState1
      await(cache.save(journeyState2)) shouldBe journeyState2

      await(cache.fetch) shouldBe Some(journeyState2)
    }
  }

}
