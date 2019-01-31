package uk.gov.hmrc.agentinvitationsfrontend.services

import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.models.{CancelAuthorisationRequest, ClientConsent, ClientConsentsJourneyState}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.concurrent.ExecutionContext.Implicits.global

class ClientConsentsJourneyStateKeyStoreCacheSpec extends BaseISpec {

  private implicit val hc = HeaderCarrier(sessionId = Some(SessionId("sessionId123456")))

  "fetch" should {
    "return an optional entry when there is something in the cache" in {
      val cache = new ClientConsentsJourneyStateKeyStoreCache(new TestSessionCache())

      val journeyState = ClientConsentsJourneyState(Seq(ClientConsent(InvitationId("A72B8OGKNOT27"), LocalDate.parse("2018-01-01"), "itsa", true, false)), Some("agencyName"))

      await(cache.save(journeyState))

      await(cache.fetch) shouldBe Some(journeyState)
    }

    "return None when there is nothing in the cache" in {
      val cache = new ClientConsentsJourneyStateKeyStoreCache(new TestSessionCache())

      await(cache.fetch) shouldBe None
    }
  }

  "fetchAndClear" should {
    "return an optional journey state when there is something in the cache and generate an empty cache" in {
      val cache = new ClientConsentsJourneyStateKeyStoreCache(new TestSessionCache())

      val journeyState = ClientConsentsJourneyState(Seq(ClientConsent(InvitationId("A72B8OGKNOT27"), LocalDate.parse("2018-01-01"), "itsa", true, false)), Some("agencyName"))

      await(cache.save(journeyState))

      await(cache.fetchAndClear) shouldBe Some(journeyState)

      await(cache.fetch) shouldBe Some(ClientConsentsJourneyState(List.empty, None))
    }

    "return None when there is nothing in the cache" in {
      val cache = new ClientConsentsJourneyStateKeyStoreCache(new TestSessionCache())

      await(cache.fetchAndClear) shouldBe None
    }
  }

  "save" should {
    "return the entry you are saving and overwrite a previous cache entry with the new one" in {
      val cache = new ClientConsentsJourneyStateKeyStoreCache(new TestSessionCache())

      val journeyState1 = ClientConsentsJourneyState(Seq(ClientConsent(InvitationId("A72B8OGKNOT27"), LocalDate.parse("2018-01-01"), "itsa", true, false)), Some("agencyName"))
      val journeyState2 = ClientConsentsJourneyState(Seq(ClientConsent(InvitationId("C72B8OGKNOT27"), LocalDate.parse("2018-01-01"), "vat", true, false)), Some("agencyName"))

      await(cache.save(journeyState1)) shouldBe journeyState1
      await(cache.save(journeyState2)) shouldBe journeyState2

      await(cache.fetch) shouldBe Some(journeyState2)
    }
  }

}
