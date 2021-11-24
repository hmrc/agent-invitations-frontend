package uk.gov.hmrc.agentinvitationsfrontend.connectors

import uk.gov.hmrc.agentinvitationsfrontend.models.{FailedDirectorCheck, FailedIV, FailedMatching, Incomplete, InsufficientEvidence, LockedOut, NinoClStoreEntry, PreconditionFailed, Success, TechnicalIssue, TimedOut, UserAborted}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import play.api.test.Helpers._
import uk.gov.hmrc.domain.Nino

class IdentityVerificationConnectorISpec extends BaseISpec {

  implicit val hc = HeaderCarrier()
  val connector = app.injector.instanceOf[IdentityVerificationConnector]

  "getIVResult" should {

    "return the TechnicalIssue reason when the UUID is valid" in {
      givenIVFailureReasonResponse(TechnicalIssue)
      val result = await(connector.getIVResult("valid-uuid"))
      result shouldBe Some(TechnicalIssue)
    }

    "return Success when the UUID is valid" in {
      givenIVFailureReasonResponse(Success)
      val result = await(connector.getIVResult("valid-uuid"))
      result shouldBe Some(Success)
    }

    "return Incomplete when the UUID is valid" in {
      givenIVFailureReasonResponse(Incomplete)
      val result = await(connector.getIVResult("valid-uuid"))
      result shouldBe Some(Incomplete)
    }

    "return PreconditionFailed when the UUID is valid" in {
      givenIVFailureReasonResponse(PreconditionFailed)
      val result = await(connector.getIVResult("valid-uuid"))
      result shouldBe Some(PreconditionFailed)
    }
    "return LockedOut when the UUID is valid" in {
      givenIVFailureReasonResponse(LockedOut)
      val result = await(connector.getIVResult("valid-uuid"))
      result shouldBe Some(LockedOut)

    }

    "return InsufficientEvidence when the UUID is valid" in {
      givenIVFailureReasonResponse(InsufficientEvidence)
      val result = await(connector.getIVResult("valid-uuid"))
      result shouldBe Some(InsufficientEvidence)
    }

  "return FailedMatching when the UUID is valid" in {
    givenIVFailureReasonResponse(FailedMatching)
    val result = await(connector.getIVResult("valid-uuid"))
    result shouldBe Some(FailedMatching)
  }

    "return UserAborted when the UUID is valid" in {
    givenIVFailureReasonResponse(UserAborted)
    val result = await(connector.getIVResult("valid-uuid"))
    result shouldBe Some(UserAborted)
    }

    "return TimedOut when the UUID is valid" in {
  givenIVFailureReasonResponse(TimedOut)
  val result = await(connector.getIVResult("valid-uuid"))
  result shouldBe Some(TimedOut)
}

    "return FailedIV when the UUID is valid" in {
      givenIVFailureReasonResponse(FailedIV)
      val result = await(connector.getIVResult("valid-uuid"))
      result shouldBe Some(FailedIV)
    }

    "return FailedDirectorCheck when the UUID is valid" in {
      givenIVFailureReasonResponse(FailedDirectorCheck)
      val result = await(connector.getIVResult("valid-uuid"))
      result shouldBe Some(FailedDirectorCheck)
    }



    "return None when the UUID is not valid" in {
      givenIVResponseInvalidUUID()
      val result = await(connector.getIVResult("invalid"))
      result shouldBe None
    }
  }

  "updateEntry" should {
    "return 200 when the response is successful" in {
      givenIVUpsertSucceeded
      val result = await(connector.updateEntry(
        NinoClStoreEntry(
          credId = "cred-1",
          nino = Nino(nino),
          confidenceLevel = None,
          createdAt = None,
          updatedAt = None), "cred-1"))
      result shouldBe OK
    }

    "return 500 when there has been an error" in {
      givenIVUpsertFailed
      val result = await(connector.updateEntry(
        NinoClStoreEntry(
          credId = "cred-1",
          nino = Nino(nino),
          confidenceLevel = None,
          createdAt = None,
          updatedAt = None), "cred-1"))
      result shouldBe INTERNAL_SERVER_ERROR
    }
  }

}
