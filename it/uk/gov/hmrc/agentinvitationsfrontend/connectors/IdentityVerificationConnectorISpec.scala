package uk.gov.hmrc.agentinvitationsfrontend.connectors

import uk.gov.hmrc.agentinvitationsfrontend.models.TechnicalIssue
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier

class IdentityVerificationConnectorISpec extends BaseISpec {

  implicit val hc = HeaderCarrier()
  val connector = app.injector.instanceOf[IdentityVerificationConnector]

  "identity verification connector" should {

    "return the failure reason when the UUID is valid" in {
      givenIVFailureReasonResponse(TechnicalIssue)
      val result = await(connector.getIVResult("valid-uuid"))
      result shouldBe Some(TechnicalIssue)
    }

    "return None when the UUID is not valid" in {
      givenIVResponseInvalidUUID()
      val result = await(connector.getIVResult("invalid"))
      result shouldBe None
    }
  }

}
