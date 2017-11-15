package uk.gov.hmrc.agentinvitationsfrontend.connectors

import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.http.{HeaderCarrier, NotFoundException}

class AgentServicesAccountConnectorISpec  extends BaseISpec {

  implicit val hc = HeaderCarrier()
  val connector = app.injector.instanceOf[AgentServicesAccountConnector]
  val arn = Arn("TARN0000001")

  "getAgencyname" should {
    "return agency name for a valid arn" in {
      givenGetAgencyNameStub(arn)

      val result = await(connector.getAgencyname(arn.value))

      result shouldBe Some("My Agency")
    }

    "return NotFound exception for an invalid arn" in {
      givenAgencyNameNotFoundStub(Arn("INVALID_ARN"))

      val exception = intercept[NotFoundException] {
        await(connector.getAgencyname("INVALID_ARN"))
      }

      exception.getMessage should include("Not Found")
    }
  }

}
