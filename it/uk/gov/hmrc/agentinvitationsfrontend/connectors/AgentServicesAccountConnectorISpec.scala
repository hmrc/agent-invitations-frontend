package uk.gov.hmrc.agentinvitationsfrontend.connectors

import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.http.{HeaderCarrier, NotFoundException}

class AgentServicesAccountConnectorISpec extends BaseISpec {

  implicit val hc = HeaderCarrier()
  val connector = app.injector.instanceOf[AgentServicesAccountConnector]
  val arn = Arn("TARN0000001")

  "getAgencyName" should {
    "return agency name for a valid arn" in {
      givenGetAgencyNameStub(arn)

      val result = await(connector.getAgencyName(arn.value))

      result shouldBe Some("My Agency")
    }

    "return AgencyNameNotFound exception for an invalid arn" in {
      givenAgencyNameNotFoundStub(Arn("INVALID_ARN"))

      intercept[AgencyNameNotFound] {
        await(connector.getAgencyName("INVALID_ARN"))
      }
    }
  }

}
