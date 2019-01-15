package uk.gov.hmrc.agentinvitationsfrontend.connectors

import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier}

import scala.concurrent.ExecutionContext.Implicits.global

class CitizenDetailsConnectorISpec extends BaseISpec {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  val connector = app.injector.instanceOf[CitizenDetailsConnector]

  "Get citizen details" should {
    "return citizen details having first and last name given valid nino" in {
      givenCitizenDetailsAreKnownFor(nino, "Johny", "Smithy")
      val result = await(connector.getCitizenDetails(validNino))
      result.firstName.get shouldBe "Johny"
      result.lastName.get shouldBe "Smithy"
      result.nino.get shouldBe nino
    }

    "return empty Citizen if nino not found" in {
      givenCitizenDetailsReturns404For(nino)
      val result = await(connector.getCitizenDetails(validNino))
      result.firstName shouldBe None
      result.lastName shouldBe None
      result.nino shouldBe None
    }

    "return BAD_REQUEST if nino not valid" in {
      givenCitizenDetailsReturns400For(nino)
      a[BadRequestException] shouldBe thrownBy {
        await(connector.getCitizenDetails(validNino))
      }
    }
  }
}
