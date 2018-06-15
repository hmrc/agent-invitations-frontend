package uk.gov.hmrc.agentinvitationsfrontend.connectors

import uk.gov.hmrc.agentinvitationsfrontend.stubs.CitizenDetailsStub
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier}

class CitizenDetailsConnectorISpec extends BaseISpec with CitizenDetailsStub {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  val connector = app.injector.instanceOf[CitizenDetailsConnector]
  val nino = Nino("AE123456A")


  "Get citizen details" should {
    "return citizen details having first and last name given valid nino" in {
      givenCitizenDetailsAreKnownFor(nino.value,"Johny","Smithy")
      val result = await(connector.getCitizenDetails(nino))
      result.firstName.get shouldBe "Johny"
      result.lastName.get shouldBe "Smithy"
      result.nino.get shouldBe nino.value
    }

    "return empty Citizen if nino not found" in {
      givenCitizenDetailsReturns404For(nino.value)
      val result = await(connector.getCitizenDetails(nino))
      result.firstName shouldBe None
      result.lastName shouldBe None
      result.nino shouldBe None
    }

    "return BAD_REQUEST if nino not valid" in {
      givenCitizenDetailsReturns400For(nino.value)
      a[BadRequestException] shouldBe thrownBy {
        await(connector.getCitizenDetails(nino))
      }
    }
  }
}
