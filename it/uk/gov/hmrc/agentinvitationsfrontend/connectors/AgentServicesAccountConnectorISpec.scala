package uk.gov.hmrc.agentinvitationsfrontend.connectors

import uk.gov.hmrc.agentinvitationsfrontend.models.{CustomerDetails, Individual}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class AgentServicesAccountConnectorISpec extends BaseISpec {

  implicit val hc = HeaderCarrier()
  val connector = app.injector.instanceOf[AgentServicesAccountConnector]




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

  "getTradingName" should {
    val nino = Nino("AB123456C")
    "when supplied with a valid nino return the trading name for that nino" in {
      givenTradingName(nino, "FooBar Ltd")

      val result = await(connector.getTradingName(nino))
      result shouldBe Some("FooBar Ltd")
    }

    "when supplied with a valid nino but trading name is empty" in {
      givenTradingNameMissing(nino)

      val result = await(connector.getTradingName(nino))
      result shouldBe None
    }

    "when supplied with a valid nino but trading name is an empty string" in {
      givenTradingName(nino, "")

      val result = await(connector.getTradingName(nino))
      result shouldBe Some("")
    }

    "return None when the nino has no trading name" in {
      givenTradingNameNotFound(nino)

      val result = await(connector.getTradingName(nino))
      result shouldBe None
    }
  }

  "getCustomerDetails" should {
    val vrn = Vrn("101747696")
    val customerDetailsAll = CustomerDetails(
      Some("Gadgetron"),
      Some(Individual(Some("Mr"), Some("Winston"), Some("H"), Some("Greenburg"))),
      Some("GDT"))
    val customerDetailsOnlyPersonal =
      CustomerDetails(None, Some(Individual(Some("Mr"), Some("Winston"), Some("H"), Some("Greenburg"))), None)
    val customerDetailsOnlyOrganisation = CustomerDetails(Some("Gadgetron"), None, None)
    val customerDetailsOnlyTrading = CustomerDetails(None, None, Some("GDT"))
    val customerDetailsNone = CustomerDetails(None, None, None)

    "return customer details when vrn has details associated" in {
      givenClientDetails(vrn)

      val result = await(connector.getCustomerDetails(vrn))
      result shouldBe customerDetailsAll
    }

    "return only trading name when organisation name and personal name are not present" in {
      givenClientDetailsOnlyTrading(vrn)

      val result = await(connector.getCustomerDetails(vrn))
      result shouldBe customerDetailsOnlyTrading
    }

    "return only organisation name when trading name and personal name are not present" in {
      givenClientDetailsOnlyOrganisation(vrn)

      val result = await(connector.getCustomerDetails(vrn))
      result shouldBe customerDetailsOnlyOrganisation
    }

    "return only personal name when trading name and organisation names are not present" in {
      givenClientDetailsOnlyPersonal(vrn)

      val result = await(connector.getCustomerDetails(vrn))
      result shouldBe customerDetailsOnlyPersonal
    }

    "return not found when vrn has no details associated" in {
      givenClientDetailsNotFound(vrn)

      val result = await(connector.getCustomerDetails(vrn))
      result shouldBe customerDetailsNone
    }
  }

  "getNinoForMtdItId" should {
    "return nino for given mtditid" in {
      givenNinoForMtdItId(mtdItId, validNino)
      val result = await(connector.getNinoForMtdItId(mtdItId))
      result shouldBe Some(validNino)
    }
  }

}
