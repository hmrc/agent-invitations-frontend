package uk.gov.hmrc.agentinvitationsfrontend.connectors

import play.api.http.Status.INTERNAL_SERVER_ERROR
import play.api.test.Helpers.{await, defaultAwaitTimeout}
import uk.gov.hmrc.agentinvitationsfrontend.stubs.AcrfStubs
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext

class AcrfConnectorISpec extends BaseISpec with AcrfStubs {

  val connector: AcrfConnector = app.injector.instanceOf[AcrfConnector]
  val formData: Map[String, Seq[String]] = Map(
    "clientIdentifier" -> Seq("123456789"), "clientIdentifierType" -> Seq("vrn"), "service" -> Seq("HMRC-MTD-VAT")
  )
  val hc: HeaderCarrier = HeaderCarrier()
  implicit val ec: ExecutionContext = app.injector.instanceOf[ExecutionContext]

  ".fastTrack" should {

    "return the redirect URL that was provided by ACRF" when {

      "all query params are provided" in {
        val redirectUrl = "/my-url"
        givenFastTrackUrlFromAcrf(redirectUrl)
        val result = await(connector.fastTrack(formData, Some("/continue"), Some("/error"), hc))

        result shouldBe appConfig.acrfBaseUrl + redirectUrl
      }

      "no query params are provided" in {
        val redirectUrl = "/my-url"
        givenFastTrackUrlFromAcrf(redirectUrl)
        val result = await(connector.fastTrack(formData, None, None, hc))

        result shouldBe appConfig.acrfBaseUrl + redirectUrl
      }
    }

    "return a default redirect URL when receiving an unexpected status from ACRF" in {
      val redirectUrl = "/my-url"
      givenFastTrackUrlFromAcrf(redirectUrl, INTERNAL_SERVER_ERROR)
      val result = await(connector.fastTrack(formData, Some("/continue"), Some("/error"), hc))

      result shouldBe appConfig.createAuthRequestUrl
    }
  }
}
