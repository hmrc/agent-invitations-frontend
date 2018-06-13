package uk.gov.hmrc.agentinvitationsfrontend.stubs

import com.github.tomakehurst.wiremock.client.WireMock.{aResponse, get, stubFor, urlEqualTo}
import uk.gov.hmrc.agentinvitationsfrontend.UriPathEncoding.encodePathSegment
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport
import uk.gov.hmrc.agentmtdidentifiers.model.Arn

trait ASAStubs {
  me: WireMockSupport =>

  def givenGetAgencyNameStub(arn: Arn) =
    stubFor(
      get(urlEqualTo(s"/agent-services-account/client/agency-name/${encodePathSegment(arn.value)}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |{
                         |  "agencyName" : "My Agency"
                         |}""".stripMargin)))

  def givenAgencyNameNotFoundStub(arn: Arn) =
    stubFor(
      get(urlEqualTo(s"/agent-services-account/client/agency-name/${encodePathSegment(arn.value)}"))
        .willReturn(aResponse()
          .withStatus(404)))

}
