package uk.gov.hmrc.agentinvitationsfrontend.stubs

import com.github.tomakehurst.wiremock.client.WireMock.{aResponse, post, stubFor, urlPathEqualTo}
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport

trait AcrfStubs {
  me: WireMockSupport =>

  def givenFastTrackUrlFromAcrf(redirectUrl: String, status: Int = 200): Any =
    stubFor(
      post(urlPathEqualTo(s"/agent-client-relationships/agents/fast-track/redirect"))
        .willReturn(
          aResponse()
            .withStatus(status)
            .withBody(redirectUrl)
        )
    )
}
