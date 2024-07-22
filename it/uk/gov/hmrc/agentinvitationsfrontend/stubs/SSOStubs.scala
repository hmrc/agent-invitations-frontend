package uk.gov.hmrc.agentinvitationsfrontend.stubs
import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.stubbing.StubMapping
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport

trait SSOStubs {

  me: WireMockSupport =>

  def givenAllowlistedDomains: StubMapping =
    stubFor(
      get(urlEqualTo("/sso/domains")).willReturn(
        aResponse()
          .withStatus(200)
          .withBody("""{"externalDomains": ["127.0.0.1","online-qa.ibt.hmrc.gov.uk","ibt.hmrc.gov.uk"],"internalDomains":["localhost"]}""")
      )
    )

}
