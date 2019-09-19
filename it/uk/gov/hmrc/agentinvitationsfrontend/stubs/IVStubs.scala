package uk.gov.hmrc.agentinvitationsfrontend.stubs
import com.github.tomakehurst.wiremock.client.WireMock._
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport

trait IVStubs {
  me: WireMockSupport =>

    def givenIVFailureReasonResponse(failureReason: String) = {
      stubFor(
        get(urlEqualTo(s"/mdtp/journey/journeyId/valid-uuid"))
          .willReturn(aResponse()
          .withStatus(200)
          .withBody(s"""{"token":"fd53ef15-5073-401f-8390-ee7b8769452f","result":"$failureReason"}
                      |""".stripMargin))
      )
    }

  def givenIVResponseInvalidUUID() = {
    stubFor(
      get(urlEqualTo(s"/mdtp/journey/journeyId/invalid"))
        .willReturn(aResponse()
        .withStatus(404)
        )
    )
  }

}
