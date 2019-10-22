package uk.gov.hmrc.agentinvitationsfrontend.stubs
import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.stubbing.StubMapping
import uk.gov.hmrc.agentinvitationsfrontend.models.IVResult
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport

trait IVStubs {
  me: WireMockSupport =>

    def givenIVFailureReasonResponse(failureReason: IVResult): StubMapping = {
      stubFor(
        get(urlEqualTo(s"/mdtp/journey/journeyId/valid-uuid"))
          .willReturn(aResponse()
          .withStatus(200)
          .withBody(s"""{"token":"fd53ef15-5073-401f-8390-ee7b8769452f","result":"${failureReason.value}"}
                      |""".stripMargin))
      )
    }

  def givenIVResponseInvalidUUID(): StubMapping = {
    stubFor(
      get(urlEqualTo(s"/mdtp/journey/journeyId/invalid"))
        .willReturn(aResponse()
        .withStatus(404))
    )
  }

  def givenIVUpsertSucceeded: StubMapping = {
    stubFor(
      put(urlPathMatching("/nino/.+"))
        .willReturn(aResponse()
                      .withStatus(200)))
  }

  def givenIVUpsertFailed: StubMapping = {
    stubFor(
      put(urlPathMatching(s"/nino/.+"))
        .willReturn(aResponse()
                      .withStatus(500)))
  }

}
