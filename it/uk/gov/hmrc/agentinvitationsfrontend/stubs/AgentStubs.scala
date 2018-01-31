package uk.gov.hmrc.agentinvitationsfrontend.stubs

import com.github.tomakehurst.wiremock.client.WireMock._
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport

trait AgentStubs {
  me: WireMockSupport =>

  def getRegisteredClientStub(vrn: String, registrationDate: String): Unit = {
    stubFor(get(urlEqualTo(agentStubsUrl(vrn)))
      .willReturn(
        aResponse()
          .withStatus(200)
          .withBody(
            s"""{
               |  "vrn": "$vrn",
               |  "registrationDate": "$registrationDate"
               |}""".stripMargin)))
  }

  def notFoundRegisteredClientStub(vrn: String, registrationDate: String): Unit = {
    stubFor(get(urlEqualTo(agentStubsUrl(vrn)))
      .willReturn(
        aResponse()
          .withStatus(404)))
  }

  private def agentStubsUrl(vrn: String) = s"/clients/registrationDate/vrn/$vrn"

}
