package uk.gov.hmrc.agentinvitationsfrontend.stubs

import com.github.tomakehurst.wiremock.client.WireMock._
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport
import uk.gov.hmrc.domain.Nino

trait CitizenDetailsStub {
  me: WireMockSupport =>

  def givenCitizenDetailsAreKnownFor(nino: Nino, firstName: String, lastName: String) =
    stubFor(
      get(urlEqualTo(s"/citizen-details/nino/${nino.value}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""{
                         |   "name": {
                         |      "current": {
                         |         "firstName": "$firstName",
                         |         "lastName": "$lastName"
                         |      },
                         |      "previous": []
                         |   },
                         |   "ids": {
                         |      "nino": "${nino.value}"
                         |   },
                         |   "dateOfBirth": "11121971"
                         |}""".stripMargin)))

  def givenCitizenDetailsReturns404For(nino: Nino) =
    stubFor(
      get(urlEqualTo(s"/citizen-details/nino/${nino.value}"))
        .willReturn(aResponse()
          .withStatus(404)))

  def givenCitizenDetailsReturns400For(nino: Nino) =
    stubFor(
      get(urlEqualTo(s"/citizen-details/nino/${nino.value}"))
        .willReturn(aResponse()
          .withStatus(400)))
}
