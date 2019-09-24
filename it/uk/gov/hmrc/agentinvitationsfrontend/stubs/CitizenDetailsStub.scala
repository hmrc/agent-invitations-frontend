package uk.gov.hmrc.agentinvitationsfrontend.stubs

import com.github.tomakehurst.wiremock.client.WireMock._
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport

trait CitizenDetailsStub {
  me: WireMockSupport =>

  def givenCitizenDetailsAreKnownFor(nino: String, firstName: String, lastName: String) =
    stubFor(
      get(urlEqualTo(s"/citizen-details/nino/$nino"))
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
                         |      "nino": "$nino"
                         |   },
                         |   "dateOfBirth": "11121971"
                         |}""".stripMargin)))

  def givenCitizenDetailsReturns404For(nino: String) =
    stubFor(
      get(urlEqualTo(s"/citizen-details/nino/$nino"))
        .willReturn(aResponse()
          .withStatus(404)))

  def givenCitizenDetailsReturns400For(nino: String) =
    stubFor(
      get(urlEqualTo(s"/citizen-details/nino/$nino"))
        .willReturn(aResponse()
          .withStatus(400)))
}
