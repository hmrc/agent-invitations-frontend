package uk.gov.hmrc.agentinvitationsfrontend.stubs

import com.github.tomakehurst.wiremock.client.WireMock.{aResponse, get, stubFor, urlEqualTo}
import com.github.tomakehurst.wiremock.stubbing.StubMapping
import uk.gov.hmrc.agentinvitationsfrontend.UriPathEncoding.encodePathSegment
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport

trait PDVStubs {
  me: WireMockSupport =>

  def givenPdvValidationSuccess(validationId: String): StubMapping = {
    stubFor(
      get(urlEqualTo(s"/${encodePathSegment(validationId)}"))
        .willReturn(aResponse()
                      .withStatus(200)
                      .withBody(s"""{
                                   |  "id": "$validationId",
                                   |  "validationStatus": "success",
                                   |  "personalDetails": {
                                   |    "firstName": "Jim",
                                   |    "lastName": "Ferguson",
                                   |    "nino": "AA000003D",
                                   |    "dateOfBirth": "1948-04-23"
                                   |  }
                                   |}""".stripMargin))
      )
  }

  def givenPdvValidationSuccessNoNino(validationId: String): StubMapping = {
    stubFor(
      get(urlEqualTo(s"/${encodePathSegment(validationId)}"))
        .willReturn(aResponse()
                      .withStatus(200)
                      .withBody(s"""{
                                   |  "id": "$validationId",
                                   |  "validationStatus": "success",
                                   |  "personalDetails": {
                                   |    "firstName": "Jim",
                                   |    "lastName": "Ferguson",
                                   |    "dateOfBirth": "1948-04-23"
                                   |  }
                                   |}""".stripMargin))
      )
  }

  def givenPdvValidationFailure(validationId: String): StubMapping = {
    stubFor(
      get(urlEqualTo(s"/${encodePathSegment(validationId)}"))
        .willReturn(aResponse()
                      .withStatus(200)
                      .withBody(s"""{
                                   |  "id": "$validationId",
                                   |  "validationStatus": "failure"
                                   |}""".stripMargin))
      )
  }

  def givenPdvValidationNotFound(validationId: String): StubMapping = {
    stubFor(
      get(urlEqualTo(s"/${encodePathSegment(validationId)}"))
        .willReturn(aResponse()
                      .withStatus(404)))
  }

}
