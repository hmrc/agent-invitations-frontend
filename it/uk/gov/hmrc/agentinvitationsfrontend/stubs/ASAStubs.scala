package uk.gov.hmrc.agentinvitationsfrontend.stubs

import com.github.tomakehurst.wiremock.client.WireMock.{aResponse, get, stubFor, urlEqualTo}
import uk.gov.hmrc.agentinvitationsfrontend.UriPathEncoding.encodePathSegment
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino

trait ASAStubs {
  me: WireMockSupport =>

  def givenGetAgencyNameClientStub(arn: Arn) =
    stubFor(
      get(urlEqualTo(s"/agent-services-account/client/agency-name/${encodePathSegment(arn.value)}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |{
                         |  "agencyName" : "My Agency"
                         |}""".stripMargin)))

  def givenGetAgencyNameAgentStub =
    stubFor(
      get(urlEqualTo(s"/agent-services-account/agent/agency-name"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |{
                         |  "agencyName" : "My Agency"
                         |}""".stripMargin)))

  def givenAgencyNameNotFoundClientStub(arn: Arn) =
    stubFor(
      get(urlEqualTo(s"/agent-services-account/client/agency-name/${encodePathSegment(arn.value)}"))
        .willReturn(aResponse()
          .withStatus(404)))

  def givenAgencyNameNotFoundAgentStub =
    stubFor(
      get(urlEqualTo(s"/agent-services-account/agent/agency-name"))
        .willReturn(aResponse()
          .withStatus(404)))

  def givenTradingName(nino: Nino, tradingName: String) =
    stubFor(
      get(urlEqualTo(s"/agent-services-account/client/trading-name/nino/$nino"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""{"tradingName": "$tradingName"}""")
        ))

  def givenTradingNameMissing(nino: Nino) =
    stubFor(
      get(urlEqualTo(s"/agent-services-account/client/trading-name/nino/$nino"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""{}""")
        ))

  def givenTradingNameNotFound(nino: Nino) =
    stubFor(
      get(urlEqualTo(s"/agent-services-account/client/trading-name/nino/${nino.value}"))
        .willReturn(
          aResponse()
            .withStatus(404)
        ))

  def givenClientDetails(vrn: Vrn) =
    stubFor(
      get(urlEqualTo(s"/agent-services-account/client/vat-customer-details/vrn/${vrn.value}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""{"organisationName": "Gadgetron",
                         |"individual" : {
                         |    "title": "Mr",
                         |    "firstName": "Winston",
                         |    "middleName": "H",
                         |    "lastName": "Greenburg"
                         |    },
                         |"tradingName": "GDT"
                         |}""".stripMargin)
        ))

  def givenClientDetailsNotFound(vrn: Vrn) =
    stubFor(
      get(urlEqualTo(s"/agent-services-account/client/vat-customer-details/vrn/${vrn.value}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""{}""".stripMargin)
        ))

  def givenClientDetailsOnlyTrading(vrn: Vrn) =
    stubFor(
      get(urlEqualTo(s"/agent-services-account/client/vat-customer-details/vrn/${vrn.value}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""{"tradingName": "GDT"}""".stripMargin)
        ))

  def givenClientDetailsOnlyOrganisation(vrn: Vrn) =
    stubFor(
      get(urlEqualTo(s"/agent-services-account/client/vat-customer-details/vrn/${vrn.value}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""{"organisationName": "Gadgetron"}""".stripMargin)
        ))

  def givenClientDetailsOnlyPersonal(vrn: Vrn) =
    stubFor(
      get(urlEqualTo(s"/agent-services-account/client/vat-customer-details/vrn/${vrn.value}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""{"individual" : {
                         |    "title": "Mr",
                         |    "firstName": "Winston",
                         |    "middleName": "H",
                         |    "lastName": "Greenburg"
                         |    }
                         |}""".stripMargin)
        ))

  def givenNinoForMtdItId(mtdItId: MtdItId, nino: Nino) =
    stubFor(
      get(urlEqualTo(s"/agent-services-account/client/mtdItId/${mtdItId.value}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(
              s"""
                 | {
                 |    "nino":"${nino.value}"
                 | }
               """.stripMargin)
        )
    )
}
