package uk.gov.hmrc.agentinvitationsfrontend.stubs
import com.github.tomakehurst.wiremock.client.WireMock._
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Utr, Vrn}
import uk.gov.hmrc.domain.Nino

import java.time.LocalDate

trait ACRStubs {
  me: WireMockSupport =>

  def givenInactiveRelationships(arn: Arn) =
    stubFor(
      get(urlEqualTo(s"/agent-client-relationships/agent/relationships/inactive"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(
              s"""
                 |[{
                 |   "arn":"${arn.value}",
                 |   "clientType":"personal",
                 |   "dateTo":"2015-09-21",
                 |   "dateFrom":"2015-09-10",
                 |   "clientId":"ABCDE1234567890",
                 |   "service":"HMRC-MTD-IT"
                 |},
                 |{  "arn":"${arn.value}",
                 |   "clientType":"personal",
                 |   "dateTo":"2015-09-24",
                 |   "dateFrom":"2015-09-10",
                 |   "clientId":"101747641",
                 |   "service":"HMRC-MTD-VAT"
                 |},
                 |{
                 |   "arn":"${arn.value}",
                 |   "clientType":"personal",
                 |   "dateTo":"2015-09-21",
                 |   "dateFrom":"2015-09-10",
                 |   "clientId":"4937455253",
                 |   "service":"HMRC-TERS-ORG"
                 |},
                 |{
                 |   "arn":"${arn.value}",
                 |   "clientType":"personal",
                 |   "dateTo":"2015-09-21",
                 |   "dateFrom":"2015-09-10",
                 |   "clientId":"XMCGTP123456789",
                 |   "service":"HMRC-CGT-PD"
                 |}
                 |]""".stripMargin
            )
        )
    )

  def givenASingleInactiveRelationship(service: String, clientId: String, dateFrom: String, dateTo: String) =
    stubFor(
      get(urlEqualTo(s"/agent-client-relationships/agent/relationships/inactive"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(
              s"""
                 |[{
                 |   "arn":"TARN0000001",
                 |   "clientType":"personal",
                 |   "dateTo":"$dateTo",
                 |   "dateFrom":"$dateFrom",
                 |   "clientId":"$clientId",
                 |   "service":"$service"
                 |}]""".stripMargin
            )
        )
    )

  def givenInactiveRelationships() = {
    val now = LocalDate.now()
    stubFor(
      get(urlEqualTo(s"/agent-client-relationships/agent/relationships/inactive"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(
              s"""
                 |[{
                 |   "arn":"TARN0000001",
                 |   "clientType":"personal",
                 |   "dateTo":"${now.toString}",
                 |   "dateFrom":"${now.minusDays(14).toString}",
                 |   "clientId":"ABCDE1234567890",
                 |   "service":"HMRC-MTD-IT"
                 |},
                 |{  "arn":"TARN0000001",
                 |   "clientType":"personal",
                 |   "dateTo":"${now.minusDays(1).toString}",
                 |   "dateFrom":"${now.minusDays(12)}",
                 |   "clientId":"101747641",
                 |   "service":"HMRC-MTD-VAT"
                 |},
                 |{
                 |   "arn":"TARN0000001",
                 |   "clientType":"personal",
                 |   "dateTo":"${now.minusDays(3).toString}",
                 |   "dateFrom":"${now.minusDays(25).toString}",
                 |   "clientId":"4937455253",
                 |   "service":"HMRC-TERS-ORG"
                 |},
                 |{
                 |   "arn":"TARN0000001",
                 |   "clientType":"personal",
                 |   "dateTo":"${now.minusDays(5).toString}",
                 |   "dateFrom":"${now.minusDays(22).toString}",
                 |   "clientId":"XMCGTP123456789",
                 |   "service":"HMRC-CGT-PD"
                 |}
                 |]""".stripMargin
            )
        )
    )
  }


  def givenInactiveRelationshipsNotFound =
    stubFor(
      get(urlEqualTo(s"/agent-client-relationships/agent/relationships/inactive"))
        .willReturn(aResponse()
          .withStatus(404)))

  def givenCancelledAuthorisationItsa(arn: Arn, nino: Nino, status: Int) =
    stubFor(
      delete(urlEqualTo(s"/agent-client-relationships/agent/${arn.value}/service/HMRC-MTD-IT/client/NI/${nino.value}"))
        .willReturn(
          aResponse()
            .withStatus(status)
        )
    )

  def givenCancelledAuthorisationVat(arn: Arn, vrn: Vrn, status: Int) =
    stubFor(
      delete(urlEqualTo(s"/agent-client-relationships/agent/${arn.value}/service/HMRC-MTD-VAT/client/VRN/${vrn.value}"))
        .willReturn(
          aResponse()
            .withStatus(status)
        )
    )

  def givenCancelledAuthorisationTrust(arn: Arn, utr: Utr, status: Int) =
    stubFor(
      delete(urlEqualTo(s"/agent-client-relationships/agent/${arn.value}/service/HMRC-TERS-ORG/client/SAUTR/${utr.value}"))
        .willReturn(
          aResponse()
            .withStatus(status)
        )
    )

  def givenCheckRelationshipItsaWithStatus(arn: Arn, nino: String, status: Int) =
    stubFor(
      get(urlEqualTo(s"/agent-client-relationships/agent/${arn.value}/service/HMRC-MTD-IT/client/NI/$nino"))
        .willReturn(
          aResponse()
            .withStatus(status)
        )
    )

  def givenCheckRelationshipVatWithStatus(arn: Arn, vrn: String, status: Int) =
    stubFor(
      get(urlEqualTo(s"/agent-client-relationships/agent/${arn.value}/service/HMRC-MTD-VAT/client/VRN/$vrn"))
        .willReturn(
          aResponse()
            .withStatus(status)
        )
    )

  def givenCheckRelationshipTrustWithStatus(arn: Arn, utr: String, status: Int) =
    stubFor(
      get(urlEqualTo(s"/agent-client-relationships/agent/${arn.value}/service/HMRC-TERS-ORG/client/SAUTR/$utr"))
        .willReturn(
          aResponse()
            .withStatus(status)
        )
    )
}
