package uk.gov.hmrc.agentinvitationsfrontend.stubs
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport

import com.github.tomakehurst.wiremock.client.WireMock.{put, _}
import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.UriPathEncoding._
import uk.gov.hmrc.agentinvitationsfrontend.models.StoredInvitation
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, Vrn}
import uk.gov.hmrc.domain.Nino


trait ACRStubs {
  me: WireMockSupport =>


  def givenInactiveITSARelationships(arn: Arn) =
    stubFor(
      get(urlEqualTo(s"/agent-client-relationships/relationships/inactive/service/HMRC-MTD-IT"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(
              s"""
                 |[{
                 |   "arn":"${arn.value}",
                 |   "dateTo":"2015-09-21",
                 |   "dateFrom":"2015-09-10",
                 |   "referenceNumber":"ABCDE1234567890"
                 |},
                 |{  "arn":"${arn.value}",
                 |   "dateTo":"2015-09-24",
                 |   "dateFrom":"2015-09-10",
                 |   "referenceNumber":"JKKL80894713304"
                 |}]""".stripMargin
            )
        )
    )

  def givenInactiveITSARelationshipsNotFound =
    stubFor(
      get(urlEqualTo(s"/agent-client-relationships/relationships/inactive/service/HMRC-MTD-IT"))
        .willReturn(
          aResponse()
            .withStatus(404)))

  def givenInactiveVATRelationships(arn: Arn) =
    stubFor(
      get(urlEqualTo(s"/agent-client-relationships/relationships/inactive/service/HMRC-MTD-VAT"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(
              s"""
                 |[{
                 |   "arn":"${arn.value}",
                 |   "dateTo":"2015-09-21",
                 |   "dateFrom":"2015-09-10",
                 |   "referenceNumber":"101747696"
                 |},
                 |{  "arn":"${arn.value}",
                 |   "dateTo":"2018-09-24",
                 |   "dateFrom":"2015-09-10",
                 |   "referenceNumber":"101747641"
                 |}]""".stripMargin
            )
        )
    )

  def givenInactiveVATRelationshipsNotFound =
    stubFor(
      get(urlEqualTo(s"/agent-client-relationships/relationships/inactive/service/HMRC-MTD-VAT"))
        .willReturn(
          aResponse()
            .withStatus(404)))

  def givenCancelledAuthorisationItsa(arn: Arn, nino:Nino, status: Int) =
    stubFor(
      delete(urlEqualTo(s"/agent-client-relationships/agent/${arn.value}/service/HMRC-MTD-IT/client/NI/${nino.value}"))
        .willReturn(
          aResponse()
            .withStatus(status)
        )
    )

  def givenCancelledAuthorisationVat(arn: Arn, vrn:Vrn, status: Int) =
    stubFor(
      delete(urlEqualTo(s"/agent-client-relationships/agent/${arn.value}/service/HMRC-MTD-VAT/client/VRN/${vrn.value}"))
        .willReturn(
          aResponse()
            .withStatus(status)
        )
    )
}
