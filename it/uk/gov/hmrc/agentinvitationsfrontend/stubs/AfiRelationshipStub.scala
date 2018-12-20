package uk.gov.hmrc.agentinvitationsfrontend.stubs

import java.time.LocalDateTime

import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.stubbing.StubMapping
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport
import uk.gov.hmrc.agentmtdidentifiers.model.Arn

trait AfiRelationshipStub {
  me: WireMockSupport =>

  def givenAfiRelationshipIsActive(arn: Arn, service: String, clientId: String, fromCesa: Boolean): Unit =
    stubFor(
      get(urlEqualTo(s"/agent-fi-relationship/relationships/service/$service/clientId/$clientId"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |[{
                         |  "arn" : "${arn.value}",
                         |  "service" : "$service",
                         |  "clientId" : "$clientId",
                         |  "relationshipStatus" : "ACTIVE",
                         |  "startDate" : "2017-12-08T15:21:51.040",
                         |  "fromCesa" : $fromCesa
                         |}]""".stripMargin)))

  def givenAfiRelationshipNotFound(service: String, clientId: String): Unit =
    stubFor(
      get(urlEqualTo(s"/agent-fi-relationship/relationships/service/$service/clientId/$clientId"))
        .willReturn(aResponse()
          .withStatus(404)))

  def givenTerminateAllAfiRelationshipsSucceeds(service: String, clientId: String): Unit =
    stubFor(
      delete(urlEqualTo(s"/agent-fi-relationship/relationships/service/$service/clientId/$clientId"))
        .willReturn(aResponse()
          .withStatus(200)))

  def givenTerminateAllAfiRelationshipsFails(service: String, clientId: String): Unit =
    stubFor(
      delete(urlEqualTo(s"/agent-fi-relationship/relationships/service/$service/clientId/$clientId"))
        .willReturn(aResponse()
          .withStatus(500)))

  def verifyTerminateAfiRelationshipsAttempt(service: String, clientId: String): Unit =
    verify(
      1,
      deleteRequestedFor(urlEqualTo(s"/agent-fi-relationship/relationships/service/$service/clientId/$clientId")))

  def givenTerminateAfiRelationshipSucceeds(arn: Arn, service: String, clientId: String): Unit =
    stubFor(
      delete(urlEqualTo(s"/agent-fi-relationship/relationships/agent/${arn.value}/service/$service/client/$clientId"))
        .willReturn(
          aResponse()
            .withStatus(200)
        ))

  def givenTestOnlyTerminateAfiRelationshipSucceeds(arn: Arn, service: String, clientId: String): Unit =
    stubFor(
      delete(urlEqualTo(s"/agent-fi-relationship/test-only/relationships/agent/${arn.value}/service/$service/client/$clientId"))
        .willReturn(
          aResponse()
            .withStatus(200)
        ))


  def givenTerminateAfiRelationshipFails(arn: Arn, service: String, clientId: String): Unit =
    stubFor(
      delete(urlEqualTo(s"/agent-fi-relationship/relationships/agent/${arn.value}/service/$service/client/$clientId"))
        .willReturn(
          aResponse()
            .withStatus(500)
        ))

  def givenCreateAfiRelationshipSucceeds(arn: Arn, service: String, clientId: String): Unit =
    stubFor(
      put(urlEqualTo(s"/agent-fi-relationship/relationships/agent/${arn.value}/service/$service/client/$clientId"))
        .willReturn(aResponse()
          .withStatus(201)))

  def givenTestOnlyCreateAfiRelationshipSucceeds(arn: Arn, service: String, clientId: String): Unit =
    stubFor(
      put(urlEqualTo(s"/agent-fi-relationship/test-only/relationships/agent/${arn.value}/service/$service/client/$clientId"))
        .willReturn(aResponse()
          .withStatus(201)))

  def givenCreateAfiRelationshipFails(arn: Arn, service: String, clientId: String): Unit =
    stubFor(
      put(urlEqualTo(s"/agent-fi-relationship/relationships/agent/${arn.value}/service/$service/client/$clientId"))
        .willReturn(aResponse()
          .withStatus(500)))

  def givenTestOnlyCreateAfiRelationshipFails(arn: Arn, service: String, clientId: String): Unit =
    stubFor(
      put(urlEqualTo(s"/agent-fi-relationship/test-only/relationships/agent/${arn.value}/service/$service/client/$clientId"))
        .willReturn(aResponse()
          .withStatus(500)))

  def givenInactiveAfiRelationship(arn: Arn) =
    stubFor(
      get(urlEqualTo(s"/agent-fi-relationship/relationships/inactive"))
        .willReturn(aResponse()
          .withStatus(200)
          .withBody(
            s"""
               |[{
               |   "arn":"${arn.value}",
               |   "endDate":"2015-09-21T15:21:51.040",
               |   "clientId":"AB123456A"
               |},
               |{  "arn":"${arn.value}",
               |   "endDate":"2018-09-24T15:21:51.040",
               |   "clientId":"GZ753451B"
               |}]""".stripMargin
          )
    ))

  def givenInactiveAfiRelationshipNotFound =
    stubFor(
      get(urlEqualTo(s"/agent-fi-relationship/relationships/inactive"))
        .willReturn(aResponse()
          .withStatus(404)
    ))
}
