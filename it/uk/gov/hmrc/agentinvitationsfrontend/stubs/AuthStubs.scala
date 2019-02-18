package uk.gov.hmrc.agentinvitationsfrontend.stubs

import com.github.tomakehurst.wiremock.client.WireMock._
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.{AffinityGroup, ConfidenceLevel}
import uk.gov.hmrc.http.SessionKeys

trait AuthStubs {
  me: WireMockSupport =>

  case class Enrolment(serviceName: String, identifierName: String, identifierValue: String)

  def authorisedAsValidAgent[A](request: FakeRequest[A], arn: String, sessionId: String = "session12345"): FakeRequest[A] =
    authenticatedAgent(request, Enrolment("HMRC-AS-AGENT", "AgentReferenceNumber", arn), sessionId)

  def authorisedAsValidClientITSA[A](request: FakeRequest[A], mtditid: String) =
    authenticatedClient(request, "Individual", Enrolment("HMRC-MTD-IT", "MTDITID", mtditid))

  def authorisedAsValidClientAFI[A](request: FakeRequest[A], clientId: String) =
    authenticatedClient(request, "Individual", Enrolment("HMRC-NI", "NINO", clientId))

  def authorisedAsValidClientVAT[A](request: FakeRequest[A], clientId: String) =
    authenticatedClient(request, "Organisation", Enrolment("HMRC-MTD-VAT", "VRN", clientId))

  def authorisedAsAnyIndividualClient[A](request: FakeRequest[A], sessionId: String = "clientSession12345"): FakeRequest[A] = {
    givenAuthorisedFor(
      """
        |{
        |"authorise": [ {
        |  "authProviders": [ "GovernmentGateway" ]
        |},
        |{
        |  "$or" : [ {
        |      "affinityGroup" : "Individual"
        |    }, {
        |      "affinityGroup" : "Organisation"
        |    } ]
        |} ],
        |  "retrieve": [ "allEnrolments" ]
        |}
       """.stripMargin,
      s"""
         |{
         |  "affinityGroup":"Individual",
         |  "confidenceLevel":200,
         |  "allEnrolments":
         |  [
         |    {
         |      "key": "HMRC-MTD-IT",
         |      "identifiers": [
         |         {"key":"MTDITID", "value": "ABCDEF123456789"}
         |      ]
         |     },
         |     {
         |      "key": "HMRC-NI",
         |      "identifiers": [
         |         {"key":"NINO", "value": "AB123456A"}
         |      ]
         |     },
         |     {
         |      "key": "HMRC-MTD-VAT",
         |      "identifiers": [
         |         {"key":"VRN", "value": "101747696"}
         |      ]
         |     }
         |  ]
         |}
          """.stripMargin
    )
    request.withSession(SessionKeys.authToken -> "Client Bearer XYZ", SessionKeys.sessionId -> sessionId)
  }

  def authorisedAsAnyOrganisationClient[A](request: FakeRequest[A], sessionId: String = "clientSession-12345"): FakeRequest[A] = {
    givenAuthorisedFor(
      """
        |{
        |"authorise": [ {
        |  "authProviders": [ "GovernmentGateway" ]
        |},
        |{
        |  "$or" : [ {
        |      "affinityGroup" : "Individual"
        |    }, {
        |      "affinityGroup" : "Organisation"
        |    } ]
        |} ],
        |  "retrieve": [ "allEnrolments" ]
        |}
       """.stripMargin,
      s"""
         |{
         |  "affinityGroup":"Organisation",
         |  "confidenceLevel":50,
         |  "allEnrolments":
         |  [
         |     {
         |      "key": "HMRC-MTD-VAT",
         |      "identifiers": [
         |         {"key":"VRN", "value": "101747696"}
         |      ]
         |     }
         |  ]
         |}
          """.stripMargin
    )
    request.withSession(SessionKeys.authToken -> "Bearer XYZ", SessionKeys.sessionId -> sessionId)
  }

  def authorisedAsAnyClientFalse[A](request: FakeRequest[A], sessionId: String = "clientSession-1234567"): FakeRequest[A] = {
    givenAuthorisedFor(
      """
        |{
        |"authorise": [ {
        |  "authProviders": [ "GovernmentGateway" ]
        |},
        |{
        |  "$or" : [ {
        |      "affinityGroup" : "Individual"
        |    }, {
        |      "affinityGroup" : "Organisation"
        |    } ]
        |} ],
        |  "retrieve": [ "allEnrolments" ]
        |}
      """.stripMargin,
      s"""
         |{
         |  "affinityGroup":"Individual",
         |  "confidenceLevel": 200,
         |  "allEnrolments":
         |  [
         |    {
         |      "key": "HMRC-MTD-IT",
         |      "identifiers": [
         |         {"key":"VRN", "value": "101747696"}
         |      ]
         |     },
         |     {
         |      "key": "HMRC-NI",
         |      "identifiers": [
         |         {"key":"VRN", "value": "ABCDEF123456789"}
         |      ]
         |     },
         |     {
         |      "key": "HMRC-MTD-VAT",
         |      "identifiers": [
         |         {"key":"NINO", "value": "101747696"}
         |      ]
         |     }
         |  ]
         |}
          """.stripMargin
    )
    request.withSession(SessionKeys.authToken -> "Bearer XYZ", SessionKeys.sessionId -> sessionId)
  }

  def authenticatedClient[A](
    request: FakeRequest[A],
    affinityGroup: String,
    enrolment: Enrolment,
    confidenceLevel: String = "200"): FakeRequest[A] = {
    givenAuthorisedFor(
      s"""
         |{
         |  "authorise": [
         |    { "identifiers":[], "state":"Activated", "enrolment": "${enrolment.serviceName}" },
         |    { "authProviders": ["GovernmentGateway"] }
         |    ],
         |  "retrieve":["authorisedEnrolments"]
         |}
           """.stripMargin,
      s"""
         |{
         |  "affinityGroup":"$affinityGroup",
         |  "authorisedEnrolments": [
         |    { "key":"${enrolment.serviceName}", "identifiers": [
         |      {"key":"${enrolment.identifierName}", "value": "${enrolment.identifierValue}"}
         |    ]}
         |  ],
         |  "confidenceLevel":$confidenceLevel}
          """.stripMargin
    )
    request.withSession(SessionKeys.authToken -> "Bearer XYZ")
  }

  def authenticatedAgent[A](request: FakeRequest[A], enrolment: Enrolment, sessionId: String): FakeRequest[A] = {
    givenAuthorisedFor(
      s"""
         |{
         |  "authorise": [
         |    { "identifiers":[], "state":"Activated", "enrolment": "${enrolment.serviceName}" },
         |    { "authProviders": ["GovernmentGateway"] }
         |  ],
         |  "retrieve":["authorisedEnrolments"]
         |}
           """.stripMargin,
      s"""
         |{
         |"authorisedEnrolments": [
         |  { "key":"${enrolment.serviceName}", "identifiers": [
         |    {"key":"${enrolment.identifierName}", "value": "${enrolment.identifierValue}"}
         |  ]}
         |]}
          """.stripMargin
    )
    request.withSession(SessionKeys.authToken -> "Bearer XYZ", SessionKeys.sessionId -> sessionId)
  }

  def givenUnauthorisedWith(mdtpDetail: String): Unit =
    stubFor(
      post(urlEqualTo("/auth/authorise"))
        .willReturn(
          aResponse()
            .withStatus(401)
            .withHeader("WWW-Authenticate", s"""MDTP detail="$mdtpDetail"""")))

  def givenAuthorisedFor(payload: String, responseBody: String): Unit =
    stubFor(
      post(urlEqualTo("/auth/authorise"))
        .withRequestBody(equalToJson(payload, true, true))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withHeader("Content-Type", "application/json")
            .withBody(responseBody)))

  def givenUnauthorisedForInsufficientEnrolments(): Unit =
    stubFor(
      post(urlEqualTo("/auth/authorise"))
        .willReturn(
          aResponse()
            .withStatus(401)
            .withHeader("WWW-Authenticate", "MDTP detail=\"InsufficientEnrolments\"")))

  def givenUnauthorisedForInsufficientConfidenceLevel(): Unit =
    stubFor(
      post(urlEqualTo("/auth/authorise"))
        .willReturn(
          aResponse()
            .withStatus(401)
            .withHeader("WWW-Authenticate", "MDTP detail=\"InsufficientConfidenceLevel\"")))

  def verifyAuthoriseAttempt(): Unit =
    verify(1, postRequestedFor(urlEqualTo("/auth/authorise")))

  def verify2AuthoriseAttempt(): Unit =
    verify(2, postRequestedFor(urlEqualTo("/auth/authorise")))

  def verifyNoAuthoriseAttempt(): Unit =
    verify(0, postRequestedFor(urlEqualTo("/auth/authorise")))

}
