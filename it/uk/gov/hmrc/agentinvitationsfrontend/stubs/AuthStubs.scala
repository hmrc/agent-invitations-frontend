package uk.gov.hmrc.agentinvitationsfrontend.stubs

import com.github.tomakehurst.wiremock.client.WireMock._
import play.api.http.Status.{NO_CONTENT, OK}
import play.api.libs.json.Json
import play.api.test.FakeRequest
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport
import uk.gov.hmrc.agentmtdidentifiers.model.{SuspensionDetails, TrustTaxIdentifier, Urn, Utr}
import uk.gov.hmrc.http.{HeaderCarrier, SessionKeys}

trait AuthStubs extends AfiRelationshipStub {
  me: WireMockSupport =>

  case class Enrolment(serviceName: String, identifierName: String, identifierValue: String)

  def authorisedAsValidAgent[A](request: FakeRequest[A], arn: String, suspended: Boolean = false)(implicit hc: HeaderCarrier): FakeRequest[A] =
    authenticatedAgent(request, Enrolment("HMRC-AS-AGENT", "AgentReferenceNumber", arn))

  def authorisedAsAnyAgent[A](request: FakeRequest[A])(implicit hc: HeaderCarrier): FakeRequest[A] =
    anyAgent(request)

  def authorisedAsValidClientITSA[A](request: FakeRequest[A], mtditid: String) =
    authenticatedClient(request, "Individual", Enrolment("HMRC-MTD-IT", "MTDITID", mtditid))

  def authorisedAsValidClientAFI[A](request: FakeRequest[A], clientId: String) =
    authenticatedClient(request, "Individual", Enrolment("HMRC-NI", "NINO", clientId))

  def authorisedAsValidClientVAT[A](request: FakeRequest[A], clientId: String) =
    authenticatedClient(request, "Organisation", Enrolment("HMRC-MTD-VAT", "VRN", clientId))

  def authorisedAsIndividualWithCredentialRetrieval[A](request: FakeRequest[A], providerId: String)(implicit hc: HeaderCarrier): FakeRequest[A] = {
    givenAuthorisedFor(
      payload = """
                  |{
                  |"authorise": [ {
                  |  "authProviders": [ "GovernmentGateway" ]
                  |}],
                  |  "retrieve": [ "affinityGroup", "optionalCredentials"]
                  |}
       """.stripMargin,
      responseBody = s"""
                        |{
                        |  "affinityGroup":"Individual",
                        |  "optionalCredentials": {"providerId": "$providerId", "providerType": "GovernmentGateway"}
                        |}
          """.stripMargin
    )
    request.withSession(
      SessionKeys.authToken -> "Client Bearer XYZ",
      SessionKeys.sessionId -> hc.sessionId.map(_.value).getOrElse("ClientSession123456")
    )
  }

  def authorisedAsIndividualClientWithNoSupportedEnrolments[A](request: FakeRequest[A], confidenceLevel: Int = 250, hasNino: Boolean = true)(implicit
    hc: HeaderCarrier
  ): FakeRequest[A] = {
    val ninoRetrieval = if (hasNino) ",\"nino\": \"AB123456A\"" else ""
    givenAuthorisedFor(
      payload = """
                  |{
                  |"authorise": [ {
                  |  "authProviders": [ "GovernmentGateway" ]
                  |}],
                  |  "retrieve": [ "affinityGroup", "confidenceLevel", "allEnrolments", "nino" ]
                  |}
       """.stripMargin,
      responseBody = s"""
                        |{
                        |  "affinityGroup":"Individual",
                        |  "confidenceLevel":$confidenceLevel,
                        |  "allEnrolments":
                        |  [
                        |    {
                        |      "key": "HMCE-VATVAR-ORG",
                        |      "identifiers": [
                        |         {"key":"VRN", "value": "101747696"}
                        |      ]
                        |     }
                        |  ]
                        |  $ninoRetrieval
                        |}
          """.stripMargin
    )
    request.withSession(
      SessionKeys.authToken -> "Client Bearer XYZ",
      SessionKeys.sessionId -> hc.sessionId.map(_.value).getOrElse("ClientSession123456")
    )
  }

  def authorisedAsIndividualClientWithSomeSupportedEnrolments[A](request: FakeRequest[A], confidenceLevel: Int = 250, hasNino: Boolean = true)(
    implicit hc: HeaderCarrier
  ): FakeRequest[A] = {
    val ninoRetrieval = if (hasNino) ",\"nino\": \"AB123456A\"" else ""
    givenAuthorisedFor(
      payload = """
                  |{
                  |"authorise": [ {
                  |  "authProviders": [ "GovernmentGateway" ]
                  |}],
                  |  "retrieve": [ "affinityGroup", "confidenceLevel", "allEnrolments", "nino" ]
                  |}
       """.stripMargin,
      responseBody = s"""
                        |{
                        |  "affinityGroup":"Individual",
                        |  "confidenceLevel":$confidenceLevel,
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
                        |  $ninoRetrieval
                        |}
          """.stripMargin
    )
    request.withSession(
      SessionKeys.authToken -> "Client Bearer XYZ",
      SessionKeys.sessionId -> hc.sessionId.map(_.value).getOrElse("ClientSession123456")
    )
  }

  def authorisedAsIndividualClientWithAllSupportedEnrolments[A](request: FakeRequest[A], confidenceLevel: Int = 250, hasNino: Boolean = true)(implicit
    hc: HeaderCarrier
  ): FakeRequest[A] = {
    val ninoRetrieval = if (hasNino) ",\"nino\": \"AB123456A\"" else ""
    givenAuthorisedFor(
      payload = """
                  |{
                  |"authorise": [ {
                  |  "authProviders": [ "GovernmentGateway" ]
                  |}],
                  |  "retrieve": [ "affinityGroup", "confidenceLevel", "allEnrolments", "nino" ]
                  |}
       """.stripMargin,
      responseBody = s"""
                        |{
                        |  "affinityGroup":"Individual",
                        |  "confidenceLevel":$confidenceLevel,
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
                        |     },
                        |     {
                        |      "key": "HMRC-CGT-PD",
                        |      "identifiers": [
                        |         {"key":"CGTPDRef", "value": "XMCGTP485579071"}
                        |      ]
                        |     },
                        |     {
                        |      "key": "HMRC-PPT-ORG",
                        |      "identifiers": [
                        |         {"key":"EtmpRegistrationNumber", "value": "XAPPT0000012345"}
                        |      ]
                        |     }
                        |  ]
                        |  $ninoRetrieval
                        |}
          """.stripMargin
    )
    request.withSession(
      SessionKeys.authToken -> "Client Bearer XYZ",
      SessionKeys.sessionId -> hc.sessionId.map(_.value).getOrElse("ClientSession123456")
    )
  }

  def authorisedAsAnyCGTIndividualClientWithLowCL[A](request: FakeRequest[A], hasNino: Boolean = true)(implicit hc: HeaderCarrier): FakeRequest[A] = {
    val ninoRetrieval = if (hasNino) ",\"nino\": \"AB123456A\"" else ""
    givenAuthorisedFor(
      payload = """
                  |{
                  |"authorise": [ {
                  |  "authProviders": [ "GovernmentGateway" ]
                  |}],
                  |  "retrieve": [ "affinityGroup", "confidenceLevel", "allEnrolments", "nino" ]
                  |}
       """.stripMargin,
      responseBody = s"""
                        |{
                        |  "affinityGroup":"Individual",
                        |  "confidenceLevel": 50,
                        |  "allEnrolments":
                        |  [
                        |    {
                        |      "key": "HMRC-CGT-PD",
                        |      "identifiers": [
                        |         {"key":"CGTPDRef", "value": "ABCDEF123456789"}
                        |      ]
                        |     }
                        |  ]
                        |  $ninoRetrieval
                        |}
          """.stripMargin
    )
    request.withSession(
      SessionKeys.authToken -> "Client Bearer XYZ",
      SessionKeys.sessionId -> hc.sessionId.map(_.value).getOrElse("ClientSession123456")
    )
  }

  def authorisedAsAnyOrganisationClient[A](request: FakeRequest[A])(implicit hc: HeaderCarrier): FakeRequest[A] = {
    givenAuthorisedFor(
      """
      {
        "authorise": [ {
        "authProviders": [ "GovernmentGateway" ]
       }],
        "retrieve": [ "affinityGroup", "confidenceLevel", "allEnrolments" ]
      }
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
    request.withSession(SessionKeys.authToken -> "Bearer XYZ", SessionKeys.sessionId -> hc.sessionId.map(_.value).getOrElse("clientSession123456"))
  }

  def authorisedAsOrganisationTrustClient[A](taxId: TrustTaxIdentifier)(request: FakeRequest[A])(implicit hc: HeaderCarrier): FakeRequest[A] = {
    val (enrolKey, enrolType, enrolValue) = taxId match {
      case Utr(utr) => ("HMRC-TERS-ORG", "SAUTR", utr)
      case Urn(urn) => ("HMRC-TERSNT-ORG", "URN", urn)
    }
    givenAuthorisedFor(
      """
      {
        "authorise": [ {
        "authProviders": [ "GovernmentGateway" ]
       }],
        "retrieve": [ "affinityGroup", "confidenceLevel", "allEnrolments" ]
      }
       """.stripMargin,
      s"""
         |{
         |  "affinityGroup":"Organisation",
         |  "confidenceLevel":50,
         |  "allEnrolments":
         |  [
         |     {
         |      "key": "$enrolKey",
         |      "identifiers": [
         |         {"key":"$enrolType", "value": "$enrolValue"}
         |      ]
         |     }
         |  ]
         |}
          """.stripMargin
    )
    request.withSession(SessionKeys.authToken -> "Bearer XYZ", SessionKeys.sessionId -> hc.sessionId.map(_.value).getOrElse("clientSession123456"))
  }

  def authorisedAsITSAOrganisationClient[A](request: FakeRequest[A], confidenceLevel: Int = 250)(implicit hc: HeaderCarrier): FakeRequest[A] = {
    givenAuthorisedFor(
      """
      {
        "authorise": [ {
        "authProviders": [ "GovernmentGateway" ]
       }],
        "retrieve": [ "affinityGroup", "confidenceLevel", "allEnrolments" ]
      }
       """.stripMargin,
      s"""
         |{
         |  "affinityGroup":"Organisation",
         |  "confidenceLevel":$confidenceLevel,
         |  "allEnrolments":
         |  [
         |     {
         |      "key": "HMRC-MTD-IT",
         |      "identifiers": [
         |         {"key":"MTDITID", "value": "ABCDEF123456789"}
         |      ]
         |     },
         |     {
         |     "key": "HMRC-MTD-VAT",
         |     "identifiers": [
         |     { "key":"VRN", "value": "101747696"}
         |     ]
         |     }
         |  ]
         |}
          """.stripMargin
    )
    request.withSession(SessionKeys.authToken -> "Bearer XYZ", SessionKeys.sessionId -> hc.sessionId.map(_.value).getOrElse("clientSession123456"))
  }

  def authenticatedAnyClientWithAffinity[A](request: FakeRequest[A])(implicit hc: HeaderCarrier): FakeRequest[A] = {
    givenAuthorisedFor(
      """
      {
        "authorise": [ {
        "authProviders": [ "GovernmentGateway" ]
       }],
        "retrieve": [ "affinityGroup", "confidenceLevel", "allEnrolments" ]
      }
       """.stripMargin,
      s"""
         |{
         |  "affinityGroup": "Agent",
         |  "confidenceLevel": 250,
         |  "allEnrolments": []
         |}
          """.stripMargin
    )
    request.withSession(SessionKeys.authToken -> "Bearer XYZ", SessionKeys.sessionId -> hc.sessionId.map(_.value).getOrElse("clientSession12345"))
  }

  def authenticatedClient[A](
    request: FakeRequest[A],
    affinityGroup: String,
    enrolment: Enrolment,
    confidenceLevel: String = "250"
  ): FakeRequest[A] = {
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

  def authenticatedAgent[A](request: FakeRequest[A], enrolment: Enrolment, suspended: Boolean = false)(implicit hc: HeaderCarrier): FakeRequest[A] = {
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
    // suspension check
    if (suspended) givenSuspended() else givenNotSuspended()
    request.withSession(SessionKeys.authToken -> "Bearer XYZ", SessionKeys.sessionId -> hc.sessionId.map(_.value).getOrElse("session12345"))
  }

  def anyAgent[A](request: FakeRequest[A])(implicit hc: HeaderCarrier): FakeRequest[A] = {
    givenAuthorisedFor(
      s"""
         |{
         |  "authorise": [
         |    { "authProviders": ["GovernmentGateway"] }
         |  ],
         |  "retrieve":["affinityGroup"]
         |}
           """.stripMargin,
      s"""
         |{
         |  "affinityGroup":"Agent"
         |}
          """.stripMargin
    )
    request.withSession(SessionKeys.authToken -> "Bearer XYZ", SessionKeys.sessionId -> hc.sessionId.map(_.value).getOrElse("session12345"))
  }

  def givenSuspended() =
    stubFor(
      get(urlPathMatching("""\/agent\-client\-authorisation\/client\/suspension\-details\/.*"""))
        .willReturn(
          aResponse()
            .withStatus(OK)
            .withHeader("Content-Type", "application/json")
            .withBody(Json.toJson(SuspensionDetails(suspensionStatus = true, Some(Set("ALL")))).toString)
        )
    )

  def givenNotSuspended() =
    stubFor(
      get(urlPathMatching("""\/agent\-client\-authorisation\/client\/suspension\-details\/.*"""))
        .willReturn(aResponse().withStatus(NO_CONTENT))
    )

  def givenUnauthorisedWith(mdtpDetail: String) =
    stubFor(
      post(urlEqualTo("/auth/authorise"))
        .willReturn(
          aResponse()
            .withStatus(401)
            .withHeader("WWW-Authenticate", s"""MDTP detail="$mdtpDetail"""")
        )
    )

  def givenAuthorisedFor(payload: String, responseBody: String) =
    stubFor(
      post(urlEqualTo("/auth/authorise"))
        .withRequestBody(equalToJson(payload, true, true))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withHeader("Content-Type", "application/json")
            .withBody(responseBody)
        )
    )

  def givenUnauthorisedForInsufficientEnrolments() =
    stubFor(
      post(urlEqualTo("/auth/authorise"))
        .willReturn(
          aResponse()
            .withStatus(401)
            .withHeader("WWW-Authenticate", "MDTP detail=\"InsufficientEnrolments\"")
        )
    )

  def givenUnauthorisedForInsufficientConfidenceLevel() =
    stubFor(
      post(urlEqualTo("/auth/authorise"))
        .willReturn(
          aResponse()
            .withStatus(401)
            .withHeader("WWW-Authenticate", "MDTP detail=\"InsufficientConfidenceLevel\"")
        )
    )

  def givenUnauthorisedForUnsupportedAffinityGroup() =
    stubFor(
      post(urlEqualTo("/auth/authorise"))
        .willReturn(
          aResponse()
            .withStatus(401)
            .withHeader("WWW-Authenticate", "MDTP detail=\"UnsupportedAffinityGroup\"")
        )
    )

  def verifyAuthoriseAttempt() =
    verify(1, postRequestedFor(urlEqualTo("/auth/authorise")))

  def verify2AuthoriseAttempt() =
    verify(2, postRequestedFor(urlEqualTo("/auth/authorise")))

  def verifyNoAuthoriseAttempt() =
    verify(0, postRequestedFor(urlEqualTo("/auth/authorise")))

}
