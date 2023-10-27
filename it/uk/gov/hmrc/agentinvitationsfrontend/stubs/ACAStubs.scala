package uk.gov.hmrc.agentinvitationsfrontend.stubs

import com.github.tomakehurst.wiremock.client.WireMock.{put, _}
import com.github.tomakehurst.wiremock.stubbing.StubMapping

import java.time.{Instant, LocalDate, LocalDateTime, ZoneOffset}
import play.api.libs.json.Json
import uk.gov.hmrc.agentmtdidentifiers.model.SuspensionDetails
import uk.gov.hmrc.agentinvitationsfrontend.UriPathEncoding._
import uk.gov.hmrc.agentinvitationsfrontend.models.{CbcClient, ClientType, Pillar2Client, Pillar2Subscription, PptClient}
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.Nino

trait ACAStubs {
  me: WireMockSupport =>

  def givenAgentReference(arn: Arn, uid: String, clientType: ClientType): Any =
    stubFor(
      post(urlEqualTo(
        s"/agent-client-authorisation/agencies/references/arn/${encodePathSegment(arn.value)}/clientType/$clientType"))
        .willReturn(
          aResponse()
            .withStatus(201)
            .withHeader("location", s"/invitations/$clientType/$uid/99-with-flake")))

  def givenAgentReferenceRecordExistsForUid(arn: Arn, uid: String): StubMapping =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/agencies/references/uid/$uid"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |{
                         |  "arn" : "${arn.value}",
                         |  "uid" : "$uid",
                         |  "normalisedAgentNames" : ["99-with-flake", "My-Agency"]
                         |}""".stripMargin)))

  def givenAgentReferenceRecordNotFoundForUid(uid: String): StubMapping =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/agencies/references/uid/$uid"))
        .willReturn(aResponse()
          .withStatus(404)))

  def givenAgentReferenceRecordExistsForArn(arn: Arn, uid: String): StubMapping =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/agencies/references/arn/${arn.value}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |{
                         |  "arn" : "${arn.value}",
                         |  "uid" : "$uid",
                         |  "normalisedAgentNames" : ["99-with-flake"]
                         |}""".stripMargin)))

  def givenAgentReferenceRecordNotFoundForArn(arn: Arn): StubMapping =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/agencies/references/arn/${arn.value}"))
        .willReturn(aResponse()
          .withStatus(404)))

  def givenAllInvitationIdsByStatus(uid: String, status: String, isAltItsa: Boolean = false): StubMapping =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/clients/invitations/uid/$uid"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |[
                         |  {
                         |    "invitationId": {
                         |      "value": "AG1UGUKTPNJ7W"
                         |    },
                         |    "expiryDate": "9999-11-01",
                         |    "status": "$status",
                         |    "isRelationshipEnded" : false,
                         |    "events" : [
                         |        {
                         |           "time" : 1595502969891,
                         |           "status" : "$status"
                         |        }
                         |    ],
                         |    "isAltItsa":$isAltItsa
                         |  },
                         |  {
                         |    "invitationId": {
                         |      "value": "B9SCS2T4NZBAX"
                         |    },
                         |    "expiryDate": "9999-03-05",
                         |    "status": "$status",
                         |    "isRelationshipEnded" : false,
                         |    "events": [
                         |        {
                         |           "time": 1595502969892,
                         |           "status": "$status"
                         |        }
                         |    ],
                         |    "isAltItsa":$isAltItsa
                         |  },
                         |  {
                         |    "invitationId": {
                         |      "value": "CZTW1KY6RTAAT"
                         |    },
                         |    "expiryDate": "9999-12-25",
                         |    "status": "$status",
                         |    "isRelationshipEnded" : false,
                         |    "events" : [
                         |        {
                         |            "time" : 1595502969893,
                         |            "status" : "$status"
                         |        }
                         |    ],
                         |    "isAltItsa":$isAltItsa
                         |  }
                         |]
                         |""".stripMargin)
        )
    )

  def givenAllInvitationIdsWithMixedStatus(uid: String, mostRecentStatus: String = "Cancelled"): StubMapping =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/clients/invitations/uid/$uid"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |[
                         |  {
                         |    "invitationId": {
                         |      "value": "AG1UGUKTPNJ7W"
                         |    },
                         |    "expiryDate": "9999-11-01",
                         |    "status": "Accepted",
                         |    "isRelationshipEnded" : false,
                         |    "events" : [
                         |        {
                         |            "time" : 1595502969898,
                         |            "status" : "Accepted"
                         |        }
                         |    ],
                         |    "isAltItsa":false
                         |  },
                         |  {
                         |    "invitationId": {
                         |      "value": "B9SCS2T4NZBAX"
                         |    },
                         |    "expiryDate": "9999-03-05",
                         |    "status": "$mostRecentStatus",
                         |    "isRelationshipEnded" : false,
                         |    "events" : [
                         |        {
                         |            "time" : 1595502969899,
                         |            "status" : "$mostRecentStatus"
                         |        }
                         |    ],
                         |    "isAltItsa":false
                         |  },
                         |  {
                         |    "invitationId": {
                         |      "value": "CZTW1KY6RTAAT"
                         |    },
                         |    "expiryDate": "9999-12-25",
                         |    "status": "Cancelled",
                         |    "isRelationshipEnded" : false,
                         |    "events" : [
                         |        {
                         |            "time" : 1595502969890,
                         |            "status" : "Cancelled"
                         |        }
                         |    ],
                         |    "isAltItsa":false
                         |  }
                         |]
                         |""".stripMargin)
        )
    )

  def givenAllInvitationIdsWithTrustByStatus(uid: String, status: String): StubMapping =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/clients/invitations/uid/$uid"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |[
                         |  {
                         |    "invitationId": {
                         |      "value": "AG1UGUKTPNJ7W"
                         |    },
                         |    "expiryDate": "9999-11-01",
                         |    "status": "$status",
                         |    "isRelationshipEnded" : false,
                         |    "events" : [
                         |        {
                         |            "time" : 1595502969897,
                         |            "status" : "$status"
                         |        }
                         |    ],
                         |    "isAltItsa":false
                         |  },
                         |  {
                         |    "invitationId": {
                         |      "value": "B9SCS2T4NZBAX"
                         |    },
                         |     "expiryDate": "9999-03-05",
                         |    "status": "$status",
                         |    "isRelationshipEnded" : false,
                         |    "events" : [
                         |        {
                         |            "time" : 1595502969896,
                         |            "status" : "$status"
                         |        }
                         |    ],
                         |    "isAltItsa":false
                         |  },
                         |  {
                         |    "invitationId": {
                         |      "value": "CZTW1KY6RTAAT"
                         |    },
                         |    "expiryDate": "9999-12-25",
                         |    "status": "$status",
                         |    "isRelationshipEnded" : false,
                         |    "events" : [
                         |        {
                         |            "time" : 1595502969895,
                         |            "status" : "$status"
                         |        }
                         |    ],
                         |    "isAltItsa":false
                         |  },
                         |  {
                         |    "invitationId": {
                         |      "value": "DF99K6PXSBHTF"
                         |    },
                         |    "expiryDate": "9999-12-25",
                         |    "status": "$status",
                         |    "isRelationshipEnded" : false,
                         |    "events" : [
                         |        {
                         |            "time" : 1595502969894,
                         |            "status" : "$status"
                         |        }
                         |    ],
                         |    "isAltItsa":false
                         |  }
                         |]
                         |""".stripMargin)
        )
    )


  def givenAllInvitationIdsByStatusReturnsSomeDuplicated(uid: String, status: String): StubMapping =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/clients/invitations/uid/$uid?status=$status"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |[
                         |  {
                         |    "invitationId": {
                         |      "value": "AG1UGUKTPNJ7W"
                         |    },
                         |    "expiryDate": "9999-11-01",
                         |    "isAltItsa":false
                         |  },
                         |  {
                         |    "invitationId": {
                         |      "value": "AG1UGUKTPNJ7Z"
                         |    },
                         |    "expiryDate": "9999-11-01",
                         |    "isAltItsa":false
                         |  },
                         |  {
                         |    "invitationId": {
                         |      "value": "CZTW1KY6RTAAT"
                         |    },
                         |    "expiryDate": "9999-12-25",
                         |    "isAltItsa":false
                         |  }
                         |]
                         |""".stripMargin)
        )
    )

  def givenAllInvitationIdsByStatusReturnsSomeExpired(uid: String, status: String): StubMapping =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/clients/invitations/uid/$uid?status=$status"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |[
                         |  {
                         |    "invitationId": {
                         |      "value": "AG1UGUKTPNJ7W"
                         |    },
                         |    "expiryDate": "9999-11-01"
                         |  },
                         |  {
                         |    "invitationId": {
                         |      "value": "B9SCS2T4NZBAX"
                         |    },
                         |     "expiryDate": "2000-03-05"
                         |  },
                         |  {
                         |    "invitationId": {
                         |      "value": "CZTW1KY6RTAAT"
                         |    },
                         |    "expiryDate": "2000-12-25"
                         |  }
                         |]
                         |""".stripMargin)
        )
    )

  def givenAllPendingInvitationIdsReturnsFakePending(uid: String): StubMapping =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/clients/invitations/uid/$uid?status=Pending"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |[
                         |  {
                         |    "invitationId": {
                         |      "value": "EG1UGUKTPNJ7W"
                         |    },
                         |    "expiryDate": "2018-11-01"
                         |  },
                         |  {
                         |    "invitationId": {
                         |      "value": "F9SCS2T4NZBAX"
                         |    },
                         |     "expiryDate": "2018-03-05"
                         |  },
                         |  {
                         |    "invitationId": {
                         |      "value": "GZTW1KY6RTAAT"
                         |    },
                         |    "expiryDate": "2018-12-25"
                         |  }
                         |]
                         |""".stripMargin)
        )
    )

  def givenAllInvitationIdsByStatusReturnsNotFound(uid: String, status: String): StubMapping =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/clients/invitations/uid/$uid?status=$status"))
        .willReturn(
          aResponse()
            .withStatus(404)
        )
    )

  def givenAllInvitationIdsByStatusReturnsEmpty(uid: String): StubMapping =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/clients/invitations/uid/$uid"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |[]
                         |""".stripMargin)))

  def givenAgentReferenceNotFound(arn: Arn, clientType: ClientType): StubMapping =
    stubFor(
      post(urlEqualTo(
        s"/agent-client-authorisation/agencies/references/arn/${encodePathSegment(arn.value)}/clientType/$clientType"))
        .willReturn(aResponse()
          .withStatus(404)))

  def givenInvitationCreationSucceeds(
    arn: Arn,
    clientType: Option[ClientType],
    clientId: String,
    invitationId: InvitationId,
    suppliedClientId: String,
    suppliedClientType: String,
    service: Service,
    serviceIdentifier: String) = {
    stubFor(
      post(urlEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent"))
        .withRequestBody(
          equalToJson(s"""
                         |{
                         |   "clientType": "${clientType.getOrElse("")}",
                         |   "service": "${service.id}",
                         |   "clientIdType": "$suppliedClientType",
                         |   "clientId":"$suppliedClientId"
                         |}""".stripMargin)
        )
        .willReturn(
          aResponse()
            .withStatus(201)
            .withHeader(
              "location",
              s"$wireMockBaseUrlAsString/agent-client-authorisation/clients/$serviceIdentifier/${encodePathSegment(
                clientId)}/invitations/received/${invitationId.value}"
            )))

    givenInvitationExists(arn, clientId, invitationId, service, serviceIdentifier, "Pending")
  }

  def givenInvitationCreationFailsForService(
                                       arn: Arn,
                                       clientType: Option[ClientType],
                                       clientId: String,
                                       invitationId: InvitationId,
                                       suppliedClientId: String,
                                       suppliedClientType: String,
                                       service: Service,
                                       serviceIdentifier: String): StubMapping = {
    stubFor(
      post(urlEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent"))
        .withRequestBody(
          equalToJson(s"""
                         |{
                         |   "clientType": "${clientType.getOrElse("")}",
                         |   "service": "${service.id}",
                         |   "clientIdType": "$suppliedClientType",
                         |   "clientId":"$suppliedClientId"
                         |}""".stripMargin)
        )
        .willReturn(
          aResponse()
            .withStatus(404)))
  }

  def givenInvitationCreationFails(arn: Arn): StubMapping =
    stubFor(
      post(urlEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent"))
        .willReturn(aResponse()
          .withStatus(400)))

  def givenInvitationExists(
    arn: Arn,
    clientId: String,
    invitationId: InvitationId,
    service: Service,
    serviceIdentifier: String,
    status: String): StubMapping =
    stubFor(
      get(urlEqualTo(
        s"/agent-client-authorisation/clients/$serviceIdentifier/${encodePathSegment(clientId)}/invitations/received/${invitationId.value}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |{
                         |  "arn" : "${arn.value}",
                         |  "service" : "${service.id}",
                         |  "clientId" : "$clientId",
                         |  "clientIdType" : "${service.supportedSuppliedClientIdType}",
                         |  "suppliedClientId" : "$clientId",
                         |  "suppliedClientIdType" : "${service.supportedSuppliedClientIdType}",
                         |  "status" : "$status",
                         |  "created" : "2017-10-31T23:22:50.971",
                         |  "lastUpdated" : "2017-10-31T23:22:50.971",
                         |  "expiryDate" : "2017-12-18",
                         |  "invitationId": "$invitationId",
                         |  "isRelationshipEnded": false,
                         |  "_links": {
                         |    	"self" : {
                         |			  "href" : "$wireMockBaseUrlAsString/agent-client-authorisation/agencies/${arn.value}/invitations/sent/${invitationId.value}"
                         |		  }
                         |  }
                         |}""".stripMargin)))

  def givenInvitationByIdSuccess(invitationId: InvitationId, clientId: String, service: Service = Service.MtdIt, clientIdType: String = "mtditid", suppliedClientId: Option[String] = None, suppliedClientIdType: Option[String] = None): StubMapping =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/invitations/${invitationId.value}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(
              s"""
                 |{
                 |  "arn" : "TARN00001",
                 |  "service" : "${service.id}",
                 |  "clientId" : "$clientId",
                 |  "clientIdType" : "$clientIdType",
                 |  "suppliedClientId" : "${suppliedClientId.getOrElse(clientId)}",
                 |  "suppliedClientIdType" : "${suppliedClientIdType.getOrElse(suppliedClientIdType)}",
                 |  "status" : "Pending",
                 |  "created" : "2017-10-31T23:22:50.971",
                 |  "lastUpdated" : "2017-10-31T23:22:50.971",
                 |  "expiryDate" : "2017-12-18",
                 |  "invitationId": "$invitationId",
                 |  "isRelationshipEnded": false,
                 |  "_links": {
                 |    	"self" : {
                 |			  "href" : "$wireMockBaseUrlAsString/agent-client-authorisation/invitations/${invitationId.value}"
                 |		  }
                 |  }
                 |}""".stripMargin
            )
        )
    )

  def givenInvitationExpired(
    arn: Arn,
    clientId: String,
    invitationId: InvitationId,
    service: Service,
    serviceIdentifier: String): StubMapping =
    stubFor(
      get(urlEqualTo(
        s"/agent-client-authorisation/clients/$serviceIdentifier/${encodePathSegment(clientId)}/invitations/received/${invitationId.value}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |{
                         |  "arn" : "${arn.value}",
                         |  "service" : "${service.id}",
                         |  "clientId" : "$clientId",
                         |  "clientIdType" : "${service.supportedSuppliedClientIdType}",
                         |  "suppliedClientId" : "$clientId",
                         |  "suppliedClientIdType" : "${service.supportedSuppliedClientIdType}",
                         |  "status" : "Expired",
                         |  "created" : "2017-7-31T23:22:50.971",
                         |  "lastUpdated" : "2017-10-31T23:22:50.971",
                         |  "expiryDate" : "2017-12-18",
                         |  "invitationId": "$invitationId",
                         |  "isRelationshipEnded": false,
                         |  "_links": {
                         |    	"self" : {
                         |			  "href" : "/agent-client-authorisation/agencies/${arn.value}/invitations/sent/${invitationId.value}"
                         |		  }
                         |  }
                         |}""".stripMargin)))

  def givenInvitationCancelled(
    arn: Arn,
    clientId: String,
    invitationId: InvitationId,
    service: Service,
    serviceIdentifier: String): StubMapping =
    stubFor(
      get(urlEqualTo(
        s"/agent-client-authorisation/clients/$serviceIdentifier/${encodePathSegment(clientId)}/invitations/received/${invitationId.value}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |{
                         |  "arn" : "${arn.value}",
                         |  "service" : "${service.id}",
                         |  "clientId" : "$clientId",
                         |  "clientIdType" : "${service.supportedSuppliedClientIdType}",
                         |  "suppliedClientId" : "$clientId",
                         |  "suppliedClientIdType" : "${service.supportedSuppliedClientIdType}",
                         |  "status" : "Cancelled",
                         |  "created" : "2017-7-31T23:22:50.971",
                         |  "lastUpdated" : "2017-10-31T23:22:50.971",
                         |  "expiryDate" : "2017-12-18",
                         |  "invitationId": "$invitationId",
                         |  "isRelationshipEnded": false,
                         |  "_links": {
                         |    	"self" : {
                         |			  "href" : "/agent-client-authorisation/agencies/${arn.value}/invitations/sent/${invitationId.value}"
                         |		  }
                         |  }
                         |}""".stripMargin)))

  def givenInvitationAccepted(
    arn: Arn,
    clientId: String,
    invitationId: InvitationId,
    service: Service,
    serviceIdentifier: String) =
    stubFor(
      get(urlEqualTo(
        s"/agent-client-authorisation/clients/$serviceIdentifier/${encodePathSegment(clientId)}/invitations/received/${invitationId.value}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |{
                         |  "arn" : "${arn.value}",
                         |  "service" : "${service.id}",
                         |  "clientId" : "$clientId",
                         |  "clientIdType" : "${service.supportedSuppliedClientIdType}",
                         |  "suppliedClientId" : "$clientId",
                         |  "suppliedClientIdType" : "${service.supportedSuppliedClientIdType}",
                         |  "status" : "Accepted",
                         |  "created" : "2017-10-31T23:22:50.971",
                         |  "lastUpdated" : "2017-10-31T23:22:50.971",
                         |  "expiryDate" : "2017-12-18",
                         |  "invitationId": "$invitationId",
                         |  "isRelationshipEnded": false,
                         |  "_links": {
                         |    	"self" : {
                         |			  "href" : "$wireMockBaseUrlAsString/agent-client-authorisation/agencies/${arn.value}/invitations/sent/${invitationId.value}"
                         |		  }
                         |  }
                         |}""".stripMargin)))

  def givenInvitationNotFound(clientId: String, invitationId: InvitationId, serviceIdentifier: String) =
    stubFor(get(urlEqualTo(
      s"/agent-client-authorisation/clients/$serviceIdentifier/${encodePathSegment(clientId)}/invitations/received/${invitationId.value}"))
      .willReturn(aResponse()
        .withStatus(404)))

  def givenInvitationNoPermission(clientId: String, invitationId: InvitationId, serviceIdentifier: String) =
    stubFor(
      get(urlEqualTo(
        s"/agent-client-authorisation/clients/$serviceIdentifier/${encodePathSegment(clientId)}/invitations/received/${invitationId.value}"))
        .willReturn(
          aResponse()
            .withStatus(403)
            .withBody(
              s"""
                 |{
                 |   "code":"NO_PERMISSION_ON_CLIENT",
                 |   "message":"The logged in client is not permitted to access invitations for the specified client."
                 |}
           """.stripMargin
            )))

  def givenAcceptInvitationSucceeds(clientId: String, invitationId: InvitationId, serviceIdentifier: String) =
    acceptInvitationStub(clientId, invitationId, responseStatus = 204, serviceIdentifier)

  def givenAcceptInvitationReturnsNotFound(
    clientId: String,
    invitationId: InvitationId,
    serviceIdentifier: String) =
    acceptInvitationStub(clientId, invitationId, responseStatus = 404, serviceIdentifier)

  private def acceptInvitationStub(
    clientId: String,
    invitationId: InvitationId,
    responseStatus: Int,
    serviceIdentifier: String) = {
    val invitationIdEncoded = encodePathSegment(invitationId.value)
    stubFor(put(urlEqualTo(
      s"/agent-client-authorisation/clients/$serviceIdentifier/${encodePathSegment(clientId)}/invitations/received/$invitationIdEncoded/accept"))
      .willReturn(aResponse()
        .withStatus(responseStatus)))
  }

  def givenGetInvitationReturnsAlreadyActioned(
    clientId: String,
    invitationId: InvitationId,
    serviceIdentifier: String) =
    stubFor(
      get(urlEqualTo(
        s"/agent-client-authorisation/clients/$serviceIdentifier/${encodePathSegment(clientId)}/invitations/received/${invitationId.value}"))
        .willReturn(
          aResponse()
            .withStatus(403)
            .withBody(
              s"""
                 |{
                 |   "code":"INVALID_INVITATION_STATUS",
                 |   "message":"The invitation cannot be transitioned to Rejected because its current status is Rejected. Only Pending invitations may be transitioned to Rejected."
                 |}
           """.stripMargin
            )))

  def givenAcceptInvitationReturnsAlreadyActioned(
    clientId: String,
    invitationId: InvitationId,
    serviceIdentifier: String) = {
    val identifierEncoded = encodePathSegment(clientId)
    val invitationIdEncoded = encodePathSegment(invitationId.value)
    stubFor(
      put(urlEqualTo(
        s"/agent-client-authorisation/clients/$serviceIdentifier/$identifierEncoded/invitations/received/$invitationIdEncoded/accept"))
        .willReturn(
          aResponse()
            .withStatus(403)
            .withBody(
              s"""
                 |{
                 |   "code":"INVALID_INVITATION_STATUS",
                 |   "message":"The invitation cannot be transitioned to Rejected because its current status is Rejected. Only Pending invitations may be transitioned to Rejected."
                 |}
           """.stripMargin
            )))
  }

  def givenGetInvitationReturnsNoPermission(
    clientId: String,
    invitationId: InvitationId,
    serviceIdentifier: String) =
    stubFor(
      get(urlEqualTo(
        s"/agent-client-authorisation/clients/$serviceIdentifier/${encodePathSegment(clientId)}/invitations/received/${invitationId.value}"))
        .willReturn(
          aResponse()
            .withStatus(403)
            .withBody(
              s"""
                 |{
                 |   "code":"NO_PERMISSION_ON_CLIENT",
                 |   "message":"The logged in client is not permitted to access invitations for the specified client."
                 |}
           """.stripMargin
            )))

  def givenAcceptInvitationReturnsNoPermission(
    clientId: String,
    invitationId: InvitationId,
    serviceIdentifier: String) = {
    val identifierEncoded = encodePathSegment(clientId)
    val invitationIdEncoded = encodePathSegment(invitationId.value)
    stubFor(
      put(urlEqualTo(
        s"/agent-client-authorisation/clients/$serviceIdentifier/$identifierEncoded/invitations/received/$invitationIdEncoded/accept"))
        .willReturn(
          aResponse()
            .withStatus(403)
            .withBody(
              s"""
                 |{
                 |   "code":"NO_PERMISSION_ON_CLIENT",
                 |   "message":"The logged in client is not permitted to access invitations for the specified client."
                 |}
           """.stripMargin
            )))
  }

  def verifyAcceptInvitationAttempt(clientId: String, invitationId: InvitationId, serviceIdentifier: String) = {
    val identifierEncoded = encodePathSegment(clientId)
    val invitationIdEncoded = encodePathSegment(invitationId.value)
    verify(
      1,
      putRequestedFor(urlEqualTo(
        s"/agent-client-authorisation/clients/$serviceIdentifier/$identifierEncoded/invitations/received/$invitationIdEncoded/accept"))
    )
  }

  def givenCancelInvitationReturns(arn: Arn, invitationId: InvitationId, status: Int) =
    stubFor(
      put(
        urlEqualTo(s"/agent-client-authorisation/agencies/${arn.value}/invitations/sent/${invitationId.value}/cancel"))
        .willReturn(
          aResponse()
            .withStatus(status)
        )
    )

  def givenSetRelationshipEndedReturns(arn: Arn, clientId: String, service: Service, status: Int) =
    stubFor(
      put(
        urlEqualTo(s"/agent-client-authorisation/invitations/set-relationship-ended"))
        .withRequestBody(equalToJson(
            s"""{
               |"arn": "${arn.value}",
               |"clientId": "$clientId",
               |"service": "${service.id}",
               |"endedBy": "Agent"
               |}""".stripMargin))
        .willReturn(
          aResponse()
            .withStatus(status)
        )
    )

  def givenRejectInvitationSucceeds(clientId: String, invitationId: InvitationId, serviceIdentifier: String) =
    rejectInvitationStub(clientId, invitationId, responseStatus = 204, serviceIdentifier)

  def givenRejectInvitationReturnsWithStatus(
    clientId: String,
    invitationId: InvitationId,
    serviceIdentifier: String,
    status: Int = 404) =
    rejectInvitationStub(clientId, invitationId, responseStatus = status, serviceIdentifier)

  private def rejectInvitationStub(
    clientId: String,
    invitationId: InvitationId,
    responseStatus: Int,
    serviceIdentifier: String) = {
    val identifierEncoded = encodePathSegment(clientId)
    val invitationIdEncoded = encodePathSegment(invitationId.value)
    stubFor(
      put(urlEqualTo(
        s"/agent-client-authorisation/clients/$serviceIdentifier/$identifierEncoded/invitations/received/$invitationIdEncoded/reject"))
        .willReturn(aResponse()
          .withStatus(responseStatus)))
  }

  def givenRejectInvitationReturnsAlreadyActioned(
    clientId: String,
    invitationId: InvitationId,
    serviceIdentifier: String) = {
    val identifierEncoded = encodePathSegment(clientId)
    val invitationIdEncoded = encodePathSegment(invitationId.value)
    stubFor(
      put(urlEqualTo(
        s"/agent-client-authorisation/clients/$serviceIdentifier/$identifierEncoded/invitations/received/$invitationIdEncoded/reject"))
        .willReturn(
          aResponse()
            .withStatus(403)
            .withBody(
              s"""
                 |{
                 |   "code":"INVALID_INVITATION_STATUS",
                 |   "message":"The invitation cannot be transitioned to Rejected because its current status is Rejected. Only Pending invitations may be transitioned to Rejected."
                 |}
           """.stripMargin
            )))
  }

  def verifyRejectInvitationAttempt(clientId: String, invitationId: InvitationId, serviceIdentifier: String) = {
    val identifierEncoded = encodePathSegment(clientId)
    val invitationIdEncoded = encodePathSegment(invitationId.value)
    verify(
      1,
      putRequestedFor(urlEqualTo(
        s"/agent-client-authorisation/clients/$serviceIdentifier/$identifierEncoded/invitations/received/$invitationIdEncoded/reject"))
    )
  }

  def givenMatchingClientIdAndPostcode(nino: Nino, postcode: String) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/known-facts/individuals/nino/${nino.value}/sa/postcode/$postcode"))
        .willReturn(aResponse()
          .withStatus(204)))

  def givenNonMatchingClientIdAndPostcode(nino: Nino, postcode: String) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/known-facts/individuals/nino/${nino.value}/sa/postcode/$postcode"))
        .willReturn(
          aResponse()
            .withStatus(403)
            .withBody(s"""
                         |{
                         |   "code":"POSTCODE_DOES_NOT_MATCH",
                         |   "message":"The submitted postcode did not match the client's postcode as held by HMRC."
                         |}
           """.stripMargin)))

  def givenNotEnrolledClientITSA(nino: Nino, postcode: String) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/known-facts/individuals/nino/${nino.value}/sa/postcode/$postcode"))
        .willReturn(
          aResponse()
            .withStatus(403)
            .withBody(s"""
                         |{
                         |   "code":"CLIENT_REGISTRATION_NOT_FOUND",
                         |   "message":"The Client's MTDfB registration was not found."
                         |}
           """.stripMargin)))

  def givenServiceUnavailableITSA(nino: Nino, postcode: String) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/known-facts/individuals/nino/${nino.value}/sa/postcode/$postcode"))
        .willReturn(aResponse()
          .withStatus(502)))

  def givenVatRegisteredClientReturns(vrn: Vrn, date: LocalDate, responseStatus: Int, clientInsolvent: Boolean = false) = {
    val bodyMsg = if(responseStatus == 403){ if(clientInsolvent) """VAT_RECORD_CLIENT_INSOLVENT_TRUE"""  else s"""VAT_REGISTRATION_DATE_DOES_NOT_MATCH"""} else """"""
    stubFor(
      get(urlEqualTo(
        s"/agent-client-authorisation/known-facts/organisations/vat/${vrn.value}/registration-date/${date.toString}"))
        .willReturn(aResponse()
          .withStatus(responseStatus).withBody(bodyMsg)))
  }


  def givenTrustClientReturns(taxIdentifier: TrustTaxIdentifier, responseStatus: Int, body: String) =
    stubFor(
      get(urlEqualTo(
        s"/agent-client-authorisation/known-facts/organisations/trust/${taxIdentifier.value}"))
        .willReturn(aResponse()
          .withStatus(responseStatus)
        .withBody(body)))

  def givenGetCgtSubscriptionReturns(cgtRef: CgtRef, responseStatus: Int, body: String) =
    stubFor(
      get(urlEqualTo(
        s"/agent-client-authorisation/cgt/subscriptions/${cgtRef.value}"))
        .willReturn(aResponse()
          .withStatus(responseStatus)
          .withBody(body)))

  def givenGetPptSubscriptionReturns(pptRef: PptRef, responseStatus: Int, body: String) =
    stubFor(
      get(urlEqualTo(
        s"/agent-client-authorisation/ppt/subscriptions/${pptRef.value}"))
        .willReturn(aResponse()
          .withStatus(responseStatus)
          .withBody(body)))

  def givenMatchingCitizenRecord(nino: Nino, dob: LocalDate) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/known-facts/individuals/${nino.value}/dob/${dob.toString}"))
        .willReturn(aResponse()
          .withStatus(204)))

  def givenNonMatchingCitizenRecord(nino: Nino, dob: LocalDate) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/known-facts/individuals/${nino.value}/dob/${dob.toString}"))
        .willReturn(
          aResponse()
            .withStatus(403)
            .withBody(s"""
                         |{
                         |  "code":"DATE_OF_BIRTH_DOES_NOT_MATCH",
                         |  "message":"The submitted date of birth did not match the client's date of birth as held by HMRC."
                         |}
               """.stripMargin)))

  def givenNotFoundCitizenRecord(nino: Nino, dob: LocalDate) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/known-facts/individuals/${nino.value}/dob/${dob.toString}"))
        .willReturn(aResponse()
          .withStatus(404)))

  def verifyCheckVatRegisteredClientStubAttempt(vrn: Vrn, date: LocalDate) = {
    val vrnEncoded = encodePathSegment(vrn.value)
    val dateEncoded = encodePathSegment(date.toString)
    verify(
      1,
      getRequestedFor(
        urlEqualTo(
          s"/agent-client-authorisation/known-facts/organisations/vat/$vrnEncoded/registration-date/$dateEncoded")))
  }

  def verifyCheckItsaRegisteredClientStubAttempt(nino: Nino, postcode: String) = {
    val ninoEncoded = encodePathSegment(nino.value)
    val postEncoded = encodePathSegment(postcode)
    verify(
      1,
      getRequestedFor(
        urlEqualTo(s"/agent-client-authorisation/known-facts/individuals/nino/$ninoEncoded/sa/postcode/$postEncoded")))
  }

  def verifyNoCheckVatRegisteredClientStubAttempt() =
    verify(
      0,
      getRequestedFor(urlPathMatching("/agent-client-authorisation/known-facts/organisations/.*/registration-date/.*")))

  def givenGetInvitations(arn: Arn) =
    stubFor(
      get(urlPathEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent"))
        .withQueryParam("createdOnOrAfter", equalTo(LocalDate.now.minusDays(30).toString))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(halEnvelope(Seq(
              invitation(arn, "Pending", Service.MtdIt, "ni", "AB123456A", "foo1", "2017-12-18", false, None),
              invitation(arn, "Pending", Service.Vat, "vrn", "101747696", "foo2", "2017-12-18", false, None),
              invitation(arn, "Pending", Service.PersonalIncomeRecord, "ni", "AB123456B", "foo3", "2017-12-18", false, None),
              invitation(arn, "Accepted", Service.MtdIt, "ni", "AB123456A", "foo4", "2017-12-18", false, None),
              invitation(arn, "Accepted", Service.Vat, "vrn", "101747696", "foo5", "2017-12-18", false, None),
              invitation(arn, "Accepted", Service.PersonalIncomeRecord, "ni", "AB123456B", "foo6", "2017-12-18", false, None),
              invitation(arn, "Rejected", Service.MtdIt, "ni", "AB123456A", "foo7", "2017-12-18", false, None),
              invitation(arn, "Rejected", Service.Vat, "vrn", "101747696", "foo2", "2017-12-18", false, None),
              invitation(arn, "Rejected", Service.PersonalIncomeRecord, "ni", "AB123456B", "foo8", "2017-12-18", false, None),
              invitation(arn, "Cancelled", Service.MtdIt, "ni", "AB123456A", "foo9", "2017-12-18", false, None),
              invitation(arn, "Cancelled", Service.Vat, "vrn", "101747696", "fo10", "2017-12-18", false, None),
              invitation(arn, "Cancelled", Service.PersonalIncomeRecord, "ni", "AB123456B", "fo11", "2017-12-18", false, None),
              invitation(arn, "Expired", Service.MtdIt, "ni", "AB123456A", "fo12", "2017-12-18", false, None),
              invitation(arn, "Expired", Service.Vat, "vrn", "101747696", "fo13", "2017-12-18", false, None),
              invitation(arn, "Expired", Service.PersonalIncomeRecord, "ni", "AB123456B", "fo14", "2017-12-18", false, None),
              invitation(arn, "Pending", Service.MtdIt, "ni", "AB123456A", "foo1", "2099-01-01", false, None),
              invitation(arn, "Pending", Service.Vat, "vrn", "101747696", "foo2", "2099-01-01", false, None),
              invitation(arn, "Pending", Service.PersonalIncomeRecord, "ni", "AB123456B", "foo3", "2099-01-01", false, None)
            ).mkString("[", ",", "]")))))

  def givenGetInvitationsTrack() = {
    def nowMinus(d: Int) = LocalDate.now().minusDays(d).atStartOfDay()
    val expiryDate = nowMinus(10).toLocalDate.toString
    stubFor(
      get(urlPathEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment("TARN0000001")}/invitations/sent"))
        .withQueryParam("createdOnOrAfter", equalTo(LocalDate.now.minusDays(30).toString))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(halEnvelope(Seq(
              invitationTemporal(nowMinus(10), "Pending", Service.MtdIt, "ni", "Aaa Itsa Trader","AB127456A", "foo1", expiryDate, false, None),
              invitationTemporal(nowMinus(10), "Pending", Service.Vat, "vrn", "Superior Ltd","101747696", "foo2", expiryDate, false, None),
              invitationTemporal(nowMinus(10), "Pending", Service.MtdIt, "ni", "Bbb Itsa Trader","AB129456B", "foo3", expiryDate, false, None),
              invitationTemporal(nowMinus(25), "Accepted", Service.MtdIt, "ni", "Ccc Itsa Trader","AB123256B", "foo4", expiryDate, false, None),
              invitationTemporal(nowMinus(5), "Accepted", Service.MtdIt, "ni", "Ddd Itsa Trader","AB123456A", "foo5",expiryDate, true, Some("Agent")),
              invitationTemporal(nowMinus(12), "Accepted", Service.Vat, "vrn","Excel Ltd", "101747641", "foo6", expiryDate, true, Some("Client")),
              invitationTemporal(nowMinus(25), "Accepted", Service.Trust, "utr", "D Trust","4937455253", "foo7", expiryDate, true, Some("Agent")),
              invitationTemporal(nowMinus(22), "Accepted", Service.CapitalGains, "cgtRef", "Property Dev","XMCGTP123456789", "foo8", expiryDate, true, Some("Client")),
              invitationTemporal(nowMinus(30), "Cancelled", Service.MtdIt, "ni", "Ddd Itsa Trader","AB123456A", "foo9", expiryDate, false, None),
              invitationTemporal(nowMinus(0), "Accepted", Service.MtdIt, "ni", "Ddd Itsa Trader","AB123456A", "foo10",expiryDate, false, None),
              invitationTemporal(nowMinus(0), "Accepted", Service.Vat, "vrn","Excel Ltd", "101747641", "foo11", expiryDate, false, None),
              invitationTemporal(nowMinus(2), "Accepted", Service.Trust, "utr", "D Trust","4937455253", "foo12", expiryDate, false, None),
              invitationTemporal(nowMinus(25), "Accepted", Service.PersonalIncomeRecord, "ni", "John Jones","AB123456A", "foo13", expiryDate, true, Some("HMRC")),
              invitationTemporal(nowMinus(25), "Accepted", Service.PersonalIncomeRecord, "ni", "Sally Ship","GZ753451B", "foo14", expiryDate, true, Some("HMRC")),
            ).mkString("[", ",", "]")))))
  }

  def givenGetCbcInvitations() = {
    def nowMinus(d: Int) = LocalDate.now().minusDays(d).atStartOfDay()
    val expiryDate = nowMinus(10).toLocalDate.toString
    stubFor(
      get(urlPathEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment("TARN0000001")}/invitations/sent"))
        .withQueryParam("createdOnOrAfter", equalTo(LocalDate.now.minusDays(30).toString))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(halEnvelope(Seq(
              invitationTemporal(nowMinus(10), "Accepted", Service.Cbc, "cbcId", "Cbc UK Company","XCBC1111111111", "foo1", expiryDate, false, None),
              invitationTemporal(nowMinus(10), "Accepted", Service.CbcNonUk, "cbcId", "Cbc Non-UK Company","XCBC2222222222", "foo2", expiryDate, false, None)
            ).mkString("[", ",", "]")))))
  }



  def givenASingleInvitationWithRelationshipEnded(
                                                   clientId: String,
                                                   service: Service,
                                                   clientIdType: String,
                                                   lastUpdated: LocalDateTime) = {
    stubFor(
      get(urlPathEqualTo(s"/agent-client-authorisation/agencies/TARN0000001/invitations/sent"))
        .withQueryParam("createdOnOrAfter", equalTo(LocalDate.now.minusDays(30).toString))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(halEnvelope(Seq(
              invitationTemporal(lastUpdated, "Accepted", service, clientIdType, "Dave", clientId, "foo1", "2021-03-13", true, Some("Agent"))
            ).mkString("[", ",", "]")))))
  }

  def givenASingleAcceptedInvitation(
                                    arn: Arn,
                                    clientId: String,
                                    service: Service,
                                    clientIdType: String,
                                    lastUpdated: LocalDateTime,
                                    isPartialAuth: Boolean = false
                                    ) = {
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent?clientId=$clientId&service=$service"))
        .willReturn(
          aResponse()
        .withStatus(200)
        .withBody(halEnvelope(
          Seq(
            invitationTemporal(lastUpdated, if(isPartialAuth)"Partialauth" else "Accepted", service, clientIdType, "Julius", clientId, "foo1", "2021-03-13", false, None)
          ).mkString("[", ",", "]")))))

  }

  def givenNoAcceptedInvitationFound(
                                      arn: Arn,
                                      clientId: String,
                                      service: Service
                                    ) = {
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent?status=Accepted&clientId=$clientId&service=$service"))
        .willReturn(
          aResponse()
            .withStatus(404)
            ))

  }

  def givenASingleInvitationWithRelationshipStillActive(
                                                   clientId: String,
                                                   service: Service,
                                                   clientIdType: String,
                                                   lastUpdated: LocalDateTime) = {
    stubFor(
      get(urlPathEqualTo(s"/agent-client-authorisation/agencies/TARN0000001/invitations/sent"))
        .withQueryParam("createdOnOrAfter", equalTo(LocalDate.now.minusDays(30).toString))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(halEnvelope(Seq(
              invitationTemporal(lastUpdated, "Accepted", service, clientIdType, "Dave", clientId, "foo1", lastUpdated.toLocalDate.plusDays(5).toString, false, None)
            ).mkString("[", ",", "]")))))
  }

  def givenTwoInvitationsExistForSameClientOneWithDeauthedStatus(
    clientId: String,
    service: Service,
    clientIdType: String,
    accepted: LocalDateTime,
    deauthed: LocalDateTime) = {
    stubFor(
      get(urlPathEqualTo(s"/agent-client-authorisation/agencies/TARN0000001/invitations/sent"))
        .withQueryParam("createdOnOrAfter", equalTo(LocalDate.now.minusDays(30).toString))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(halEnvelope(Seq(
              invitationTemporal(accepted, "Accepted", service, clientIdType, "Dave", clientId, "foo1", "2021-03-13", true, Some("Agent")),
              invitationTemporal(deauthed, "Deauthorised", service, clientIdType, "Dave", clientId, "foo2", "2021-03-17", true, None)
            ).mkString("[", ",", "]")))))
  }

  def givenTwoInvitationsExistForSameClientWithOneDeAuthorised(
                                                   clientId: String,
                                                   service: Service,
                                                   clientIdType: String,
                                                   accepted1: LocalDateTime,
                                                   accepted2: LocalDateTime) = {
    stubFor(
      get(urlPathEqualTo(s"/agent-client-authorisation/agencies/TARN0000001/invitations/sent"))
        .withQueryParam("createdOnOrAfter", equalTo(LocalDate.now.minusDays(30).toString))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(halEnvelope(Seq(
              invitationTemporal(accepted1, "Accepted", service, clientIdType, "Dave", clientId, "foo1", "2021-03-13", true, Some("Agent")),
              invitationTemporal(accepted2, "Accepted", service, clientIdType, "Dave", clientId, "foo2", "2021-03-17", false, None)
            ).mkString("[", ",", "]")))))
  }

  def givenGetInvitations() = {
    def nowMinus(d: Int) = Instant.now.atZone(ZoneOffset.UTC).toLocalDate.minusDays(d).atStartOfDay()
    stubFor(
      get(urlPathEqualTo(s"/agent-client-authorisation/agencies/TARN0000001/invitations/sent"))
        .withQueryParam("createdOnOrAfter", equalTo(LocalDate.now.minusDays(30).toString))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(halEnvelope(Seq(
              invitationTemporal(nowMinus(29), "Pending", Service.MtdIt, "ni", "Dave","AB127456A", "foo1", "2017-12-18", false, None),
              invitationTemporal(nowMinus(22), "Pending", Service.Vat, "vrn", "Doug","101747696", "foo2", "2017-12-18", false, None),
              invitationTemporal(nowMinus(3), "Pending", Service.MtdIt, "ni", "Darian","AB129456B", "foo3", "2017-12-18", false, None),
              invitationTemporal(nowMinus(25), "Accepted", Service.MtdIt, "ni", "Darian","AB123256B", "foo4", "2017-12-18", false, None),
              invitationTemporal(nowMinus(0), "Accepted", Service.MtdIt, "ni", "Don","AB123456A", "foo5","2017-12-18", true, Some("Agent")),
              invitationTemporal(nowMinus(12), "Accepted", Service.Vat, "vrn","Diane", "101747641", "foo6", "2017-12-18", true, Some("Client")),
              invitationTemporal(nowMinus(25), "Accepted", Service.Trust, "utr", "Doreen","4937455253", "foo7", "2017-12-18", true, Some("Agent")),
              invitationTemporal(nowMinus(22), "Accepted", Service.CapitalGains, "cgtRef", "Duck","XMCGTP123456789", "foo2", "2017-12-18", true, Some("Client")),
              invitationTemporal(nowMinus(30), "Cancelled", Service.MtdIt, "ni", "Dean","AB123456A", "foo9", "2017-12-18", false, None),
              invitationTemporal(nowMinus(30), "Partialauth", Service.MtdIt, "ni", "Debby","AB123456C", "foo10", "2017-12-18", false, Some("Client")),
            ).mkString("[", ",", "]")))))
  }

  def givenGetAllPendingInvitationsReturnsSome(arn: Arn, clientId: String, service: Service) = {
    val body = halEnvelope(Seq(service match {
      case  Service.MtdIt => invitation(arn, "Pending", Service.MtdIt, "ni", clientId, "foo1", "2017-12-18", false, None)
      case  Service.Vat => invitation(arn, "Pending", Service.Vat, "vrn", clientId, "foo2", "2017-12-18", false, None)
      case  Service.PersonalIncomeRecord => invitation(arn, "Pending", Service.PersonalIncomeRecord, "ni", clientId, "foo3", "2017-12-18", false, None)
    }).mkString("[", ",", "]"))

    stubFor(get(urlPathEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent"))
    .withQueryParam("status", equalTo("Pending"))
    .withQueryParam("clientId", equalTo(clientId))
    .withQueryParam("service", equalTo(service.id))
    .willReturn(aResponse()
      .withStatus(200)
      .withBody(body)))
  }

  def givenGetAllPendingInvitationsReturnsEmpty(arn: Arn, clientId: String, service: Service) = {
    stubFor(get(urlPathEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent"))
      .withQueryParam("status", equalTo("Pending"))
      .withQueryParam("clientId", equalTo(clientId))
      .withQueryParam("service", equalTo(service.id))
      .willReturn(aResponse()
        .withStatus(200)
        .withBody(halEnvelope("[]"))))
  }

  def givenGetInvitationsReturnsEmpty(arn: Arn) =
    stubFor(
      get(urlPathEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(halEnvelope("[]"))
        ))

  val invitation = (
    arn: Arn,
    status: String,
    service: Service,
    clientIdType: String,
    clientId: String,
    invitationId: String,
    expiryDate: String,
                   isRelationshipEnded: Boolean,
                   relationshipEndedBy: Option[String]) => s"""
                              |{
                              |  "arn" : "${arn.value}",
                              |  "clientType" : "personal",
                              |  "service" : "${service.id}",
                              |  "clientId" : "$clientId",
                              |  "clientIdType" : "$clientIdType",
                              |  "suppliedClientId" : "$clientId",
                              |   "detailsForEmail" : {
                              |   "agencyEmail": "agent@email.com",
                              |   "agencyName" : "someAgent",
                              |   "clientName" : "The Client name"
                              |   },
                              |  "suppliedClientIdType" : "$clientIdType",
                              |  "status" : "$status",
                              |  "created" : "2017-10-31T23:22:50.971",
                              |  "lastUpdated" : "2018-09-11T00:00:00.000",
                              |  "expiryDate" : "$expiryDate",
                              |  "invitationId": "$invitationId",
                              |  "isRelationshipEnded": $isRelationshipEnded,
                              |  ${relationshipEndedBy.map(v => s""" "relationshipEndedBy" : "$v", """).getOrElse("")}
                              |  "_links": {
                              |    	"self" : {
                              |			  "href" : "$wireMockBaseUrlAsString/agent-client-authorisation/agencies/${arn.value}/invitations/sent/$invitationId"
                              |		  }
                              |  }
                              |}""".stripMargin

  val invitationTemporal = (
                     lastUpdated: LocalDateTime,
                     status: String,
                     service: Service,
                     clientIdType: String,
                     clientName: String,
                     clientId: String,
                     invitationId: String,
                     expiryDate: String,
                     isRelationshipEnded: Boolean,
                     relationshipEndedBy: Option[String]) => s"""
                                                                |{
                                                                |  "arn" : "TARN0000001",
                                                                |  "clientType" : "personal",
                                                                |  "service" : "${service.id}",
                                                                |  "clientId" : "$clientId",
                                                                |  "clientIdType" : "$clientIdType",
                                                                |  "suppliedClientId" : "$clientId",
                                                                |  "detailsForEmail" : {
                                                                |   "agencyEmail": "agent@email.com",
                                                                |   "agencyName" : "someAgent",
                                                                |   "clientName" : "$clientName"
                                                                |   },
                                                                |  "suppliedClientIdType" : "$clientIdType",
                                                                |  "status" : "$status",
                                                                |  "created" : "2017-10-31T23:22:50.971",
                                                                |  "lastUpdated" : "${lastUpdated.toString}",
                                                                |  "expiryDate" : "$expiryDate",
                                                                |  "invitationId": "$invitationId",
                                                                |  "isRelationshipEnded": $isRelationshipEnded,
                                                                |  ${relationshipEndedBy.map(v => s""" "relationshipEndedBy" : "$v", """).getOrElse("")}
                                                                |  "_links": {
                                                                |    	"self" : {
                                                                |			  "href" : "$wireMockBaseUrlAsString/agent-client-authorisation/agencies/TARN0000001/invitations/sent/$invitationId"
                                                                |		  }
                                                                |  }
                                                                |}""".stripMargin

  def halEnvelope(embedded: String): String =
    s"""{"_links": {
        "invitations": [
          {
            "href": "/agent-client-authorisation/agencies/TARN0000001/invitations/sent/AK77NLH3ETXM9"
          }
        ],
        "self": {
          "href": "/agent-client-authorisation/agencies/TARN0000001/invitations/sent"
        }
      },
      "_embedded": {
        "invitations": $embedded
      }
    }""".stripMargin

  def givenGetAgencyNameClientStub(arn: Arn) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/client/agency-name/${encodePathSegment(arn.value)}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |{
                         |  "agencyName" : "My Agency"
                         |}""".stripMargin)))

  def givenGetSuspensionDetailsClientStub(arn: Arn, suspensionDetails: SuspensionDetails) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/client/suspension-details/${encodePathSegment(arn.value)}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(Json.toJson(suspensionDetails).toString)))

  def givenGetAgencyNameNotFoundClientStub(arn: Arn) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/client/agency-name/${encodePathSegment(arn.value)}"))
        .willReturn(aResponse()
          .withStatus(404)))

  def givenGetAgencyNameAgentStub =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/agent/agency-name"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |{
                         |  "agencyName" : "My Agency"
                         |}""".stripMargin)))

  def givenGetSuspensionDetailsAgentStub(suspensionDetails: SuspensionDetails) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/agent/suspension-details"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(Json.toJson(suspensionDetails).toString())))

  def givenGetAgencyEmailAgentStub =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/agent/agency-email"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |{
                         |  "agencyEmail" : "abc@xyz.com"
                         |}""".stripMargin)))

  def givenNoContentAgencyEmailAgentStub =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/agent/agency-email"))
        .willReturn(aResponse()
          .withStatus(204)))

  def givenNotFoundAgencyEmailAgentStub =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/agent/agency-email"))
        .willReturn(aResponse()
          .withStatus(404)))

  def givenAgencyNameNotFoundClientStub(arn: Arn) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/client/agency-name/${encodePathSegment(arn.value)}"))
        .willReturn(aResponse()
          .withStatus(404)))

  def givenAgencyNameNotFoundAgentStub =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/agent/agency-name"))
        .willReturn(aResponse()
          .withStatus(404)))

  def givenTradingName(nino: Nino, tradingName: String) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/client/trading-name/nino/$nino"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""{"tradingName": "$tradingName"}""")
        ))

  def givenTradingNameMissing(nino: Nino) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/client/trading-name/nino/$nino"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""{}""")
        ))

  def givenTradingNameNotFound(nino: Nino) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/client/trading-name/nino/${nino.value}"))
        .willReturn(
          aResponse()
            .withStatus(404)
        ))

  def givenClientDetails(vrn: Vrn) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/client/vat-customer-details/vrn/${vrn.value}"))
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
      get(urlEqualTo(s"/agent-client-authorisation/client/vat-customer-details/vrn/${vrn.value}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""{}""".stripMargin)
        ))

  def givenClientDetailsOnlyTrading(vrn: Vrn) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/client/vat-customer-details/vrn/${vrn.value}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""{"tradingName": "GDT"}""".stripMargin)
        ))

  def givenClientDetailsOnlyOrganisation(vrn: Vrn) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/client/vat-customer-details/vrn/${vrn.value}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""{"organisationName": "Gadgetron"}""".stripMargin)
        ))

  def givenClientDetailsOnlyPersonal(vrn: Vrn) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/client/vat-customer-details/vrn/${vrn.value}"))
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
      get(urlEqualTo(s"/agent-client-authorisation/client/mtdItId/${mtdItId.value}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         | {
                         |    "nino":"${nino.value}"
                         | }
               """.stripMargin)
        )
    )

  def givenPartialAuthorisationExists(arn: Arn, clientId: String) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent?status=PartialAuth&clientId=$clientId&service=HMRC-MTD-IT"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(halEnvelope(Seq(invitation(arn, "Partialauth", Service.MtdIt, "ni", clientId, "foo1", "2017-12-18", false, None)).mkString("[", ",", "]")))))

  def givenPartialAuthNotExists(arn: Arn, clientId: String) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent?status=PartialAuth&clientId=$clientId&service=HMRC-MTD-IT"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(halEnvelope(Seq.empty.mkString("[", ",", "]")))))


  def givenPutAltItsaAuth(arn: Arn) =
    stubFor(
      put(urlEqualTo(s"/agent-client-authorisation/agent/alt-itsa/update/${arn.value}"))
        .willReturn(
          aResponse()
            .withStatus(204)
        )
    )

  def givenPptCheckKnownFactReturns(pptClient: PptClient, status: Int) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/known-facts/ppt/${pptClient.pptRef.value}/${pptClient.registrationDate}"))
        .willReturn(
          aResponse()
            .withStatus(status)
        )
    )

  def givenCbcCheckKnownFactReturns(cbcClient: CbcClient, status: Int) =
    stubFor(
      post(urlEqualTo(s"/agent-client-authorisation/known-facts/cbc/${cbcClient.cbcId.value}"))
        .withRequestBody(
          equalToJson(s"""{"email":"${cbcClient.email}"}"""))
        .willReturn(
          aResponse()
            .withStatus(status)
        )
    )

  def givenPillar2CheckKnownFactReturns(pillar2Client: Pillar2Client, status: Int) =
    stubFor(
      post(urlEqualTo(s"/agent-client-authorisation/known-facts/pillar2/${pillar2Client.plrId.value}"))
        .withRequestBody(
          equalToJson(s"""{"registrationDate":"${pillar2Client.registrationDate}"}"""))
        .willReturn(
          aResponse()
            .withStatus(status)
        )
    )

  def givenGetPptCustomerName(pptRef: PptRef, name: String) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/client/ppt-customer-name/pptref/${pptRef.value}"))
      .willReturn(
        aResponse()
          .withBody(
            s"""{"customerName":"$name"}""".stripMargin)
          .withStatus(200))
    )

  def givenGetCbcSubscription(cbcId: CbcId, customerName: String, isGBUser: Boolean) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/cbc/subscriptions/${cbcId.value}"))
        .willReturn(
          aResponse()
            .withBody(
              s"""{
                 |"customerName": "$customerName",
                 |"otherNames": ["John"],
                 |"isGBUser": $isGBUser
                 |}""".stripMargin)
            .withStatus(200)
        )
    )

  def givenGetPillar2Subscription(plrId: PlrId, pillar2Subscription: Pillar2Subscription) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/pillar2/subscriptions/${plrId.value}"))
        .willReturn(
          aResponse()
            .withBody(Json.toJson(pillar2Subscription).toString)
            .withStatus(200)
        )
    )
}
