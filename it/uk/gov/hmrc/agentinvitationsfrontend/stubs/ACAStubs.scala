package uk.gov.hmrc.agentinvitationsfrontend.stubs

import com.github.tomakehurst.wiremock.client.WireMock.{put, _}
import com.github.tomakehurst.wiremock.stubbing.StubMapping
import org.joda.time.LocalDate
import play.api.libs.json.Json
import uk.gov.hmrc.agentinvitationsfrontend.UriPathEncoding._
import uk.gov.hmrc.agentinvitationsfrontend.models.{ClientType, StoredInvitation, SuspensionDetails}
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

  def givenAllInvitationIdsByStatus(uid: String, status: String): StubMapping =
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
                         |    ]
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
                         |    ]
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
                         |    ]
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
                         |    ]
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
                         |    ]
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
                         |    ]
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
                         |    ]
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
                         |    ]
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
                         |    ]
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
                         |    ]
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
                         |    "expiryDate": "9999-11-01"
                         |  },
                         |  {
                         |    "invitationId": {
                         |      "value": "AG1UGUKTPNJ7Z"
                         |    },
                         |    "expiryDate": "9999-11-01"
                         |  },
                         |  {
                         |    "invitationId": {
                         |      "value": "CZTW1KY6RTAAT"
                         |    },
                         |    "expiryDate": "9999-12-25"
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
    service: String,
    serviceIdentifier: String) = {
    stubFor(
      post(urlEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent"))
        .withRequestBody(
          equalToJson(s"""
                         |{
                         |   "clientType": "${clientType.getOrElse("")}",
                         |   "service": "$service",
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
                                       service: String,
                                       serviceIdentifier: String): StubMapping = {
    stubFor(
      post(urlEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent"))
        .withRequestBody(
          equalToJson(s"""
                         |{
                         |   "clientType": "${clientType.getOrElse("")}",
                         |   "service": "$service",
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
    service: String,
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
                         |  "service" : "$service",
                         |  "clientId" : "$clientId",
                         |  "clientIdType" : "${StoredInvitation.clientIdTypeByService(service)}",
                         |  "suppliedClientId" : "$clientId",
                         |  "suppliedClientIdType" : "${StoredInvitation.clientIdTypeByService(service)}",
                         |  "status" : "$status",
                         |  "created" : "2017-10-31T23:22:50.971Z",
                         |  "lastUpdated" : "2017-10-31T23:22:50.971Z",
                         |  "expiryDate" : "2017-12-18",
                         |  "invitationId": "$invitationId",
                         |  "isRelationshipEnded": false,
                         |  "_links": {
                         |    	"self" : {
                         |			  "href" : "$wireMockBaseUrlAsString/agent-client-authorisation/agencies/${arn.value}/invitations/sent/${invitationId.value}"
                         |		  }
                         |  }
                         |}""".stripMargin)))

  def givenInvitationByIdSuccess(invitationId: InvitationId, clientId: String, service: String = "HMRC-MTD-IT", clientIdType: String = "mtditid"): StubMapping =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/invitations/${invitationId.value}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(
              s"""
                 |{
                 |  "arn" : "TARN00001",
                 |  "service" : "$service",
                 |  "clientId" : "$clientId",
                 |  "clientIdType" : "$clientIdType",
                 |  "suppliedClientId" : "$clientId",
                 |  "suppliedClientIdType" : "$clientIdType",
                 |  "status" : "Pending",
                 |  "created" : "2017-10-31T23:22:50.971Z",
                 |  "lastUpdated" : "2017-10-31T23:22:50.971Z",
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
    service: String,
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
                         |  "service" : "$service",
                         |  "clientId" : "$clientId",
                         |  "clientIdType" : "${StoredInvitation.clientIdTypeByService(service)}",
                         |  "suppliedClientId" : "$clientId",
                         |  "suppliedClientIdType" : "${StoredInvitation.clientIdTypeByService(service)}",
                         |  "status" : "Expired",
                         |  "created" : "2017-7-31T23:22:50.971Z",
                         |  "lastUpdated" : "2017-10-31T23:22:50.971Z",
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
    service: String,
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
                         |  "service" : "$service",
                         |  "clientId" : "$clientId",
                         |  "clientIdType" : "${StoredInvitation.clientIdTypeByService(service)}",
                         |  "suppliedClientId" : "$clientId",
                         |  "suppliedClientIdType" : "${StoredInvitation.clientIdTypeByService(service)}",
                         |  "status" : "Cancelled",
                         |  "created" : "2017-7-31T23:22:50.971Z",
                         |  "lastUpdated" : "2017-10-31T23:22:50.971Z",
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
    service: String,
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
                         |  "service" : "$service",
                         |  "clientId" : "$clientId",
                         |  "clientIdType" : "${StoredInvitation.clientIdTypeByService(service)}",
                         |  "suppliedClientId" : "$clientId",
                         |  "suppliedClientIdType" : "${StoredInvitation.clientIdTypeByService(service)}",
                         |  "status" : "Accepted",
                         |  "created" : "2017-10-31T23:22:50.971Z",
                         |  "lastUpdated" : "2017-10-31T23:22:50.971Z",
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

  def givenSetRelationshipEndedReturns(invitationId: InvitationId, status: Int) =
    stubFor(
      put(
        urlEqualTo(s"/agent-client-authorisation/invitations/${invitationId.value}/relationship-ended?endedBy=Agent"))
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

  def givenVatRegisteredClientReturns(vrn: Vrn, date: LocalDate, responseStatus: Int) =
    stubFor(
      get(urlEqualTo(
        s"/agent-client-authorisation/known-facts/organisations/vat/${vrn.value}/registration-date/${date.toString}"))
        .willReturn(aResponse()
          .withStatus(responseStatus)))

  def givenTrustClientReturns(utr: Utr, responseStatus: Int, body: String) =
    stubFor(
      get(urlEqualTo(
        s"/agent-client-authorisation/known-facts/organisations/trust/${utr.value}"))
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
        .withQueryParam("createdOnOrAfter", equalTo(LocalDate.now.minusDays(30).toString("yyyy-MM-dd")))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(halEnvelope(Seq(
              invitation(arn, "Pending", "HMRC-MTD-IT", "ni", "AB123456A", "foo1", "2017-12-18", false, None),
              invitation(arn, "Pending", "HMRC-MTD-VAT", "vrn", "101747696", "foo2", "2017-12-18", false, None),
              invitation(arn, "Pending", "PERSONAL-INCOME-RECORD", "ni", "AB123456B", "foo3", "2017-12-18", false, None),
              invitation(arn, "Accepted", "HMRC-MTD-IT", "ni", "AB123456A", "foo4", "2017-12-18", false, None),
              invitation(arn, "Accepted", "HMRC-MTD-VAT", "vrn", "101747696", "foo5", "2017-12-18", false, None),
              invitation(arn, "Accepted", "PERSONAL-INCOME-RECORD", "ni", "AB123456B", "foo6", "2017-12-18", false, None),
              invitation(arn, "Rejected", "HMRC-MTD-IT", "ni", "AB123456A", "foo7", "2017-12-18", false, None),
              invitation(arn, "Rejected", "HMRC-MTD-VAT", "vrn", "101747696", "foo2", "2017-12-18", false, None),
              invitation(arn, "Rejected", "PERSONAL-INCOME-RECORD", "ni", "AB123456B", "foo8", "2017-12-18", false, None),
              invitation(arn, "Cancelled", "HMRC-MTD-IT", "ni", "AB123456A", "foo9", "2017-12-18", false, None),
              invitation(arn, "Cancelled", "HMRC-MTD-VAT", "vrn", "101747696", "fo10", "2017-12-18", false, None),
              invitation(arn, "Cancelled", "PERSONAL-INCOME-RECORD", "ni", "AB123456B", "fo11", "2017-12-18", false, None),
              invitation(arn, "Expired", "HMRC-MTD-IT", "ni", "AB123456A", "fo12", "2017-12-18", false, None),
              invitation(arn, "Expired", "HMRC-MTD-VAT", "vrn", "101747696", "fo13", "2017-12-18", false, None),
              invitation(arn, "Expired", "PERSONAL-INCOME-RECORD", "ni", "AB123456B", "fo14", "2017-12-18", false, None),
              invitation(arn, "Pending", "HMRC-MTD-IT", "ni", "AB123456A", "foo1", "2099-01-01", false, None),
              invitation(arn, "Pending", "HMRC-MTD-VAT", "vrn", "101747696", "foo2", "2099-01-01", false, None),
              invitation(arn, "Pending", "PERSONAL-INCOME-RECORD", "ni", "AB123456B", "foo3", "2099-01-01", false, None)
            ).mkString("[", ",", "]")))))

  def givenGetAllPendingInvitationsReturnsSome(arn: Arn, clientId: String, service: String) = {
    val body = halEnvelope(Seq(service match {
      case  "HMRC-MTD-IT" => invitation(arn, "Pending", "HMRC-MTD-IT", "ni", clientId, "foo1", "2017-12-18", false, None)
      case  "HMRC-MTD-VAT" => invitation(arn, "Pending", "HMRC-MTD-VAT", "vrn", clientId, "foo2", "2017-12-18", false, None)
      case  "PERSONAL-INCOME-RECORD" => invitation(arn, "Pending", "PERSONAL-INCOME-RECORD", "ni", clientId, "foo3", "2017-12-18", false, None)
    }).mkString("[", ",", "]"))

    stubFor(get(urlPathEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent"))
    .withQueryParam("status", equalTo("Pending"))
    .withQueryParam("clientId", equalTo(clientId))
    .withQueryParam("service", equalTo(service))
    .willReturn(aResponse()
      .withStatus(200)
      .withBody(body)))
  }

  def givenGetAllPendingInvitationsReturnsEmpty(arn: Arn, clientId: String, service: String) = {
    stubFor(get(urlPathEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent"))
      .withQueryParam("status", equalTo("Pending"))
      .withQueryParam("clientId", equalTo(clientId))
      .withQueryParam("service", equalTo(service))
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
    service: String,
    clientIdType: String,
    clientId: String,
    invitationId: String,
    expiryDate: String,
                   isRelationshipEnded: Boolean,
                   relationshipEndedBy: Option[String]) => s"""
                              |{
                              |  "arn" : "${arn.value}",
                              |  "clientType" : "personal",
                              |  "service" : "$service",
                              |  "clientId" : "$clientId",
                              |  "clientIdType" : "$clientIdType",
                              |  "suppliedClientId" : "$clientId",
                              |  "suppliedClientIdType" : "$clientIdType",
                              |  "status" : "$status",
                              |  "created" : "2017-10-31T23:22:50.971Z",
                              |  "lastUpdated" : "2018-09-11T21:02:00.000Z",
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
}
