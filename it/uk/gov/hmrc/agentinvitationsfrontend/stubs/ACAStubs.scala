package uk.gov.hmrc.agentinvitationsfrontend.stubs

import com.github.tomakehurst.wiremock.client.WireMock.{put, status, _}
import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.UriPathEncoding._
import uk.gov.hmrc.agentinvitationsfrontend.models.{ClientType, StoredInvitation}
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, Vrn}
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

  def givenAgentReferenceRecordExistsForUid(arn: Arn, uid: String): Unit =
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

  def givenAgentReferenceRecordNotFoundForUid(uid: String): Unit =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/agencies/references/uid/$uid"))
        .willReturn(aResponse()
          .withStatus(404)))

  def givenAgentReferenceRecordExistsForArn(arn: Arn, uid: String): Unit =
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

  def givenAgentReferenceRecordNotFoundForArn(arn: Arn): Unit =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/agencies/references/arn/${arn.value}"))
        .willReturn(aResponse()
          .withStatus(404)))

  def givenAllInvitationIdsByStatus(uid: String, status: String): Unit =
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
                         |     "expiryDate": "9999-03-05"
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

  def givenAllInvitationIdsByStatusReturnsSomeDuplicated(uid: String, status: String): Unit =
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

  def givenAllInvitationIdsByStatusReturnsSomeExpired(uid: String, status: String): Unit =
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

  def givenAllPendingInvitationIdsReturnsFakePending(uid: String): Unit =
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

  def givenAllInvitationIdsByStatusReturnsNotFound(uid: String, status: String): Unit =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/clients/invitations/uid/$uid?status=$status"))
        .willReturn(
          aResponse()
            .withStatus(404)
        )
    )

  def givenAllInvitationIdsByStatusReturnsEmpty(uid: String, status: String): Unit =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/clients/invitations/uid/$uid?status=$status"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(s"""
                         |[]
                         |""".stripMargin)))

  def givenAgentReferenceNotFound(arn: Arn, clientType: ClientType): Unit =
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
    serviceIdentifier: String): Unit = {
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
                                       serviceIdentifier: String): Unit = {
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

  def givenInvitationCreationFails(arn: Arn): Unit =
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
    status: String): Unit =
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
                         |  "_links": {
                         |    	"self" : {
                         |			  "href" : "$wireMockBaseUrlAsString/agent-client-authorisation/agencies/${arn.value}/invitations/sent/${invitationId.value}"
                         |		  }
                         |  }
                         |}""".stripMargin)))

  def givenInvitationByIdSuccess(invitationId: InvitationId, clientId: String) =
    stubFor(
      get(urlEqualTo(s"/agent-client-authorisation/invitations/${invitationId.value}"))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(
              s"""
                 |{
                 |  "arn" : "TARN00001",
                 |  "service" : "HMRC-MTD-IT",
                 |  "clientId" : "$clientId",
                 |  "clientIdType" : "mtditid",
                 |  "suppliedClientId" : "$clientId",
                 |  "suppliedClientIdType" : "mtditid",
                 |  "status" : "Pending",
                 |  "created" : "2017-10-31T23:22:50.971Z",
                 |  "lastUpdated" : "2017-10-31T23:22:50.971Z",
                 |  "expiryDate" : "2017-12-18",
                 |  "invitationId": "$invitationId",
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
    serviceIdentifier: String): Unit =
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
    serviceIdentifier: String): Unit =
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
    serviceIdentifier: String): Unit =
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
                         |  "_links": {
                         |    	"self" : {
                         |			  "href" : "$wireMockBaseUrlAsString/agent-client-authorisation/agencies/${arn.value}/invitations/sent/${invitationId.value}"
                         |		  }
                         |  }
                         |}""".stripMargin)))

  def givenInvitationNotFound(clientId: String, invitationId: InvitationId, serviceIdentifier: String): Unit =
    stubFor(get(urlEqualTo(
      s"/agent-client-authorisation/clients/$serviceIdentifier/${encodePathSegment(clientId)}/invitations/received/${invitationId.value}"))
      .willReturn(aResponse()
        .withStatus(404)))

  def givenInvitationNoPermission(clientId: String, invitationId: InvitationId, serviceIdentifier: String): Unit =
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

  def givenAcceptInvitationSucceeds(clientId: String, invitationId: InvitationId, serviceIdentifier: String): Unit =
    acceptInvitationStub(clientId, invitationId, responseStatus = 204, serviceIdentifier)

  def givenAcceptInvitationReturnsNotFound(
    clientId: String,
    invitationId: InvitationId,
    serviceIdentifier: String): Unit =
    acceptInvitationStub(clientId, invitationId, responseStatus = 404, serviceIdentifier)

  private def acceptInvitationStub(
    clientId: String,
    invitationId: InvitationId,
    responseStatus: Int,
    serviceIdentifier: String): Unit = {
    val mtdItIdEncoded = encodePathSegment(clientId)
    val invitationIdEncoded = encodePathSegment(invitationId.value)
    stubFor(put(urlEqualTo(
      s"/agent-client-authorisation/clients/$serviceIdentifier/$mtdItIdEncoded/invitations/received/$invitationIdEncoded/accept"))
      .willReturn(aResponse()
        .withStatus(responseStatus)))
  }

  def givenGetInvitationReturnsAlreadyActioned(
    clientId: String,
    invitationId: InvitationId,
    serviceIdentifier: String): Unit =
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
    serviceIdentifier: String): Unit = {
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
    serviceIdentifier: String): Unit =
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
    serviceIdentifier: String): Unit = {
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

  def verifyAcceptInvitationAttempt(clientId: String, invitationId: InvitationId, serviceIdentifier: String): Unit = {
    val identifierEncoded = encodePathSegment(clientId)
    val invitationIdEncoded = encodePathSegment(invitationId.value)
    verify(
      1,
      putRequestedFor(urlEqualTo(
        s"/agent-client-authorisation/clients/$serviceIdentifier/$identifierEncoded/invitations/received/$invitationIdEncoded/accept"))
    )
  }

  def givenCancelInvitationReturns(arn: Arn, invitationId: InvitationId, status: Int): Unit =
    stubFor(
      put(
        urlEqualTo(s"/agent-client-authorisation/agencies/${arn.value}/invitations/sent/${invitationId.value}/cancel"))
        .willReturn(
          aResponse()
            .withStatus(status)
        )
    )

  def givenRejectInvitationSucceeds(clientId: String, invitationId: InvitationId, serviceIdentifier: String): Unit =
    rejectInvitationStub(clientId, invitationId, responseStatus = 204, serviceIdentifier)

  def givenRejectInvitationReturnsNotFound(
    clientId: String,
    invitationId: InvitationId,
    serviceIdentifier: String): Unit =
    rejectInvitationStub(clientId, invitationId, responseStatus = 404, serviceIdentifier)

  private def rejectInvitationStub(
    clientId: String,
    invitationId: InvitationId,
    responseStatus: Int,
    serviceIdentifier: String): Unit = {
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
    serviceIdentifier: String): Unit = {
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

  def verifyRejectInvitationAttempt(clientId: String, invitationId: InvitationId, serviceIdentifier: String): Unit = {
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

  def verifyCheckVatRegisteredClientStubAttempt(vrn: Vrn, date: LocalDate): Unit = {
    val vrnEncoded = encodePathSegment(vrn.value)
    val dateEncoded = encodePathSegment(date.toString)
    verify(
      1,
      getRequestedFor(
        urlEqualTo(
          s"/agent-client-authorisation/known-facts/organisations/vat/$vrnEncoded/registration-date/$dateEncoded")))
  }

  def verifyCheckItsaRegisteredClientStubAttempt(nino: Nino, postcode: String): Unit = {
    val ninoEncoded = encodePathSegment(nino.value)
    val postEncoded = encodePathSegment(postcode)
    verify(
      1,
      getRequestedFor(
        urlEqualTo(s"/agent-client-authorisation/known-facts/individuals/nino/$ninoEncoded/sa/postcode/$postEncoded")))
  }

  def verifyNoCheckVatRegisteredClientStubAttempt(): Unit =
    verify(
      0,
      getRequestedFor(urlPathMatching("/agent-client-authorisation/known-facts/organisations/.*/registration-date/.*")))

  def givenGetInvitations(arn: Arn): Unit =
    stubFor(
      get(urlPathEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent"))
        .withQueryParam("createdOnOrAfter", equalTo(LocalDate.now.minusDays(30).toString("yyyy-MM-dd")))
        .willReturn(
          aResponse()
            .withStatus(200)
            .withBody(halEnvelope(Seq(
              invitation(arn, "Pending", "HMRC-MTD-IT", "ni", "AB123456A", "foo1", "2017-12-18"),
              invitation(arn, "Pending", "HMRC-MTD-VAT", "vrn", "101747696", "foo2", "2017-12-18"),
              invitation(arn, "Pending", "PERSONAL-INCOME-RECORD", "ni", "AB123456B", "foo3", "2017-12-18"),
              invitation(arn, "Accepted", "HMRC-MTD-IT", "ni", "AB123456A", "foo4", "2017-12-18"),
              invitation(arn, "Accepted", "HMRC-MTD-VAT", "vrn", "101747696", "foo5", "2017-12-18"),
              invitation(arn, "Accepted", "PERSONAL-INCOME-RECORD", "ni", "AB123456B", "foo6", "2017-12-18"),
              invitation(arn, "Rejected", "HMRC-MTD-IT", "ni", "AB123456A", "foo7", "2017-12-18"),
              invitation(arn, "Rejected", "HMRC-MTD-VAT", "vrn", "101747696", "foo2", "2017-12-18"),
              invitation(arn, "Rejected", "PERSONAL-INCOME-RECORD", "ni", "AB123456B", "foo8", "2017-12-18"),
              invitation(arn, "Cancelled", "HMRC-MTD-IT", "ni", "AB123456A", "foo9", "2017-12-18"),
              invitation(arn, "Cancelled", "HMRC-MTD-VAT", "vrn", "101747696", "fo10", "2017-12-18"),
              invitation(arn, "Cancelled", "PERSONAL-INCOME-RECORD", "ni", "AB123456B", "fo11", "2017-12-18"),
              invitation(arn, "Expired", "HMRC-MTD-IT", "ni", "AB123456A", "fo12", "2017-12-18"),
              invitation(arn, "Expired", "HMRC-MTD-VAT", "vrn", "101747696", "fo13", "2017-12-18"),
              invitation(arn, "Expired", "PERSONAL-INCOME-RECORD", "ni", "AB123456B", "fo14", "2017-12-18"),
              invitation(arn, "Pending", "HMRC-MTD-IT", "ni", "AB123456A", "foo1", "2099-01-01"),
              invitation(arn, "Pending", "HMRC-MTD-VAT", "vrn", "101747696", "foo2", "2099-01-01"),
              invitation(arn, "Pending", "PERSONAL-INCOME-RECORD", "ni", "AB123456B", "foo3", "2099-01-01")
            ).mkString("[", ",", "]")))))

  def givenGetAllPendingInvitationsReturnsSome(arn: Arn, clientId: String, service: String): Unit = {
    val body = halEnvelope(Seq(service match {
      case  "HMRC-MTD-IT" => invitation(arn, "Pending", "HMRC-MTD-IT", "ni", clientId, "foo1", "2017-12-18")
      case  "HMRC-MTD-VAT" => invitation(arn, "Pending", "HMRC-MTD-VAT", "vrn", clientId, "foo2", "2017-12-18")
      case  "PERSONAL-INCOME-RECORD" => invitation(arn, "Pending", "PERSONAL-INCOME-RECORD", "ni", clientId, "foo3", "2017-12-18")
    }).mkString("[", ",", "]"))

    stubFor(get(urlPathEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent"))
    .withQueryParam("status", equalTo("Pending"))
    .withQueryParam("clientId", equalTo(clientId))
    .withQueryParam("service", equalTo(service))
    .willReturn(aResponse()
      .withStatus(200)
      .withBody(body)))
  }

  def givenGetAllPendingInvitationsReturnsEmpty(arn: Arn, clientId: String, service: String): Unit = {
    stubFor(get(urlPathEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent"))
      .withQueryParam("status", equalTo("Pending"))
      .withQueryParam("clientId", equalTo(clientId))
      .withQueryParam("service", equalTo(service))
      .willReturn(aResponse()
        .withStatus(200)
        .withBody(halEnvelope("[]"))))
  }

  def givenGetInvitationsReturnsEmpty(arn: Arn): Unit =
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
    expiryDate: String) => s"""
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
}
