package uk.gov.hmrc.agentinvitationsfrontend.stubs

import com.github.tomakehurst.wiremock.client.WireMock._
import uk.gov.hmrc.agentinvitationsfrontend.UriPathEncoding._
import uk.gov.hmrc.agentinvitationsfrontend.models.Invitation
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport
import uk.gov.hmrc.agentmtdidentifiers.model.{ Arn, MtdItId }

trait ACAStubs {
  me: WireMockSupport =>

  def createInvitationStub(arn: Arn, mtdItId: MtdItId, invitationId: String): Unit = {
    stubFor(post(urlEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent"))
      .willReturn(
        aResponse()
          .withStatus(201)
          .withHeader("location", s"$wireMockBaseUrlAsString/agent-client-authorisation/clients/MTDITID/${encodePathSegment(mtdItId.value)}/invitations/received/$invitationId")))
  }

  def failedCreateInvitation(arn: Arn): Unit = {
    stubFor(post(urlEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent"))
      .willReturn(
        aResponse()
          .withStatus(400)))
  }

  def failedCreateInvitationForNotEnrolled(arn: Arn): Unit = {
    stubFor(post(urlEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent"))
      .willReturn(
        aResponse()
          .withStatus(403).withBody(
            s"""
             |{
             |   "code":"CLIENT_REGISTRATION_NOT_FOUND",
             |   "message":"The Client's MTDfB registration was not found."
             |}
           """.stripMargin)))
  }

  def failedCreateInvitationFoInvalidPostcode(arn: Arn): Unit = {
    stubFor(post(urlEqualTo(s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent"))
      .willReturn(
        aResponse()
          .withStatus(403).withBody(
            s"""
             |{
             |   "code":"POSTCODE_DOES_NOT_MATCH",
             |   "message":"The submitted postcode did not match the client's postcode as held by HMRC."
             |}
           """.stripMargin)))
  }

  def getInvitationStub(arn: Arn, mtdItId: MtdItId, invitationId: String): Unit = {
    stubFor(get(urlEqualTo(s"/agent-client-authorisation/clients/MTDITID/${encodePathSegment(mtdItId.value)}/invitations/received/$invitationId"))
      .willReturn(
        aResponse()
          .withStatus(200)
          .withBody(
            s"""
               |{
               |  "arn" : "${arn.value}",
               |  "service" : "HMRC-MTD-IT",
               |  "clientId" : "${mtdItId.value}",
               |  "status" : "Pending",
               |  "created" : "2017-10-31T23:22:50.971Z",
               |  "lastUpdated" : "2017-10-31T23:22:50.971Z",
               |  "_links": {
               |    	"self" : {
               |			  "href" : "$wireMockBaseUrlAsString/agent-client-authorisation/agencies/${arn.value}/invitations/sent/$invitationId"
               |		  }
               |  }
               |}""".stripMargin)))
  }

  def notFoundGetInvitationStub(arn: Arn, mtdItId: MtdItId, invitationId: String): Unit = {
    stubFor(get(urlEqualTo(s"/agent-client-authorisation/clients/MTDITID/${encodePathSegment(mtdItId.value)}/invitations/received/$invitationId"))
      .willReturn(
        aResponse()
          .withStatus(404)))
  }

}
