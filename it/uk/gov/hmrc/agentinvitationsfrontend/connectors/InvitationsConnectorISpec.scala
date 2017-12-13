package uk.gov.hmrc.agentinvitationsfrontend.connectors

import uk.gov.hmrc.agentinvitationsfrontend.UriPathEncoding._
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentInvitation
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId}
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier, NotFoundException, Upstream4xxResponse}

class InvitationsConnectorISpec extends BaseISpec {

  implicit val hc = HeaderCarrier()
  val connector = app.injector.instanceOf[InvitationsConnector]
  val arn = Arn("TARN0000001")
  val mtdItId = MtdItId("ABCDEF123456789")
  val invitationIdITSA = InvitationId("ABERULMHCKKW3")

  val serviceITSA = "HMRC-MTD-IT"
  val servicePIR = "PERSONAL-INCOME-RECORD"

  "Create Invitation" should {

    val agentInvitation = AgentInvitation("HMRC-MTD-IT", "ni", "AB123456B", "W12 7TQ")

    "return a link of a specific created invitation" in {
      createInvitationStub(arn, "mtdItId", invitationIdITSA, "AB123456B", "W12 7TQ", serviceITSA)
      val result: Option[String] = await(connector.createInvitation(arn, agentInvitation))
      result.isDefined shouldBe true
      result.get should include("agent-client-authorisation/clients/MTDITID/mtdItId/invitations/received/ABERULMHCKKW3")
    }

    "return an error if unexpected response when creating invitation" in {
      failedCreateInvitation(arn)
      intercept[BadRequestException] {
        await(connector.createInvitation(arn, agentInvitation))
      }
    }
  }

  "Get Invitation" should {
    "return an invitation" in {
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA)
      val result = await(connector
        .getInvitation(s"/agent-client-authorisation/clients/MTDITID/${encodePathSegment(mtdItId.value)}/invitations/received/${invitationIdITSA.value}"))
      result.isDefined shouldBe true
      result.get.arn shouldBe Arn("TARN0000001")
    }

    "return an error if invitation not found" in {
      notFoundGetInvitationStub(mtdItId.value, invitationIdITSA)
      val result = await(connector
        .getInvitation(s"/agent-client-authorisation/clients/MTDITID/${encodePathSegment(mtdItId.value)}/invitations/received/${invitationIdITSA.value}"))

      result.isEmpty shouldBe true
    }
  }

  "Accept invitation" should {
    "return status 204 if invitation was accepted" in {
      acceptInvitationStub(mtdItId.value, invitationIdITSA)
      val result = await(connector.acceptInvitation(mtdItId, invitationIdITSA))

      result shouldBe 204
      verifyAcceptInvitationAttempt(mtdItId.value, invitationIdITSA)
    }

    "return an error if invitation is already actioned" in {
      alreadyActionedAcceptInvitationStub(mtdItId.value, invitationIdITSA)

      intercept[Upstream4xxResponse] {
        await(connector.acceptInvitation(mtdItId, invitationIdITSA))
      }

      verifyAcceptInvitationAttempt(mtdItId.value, invitationIdITSA)
    }

    "return an error if invitation not found" in {
      notFoundAcceptInvitationStub(mtdItId.value, invitationIdITSA)

      intercept[NotFoundException] {
        await(connector.acceptInvitation(mtdItId, invitationIdITSA))
      }

      verifyAcceptInvitationAttempt(mtdItId.value, invitationIdITSA)
    }
  }

  "Reject invitation" should {
    "return status 204 if invitation was rejected" in {
      rejectInvitationStub(mtdItId.value, invitationIdITSA)
      val result = await(connector.rejectInvitation(mtdItId, invitationIdITSA))

      result shouldBe 204
      verifyRejectInvitationAttempt(mtdItId.value, invitationIdITSA)
    }

    "return an error if invitation is already actioned" in {
      alreadyActionedRejectInvitationStub(mtdItId.value, invitationIdITSA)

      intercept[Upstream4xxResponse] {
        await(connector.rejectInvitation(mtdItId, invitationIdITSA))
      }

      verifyRejectInvitationAttempt(mtdItId.value, invitationIdITSA)
    }

    "return an error if invitation not found" in {
      notFoundRejectInvitationStub(mtdItId.value, invitationIdITSA)

      intercept[NotFoundException] {
        await(connector.rejectInvitation(mtdItId, invitationIdITSA))
      }

      verifyRejectInvitationAttempt(mtdItId.value, invitationIdITSA)
    }
  }
}
