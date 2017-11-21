package uk.gov.hmrc.agentinvitationsfrontend.connectors

import uk.gov.hmrc.agentinvitationsfrontend.UriPathEncoding._
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentInvitation
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, MtdItId}
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier, NotFoundException, Upstream4xxResponse}

class InvitationsConnectorISpec extends BaseISpec {

  implicit val hc = HeaderCarrier()
  val connector = app.injector.instanceOf[InvitationsConnector]
  val arn = Arn("TARN0000001")
  val mtdItId = MtdItId("ABCDEF123456789")
  val invitationId = "1"

  "Create Invitation" should {

    val agentInvitation = AgentInvitation("HMRC-MTD-IT", "ni", "AB123456B", "W12 7TQ")

    "return a link of a specific created invitation" in {
      createInvitationStub(arn, MtdItId("mtdItId"), "1", "AB123456B", "W12 7TQ")
      val result: Option[String] = await(connector.createInvitation(arn, agentInvitation))
      result.isDefined shouldBe true
      result.get should include("agent-client-authorisation/clients/MTDITID/mtdItId/invitations/received/1")
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
      getInvitationStub(arn, mtdItId, invitationId)
      val result = await(connector
        .getInvitation(s"/agent-client-authorisation/clients/MTDITID/${encodePathSegment(mtdItId.value)}/invitations/received/$invitationId"))
      result.isDefined shouldBe true
      result.get.arn shouldBe Arn("TARN0000001")
    }

    "return an error if invitation not found" in {
      notFoundGetInvitationStub(mtdItId, invitationId)
      val result = await(connector
        .getInvitation(s"/agent-client-authorisation/clients/MTDITID/${encodePathSegment(mtdItId.value)}/invitations/received/$invitationId"))

      result.isEmpty shouldBe true
    }
  }

  "Accept invitation" should {
    "return status 204 if invitation was accepted" in {
      acceptInvitationStub(mtdItId, invitationId)
      val result = await(connector.acceptInvitation(mtdItId, invitationId))

      result shouldBe 204
      verifyAcceptInvitationAttempt(mtdItId, invitationId)
    }

    "return an error if invitation is already actioned" in {
      alreadyActionedAcceptInvitationStub(mtdItId, invitationId)

      intercept[Upstream4xxResponse] {
        await(connector.acceptInvitation(mtdItId, invitationId))
      }

      verifyAcceptInvitationAttempt(mtdItId, invitationId)
    }

    "return an error if invitation not found" in {
      notFoundAcceptInvitationStub(mtdItId, invitationId)

      intercept[NotFoundException] {
        await(connector.acceptInvitation(mtdItId, invitationId))
      }

      verifyAcceptInvitationAttempt(mtdItId, invitationId)
    }
  }

  "Reject invitation" should {
    "return status 204 if invitation was rejected" in {
      rejectInvitationStub(mtdItId, invitationId)
      val result = await(connector.rejectInvitation(mtdItId, invitationId))

      result shouldBe 204
      verifyRejectInvitationAttempt(mtdItId, invitationId)
    }

    "return an error if invitation is already actioned" in {
      alreadyActionedRejectInvitationStub(mtdItId, invitationId)

      intercept[Upstream4xxResponse] {
        await(connector.rejectInvitation(mtdItId, invitationId))
      }

      verifyRejectInvitationAttempt(mtdItId, invitationId)
    }

    "return an error if invitation not found" in {
      notFoundRejectInvitationStub(mtdItId, invitationId)

      intercept[NotFoundException] {
        await(connector.rejectInvitation(mtdItId, invitationId))
      }

      verifyRejectInvitationAttempt(mtdItId, invitationId)
    }
  }
}
