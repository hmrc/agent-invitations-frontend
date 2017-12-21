package uk.gov.hmrc.agentinvitationsfrontend.connectors

import uk.gov.hmrc.agentinvitationsfrontend.UriPathEncoding._
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentInvitation, Invitation}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier, NotFoundException, Upstream4xxResponse}

class InvitationsConnectorISpec extends BaseISpec {

  implicit val hc = HeaderCarrier()
  val connector = app.injector.instanceOf[InvitationsConnector]
  val arn = Arn("TARN0000001")
  val mtdItId = MtdItId("ABCDEF123456789")
  private val validNino = Nino("AB123456A")
  val invitationIdITSA = InvitationId("ABERULMHCKKW3")
  val invitationIdAFI = InvitationId("BT5YMLY6GG2L6")
  val identifierITSA = "MTDITID"
  val identifierAFI = "NI"

  val serviceITSA = "HMRC-MTD-IT"
  val servicePIR = "PERSONAL-INCOME-RECORD"

  val getITSAInvitation = s"/agent-client-authorisation/clients/MTDITID/${encodePathSegment(mtdItId.value)}/invitations/received/${invitationIdITSA.value}"
  val getAFIInvitation = s"/agent-client-authorisation/clients/NI/${encodePathSegment(validNino.value)}/invitations/received/${invitationIdAFI.value}"

  "Create Invitation" should {

    val agentInvitation = AgentInvitation("HMRC-MTD-IT", "ni", "AB123456B", Some("W12 7TQ"))

    "return a link of a specific created invitation" in {
      createInvitationStubForITSA(arn, "mtdItId", invitationIdITSA, "AB123456B", "W12 7TQ", serviceITSA, identifierITSA)
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
      getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA,"Pending")
      val result = await(connector
        .getInvitation(getITSAInvitation))
      result.arn shouldBe Arn("TARN0000001")
    }

    "return an error if invitation not found" in {
      notFoundGetInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      an[NotFoundException] shouldBe thrownBy (await(connector
        .getInvitation(getITSAInvitation)))
    }

    behave like anGetAFIInvitationEndpoint(connector)
  }

  "Accept invitation" should {
    "return status 204 if invitation was accepted" in {
      acceptInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      val result = await(connector.acceptITSAInvitation(mtdItId, invitationIdITSA))

      result shouldBe 204
      verifyAcceptInvitationAttempt(mtdItId.value, invitationIdITSA, identifierITSA)
    }

    "return an error if invitation is already actioned" in {
      alreadyActionedAcceptInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)

      intercept[Upstream4xxResponse] {
        await(connector.acceptITSAInvitation(mtdItId, invitationIdITSA))
      }

      verifyAcceptInvitationAttempt(mtdItId.value, invitationIdITSA, identifierITSA)
    }

    "return an error if invitation not found" in {
      notFoundAcceptInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)

      intercept[NotFoundException] {
        await(connector.acceptITSAInvitation(mtdItId, invitationIdITSA))
      }

      verifyAcceptInvitationAttempt(mtdItId.value, invitationIdITSA, identifierITSA)
    }

    behave like anAcceptAFIInvitationEndpoint(connector)
  }

  "Reject invitation" should {
    "return status 204 if invitation was rejected" in {
      rejectInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
      val result = await(connector.rejectITSAInvitation(mtdItId, invitationIdITSA))

      result shouldBe 204
      verifyRejectInvitationAttempt(mtdItId.value, invitationIdITSA, identifierITSA)
    }

    "return an error if invitation is already actioned" in {
      alreadyActionedRejectInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)

      intercept[Upstream4xxResponse] {
        await(connector.rejectITSAInvitation(mtdItId, invitationIdITSA))
      }

      verifyRejectInvitationAttempt(mtdItId.value, invitationIdITSA, identifierITSA)
    }

    "return an error if invitation not found" in {
      notFoundRejectInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)

      intercept[NotFoundException] {
        await(connector.rejectITSAInvitation(mtdItId, invitationIdITSA))
      }

      verifyRejectInvitationAttempt(mtdItId.value, invitationIdITSA, identifierITSA)
    }

    behave like anRejectAFIInvitationEndpoint(connector)
  }

  def anGetAFIInvitationEndpoint(invitationsConnector: InvitationsConnector): Unit = {
    "return AFI Invitation" in {
      getInvitationStub(arn, validNino.value, invitationIdAFI, servicePIR, identifierAFI,"Pending")
      val result = await(invitationsConnector
        .getInvitation(getAFIInvitation))
      result.arn shouldBe Arn("TARN0000001")
    }

    "return an error if AFI invitation not found" in {
      notFoundGetInvitationStub(validNino.value, invitationIdAFI, identifierAFI)
      an[NotFoundException] shouldBe thrownBy (await(invitationsConnector
        .getInvitation(getAFIInvitation)))
    }
  }

  def anAcceptAFIInvitationEndpoint(invitationsConnector: InvitationsConnector): Unit = {
    "return status 204 if AFI invitation was accepted" in {
      acceptInvitationStub(validNino.value, invitationIdAFI, identifierAFI)
      val result = await(invitationsConnector.acceptAFIInvitation(validNino, invitationIdAFI))
      result shouldBe 204
      verifyAcceptInvitationAttempt(validNino.value, invitationIdAFI, "NI")
    }

    "return an error if AFI invitation is already actioned" in {
      alreadyActionedAcceptInvitationStub(validNino.value, invitationIdAFI, identifierAFI)

      intercept[Upstream4xxResponse] {
        await(invitationsConnector.acceptAFIInvitation(validNino, invitationIdAFI))
      }

      verifyAcceptInvitationAttempt(validNino.value, invitationIdAFI, "NI")
    }

    "return an error if AFI invitation not found" in {
      notFoundAcceptInvitationStub(validNino.value, invitationIdAFI, identifierITSA)

      intercept[NotFoundException] {
        await(invitationsConnector.acceptAFIInvitation(validNino, invitationIdAFI))
      }

      verifyAcceptInvitationAttempt(validNino.value, invitationIdAFI, identifierAFI)
    }
  }

  def anRejectAFIInvitationEndpoint(invitationsConnector: InvitationsConnector): Unit = {
    "return status 204 if AFI invitation was rejected" in {
      rejectInvitationStub(validNino.value, invitationIdAFI, identifierAFI)
      val result = await(invitationsConnector.rejectAFIInvitation(validNino, invitationIdAFI))
      result shouldBe 204
      verifyRejectInvitationAttempt(validNino.value, invitationIdAFI, identifierAFI)
    }

    "return an error if AFI invitation is already actioned" in {
      alreadyActionedRejectInvitationStub(validNino.value, invitationIdAFI, identifierAFI)

      intercept[Upstream4xxResponse] {
        await(invitationsConnector.rejectAFIInvitation(validNino, invitationIdAFI))
      }

      verifyRejectInvitationAttempt(validNino.value, invitationIdAFI, identifierAFI)
    }

    "return an error if AFI invitation not found" in {
      notFoundRejectInvitationStub(validNino.value, invitationIdAFI, identifierAFI)

      intercept[NotFoundException] {
        await(invitationsConnector.rejectAFIInvitation(validNino, invitationIdAFI))
      }

      verifyRejectInvitationAttempt(validNino.value, invitationIdAFI, identifierAFI)
    }
  }
}
