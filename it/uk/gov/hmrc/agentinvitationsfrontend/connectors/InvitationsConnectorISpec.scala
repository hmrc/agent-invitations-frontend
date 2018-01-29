package uk.gov.hmrc.agentinvitationsfrontend.connectors

import uk.gov.hmrc.agentinvitationsfrontend.UriPathEncoding._
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentInvitation
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier, NotFoundException, Upstream4xxResponse}

class InvitationsConnectorISpec extends BaseISpec {

  implicit val hc = HeaderCarrier()
  val connector = app.injector.instanceOf[InvitationsConnector]

  val arn = Arn("TARN0000001")
  val mtdItId = MtdItId("ABCDEF123456789")
  val validNino = Nino("AB123456A")
  val validVrn97 = Vrn("101747696")

  val invitationIdITSA = InvitationId("ABERULMHCKKW3")
  val invitationIdPIR = InvitationId("BT5YMLY6GG2L6")

  val invitationIdVAT = InvitationId("CZTW1KY6RTAAT")
  val identifierITSA = "MTDITID"
  val identifierPIR = "NI"

  val identifierVAT = "VAT"
  val serviceITSA = "HMRC-MTD-IT"
  val servicePIR = "PERSONAL-INCOME-RECORD"

  val serviceVAT = "HMRC-MTD-VAT"

  "Create Invitation" when {

    "service is for ITSA" should {
      val agentInvitationITSA = AgentInvitation("HMRC-MTD-IT", "ni", "AB123456B")
      "return a link of a ITSA created invitation" in {
        createInvitationStubWithKnownFacts(arn, "mtdItId", invitationIdITSA, "AB123456B", serviceITSA, identifierITSA)
        val result: Option[String] = await(connector.createInvitation(arn, agentInvitationITSA))
        result.isDefined shouldBe true
        result.get should include("agent-client-authorisation/clients/MTDITID/mtdItId/invitations/received/ABERULMHCKKW3")
      }

      "return an error if unexpected response when creating ITSA invitation" in {
        failedCreateInvitation(arn)
        intercept[BadRequestException] {
          await(connector.createInvitation(arn, agentInvitationITSA))
        }
      }
    }

    "service is for PIR" should {
      val agentInvitationPIR = AgentInvitation("PERSONAL-INCOME-RECORD", "ni", "AB123456B")
      "return a link of a PIR created invitation" in {
        createInvitationStubForNoKnownFacts(arn, "AB123456B", invitationIdPIR, "AB123456B", "ni", servicePIR, identifierPIR)
        val result: Option[String] = await(connector.createInvitation(arn, agentInvitationPIR))
        result.isDefined shouldBe true
        result.get should include("agent-client-authorisation/clients/NI/AB123456B/invitations/received/BT5YMLY6GG2L6")
      }

      "return an error if unexpected response when creating PIR invitation" in {
        failedCreateInvitation(arn)
        intercept[BadRequestException] {
          await(connector.createInvitation(arn, agentInvitationPIR))
        }
      }
    }

    "service is for VAT" should {
      val agentInvitationVAT = AgentInvitation("HMRC-MTD-VAT", "vrn", validVrn97.value)
      "return a link of a VAT created invitation" in {
        createInvitationStubForNoKnownFacts(arn, validVrn97.value, invitationIdVAT, validVrn97.value, "vrn", serviceVAT, identifierVAT)
        val result: Option[String] = await(connector.createInvitation(arn, agentInvitationVAT))
        result.isDefined shouldBe true
        result.get should include("agent-client-authorisation/clients/VAT/101747696/invitations/received/CZTW1KY6RTAAT")
      }

      "return an error if unexpected response when creating VAT invitation" in {
        failedCreateInvitation(arn)
        intercept[BadRequestException] {
          await(connector.createInvitation(arn, agentInvitationVAT))
        }
      }
    }
  }

  "Get Invitation" when {

    "service is for ITSA" should {
      val getITSAInvitation = s"/agent-client-authorisation/clients/MTDITID/${encodePathSegment(mtdItId.value)}/invitations/received/${invitationIdITSA.value}"
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
    }

    "service is for PIR" should {
      val getPIRInvitation = s"/agent-client-authorisation/clients/NI/${encodePathSegment(validNino.value)}/invitations/received/${invitationIdPIR.value}"
      "return PIR Invitation" in {
        getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, identifierPIR,"Pending")
        val result = await(connector
          .getInvitation(getPIRInvitation))
        result.arn shouldBe Arn("TARN0000001")
      }

      "return an error if PIR invitation not found" in {
        notFoundGetInvitationStub(validNino.value, invitationIdPIR, identifierPIR)
        an[NotFoundException] shouldBe thrownBy (await(connector
          .getInvitation(getPIRInvitation)))
      }
    }

    "service is for VAT" should {
      val getVATInvitation = s"/agent-client-authorisation/clients/VAT/${encodePathSegment(validVrn97.value)}/invitations/received/${invitationIdVAT.value}"
      "return VAT Invitation" in {
        getInvitationStub(arn, validVrn97.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
        val result = await(connector
          .getInvitation(getVATInvitation))
        result.arn shouldBe Arn("TARN0000001")
      }

      "return an error if VAT invitation not found" in {
        notFoundGetInvitationStub(validVrn97.value, invitationIdVAT, identifierVAT)
        an[NotFoundException] shouldBe thrownBy (await(connector
          .getInvitation(getVATInvitation)))
      }
    }
  }

  "Accept invitation" should {

    "service is for ITSA" should {
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
    }

    "service is for PIR" should {
      "return status 204 if PIR invitation was accepted" in {
        acceptInvitationStub(validNino.value, invitationIdPIR, identifierPIR)
        val result = await(connector.acceptAFIInvitation(validNino, invitationIdPIR))
        result shouldBe 204
        verifyAcceptInvitationAttempt(validNino.value, invitationIdPIR, "NI")
      }

      "return an error if PIR invitation is already actioned" in {
        alreadyActionedAcceptInvitationStub(validNino.value, invitationIdPIR, identifierPIR)
        intercept[Upstream4xxResponse] {
          await(connector.acceptAFIInvitation(validNino, invitationIdPIR))
        }
        verifyAcceptInvitationAttempt(validNino.value, invitationIdPIR, "NI")
      }

      "return an error if PIR invitation not found" in {
        notFoundAcceptInvitationStub(validNino.value, invitationIdPIR, identifierPIR)
        intercept[NotFoundException] {
          await(connector.acceptAFIInvitation(validNino, invitationIdPIR))
        }
        verifyAcceptInvitationAttempt(validNino.value, invitationIdPIR, identifierPIR)
      }
    }

    "service is for VAT" should {
      "return status 204 if VAT invitation was accepted" in {
        acceptInvitationStub(validVrn97.value, invitationIdVAT, identifierVAT)
        val result = await(connector.acceptVATInvitation(validVrn97, invitationIdVAT))
        result shouldBe 204
        verifyAcceptInvitationAttempt(validVrn97.value, invitationIdVAT, identifierVAT)
      }

      "return an error if VAT invitation is already actioned" in {
        alreadyActionedAcceptInvitationStub(validVrn97.value, invitationIdVAT, identifierVAT)
        intercept[Upstream4xxResponse] {
          await(connector.acceptVATInvitation(validVrn97, invitationIdVAT))
        }
        verifyAcceptInvitationAttempt(validVrn97.value, invitationIdVAT, identifierVAT)
      }

      "return an error if VAT invitation not found" in {
        notFoundAcceptInvitationStub(validVrn97.value, invitationIdVAT, identifierVAT)
        intercept[NotFoundException] {
          await(connector.acceptVATInvitation(validVrn97, invitationIdVAT))
        }
        verifyAcceptInvitationAttempt(validVrn97.value, invitationIdVAT, identifierVAT)
      }
    }
  }

  "Reject invitation" when {

    "service is for ITSA" should {
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
    }

    "service is for PIR" should {
      "return status 204 if PIR invitation was rejected" in {
        rejectInvitationStub(validNino.value, invitationIdPIR, identifierPIR)
        val result = await(connector.rejectAFIInvitation(validNino, invitationIdPIR))
        result shouldBe 204
        verifyRejectInvitationAttempt(validNino.value, invitationIdPIR, identifierPIR)
      }

      "return an error if PIR invitation is already actioned" in {
        alreadyActionedRejectInvitationStub(validNino.value, invitationIdPIR, identifierPIR)
        intercept[Upstream4xxResponse] {
          await(connector.rejectAFIInvitation(validNino, invitationIdPIR))
        }
        verifyRejectInvitationAttempt(validNino.value, invitationIdPIR, identifierPIR)
      }

      "return an error if PIR invitation not found" in {
        notFoundRejectInvitationStub(validNino.value, invitationIdPIR, identifierPIR)
        intercept[NotFoundException] {
          await(connector.rejectAFIInvitation(validNino, invitationIdPIR))
        }
        verifyRejectInvitationAttempt(validNino.value, invitationIdPIR, identifierPIR)
      }
    }

    "service is for VAT" should {
      "return status 204 if VAT invitation was rejected" in {
        rejectInvitationStub(validVrn97.value, invitationIdVAT, identifierVAT)
        val result = await(connector.rejectVATInvitation(validVrn97, invitationIdVAT))
        result shouldBe 204
        verifyRejectInvitationAttempt(validVrn97.value, invitationIdVAT, identifierVAT)
      }

      "return an error if VAT invitation is already actioned" in {
        alreadyActionedRejectInvitationStub(validVrn97.value, invitationIdVAT, identifierVAT)
        intercept[Upstream4xxResponse] {
          await(connector.rejectVATInvitation(validVrn97, invitationIdVAT))
        }
        verifyRejectInvitationAttempt(validVrn97.value, invitationIdVAT, identifierVAT)
      }

      "return an error if VAT invitation not found" in {
        notFoundRejectInvitationStub(validVrn97.value, invitationIdVAT, identifierVAT)
        intercept[NotFoundException] {
          await(connector.rejectVATInvitation(validVrn97, invitationIdVAT))
        }
        verifyRejectInvitationAttempt(validVrn97.value, invitationIdVAT, identifierVAT)
      }
    }
  }
}
