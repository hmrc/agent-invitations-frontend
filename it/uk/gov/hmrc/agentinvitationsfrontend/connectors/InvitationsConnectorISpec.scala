package uk.gov.hmrc.agentinvitationsfrontend.connectors

import java.net.URL

import org.joda.time.{DateTime, LocalDate}
import uk.gov.hmrc.agentinvitationsfrontend.UriPathEncoding._
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentInvitation, MultiAgentInvitation, MultiInvitationRecord, StoredInvitation}
import uk.gov.hmrc.agentinvitationsfrontend.support.{BaseISpec, TestDataCommonSupport}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId}
import uk.gov.hmrc.http._

import scala.concurrent.ExecutionContext.Implicits.global

class InvitationsConnectorISpec extends BaseISpec with TestDataCommonSupport {

  implicit val hc = HeaderCarrier()
  val connector = app.injector.instanceOf[InvitationsConnector]

  "Create Invitation" when {

    "createAgentLink" should {

      val multiInvitation = MultiAgentInvitation("personal", Seq(InvitationId("ABBBBBBBBBBCA")))

      "return multi-invitation link for valid data" in {

        getAgentLinkStub(arn, hash, personal.get)

        val result = await(connector.createAgentLink(arn, "personal"))
        result.isDefined shouldBe true
        result.get should include(
          s"invitations/${personal.get}/$hash/99-with-flake"
        )
      }

      "return an error if unexpected response when creating multi-invitation link" in {
        failedGetAgentLinkStub(arn, personal.get)
        intercept[NotFoundException] {
          await(connector.createAgentLink(arn, "personal"))
        }
      }
    }

    "getMultiInvitationRecord" should {

      val multiInvitation = MultiAgentInvitation("personal", Seq(InvitationId("ABBBBBBBBBBCA")))

      "return multi-invitation record for valid uid" in {

        getAgentReferenceRecordStub(arn, hash, personal.get, Seq(InvitationId("ABBBBBBBBBBCA")))

        val result = await(connector.getAgentReferenceRecord(hash))
        result.isDefined shouldBe true
        result.get shouldBe MultiInvitationRecord(
          hash,
          arn,
          Seq(InvitationId("ABBBBBBBBBBCA")),
          personal.get,
          "99-with-flake",
          DateTime.parse("2017-10-31T23:22:50.971Z"))
      }
    }

    "service is for ITSA" should {
      val agentInvitationITSA = AgentInvitation("HMRC-MTD-IT", "ni", "AB123456B")
      "return a link of a ITSA created invitation" in {
        createInvitationStub(arn, "AB123456B", invitationIdITSA, "AB123456B", "ni", serviceITSA, identifierITSA)
        val result: Option[String] = await(connector.createInvitation(arn, agentInvitationITSA))
        result.isDefined shouldBe true
        result.get should include(
          "agent-client-authorisation/clients/MTDITID/AB123456B/invitations/received/ABERULMHCKKW3")
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
        createInvitationStub(arn, "AB123456B", invitationIdPIR, "AB123456B", "ni", servicePIR, identifierPIR)
        val result: Option[String] = await(connector.createInvitation(arn, agentInvitationPIR))
        result.isDefined shouldBe true
        result.get should include("agent-client-authorisation/clients/NI/AB123456B/invitations/received/B9SCS2T4NZBAX")
      }

      "return an error if unexpected response when creating PIR invitation" in {
        failedCreateInvitation(arn)
        intercept[BadRequestException] {
          await(connector.createInvitation(arn, agentInvitationPIR))
        }
      }
    }

    "service is for VAT" should {
      val agentInvitationVAT = AgentInvitation("HMRC-MTD-VAT", "vrn", validVrn.value)
      "return a link of a VAT created invitation" in {
        createInvitationStub(arn, validVrn.value, invitationIdVAT, validVrn.value, "vrn", serviceVAT, identifierVAT)
        val result: Option[String] = await(connector.createInvitation(arn, agentInvitationVAT))
        result.isDefined shouldBe true
        result.get should include("agent-client-authorisation/clients/VRN/101747696/invitations/received/CZTW1KY6RTAAT")
      }

      "return an error if unexpected response when creating VAT invitation" in {
        failedCreateInvitation(arn)
        intercept[BadRequestException] {
          await(connector.createInvitation(arn, agentInvitationVAT))
        }
      }
    }
  }

  "Get All Agency Invitations" should {

    "return all Agency invitations" in {
      givenAllInvitationsStub(arn)
      val result: Seq[StoredInvitation] =
        await(connector.getAllInvitations(Arn("TARN0000001"), LocalDate.now().minusDays(30)))
      result should not be empty
      result.length shouldBe 18
      result.count(_.clientId == validNino.value) shouldBe 6
      result.count(_.clientId == validVrn.value) shouldBe 6
      result.count(_.clientId == "AB123456B") shouldBe 6
      result.count(_.service == "HMRC-MTD-IT") shouldBe 6
      result.count(_.service == "HMRC-MTD-VAT") shouldBe 6
      result.count(_.service == "PERSONAL-INCOME-RECORD") shouldBe 6
      result.count(_.status == "Pending") shouldBe 6
      result.count(_.status == "Accepted") shouldBe 3
      result.count(_.status == "Rejected") shouldBe 3
      result.count(_.status == "Cancelled") shouldBe 3
      result.count(_.status == "Expired") shouldBe 3
      result(0) shouldBe StoredInvitation(
        arn,
        "HMRC-MTD-IT",
        "AB123456A",
        "Pending",
        DateTime.parse("2017-10-31T23:22:50.971Z"),
        DateTime.parse("2018-09-11T21:02:00.000Z"),
        LocalDate.parse("2017-12-18"),
        "foo1",
        new URL(s"$wireMockBaseUrlAsString/agent-client-authorisation/agencies/${arn.value}/invitations/sent/foo1")
      )
      result(4) shouldBe StoredInvitation(
        arn,
        "HMRC-MTD-VAT",
        "101747696",
        "Accepted",
        DateTime.parse("2017-10-31T23:22:50.971Z"),
        DateTime.parse("2018-09-11T21:02:00.000Z"),
        LocalDate.parse("2017-12-18"),
        "foo5",
        new URL(s"$wireMockBaseUrlAsString/agent-client-authorisation/agencies/${arn.value}/invitations/sent/foo5")
      )
      result(8) shouldBe StoredInvitation(
        arn,
        "PERSONAL-INCOME-RECORD",
        "AB123456B",
        "Rejected",
        DateTime.parse("2017-10-31T23:22:50.971Z"),
        DateTime.parse("2018-09-11T21:02:00.000Z"),
        LocalDate.parse("2017-12-18"),
        "foo8",
        new URL(s"$wireMockBaseUrlAsString/agent-client-authorisation/agencies/${arn.value}/invitations/sent/foo8")
      )
    }

    "return an empty set of invitations" in {
      givenAllInvitationsEmptyStub(arn)
      val result: Seq[StoredInvitation] =
        await(connector.getAllInvitations(Arn("TARN0000001"), LocalDate.now().minusDays(30)))
      result shouldBe empty
    }

  }

  "Get Invitation" when {

    "service is for ITSA" should {
      val getITSAInvitation =
        s"/agent-client-authorisation/clients/MTDITID/${encodePathSegment(mtdItId.value)}/invitations/received/${invitationIdITSA.value}"
      "return an invitation" in {
        getInvitationStub(arn, mtdItId.value, invitationIdITSA, serviceITSA, identifierITSA, "Pending")
        val result = await(
          connector
            .getInvitation(getITSAInvitation))
        result.arn shouldBe Arn("TARN0000001")
      }

      "return an error if invitation not found" in {
        notFoundGetInvitationStub(mtdItId.value, invitationIdITSA, identifierITSA)
        an[NotFoundException] shouldBe thrownBy(
          await(connector
            .getInvitation(getITSAInvitation)))
      }
    }

    "service is for PIR" should {
      val getPIRInvitation =
        s"/agent-client-authorisation/clients/NI/${encodePathSegment(validNino.value)}/invitations/received/${invitationIdPIR.value}"
      "return PIR Invitation" in {
        getInvitationStub(arn, validNino.value, invitationIdPIR, servicePIR, identifierPIR, "Pending")
        val result = await(
          connector
            .getInvitation(getPIRInvitation))
        result.arn shouldBe Arn("TARN0000001")
      }

      "return an error if PIR invitation not found" in {
        notFoundGetInvitationStub(validNino.value, invitationIdPIR, identifierPIR)
        an[NotFoundException] shouldBe thrownBy(
          await(connector
            .getInvitation(getPIRInvitation)))
      }
    }

    "service is for VAT" should {
      val getVATInvitation =
        s"/agent-client-authorisation/clients/VRN/${encodePathSegment(validVrn.value)}/invitations/received/${invitationIdVAT.value}"
      "return VAT Invitation" in {
        getInvitationStub(arn, validVrn.value, invitationIdVAT, serviceVAT, identifierVAT, "Pending")
        val result = await(
          connector
            .getInvitation(getVATInvitation))
        result.arn shouldBe Arn("TARN0000001")
      }

      "return an error if VAT invitation not found" in {
        notFoundGetInvitationStub(validVrn.value, invitationIdVAT, identifierVAT)
        an[NotFoundException] shouldBe thrownBy(
          await(connector
            .getInvitation(getVATInvitation)))
      }
    }
  }

  "Cancel invitation" should {

    "return true if 204 is returned from DES" in {
      cancelInvitationStub(arn, invitationIdITSA, 204)
      val result = await(connector.cancelInvitation(arn, invitationIdITSA))
      result shouldBe Some(true)
    }

    "return false if invitation is not found" in {
      cancelInvitationStub(arn, invitationIdITSA, 404)
      val result = await(connector.cancelInvitation(arn, invitationIdITSA))
      result shouldBe Some(false)
    }

    "return None if status returned is invalid" in {
      cancelInvitationStub(arn, invitationIdITSA, 403)
      val result = await(connector.cancelInvitation(arn, invitationIdITSA))
      result shouldBe None
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
        acceptInvitationStub(validVrn.value, invitationIdVAT, identifierVAT)
        val result = await(connector.acceptVATInvitation(validVrn, invitationIdVAT))
        result shouldBe 204
        verifyAcceptInvitationAttempt(validVrn.value, invitationIdVAT, identifierVAT)
      }

      "return an error if VAT invitation is already actioned" in {
        alreadyActionedAcceptInvitationStub(validVrn.value, invitationIdVAT, identifierVAT)
        intercept[Upstream4xxResponse] {
          await(connector.acceptVATInvitation(validVrn, invitationIdVAT))
        }
        verifyAcceptInvitationAttempt(validVrn.value, invitationIdVAT, identifierVAT)
      }

      "return an error if VAT invitation not found" in {
        notFoundAcceptInvitationStub(validVrn.value, invitationIdVAT, identifierVAT)
        intercept[NotFoundException] {
          await(connector.acceptVATInvitation(validVrn, invitationIdVAT))
        }
        verifyAcceptInvitationAttempt(validVrn.value, invitationIdVAT, identifierVAT)
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
        rejectInvitationStub(validVrn.value, invitationIdVAT, identifierVAT)
        val result = await(connector.rejectVATInvitation(validVrn, invitationIdVAT))
        result shouldBe 204
        verifyRejectInvitationAttempt(validVrn.value, invitationIdVAT, identifierVAT)
      }

      "return an error if VAT invitation is already actioned" in {
        alreadyActionedRejectInvitationStub(validVrn.value, invitationIdVAT, identifierVAT)
        intercept[Upstream4xxResponse] {
          await(connector.rejectVATInvitation(validVrn, invitationIdVAT))
        }
        verifyRejectInvitationAttempt(validVrn.value, invitationIdVAT, identifierVAT)
      }

      "return an error if VAT invitation not found" in {
        notFoundRejectInvitationStub(validVrn.value, invitationIdVAT, identifierVAT)
        intercept[NotFoundException] {
          await(connector.rejectVATInvitation(validVrn, invitationIdVAT))
        }
        verifyRejectInvitationAttempt(validVrn.value, invitationIdVAT, identifierVAT)
      }
    }
  }

  "Check ITSA Registered Client KFC" should {
    "return Some(true) if DES/ETMP has a matching postcode" in {
      givenMatchingClientIdAndPostcode(validNino, validPostcode)

      await(connector.checkPostcodeForClient(validNino, validPostcode)) shouldBe Some(true)

      verifyCheckItsaRegisteredClientStubAttempt(validNino, validPostcode)
    }

    "return Some(false) if DES/ETMP has customer ITSA information but has no matching postcode" in {
      givenNonMatchingClientIdAndPostcode(validNino, validPostcode)

      await(connector.checkPostcodeForClient(validNino, validPostcode)) shouldBe Some(false)

      verifyCheckItsaRegisteredClientStubAttempt(validNino, validPostcode)
    }

    "return None if DES/ETMP has no customer ITSA information" in {
      givenNotEnrolledClientITSA(validNino, validPostcode)

      await(connector.checkPostcodeForClient(validNino, validPostcode)) shouldBe None

      verifyCheckItsaRegisteredClientStubAttempt(validNino, validPostcode)
    }

    "throws 5xx is DES/ETMP is unavailable" in {
      givenServiceUnavailableITSA(validNino, validPostcode)

      assertThrows[Upstream5xxResponse] {
        await(connector.checkPostcodeForClient(validNino, validPostcode))
      }

      verifyCheckItsaRegisteredClientStubAttempt(validNino, validPostcode)
    }
  }

  "Check Vat Registered Client KFC" should {
    "return Some(true) if DES/ETMP has a matching effectiveRegistrationDate" in {
      val suppliedDate = LocalDate.parse("2001-02-03")
      checkVatRegisteredClientStub(validVrn, suppliedDate, 204)

      await(connector.checkVatRegisteredClient(validVrn, suppliedDate)) shouldBe Some(true)

      verifyCheckVatRegisteredClientStubAttempt(validVrn, suppliedDate)
    }

    "return Some(false) if DES/ETMP has customer VAT information but has no matching effectiveRegistrationDate" in {
      val suppliedDate = LocalDate.parse("2001-02-03")
      checkVatRegisteredClientStub(validVrn, suppliedDate, 403)

      await(connector.checkVatRegisteredClient(validVrn, suppliedDate)) shouldBe Some(false)

      verifyCheckVatRegisteredClientStubAttempt(validVrn, suppliedDate)
    }

    "return None if DES/ETMP has no customer VAT information" in {
      val suppliedDate = LocalDate.parse("2001-02-03")
      checkVatRegisteredClientStub(validVrn, suppliedDate, 404)

      await(connector.checkVatRegisteredClient(validVrn, suppliedDate)) shouldBe None

      verifyCheckVatRegisteredClientStubAttempt(validVrn, suppliedDate)
    }

    "throws 5xx is DES/ETMP is unavailable" in {
      val suppliedDate = LocalDate.parse("2001-02-03")
      checkVatRegisteredClientStub(validVrn, suppliedDate, 502)

      assertThrows[Upstream5xxResponse] {
        await(connector.checkVatRegisteredClient(validVrn, suppliedDate))
      }

      verifyCheckVatRegisteredClientStubAttempt(validVrn, suppliedDate)
    }
  }
}
