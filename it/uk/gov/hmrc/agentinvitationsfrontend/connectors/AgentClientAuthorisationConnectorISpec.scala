package uk.gov.hmrc.agentinvitationsfrontend.connectors

import java.net.URL
import java.time.{LocalDate, LocalDateTime}
import play.api.test.Helpers._
import uk.gov.hmrc.agentinvitationsfrontend.UriPathEncoding._
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{Business, Personal}
import uk.gov.hmrc.agentinvitationsfrontend.models.KnownFactResult._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.support.{BaseISpec, TestDataCommonSupport}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, CbcIdType, CgtRefType, InvitationId, PlrIdType, PptRefType, Service, Vrn}
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.http._

import scala.concurrent.ExecutionContext.Implicits.global

class AgentClientAuthorisationConnectorISpec extends BaseISpec with TestDataCommonSupport {

  implicit val hc = HeaderCarrier()
  val connector = app.injector.instanceOf[AgentClientAuthorisationConnector]

  "Create Invitation" when {

    "createAgentLink" should {

      "return multi-invitation link for valid data" in {

        givenAgentReference(arn, hash, Personal)

        val result = await(connector.createAgentLink(arn, "personal"))
        result.isDefined shouldBe true
        result.get should include(
          s"invitations/$Personal/$hash/99-with-flake"
        )
      }

      "return None if unexpected response when creating multi-invitation link" in {
        givenAgentReferenceNotFound(arn, Personal)
        await(connector.createAgentLink(arn, "personal")) shouldBe None
      }
    }

    "getMultiInvitationRecord" should {

      "return multi-invitation record for valid uid" in {

        givenAgentReferenceRecordExistsForUid(arn, hash)

        val result = await(connector.getAgentReferenceRecord(hash))
        result.isDefined shouldBe true
        result.get shouldBe AgentReferenceRecord(hash, arn, Seq("99-with-flake", "My-Agency"))
      }
    }

    "service is for ITSA" should {
      val agentInvitationITSA = AgentInvitation(Some(Personal), "HMRC-MTD-IT", "ni", "AB123456B")
      "return a link of a ITSA created invitation" in {
        givenInvitationCreationSucceeds(arn, Some(Personal), "AB123456B", invitationIdITSA, "AB123456B", "ni", Service.MtdIt, identifierITSA)
        val result: Option[String] = await(connector.createInvitation(arn, agentInvitationITSA))
        result.isDefined shouldBe true
        result.get should include("agent-client-authorisation/clients/MTDITID/AB123456B/invitations/received/ABERULMHCKKW3")
      }

      "return None if unexpected response when creating ITSA invitation" in {
        givenInvitationCreationFails(arn)
        await(connector.createInvitation(arn, agentInvitationITSA)) shouldBe None
      }
    }

    "service is for PIR" should {
      val agentInvitationPIR = AgentInvitation(Some(Personal), "PERSONAL-INCOME-RECORD", "ni", "AB123456B")
      "return a link of a PIR created invitation" in {
        givenInvitationCreationSucceeds(
          arn,
          Some(Personal),
          "AB123456B",
          invitationIdPIR,
          "AB123456B",
          "ni",
          Service.PersonalIncomeRecord,
          identifierPIR
        )
        val result: Option[String] = await(connector.createInvitation(arn, agentInvitationPIR))
        result.isDefined shouldBe true
        result.get should include("agent-client-authorisation/clients/NI/AB123456B/invitations/received/B9SCS2T4NZBAX")
      }

      "return an error if unexpected response when creating PIR invitation" in {
        givenInvitationCreationFails(arn)
        await(connector.createInvitation(arn, agentInvitationPIR)) shouldBe None
      }
    }

    "service is for VAT" should {
      val agentInvitationVAT = AgentInvitation(Some(Business), "HMRC-MTD-VAT", "vrn", validVrn.value)
      "return a link of a VAT created invitation" in {
        givenInvitationCreationSucceeds(arn, Some(Business), validVrn.value, invitationIdVAT, validVrn.value, "vrn", Service.Vat, identifierVAT)
        val result: Option[String] = await(connector.createInvitation(arn, agentInvitationVAT))
        result.isDefined shouldBe true
        result.get should include("agent-client-authorisation/clients/VRN/101747696/invitations/received/CZTW1KY6RTAAT")
      }

      "return an error if unexpected response when creating VAT invitation" in {
        givenInvitationCreationFails(arn)
        await(connector.createInvitation(arn, agentInvitationVAT)) shouldBe None
      }
    }

    "service is for Trust" should {
      val agentInvitationTrust = AgentInvitation(Some(Business), "HMRC-TERS-ORG", "utr", validUtr.value)
      "return a link of a Trust created invitation" in {
        givenInvitationCreationSucceeds(arn, Some(Business), validUtr.value, invitationIdTrust, validUtr.value, "utr", Service.Trust, identifierTrust)
        val result: Option[String] = await(connector.createInvitation(arn, agentInvitationTrust))
        result.isDefined shouldBe true
        result.get should include("agent-client-authorisation/clients/UTR/4937455253/invitations/received/DF99K6PXSBHTF")
      }

      "return an error if unexpected response when creating Trust invitation" in {
        givenInvitationCreationFails(arn)
        await(connector.createInvitation(arn, agentInvitationTrust)) shouldBe None
      }
    }
  }

  "Get All Agency Invitations" should {

    "return all Agency invitations" in {
      givenGetInvitations(arn)
      val result: Seq[StoredInvitation] =
        await(connector.getAllInvitations(Arn("TARN0000001"), LocalDate.now().minusDays(30)))
      result should not be empty
      result.length shouldBe 18
      result.count(_.clientId == nino.value) shouldBe 6
      result.count(_.clientId == validVrn.value) shouldBe 6
      result.count(_.clientId == "AB123456B") shouldBe 6
      result.count(_.service == Service.MtdIt) shouldBe 6
      result.count(_.service == Service.Vat) shouldBe 6
      result.count(_.service == Service.PersonalIncomeRecord) shouldBe 6
      result.count(_.status == "Pending") shouldBe 6
      result.count(_.status == "Accepted") shouldBe 3
      result.count(_.status == "Rejected") shouldBe 3
      result.count(_.status == "Cancelled") shouldBe 3
      result.count(_.status == "Expired") shouldBe 3
      result(0) shouldBe StoredInvitation(
        arn,
        Some("personal"),
        Service.MtdIt,
        "AB123456A",
        Some(DetailsForEmail("agent@email.com", "someAgent", "The Client name")),
        "Pending",
        LocalDateTime.parse("2017-10-31T23:22:50.971"),
        LocalDateTime.parse("2018-09-11T00:00:00.000"),
        LocalDate.parse("2017-12-18"),
        "foo1",
        false,
        None,
        new URL(s"$wireMockBaseUrlAsString/agent-client-authorisation/agencies/${arn.value}/invitations/sent/foo1")
      )
      result(4) shouldBe StoredInvitation(
        arn,
        Some("personal"),
        Service.Vat,
        "101747696",
        Some(DetailsForEmail("agent@email.com", "someAgent", "The Client name")),
        "Accepted",
        LocalDateTime.parse("2017-10-31T23:22:50.971"),
        LocalDateTime.parse("2018-09-11T00:00:00.000"),
        LocalDate.parse("2017-12-18"),
        "foo5",
        false,
        None,
        new URL(s"$wireMockBaseUrlAsString/agent-client-authorisation/agencies/${arn.value}/invitations/sent/foo5")
      )
      result(8) shouldBe StoredInvitation(
        arn,
        Some("personal"),
        Service.PersonalIncomeRecord,
        "AB123456B",
        Some(DetailsForEmail("agent@email.com", "someAgent", "The Client name")),
        "Rejected",
        LocalDateTime.parse("2017-10-31T23:22:50.971"),
        LocalDateTime.parse("2018-09-11T00:00:00.000"),
        LocalDate.parse("2017-12-18"),
        "foo8",
        false,
        None,
        new URL(s"$wireMockBaseUrlAsString/agent-client-authorisation/agencies/${arn.value}/invitations/sent/foo8")
      )
    }

    "return an empty set of invitations" in {
      givenGetInvitationsReturnsEmpty(arn)
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
        givenInvitationExists(arn, mtdItId.value, invitationIdITSA, Service.MtdIt, identifierITSA, "Pending")
        val result = await(
          connector
            .getInvitation(getITSAInvitation)
        )
        result.arn shouldBe Arn("TARN0000001")
      }

      "return an error if invitation not found" in {
        givenInvitationNotFound(mtdItId.value, invitationIdITSA, identifierITSA)
        an[RuntimeException] shouldBe thrownBy(
          await(
            connector
              .getInvitation(getITSAInvitation)
          )
        )
      }
    }

    "service is for PIR" should {
      val getPIRInvitation =
        s"/agent-client-authorisation/clients/NI/${encodePathSegment(nino.value)}/invitations/received/${invitationIdPIR.value}"
      "return PIR Invitation" in {
        givenInvitationExists(arn, nino.value, invitationIdPIR, Service.PersonalIncomeRecord, identifierPIR, "Pending")
        val result = await(
          connector
            .getInvitation(getPIRInvitation)
        )
        result.arn shouldBe Arn("TARN0000001")
      }

      "return an error if PIR invitation not found" in {
        givenInvitationNotFound(nino.value, invitationIdPIR, identifierPIR)
        an[RuntimeException] shouldBe thrownBy(
          await(
            connector
              .getInvitation(getPIRInvitation)
          )
        )
      }
    }

    "service is for VAT" should {
      val getVATInvitation =
        s"/agent-client-authorisation/clients/VRN/${encodePathSegment(validVrn.value)}/invitations/received/${invitationIdVAT.value}"
      "return VAT Invitation" in {
        givenInvitationExists(arn, validVrn.value, invitationIdVAT, Service.Vat, identifierVAT, "Pending")
        val result = await(
          connector
            .getInvitation(getVATInvitation)
        )
        result.arn shouldBe Arn("TARN0000001")
      }

      "return an error if VAT invitation not found" in {
        givenInvitationNotFound(validVrn.value, invitationIdVAT, identifierVAT)
        an[RuntimeException] shouldBe thrownBy(
          await(
            connector
              .getInvitation(getVATInvitation)
          )
        )
      }
    }
  }

  "Cancel invitation" should {

    "return true if 204 is returned from DES" in {
      givenCancelInvitationReturns(arn, invitationIdITSA, 204)
      val result = await(connector.cancelInvitation(arn, invitationIdITSA))
      result shouldBe Some(true)
    }

    "return false if invitation is not found" in {
      givenCancelInvitationReturns(arn, invitationIdITSA, 404)
      val result = await(connector.cancelInvitation(arn, invitationIdITSA))
      result shouldBe Some(false)
    }

    "return None if status returned is invalid" in {
      givenCancelInvitationReturns(arn, invitationIdITSA, 403)
      val result = await(connector.cancelInvitation(arn, invitationIdITSA))
      result shouldBe None
    }
  }

  "Responding to an invitation" when {

    Seq[(String, Service, TaxIdentifier, InvitationId, String)](
      ("ITSA", Service.MtdIt, mtdItId, invitationIdITSA, identifierITSA),
      ("alt-ITSA", Service.MtdIt, nino, invitationIdITSA, "NI"),
      ("PIR", Service.PersonalIncomeRecord, nino, invitationIdPIR, identifierPIR),
      ("VAT", Service.Vat, validVrn, invitationIdVAT, identifierVAT),
      ("Trust", Service.Trust, validUtr, invitationIdTrust, identifierTrust),
      ("Trust (NT)", Service.TrustNT, validUrn, invitationIdTrustNT, "URN"),
      ("CGT", Service.CapitalGains, cgtRef, invitationIdCgt, CgtRefType.id),
      ("PPT", Service.Ppt, pptRef, invitationIdPpt, PptRefType.id),
      ("CBC", Service.Cbc, cbcId, invitationIdCbc, CbcIdType.id),
      ("CBC (non-UK)", Service.CbcNonUk, cbcId, invitationIdCbcNonUk, CbcIdType.id),
      ("Pillar2", Service.Pillar2, plrId, invitationIdPillar2, PlrIdType.id)
    ).foreach { case (serviceName, service, taxId, invitationId, taxIdTag) =>
      s"accepting an invitation for $serviceName" should {
        "return true if invitation was accepted" in {
          givenAcceptInvitationSucceeds(taxId.value, invitationId, taxIdTag)
          val result = await(connector.respondToInvitation(service, taxId, invitationId, accepted = true))
          result shouldBe true
          verifyAcceptInvitationAttempt(taxId.value, invitationId, taxIdTag)
        }

        "return false if invitation is already actioned" in {
          givenAcceptInvitationReturnsAlreadyActioned(taxId.value, invitationId, taxIdTag)
          val result = await(connector.respondToInvitation(service, taxId, invitationId, accepted = true))
          result shouldBe false
          verifyAcceptInvitationAttempt(taxId.value, invitationId, taxIdTag)
        }

        "return an error if invitation not found" in {
          givenAcceptInvitationReturnsNotFound(taxId.value, invitationId, taxIdTag)
          val result = await(connector.respondToInvitation(service, taxId, invitationId, accepted = true))
          result shouldBe false
          verifyAcceptInvitationAttempt(taxId.value, invitationId, taxIdTag)
        }
      }

      s"rejecting an invitation for $serviceName" should {
        "return status 204 if invitation was rejected" in {
          givenRejectInvitationSucceeds(taxId.value, invitationId, taxIdTag)
          val result = await(connector.respondToInvitation(service, taxId, invitationId, accepted = false))
          result shouldBe true
          verifyRejectInvitationAttempt(taxId.value, invitationId, taxIdTag)
        }

        "return an error if invitation is already actioned" in {
          givenRejectInvitationReturnsAlreadyActioned(taxId.value, invitationId, taxIdTag)
          val result = await(connector.respondToInvitation(service, taxId, invitationId, accepted = false))
          result shouldBe false
          verifyRejectInvitationAttempt(taxId.value, invitationId, taxIdTag)
        }

        "return an error if invitation not found" in {
          givenRejectInvitationReturnsWithStatus(taxId.value, invitationId, taxIdTag)
          val result = await(connector.respondToInvitation(service, taxId, invitationId, accepted = false))
          result shouldBe false
          verifyRejectInvitationAttempt(taxId.value, invitationId, taxIdTag)
        }
      }
    }
  }

  "Check ITSA Registered Client KFC" should {
    "return Some(true) if DES/ETMP has a matching postcode" in {
      givenMatchingClientIdAndPostcode(nino, validPostcode)

      await(connector.checkPostcodeForClient(nino, validPostcode)) shouldBe Pass

      verifyCheckItsaRegisteredClientStubAttempt(nino, validPostcode)
    }

    "return Some(false) if DES/ETMP has customer ITSA information but has no matching postcode" in {
      givenNonMatchingClientIdAndPostcode(nino, validPostcode)

      await(connector.checkPostcodeForClient(nino, validPostcode)) shouldBe Fail(NotMatched)

      verifyCheckItsaRegisteredClientStubAttempt(nino, validPostcode)
    }

    "return None if DES/ETMP has no customer ITSA information" in {
      givenNotEnrolledClientITSA(nino, validPostcode)

      await(connector.checkPostcodeForClient(nino, validPostcode)) shouldBe Fail(NotFound)

      verifyCheckItsaRegisteredClientStubAttempt(nino, validPostcode)
    }

    "throws 5xx is DES/ETMP is unavailable" in {
      givenServiceUnavailableITSA(nino, validPostcode)

      await(connector.checkPostcodeForClient(nino, validPostcode)) shouldBe Fail(HttpStatus(502))

      verifyCheckItsaRegisteredClientStubAttempt(nino, validPostcode)
    }
  }

  "Check Vat Registered Client KFC" should {
    "return VatKnownFactCheckOk if DES/ETMP has a matching effectiveRegistrationDate" in {
      val suppliedDate = LocalDate.parse("2001-02-03")
      givenVatRegisteredClientReturns(validVrn, suppliedDate, 204)

      await(connector.checkVatRegisteredClient(validVrn, suppliedDate)) shouldBe Pass

      verifyCheckVatRegisteredClientStubAttempt(validVrn, suppliedDate)
    }

    "return VatRecordClientInsolvent if DES/ETMP has a matching effectiveRegistrationDate but client is insolvent" in {
      val suppliedDate = LocalDate.parse("2001-02-03")
      givenVatRegisteredClientReturns(validVrn, suppliedDate, 403, true)

      await(connector.checkVatRegisteredClient(validVrn, suppliedDate)) shouldBe Fail(VatClientInsolvent)

      verifyCheckVatRegisteredClientStubAttempt(validVrn, suppliedDate)
    }

    "return VatKnownFactNotMatched if DES/ETMP has customer VAT information but has no matching effectiveRegistrationDate" in {
      val suppliedDate = LocalDate.parse("2001-02-03")
      givenVatRegisteredClientReturns(validVrn, suppliedDate, 403)

      await(connector.checkVatRegisteredClient(validVrn, suppliedDate)) shouldBe Fail(NotMatched)

      verifyCheckVatRegisteredClientStubAttempt(validVrn, suppliedDate)
    }

    "return VatRecordMigrationInProgress if DES/ETMP VAT information is under going migration" in {
      val suppliedDate = LocalDate.parse("2001-02-03")
      givenVatRegisteredClientReturns(validVrn, suppliedDate, 423)

      await(connector.checkVatRegisteredClient(validVrn, suppliedDate)) shouldBe Fail(VatMigrationInProgress)

      verifyCheckVatRegisteredClientStubAttempt(validVrn, suppliedDate)
    }

    "return VatDetailsNotFound if DES/ETMP has no customer VAT information" in {
      val suppliedDate = LocalDate.parse("2001-02-03")
      givenVatRegisteredClientReturns(validVrn, suppliedDate, 404)

      await(connector.checkVatRegisteredClient(validVrn, suppliedDate)) shouldBe Fail(NotFound)

      verifyCheckVatRegisteredClientStubAttempt(validVrn, suppliedDate)
    }

    "throws 5xx is DES/ETMP is unavailable" in {
      val suppliedDate = LocalDate.parse("2001-02-03")
      givenVatRegisteredClientReturns(validVrn, suppliedDate, 502)

      assertThrows[RuntimeException] {
        await(connector.checkVatRegisteredClient(validVrn, suppliedDate))
      }

      verifyCheckVatRegisteredClientStubAttempt(validVrn, suppliedDate)
    }
  }

  "getAgencyNameClient" should {
    "return agency name for a valid arn for a client" in {
      givenGetAgencyNameClientStub(arn)

      val result = await(connector.getAgencyName(arn.value))

      result shouldBe Some("My Agency")
    }

    "return AgencyNameNotFound exception for an invalid arn" in {
      givenAgencyNameNotFoundClientStub(Arn("INVALID_ARN"))

      intercept[AgencyNameNotFound] {
        await(connector.getAgencyName("INVALID_ARN"))
      }
    }
  }

  "getAgencyEmail" should {
    "return an agency email for a valid agent" in {
      givenGetAgencyEmailAgentStub

      val result = await(connector.getAgencyEmail)

      result shouldBe "abc@xyz.com"
    }
    "return AgencyEmailNotFound when there is no email in the agents record" in {
      givenNoContentAgencyEmailAgentStub

      intercept[AgencyEmailNotFound] {
        await(connector.getAgencyEmail)
      }.getMessage shouldBe "No email found in the record for this agent"
    }
    "return AgencyEmailNotFound when there is no record" in {
      givenNotFoundAgencyEmailAgentStub

      intercept[AgencyEmailNotFound] {
        await(connector.getAgencyEmail)
      }.getMessage shouldBe "No record found for this agent"
    }
  }

  "getTradingName" should {
    val nino = Nino("AB123456C")
    "when supplied with a valid nino return the trading name for that nino" in {
      givenTradingName(nino, "FooBar Ltd")

      val result = await(connector.getTradingName(nino))
      result shouldBe Some("FooBar Ltd")
    }

    "when supplied with a valid nino but trading name is empty" in {
      givenTradingNameMissing(nino)

      val result = await(connector.getTradingName(nino))
      result shouldBe None
    }

    "when supplied with a valid nino but trading name is an empty string" in {
      givenTradingName(nino, "")

      val result = await(connector.getTradingName(nino))
      result shouldBe Some("")
    }

    "return None when the nino has no trading name" in {
      givenTradingNameNotFound(nino)

      val result = await(connector.getTradingName(nino))
      result shouldBe None
    }
  }

  "getCustomerDetails" should {
    val vrn = Vrn("101747696")
    val customerDetailsAll =
      CustomerDetails(Some("Gadgetron"), Some(IndividualDetails(Some("Mr"), Some("Winston"), Some("H"), Some("Greenburg"))), Some("GDT"))
    val customerDetailsOnlyPersonal =
      CustomerDetails(None, Some(IndividualDetails(Some("Mr"), Some("Winston"), Some("H"), Some("Greenburg"))), None)
    val customerDetailsOnlyOrganisation = CustomerDetails(Some("Gadgetron"), None, None)
    val customerDetailsOnlyTrading = CustomerDetails(None, None, Some("GDT"))
    val customerDetailsNone = CustomerDetails(None, None, None)

    "return customer details when vrn has details associated" in {
      givenClientDetails(vrn)

      val result = await(connector.getCustomerDetails(vrn))
      result shouldBe customerDetailsAll
    }

    "return only trading name when organisation name and personal name are not present" in {
      givenClientDetailsOnlyTrading(vrn)

      val result = await(connector.getCustomerDetails(vrn))
      result shouldBe customerDetailsOnlyTrading
    }

    "return only organisation name when trading name and personal name are not present" in {
      givenClientDetailsOnlyOrganisation(vrn)

      val result = await(connector.getCustomerDetails(vrn))
      result shouldBe customerDetailsOnlyOrganisation
    }

    "return only personal name when trading name and organisation names are not present" in {
      givenClientDetailsOnlyPersonal(vrn)

      val result = await(connector.getCustomerDetails(vrn))
      result shouldBe customerDetailsOnlyPersonal
    }

    "return not found when vrn has no details associated" in {
      givenClientDetailsNotFound(vrn)

      val result = await(connector.getCustomerDetails(vrn))
      result shouldBe customerDetailsNone
    }
  }

  "getNinoForMtdItId" should {
    "return nino for given mtditid" in {
      givenNinoForMtdItId(mtdItId, nino)
      val result = await(connector.getNinoForMtdItId(mtdItId))
      result shouldBe Some(nino)
    }
  }

  "getAllClientInvitationDetailsForAgent" should {
    "return List of InvitationDetails" in {
      givenAllInvitationIdsByStatus("uid123", "Partialauth", true)
      val result = await(connector.getAllClientInvitationDetailsForAgent("uid123"))
      result.nonEmpty shouldBe true

    }
  }

  "setRelationshipEnded" should {
    "return Some(true) if successful" in {
      givenSetRelationshipEndedReturns(arn, "123456789", Service.Vat, 204)
      val result = await(connector.setRelationshipEnded(arn, "123456789", "HMRC-MTD-VAT"))
      result shouldBe Some(true)
    }

    "return Some(false) if Not Found" in {
      givenSetRelationshipEndedReturns(arn, "123456789", Service.Vat, 404)
      val result = await(connector.setRelationshipEnded(arn, "123456789", "HMRC-MTD-VAT"))
      result shouldBe Some(false)
    }

    "return None if Service Unavailable" in {
      givenSetRelationshipEndedReturns(arn, "123456789", Service.Vat, 503)
      val result = await(connector.setRelationshipEnded(arn, "123456789", "HMRC-MTD-VAT"))
      result shouldBe None
    }
  }
}
