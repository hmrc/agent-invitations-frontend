package uk.gov.hmrc.agentinvitationsfrontend.support

import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.Nino

trait TestDataCommonSupport {

  val arn = Arn("TARN0000001")
  val validNino = Nino("AB123456A")
  val validNinoSpace = Nino("AB 12 34 56 A")
  val nino = "AB123456A"

  val mtdItId = MtdItId("ABCDEF123456789")
  val serviceITSA = "HMRC-MTD-IT"
  val validPostcode = "DH14EJ"
  val validPostcodeLong = "BN114AW"
  val validPostcodeSpaces = "DH1 4EJ"
  val invitationIdITSA = InvitationId("ABERULMHCKKW3")
  val identifierITSA = "MTDITID"
  val expiryDate = LocalDate.parse("2010-10-10")

  val servicePIR = "PERSONAL-INCOME-RECORD"
  val invitationIdPIR = InvitationId("B9SCS2T4NZBAX")
  val identifierPIR = "NI"

  val invitationIdVAT = InvitationId("CZTW1KY6RTAAT")
  val serviceVAT = "HMRC-MTD-VAT"
  val identifierVAT = "VRN"
  val validVrn = Vrn("101747696")
  val validRegistrationDate = "2007-07-07"
  val dateOfBirth = "1980-07-07"
  val validVrn9755 = Vrn("101747641")
  val vrn = "101747696"

  val uid = "X4BZC17W"
  val normalisedAgentName = "99-with-flake"

  val fromFastTrack: Boolean = true
  val fromManual: Boolean = false
  val invalidInvitationId = InvitationId("ZTSF4OW9CCRPT")

  val hash = "12345678"

  val validUtr = Utr("4937455253")
  val invitationIdTrust = InvitationId("DF99K6PXSBHTF")
  val serviceTrust = "HMRC-TERS-ORG"
  val identifierTrust = "UTR"

  val serviceCgt = "HMRC-CGT-PD"
  val invitationIdCgt = InvitationId("EF99K6PXSBHTF")

  val trustResponse = TrustResponse(Right(TrustName("some-trust")))
  val trustNotFoundJson =
    """{"code": "RESOURCE_NOT_FOUND","reason": "The remote endpoint has indicated that the trust is not found"}"""
  val invalidTrustJson =
    """{"code": "INVALID_TRUST_STATE","reason": "The remote endpoint has indicated that the Trust/Estate is Closed and playback is not possible"}"""

  val cgtRef = CgtRef("XMCGTP123456789")

  val tpd = TypeOfPersonDetails("Individual", Left(IndividualName("firstName", "lastName")))

  def cgtAddressDetails(countryCode: String = "FR") =
    CgtAddressDetails("line1", Some("line2"), Some("line2"), Some("line2"), countryCode, Some("BN13 1FN"))

  def cgtSubscription(countryCode: String = "FR") = CgtSubscription("CGT", SubscriptionDetails(tpd, cgtAddressDetails(countryCode)))

  val cgtNotFoundJson = """[{"code":"NOT_FOUND","reason":"Data not found  for the provided Registration Number."}]"""

}
