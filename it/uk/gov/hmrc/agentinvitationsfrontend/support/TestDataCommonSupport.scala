package uk.gov.hmrc.agentinvitationsfrontend.support

import java.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.Nino

trait TestDataCommonSupport {

  val arn = Arn("TARN0000001")
  val nino = Nino("AB123456A")
  val validNino = Nino("AB123456A")
  val validNinoSpace = Nino("AB 12 34 56 A")

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
  val vrn = Vrn("101747696")

  val uid = "X4BZC17W"
  val normalisedAgentName = "99-with-flake"

  val fromFastTrack: Boolean = true
  val fromManual: Boolean = false
  val invalidInvitationId = InvitationId("ZTSF4OW9CCRPT")

  val hash = "12345678"

  val validUtr = Utr("4937455253")
  val validUrn = Urn("XXTRUST10000100")
  val invitationIdTrust = InvitationId("DF99K6PXSBHTF")
  val invitationIdTrustNT = InvitationId("FF99K6PXSBHTF")
  val identifierTrust = "UTR"

  val invitationIdCgt = InvitationId("EF99K6PXSBHTF")

  val invitationIdPpt = InvitationId("GF99K6PXSBHTG")
  val invitationIdCbc = InvitationId("HF99K6PXSBHTG")
  val invitationIdCbcNonUk = InvitationId("IF99K6PXSBHTG")

  val trustResponse = TrustResponse(Right(TrustName("some-trust")))
  val trustNotFoundJson =
    """{"code": "RESOURCE_NOT_FOUND","reason": "The remote endpoint has indicated that the trust is not found"}"""
  val invalidTrustJson =
    """{"code": "INVALID_TRUST_STATE","reason": "The remote endpoint has indicated that the Trust/Estate is Closed and playback is not possible"}"""

  val cgtRef = CgtRef("XMCGTP123456789")
  val pptRef = PptRef("XAPPT0000012345")
  val cbcId = CbcId("XACBC0516273849")

  val tpd = TypeOfPersonDetails("Individual", Left(IndividualName("firstName", "lastName")))

  def cgtAddressDetails(countryCode: String = "FR") =
    CgtAddressDetails("line1", Some("line2"), Some("line2"), Some("line2"), countryCode, Some("BN13 1FN"))

  def cgtSubscription(countryCode: String = "FR") = CgtSubscription("CGT", SubscriptionDetails(tpd, cgtAddressDetails(countryCode)))

  val cgtNotFoundJson = """[{"code":"NOT_FOUND","reason":"Data not found  for the provided Registration Number."}]"""

  val pptDefaultRegDate: LocalDate = LocalDate.parse("2021-01-01")
  def pptSubscription(regDate: LocalDate = pptDefaultRegDate) = PptSubscription("PPT", regDate, None)

  def pptSubscriptionSuccessBodyJson(pptRef: PptRef, registrationDate: LocalDate) =
    s"""{"pptReference": "${pptRef.value}",
                                                                               |"legalEntityDetails": {
                                                                               |"dateOfApplication": "$registrationDate",
                                                                               |"customerDetails": {
                                                                               |"customerType": "Organisation",
                                                                               |"organisationDetails": {
                                                                               |"organisationName": "Life Insuranco"
                                                                               |}
                                                                               |}
                                                                               |},
                                                                               |"changeOfCircumstanceDetails": {
                                                                               |"deregistrationDetails": {
                                                                               |"deregistrationDate": "2028-01-09"
                                                                               |}
                                                                               |}}""".stripMargin

  val pptNotFoundJson = """[{"code":"NOT_FOUND","reason":"Data not found  for the provided Registration Number."}]"""

  val cbcDefaultEmail = "cbc@email.com"
}
