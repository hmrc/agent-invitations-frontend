package uk.gov.hmrc.agentinvitationsfrontend.support

import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
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

  val servicePIR = "PERSONAL-INCOME-RECORD"
  val invitationIdPIR = InvitationId("B9SCS2T4NZBAX")
  val identifierPIR = "NI"

  val invitationIdVAT = InvitationId("CZTW1KY6RTAAT")
  val serviceVAT = "HMRC-MTD-VAT"
  val identifierVAT = "VRN"
  val validVrn = Vrn("101747696")
  val validVrn2 = Vrn("442820662")
  val invalidVrn = Vrn("101747692")
  val validRegistrationDate = "2007-07-07"
  val dateOfBirth = "1980-07-07"
  val validVrn9755 = Vrn("101747641")

  val personal = Some("personal")
  val business = Some("business")

  val uid = "X4BZC17W"
  val normalisedAgentName = "99-with-flake"

  val fromFastTrack: Boolean = true
  val fromManual: Boolean = false
  val invalidInvitationId = InvitationId("ZTSF4OW9CCRPT")

  val clientFeedbackSurveyURNWithOriginToken = "feedback-survey/?origin=INVITCLIENT"

  val hash = "12345678"

}
