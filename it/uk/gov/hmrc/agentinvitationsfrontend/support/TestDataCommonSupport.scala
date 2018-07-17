package uk.gov.hmrc.agentinvitationsfrontend.support

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import play.api.mvc.Result
import uk.gov.hmrc.agentinvitationsfrontend.audit.AgentInvitationEvent.AgentClientInvitationResponse
import uk.gov.hmrc.agentinvitationsfrontend.stubs.DataStreamStubs
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino

import scala.concurrent.Future

trait TestDataCommonSupport extends BaseISpec {

  val arn = Arn("TARN0000001")
  val validNino = Nino("AB123456A")
  val validNinoSpace = Nino("AB 12 34 56 A")
  val nino = "AB123456A"

  val mtdItId = MtdItId("ABCDEF123456789")
  val serviceITSA = "HMRC-MTD-IT"
  val validPostcode = "DH14EJ"
  val invitationIdITSA = InvitationId("ABERULMHCKKW3")
  val identifierITSA = "MTDITID"

  val servicePIR = "PERSONAL-INCOME-RECORD"
  val invitationIdPIR = InvitationId("B9SCS2T4NZBAX")
  val identifierPIR = "NI"

  val invitationIdVAT = InvitationId("CZTW1KY6RTAAT")
  val serviceVAT = "HMRC-MTD-VAT"
  val identifierVAT = "VRN"
  val validVrn = Vrn("101747696")
  val invalidVrn = Vrn("101747692")
  val validRegistrationDate = "2007-07-07"
  val dateOfBirth = "1980-07-07"
  val validVrn9755 = Vrn("101747641")

  val fromFastTrack: Boolean = true
  val fromManual: Boolean = false
  val invalidInvitationId = InvitationId("ZTSF4OW9CCRPT")

  val clientFeedbackSurveyURNWithOriginToken = "feedback-survey/?origin=INVITCLIENT"

  def verifyAgentInvitationResponseEvent(
                                          invitationId: InvitationId,
                                          arn: String,
                                          clientResponse: String,
                                          clientIdType: String,
                                          clientId: String,
                                          service: String,
                                          agencyName: String): Unit =
    verifyAuditRequestSent(
      1,
      AgentClientInvitationResponse,
      detail = Map(
        "invitationId"         -> invitationId.value,
        "agentReferenceNumber" -> arn,
        "agencyName"           -> agencyName,
        "clientIdType"         -> clientIdType,
        "clientId"             -> clientId,
        "service"              -> service,
        "clientResponse"       -> clientResponse
      ),
      tags = Map(
        "transactionName" -> "agent-client-invitation-response",
        "path"            -> "/"
      )
    )

  def checkHasClientSignOutUrl(result: Future[Result]) = {
    checkHtmlResultWithBodyText(result, htmlEscapedMessage("common.sign-out"))
    val continueUrl = URLEncoder.encode(s"$businessTaxAccountUrl/business-account", StandardCharsets.UTF_8.name())
    checkHtmlResultWithBodyText(result, s"$companyAuthUrl$companyAuthSignOutPath?continue=$continueUrl")
  }

  def checkExitSurveyAfterInviteResponseSignOutUrl(result: Future[Result]) = {
    checkHtmlResultWithBodyText(result, htmlEscapedMessage("common.sign-out"))
    val continueUrl = URLEncoder.encode(clientFeedbackSurveyURNWithOriginToken, StandardCharsets.UTF_8.name())
    checkHtmlResultWithBodyText(result, continueUrl)
  }
}
