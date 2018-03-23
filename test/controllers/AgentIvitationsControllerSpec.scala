package controllers

import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.{ClientForMtdItWithFlagOn, ClientForPirWithFlagOn}
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentInvitationUserInput
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.test.UnitSpec

class AgentIvitationsControllerSpec extends BaseISpec {

  val featureFlags: FeatureFlags = app.injector.instanceOf[FeatureFlags]
  val arn = Arn("TARN0000001")
  val mtdItId = MtdItId("ABCDEF123456789")
  private val validNino = Nino("AB123456A")
  private val validNinoSpace = Nino("AB 12 34 56 A")
  val serviceITSA = "HMRC-MTD-IT"
  val servicePIR = "PERSONAL-INCOME-RECORD"
  val validPostcode = "BN12 6BX"
  val invitationIdITSA = InvitationId("ABERULMHCKKW3")
  val invitationIdPIR = InvitationId("B9SCS2T4NZBAX")

  val invitationIdVAT = InvitationId("CZTW1KY6RTAAT")
  val serviceVAT = "HMRC-MTD-VAT"
  val identifierVAT = "VRN"
  val validVrn97 = Vrn("101747696")
  val validRegDateForVrn97 = Some("2007-07-07")
  val validVrn9755 = Vrn("101747641")

  "A client for HMRC-MTD-IT" should {
    "Return a client identifier when details match" in {
      val args = (AgentInvitationUserInput(serviceITSA, Some(mtdItId), None), featureFlags)
      ClientForMtdItWithFlagOn.unapply(args) shouldBe Some(mtdItId)
    }
    "Return None when details don't match" in {
      val args = (AgentInvitationUserInput(serviceITSA, None, None), featureFlags)
      ClientForMtdItWithFlagOn.unapply(args) shouldBe None
    }

  }

  //Flag is set to off??
  "A client for PERSONAL-INCOME-RECORD" should {
    "Return true when details match" in {
      val args = (AgentInvitationUserInput(servicePIR, None, None), featureFlags)
      ClientForPirWithFlagOn.unapply(args) shouldBe None
    }
    "Return None when details dont match" in {
      val args = (AgentInvitationUserInput(serviceITSA, None, None), featureFlags)
      ClientForPirWithFlagOn.unapply(args) shouldBe None
    }
  }



}
