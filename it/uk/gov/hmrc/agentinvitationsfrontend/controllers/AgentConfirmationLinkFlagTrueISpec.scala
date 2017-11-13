package uk.gov.hmrc.agentinvitationsfrontend.controllers
import play.api.test.FakeRequest
import uk.gov.hmrc.agentinvitationsfrontend.audit.AgentInvitationEvent
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.agentInvitationPostCodeForm
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentInvitationUserInput
import uk.gov.hmrc.agentinvitationsfrontend.stubs.DataStreamStubs
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, MtdItId}
import uk.gov.hmrc.domain.Nino


class AgentConfirmationLinkFlagTrueISpec extends BaseISpec with DataStreamStubs {

  override def secureUrlFlag: Boolean = true

  lazy val controller: AgentsInvitationController = app.injector.instanceOf[AgentsInvitationController]
  val arn = Arn("TARN0000001")
  val mtdItId = MtdItId("ABCDEF123456789")

  "POST /agents/invitation-sent" should {

    val request = FakeRequest("POST", "/agents/invitation-sent").withHeaders("HOST" -> s"$wireMockHost:$wireMockPort")
    val submitPostcode = controller.submitPostcode()

    "return 200 for authorised Agent with valid postcode and redirected to Confirm Invitation Page (secureFlag = true)" in {
      givenAuditConnector()
      createInvitationStub(arn, mtdItId, "1")
      getInvitationStub(arn, mtdItId, "1")
      val postcode = agentInvitationPostCodeForm.fill(AgentInvitationUserInput(Nino("AB123456A"), "BN12 6BX"))
      val result = submitPostcode(authorisedAsValidAgent(request
        .withFormUrlEncodedBody(postcode.data.toSeq: _*), arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(result, htmlEscapedMessage("invitation-sent-link.title"))
      checkHtmlResultWithBodyText(result, htmlEscapedMessage(s"https://$wireMockHost:$wireMockPort/invitations/1"))
      verifyAuthoriseAttempt()
      verifyAgentClientInvitationSubmittedEvent(arn.value, "AB123456A", "Success")
    }
  }

  def verifyAgentClientInvitationSubmittedEvent(arn: String, nino: String, result: String): Unit = {
    verifyAuditRequestSent(1, AgentInvitationEvent.AgentClientInvitationSubmitted,
      detail = Map(
        "result" -> result,
        "agentReferenceNumber" -> arn,
        "regimeId" -> nino,
        "regime" -> "HMRC-MTD-IT"
      ),
      tags = Map(
        "transactionName" -> "agent-client-invitation-submitted",
        "path" -> "/agents/invitation-sent"
      )
    )
  }
}
