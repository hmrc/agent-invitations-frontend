package uk.gov.hmrc.agentinvitationsfrontend.connectors

import java.net.URL

import org.joda.time.DateTime
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentInvitation, Invitation}
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, MtdItId}
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier}

class InvitationsConnectorISpec extends BaseISpec {

  implicit val hc = HeaderCarrier()
  val connector = app.injector.instanceOf[InvitationsConnector]
  val arn = Arn("TARN0000001")
  val mtdItId = MtdItId("ABCDEF123456789")

  "Create Invitation" should {

    val agentInvitation = AgentInvitation("HMRC-MTD-IT", "ni", "AB123456B", "W12 7TQ")

    "return a link of a specific created invitation" in {
      createInvitationStub(arn, MtdItId("mtdItId"), "1")
      val result:Option[String] = await(connector.createInvitation(arn, agentInvitation))
      result.isDefined shouldBe true
      result.get should include("agent-client-authorisation/clients/MTDITID/mtdItId/invitations/received/1")
    }

    "return an error if unexpected response when creating invitation" in {
      failedCreateInvitation(arn, MtdItId("mtdItId"), "1")
      intercept[BadRequestException] {
        await(connector.createInvitation(arn, agentInvitation))
      }
    }
  }

  "Get Invitation" should {
    "return an invitation" in {
      getInvitationStub(arn, mtdItId, "1")
      val result = await(connector.getInvitation(s"/agent-client-authorisation/agencies/TARN0000001/invitations/sent/1"))
      result.isDefined shouldBe true
      result.get.arn shouldBe Arn("TARN0000001")
    }

    "return an error if invitation not found" in {
      getInvitationStub(arn, mtdItId, "1")
      val result = await(connector.getInvitation(s"/agent-client-authorisation/agencies/TARN0000001/invitations/sent/1"))

    }

    "return an error if unable to retrieve invitation" in {

    }
  }

}
