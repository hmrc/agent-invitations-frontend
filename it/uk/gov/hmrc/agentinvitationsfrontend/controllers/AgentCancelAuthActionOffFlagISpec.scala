package uk.gov.hmrc.agentinvitationsfrontend.controllers

import java.util.UUID

import play.api.Application
import play.api.test.FakeRequest
import uk.gov.hmrc.agentinvitationsfrontend.support.BaseISpec
import uk.gov.hmrc.agentmtdidentifiers.model.{MtdItId, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

class AgentCancelAuthActionOffFlagISpec extends BaseISpec {

  override implicit lazy val app: Application =
    appBuilder(featureFlags)
      .build()

  lazy val requestTrackingController: AgentsRequestTrackingController =
    app.injector.instanceOf[AgentsRequestTrackingController]
  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId(UUID.randomUUID().toString)))

  "GET /track/" should {

    val request = FakeRequest("GET", "/track/")
    val showTrackRequests = requestTrackingController.showTrackRequests

    "render a page without cancel authorisation link when flag is off" in {
      givenGetInvitations(arn)
      givenInactiveITSARelationships(arn)
      givenInactiveVATRelationships(arn)
      givenInactiveAfiRelationship(arn)
      givenNinoForMtdItId(MtdItId("JKKL80894713304"), Nino("AB123456A"))
      givenNinoForMtdItId(MtdItId("ABCDE1234567890"), Nino("AB123456A"))
      givenTradingName(Nino("AB123456A"), "FooBar Ltd.")
      givenCitizenDetailsAreKnownFor("AB123456B", "John", "Smith")
      givenCitizenDetailsAreKnownFor("GZ753451B", "Cosmo", "Kramer")
      givenCitizenDetailsAreKnownFor("AB123456A", "Rodney", "Jones")
      givenClientDetails(Vrn("101747696"))
      givenClientDetails(Vrn("101747641"))
      val result = showTrackRequests(authorisedAsValidAgent(request, arn.value))
      status(result) shouldBe 200
      checkHtmlResultWithBodyText(
        result,
        "Accepted by client",
        "Client has not yet responded",
        "Declined by client",
        "Request expired as client did not respond in time",
        "You cancelled this request",
        "You cancelled your authorisation",
        "FooBar Ltd.",
        "John Smith",
        "Cosmo Kramer",
        "GDT",
        "11 September 2018",
        "21 September 2015",
        "24 September 2018",
        "01 January 2099",
        "Resend request to client",
        "Cancel this request",
        "Start new request",
        htmlEscapedMessage("recent-invitations.description", 30)
      )
      checkHtmlResultWithNotBodyText(result, "Cancel your authorisation")
      checkHtmlResultWithBodyMsgs(
        result,
        "recent-invitations.header",
        "recent-invitations.table-row-header.clientName",
        "recent-invitations.table-row-header.service",
        "recent-invitations.table-row-header.status",
        "recent-invitations.table-row-header.actions",
        "recent-invitations.invitation.service.HMRC-MTD-IT",
        "recent-invitations.invitation.service.HMRC-MTD-VAT",
        "recent-invitations.invitation.service.PERSONAL-INCOME-RECORD"
      )

    }
  }

}
