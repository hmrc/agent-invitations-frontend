package uk.gov.hmrc.agentinvitationsfrontend.support

import org.scalatestplus.play.OneAppPerSuite
import play.api.Application
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.{AnyContentAsEmpty, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.{contentType, _}
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.agentinvitationsfrontend.stubs.AuthStubs
import uk.gov.hmrc.http.{HeaderCarrier, SessionKeys}
import uk.gov.hmrc.play.HeaderCarrierConverter
import uk.gov.hmrc.play.test.UnitSpec

abstract class BaseISpec extends UnitSpec with OneAppPerSuite with WireMockSupport with AuthStubs {

  override implicit lazy val app: Application = appBuilder.build()

  protected def appBuilder: GuiceApplicationBuilder = {
    new GuiceApplicationBuilder()
      .configure(
        "microservice.services.auth.port" -> wireMockPort,
        "microservice.services.agent-client-authorisation.port" -> wireMockPort
      )
  }


  protected implicit val materializer = app.materializer


  protected def authorisedAsValidAgent[A](request: FakeRequest[A], arn: String) = authenticated(request, Some(Enrolment("HMRC-AS-AGENT", "AgentReferenceNumber", arn)), isAgent = true)

  protected def authenticated[A](request: FakeRequest[A], enrolment: Option[Enrolment], isAgent: Boolean): FakeRequest[A] = {
    givenAuthorisedFor(enrolment, if(isAgent) "Agent" else "Individual")
    request.withSession(SessionKeys.authToken -> "Bearer XYZ")
  }


  protected def checkHtmlResultWithBodyText(result: Result, expectedSubstrings: String*): Unit = {
    status(result) shouldBe OK
    contentType(result) shouldBe Some("text/html")
    charset(result) shouldBe Some("utf-8")
    expectedSubstrings.foreach(s => bodyOf(result) should include(s))
  }

  private val messagesApi = app.injector.instanceOf[MessagesApi]
  private implicit val messages: Messages = messagesApi.preferred(Seq.empty[Lang])

  protected def htmlEscapedMessage(key: String, args: Any*): String = HtmlFormat.escape(Messages(key, args: _*)).toString

  implicit def hc(implicit request: FakeRequest[_]): HeaderCarrier = HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))

}


