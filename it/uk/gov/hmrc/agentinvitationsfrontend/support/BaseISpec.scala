package uk.gov.hmrc.agentinvitationsfrontend.support

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import com.google.inject.AbstractModule
import org.scalatestplus.play.OneAppPerSuite
import play.api.Application
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.{contentType, _}
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.agentinvitationsfrontend.services.{ContinueUrlStoreService, FastTrackCache, FastTrackKeyStoreCache, InvitationsCache}
import uk.gov.hmrc.agentinvitationsfrontend.stubs._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.Future

abstract class BaseISpec
    extends UnitSpec with OneAppPerSuite with WireMockSupport with AuthStubs with ACAStubs with ASAStubs
    with AfiRelationshipStub with DataStreamStubs {

  override implicit lazy val app: Application = appBuilder.build()

  val companyAuthUrl = "https://company-auth-url"
  val companyAuthSignOutPath = "/sign-out-path"
  val businessTaxAccountUrl = "https://business-tax-account-url"
  val personalTaxAccountUrl = "https://personal-tax-account-url/pta"
  val taxAccountRelativeUrl = "/account"
  val agentFeedbackSurveyURNWithOriginToken = "/feedback-survey/?origin=INVITAGENT"

  protected def appBuilder: GuiceApplicationBuilder =
    new GuiceApplicationBuilder()
      .configure(
        "microservice.services.auth.port"                                     -> wireMockPort,
        "microservice.services.agent-client-authorisation.port"               -> wireMockPort,
        "microservice.services.agent-services-account.port"                   -> wireMockPort,
        "microservice.services.company-auth.login-url"                        -> wireMockHost,
        "microservice.services.company-auth.port"                             -> wireMockPort,
        "microservice.services.des.port"                                      -> wireMockPort,
        "microservice.services.agent-fi-relationship.port"                    -> wireMockPort,
        "microservice.services.agent-invitations-frontend.external-url"       -> wireMockBaseUrlAsString,
        "microservice.services.agent-services-account-frontend.external-url"  -> wireMockBaseUrlAsString,
        "microservice.services.company-auth-frontend.external-url"            -> companyAuthUrl,
        "microservice.services.company-auth-frontend.sign-out.path"           -> companyAuthSignOutPath,
        "microservice.services.business-tax-account.external-url"             -> businessTaxAccountUrl,
        "microservice.services.tax-account-router-frontend.account-url"       -> taxAccountRelativeUrl,
        "microservice.services.personal-tax-account.external-url"             -> personalTaxAccountUrl,
        "auditing.enabled"                                                    -> true,
        "auditing.consumer.baseUri.host"                                      -> wireMockHost,
        "auditing.consumer.baseUri.port"                                      -> wireMockPort,
        "features.show-hmrc-mtd-it"                                           -> true,
        "features.show-personal-income"                                       -> true,
        "features.show-hmrc-mtd-vat"                                          -> true,
        "features.show-kfc-mtd-it"                                            -> true,
        "features.show-kfc-personal-income"                                   -> false,
        "features.show-kfc-mtd-vat"                                           -> true,
        "features.enable-fast-track"                                          -> true,
        "microservice.services.agent-subscription-frontend.external-url"      -> "someSubscriptionExternalUrl",
        "microservice.services.agent-client-management-frontend.external-url" -> "someAgentClientManagementFrontendExternalUrl"
      )
      .overrides(new TestGuiceModule)

  def commonStubs(): Unit =
    givenAuditConnector()

  protected lazy val testFastTrackCache = new TestFastTrackCache

  protected lazy val continueUrlKeyStoreCache = new TestContinueUrlKeyStoreCache

  protected implicit val materializer = app.materializer

  protected def checkHtmlResultWithBodyText(result: Result, expectedSubstrings: String*): Unit = {
    contentType(result) shouldBe Some("text/html")
    charset(result) shouldBe Some("utf-8")
    expectedSubstrings.foreach(s => bodyOf(result) should include(s))
  }

  protected def checkHtmlResultWithBodyMsgs(result: Result, expectedMessageKeys: String*): Unit = {
    expectedMessageKeys.foreach { messageKey =>
      withClue(s"Message key '$messageKey' exists:") {
        Messages.isDefinedAt(messageKey) shouldBe true
      }
    }
    val expectedSubstrings = expectedMessageKeys.map(htmlEscapedMessage(_))
    checkHtmlResultWithBodyText(result, expectedSubstrings: _*)

    expectedSubstrings.foreach(s => bodyOf(result) should include(s))
  }

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    testFastTrackCache.clear()
    continueUrlKeyStoreCache.clear()
  }

  private class TestGuiceModule extends AbstractModule {
    override def configure(): Unit = {
      bind(classOf[FastTrackCache]).toInstance(testFastTrackCache)
      bind(classOf[ContinueUrlStoreService]).toInstance(continueUrlKeyStoreCache)
    }
  }

  protected def checkHtmlResultWithoutBodyText(result: Result, expectedSubstrings: String*): Unit = {
    contentType(result) shouldBe Some("text/html")
    charset(result) shouldBe Some("utf-8")
    expectedSubstrings.foreach(s => bodyOf(result) should not include s)
  }

  private val messagesApi = app.injector.instanceOf[MessagesApi]
  private implicit val messages: Messages = messagesApi.preferred(Seq.empty[Lang])

  protected def htmlEscapedMessage(key: String, args: Any*): String =
    HtmlFormat.escape(Messages(key, args: _*)).toString

  protected def hasMessage(key: String, args: Any*): String = Messages(key, args: _*).toString

  implicit def hc(implicit request: FakeRequest[_]): HeaderCarrier =
    HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))

  def checkInviteSentExitSurveyAgentSignOutLink(result: Future[Result]) = {
    checkHtmlResultWithBodyText(result, htmlEscapedMessage("common.sign-out"))
    val continueUrl = URLEncoder.encode(agentFeedbackSurveyURNWithOriginToken, StandardCharsets.UTF_8.name())
    checkHtmlResultWithBodyText(result, continueUrl)
  }
}
