package uk.gov.hmrc.agentinvitationsfrontend.support

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import akka.stream.Materializer
import com.github.tomakehurst.wiremock.stubbing.StubMapping
import com.google.inject.AbstractModule
import org.scalatest.Assertion
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.Application
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.{contentType, _}
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.agentinvitationsfrontend.audit.AgentInvitationEvent
import uk.gov.hmrc.agentinvitationsfrontend.audit.AgentInvitationEvent.AgentClientInvitationResponse
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentinvitationsfrontend.stubs._
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.Future

abstract class BaseISpec
    extends UnitSpec with GuiceOneServerPerSuite with WireMockSupport with AuthStubs with ACAStubs
    with CitizenDetailsStub with AfiRelationshipStub with DataStreamStubs with ACRStubs with SSOStubs
    with TestDataCommonSupport with MongoSupport with IVStubs with PDVStubs {

  override implicit lazy val app: Application = appBuilder.build()

  val pdvFrontendUrl = "http://localhost:9968"
  val companyAuthUrl = "https://company-auth-url"
  val companyAuthSignOutPath = "/sign-out-path"
  val businessTaxAccountUrl = "https://business-tax-account-url"
  val personalTaxAccountUrl = "https://personal-tax-account-url/pta"
  val taxAccountRelativeUrl = "/account"
  val agentFeedbackSurveyURNWithOriginToken = "/feedback-survey/?origin=INVITAGENT"
  val pdvBaseUrl = "/pdv-base-url"

  val problemHeader = "There is a problem - Ask a client to authorise you - GOV.UK"

  protected def appBuilder: GuiceApplicationBuilder =
    new GuiceApplicationBuilder()
      .configure(
        "microservice.services.auth.port"                                         -> wireMockPort,
        "microservice.services.agent-client-authorisation.port"                   -> wireMockPort,
        "microservice.services.agent-client-relationships.port"                   -> wireMockPort,
        "microservice.services.company-auth.login-url"                            -> wireMockHost,
        "microservice.services.company-auth.port"                                 -> wireMockPort,
        "microservice.services.des.port"                                          -> wireMockPort,
        "microservice.services.agent-fi-relationship.port"                        -> wireMockPort,
        "microservice.services.citizen-details.host"                              -> wireMockHost,
        "microservice.services.citizen-details.port"                              -> wireMockPort,
        "microservice.services.agent-invitations-frontend.external-url"           -> wireMockBaseUrlAsString,
        "microservice.services.agent-services-account-frontend.external-url"      -> wireMockBaseUrlAsString,
        "microservice.services.company-auth-frontend.external-url"                -> companyAuthUrl,
        "microservice.services.company-auth-frontend.sign-out.path"               -> companyAuthSignOutPath,
        "microservice.services.business-tax-account.external-url"                 -> businessTaxAccountUrl,
        "microservice.services.personal-tax-account.external-url"                 -> personalTaxAccountUrl,
        "microservice.services.citizen-details.host"                              -> wireMockHost,
        "microservice.services.citizen-details.port"                              -> wireMockPort,
        "microservice.services.sso.host"                                          -> wireMockHost,
        "microservice.services.sso.port"                                          -> wireMockPort,
        "microservice.services.identity-verification-frontend.host"               -> wireMockHost,
        "microservice.services.identity-verification-frontend.port"               -> wireMockPort,
        "microservice.services.identity-verification.host"                        -> wireMockHost,
        "microservice.services.identity-verification.port"                        -> wireMockPort,
        "microservice.services.personal-details-validation.host"                  -> wireMockHost,
        "microservice.services.personal-details-validation.port"                  -> wireMockPort,
        "microservice.services.personal-details-validation-frontend.external-url" -> pdvFrontendUrl,
        "auditing.enabled"                                                        -> true,
        "auditing.consumer.baseUri.host"                                          -> wireMockHost,
        "auditing.consumer.baseUri.port"                                          -> wireMockPort,
        "metrics.jvm"                                                             -> false,
        "metrics.logback"                                                         -> false,
        "passcodeAuthentication.enabled"                                          -> false,
        "track-requests-per-page"                                                 -> 10,
        "features.show-hmrc-mtd-it"                                               -> true,
        "features.show-personal-income"                                           -> true,
        "features.show-hmrc-mtd-vat"                                              -> true,
        "features.show-hmrc-trust"                                                -> true,
        "features.show-hmrc-cgt"                                                  -> true,
        "features.enable-agent-suspension"                                        -> true,
        "features.enable-track-cancel-auth-action"                                -> true,
        "features.show-agent-led-de-auth"                                         -> true,
        "features.enable-welsh-toggle"                                            -> true,
        "microservice.services.agent-subscription-frontend.external-url"          -> "someSubscriptionExternalUrl",
        "microservice.services.agent-client-management-frontend.external-url"     -> "someAgentClientManagementFrontendExternalUrl",
        "mongodb.uri"                                                             -> "mongodb://localhost:27017/agent-invitations-frontend?rm.monitorRefreshMS=1000&rm.failover=default"
      )
      .overrides(new TestGuiceModule)

  def commonStubs(): Seq[StubMapping] = givenAuditConnector()

  implicit val appConfig: AppConfig = app.injector.instanceOf[AppConfig]

  protected implicit val materializer: Materializer = app.materializer

  protected def checkHtmlResultWithBodyText(result: Result, expectedSubstrings: String*): Unit = {
    contentType(result) shouldBe Some("text/html")
    charset(result) shouldBe Some("utf-8")
    expectedSubstrings.foreach(s => bodyOf(result) should include(s))
  }

  protected def checkHtmlResultWithNotBodyText(result: Result, expectedSubstrings: String*): Unit = {
    contentType(result) shouldBe Some("text/html")
    charset(result) shouldBe Some("utf-8")
    expectedSubstrings.foreach(s => bodyOf(result) should not include s)
  }

  protected def checkIncludesText(result: Future[Result], expectedSubstrings: String*): Unit =
    expectedSubstrings.foreach { substring =>
      contentAsString(result) should include(substring.toString)
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
    dropMongoDb()
  }

  private class TestGuiceModule extends AbstractModule {
    override def configure(): Unit = {}
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

  def checkInviteSentExitSurveyAgentSignOutLink(result: Future[Result]): Unit = {
    checkHtmlResultWithBodyText(result, htmlEscapedMessage("common.sign-out"))
    val continueUrl = URLEncoder.encode(agentFeedbackSurveyURNWithOriginToken, StandardCharsets.UTF_8.name())
    checkHtmlResultWithBodyText(result, continueUrl)
  }

  def checkResultContainsLink(
    result: Future[Result],
    linkUrl: String,
    linkText: String,
    clazz: Option[String] = None,
    newWin: Boolean = false): Unit = {
    val t = if(newWin) "target=" + "\"" + """_blank""" + "\"" else ""
    val a = s"<a $t".trim
    val element = if (clazz.isDefined) {
      s"""$a href="$linkUrl" class="${clazz.get}">$linkText</a>"""
    } else {
      s"""$a href="$linkUrl">$linkText</a>"""
    }
    checkHtmlResultWithBodyText(result, element)
  }

  def checkResultContainsBackLink(result: Future[Result], backLinkUrl: String): Unit = {
    val element = s"""<a id="backLink" href="$backLinkUrl" class="link-back">Back</a>"""
    checkHtmlResultWithBodyText(result, element)
  }

  def checkResultBodyContainsTitle(result: Future[Result], title: String): Unit = {
    val element = s"""<title>$title</title>"""
    checkHtmlResultWithBodyText(result, element)
  }

  def checkHasAgentSignOutLink(result: Future[Result]): Unit = {
    checkHtmlResultWithBodyText(result, htmlEscapedMessage("common.sign-out"))
    val asAcHomepageExternalUrl = wireMockBaseUrlAsString
    val continueUrl =
      URLEncoder.encode(s"$asAcHomepageExternalUrl/agent-services-account", StandardCharsets.UTF_8.name())
    checkHtmlResultWithBodyText(result, s"$companyAuthUrl$companyAuthSignOutPath?continue=$continueUrl")
  }

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

  def checkHasClientSignOutUrl(result: Future[Result]): Unit = {
    checkHtmlResultWithBodyText(result, htmlEscapedMessage("common.sign-out"))
    val continueUrl = URLEncoder.encode(s"$businessTaxAccountUrl/business-account", StandardCharsets.UTF_8.name())
    checkHtmlResultWithBodyText(result, s"$companyAuthUrl$companyAuthSignOutPath?continue=$continueUrl")
  }

  def checkExitSurveyAfterInviteResponseSignOutUrl(result: Future[Result]): Unit = {
    checkHtmlResultWithBodyText(result, htmlEscapedMessage("common.sign-out"))
    val continueUrl = URLEncoder.encode(clientFeedbackSurveyURNWithOriginToken, StandardCharsets.UTF_8.name())
    checkHtmlResultWithBodyText(result, continueUrl)
  }

  def verifyAgentClientInvitationSubmittedEvent(
    arn: String,
    clientType: String,
    clientId: String,
    clientIdType: String,
    result: String,
    service: String,
    uid: String): Unit =
    verifyAuditRequestSent(
      1,
      AgentInvitationEvent.AgentClientAuthorisationRequestCreated,
      detail = Map(
        "factCheck"            -> result,
        "agentReferenceNumber" -> arn,
        "clientType"           -> clientType,
        "clientIdType"         -> clientIdType,
        "clientId"             -> clientId,
        "service"              -> service,
        "uid"                  -> uid
      ),
      tags = Map(
        "transactionName" -> "Agent client service authorisation request created"
      )
    )

  def verifyAgentClientInvitationSubmittedEventFailed(
    arn: String,
    clientType: String,
    clientId: String,
    clientIdType: String,
    result: String,
    service: String): Unit =
    verifyAuditRequestSent(
      1,
      AgentInvitationEvent.AgentClientAuthorisationRequestCreated,
      detail = Map(
        "factCheck"            -> result,
        "agentReferenceNumber" -> arn,
        "clientType"           -> clientType,
        "clientIdType"         -> clientIdType,
        "clientId"             -> clientId,
        "service"              -> service
      ),
      tags = Map(
        "transactionName" -> "Agent client service authorisation request created"
      )
    )

  protected def checkRedirectedToIVUplift(
    result: Future[Result],
    expectedCompletionUrl: String,
    expectedFailureUrl: String): Assertion = {
    val expectedRedirectUrl = CallOps.addParamsToUrl(
      url = "/mdtp/uplift?origin=aif",
      "confidenceLevel" -> Some("200"),
      "completionURL"   -> Some(expectedCompletionUrl),
      "failureURL"      -> Some(expectedFailureUrl)
    )

    status(result) shouldBe SEE_OTHER
    redirectLocation(result) shouldBe Some(expectedRedirectUrl)
  }
}
