/*
 * Copyright 2019 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import javax.inject.{Inject, Singleton}
import com.google.inject.name.Named
import play.api.http.HeaderNames.CACHE_CONTROL
import play.api.http.HttpErrorHandler
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc.Results._
import play.api.mvc.{RequestHeader, Result}
import play.api.{Configuration, Environment, Logger, Mode}
import uk.gov.hmrc.agentinvitationsfrontend.binders.ErrorConstants
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.controllers.routes
import uk.gov.hmrc.agentinvitationsfrontend.views.html.error_template
import uk.gov.hmrc.auth.core.{AuthorisationException, InsufficientEnrolments, NoActiveSession, UnsupportedAffinityGroup}
import uk.gov.hmrc.auth.otac.OtacFailureThrowable
import uk.gov.hmrc.http.{JsValidationException, NotFoundException}
import uk.gov.hmrc.play.HeaderCarrierConverter
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.bootstrap.config.{AuthRedirects, HttpAuditEvent}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ErrorHandler @Inject()(
  val env: Environment,
  val messagesApi: MessagesApi,
  val auditConnector: AuditConnector,
  @Named("appName") val appName: String)(
  implicit val config: Configuration,
  ec: ExecutionContext,
  externalUrls: ExternalUrls)
    extends HttpErrorHandler with I18nSupport with AuthRedirects with ErrorAuditing {

  lazy val authenticationRedirect: String = config
    .getString("authentication.login-callback.url")
    .getOrElse(
      throw new IllegalStateException(s"No value found for configuration property: authentication.login-callback.url"))

  override def onClientError(request: RequestHeader, statusCode: Int, message: String): Future[Result] = {
    auditClientError(request, statusCode, message)

    val response = statusCode match {
      case 400 if message.equals(ErrorConstants.InvitationIdNotFound) =>
        Redirect(routes.ClientsInvitationController.notFoundInvitation())
      case _ =>
        Status(statusCode)(
          error_template(
            Messages(s"global.error.$statusCode.title"),
            Messages(s"global.error.$statusCode.heading"),
            Messages(s"global.error.$statusCode.message")))
    }

    Future.successful(response)
  }

  override def onServerError(request: RequestHeader, exception: Throwable): Future[Result] = {
    auditServerError(request, exception)
    val response = exception match {

      case _: AuthorisationException =>
        val isDevEnv =
          if (env.mode.equals(Mode.Test)) false else config.getString("run.mode").forall(Mode.Dev.toString.equals)
        toGGLogin(if (isDevEnv) s"http://${request.host}${request.uri}" else s"$authenticationRedirect${request.uri}")

      case ex: OtacFailureThrowable =>
        Logger(getClass).warn(s"There has been an Unauthorised Attempt: ${ex.getMessage}")
        Forbidden(
          error_template(
            Messages("global.error.passcode.title"),
            Messages("global.error.passcode.heading"),
            Messages("global.error.passcode.message"))).withHeaders(CACHE_CONTROL -> "no-cache")

      case ex =>
        Logger(getClass).warn(s"There has been a failure", ex)
        InternalServerError(
          error_template(
            Messages("global.error.500.title"),
            Messages("global.error.500.heading"),
            Messages("global.error.500.message"))).withHeaders(CACHE_CONTROL -> "no-cache")

    }
    Future.successful(response)
  }
}

object EventTypes {

  val RequestReceived: String = "RequestReceived"
  val TransactionFailureReason: String = "transactionFailureReason"
  val ServerInternalError: String = "ServerInternalError"
  val ResourceNotFound: String = "ResourceNotFound"
  val ServerValidationError: String = "ServerValidationError"
}

trait ErrorAuditing extends HttpAuditEvent {

  import EventTypes._

  def auditConnector: AuditConnector

  private val unexpectedError = "Unexpected error"
  private val notFoundError = "Resource Endpoint Not Found"
  private val badRequestError = "Request bad format exception"

  def auditServerError(request: RequestHeader, ex: Throwable)(implicit ec: ExecutionContext): Unit = {
    val eventType = ex match {
      case _: NotFoundException     => ResourceNotFound
      case _: JsValidationException => ServerValidationError
      case _                        => ServerInternalError
    }
    val transactionName = ex match {
      case _: NotFoundException => notFoundError
      case _                    => unexpectedError
    }
    auditConnector.sendEvent(
      dataEvent(eventType, transactionName, request, Map(TransactionFailureReason -> ex.getMessage))(
        HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))))
  }

  def auditClientError(request: RequestHeader, statusCode: Int, message: String)(
    implicit ec: ExecutionContext): Unit = {
    import play.api.http.Status._
    statusCode match {
      case NOT_FOUND =>
        auditConnector.sendEvent(
          dataEvent(ResourceNotFound, notFoundError, request, Map(TransactionFailureReason -> message))(
            HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))))
      case BAD_REQUEST =>
        auditConnector.sendEvent(
          dataEvent(ServerValidationError, badRequestError, request, Map(TransactionFailureReason -> message))(
            HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))))
      case _ =>
    }
  }
}
