/*
 * Copyright 2020 HM Revenue & Customs
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
import play.api.http.HeaderNames
import play.api.i18n.{Messages, MessagesApi}
import play.api.mvc.Results._
import play.api.mvc.{Request, RequestHeader, Result}
import play.api.{Configuration, Environment, Logging}
import play.twirl.api.Html
import uk.gov.hmrc.agentinvitationsfrontend.binders.ErrorConstants
import uk.gov.hmrc.agentinvitationsfrontend.config.{AppConfig, ExternalUrls}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.routes
import uk.gov.hmrc.agentinvitationsfrontend.views.html.{error_template, error_template_5xx}
import uk.gov.hmrc.auth.otac.OtacFailureThrowable
import uk.gov.hmrc.http.{JsValidationException, NotFoundException}
import uk.gov.hmrc.play.HeaderCarrierConverter
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.bootstrap.config.{AuthRedirects, HttpAuditEvent}
import uk.gov.hmrc.play.bootstrap.frontend.http.FrontendErrorHandler

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ErrorHandler @Inject()(
  val env: Environment,
  val auditConnector: AuditConnector,
  errorTemplate: error_template,
  errorTemplate5xx: error_template_5xx)(
  implicit val config: Configuration,
  ec: ExecutionContext,
  externalUrls: ExternalUrls,
  appConfig: AppConfig,
  val messagesApi: MessagesApi)
    extends FrontendErrorHandler with AuthRedirects with ErrorAuditing with HeaderNames with Logging {

  override def onClientError(request: RequestHeader, statusCode: Int, message: String): Future[Result] = {
    auditClientError(request, statusCode, message)

    val response = statusCode match {
      case 400 if message.equals(ErrorConstants.InvitationIdNotFound) =>
        Future.successful(Redirect(routes.ClientInvitationJourneyController.showNotFoundInvitation()))
      case _ =>
        super.onClientError(request, statusCode, message)
    }

    response
  }

  override def onServerError(request: RequestHeader, exception: Throwable): Future[Result] = {
    auditServerError(request, exception)
    implicit val r: Request[String] = Request(request, "")
    val response = exception match {

      case ex: OtacFailureThrowable =>
        logger.warn(s"There has been an Unauthorised Attempt: ${ex.getMessage}")
        Forbidden(
          errorTemplate(
            Messages("global.error.passcode.title"),
            Messages("global.error.passcode.heading"),
            Messages("global.error.passcode.message"))).withHeaders(CACHE_CONTROL -> "no-cache")

      case ex =>
        logger.warn(s"There has been a failure", ex)
        InternalServerError(errorTemplate5xx()).withHeaders(CACHE_CONTROL -> "no-cache")
    }
    Future.successful(response)
  }

  override def appName: String = appConfig.appName

  override def standardErrorTemplate(pageTitle: String, heading: String, message: String)(implicit request: Request[_]): Html =
    errorTemplate(pageTitle, heading, message)

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
    ()
  }

  def auditClientError(request: RequestHeader, statusCode: Int, message: String)(implicit ec: ExecutionContext): Unit = {
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
    ()
  }
}
