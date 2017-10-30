/*
 * Copyright 2017 HM Revenue & Customs
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

import play.api.http.HeaderNames.CACHE_CONTROL
import play.api.http.HttpErrorHandler
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc.Results._
import play.api.mvc.{RequestHeader, Result}
import play.api.{Configuration, Environment, Mode}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.error_template
import uk.gov.hmrc.auth.core.{InsufficientEnrolments, NoActiveSession}
import uk.gov.hmrc.play.frontend.config.AuthRedirects

import scala.concurrent.Future

@Singleton
class ErrorHandler @Inject() (implicit val config: Configuration, val env: Environment, val messagesApi: MessagesApi)
  extends HttpErrorHandler with I18nSupport with AuthRedirects {

  override def onClientError(request: RequestHeader, statusCode: Int, message: String): Future[Result] = {
    Future successful
      Status(statusCode)(error_template(
        Messages(s"global.error.$statusCode.title"),
        Messages(s"global.error.$statusCode.heading"),
        Messages(s"global.error.$statusCode.message")))
  }

  override def onServerError(request: RequestHeader, exception: Throwable): Future[Result] = {
    val response = exception match {
      case _: NoActiveSession => toGGLogin(if (env.mode.equals(Mode.Dev)) s"http://${request.host}${request.uri}" else s"${request.uri}")
      case _: InsufficientEnrolments => Forbidden
      case _ => InternalServerError(error_template(
        Messages("global.error.500.title"),
        Messages("global.error.500.heading"),
        Messages("global.error.500.message"))).withHeaders(CACHE_CONTROL -> "no-cache")
    }
    Future.successful(response)
  }
}