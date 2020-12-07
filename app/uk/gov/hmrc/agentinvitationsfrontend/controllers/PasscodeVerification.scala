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

package uk.gov.hmrc.agentinvitationsfrontend.controllers

import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import play.api.mvc.Results._
import play.api.mvc.{Request, Result}
import play.api.{Environment, Logging}
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.auth.otac.{Authorised, PlayOtacAuthConnector}
import uk.gov.hmrc.http.{CoreGet, HeaderCarrier, HttpClient, SessionKeys}

import scala.concurrent.{ExecutionContext, Future}

class PasscodeVerificationException(msg: String) extends RuntimeException(msg)

@ImplementedBy(classOf[FrontendPasscodeVerification])
trait PasscodeVerification {
  def apply[A](body: Boolean => Future[Result])(implicit request: Request[A], headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result]
}

@Singleton
class OtacAuthConnectorImpl @Inject()(val httpClient: HttpClient)(implicit appConfig: AppConfig) extends PlayOtacAuthConnector {
  override val serviceUrl: String = appConfig.authBaseUrl
  override def http: CoreGet = httpClient
}

@Singleton
class FrontendPasscodeVerification @Inject()(environment: Environment, otacAuthConnector: OtacAuthConnectorImpl)(implicit appConfig: AppConfig)
    extends PasscodeVerification with Logging {

  val tokenParam = "p"
  val passcodeEnabledKey = "passcodeAuthentication.enabled"
  val passcodeRegimeKey = "passcodeAuthentication.regime"

  lazy val passcodeEnabled: Boolean = appConfig.passcodeAuthEnabled
  lazy val passcodeRegime: String = appConfig.passcodeAuthRegime
  lazy val verificationURL: String = appConfig.passcodeVerificationUrl
  lazy val logoutUrl = s"${appConfig.passcodeVerificationUrl}/otac/logout/$passcodeRegime"

  def loginUrl[A](queryParam: String) = s"$verificationURL/otac/login$queryParam"

  def throwConfigNotFound(configKey: String) =
    throw new PasscodeVerificationException(s"The value for the key '$configKey' should be setup in the config file.")

  def addRedirectUrl[A](token: String)(implicit request: Request[A]): Result => Result =
    e =>
      e.addingToSession(SessionKeys.redirect -> buildRedirectUrl(request))
        .addingToSession("otacTokenParam" -> token)

  def buildRedirectUrl[A](req: Request[A]): String =
    if (appConfig.runMode.env != "Prod") s"http${if (req.secure) "s" else ""}://${req.host}${req.path}" else req.path

  def apply[A](body: Boolean => Future[Result])(implicit request: Request[A], headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] =
    if (passcodeEnabled) {
      request.session
        .get(SessionKeys.otacToken)
        .fold(
          request.getQueryString(tokenParam) match {
            case Some(token) => {
              val queryParam = s"?$tokenParam=$token"
              Future.successful(Redirect(loginUrl(queryParam))) map addRedirectUrl(token)(request)
            }
            case _ => body(false)
          }
        ) { otacToken =>
          otacAuthConnector
            .authorise(passcodeRegime, headerCarrier, Option(otacToken))
            .flatMap {
              case Authorised => body(true)
              case _          => body(false)
            }
            .recoverWith {
              case ex =>
                logger.warn("error during passcode authentication check", ex)
                body(false)
            }
        }
    } else {
      body(true)
    }
}
