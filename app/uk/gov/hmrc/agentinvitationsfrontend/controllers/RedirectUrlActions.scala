/*
 * Copyright 2021 HM Revenue & Customs
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

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Request, Result}
import uk.gov.hmrc.agentinvitationsfrontend.connectors.SsoConnector
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier}
import uk.gov.hmrc.play.bootstrap.binders.RedirectUrl._
import uk.gov.hmrc.play.bootstrap.binders.{AbsoluteWithHostnameFromAllowlist, RedirectUrl, UnsafePermitAll}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

@Singleton
class RedirectUrlActions @Inject()(ssoConnector: SsoConnector) {

  def getRedirectUrl[A](implicit request: Request[A]): Option[RedirectUrl] = getUrl(request.getQueryString("continue"))

  def getErrorUrl[A](implicit request: Request[A]): Option[RedirectUrl] = getUrl(request.getQueryString("error"))

  def getRefererUrl[A](implicit request: Request[A]): Option[RedirectUrl] = getUrl(request.headers.get("Referer"))

  def getUrl[A](urlOpt: Option[String]): Option[RedirectUrl] =
    urlOpt match {
      case Some(redirectUrl) =>
        Try(RedirectUrl(redirectUrl)) match {
          case Success(url) => Some(url)
          case Failure(e) =>
            throw new BadRequestException(s"[$redirectUrl] is not a valid error URL, $e")
        }
      case None =>
        None
    }

  def maybeRedirectUrlOrBadRequest(redirectUrlOpt: Option[RedirectUrl])(
    block: Option[String] => Future[Result])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Result] = {

    val allowlistPolicy = AbsoluteWithHostnameFromAllowlist(ssoConnector.getAllowlistedDomains())

    redirectUrlOpt match {
      case Some(redirectUrl) =>
        val unsafeUrl = redirectUrl.get(UnsafePermitAll).url
        if (RedirectUrl.isRelativeUrl(unsafeUrl)) block(Some(unsafeUrl))
        else
          redirectUrl.getEither(allowlistPolicy).flatMap {
            case Right(safeRedirectUrl) => block(Some(safeRedirectUrl.url))
            case Left(errorMessage) =>
              throw new BadRequestException(errorMessage)
          }
      case None => block(None)
    }
  }
}
