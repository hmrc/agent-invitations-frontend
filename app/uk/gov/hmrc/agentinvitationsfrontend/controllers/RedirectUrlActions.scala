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

package uk.gov.hmrc.agentinvitationsfrontend.controllers

import javax.inject.{Inject, Singleton}
import play.api.Logger
import play.api.mvc.{Request, Result}
import uk.gov.hmrc.agentinvitationsfrontend.services.HostnameWhiteListService
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier}
import uk.gov.hmrc.play.HeaderCarrierConverter
import uk.gov.hmrc.play.bootstrap.binders.RedirectUrl._
import uk.gov.hmrc.play.bootstrap.binders.{AbsoluteWithHostnameFromWhitelist, RedirectUrl, UnsafePermitAll}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
@Singleton
class RedirectUrlActions @Inject()(whiteListService: HostnameWhiteListService) {

  val policy = AbsoluteWithHostnameFromWhitelist(whiteListService.domainWhiteList)

  def extractErrorUrl[A](implicit request: Request[A], ec: ExecutionContext): Future[Option[RedirectUrl]] = {
    implicit val hc = HeaderCarrierConverter.fromHeadersAndSession(request.headers, Option(request.session))

    request.getQueryString("error") match {
      case Some(redirectUrl) =>
        Try(RedirectUrl(redirectUrl)) match {
          case Success(url) =>
            isRelativeOrAbsoluteWhiteListed(url)
              .collect {
                case true => Some(url)
              }
              .recover {
                case NonFatal(e) =>
                  Logger(getClass).warn(s"Check for whitelisted hostname failed", e)
                  None
              }
          case Failure(e) =>
            Logger(getClass).warn(s"$redirectUrl is not a valid continue URL", e)
            Future.successful(None)
        }
      case None =>
        Future.successful(None)
    }
  }

  def extractRedirectUrl[A](implicit request: Request[A], ec: ExecutionContext): Future[Option[RedirectUrl]] = {
    implicit val hc = HeaderCarrierConverter.fromHeadersAndSession(request.headers, Option(request.session))

    request.getQueryString("continue") match {
      case Some(redirectUrl) =>
        Try(RedirectUrl(redirectUrl)) match {
          case Success(url) =>
            isRelativeOrAbsoluteWhiteListed(url)
              .collect {
                case true => Some(url)
              }
              .recover {
                case NonFatal(e) =>
                  Logger(getClass).warn(s"Check for whitelisted hostname failed", e)
                  None
              }
          case Failure(e) =>
            Logger(getClass).warn(s"$redirectUrl is not a valid continue URL", e)
            Future.successful(None)
        }
      case None =>
        Future.successful(None)
    }
  }

  def getRedirectUrl[A](implicit request: Request[A]): Option[RedirectUrl] =
    request.getQueryString("continue") match {
      case Some(redirectUrl) =>
        Try(RedirectUrl(redirectUrl)) match {
          case Success(url) => Some(url)
          case Failure(e) =>
            throw new BadRequestException(s"[$redirectUrl] is not a valid continue URL, $e")
        }
      case None =>
        None
    }

  def getErrorUrl[A](implicit request: Request[A]): Option[RedirectUrl] =
    request.getQueryString("error") match {
      case Some(redirectUrl) =>
        Try(RedirectUrl(redirectUrl)) match {
          case Success(url) => Some(url)
          case Failure(e) =>
            throw new BadRequestException(s"[$redirectUrl] is not a valid error URL, $e")
        }
      case None =>
        None
    }

  private def isRelativeOrAbsoluteWhiteListed(
    redirectUrl: RedirectUrl)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    if (!RedirectUrl.isRelativeUrl(redirectUrl.get(policy).url))
      whiteListService.isAbsoluteUrlWhiteListed(redirectUrl)
    else Future.successful(true)

  def withMaybeRedirectUrl[A](block: Option[RedirectUrl] => Future[Result])(
    implicit request: Request[A],
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[Result] = {
    val redirectUrl: Future[Option[RedirectUrl]] = extractRedirectUrl
    redirectUrl.flatMap(block(_))
  }

  def maybeRedirectUrlOrBadRequest(redirectUrlOpt: Option[RedirectUrl])(block: Option[String] => Future[Result])(
    implicit request: Request[Any]): Future[Result] =
    redirectUrlOpt match {
      case Some(redirectUrl) =>
        val unsafeUrl = redirectUrl.get(UnsafePermitAll).url
        if (RedirectUrl.isRelativeUrl(unsafeUrl)) block(Some(unsafeUrl))
        else
          redirectUrl.getEither(policy) match {
            case Right(safeRedirectUrl) => block(Some(safeRedirectUrl.url))
            case Left(errorMessage) =>
              throw new BadRequestException(errorMessage)
          }
      case None => block(None)
    }

  def withMaybeErrorUrl[A](block: Option[RedirectUrl] => Future[Result])(
    implicit request: Request[A],
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[Result] = {
    val redirectUrl: Future[Option[RedirectUrl]] = extractErrorUrl
    redirectUrl.flatMap(block(_))
  }

}
