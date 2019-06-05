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

package uk.gov.hmrc.agentinvitationsfrontend.services

import java.net.URL
import java.util.Collections.emptyList

import javax.inject.{Inject, Singleton}
import play.api.Configuration
import uk.gov.hmrc.agentinvitationsfrontend.connectors.SsoConnector
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.binders.RedirectUrl._
import uk.gov.hmrc.play.bootstrap.binders.{AbsoluteWithHostnameFromWhitelist, RedirectUrl}

import scala.collection.JavaConversions._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

@Singleton
class HostnameWhiteListService @Inject()(config: Configuration, ssoConnector: SsoConnector) {

  val domainWhiteList: Set[String] = config.getStringList("continueUrl.domainWhiteList").getOrElse(emptyList()).toSet

  def hasExternalDomain(redirectUrl: RedirectUrl)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    ssoConnector.validateExternalDomain(getHost(redirectUrl))

  def isAbsoluteUrlWhiteListed(
    redirectUrl: RedirectUrl)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    if (!hasInternalDomain(redirectUrl)) hasExternalDomain(redirectUrl)
    else Future.successful(true)

  def hasInternalDomain(redirectUrl: RedirectUrl): Boolean = domainWhiteList.contains(getHost(redirectUrl))

  private def getHost(redirectUrl: RedirectUrl): String =
    Try(new URL(redirectUrl.get(AbsoluteWithHostnameFromWhitelist(domainWhiteList)).url).getHost).getOrElse("invalid")
}
