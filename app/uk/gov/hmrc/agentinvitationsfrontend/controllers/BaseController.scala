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

import play.api.Configuration
import play.api.i18n.{I18nSupport, Messages}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext

abstract class BaseController(
  val withVerifiedPasscode: PasscodeVerification,
  val authConnector: AuthConnector,
  featureFlags: FeatureFlags)(
  implicit val externalUrls: ExternalUrls,
  val messagesApi: play.api.i18n.MessagesApi,
  val configuration: Configuration,
  ec: ExecutionContext)
    extends FrontendController with I18nSupport with AuthActions {

  val personalOption = Seq("personal" -> Messages("client-type.personal"))
  val businessOption = Seq("business" -> Messages("client-type.business"))
  val clientTypes = personalOption ++ businessOption

  val personalIncomeRecord =
    if (featureFlags.showPersonalIncome)
      Seq(HMRCPIR -> Messages("personal-select-service.personal-income-viewer"))
    else Seq.empty

  val mtdItId =
    if (featureFlags.showHmrcMtdIt) Seq(HMRCMTDIT -> Messages("personal-select-service.itsa")) else Seq.empty

  val vat =
    if (featureFlags.showHmrcMtdVat) Seq(HMRCMTDVAT -> Messages("select-service.vat")) else Seq.empty

  def enabledPersonalServicesForCancelAuth(isWhitelisted: Boolean): Seq[(String, String)] =
    if (isWhitelisted) {
      personalIncomeRecord ++ mtdItId ++ vat
    } else {
      mtdItId ++ vat
    }

  def enabledBusinessServicesForCancelAuthorisation: Seq[(String, String)] = vat

  def enabledPersonalServicesForInvitation(isWhitelisted: Boolean): Seq[(String, String)] =
    if (isWhitelisted) {
      personalIncomeRecord ++ mtdItId ++ vat
    } else {
      mtdItId ++ vat
    }

  val serviceToMessageKey: String => String = {
    case HMRCMTDIT  => messageKeyForITSA
    case HMRCPIR    => messageKeyForAfi
    case HMRCMTDVAT => messageKeyForVAT
    case _          => "Service is missing"
  }

}
