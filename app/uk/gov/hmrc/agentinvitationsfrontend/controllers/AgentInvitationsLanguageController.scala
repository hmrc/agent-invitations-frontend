/*
 * Copyright 2023 HM Revenue & Customs
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

import javax.inject.Inject
import play.api.i18n.{Lang, MessagesApi}
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.play.language.{LanguageController, LanguageUtils}

class AgentInvitationsLanguageController @Inject() (languageUtils: LanguageUtils, cc: ControllerComponents, appConfig: AppConfig)(implicit
  override val messagesApi: MessagesApi
) extends LanguageController(languageUtils, cc) {

  override def languageMap: Map[String, Lang] = appConfig.languageMap

  override def fallbackURL: String = "https://www.tax.service.gov.uk/agent-services-account/"

  override def switchToLanguage(language: String): Action[AnyContent] = Action { implicit request =>
    val enabled: Boolean = languageMap.get(language).exists(languageUtils.isLangAvailable)
    val lang: Lang =
      if (enabled) languageMap.getOrElse(language, languageUtils.getCurrentLang)
      else languageUtils.getCurrentLang
    val redirectURL: String = request.headers.get(REFERER).getOrElse(fallbackURL)
    Redirect(redirectURL).withLang(Lang.apply(lang.code))
  }

}
